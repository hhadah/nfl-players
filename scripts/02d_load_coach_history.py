"""Historical opening head coaches and team records, 1989-1998.

Purpose: extend the head-coach hiring analysis before the nflverse window
(1999+). 1989 is included only as the lag season, so that a 1990 hire is the
first observable change of opening coach; 1989 rows and first-season rows of
a franchise (Carolina and Jacksonville 1995, Baltimore 1996) are left-
censored entries, not hires (prior_season_observed = 0).

Sources (every response cached under data/raw/, so a second run is offline;
--refresh re-downloads)
  1. Pro-Football-Reference year coaches pages, /years/<season>/coaches.htm:
     every head coach of the season with team, games, wins, losses and ties
     (interim coaches have their own row). PFR answers automated requests
     with HTTP 403, so the pages are read from the Internet Archive's copy
     (web.archive.org, 'id_' raw mode). The archive URL actually served is
     recorded next to the page (source_archive_url) and the PFR URL is kept
     as source_url. Cache: data/raw/pfr_wayback/.
  2. Wikipedia team-season articles ("1989 Atlanta Falcons season"): the
     infobox 'coach' field lists the season's head coaches in chronological
     order with annotations ("fired November 28, 3-9 record", "interim";
     dates only where the article gives them) and the 'record' field gives
     the W-L(-T). PFR rows are not in chronological order (the 1989 Falcons
     list the interim Hanifan before Campbell), so the opening coach of a
     multi-coach season is the first coach in the Wikipedia order; the
     assignment counts as confirmed only when the annotations say who left
     or who was interim, otherwise the row is flagged (opening_coach_
     assignment = 'multi_order_unconfirmed', opening_coach_uncertain = 1).
     The Wikipedia record must equal the PFR sum; a mismatch stops the run.
  3. Coach demographics, EXPLICIT sources only (never names or photographs):
     Wikidata 'ethnic group' (P172) statements of the coach's item and the
     coach article's non-hidden Wikipedia categories, mapped with the same
     dictionaries used for current staff (04d_race_documented.ETHNIC_GROUP_MAP,
     02b_load_nfl_staff.CATEGORY_FLAGS). documented_black / documented_minority
     are 1 when a source states it, 0 when a source states a different race
     and none states Black / a minority, NULL otherwise. Hispanic is an
     ethnicity, so a Hispanic-only statement leaves documented_black NULL.
     Nobody is coded white for lack of evidence. In the 2026-10 pull no coach
     item carried P172; the five documented opening coaches come from
     Wikipedia categories, and the other 66 coaches have no explicit
     statement (documented_* NULL). Person-level results stay in the
     git-ignored DuckDB; this file names nobody's race.
  4. Ascertainment assumption for the 'assumed' indicators: the Wikipedia
     'Rooney Rule' article names the only minority coaches who held
     modern-era NFL head-coaching jobs before the rule (six people). The
     loader extracts that sentence's links, checks that every documented-
     minority opening coach of 1989-1998 is on it and that every listed
     coach present in the window is documented, and only then sets
     *_assumed = documented positive else 0. The assumption (documented-
     positive vs all other, not
     'all other = white') is spelled out in demographic_assumption and
     benchmarked against the TIDES 1992-1999 head-coach counts in
     nfl_hc_history_coverage. Season-level counts are the only demographic
     output outside the (git-ignored) DuckDB.

Hire dates: Wikidata holds no coaching-spell start dates for these coaches
and PFR gives seasons only, so no hire date column exists. In-season change
dates appear only as written in hc_change_note.

Tables written (common.write_table, full refresh)
  nfl_hc_history           franchise_id x season (288 rows: 28 teams 1989-94,
                           30 from 1995; Browns 1989-95 only, Ravens from
                           1996). Contract columns: franchise_id, season,
                           opening_coach_name, wins, losses, ties, games,
                           win_pct, n_head_coaches, source_url,
                           coach_source_url, opening_coach_documented_black,
                           opening_coach_documented_minority,
                           demographic_source_url, demographic_basis; plus
                           the identification, order, censoring and
                           assumption columns described in COLUMNS below.
  nfl_hc_history_coaches   coach x franchise x season: PFR games and record,
                           Wikipedia order, annotation, article, QID and the
                           documented flags (person level; DuckDB only)
  nfl_hc_history_coverage  season aggregates: teams, multi-coach seasons,
                           unconfirmed orders, documented Black / minority /
                           unknown opening coaches, TIDES head-coach counts

Usage: .venv/bin/python scripts/02d_load_coach_history.py [--refresh] [--no-db]
"""
import argparse
import importlib
import json
import re
import unicodedata

import pandas as pd
from bs4 import BeautifulSoup

from common import FRANCHISES, Wiki, connect, http_session, now_utc, to_franchise, write_table
from config import DATA_DIR, RAW_DIR

staff_mod = importlib.import_module("02b_load_nfl_staff")      # query_all, chunks, PAGE_PROPS
race_mod = importlib.import_module("04d_race_documented")      # P172 helpers, map_label

HC_HISTORY_SEASONS = list(range(1989, 1999))
PFR_URL = "https://www.pro-football-reference.com/years/{season}/coaches.htm"
WAYBACK_URL = "https://web.archive.org/web/2024id_/{url}"     # nearest 2024 capture, raw
PFR_CACHE = RAW_DIR / "pfr_wayback"
WIKI_NS = "coach_history"                                      # data/raw/wikipedia/coach_history/
ROONEY_TITLE = "Rooney Rule"
TIDES_PATH = DATA_DIR / "reference" / "tides_nfl_race_shares.csv"
GAMES_PER_SEASON = 16

# Team name used in each season (Wikipedia article title "<season> <name> season").
# Franchises not listed keep their current name for the whole window.
HISTORICAL_NAMES = {
    "ARI": [(1989, 1993, "Phoenix Cardinals"), (1994, 1998, "Arizona Cardinals")],
    "TEN": [(1989, 1996, "Houston Oilers"), (1997, 1998, "Tennessee Oilers")],
    "LV": [(1989, 1994, "Los Angeles Raiders"), (1995, 1998, "Oakland Raiders")],
    "LA": [(1989, 1994, "Los Angeles Rams"), (1995, 1998, "St. Louis Rams")],
    "LAC": [(1989, 1998, "San Diego Chargers")],
    "WAS": [(1989, 1998, "Washington Redskins")],
}
# First and last season of each franchise inside the window (NFL/PFR
# convention: the Browns' record stays with Cleveland, suspended 1996-98;
# the Ravens start as a new franchise in 1996).
FRANCHISE_WINDOW = {"CLE": (1989, 1995), "BAL": (1996, 1998), "CAR": (1995, 1998),
                    "JAX": (1995, 1998), "HOU": (None, None)}

# Annotation words in the infobox that say a coach LEFT during the season
# (confirms the first-listed coach opened) or JOINED during it.
LEFT_RE = re.compile(r"\b(fired|resigned|retired|dismissed|stepped down|relieved|"
                     r"replaced|first \d+|through week|until|died|hospitali[sz]ed|"
                     r"quit|left)\b", re.I)
JOINED_RE = re.compile(r"\b(interim|last \d+|final \d+|remainder|rest of|took over|"
                       r"replaced|from week|weeks? \d+\s*[-\u2013]\s*\d+|succeeded)\b", re.I)

MINORITY_CATEGORIES = {"black", "hispanic", "asian", "pacific_islander",
                       "american_indian", "multiracial"}
CATEGORY_TOKENS = {"cat_black": "black", "cat_hispanic_latino": "hispanic",
                   "cat_asian": "asian", "cat_pacific_islander": "pacific_islander",
                   "cat_native_american": "american_indian"}
DEMOGRAPHIC_ASSUMPTION = (
    "opening_coach_*_assumed = 1 when a source documents the category, else 0. "
    "Ascertainment assumption: every Black / minority NFL head coach of 1989-1998 "
    "is documented in Wikidata P172 or Wikipedia ancestry categories, and the "
    "Wikipedia 'Rooney Rule' article's list of the only minority coaches who held "
    "modern-era head coaching jobs before the rule (permalink in demographic_basis) "
    "is complete; the loader checks the two against each other. 0 means 'not "
    "documented as such', NOT 'documented white'.")

COLUMNS = [
    "franchise_id", "season", "team_name", "pfr_team_slug",
    "opening_coach_name", "opening_coach_pfr_id", "opening_coach_wiki_title",
    "opening_coach_wikidata_qid", "opening_coach_games", "opening_coach_wins",
    "opening_coach_losses", "opening_coach_ties",
    "wins", "losses", "ties", "games", "win_pct", "n_head_coaches",
    "head_coaches_all", "in_season_hc_change", "hc_change_note",
    "opening_coach_assignment", "opening_coach_uncertain", "prior_season_observed",
    "source_url", "source_archive_url", "coach_source_url", "record_check",
    "opening_coach_documented_black", "opening_coach_documented_minority",
    "demographic_source_url", "demographic_basis",
    "opening_coach_black_assumed", "opening_coach_minority_assumed",
    "demographic_assumption",
]


# ============================================================================
# Franchise universe
# ============================================================================

def team_name(fid, season):
    for lo, hi, name in HISTORICAL_NAMES.get(fid, []):
        if lo <= season <= hi:
            return name
    return FRANCHISES[fid]["name"]


def expected_franchises(season):
    out = []
    for fid in FRANCHISES:
        lo, hi = FRANCHISE_WINDOW.get(fid, (1989, 1998))
        if lo is not None and lo <= season <= hi:
            out.append(fid)
    return sorted(out)


# ============================================================================
# PFR year coaches pages (Internet Archive copies)
# ============================================================================

def fetch_pfr_coaches(session, season, refresh):
    """(html, meta) for one season; meta has the PFR and archive URLs."""
    PFR_CACHE.mkdir(parents=True, exist_ok=True)
    html_path = PFR_CACHE / f"years_{season}_coaches.htm"
    meta_path = html_path.with_suffix(".json")
    if html_path.exists() and meta_path.exists() and not refresh:
        return html_path.read_text(encoding="utf-8"), json.loads(meta_path.read_text())
    url = PFR_URL.format(season=season)
    r = session.get(WAYBACK_URL.format(url=url), timeout=180)
    r.raise_for_status()
    if "web.archive.org" not in r.url:
        raise RuntimeError(f"Archive did not serve a capture for {url}: {r.url}")
    html_path.write_text(r.text, encoding="utf-8")
    meta = {"source_url": url, "archive_url": r.url, "fetched_at": now_utc(),
            "bytes": len(r.content)}
    meta_path.write_text(json.dumps(meta, indent=1))
    return r.text, meta


def parse_pfr_coaches(html, season):
    """One row per coach x team from the 'coaches' table."""
    soup = BeautifulSoup(html, "html.parser")
    table = soup.find("table", id="coaches")
    if table is None:
        raise RuntimeError(f"No coaches table in the {season} PFR page")
    rows = []
    for tr in table.find("tbody").find_all("tr"):
        cells = {c.get("data-stat"): c for c in tr.find_all(["th", "td"])}
        if "coach" not in cells or not cells["coach"].get_text(strip=True):
            continue
        coach_a = cells["coach"].find("a")
        team_a = cells["team"].find("a")
        if coach_a is None or team_a is None:
            raise RuntimeError(f"Unlinked coach/team cell in {season}: {tr.get_text(' ')}")
        slug = re.search(r"/teams/([a-z]{3})/", team_a["href"]).group(1)
        rows.append(dict(
            season=season, coach_name=coach_a.get_text(strip=True),
            coach_pfr_id=re.search(r"/coaches/([^./]+)\.htm", coach_a["href"]).group(1),
            pfr_team_slug=slug, pfr_team_code=team_a.get_text(strip=True),
            games=int(cells["g"].get_text(strip=True)),
            wins=int(cells["wins"].get_text(strip=True)),
            losses=int(cells["losses"].get_text(strip=True)),
            ties=int(cells["ties"].get_text(strip=True) or 0),
            pfr_remark=cells["remark"].get_text(strip=True) or None,
            pfr_row_order=len(rows)))
    return rows


def load_pfr(session, refresh):
    rows, meta = [], {}
    for season in HC_HISTORY_SEASONS:
        html, m = fetch_pfr_coaches(session, season, refresh)
        meta[season] = m
        rows += parse_pfr_coaches(html, season)
    df = pd.DataFrame(rows)
    df["franchise_id"] = df["pfr_team_slug"].map(to_franchise)
    bad = df[df["franchise_id"].isna()]
    if len(bad):
        raise RuntimeError(f"Unmapped PFR team slugs: {sorted(bad['pfr_team_slug'].unique())}")
    # Universe and game counts: every expected franchise, 16 games each,
    # no unexpected franchise.
    for season, g in df.groupby("season"):
        have = sorted(g["franchise_id"].unique())
        want = expected_franchises(season)
        if have != want:
            raise RuntimeError(f"{season}: PFR franchises {have} != expected {want}")
        tot = g.groupby("franchise_id")["games"].sum()
        off = tot[tot != GAMES_PER_SEASON]
        if len(off):
            raise RuntimeError(f"{season}: games per team != {GAMES_PER_SEASON}: {off.to_dict()}")
    if (df["games"] <= 0).any():
        raise RuntimeError("PFR coach row with zero games")
    if (df["wins"] + df["losses"] + df["ties"] != df["games"]).any():
        raise RuntimeError("PFR coach row where W+L+T != G")
    return df, meta


# ============================================================================
# Wikipedia team-season articles
# ============================================================================

def wiki_title(fid, season):
    return f"{season} {team_name(fid, season)} season"


def fetch_pages(wiki, titles, refresh, extra):
    """{requested title: page dict} for up to 50 titles per request, with
    redirects and normalisation resolved back to the requested title(s).
    Two requested titles that redirect to one page both get that page."""
    out = {}
    for chunk in staff_mod.chunks(titles, 50):
        params = {"action": "query", "titles": "|".join(chunk), "redirects": 1, **extra}
        back = {t: {t} for t in chunk}          # final title -> requested titles
        for d in staff_mod.query_all(wiki, params, WIKI_NS, refresh):
            q = d["query"]
            for kind in ("normalized", "redirects"):
                for e in q.get(kind, []):
                    back.setdefault(e["to"], set()).update(back.pop(e["from"], {e["from"]}))
            for p in q.get("pages", []):
                for req in back.get(p["title"], {p["title"]}):
                    page = out.setdefault(req, {"title": p["title"]})
                    for k, v in p.items():
                        if isinstance(v, list):
                            page.setdefault(k, []).extend(v)
                        else:
                            page[k] = v
    return out


CONTENT_PROPS = {"prop": "revisions", "rvprop": "content|ids|timestamp", "rvslots": "main"}


def infobox_field(text, name):
    """Raw value of an infobox parameter (multi-line until the next '|' line)."""
    m = re.search(r"^\s*\|\s*" + name + r"\s*=(.*?)(?=^\s*\|[^\n]*?=|^\s*\}\})",
                  text, re.M | re.S)
    return m.group(1).strip() if m else None


def strip_markup(s):
    s = re.sub(r"\{\{\s*(small|nowrap|smaller)\s*\|(.*?)\}\}", r"\2", s, flags=re.I | re.S)
    s = re.sub(r"<ref[^>]*/>|<ref[^>]*>.*?</ref>", "", s, flags=re.S)
    s = re.sub(r"<!--.*?-->", "", s, flags=re.S)
    s = re.sub(r"</?(small|span|i|b)\b[^>]*>", "", s, flags=re.I)
    s = re.sub(r"'{2,}", "", s)
    return s


def parse_coach_field(raw):
    """Ordered [(link target, display, annotation)] from an infobox coach
    value such as '[[A]] (fired Nov 28, 3-9 record)<br />[[B]] (interim)'."""
    s = strip_markup(raw)
    s = re.sub(r"<br\s*/?>|\{\{(?:plainlist|ubl|unbulleted list)\|", "\n", s, flags=re.I)
    s = re.sub(r"\n\*", "\n", s)
    entries = []
    for part in re.split(r"\n+", s):
        link = re.search(r"\[\[([^\]|]+)(?:\|([^\]]+))?\]\]", part)
        if not link:
            continue
        target = link.group(1).split("#")[0].strip()
        display = (link.group(2) or target).strip()
        note = re.sub(r"[\[\]{}()]|^\s*[|;,]+|\s+", " ", part.replace(link.group(0), "")).strip()
        entries.append((target, display, note or None))
    return entries


def parse_record(raw):
    """(W, L, T) from '3–13' or '8–7–1'; None if unparseable."""
    if raw is None:
        return None
    m = re.search(r"(\d+)\s*[-\u2013\u2014]\s*(\d+)(?:\s*[-\u2013\u2014]\s*(\d+))?", strip_markup(raw))
    if not m:
        return None
    return int(m.group(1)), int(m.group(2)), int(m.group(3) or 0)


def load_wikipedia(wiki, keys, refresh):
    """{(fid, season): dict(title, revid, url, coaches, record)}."""
    titles = {wiki_title(fid, season): (fid, season) for fid, season in keys}
    pages = fetch_pages(wiki, sorted(titles), refresh, CONTENT_PROPS)
    out = {}
    for req, (fid, season) in titles.items():
        p = pages.get(req)
        if p is None or p.get("missing") or not p.get("revisions"):
            raise RuntimeError(f"Wikipedia article missing: {req}")
        rev = p["revisions"][0]
        text = rev["slots"]["main"]["content"]
        coach_raw = infobox_field(text, "coach")
        if coach_raw is None:
            raise RuntimeError(f"No infobox coach field: {p['title']}")
        out[(fid, season)] = dict(
            title=p["title"], revid=rev["revid"],
            url=f"https://en.wikipedia.org/w/index.php?title="
                f"{p['title'].replace(' ', '_')}&oldid={rev['revid']}",
            coaches=parse_coach_field(coach_raw), coach_raw=coach_raw,
            record=parse_record(infobox_field(text, "record")))
    return out


# ============================================================================
# Match PFR coaches to the Wikipedia order
# ============================================================================

def norm_name(s):
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    s = re.sub(r"\(.*?\)", "", s)
    s = re.sub(r"\b(jr|sr|ii|iii)\b\.?", "", s, flags=re.I)
    return re.sub(r"[^a-z ]", "", s.lower()).split()


def match_coaches(pfr_rows, entries, label):
    """(pfr_rows in Wikipedia order with wiki_target / wiki_note added,
    Wikipedia-only entries). Exact normalised name first, then unique surname.

    Wikipedia may list a fill-in coach PFR does not credit with games (the
    1998 Falcons list Rich Brooks for weeks 16-17 while Dan Reeves recovered
    from surgery; PFR credits Reeves with all 16). Such entries are kept as
    notes; they never change n_head_coaches or the opening coach, and the
    first Wikipedia entry must be a PFR coach of record.
    """
    remaining = list(pfr_rows)
    ordered, extra = [], []
    for target, display, note in entries:
        cands = [r for r in remaining if norm_name(r["coach_name"]) in
                 (norm_name(display), norm_name(target))]
        if not cands:
            sur = norm_name(display)[-1] if norm_name(display) else None
            cands = [r for r in remaining if sur and norm_name(r["coach_name"])[-1] == sur]
        if len(cands) > 1:
            raise RuntimeError(f"{label}: Wikipedia coach '{display}' matches several PFR "
                               f"coaches {[r['coach_name'] for r in cands]}")
        if not cands:
            if not ordered:
                raise RuntimeError(f"{label}: first Wikipedia coach '{display}' is not a "
                                   f"PFR coach of record {[r['coach_name'] for r in remaining]}")
            extra.append((target, display, note))
            continue
        r = dict(cands[0], wiki_target=target, wiki_display=display, wiki_note=note,
                 wiki_order=len(ordered) + 1)
        remaining.remove(cands[0])
        ordered.append(r)
    if remaining:
        raise RuntimeError(f"{label}: PFR coaches absent from Wikipedia: "
                           f"{[r['coach_name'] for r in remaining]}")
    return ordered, extra


def note_record(note):
    """(W, L[, T]) written in an annotation ('3-9 record'), else None."""
    if not note:
        return None
    m = re.search(r"(\d+)\s*[-\u2013]\s*(\d+)(?:\s*[-\u2013]\s*(\d+))?\s*(?:record|$|\s)", note)
    return (int(m.group(1)), int(m.group(2)), int(m.group(3) or 0)) if m else None


def order_confirmed(ordered):
    """The first coach opened: its note says it left, or a later note says
    the successor joined midseason, or notes carry records matching PFR."""
    first, later = ordered[0], ordered[1:]
    if first["wiki_note"] and LEFT_RE.search(first["wiki_note"]):
        return True
    if any(r["wiki_note"] and JOINED_RE.search(r["wiki_note"]) for r in later):
        return True
    recs = [note_record(r["wiki_note"]) for r in ordered]
    return all(rec is not None and rec[:2] == (r["wins"], r["losses"])
               for rec, r in zip(recs, ordered))


# ============================================================================
# Coach demographics (explicit sources only)
# ============================================================================

def coach_pages(wiki, targets, refresh):
    """{link target: dict(title, qid, categories, missing)} via pageprops."""
    pages = fetch_pages(wiki, sorted(set(targets)), refresh, staff_mod.PAGE_PROPS)
    out = {}
    for t in targets:
        p = pages.get(t, {})
        cats = [c["title"].removeprefix("Category:") for c in p.get("categories", [])]
        out[t] = dict(title=p.get("title", t), missing=bool(p.get("missing")),
                      qid=p.get("pageprops", {}).get("wikibase_item"), categories=cats)
    return out


def category_tokens(cats):
    """{category token: [matching categories]} from the 02b category regexes."""
    hits = {}
    for flag, rx in staff_mod.CATEGORY_FLAGS.items():
        found = [c for c in cats if re.search(rx, c)
                 and ("descent" in c or not staff_mod.CATEGORY_FLAG_EXCLUDE_RE.search(c))]
        if found:
            hits[CATEGORY_TOKENS[flag]] = found
    return hits


def wikidata_tokens(wiki, qids, refresh):
    """{qid: {category token: [P172 labels]}} from non-deprecated statements."""
    if not qids:
        return {}
    raw = race_mod.p172_for_items(wiki, qids, refresh)
    if raw.empty:
        return {}
    raw["qid"] = raw["item"].map(race_mod._qid)
    raw["eg_qid"] = raw["eg"].map(race_mod._qid)
    labels = race_mod.english_labels(wiki, raw["eg_qid"].dropna().unique(), refresh)
    out = {}
    for qid, g in raw.groupby("qid"):
        toks = {}
        for eg in g["eg_qid"].dropna().unique():
            label = labels.get(eg, eg)
            mapped = race_mod.map_label(label)
            if mapped is None:
                print(f"  WARNING unmapped P172 label for {qid}: {label!r} (ignored)")
                continue
            for tok in mapped.split(";"):
                toks.setdefault(tok, []).append(label)
        out[qid] = toks
    return out


def demographic_flags(tokens):
    """(documented_black, documented_minority) from {token: evidence}; None
    when no source states anything usable. Hispanic is an ethnicity, not a
    race: a Hispanic-only statement leaves documented_black unknown (Afro-
    Latino coaches exist) but makes documented_minority 1."""
    if not tokens:
        return None, None
    stated = set(tokens)
    other_race = stated - {"black", "hispanic"}
    black = 1 if "black" in stated else (0 if other_race else None)
    minority = 1 if stated & MINORITY_CATEGORIES else (0 if "white" in stated else None)
    return black, minority


def rooney_rule_list(wiki, refresh):
    """(link targets, permalink) of the pre-rule minority head coaches named
    in the Wikipedia 'Rooney Rule' article."""
    pages = fetch_pages(wiki, [ROONEY_TITLE], refresh, CONTENT_PROPS)
    p = pages[ROONEY_TITLE]
    rev = p["revisions"][0]
    text = rev["slots"]["main"]["content"]
    m = re.search(r"by the time the rule was implemented, only (.*?) had ever held head "
                  r"coaching jobs", text, re.S)
    if not m:
        raise RuntimeError("Rooney Rule article no longer carries the pre-rule minority "
                           "head-coach sentence; review DEMOGRAPHIC_ASSUMPTION")
    targets = [t.split("|")[0].strip() for t in re.findall(r"\[\[([^\]]+)\]\]", m.group(1))]
    url = (f"https://en.wikipedia.org/w/index.php?title={p['title'].replace(' ', '_')}"
           f"&oldid={rev['revid']}")
    return targets, url


# ============================================================================
# Assemble
# ============================================================================

def build(refresh):
    session = http_session()
    wiki = Wiki()

    print("PFR year coaches pages (Internet Archive copies)")
    pfr, pfr_meta = load_pfr(session, refresh)
    print(f"  {len(pfr):,} coach x team rows, "
          f"{pfr.groupby(['season', 'franchise_id']).ngroups} team-seasons")

    keys = sorted(pfr.groupby(["franchise_id", "season"]).groups)
    print("Wikipedia team-season articles")
    wp = load_wikipedia(wiki, keys, refresh)

    # Per team-season: order, record check, opening coach
    coach_rows, team_rows, conflicts = [], [], []
    for fid, season in keys:
        g = pfr[(pfr["franchise_id"] == fid) & (pfr["season"] == season)]
        page = wp[(fid, season)]
        label = f"{season} {fid}"
        ordered, extra = match_coaches(g.to_dict("records"), page["coaches"], label)
        wins, losses, ties = (int(g["wins"].sum()), int(g["losses"].sum()),
                              int(g["ties"].sum()))
        if page["record"] is None:
            record_check = "wikipedia_record_missing"
        elif page["record"] == (wins, losses, ties):
            record_check = "match"
        else:
            record_check = "conflict"
            conflicts.append((label, page["record"], (wins, losses, ties)))
        n = len(ordered)
        if n == 1:
            assignment = "single_coach"
        elif order_confirmed(ordered):
            assignment = "multi_order_confirmed"
        else:
            assignment = "multi_order_unconfirmed"
        first = ordered[0]
        notes = [f"{r['wiki_display']}: {r['wiki_note']}" for r in ordered if r["wiki_note"]]
        notes += [f"{display}: {note or 'listed'} (Wikipedia only; not a PFR coach of record)"
                  for _, display, note in extra]
        team_rows.append(dict(
            franchise_id=fid, season=season, team_name=team_name(fid, season),
            pfr_team_slug=first["pfr_team_slug"],
            opening_coach_name=first["coach_name"], opening_coach_pfr_id=first["coach_pfr_id"],
            opening_coach_wiki_target=first["wiki_target"],
            opening_coach_games=first["games"], opening_coach_wins=first["wins"],
            opening_coach_losses=first["losses"], opening_coach_ties=first["ties"],
            wins=wins, losses=losses, ties=ties, games=wins + losses + ties,
            win_pct=(wins + 0.5 * ties) / (wins + losses + ties), n_head_coaches=n,
            head_coaches_all="; ".join(f"{r['coach_name']} ({r['wins']}-{r['losses']}-{r['ties']})"
                                       for r in ordered),
            in_season_hc_change=int(n > 1), hc_change_note="; ".join(notes) or None,
            opening_coach_assignment=assignment,
            opening_coach_uncertain=int(assignment == "multi_order_unconfirmed"),
            prior_season_observed=int((fid, season - 1) in wp),
            source_url=pfr_meta[season]["source_url"],
            source_archive_url=pfr_meta[season]["archive_url"],
            coach_source_url=page["url"], record_check=record_check))
        for r in ordered:
            coach_rows.append(dict(
                franchise_id=fid, season=season, coach_name=r["coach_name"],
                coach_pfr_id=r["coach_pfr_id"], games=r["games"], wins=r["wins"],
                losses=r["losses"], ties=r["ties"], wiki_order=r["wiki_order"],
                is_opening_coach=int(r["wiki_order"] == 1), wiki_note=r["wiki_note"],
                wiki_target=r["wiki_target"], pfr_remark=r["pfr_remark"],
                source_url=pfr_meta[season]["source_url"], coach_source_url=page["url"]))
    if conflicts:
        raise RuntimeError("Wikipedia record != PFR record: " + "; ".join(
            f"{lab} wiki {w} pfr {p}" for lab, w, p in conflicts))
    teams = pd.DataFrame(team_rows)
    coaches = pd.DataFrame(coach_rows)
    print(f"  {len(teams)} team-seasons; multi-coach "
          f"{int((teams['n_head_coaches'] > 1).sum())}, unconfirmed order "
          f"{int(teams['opening_coach_uncertain'].sum())}")

    # Coach articles, Wikidata and categories
    print("Coach articles, Wikidata P172 and Wikipedia categories")
    targets = sorted(coaches["wiki_target"].unique())
    pages = coach_pages(wiki, targets, refresh)
    missing = [t for t in targets if pages[t]["missing"]]
    if missing:
        print(f"  WARNING coach articles missing (red links): {missing}")
    qids = sorted({p["qid"] for p in pages.values() if p["qid"]})
    p172 = wikidata_tokens(wiki, qids, refresh)
    demo = {}
    for t in targets:
        p = pages[t]
        toks, basis, urls = {}, [], []
        for tok, labels in p172.get(p["qid"], {}).items():
            toks.setdefault(tok, []).extend(labels)
            basis.append(f"wikidata_P172:{'|'.join(labels)}")
            urls.append(f"https://www.wikidata.org/wiki/{p['qid']}")
        for tok, cats in category_tokens(p["categories"]).items():
            toks.setdefault(tok, []).extend(cats)
            basis.append(f"wikipedia_category:{'|'.join(cats)}")
            urls.append("https://en.wikipedia.org/wiki/" + p["title"].replace(" ", "_"))
        black, minority = demographic_flags(toks)
        demo[t] = dict(wiki_title=p["title"], wikidata_qid=p["qid"],
                       documented_black=black, documented_minority=minority,
                       demographic_source_url="; ".join(dict.fromkeys(urls)) or None,
                       demographic_basis="; ".join(basis) if basis else "no explicit source")

    # Ascertainment check against the Rooney Rule article's pre-rule list
    rooney_targets, rooney_url = rooney_rule_list(wiki, refresh)
    rooney_titles = {pages[t]["title"] if t in pages else t for t in rooney_targets}
    rooney_resolved = {p["title"] for p in fetch_pages(wiki, rooney_targets, False,
                                                       {"prop": "info"}).values()}
    rooney_titles |= rooney_resolved
    documented = {d["wiki_title"] for d in demo.values() if d["documented_minority"] == 1}
    present = {d["wiki_title"] for d in demo.values()}
    extra = documented - rooney_titles
    undocumented = (rooney_titles & present) - documented
    if extra or undocumented:
        raise RuntimeError(f"Ascertainment check failed: documented minority coaches not in "
                           f"the Rooney Rule list {sorted(extra)}; listed coaches without "
                           f"explicit documentation {sorted(undocumented)}")
    print(f"  Rooney Rule pre-rule list: {sorted(rooney_titles)}; in window: "
          f"{sorted(rooney_titles & present)}")

    for col in ("wiki_title", "wikidata_qid", "documented_black", "documented_minority",
                "demographic_source_url", "demographic_basis"):
        coaches[col] = coaches["wiki_target"].map(lambda t, c=col: demo[t][c])
    dm = teams["opening_coach_wiki_target"].map(demo)
    teams["opening_coach_wiki_title"] = dm.map(lambda d: d["wiki_title"])
    teams["opening_coach_wikidata_qid"] = dm.map(lambda d: d["wikidata_qid"])
    teams["opening_coach_documented_black"] = dm.map(lambda d: d["documented_black"])
    teams["opening_coach_documented_minority"] = dm.map(lambda d: d["documented_minority"])
    teams["demographic_source_url"] = dm.map(lambda d: d["demographic_source_url"])
    teams["demographic_basis"] = dm.map(lambda d: d["demographic_basis"]) + \
        f"; ascertainment list: {rooney_url}"
    teams["opening_coach_black_assumed"] = (teams["opening_coach_documented_black"] == 1).astype(int)
    teams["opening_coach_minority_assumed"] = (
        teams["opening_coach_documented_minority"] == 1).astype(int)
    teams["demographic_assumption"] = DEMOGRAPHIC_ASSUMPTION
    for col in ("opening_coach_documented_black", "opening_coach_documented_minority"):
        teams[col] = teams[col].astype("Int64")
    teams = teams[COLUMNS].sort_values(["season", "franchise_id"]).reset_index(drop=True)
    coaches = coaches.sort_values(["season", "franchise_id", "wiki_order"]).reset_index(drop=True)
    return teams, coaches, rooney_url


def tides_head_coaches():
    """TIDES head-coach counts by season (from the quote's '(n=k)')."""
    if not TIDES_PATH.exists():
        return pd.DataFrame(columns=["season", "tides_black_n", "tides_latino_n", "tides_white_n"])
    t = pd.read_csv(TIDES_PATH)
    t = t[(t["group"] == "head_coaches") & t["season"].between(1989, 1999)].copy()
    t["n"] = t["quote"].str.extract(r"\(n=(\d+)\)")[0].astype("Int64")
    wide = t.pivot_table(index="season", columns="category", values="n", aggfunc="first")
    wide = wide.rename(columns={"black": "tides_black_n", "latino": "tides_latino_n",
                                "white": "tides_white_n"}).reset_index()
    return wide[["season"] + [c for c in ("tides_black_n", "tides_latino_n", "tides_white_n")
                              if c in wide]]


def coverage(teams):
    g = teams.groupby("season")
    cov = pd.DataFrame({
        "n_teams": g.size(),
        "n_multi_coach": g["in_season_hc_change"].sum(),
        "n_order_unconfirmed": g["opening_coach_uncertain"].sum(),
        "n_left_censored": g["prior_season_observed"].apply(lambda s: int((s == 0).sum())),
        "n_documented_black": g["opening_coach_documented_black"].apply(lambda s: int((s == 1).sum())),
        "n_documented_minority": g["opening_coach_documented_minority"].apply(
            lambda s: int((s == 1).sum())),
        "n_documented_nonblack": g["opening_coach_documented_black"].apply(
            lambda s: int((s == 0).sum())),
        "n_demographic_unknown": g["opening_coach_documented_black"].apply(
            lambda s: int(s.isna().sum())),
    }).reset_index()
    cov = cov.merge(tides_head_coaches(), on="season", how="left")
    # Author note Oct 2026: opening-day counts equal TIDES in 1992, 1993 and
    # 1997. TIDES 1994 (2 Black + 28 white = 30 for a 28-team league) and
    # 1995 (3 Black while Dungy was hired in January 1996) are retrospective
    # rows whose reference date is not stated; they are kept for comparison,
    # not used to overrule the sources.
    cov["tides_note"] = ("TIDES 2023 RGRC Appendix II retrospective head-coach counts; "
                         "reference date within the season not stated; 1994 total (30) "
                         "exceeds the 28 teams and 1995 Black count (3) matches the "
                         "1996 opening staffs, not 1995")
    return cov


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--refresh", action="store_true")
    ap.add_argument("--no-db", action="store_true", help="build and report, no DuckDB write")
    args = ap.parse_args()

    teams, coaches, rooney_url = build(args.refresh)
    cov = coverage(teams)
    print("\nCoverage by season")
    print(cov.to_string(index=False))
    print("\nMulti-coach seasons")
    multi = teams[teams["n_head_coaches"] > 1]
    print(multi[["season", "franchise_id", "head_coaches_all", "opening_coach_assignment",
                 "hc_change_note"]].to_string(index=False))
    if args.no_db:
        return
    con = connect()
    write_table(con, "nfl_hc_history", teams,
                source="PFR year coaches pages (Internet Archive) + Wikipedia team-season "
                       "articles + Wikidata P172 / Wikipedia categories",
                note="franchise x season 1989-1998; opening coach, record, documented "
                     "demographics; see 02d_load_coach_history.py")
    write_table(con, "nfl_hc_history_coaches", coaches,
                source="PFR year coaches pages (Internet Archive) + Wikipedia",
                note="coach x franchise x season 1989-1998, Wikipedia chronological order")
    write_table(con, "nfl_hc_history_coverage", cov,
                source="nfl_hc_history + data/reference/tides_nfl_race_shares.csv",
                note="season aggregates; TIDES head-coach counts for benchmarking")
    con.close()


if __name__ == "__main__":
    main()

"""Load the full NFL coaching staff and front office from Wikipedia.

Every franchise has a navbox "Template:<Team> staff" listing its front office
(owner, president, GM, personnel and scouting directors, ...), head coach,
coordinators, every position coach and assistant, and strength & conditioning
staff, with wikilinks to person articles. The template's revision history
starts in 2007, so the revision in force on a date is the staff on that date.

Sources
  - Template revision history (STAFF_SEASONS, 2007-2025): for every
    franchise x season, the revision in force at 00:00 UTC on Sep 10
    (preseason), Nov 1 (midseason) and Dec 31 (late; or the day after the
    team's last regular-season game when that is earlier, so the snapshot is
    not taken after "Black Monday" firings). A revision that was reverted
    within 3 days (vandalism) is skipped in favour of the restored one.
  - Season articles "<season> <team name that season> season" for 1999-2006:
    their staff box ({{NFL final staff}} or a substituted table; snapshot
    season_article) and their {{Infobox NFL team season}} (snapshot
    season_infobox: every head coach of the season including interims, GM,
    owner, president and, for some seasons, the coordinators), latest
    revision. Box coverage is partial (not every article has a box); the
    boxes are written after the fact and some carry stale front-office
    entries copied from other seasons (see multi_franchise_season).
  - Person articles linked from the boxes: redirect resolution, Wikidata QID,
    non-hidden categories, and (for senior staff) article text. A link is
    kept only if the article can be the listed person: a football person
    (categories), alive in the season, and named with the shown given names
    (article intro, fetched when the given names differ). Templates also
    link namesakes (John Glenn the astronaut) and relatives (A. G. Spanos to
    Dean Spanos); those entries are treated as unlinked.
  - nflverse schedules (shared cache 'schedules'): each team's last
    regular-season game date (late snapshot) and the game-level head coach
    (head-coach reconciliation).

Tables written
  staff_snapshots            franchise x season x snapshot: revision used,
                             staleness, entry counts, parse status
  staff_entries              snapshot x listed person x role (raw + standard)
  staff_team_season          franchise x season x person x role_std with the
                             snapshots the person was listed in (analysis table)
  staff_persons              one row per person (Wikipedia title or name key)
  staff_person_wiki_signals  Wikipedia categories and race/ethnicity evidence
                             sentences (a machine signal, NOT race coding;
                             kept apart so human coders are not anchored)
  staff_hc_reconciliation    Wikipedia head coach(es) vs nflverse schedules

Wikipedia responses are cached under data/raw/wikipedia/staff/ (common.Wiki,
1 request per second), so a rebuild makes no network calls. --refresh
re-downloads them (the shared nflverse 'schedules' cache is not refreshed).
"""
import argparse
import bisect
import datetime as dt
import re
from collections import Counter, defaultdict
from urllib.parse import quote

import pandas as pd

from common import (FRANCHISES, Wiki, cached_parquet, connect, team_season_name,
                    to_franchise, write_table)
from config import NFL_SEASONS, STAFF_SEASONS
from staff_roles import COACH_GROUPS, FRONT_OFFICE_GROUPS, standardize_role
from staff_wikitext import (evidence_sentences, extract_season_infobox,
                            extract_season_staff_box, given_names_match,
                            intro_names_person, normalize_name, parse_staff_box,
                            split_name, title_to_name)

# Snapshot dates; "late" is moved earlier when the regular season ended
# before Dec 31 (see late_snapshot_targets).
SNAPSHOTS = [("preseason", 9, 10), ("midseason", 11, 1), ("late", 12, 31)]
SNAPSHOT_ORDER = {"season_infobox": 0, "season_article": 1, "preseason": 2, "midseason": 3,
                  "late": 4}
ARTICLE_SEASONS = [s for s in NFL_SEASONS if s < STAFF_SEASONS[0]]
REVERT_WINDOW = dt.timedelta(days=3)
BATCH = 50                       # titles / revids per API request
MIN_ENTRIES = 15                 # a parsed box with fewer people is flagged
LEADER_ROLES = {"GM", "ASST_GM", "FOOTBALL_OPS_EXEC"}
LEADER_GROUPS = {"head_coach", "coordinator", "owner_executive"}
PERSON_CATEGORY_RE = re.compile(r"\b\d{1,4}s? (BC )?births\b|Living people|\bdeaths\b|"
                                r"Year of birth (missing|unknown)|Possibly living people")
# A category marking a football person ("American football linebackers",
# "National Football League executives", "Chicago Bears scouts"; soccer and
# Australian rules excluded). A linked article without one is a namesake
# (an actor, singer or politician), not the staff member.
TEAM_NAMES = sorted({team_season_name(f, s) for f in FRANCHISES for s in NFL_SEASONS})
FOOTBALL_CATEGORY_RE = re.compile(r"(?<!Association )(?<!rules )football\b|\bNFL\b|"
                                  r"athletic trainers|strength and conditioning|"
                                  + "|".join(map(re.escape, TEAM_NAMES)), re.I)
# A football category naming a staff role ("Chicago Bears coaches"); used to
# resolve links that point at a disambiguation page.
STAFF_ROLE_CATEGORY_RE = re.compile(r"\b(coaches|executives|general managers|scouts|"
                                    r"owners|presidents|staff)\b")
DEATH_CATEGORY_RE = re.compile(r"(\d{3,4}) deaths$")
# Broad kind of a role group; a name is matched across franchises only
# within one kind (a strength coach is not merged into a position coach).
KIND = {"head_coach": "coach", "coordinator": "coach", "position_coach": "coach",
        "assistant_coach": "coach", "strength_conditioning": "strength",
        "support_staff": "support", **{g: "front_office" for g in FRONT_OFFICE_GROUPS}}
SNAPSHOT_KEY = ["franchise_id", "season", "snapshot"]
CATEGORY_FLAGS = {
    "cat_black": r"African[- ]American|Black American|Afro-|Black (British|Canadian)|"
                 r"of (Nigerian|Ghanaian|Liberian|Sierra Leonean|Cameroonian|Congolese|"
                 r"Ethiopian|Eritrean|Kenyan|Somali|Senegalese|Ivorian|Ugandan|Sudanese|"
                 r"Jamaican|Haitian|Barbadian|Bahamian|Trinidad and Tobago|Bermudian|"
                 r"Virgin Islands|African) descent",
    "cat_hispanic_latino": r"Hispanic|Latino|Latina|Mexican[- ]American|Puerto Rican|"
                           r"Cuban[- ]American|of (Mexican|Puerto Rican|Cuban|Dominican|"
                           r"Colombian|Salvadoran|Guatemalan|Honduran|Nicaraguan|"
                           r"Venezuelan|Peruvian|Ecuadorian|Argentine|Chilean|Panamanian|"
                           r"Costa Rican|Bolivian|Uruguayan|Paraguayan|Spanish|"
                           r"Latin American) descent",
    "cat_asian": r"Asian[- ]American|(Japanese|Korean|Chinese|Filipino|Vietnamese|"
                 r"Taiwanese|Indian|Thai|Cambodian|Laotian|Hmong|Pakistani)[- ]American|"
                 r"of (Japanese|Korean|Chinese|Filipino|Vietnamese|Taiwanese|Indian|Thai|"
                 r"Cambodian|Laotian|Hmong|Pakistani|Bangladeshi|Indonesian|Okinawan|"
                 r"Asian) descent",
    "cat_pacific_islander": r"Samoan|Tongan|Polynesian|Native Hawaiian|Pacific Islander|"
                            r"Fijian|Chamorro|Guamanian|M[aā]ori|Micronesian",
    "cat_native_american": r"Native American|American Indian|Cherokee|Navajo|Lakota|Sioux "
                           r"people|Choctaw|Chickasaw|Muscogee|Creek people|Seminole "
                           r"people|Ojibwe|Comanche|Kiowa|Osage|Lumbee|Iroquois|Mohawk|"
                           r"Oneida|Blackfeet|Hopi|Apache people|Cheyenne people|"
                           r"Alaska Native|First Nations|Indigenous peoples",
}
# Categories that name a place, school or team rather than the person's
# ancestry ("People from Oneida, New York", "Cherokee High School alumni",
# "Mississippi College Choctaws football coaches"); not flagged unless the
# category also names a descent ("People from Hawaii of Chinese descent").
CATEGORY_FLAG_EXCLUDE_RE = re.compile(
    r"^(People|Players of American football|Sportspeople) from |\bCounty\b|"
    r"\bHigh School\b|\balumni\b|"
    r"\b[A-Z][a-z]+s (football|basketball|baseball) (coaches|players)$")


# ============================================================================
# Wikipedia requests
# ============================================================================

def query_all(wiki, params, namespace, refresh):
    """Run an action=query request and follow 'continue'. Returns responses."""
    out, cont = [], {}
    while True:
        d = wiki.get({**params, **cont}, namespace=namespace, refresh=refresh)
        if "error" in d:
            raise RuntimeError(f"Wikipedia API error {d['error']} for {params}")
        out.append(d)
        if "continue" not in d:
            return out
        cont = d["continue"]


def chunks(items, n=BATCH):
    items = list(items)
    for i in range(0, len(items), n):
        yield items[i:i + n]


def template_title(fid):
    return f"Template:{FRANCHISES[fid]['name']} staff"


def fetch_history(wiki, title, refresh):
    """(resolved title, revisions oldest first) of a page, redirects followed."""
    params = {"action": "query", "prop": "revisions", "titles": title,
              "redirects": 1, "rvprop": "ids|timestamp|sha1|size",
              "rvlimit": "max", "rvdir": "newer"}
    revs, resolved = [], None
    for d in query_all(wiki, params, "staff/template_history", refresh):
        page = d["query"]["pages"][0]
        if page.get("missing") or page.get("invalid"):
            raise RuntimeError(f"Staff template not found: {title}")
        resolved = page["title"]
        revs += page.get("revisions", [])
    if not revs:
        raise RuntimeError(f"No revisions returned for {title}")
    return resolved, revs


def _ts(text):
    return dt.datetime.fromisoformat(text.replace("Z", "+00:00"))


def pick_revision(revs, target):
    """Revision in force at `target`; skips one reverted within 3 days.

    A revision counts as reverted when a later edit (within REVERT_WINDOW)
    restores the exact content (sha1) of an earlier revision.
    Returns (revision, skipped_revert) or (None, False) before the first edit.
    """
    stamps = [r["timestamp"] for r in revs]
    i = bisect.bisect_right(stamps, target.strftime("%Y-%m-%dT%H:%M:%SZ")) - 1
    if i < 0:
        return None, False
    nxt = revs[i + 1] if i + 1 < len(revs) else None
    if nxt and nxt.get("sha1"):
        for k in range(i - 1, max(-1, i - 25), -1):
            if revs[k].get("sha1") == nxt["sha1"]:
                if _ts(nxt["timestamp"]) - _ts(revs[k + 1]["timestamp"]) <= REVERT_WINDOW:
                    return revs[k], True
                break
    return revs[i], False


def fetch_revision_texts(wiki, revids, refresh):
    """{revid: wikitext} for template revisions, 50 revids per request."""
    out = {}
    for chunk in chunks(sorted(revids)):
        params = {"action": "query", "prop": "revisions",
                  "revids": "|".join(map(str, chunk)),
                  "rvprop": "ids|timestamp|content", "rvslots": "main"}
        for d in query_all(wiki, params, "staff/template_revisions", refresh):
            if d["query"].get("badrevids"):
                raise RuntimeError(f"Bad revids: {d['query']['badrevids']}")
            for page in d["query"]["pages"]:
                for r in page.get("revisions", []):
                    if "content" in r["slots"]["main"]:
                        out[r["revid"]] = r["slots"]["main"]["content"]
    missing = set(revids) - set(out)
    if missing:
        raise RuntimeError(f"No content returned for revisions {sorted(missing)}")
    return out


def fetch_pages(wiki, titles, namespace, refresh, props):
    """Query titles in batches with redirects followed.

    Returns ({requested title: resolved title or None}, {resolved: page}).
    Pages from continuation responses are merged (list items de-duplicated,
    since a continued response can repeat a page's earlier items).
    """
    resolved, pages = {}, {}
    for chunk in chunks(sorted(set(titles))):
        params = {"action": "query", "titles": "|".join(chunk), "redirects": 1, **props}
        norm, redir = {}, {}
        for d in query_all(wiki, params, namespace, refresh):
            q = d["query"]
            norm.update({x["from"]: x["to"] for x in q.get("normalized", [])})
            redir.update({x["from"]: x["to"] for x in q.get("redirects", [])})
            for p in q.get("pages", []):
                cur = pages.setdefault(p["title"], {})
                for k, v in p.items():
                    if isinstance(v, list):
                        cur[k] = cur.get(k, []) + [x for x in v if x not in cur.get(k, [])]
                    elif k not in cur:
                        cur[k] = v
        for t in chunk:
            n = norm.get(t, t)
            r = redir.get(n, n)
            page = pages.get(r)
            ok = page is not None and not page.get("missing") and not page.get("invalid")
            resolved[t] = r if ok else None
    return resolved, pages


# ============================================================================
# Snapshots and parsing
# ============================================================================

def entry_rows(parsed, meta):
    """Flatten parse_staff_box output into staff_entries rows."""
    rows = []
    entries, _, _ = parsed
    for e in entries:
        p = e["person"]
        role_raw = e["role_text"] or p["note"] or e["subsection"] or e["section"]
        role_from = ("text" if e["role_text"] else "note" if p["note"]
                     else "section_header")
        section_for_role = e["subsection"] or e["section"]
        roles = standardize_role(role_raw, section_for_role)
        # A person marked "(interim)" on a line that includes the head-coach
        # role is the interim head coach, not an interim coordinator too.
        has_hc = any(r["role_std"] == "HC" for r in roles)
        for r in roles:
            rows.append(dict(
                **meta, section=e["section"], subsection=e["subsection"],
                role_raw=role_raw, role_from=role_from, role_part=r["role_part"],
                role_std=r["role_std"], role_group=r["role_group"], unit=r["unit"],
                coach_position=r["coach_position"], is_primary_role=r["is_primary"],
                person_name=p["name"], link_target=p["link"],
                interim=bool((p["interim"] and (r["role_std"] == "HC" or not has_hc))
                             or r["interim"]), vacant=p["vacant"],
                is_entity=bool(p.get("entity")),
                name_note=p["note"] if e["role_text"] else None,
                footnote=p["footnote"], footnote_text=e["footnote_text"],
                entry_order=e["entry_order"], person_order=e["person_order"],
                line_raw=e["line_raw"]))
    return rows


def snapshot_summary(meta, parsed, rows):
    """One staff_snapshots row from a parsed box."""
    people = {(r["entry_order"], r["person_order"]) for r in rows
              if not r["vacant"] and not r["is_entity"]}
    groups = Counter(r["role_group"] for r in rows
                     if r["is_primary_role"] and not r["vacant"] and not r["is_entity"])
    n_coach = sum(v for k, v in groups.items() if k in COACH_GROUPS)
    n_fo = sum(v for k, v in groups.items() if k in FRONT_OFFICE_GROUPS)
    has_hc = any(r["role_std"] == "HC" and not r["vacant"] for r in rows)
    return dict(**meta, n_entries=len(people), n_coaches=n_coach, n_front_office=n_fo,
                n_unparsed_lines=parsed[1], has_head_coach=has_hc,
                parse_ok=len(people) >= MIN_ENTRIES and has_hc)


def load_template_snapshots(wiki, refresh, late_targets):
    """Snapshots and entries from the staff templates (2007 onward)."""
    snaps, entries = [], []
    for fid in FRANCHISES:
        title, revs = fetch_history(wiki, template_title(fid), refresh)
        picks = []
        for season in STAFF_SEASONS:
            for snap, month, day in SNAPSHOTS:
                target = dt.datetime(season, month, day, tzinfo=dt.timezone.utc)
                if snap == "late":
                    target = late_targets[(fid, season)]
                rev, skipped = pick_revision(revs, target)
                if rev is None:
                    raise RuntimeError(f"{title} has no revision before {target:%Y-%m-%d}")
                picks.append((season, snap, target, rev, skipped))
        texts = fetch_revision_texts(wiki, {p[3]["revid"] for p in picks}, refresh)
        parsed = {rid: parse_staff_box(txt) for rid, txt in texts.items()}
        for season, snap, target, rev, skipped in picks:
            meta = dict(franchise_id=fid, season=season, snapshot=snap,
                        source="staff_template")
            rows = entry_rows(parsed[rev["revid"]], meta)
            entries += rows
            rev_ts = _ts(rev["timestamp"])
            snaps.append(dict(
                **snapshot_summary(meta, parsed[rev["revid"]], rows),
                target_date=target.date(), template_title=title, revid=rev["revid"],
                revision_timestamp=rev_ts, days_stale=(target - rev_ts).days,
                reverted_revision_skipped=skipped, status="ok"))
        print(f"  {fid}: {len(revs)} revisions, {len({p[3]['revid'] for p in picks})} "
              f"used for {len(picks)} snapshots")
    return snaps, entries


def load_season_articles(wiki, refresh):
    """Snapshots and entries from pre-2007 season articles (where available)."""
    snaps, entries = [], []
    for season in ARTICLE_SEASONS:
        fids = [f for f in FRANCHISES if not (f == "HOU" and season < 2002)]
        titles = {f: f"{season} {team_season_name(f, season)} season" for f in fids}
        resolved, pages = fetch_pages(
            wiki, titles.values(), "staff/season_articles", refresh,
            {"prop": "revisions", "rvprop": "ids|timestamp|content", "rvslots": "main"})
        for fid in fids:
            meta = dict(franchise_id=fid, season=season, snapshot="season_article",
                        source="season_article")
            title = resolved[titles[fid]]
            page = pages.get(title) if title else None
            base = dict(target_date=None, template_title=title or titles[fid],
                        revid=None, revision_timestamp=None, days_stale=None,
                        reverted_revision_skipped=False)
            if not page or not page.get("revisions"):
                snaps.append(dict(**meta, **base, n_entries=0, n_coaches=0,
                                  n_front_office=0, n_unparsed_lines=0,
                                  has_head_coach=False, parse_ok=False,
                                  status="no_article"))
                continue
            rev = page["revisions"][-1]
            base.update(revid=rev["revid"], revision_timestamp=_ts(rev["timestamp"]))
            text = rev["slots"]["main"]["content"]
            # The infobox (every head coach of the season, GM, owner, and
            # sometimes the coordinators) is its own snapshot; parse_ok means
            # it names a head coach, since it lists a handful of people.
            imeta = dict(meta, snapshot="season_infobox")
            infobox = extract_season_infobox(text)
            if infobox is None:
                snaps.append(dict(**imeta, **base, n_entries=0, n_coaches=0,
                                  n_front_office=0, n_unparsed_lines=0,
                                  has_head_coach=False, parse_ok=False,
                                  status="no_infobox"))
            else:
                parsed = parse_staff_box(infobox)
                rows = entry_rows(parsed, imeta)
                entries += rows
                summary = snapshot_summary(imeta, parsed, rows)
                summary["parse_ok"] = summary["has_head_coach"]
                snaps.append(dict(**summary, **base, status="ok"))
            box = extract_season_staff_box(text)
            if box is None:
                snaps.append(dict(**meta, **base, n_entries=0, n_coaches=0,
                                  n_front_office=0, n_unparsed_lines=0,
                                  has_head_coach=False, parse_ok=False,
                                  status="no_staff_box"))
                continue
            parsed = parse_staff_box(box)
            rows = entry_rows(parsed, meta)
            entries += rows
            snaps.append(dict(**snapshot_summary(meta, parsed, rows), **base,
                              status="ok"))
    return snaps, entries


# ============================================================================
# Persons
# ============================================================================

def clean_link_target(target):
    """Link target -> API title (no #fragment, no leading colon)."""
    t = re.sub(r"#.*$", "", target or "").strip().lstrip(":").strip()
    return t.replace("_", " ") or None


def _page_info(pages):
    """{title: categories, Wikidata QID, disambiguation, is_person_article,
    is_football_person, death_year}."""
    info = {}
    for title, p in pages.items():
        cats = [c["title"].removeprefix("Category:") for c in p.get("categories", [])]
        props = p.get("pageprops", {})
        deaths = [int(m.group(1)) for c in cats if (m := DEATH_CATEGORY_RE.match(c))]
        info[title] = dict(categories=cats, qid=props.get("wikibase_item"),
                           disambiguation="disambiguation" in props,
                           is_person_article=any(PERSON_CATEGORY_RE.search(c)
                                                 for c in cats),
                           is_football_person=any(FOOTBALL_CATEGORY_RE.search(c)
                                                  for c in cats),
                           death_year=max(deaths, default=None))
    return info


PAGE_PROPS = {"prop": "pageprops|categories", "ppprop": "wikibase_item|disambiguation",
              "clshow": "!hidden", "cllimit": "max"}


def resolve_links(wiki, entries, refresh):
    """Resolve every link target; returns (target -> title map, page info,
    {(link target, franchise_id): person article}).

    A link to a disambiguation page ("[[Mike Smith (American football)]]") is
    resolved to the one listed person article with the same name that is an
    NFL staff article, preferring one categorised under the linking franchise
    ("Atlanta Falcons head coaches"); ambiguous cases stay unresolved.
    """
    targets = set(entries["link_target"].dropna().map(clean_link_target).dropna())
    resolved, pages = fetch_pages(wiki, targets, "staff/person_pages", refresh, PAGE_PROPS)
    info = _page_info(pages)
    dabs = {t for t in resolved.values() if t and info[t]["disambiguation"]}
    _, dab_pages = fetch_pages(wiki, dabs, "staff/disambiguation_links", refresh,
                               {"prop": "links", "plnamespace": 0, "pllimit": "max"})
    # Candidates: articles listed on the disambiguation page whose name is the
    # page's or the link's ("Mike Smith (American football)" redirects to the
    # "Michael Smith" page, which lists "Mike Smith (American football coach)").
    used = entries.loc[entries["link_target"].notna(), ["link_target", "franchise_id"]] \
        .drop_duplicates()
    cands = {}
    for target in used["link_target"].unique():
        d = resolved.get(clean_link_target(target))
        if d in dabs:
            keys = {normalize_name(title_to_name(d)),
                    normalize_name(title_to_name(clean_link_target(target)))}
            cands[target] = [x["title"] for x in dab_pages[d].get("links", [])
                             if normalize_name(title_to_name(x["title"])) in keys]
    _, cand_pages = fetch_pages(wiki, {c for v in cands.values() for c in v},
                                "staff/person_pages", refresh, PAGE_PROPS)
    info.update(_page_info(cand_pages))
    team_names = {f: {team_season_name(f, s) for s in NFL_SEASONS} for f in FRANCHISES}

    def staff_cats(title):
        return [c for c in info[title]["categories"]
                if STAFF_ROLE_CATEGORY_RE.search(c) and FOOTBALL_CATEGORY_RE.search(c)]

    dab_choice = {}
    for target, fid in used.itertuples(index=False):
        if target not in cands:
            continue
        staff = [c for c in cands[target] if c in info and info[c]["is_person_article"]
                 and staff_cats(c)]
        team = [c for c in staff if any(n in x for x in staff_cats(c)
                                        for n in team_names[fid])]
        pick = team if len(team) == 1 else staff
        if len(pick) == 1:
            dab_choice[(target, fid)] = pick[0]
    return resolved, info, dab_choice


def fetch_intros(wiki, entries, resolved, info, refresh):
    """{title: first sentence of the article} for person links whose shown
    given name starts with another letter than the title's ("[[Dean
    Spanos|A. G. Spanos]]", "[[Ray Ventrone|Bubba Ventrone]]"), so
    check_links can tell a nickname from a link to a relative."""
    pairs = entries[["link_target", "person_name"]].dropna().drop_duplicates()
    titles = set()
    for target, name in pairs.itertuples(index=False):
        t = resolved.get(clean_link_target(target))
        if t and info[t]["is_person_article"] and not given_names_match(name, t):
            titles.add(t)
    _, pages = fetch_pages(wiki, titles, "staff/person_intro", refresh,
                           {"prop": "extracts", "exintro": 1, "explaintext": 1,
                            "exsentences": 1, "exlimit": "max"})
    missing = titles - {t for t, p in pages.items() if p.get("extract")}
    if missing:
        raise RuntimeError(f"No intro returned for {sorted(missing)}")
    return {t: pages[t]["extract"] for t in titles}


def check_links(df, resolved, info, dab_choice, intros):
    """Add link_article (the article a link resolves to), link_status and
    wiki_title (the article accepted as the listed person) to entries.

    A link to a person article is rejected, and the entry treated as
    unlinked, when the article cannot be this staff member:
      non_football_article  no football category: a namesake actor, singer or
                            politician (lines with an owner/executive role
                            are exempt; many owners are businesspeople)
      died_before_listing   the person died before the previous season
      name_mismatch         the shown given names are not in the article's
                            name: a link to a relative ([[Dave Toub|Shane Toub]])
    """
    line = [*SNAPSHOT_KEY, "entry_order", "person_order"]
    owner_line = df["role_group"].eq("owner_executive").groupby(
        [df[c] for c in line]).transform("any")
    article, status = [], []
    for target, fid, season, name, owner in zip(df["link_target"], df["franchise_id"],
                                                 df["season"], df["person_name"],
                                                 owner_line):
        t = resolved.get(clean_link_target(target)) if target else None
        page = info.get(t) if t else None
        if page is not None and page["disambiguation"] and (target, fid) in dab_choice:
            t, s = dab_choice[(target, fid)], "disambiguation_resolved"
        elif not target:
            s = "no_link"
        elif page is None:
            s = "red_link"
        elif page["disambiguation"]:
            s = "disambiguation"
        elif not page["is_person_article"]:
            s = "non_person_article"
        elif not (page["is_football_person"] or owner):
            s = "non_football_article"
        elif page["death_year"] and season > page["death_year"] + 1:
            s = "died_before_listing"
        elif not (given_names_match(name, t) or intro_names_person(name, t, intros[t])):
            s = "name_mismatch"
        else:
            s = "person_article"
        article.append(t)
        status.append(s)
    df["link_article"] = article
    df["link_status"] = status
    df["wiki_title"] = df["link_article"].where(
        df["link_status"].isin(["person_article", "disambiguation_resolved"]))
    return df


def match_unlinked_names(df):
    """Add person_id and person_id_method; returns {name key: linked titles}.

    person_id is the accepted Wikipedia title, else 'name:<normalized name>'.
    An unlinked name reuses the title of the one linked person with the same
    normalized name ('name_matched_same_franchise' when that person is linked
    by the same franchise in any season). At another franchise
    ('name_matched_other_franchise') the roles must be of the same kind
    (coach, strength, support, front office) and the name must never be
    listed elsewhere in a season in which the linked person is listed. A name
    is never matched to the title its own link was rejected for, nor in a
    snapshot that lists that title under another name (the Steelers listed
    chairman Daniel M. Rooney and a scout named Dan Rooney).
    """
    df["name_key"] = df["person_name"].map(normalize_name)
    df["person_id_method"] = None
    df.loc[df["link_status"].eq("person_article"), "person_id_method"] = "wiki_link"
    df.loc[df["link_status"].eq("disambiguation_resolved"),
           "person_id_method"] = "wiki_link_disambiguated"
    linked = df[df["wiki_title"].notna() & ~df["is_entity"]]
    title_by_norm = defaultdict(set)
    for t, n in zip(linked["wiki_title"], linked["name_key"]):
        title_by_norm[n].add(t)
        title_by_norm[normalize_name(title_to_name(t))].add(t)
    by_title = linked.groupby("wiki_title")
    franchises_by_title = by_title["franchise_id"].agg(set)
    seasons_by_title = by_title["season"].agg(set)
    kinds_by_title = by_title["role_group"].agg(lambda g: set(g.map(KIND)))
    names_in_snapshot = linked.groupby(["wiki_title", *SNAPSHOT_KEY])["name_key"].agg(set)
    unlinked = df[df["wiki_title"].isna() & ~df["is_entity"] & ~df["vacant"]
                  & df["name_key"].notna()]
    for key, rows in unlinked.groupby("name_key"):
        cands = title_by_norm.get(key, set())
        if len(cands) != 1:
            continue
        t = next(iter(cands))
        listed_as = [names_in_snapshot.get((t, *k), {key})
                     for k in rows[SNAPSHOT_KEY].itertuples(index=False)]
        rows = rows[rows["link_article"].ne(t).to_numpy()
                    & [key in names for names in listed_as]]
        same = rows["franchise_id"].isin(franchises_by_title[t])
        other = rows[~same]
        kinds = other.groupby("franchise_id")["role_group"].agg(lambda g: set(g.map(KIND)))
        other = other[other["franchise_id"].isin(
            [f for f, k in kinds.items() if k & kinds_by_title[t]])]
        clash = (set(other["season"]) & (seasons_by_title[t] | set(rows.loc[same, "season"]))
                 or other.groupby("season")["franchise_id"].nunique().gt(1).any())
        if clash:
            other = other.iloc[0:0]          # simultaneous listings: keep apart
        df.loc[rows.index[same], "wiki_title"] = t
        df.loc[rows.index[same], "person_id_method"] = "name_matched_same_franchise"
        df.loc[other.index, "wiki_title"] = t
        df.loc[other.index, "person_id_method"] = "name_matched_other_franchise"
    is_person = ~df["vacant"] & ~df["is_entity"] & df["person_name"].notna()
    df.loc[is_person & df["wiki_title"].isna(), "person_id_method"] = "name"
    df["person_id"] = None
    df.loc[is_person, "person_id"] = [
        t if isinstance(t, str) else f"name:{k}"
        for t, k in zip(df.loc[is_person, "wiki_title"], df.loc[is_person, "name_key"])]
    return title_by_norm


def build_team_season(df):
    """Canonical franchise x season x person x role_std roster."""
    people = df[df["person_id"].notna()].copy()
    people["snap_order"] = people["snapshot"].map(SNAPSHOT_ORDER)
    order_to_snap = {v: k for k, v in SNAPSHOT_ORDER.items()}
    g = people.groupby(["franchise_id", "season", "person_id", "role_std"], sort=True)
    out = g.agg(
        source=("source", "first"),
        role_group=("role_group", "first"),
        unit=("unit", "first"),
        coach_position=("coach_position", lambda s: s.dropna().mode().iat[0]
                        if s.notna().any() else None),
        person_name=("person_name", lambda s: s.mode().iat[0]),   # as listed
        wiki_title=("wiki_title", "first"),
        role_raw=("role_raw", lambda s: s.mode().iat[0]),
        is_primary_any=("is_primary_role", "any"),
        interim_any=("interim", "any"),
        first_order=("snap_order", "min"),
        last_order=("snap_order", "max"),
        n_snapshots=("snapshot", "nunique"),
    ).reset_index()
    for snap in ("preseason", "midseason", "late", "season_article", "season_infobox"):
        seen = people.loc[people["snapshot"] == snap, ["franchise_id", "season",
                                                      "person_id", "role_std"]]
        key = set(map(tuple, seen.drop_duplicates().to_numpy()))
        out[f"in_{snap}"] = [k in key for k in map(tuple, out[["franchise_id", "season",
                                                               "person_id", "role_std"]]
                                                   .to_numpy())]
    # QA flag: one snapshot of this season lists the person at 2+ franchises
    # (a stale box, e.g. a pre-2007 season article carrying last year's front
    # office, a mislinked namesake, or a name collision).
    per_snap = people.groupby(["person_id", "season", "snapshot"])["franchise_id"].nunique()
    multi = set(per_snap[per_snap > 1].reset_index()[["person_id", "season"]]
                .itertuples(index=False, name=None))
    out["multi_franchise_season"] = [k in multi for k in
                                     zip(out["person_id"], out["season"])]
    # Canonical name: the Wikipedia title without its disambiguator when
    # linked ("[[John York|John]]" -> "John York"), else the listed name.
    linked = out["wiki_title"].notna()
    out.loc[linked, "person_name"] = out.loc[linked, "wiki_title"].map(title_to_name)
    out["first_seen"] = out.pop("first_order").map(order_to_snap)
    out["last_seen"] = out.pop("last_order").map(order_to_snap)
    return out


def build_persons(df, team_season, info, title_by_norm):
    """One row per person_id."""
    rows = []
    ts = team_season
    by_person = ts.groupby("person_id")
    names = df[df["person_id"].notna()].groupby("person_id")["person_name"]
    display = names.agg(lambda s: sorted(set(s)))
    n_matched = df[df["person_id_method"].fillna("").str.startswith("name_matched")] \
        .groupby("person_id").size()
    # Seasons in which one snapshot lists the person at 2+ franchises: a
    # Wikipedia mislink, a stale template or a name collision (QA flag).
    per_snap = df[df["person_id"].notna()].groupby(
        ["person_id", "season", "snapshot"])["franchise_id"].nunique()
    multi_fr = (per_snap[per_snap > 1].reset_index()
                .groupby("person_id")["season"].nunique())
    common_name = names.agg(lambda s: s.mode().iat[0])
    # Name keys listed by 2+ franchises in the same season (possible collision).
    unlinked = ts[ts["wiki_title"].isna()]
    multi = (unlinked.groupby(["person_id", "season"])["franchise_id"].nunique() > 1)
    multi_ids = set(multi[multi].index.get_level_values(0))
    for pid, g in by_person:
        title = g["wiki_title"].dropna().iat[0] if g["wiki_title"].notna().any() else None
        name = title_to_name(title) if title else common_name[pid]
        first, last, suffix = split_name(name)
        roles = sorted(set(g["role_std"]))
        page = info.get(title, {}) if title else {}
        key = pid.removeprefix("name:") if not title else None
        rows.append(dict(
            person_id=pid, person_name=name, first_name=first, last_name=last,
            name_suffix=suffix, display_names=display[pid], wiki_title=title,
            wiki_url=("https://en.wikipedia.org/wiki/" + quote(title.replace(" ", "_"))
                      if title else None),
            wikidata_qid=page.get("qid"),
            possible_collision=(False if title else
                                bool(title_by_norm.get(key)) or pid in multi_ids),
            n_rows_name_matched=int(n_matched.get(pid, 0)),
            n_seasons_multi_franchise=int(multi_fr.get(pid, 0)),
            first_season=int(g["season"].min()), last_season=int(g["season"].max()),
            n_team_seasons=int(g[["franchise_id", "season"]].drop_duplicates().shape[0]),
            n_franchises=int(g["franchise_id"].nunique()),
            franchises=sorted(set(g["franchise_id"])),
            roles=roles, role_groups=sorted(set(g["role_group"])),
            ever_head_coach="HC" in roles,
            ever_interim_head_coach=bool(g.loc[g["role_std"] == "HC", "interim_any"].any()),
            ever_coordinator=bool({"OC", "DC", "STC"} & set(roles)),
            ever_gm="GM" in roles,
            ever_coach=bool(set(g["role_group"]) & COACH_GROUPS),
            ever_front_office=bool(set(g["role_group"]) & FRONT_OFFICE_GROUPS)))
    return pd.DataFrame(rows)


def build_wiki_signals(wiki, persons, team_season, info, refresh):
    """Categories, category flags and evidence sentences for linked persons."""
    linked = persons[persons["wiki_title"].notna()].copy()
    ts = team_season
    leader_ids = set(ts.loc[ts["role_std"].isin(LEADER_ROLES)
                            | ts["role_group"].isin(LEADER_GROUPS), "person_id"])
    leaders = linked.loc[linked["person_id"].isin(leader_ids), "wiki_title"]
    _, pages = fetch_pages(wiki, leaders, "staff/person_text", refresh,
                           {"prop": "revisions", "rvprop": "ids|content",
                            "rvslots": "main"})
    rows = []
    for pid, title in zip(linked["person_id"], linked["wiki_title"]):
        cats = info[title]["categories"]
        row = dict(person_id=pid, wiki_title=title, categories=cats,
                   n_categories=len(cats))
        for flag, rx in CATEGORY_FLAGS.items():
            hits = [c for c in cats if re.search(rx, c)
                    and ("descent" in c or not CATEGORY_FLAG_EXCLUDE_RE.search(c))]
            row[flag] = bool(hits)
            row[f"{flag}_categories"] = hits or None
        row["is_leader"] = pid in leader_ids
        row["text_revid"], snippets = None, []
        if row["is_leader"]:
            page = pages.get(title)
            if not page or not page.get("revisions"):
                raise RuntimeError(f"No article text returned for {title}")
            rev = page["revisions"][-1]
            row["text_revid"] = rev["revid"]
            snippets = evidence_sentences(rev["slots"]["main"]["content"])
        row["n_evidence"] = len(snippets)
        for k in range(3):
            term, sent = snippets[k] if k < len(snippets) else (None, None)
            row[f"evidence_term_{k + 1}"] = term
            row[f"evidence_{k + 1}"] = sent
        rows.append(row)
    out = pd.DataFrame(rows)
    out["text_revid"] = out["text_revid"].astype("Int64")
    return out


# ============================================================================
# Head-coach reconciliation with nflverse
# ============================================================================

def schedule_team_games():
    """nflverse schedules as one row per team-game (franchise_id, coach, date)."""
    import nflreadpy as nfl
    sched = cached_parquet("schedules", lambda: nfl.load_schedules(NFL_SEASONS)).to_pandas()
    cols = ["season", "game_type", "week", "gameday"]
    games = pd.concat([
        sched[cols + ["home_team", "home_coach"]].rename(
            columns={"home_team": "team", "home_coach": "coach"}),
        sched[cols + ["away_team", "away_coach"]].rename(
            columns={"away_team": "team", "away_coach": "coach"})])
    games["franchise_id"] = games["team"].map(to_franchise)
    if games["franchise_id"].isna().any():
        raise RuntimeError("Unmapped schedule teams: "
                           f"{games.loc[games.franchise_id.isna(), 'team'].unique()}")
    return games


def late_snapshot_targets(games):
    """(franchise_id, season) -> late target: Dec 31, or 00:00 UTC on the day
    after the team's last regular-season game when that is earlier (so the
    snapshot precedes 'Black Monday' firings after a late-December finale)."""
    reg = games[games["game_type"] == "REG"]
    last = reg.groupby(["franchise_id", "season"])["gameday"].max()
    out = {}
    for (fid, season), day in last.items():
        after = dt.datetime.fromisoformat(day).replace(tzinfo=dt.timezone.utc) \
            + dt.timedelta(days=1)
        out[(fid, season)] = min(dt.datetime(season, 12, 31, tzinfo=dt.timezone.utc), after)
    return out


def nflverse_head_coaches(games):
    """franchise x season x coach: games coached (REG + POST), first/last week."""
    return (games.groupby(["franchise_id", "season", "coach"])
            .agg(n_games=("week", "size"), first_week=("week", "min"),
                 last_week=("week", "max")).reset_index())


def _names_match(a, b):
    """Normalized full-name match, or same last name and first initial."""
    if a == b:
        return True
    ta, tb = a.split(), b.split()
    strip = lambda t: [x for x in t if x not in ("jr", "sr", "ii", "iii", "iv")]
    ta, tb = strip(ta), strip(tb)
    return bool(ta and tb) and ta[-1] == tb[-1] and ta[0][0] == tb[0][0]


def build_hc_reconciliation(team_season, snapshots, games):
    """franchise x season: Wikipedia head coach(es) (any snapshot) vs the
    nflverse game-level head coach(es); names match on the normalized full
    name or on last name + first initial."""
    nv = nflverse_head_coaches(games)
    hc = team_season[team_season["role_std"] == "HC"]
    parsed = snapshots.groupby(["franchise_id", "season"])["parse_ok"].any()
    rows = []
    for (fid, season), ok in parsed.items():
        w = hc[(hc["franchise_id"] == fid) & (hc["season"] == season)]
        n = nv[(nv["franchise_id"] == fid) & (nv["season"] == season)]
        wiki = {}
        for _, r in w.iterrows():
            keys = {normalize_name(r["person_name"])}
            if isinstance(r["wiki_title"], str):
                keys.add(normalize_name(title_to_name(r["wiki_title"])))
            snaps = [s for s in ("season_infobox", "season_article", "preseason", "midseason",
                                 "late")
                     if r[f"in_{s}"]]
            wiki[r["person_name"] + (" (interim)" if r["interim_any"] else "")] = (keys, snaps)
        nfl = {r["coach"]: (normalize_name(r["coach"]), r["n_games"], r["first_week"],
                            r["last_week"]) for _, r in n.iterrows()}
        wiki_unmatched = [k for k, (keys, _) in wiki.items()
                          if not any(_names_match(x, v[0]) for x in keys for v in nfl.values())]
        nfl_unmatched = [c for c, v in nfl.items()
                         if not any(_names_match(x, v[0]) for keys, _ in wiki.values()
                                    for x in keys)]
        notes = []
        if wiki_unmatched:
            notes.append("only in Wikipedia: " + "; ".join(wiki_unmatched))
        if nfl_unmatched:
            notes.append("only in nflverse: " + "; ".join(
                f"{c} ({nfl[c][1]} games, wk {nfl[c][2]}-{nfl[c][3]})" for c in nfl_unmatched))
        rows.append(dict(
            franchise_id=fid, season=season, wiki_parse_ok=bool(ok),
            wiki_hc=" | ".join(f"{k} [{','.join(v[1])}]" for k, v in wiki.items()) or None,
            nflverse_hc=" | ".join(f"{c} ({v[1]} g)" for c, v in nfl.items()) or None,
            n_wiki_hc=len(wiki), n_nflverse_hc=len(nfl),
            match_exact=bool(wiki) and bool(nfl) and not wiki_unmatched and not nfl_unmatched,
            nflverse_all_in_wiki=bool(nfl) and not nfl_unmatched,
            wiki_all_in_nflverse=bool(wiki) and not wiki_unmatched,
            match_any=len(wiki) - len(wiki_unmatched) > 0,
            # An interim head coach nflverse does not credit with any game:
            # an in-season change its schedule-based coach field misses.
            interim_hc_only_in_wiki=any(k.endswith(" (interim)") for k in wiki_unmatched),
            notes="; ".join(notes) or None))
    return pd.DataFrame(rows)


# ============================================================================
# Main
# ============================================================================

def coverage_summary(snaps, entries, ts, persons, recon):
    """Print coverage and validation statistics."""
    print("\n=== Coverage summary ===")
    for source, label in (("staff_template", "Templates 2007-2025"),
                          ("season_article", "Season articles 1999-2006")):
        sn = snaps[snaps["source"] == source]
        ok = sn.groupby(["franchise_id", "season"])["parse_ok"].any()
        print(f"{label}: team-seasons with a parsed staff {ok.sum()} / {len(ok)}; "
              f"snapshots parse_ok {sn['parse_ok'].sum()} / {len(sn)}; "
              f"status {sn['status'].value_counts().to_dict()}")
        t = ts[ts["source"] == source]
        n_ts = t[["franchise_id", "season"]].drop_duplicates().shape[0]
        if not n_ts:
            continue
        shares = {code: t.loc[t["role_std"] == code, ["franchise_id", "season"]]
                  .drop_duplicates().shape[0] / n_ts
                  for code in ("HC", "OC", "DC", "STC", "GM")}
        pos = t[t["role_group"] == "position_coach"].groupby(
            ["franchise_id", "season"])["person_id"].nunique()
        print("  share of team-seasons with " +
              ", ".join(f"{k} {v:.3f}" for k, v in shares.items()) +
              f", >=5 position coaches {(pos >= 5).sum() / n_ts:.3f}")
    tmpl = snaps[snaps["source"] == "staff_template"]
    print(f"Reverted revisions skipped: {int(tmpl['reverted_revision_skipped'].sum())}; "
          f"median days_stale {tmpl['days_stale'].median():.0f}")
    grp = ts.assign(kind=ts["role_group"].map(
        lambda g: "coaches" if g in COACH_GROUPS else
        "front_office" if g in FRONT_OFFICE_GROUPS else "support"))
    size = grp.groupby(["season", "franchise_id", "kind"])["person_id"].nunique() \
        .unstack(fill_value=0)
    size["all"] = ts.groupby(["season", "franchise_id"])["person_id"].nunique()
    print("Median distinct persons per team-season:")
    print(size.groupby("season").median().T.to_string())
    by_group = ts[ts["source"] == "staff_template"].groupby(
        ["season", "franchise_id", "role_group"])["person_id"].nunique().unstack(fill_value=0)
    print("Median distinct persons per template team-season by role_group: " +
          ", ".join(f"{g} {v:g}" for g, v in by_group.median().sort_values(
              ascending=False).items()))
    print(f"Link status of entries: {entries['link_status'].value_counts().to_dict()}")
    prim = entries[entries["is_primary_role"] & entries["person_id"].notna()]
    print(f"Primary roles mapped to OTHER_FO: {(prim['role_std'] == 'OTHER_FO').mean():.3f}, "
          f"OTHER_COACH: {(prim['role_std'] == 'OTHER_COACH').mean():.3f}")
    print(f"Persons: {len(persons):,}; with a Wikipedia article: "
          f"{persons['wiki_title'].notna().mean():.3f}")
    r = recon[recon["wiki_parse_ok"] & recon["n_nflverse_hc"].gt(0)]
    print(f"HC reconciliation ({len(r)} team-seasons): exact {r['match_exact'].mean():.3f}, "
          f"any {r['match_any'].mean():.3f}, nflverse all in wiki "
          f"{r['nflverse_all_in_wiki'].mean():.3f}")


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--refresh", action="store_true",
                    help="re-download all Wikipedia responses")
    args = ap.parse_args()
    wiki = Wiki()

    # 1. Staff boxes: template snapshots (2007+) and season articles (1999-2006).
    print("Staff templates:")
    games = schedule_team_games()
    snaps, entries = load_template_snapshots(wiki, args.refresh,
                                             late_snapshot_targets(games))
    print("Season articles (pre-2007):")
    s2, e2 = load_season_articles(wiki, args.refresh)
    snaps = pd.DataFrame(snaps + s2)
    snaps["target_date"] = pd.to_datetime(snaps["target_date"]).dt.date
    snaps["revision_timestamp"] = pd.to_datetime(snaps["revision_timestamp"], utc=True)
    snaps["days_stale"] = snaps["days_stale"].astype("Int64")
    snaps["revid"] = snaps["revid"].astype("Int64")
    entries = pd.DataFrame(entries + e2)
    for col in ("link_target", "person_name", "role_part", "coach_position"):
        entries[col] = entries[col].astype(object).where(entries[col].notna(), None)

    # 2. People: resolve links, assign person ids, build the season roster.
    print(f"Resolving {entries['link_target'].nunique():,} link targets ...")
    resolved, info, dab_choice = resolve_links(wiki, entries, args.refresh)
    intros = fetch_intros(wiki, entries, resolved, info, args.refresh)
    entries = check_links(entries, resolved, info, dab_choice, intros)
    title_by_norm = match_unlinked_names(entries)
    team_season = build_team_season(entries)
    persons = build_persons(entries, team_season, info, title_by_norm)
    print("Fetching article text for senior staff ...")
    signals = build_wiki_signals(wiki, persons, team_season, info, args.refresh)
    recon = build_hc_reconciliation(team_season, snaps, games)

    # 3. Write tables.
    entries = entries.drop(columns=["name_key"])
    con = connect()
    src = "Wikipedia staff templates (revision history) + season articles"
    write_table(con, "staff_snapshots", snaps, source=src,
                note="franchise x season x snapshot (preseason Sep 10, midseason "
                     "Nov 1, late Dec 31 or day after last REG game; "
                     "season_article and season_infobox for 1999-2006)")
    write_table(con, "staff_entries", entries, source=src,
                note="one row per snapshot x listed person x standardized role")
    write_table(con, "staff_team_season", team_season, source=src,
                note="franchise x season x person_id x role_std; analysis roster")
    write_table(con, "staff_persons", persons, source=src + " + person articles",
                note="person_id = Wikipedia title, else 'name:<normalized name>'")
    write_table(con, "staff_person_wiki_signals", signals,
                source="Wikipedia categories and article text",
                note="machine signal for race coders' QA, NOT race coding")
    write_table(con, "staff_hc_reconciliation", recon,
                source="staff_team_season vs nflverse schedules home/away_coach",
                note="Wikipedia head coach(es) vs nflverse game-level head coach")
    con.close()
    coverage_summary(snaps, entries, team_season, persons, recon)


if __name__ == "__main__":
    main()

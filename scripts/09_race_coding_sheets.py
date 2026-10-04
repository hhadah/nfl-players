"""Human race-coding sheets and player-level Wikipedia category signals.

Purpose: build the coding universe for the race/ethnicity hand-coding protocol
(notes/race-coding-protocol.md) and write the sheets the two coders fill in.
Machine signals (Wikipedia categories, BIFSG) are deliberately kept OFF the
sheets to avoid anchoring; they stay in the DuckDB.

Universe and tiers (a person gets the lowest tier any of his roles implies):
  1 head coaches (including interim)
  2 coordinators (OC, DC, STC, pass/run-game coordinators), assistant head
    coaches, GMs / assistant GMs / football-operations executives, owners,
    chairs, presidents and CEOs
  3 all other on-field coaches (position, assistant, quality control, S&C,
    other coaching staff)
  4 all other front-office staff (personnel, scouting, administration)
  5 players with a veteran contract (UFA, RFA, ERFA, SFA, Extension,
    Franchise, Transition) signed 2011-2025
  6 players with a drafted-rookie or UDFA contract signed 2011-2026
  7 all other players on 2002-2025 season rosters
Players are the gsis_ids in nfl_players (the entity load_person_race covers).
A player who later joined a staff is coded once, as staff; his gsis_id is in
also_player_gsis_id and he has no player row. The link comes from Wikidata
(staff wikidata_qid -> P3561 "Pro Football Reference player ID", values like
"B/BradTo00"; verified on Tom Brady, Q313381 -> B/BradTo00). For staff
without a Wikidata item, a strict unique-name and timing match yields only a
candidate link shown in the context (the player row is kept).

Sources: DuckDB tables staff_persons, staff_team_season, nfl_players, nfl_ids,
nfl_rosters_season, nfl_contract_history; Wikidata SPARQL and
the Wikipedia API (cached under data/raw/wikipedia/wikidata/ and
data/raw/wikipedia/race_coding/).

Writes:
  data/hand_coded/race_coding/persons_to_code.csv   identity + context + links
  data/hand_coded/race_coding/coder_A.csv, coder_B.csv
      identity columns + blank coding columns; idempotent (existing codes are
      never overwritten or dropped; persons leaving the universe get stale=1)
  data/hand_coded/race_coding/adjudicated.csv       header only when new
  data/hand_coded/race_coding/person_links.csv      staff person -> gsis_id
  DuckDB table player_wiki_signals                  gsis_id, wiki_title,
      categories, cat_* flags (regexes from 02b_load_nfl_staff.CATEGORY_FLAGS)
"""
import argparse
import importlib
import re
import unicodedata
from pathlib import Path
from urllib.parse import quote, unquote

import pandas as pd

from common import Wiki, connect, write_table
from config import HAND_CODED_DIR

staff_mod = importlib.import_module("02b_load_nfl_staff")
CATEGORY_FLAGS = staff_mod.CATEGORY_FLAGS

OUT_DIR = HAND_CODED_DIR / "race_coding"
WIKI_NS = "race_coding"
PFR_PROPERTY = "P3561"            # Wikidata: Pro Football Reference player ID
SPARQL_BATCH = 200

IDENTITY_COLS = ["person_uid", "tier", "entity", "display_name", "context",
                 "wiki_url", "headshot_url", "pfr_url", "also_player_gsis_id"]
CODING_COLS = ["race", "multiracial_components", "hispanic", "basis",
               "source_url", "confidence", "notes"]

# Tiers by staff role group (staff_team_season.role_group)
GROUP_TIER = {"head_coach": 1, "coordinator": 2, "general_manager": 2,
              "owner_executive": 2, "position_coach": 3, "assistant_coach": 3,
              "strength_conditioning": 3, "support_staff": 3,
              "personnel_scouting": 4, "other_front_office": 4}
VETERAN_TYPES = ("UFA", "RFA", "ERFA", "SFA", "Extension", "Franchise", "Transition")
ROOKIE_TYPES = ("Drafted", "UDFA")
CONTRACT_YEARS_VETERAN = (2011, 2025)
CONTRACT_YEARS_ROOKIE = (2011, 2026)
ROSTER_SEASONS = (2002, 2025)

# Short role labels for the context string
ROLE_LABEL = {"HC": "HC", "OC": "OC", "DC": "DC", "STC": "STC",
              "ASST_HC": "Asst HC", "PASS_GAME_COORD": "Pass-game coord",
              "RUN_GAME_COORD": "Run-game coord", "GM": "GM", "ASST_GM": "Asst GM",
              "FOOTBALL_OPS_EXEC": "Football ops exec", "OWNER": "Owner",
              "PRESIDENT": "President", "CHAIR": "Chair", "CEO": "CEO",
              "VICE_CHAIR": "Vice chair", "OFF_ASST": "Off asst", "DEF_ASST": "Def asst",
              "ST_ASST": "ST asst", "QC_OFF": "Off QC", "QC_DEF": "Def QC",
              "COACH_ASST": "Coaching asst", "S_AND_C": "S&C", "OTHER_COACH": "Other coach",
              "OTHER_FO": "Front office", "CAP_ADMIN": "Cap/admin",
              "DIR_PRO_PERSONNEL": "Dir pro personnel",
              "DIR_COLLEGE_SCOUTING": "Dir college scouting",
              "DIR_PLAYER_PERSONNEL": "Dir player personnel", "SCOUT": "Scout",
              "VP_PLAYER_PERSONNEL": "VP player personnel"}
MAX_CONTEXT_ITEMS = 10


def role_label(role_std, role_group):
    if role_group == "position_coach":
        return f"{role_std.replace('_', '/')} coach"
    return ROLE_LABEL.get(role_std, role_std)


def season_spans(seasons):
    """[2007, 2008, 2009, 2012] -> '2007-2009, 2012'."""
    seasons = sorted(set(int(s) for s in seasons))
    spans, start = [], seasons[0]
    for prev, cur in zip(seasons, seasons[1:] + [None]):
        if cur != prev + 1:
            spans.append(str(start) if start == prev else f"{start}-{prev}")
            start = cur
    return ", ".join(spans)


def norm_name(name):
    """Letters only, lower case, accents and generational suffixes removed."""
    if not isinstance(name, str):
        return None
    s = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode()
    s = re.sub(r"\b(jr|sr|ii|iii|iv|v)\b\.?", "", s.lower())
    s = re.sub(r"[^a-z]", "", s)
    return s or None


def wiki_url(title):
    return None if not isinstance(title, str) else \
        "https://en.wikipedia.org/wiki/" + quote(title.replace(" ", "_"), safe="()',-._")


def pfr_url(pfr_id):
    return None if not isinstance(pfr_id, str) else \
        f"https://www.pro-football-reference.com/players/{pfr_id[0]}/{pfr_id}.htm"


# ============================================================================
# Staff universe (tiers 1-4)
# ============================================================================

def staff_universe(con):
    """One row per staff person: tier, display name, role context, links."""
    ts = con.execute("""
        SELECT person_id, franchise_id, season, role_std, role_group,
               bool_or(interim_any) AS interim
        FROM staff_team_season WHERE person_id IS NOT NULL
        GROUP BY ALL""").df()
    persons = con.execute("""
        SELECT person_id, person_name, wiki_url, wikidata_qid, first_season,
               last_season FROM staff_persons""").df()
    ts["tier"] = ts["role_group"].map(GROUP_TIER)
    if ts["tier"].isna().any():
        raise RuntimeError(f"Unmapped role groups: {set(ts.loc[ts['tier'].isna(), 'role_group'])}")

    # Context: role x franchise spans, most senior roles first, then by first
    # season; interim head-coach seasons are labelled season by season
    ts["label"] = [("Interim " if r == "HC" and pd.notna(i) and bool(i) else "")
                   + role_label(r, g)
                   for r, g, i in zip(ts["role_std"], ts["role_group"], ts["interim"])]
    items = (ts.groupby(["person_id", "label", "role_std", "franchise_id"])
               .agg(seasons=("season", list), tier=("tier", "min"),
                    first=("season", "min"))
               .reset_index()
               .sort_values(["person_id", "tier", "first", "role_std", "label", "franchise_id"]))
    items["item"] = [f"{lab} {f} {season_spans(s)}"
                     for lab, f, s in zip(items["label"], items["franchise_id"], items["seasons"])]

    def join_items(x):
        x = list(x)
        extra = len(x) - MAX_CONTEXT_ITEMS
        return "; ".join(x[:MAX_CONTEXT_ITEMS]) + (f"; +{extra} more" if extra > 0 else "")

    ctx = items.groupby("person_id").agg(context=("item", join_items),
                                         tier=("tier", "min")).reset_index()
    out = persons.merge(ctx, on="person_id", how="inner")
    out["person_uid"] = "staff:" + out["person_id"]
    out["entity"] = "staff"
    out["display_name"] = out["person_name"]
    return out


# ============================================================================
# Player universe (tiers 5-7)
# ============================================================================

def player_universe(con):
    """One row per nfl_players gsis_id in tiers 5-7, with context and links."""
    q = f"""
    WITH vet AS (
        SELECT DISTINCT gsis_id FROM nfl_contract_history
        WHERE contract_type IN {VETERAN_TYPES}
          AND year_signed BETWEEN {CONTRACT_YEARS_VETERAN[0]} AND {CONTRACT_YEARS_VETERAN[1]}),
    rook AS (
        SELECT DISTINCT gsis_id FROM nfl_contract_history
        WHERE contract_type IN {ROOKIE_TYPES}
          AND year_signed BETWEEN {CONTRACT_YEARS_ROOKIE[0]} AND {CONTRACT_YEARS_ROOKIE[1]}),
    ros AS (
        SELECT gsis_id, franchise_id, min(season) AS first, max(season) AS last
        FROM nfl_rosters_season GROUP BY ALL),
    teams AS (
        SELECT gsis_id, string_agg(franchise_id, ', ' ORDER BY first, last, franchise_id) AS teams,
               min(first) AS first_roster, max(last) AS last_roster,
               bool_or(first <= {ROSTER_SEASONS[1]} AND last >= {ROSTER_SEASONS[0]})
                   AS in_roster_window
        FROM ros GROUP BY gsis_id)
    SELECT p.gsis_id, p.display_name, p.first_name, p.common_first_name, p.last_name,
           p.pfr_id, p.headshot, p.position, p.college_name, p.draft_year,
           p.draft_round, p.draft_pick, p.draft_franchise_id, p.rookie_season,
           p.last_season, t.teams, t.first_roster, t.last_roster,
           CASE WHEN p.gsis_id IN (SELECT gsis_id FROM vet) THEN 5
                WHEN p.gsis_id IN (SELECT gsis_id FROM rook) THEN 6
                WHEN t.in_roster_window THEN 7 END AS tier
    FROM nfl_players p LEFT JOIN teams t USING (gsis_id)
    """
    df = con.execute(q).df()
    df["tier"] = df["tier"].astype("Int64")
    return df


def player_context(r):
    """'WR; Alabama; draft 2020 R1 #12 PIT; NFL 2020-2025 (PIT, NYJ)'."""
    college = r["college_name"].replace(";", " /") if isinstance(r["college_name"], str) else None
    parts = [r["position"] or "?", college or "college unknown"]
    if pd.notna(r["draft_year"]):
        rnd = f" R{int(r['draft_round'])}" if pd.notna(r["draft_round"]) else ""
        pick = f" #{int(r['draft_pick'])}" if pd.notna(r["draft_pick"]) else ""
        parts.append(f"draft {int(r['draft_year'])}{rnd}{pick} {r['draft_franchise_id'] or ''}".strip())
    else:
        parts.append("undrafted")
    first = r["rookie_season"] if pd.notna(r["rookie_season"]) else r["first_roster"]
    last = r["last_season"] if pd.notna(r["last_season"]) else r["last_roster"]
    if pd.isna(first) or pd.isna(last):
        span = "seasons unknown"
    else:
        span = str(int(first)) if first == last else f"{int(first)}-{int(last)}"
    teams = f" ({r['teams']})" if isinstance(r["teams"], str) else ""
    parts.append(f"NFL {span}{teams}")
    return "; ".join(parts)


# ============================================================================
# Wikidata lookups (cached; 1 request/second via common.Wiki)
# ============================================================================

def chunks(items, n):
    items = list(items)
    for i in range(0, len(items), n):
        yield items[i:i + n]


def sparql_rows(wiki, query):
    res = wiki.sparql(query)
    return [{k: v["value"] for k, v in b.items()} for b in res["results"]["bindings"]]


def qid_to_pfr(wiki, qids):
    """{wikidata QID: PFR player id} for items carrying P3561."""
    out = {}
    for chunk in chunks(sorted(set(qids)), SPARQL_BATCH):
        values = " ".join(f"wd:{q}" for q in chunk)
        rows = sparql_rows(wiki, f"SELECT ?item ?pfr WHERE {{ VALUES ?item {{ {values} }} "
                                 f"?item wdt:{PFR_PROPERTY} ?pfr . }}")
        for r in rows:
            out.setdefault(r["item"].rsplit("/", 1)[-1], r["pfr"].split("/")[-1])
    return out


def pfr_to_article(wiki, pfr_ids):
    """{PFR player id: English Wikipedia title} via Wikidata sitelinks."""
    out = {}
    for chunk in chunks(sorted(set(pfr_ids)), SPARQL_BATCH):
        values = " ".join(f'"{p[0].upper()}/{p}"' for p in chunk)
        rows = sparql_rows(wiki, f"SELECT ?pfr ?article WHERE {{ VALUES ?pfr {{ {values} }} "
                                 f"?item wdt:{PFR_PROPERTY} ?pfr . ?article schema:about ?item ; "
                                 f"schema:isPartOf <https://en.wikipedia.org/> . }}")
        for r in rows:
            title = r["article"].removeprefix("https://en.wikipedia.org/wiki/")
            out.setdefault(r["pfr"].split("/")[-1],
                           unquote(title).replace("_", " "))
    return out


# ============================================================================
# Staff persons who were NFL players (coded once, as staff)
# ============================================================================

def pfr_to_gsis(con):
    """{pfr_id: gsis_id} from nfl_players, then nfl_ids; ids mapping to two
    gsis_ids are dropped."""
    df = con.execute("""
        SELECT pfr_id, gsis_id FROM nfl_players WHERE pfr_id IS NOT NULL AND gsis_id <> ''
        UNION
        SELECT i.pfr_id, i.gsis_id FROM nfl_ids i JOIN nfl_players p USING (gsis_id)
        WHERE i.pfr_id IS NOT NULL""").df()
    n = df.groupby("pfr_id")["gsis_id"].transform("nunique")
    return dict(zip(df.loc[n == 1, "pfr_id"], df.loc[n == 1, "gsis_id"]))


def player_name_keys(players):
    """(gsis_id, normalized name) pairs from display, legal and common names."""
    keys = []
    for col in ("first_name", "common_first_name"):
        keys.append(pd.DataFrame({"gsis_id": players["gsis_id"],
                                  "key": (players[col].fillna("") + " "
                                          + players["last_name"].fillna("")).map(norm_name)}))
    keys.append(pd.DataFrame({"gsis_id": players["gsis_id"],
                              "key": players["display_name"].map(norm_name)}))
    return pd.concat(keys).dropna().drop_duplicates()


def staff_player_links(wiki, con, staff, players):
    """One row per staff person who played: person_id, gsis_id, link_method.

    1. wikidata: staff wikidata_qid -> P3561 -> gsis_id.
    2. name_timing (staff without a Wikidata item only): the normalized name
       equals the player's display name, is unique among nfl_players (display,
       legal and common names) and among staff_persons, and the player's last
       NFL season is 1-15 seasons before the person's first staff season.
       A review of these matches found many namesakes (e.g. Titans CEO Thomas
       Smith), so they are CANDIDATES only: the player row is kept and the
       staff context names the candidate for the coder to check.
    """
    pfr_gsis = pfr_to_gsis(con)
    with_qid = staff[staff["wikidata_qid"].notna()]
    qpfr = qid_to_pfr(wiki, with_qid["wikidata_qid"])
    wd = pd.DataFrame({"person_id": with_qid["person_id"],
                       "pfr_id": with_qid["wikidata_qid"].map(qpfr)}).dropna()
    wd["gsis_id"] = wd["pfr_id"].map(pfr_gsis)
    wd = wd.dropna(subset=["gsis_id"]).assign(link_method="wikidata")

    # Strict name + timing match for staff without a Wikidata item
    pkeys = player_name_keys(players)
    pkeys = pkeys[pkeys.groupby("key")["gsis_id"].transform("nunique") == 1]
    skeys = staff.assign(key=staff["person_name"].map(norm_name))
    skeys = skeys[skeys.groupby("key")["person_id"].transform("nunique") == 1]
    cand = (skeys.loc[skeys["wikidata_qid"].isna(), ["person_id", "key", "first_season"]]
            .merge(pkeys, on="key")
            .merge(players[["gsis_id", "display_name", "last_season"]], on="gsis_id"))
    gap = cand["first_season"] - cand["last_season"]
    keep = gap.between(1, 15) & (cand["display_name"].map(norm_name) == cand["key"])
    nm = cand.loc[keep, ["person_id", "gsis_id"]].assign(link_method="name_timing")

    links = pd.concat([wd[["person_id", "gsis_id", "link_method"]], nm])
    # A gsis_id claimed by two staff persons is ambiguous: keep neither
    links = links[links.groupby("gsis_id")["person_id"].transform("nunique") == 1]
    return links.drop_duplicates("person_id").reset_index(drop=True)


# ============================================================================
# Player-level Wikipedia category signals (screening aid; not on the sheets)
# ============================================================================

def build_player_wiki_signals(wiki, players):
    """gsis_id, wiki_title, categories and cat_* flags for players whose PFR id
    has an English Wikipedia article on Wikidata. Titles are resolved through
    redirects; categories are non-hidden only (50 titles per request)."""
    ids = players.dropna(subset=["pfr_id"])
    articles = pfr_to_article(wiki, ids["pfr_id"])
    df = pd.DataFrame({"gsis_id": ids["gsis_id"],
                       "wiki_title": ids["pfr_id"].map(articles)}).dropna()
    resolved, pages = staff_mod.fetch_pages(
        wiki, df["wiki_title"], WIKI_NS, False,
        {"prop": "categories", "clshow": "!hidden", "cllimit": "max"})
    df["wiki_title"] = df["wiki_title"].map(resolved)
    df = df.dropna(subset=["wiki_title"])
    # A title reached from two gsis_ids (Wikidata error or redirect, e.g. Ryan
    # Brown -> Richie Brown) stays only with the player whose name it carries
    shared = df["wiki_title"].duplicated(keep=False)
    names = df["gsis_id"].map(ids.set_index("gsis_id")["display_name"]).map(norm_name)
    title_names = df["wiki_title"].str.replace(r"\s*\(.*\)$", "", regex=True).map(norm_name)
    df = df[~shared | (names == title_names)]
    df = df[~df["wiki_title"].duplicated(keep=False)]
    df["categories"] = [[c["title"].removeprefix("Category:")
                         for c in pages[t].get("categories", [])] for t in df["wiki_title"]]
    df["n_categories"] = df["categories"].map(len)
    for flag, rx in CATEGORY_FLAGS.items():
        df[flag] = df["categories"].map(lambda cats: any(re.search(rx, c) for c in cats))
    return df.sort_values("gsis_id").reset_index(drop=True)


# ============================================================================
# Coding universe
# ============================================================================

def coding_universe(staff, players, links, player_titles):
    """persons_to_code rows (IDENTITY_COLS), ordered by tier then name."""
    players = players.assign(context=players.apply(player_context, axis=1),
                             pfr_url=players["pfr_id"].map(pfr_url),
                             wiki_url=players["gsis_id"].map(player_titles).map(wiki_url))
    pinfo = players.set_index("gsis_id")

    # Staff rows, with the linked player's career and links
    confirmed = links[links["link_method"] == "wikidata"].set_index("person_id")["gsis_id"]
    candidate = links[links["link_method"] == "name_timing"].set_index("person_id")["gsis_id"]
    st = staff.copy()
    st["also_player_gsis_id"] = st["person_id"].map(confirmed)
    cand = st["person_id"].map(candidate)
    st["headshot_url"] = st["also_player_gsis_id"].map(pinfo["headshot"])
    st["pfr_url"] = st["also_player_gsis_id"].map(pinfo["pfr_url"])
    pctx = st["also_player_gsis_id"].map(pinfo["context"])
    st["context"] = st["context"] + ("; NFL player: " + pctx).fillna("")
    cctx = cand.map(pinfo["display_name"]) + " (" + cand + "): " + cand.map(pinfo["context"])
    st["context"] = st["context"] + ("; POSSIBLY the NFL player " + cctx
                                     + " -- verify, note if not the same person").fillna("")

    # Player rows: tiers 5-7, minus players coded as staff
    pl = players[players["tier"].notna() & ~players["gsis_id"].isin(confirmed)].copy()
    pl["person_uid"] = "player:" + pl["gsis_id"]
    pl["entity"] = "player"
    pl["headshot_url"] = pl["headshot"]
    pl["also_player_gsis_id"] = None

    out = pd.concat([st[IDENTITY_COLS], pl[IDENTITY_COLS]], ignore_index=True)
    out["tier"] = out["tier"].astype(int)
    out["_name"] = out["display_name"].str.lower()
    out = out.sort_values(["tier", "_name", "person_uid"]).drop(columns="_name")
    if out["person_uid"].duplicated().any():
        raise RuntimeError("Duplicate person_uid in the coding universe")
    return out.reset_index(drop=True)


# ============================================================================
# Sheets (idempotent: existing codes are never overwritten or dropped)
# ============================================================================

def write_csv_atomic(df, path):
    """Write to a temporary file, then rename, so a crash never truncates a
    sheet that holds coders' work."""
    tmp = path.with_name(path.name + ".tmp")
    df.to_csv(tmp, index=False, lineterminator="\n")
    tmp.replace(path)


def read_sheet(path):
    df = pd.read_csv(path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    if "person_uid" not in df.columns:
        raise RuntimeError(f"{path} has no person_uid column")
    dup = df.loc[df["person_uid"].duplicated(), "person_uid"]
    if len(dup):
        raise RuntimeError(f"{path} repeats person_uid {sorted(set(dup))[:5]}; fix by hand")
    return df


def n_codes(df, cols):
    return int((df[[c for c in cols if c in df.columns]] != "").sum().sum())


def merge_sheet(path, universe):
    """Refresh identity columns from `universe`, append new persons, keep every
    coding value (and any extra column a coder added), flag departed persons
    stale=1. Returns (n_rows, n_new, n_stale)."""
    uni = universe.astype(object).where(universe.notna(), "").astype(str)
    uni["stale"] = "0"
    if not path.exists():
        out = uni.assign(**{c: "" for c in CODING_COLS})
        write_csv_atomic(out, path)
        return len(out), len(out), 0
    old = read_sheet(path)
    keep_cols = [c for c in old.columns if c not in IDENTITY_COLS + ["stale"]]
    for c in CODING_COLS:
        if c not in keep_cols:
            keep_cols.append(c)
            old[c] = ""
    before = n_codes(old, keep_cols)

    # Current persons: fresh identity + old codes; departed persons: old rows
    cur = uni.merge(old[["person_uid"] + keep_cols], on="person_uid", how="left")
    gone = old[~old["person_uid"].isin(uni["person_uid"])].copy()
    for c in IDENTITY_COLS:
        if c not in gone.columns:
            gone[c] = ""
    gone["stale"] = "1"
    out = pd.concat([cur, gone[cur.columns]], ignore_index=True)
    out = out[IDENTITY_COLS + ["stale"] + keep_cols].fillna("")
    if n_codes(out, keep_cols) != before:
        raise RuntimeError(f"Merging {path} would change the number of coded cells")
    write_csv_atomic(out, path)
    n_new = int((~uni["person_uid"].isin(old["person_uid"])).sum())
    return len(out), n_new, len(gone)


def init_adjudicated(path):
    """Header-only adjudication sheet; an existing file is never touched."""
    if not path.exists():
        pd.DataFrame(columns=["person_uid", "display_name", "tier"] + CODING_COLS) \
          .to_csv(path, index=False, lineterminator="\n")


# ============================================================================
# Main
# ============================================================================

def coverage_report(universe, signals):
    """Share of players with an article and share flagged, by tier."""
    pl = universe[universe["entity"] == "player"].copy()
    pl["gsis_id"] = pl["person_uid"].str.removeprefix("player:")
    pl = pl.merge(signals, on="gsis_id", how="left")
    flags = list(CATEGORY_FLAGS)
    pl["has_article"] = pl["wiki_title"].notna()
    pl[flags] = pl[flags].fillna(False).astype(bool)
    rep = pl.groupby("tier").agg(n=("person_uid", "size"),
                                 article=("has_article", "mean"),
                                 **{f: (f, "mean") for f in flags})
    print("\nPlayer Wikipedia coverage by tier (shares of all players in the tier):")
    print(rep.round(3).to_string())


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out-dir", default=str(OUT_DIR),
                        help="folder for the sheets (default data/hand_coded/race_coding)")
    args = parser.parse_args()
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    con = connect()
    wiki = Wiki()

    # Universe: staff, players, staff-player links
    staff = staff_universe(con)
    players = player_universe(con)
    links = staff_player_links(wiki, con, staff, players)
    print(f"Staff-player links: {links['link_method'].value_counts().to_dict()}")

    # Player Wikipedia signals (universe players and players linked to staff)
    in_scope = players["tier"].notna() | players["gsis_id"].isin(links["gsis_id"])
    signals = build_player_wiki_signals(wiki, players[in_scope])
    write_table(con, "player_wiki_signals", signals,
                source="Wikidata P3561 sitelinks + Wikipedia categories (clshow=!hidden)",
                note="players in race-coding tiers 5-7 or linked to a staff person")
    titles = dict(zip(signals["gsis_id"], signals["wiki_title"]))

    # Sheets
    universe = coding_universe(staff, players, links, titles)
    universe.to_csv(out_dir / "persons_to_code.csv", index=False, lineterminator="\n")
    links.to_csv(out_dir / "person_links.csv", index=False, lineterminator="\n")
    for name in ("coder_A.csv", "coder_B.csv"):
        n, new, stale = merge_sheet(out_dir / name, universe)
        print(f"{name}: {n:,} rows ({new:,} new, {stale:,} stale)")
    init_adjudicated(out_dir / "adjudicated.csv")

    # Counts
    print("\nPersons to code by tier and entity:")
    print(pd.crosstab(universe["tier"], universe["entity"], margins=True).to_string())
    print(f"Staff also coded as players (Wikidata): {universe['also_player_gsis_id'].notna().sum():,}")
    coverage_report(universe, signals)
    con.close()


if __name__ == "__main__":
    main()

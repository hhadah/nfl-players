"""Load NFL coaching staff from Pro-Football-Reference, via the Wayback Machine.

Concurrency note: we hit Wayback in parallel with a small thread pool. PFR
itself blocks scrapers but Wayback redirects to its own snapshots and is
fine with a handful of concurrent connections.

Two passes:

  1. /years/{YEAR}/coaches.htm — gives one row per team-season with the
     head coach. Use this to enumerate the (team_abbr, year) pairs that
     actually existed in a given season.
  2. /teams/{ABBR}/{YEAR}.htm — gives the team's meta block, which carries
     Head Coach (with W-L record), Offensive Coordinator, Defensive
     Coordinator, General Manager, and an "Other Notable Asst." line listing
     a handful of assistant coaches with their role in parentheses
     (e.g. "Joe Cullen (Defensive Line)").

That second pass is the broadest free historical source for NFL coaching
staff — typically 4–7 named staff per team-season. Population coverage of an
NFL coaching staff (~25 coaches per team) is not free anywhere we know of.

PFR has been blocking direct scrapes from many cloud / residential ranges,
so we go through https://web.archive.org/, which is free and well-indexed.
"""
import re
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import duckdb
import pandas as pd
import requests
from bs4 import BeautifulSoup

from config import DB_PATH, NFL_YEARS


HEADERS = {
    "User-Agent": ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                   "AppleWebKit/537.36 (KHTML, like Gecko) "
                   "Chrome/130.0.0.0 Safari/537.36"),
    "Accept": ("text/html,application/xhtml+xml,application/xml;q=0.9,"
               "image/avif,image/webp,*/*;q=0.8"),
    "Accept-Language": "en-US,en;q=0.9",
}


def upsert(con, table, df, key_cols):
    if df is None or df.empty:
        return
    table_cols = [c[0] for c in con.execute(f"DESCRIBE {table}").fetchall()]
    df = df[[c for c in table_cols if c in df.columns]]
    if df.duplicated(subset=key_cols).any():
        df = df.assign(_completeness=df.notna().sum(axis=1))
        df = (df.sort_values("_completeness", ascending=False)
                .drop_duplicates(subset=key_cols, keep="first")
                .drop(columns="_completeness"))
    con.register("staging", df)
    where = " AND ".join([f"{table}.{k} = staging.{k}" for k in key_cols])
    con.execute(f"DELETE FROM {table} WHERE EXISTS (SELECT 1 FROM staging WHERE {where})")
    cols_csv = ", ".join(df.columns)
    con.execute(f"INSERT INTO {table} ({cols_csv}) SELECT {cols_csv} FROM staging")
    con.unregister("staging")
    print(f"  -> upserted {len(df)} rows into {table}")


def fetch_year(year):
    """Fetch the PFR /coaches.htm page via the Wayback Machine.

    Wayback's /web/{ts}/{url} endpoint redirects to the closest available
    snapshot. We pass the season year as the timestamp so we land on a
    contemporaneous (or near-contemporaneous) capture."""
    url = (f"https://web.archive.org/web/{year}/"
           f"https://www.pro-football-reference.com/years/{year}/coaches.htm")
    r = requests.get(url, headers=HEADERS, timeout=60, allow_redirects=True)
    r.raise_for_status()
    return r.text


def parse_coaches(html, year):
    """Parse the coaches table. Columns: data-stat='team' (abbr) and 'coach'.
    OC/DC are NOT on this page — leave them for a separate loader."""
    soup = BeautifulSoup(html, "html.parser")
    rows = []
    table = soup.find("table", {"id": "coaches"})
    if not table or not table.find("tbody"):
        return rows
    for tr in table.find("tbody").find_all("tr"):
        if "thead" in tr.get("class", []):
            continue
        cells = {c.get("data-stat"): c.get_text(strip=True) for c in tr.find_all(["th", "td"])}
        team = cells.get("team")
        name = cells.get("coach")
        if not name or not team:
            continue
        # Strip "(interim)" etc.
        name = re.sub(r"\s*\(.*?\)", "", name).strip()
        parts = name.split(" ", 1)
        first = parts[0] if parts else ""
        last = parts[1] if len(parts) > 1 else ""
        cid = name.lower().replace(" ", "_")
        rows.append({
            "coach_id": cid,
            "first_name": first,
            "last_name": last,
            "full_name": name,
            "team": team,
            "season": year,
            "role": "HC",
        })
    return rows


# Anchor every URL to a recent timestamp so Wayback returns the closest
# available snapshot (which is usually the most recent crawl). Using the
# season year as the timestamp returned 404 for many older pages because PFR
# didn't have those URLs archived back then.
_WAYBACK_TS = "20250101"


def fetch_team_year(team_abbr, year):
    url = (f"https://web.archive.org/web/{_WAYBACK_TS}/"
           f"https://www.pro-football-reference.com/teams/"
           f"{team_abbr.lower()}/{year}.htm")
    r = requests.get(url, headers=HEADERS, timeout=30, allow_redirects=True)
    r.raise_for_status()
    return r.text


# These are the labels we look for in the meta block. The value is the
# canonical role we store under nfl_coaches.role.
SINGLE_ROLE_LABELS = {
    "Coach":                  "HC",
    "Offensive Coordinator":  "OC",
    "Defensive Coordinator":  "DC",
    "Special Teams":          "STC",
    "General Manager":        "GM",
}

# Patterns we strip from extracted names: PFR appends record info like
# "Andy Reid (11-6-0)" or asterisks for interim tags.
_NAME_NOISE = re.compile(r"\s*\([^)]*\)|\*+")


_NAME_HEAD = re.compile(
    r"\s*([A-Z][A-Za-z\.\-']+(?:\s+(?:[A-Z][A-Za-z\.\-']+|[IVX]+|Jr\.?|Sr\.?|II|III)){0,3})"
)


def _coach_row(raw, team_abbr, year, role):
    """Extract a clean 'First Last' name from a longer string.
    PFR meta blobs sometimes carry stats text after the name; we keep only
    the leading capitalized-word run."""
    cleaned = _NAME_NOISE.sub("", str(raw)).strip()
    m = _NAME_HEAD.match(cleaned)
    if not m:
        return None
    name = m.group(1).strip()
    if not name or len(name) < 3:
        return None
    parts = name.split(" ", 1)
    first = parts[0]
    last = parts[1] if len(parts) > 1 else ""
    cid = re.sub(r"\s+", "_", name.lower())
    return {
        "coach_id": cid,
        "first_name": first,
        "last_name": last,
        "full_name": name,
        "team": team_abbr,
        "season": year,
        "role": role,
    }


_LABEL_TO_ROLE = [
    ("Coach",                  "HC"),
    ("Offensive Coordinator",  "OC"),
    ("Defensive Coordinator",  "DC"),
    ("Special Teams Coordinator", "STC"),
    ("Special Teams",          "STC"),
    ("General Manager",        "GM"),
]


def _split_paragraph_label(text):
    """If `text` is "Label: value", return (Label, value); else None."""
    m = re.match(r"\s*([A-Z][A-Za-z\./\-' ]+?)\s*:\s*(.+)$", text)
    if not m:
        return None
    return m.group(1).strip(), m.group(2).strip()


def parse_team_year(html, team_abbr, year):
    """Pull HC / OC / DC / GM / STC / position-coach rows out of the meta block.

    The meta div uses one <p> per labelled field, which gives us a clean
    boundary between the value of "Coach:" and whatever stats follow."""
    soup = BeautifulSoup(html, "html.parser")
    meta = soup.find("div", id="meta")
    if not meta:
        return []
    rows = []
    for p in meta.find_all("p"):
        text = p.get_text(" ", strip=True)
        parsed = _split_paragraph_label(text)
        if not parsed:
            continue
        label, value = parsed

        # Notable assistants have their own per-paragraph format with multiple
        # "Name (Position)" segments — even if the value bleeds into other
        # labels on the same line.
        if label.lower().startswith("other notable asst"):
            # Cut off the trailing run that starts at the next known label.
            cut = re.search(r"\b(Stadium|Chairman|General Manager|Owner|"
                            r"Offensive Scheme|Defensive Scheme|Training Camp|"
                            r"Preseason Odds|Defensive Alignment|Offensive Alignment)\s*:",
                            value)
            asst_value = value[:cut.start()] if cut else value
            for am in re.finditer(
                r"([A-Z][A-Za-z\.\-']+(?:\s+[A-Z][A-Za-z\.\-']+){0,3})"
                r"\s*\(([^)]+)\)", asst_value
            ):
                name, position = am.group(1), am.group(2).strip()
                row = _coach_row(name, team_abbr, year, f"ASST:{position}")
                if row:
                    rows.append(row)
            continue

        # Map exact label to role.
        role = None
        for needle, r in _LABEL_TO_ROLE:
            if label.lower() == needle.lower():
                role = r
                break
        if role is None:
            continue
        row = _coach_row(value, team_abbr, year, role)
        if row:
            rows.append(row)
    return rows


# nflverse abbreviations → PFR 3-letter abbreviations used in URL paths.
_NFLVERSE_TO_PFR = {
    "GB": "GNB", "KC": "KAN", "NE": "NWE", "NO": "NOR",
    "SF": "SFO", "TB": "TAM", "SD": "SDG", "LV": "LVR",
    "LA": "LAR",
    # nfl_data_py occasionally returns alternate codes — map back to PFR.
    "BLT": "BAL", "HST": "HOU", "CLV": "CLE", "ARZ": "ARI", "SL": "STL",
    "STL": "STL", "OAK": "OAK", "SD": "SDG",
}


def _team_year_pairs(con):
    """Every (team_abbr_in_PFR_form, season) we know of, from nfl_team_stats.
    Skips the synthetic week=0 totals so each pair is real."""
    df = con.execute("""
        SELECT DISTINCT team, season
        FROM nfl_team_stats
        WHERE week BETWEEN 1 AND 22
        ORDER BY season, team
    """).df()
    pairs = []
    for _, row in df.iterrows():
        nflv_abbr = row["team"]
        pfr = _NFLVERSE_TO_PFR.get(nflv_abbr, nflv_abbr)
        pairs.append((pfr, int(row["season"])))
    return pairs


def _scrape_one(team, year):
    try:
        html = fetch_team_year(team, year)
        rows = parse_team_year(html, team, year)
        return team, year, rows, None
    except Exception as e:
        return team, year, [], str(e)


def main():
    con = duckdb.connect(str(DB_PATH))

    # Enumerate (team, year) directly from nfl_team_stats — much more reliable
    # than scraping PFR's /years/{YEAR}/coaches.htm index, which Wayback often
    # times out on.
    pairs = _team_year_pairs(con)
    n = len(pairs)
    print(f"Scraping {n} team-season pages from PFR (via Wayback)...")

    staff_rows = []
    failures = 0
    completed = 0
    # 3 workers stays under Wayback's burst threshold; raising it triggers
    # a "Connection refused" lockout that takes ~10 min to clear.
    with ThreadPoolExecutor(max_workers=3) as pool:
        futures = {pool.submit(_scrape_one, t, y): (t, y) for t, y in pairs}
        for fut in as_completed(futures):
            team, year, rows, err = fut.result()
            completed += 1
            if err:
                failures += 1
                if failures <= 10 or completed % 50 == 0:
                    print(f"  [{completed:>3}/{n}] {team} {year} failed: {err}",
                          flush=True)
            else:
                staff_rows.extend(rows)
                if completed % 25 == 0 or completed == n:
                    roles = sorted({r["role"].split(":")[0] for r in rows})
                    print(f"  [{completed:>3}/{n}] {team} {year}: "
                          f"{len(rows)} staff ({','.join(roles)})", flush=True)

    print(f"  done: {completed - failures}/{n} pages succeeded")

    df = pd.DataFrame(staff_rows)
    if not df.empty:
        df = df.drop_duplicates(subset=["coach_id", "team", "season", "role"])
    upsert(con, "nfl_coaches", df, ["coach_id", "team", "season", "role"])
    con.close()
    print(f"\nNFL coaches load complete. {len(df)} total staff records.")


if __name__ == "__main__":
    main()

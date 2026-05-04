"""Load NFL coaches by scraping Pro-Football-Reference.

PFR has a coaches index at /years/{YEAR}/coaches.htm with HC/OC/DC for each team.
Free, no API key, but be polite — they ask for ~20 req/min max.
"""
import time
import re
import duckdb
import pandas as pd
import requests
from bs4 import BeautifulSoup
from config import DB_PATH, NFL_YEARS


HEADERS = {
    "User-Agent": "Mozilla/5.0 (research project; respectful scraping)"
}


def upsert(con, table, df, key_cols):
    if df is None or df.empty:
        return
    table_cols = [c[0] for c in con.execute(f"DESCRIBE {table}").fetchall()]
    df = df[[c for c in table_cols if c in df.columns]]
    con.register("staging", df)
    where = " AND ".join([f"{table}.{k} = staging.{k}" for k in key_cols])
    con.execute(f"DELETE FROM {table} WHERE EXISTS (SELECT 1 FROM staging WHERE {where})")
    con.execute(f"INSERT INTO {table} SELECT * FROM staging")
    con.unregister("staging")
    print(f"  -> upserted {len(df)} rows into {table}")


def fetch_year(year):
    url = f"https://www.pro-football-reference.com/years/{year}/coaches.htm"
    r = requests.get(url, headers=HEADERS, timeout=30)
    r.raise_for_status()
    return r.text


def parse_coaches(html, year):
    """Parse the coaches table. Columns include team, head coach, OC, DC."""
    soup = BeautifulSoup(html, "html.parser")
    rows = []
    table = soup.find("table", {"id": "coaches"})
    if not table:
        return rows
    for tr in table.find("tbody").find_all("tr"):
        if "thead" in tr.get("class", []):
            continue
        cells = {c.get("data-stat"): c.get_text(strip=True) for c in tr.find_all(["th", "td"])}
        team = cells.get("team") or cells.get("team_name")
        for stat_key, role in [("coach", "HC"),
                               ("off_coord", "OC"),
                               ("def_coord", "DC")]:
            name = cells.get(stat_key)
            if not name:
                continue
            # Strip "(interim)" etc
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
                "role": role,
            })
    return rows


def main():
    con = duckdb.connect(str(DB_PATH))
    all_rows = []
    for year in NFL_YEARS:
        try:
            html = fetch_year(year)
            year_rows = parse_coaches(html, year)
            print(f"  {year}: {len(year_rows)} coach records")
            all_rows.extend(year_rows)
        except Exception as e:
            print(f"  {year} failed: {e}")
        time.sleep(3.5)        # ~17 req/min, well under PFR's limit

    df = pd.DataFrame(all_rows)
    upsert(con, "nfl_coaches", df, ["coach_id", "team", "season", "role"])
    con.close()
    print("NFL coaches load complete.")


if __name__ == "__main__":
    main()

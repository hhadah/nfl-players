"""Load recruiting / high school data from the CollegeFootballData API.

CFBD aggregates the 247Sports composite, which includes:
  - HS school name, hometown
  - star rating (2-5)
  - composite rating (numeric)
  - national / position / state ranking
  - height, weight at recruitment
  - committed school

This is your best free source for the HS layer. CFBD does NOT have HS game stats —
for those you'd need to scrape MaxPreps or similar (much harder, often blocked).
"""
import time
import duckdb
import pandas as pd
import cfbd
from cfbd.rest import ApiException
from config import DB_PATH, RECRUIT_YEARS, CFBD_API_KEY


def get_client():
    if not CFBD_API_KEY:
        raise RuntimeError("Set CFBD_API_KEY env var")
    config = cfbd.Configuration(
        host="https://api.collegefootballdata.com",
        access_token=CFBD_API_KEY,
    )
    return cfbd.ApiClient(config)


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


def load_recruits(con, client):
    print("Loading recruiting data (247 composite)...")
    api = cfbd.RecruitingApi(client)
    rows = []
    for year in RECRUIT_YEARS:
        try:
            recruits = api.get_recruits(year=year)
            print(f"  {year}: {len(recruits)} recruits")
        except ApiException as e:
            print(f"  {year} failed: {e}")
            continue
        for r in recruits:
            # Build a stable ID. CFBD doesn't always provide one, so synthesize.
            rid = f"{year}_{(r.name or '').replace(' ', '_')}_{r.committed_to or 'uncommitted'}"
            rows.append({
                "recruit_id": rid,
                "year": year,
                "name": r.name,
                "first_name": r.first_name if hasattr(r, "first_name") else None,
                "last_name": r.last_name if hasattr(r, "last_name") else None,
                "position": r.position,
                "height_inches": r.height,
                "weight_lbs": r.weight,
                "stars": r.stars,
                "rating": r.rating,
                "ranking": r.ranking,
                "position_ranking": getattr(r, "position_ranking", None),
                "state_ranking": getattr(r, "state_ranking", None),
                "committed_to": r.committed_to,
                "high_school": r.school,           # the HS the recruit attended
                "hometown_city": r.city,
                "hometown_state": r.state_province,
                "hometown_country": r.country,
            })
        time.sleep(0.3)
    df = pd.DataFrame(rows)
    upsert(con, "recruits", df, ["recruit_id"])


def main():
    con = duckdb.connect(str(DB_PATH))
    client = get_client()
    load_recruits(con, client)
    con.close()
    print("Recruiting load complete.")


if __name__ == "__main__":
    main()

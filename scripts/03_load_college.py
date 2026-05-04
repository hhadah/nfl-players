"""Load college football data from the CollegeFootballData API.

Pulls: teams, rosters, player season stats, team season stats (incl. SP+).
Requires CFBD_API_KEY in env. Get one free at https://collegefootballdata.com/key
"""
import json
import time
import duckdb
import pandas as pd
import cfbd
from cfbd.rest import ApiException
from config import DB_PATH, COLLEGE_YEARS, CFBD_API_KEY


def get_client():
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


def load_teams(con, client):
    print("Loading college teams...")
    api = cfbd.TeamsApi(client)
    teams = api.get_teams()
    rows = []
    for t in teams:
        rows.append({
            "school": t.school,
            "mascot": t.mascot,
            "abbreviation": t.abbreviation,
            "conference": t.conference,
            "division": t.division,
            "classification": t.classification,
        })
    upsert(con, "college_teams", pd.DataFrame(rows), ["school"])


def load_rosters(con, client):
    print("Loading college rosters...")
    api = cfbd.TeamsApi(client)
    all_players = {}
    for year in COLLEGE_YEARS:
        try:
            roster = api.get_roster(year=year)
            print(f"  {year}: {len(roster)} players")
        except ApiException as e:
            print(f"  {year} failed: {e}")
            continue
        for p in roster:
            pid = str(p.id) if p.id is not None else f"{p.first_name}_{p.last_name}_{p.team}"
            existing = all_players.get(pid, {"seasons": []})
            existing["seasons"].append(year)
            all_players[pid] = {
                "cfbd_id": pid,
                "first_name": p.first_name,
                "last_name": p.last_name,
                "full_name": f"{p.first_name or ''} {p.last_name or ''}".strip(),
                "team": p.team,
                "position": p.position,
                "height_inches": p.height,
                "weight_lbs": p.weight,
                "jersey": p.jersey,
                "home_city": p.home_city,
                "home_state": p.home_state,
                "home_country": p.home_country,
                "seasons": existing["seasons"],
            }
        time.sleep(0.3)        # be polite
    df = pd.DataFrame(all_players.values())
    if not df.empty:
        df["seasons_played"] = df["seasons"].apply(json.dumps)
        df = df.drop(columns=["seasons"])
    upsert(con, "college_players", df, ["cfbd_id"])


def load_player_stats(con, client):
    """Player season stats are returned per category/stat. We flatten them."""
    print("Loading college player season stats...")
    api = cfbd.StatsApi(client)
    rows = []
    for year in COLLEGE_YEARS:
        try:
            stats = api.get_player_season_stats(year=year)
            print(f"  {year}: {len(stats)} stat rows")
        except ApiException as e:
            print(f"  {year} failed: {e}")
            continue
        for s in stats:
            rows.append({
                "cfbd_id": str(s.player_id) if s.player_id else f"{s.player}_{s.team}",
                "season": year,
                "team": s.team,
                "category": s.category,
                "stat_type": s.stat_type,
                "stat_value": float(s.stat) if s.stat is not None else None,
            })
        time.sleep(0.3)
    df = pd.DataFrame(rows)
    upsert(con, "college_player_stats", df,
           ["cfbd_id", "season", "team", "category", "stat_type"])


def load_team_stats(con, client):
    print("Loading college team season stats (records + SP+)...")
    rec_api = cfbd.GamesApi(client)
    ratings_api = cfbd.RatingsApi(client)

    rows = {}
    for year in COLLEGE_YEARS:
        # Records
        try:
            recs = rec_api.get_records(year=year)
            for r in recs:
                key = (r.team, year)
                rows[key] = rows.get(key, {"school": r.team, "season": year})
                rows[key]["wins"] = r.total.wins if r.total else None
                rows[key]["losses"] = r.total.losses if r.total else None
        except ApiException as e:
            print(f"  records {year} failed: {e}")

        # SP+ ratings
        try:
            sp = ratings_api.get_sp(year=year)
            for s in sp:
                key = (s.team, year)
                rows[key] = rows.get(key, {"school": s.team, "season": year})
                rows[key]["sp_plus_rating"] = s.rating
        except ApiException as e:
            print(f"  SP+ {year} failed: {e}")

        # SRS
        try:
            srs = ratings_api.get_srs(year=year)
            for s in srs:
                key = (s.team, year)
                rows[key] = rows.get(key, {"school": s.team, "season": year})
                rows[key]["srs"] = s.rating
        except ApiException as e:
            print(f"  SRS {year} failed: {e}")

        time.sleep(0.5)

    df = pd.DataFrame(rows.values())
    upsert(con, "college_team_stats", df, ["school", "season"])


def load_coaches(con, client):
    """College coaches: one record per coach per season per school.
    A coach who moves schools shows up in multiple rows."""
    print("Loading college coaches...")
    api = cfbd.CoachesApi(client)
    rows = []
    for year in COLLEGE_YEARS:
        try:
            coaches = api.get_coaches(year=year)
            print(f"  {year}: {len(coaches)} coach-school records")
        except ApiException as e:
            print(f"  {year} failed: {e}")
            continue
        for c in coaches:
            full = f"{c.first_name or ''} {c.last_name or ''}".strip()
            cid = full.lower().replace(" ", "_")
            for season_rec in (c.seasons or []):
                if season_rec.year != year:
                    continue        # only the year we asked for
                rows.append({
                    "coach_id": cid,
                    "first_name": c.first_name,
                    "last_name": c.last_name,
                    "full_name": full,
                    "school": season_rec.school,
                    "season": season_rec.year,
                    "games": season_rec.games,
                    "wins": season_rec.wins,
                    "losses": season_rec.losses,
                    "ties": season_rec.ties,
                    "preseason_rank": season_rec.preseason_rank,
                    "postseason_rank": season_rec.postseason_rank,
                    "srs": season_rec.srs,
                    "sp_overall": season_rec.sp_overall,
                    "sp_offense": season_rec.sp_offense,
                    "sp_defense": season_rec.sp_defense,
                })
        time.sleep(0.3)
    df = pd.DataFrame(rows)
    upsert(con, "college_coaches", df, ["coach_id", "school", "season"])


def main():
    if not CFBD_API_KEY:
        print("CFBD_API_KEY not set — skipping college load. "
              "Get a free key at https://collegefootballdata.com/key, then "
              "`export CFBD_API_KEY=...` and re-run.")
        return
    con = duckdb.connect(str(DB_PATH))
    client = get_client()
    load_teams(con, client)
    load_rosters(con, client)
    load_player_stats(con, client)
    load_team_stats(con, client)
    load_coaches(con, client)
    con.close()
    print("College load complete.")


if __name__ == "__main__":
    main()

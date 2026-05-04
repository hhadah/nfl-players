"""Load NFL data from nflverse via nfl_data_py.

Pulls: rosters, weekly stats, seasonal stats, combine, NextGen Stats, schedules.
"""
import duckdb
import pandas as pd
import nfl_data_py as nfl
from config import DB_PATH, NFL_YEARS, INCLUDE_NEXTGEN, INCLUDE_WEEKLY_NFL


def upsert(con, table, df, key_cols):
    """Insert or replace rows. DuckDB doesn't have upsert syntax for all cases,
    so we delete-then-insert by key. For first run this is just an insert."""
    if df.empty:
        return
    # Match column order to table
    table_cols = [c[0] for c in con.execute(f"DESCRIBE {table}").fetchall()]
    df = df[[c for c in table_cols if c in df.columns]]
    # Dedupe on PK — source data occasionally has duplicate keys (e.g. one
    # populated row + one all-zero row). Keep the row with the most non-null
    # values.
    if df.duplicated(subset=key_cols).any():
        df = df.assign(_completeness=df.notna().sum(axis=1))
        df = (df.sort_values("_completeness", ascending=False)
                .drop_duplicates(subset=key_cols, keep="first")
                .drop(columns="_completeness"))
    # Register and merge
    con.register("staging", df)
    where = " AND ".join([f"{table}.{k} = staging.{k}" for k in key_cols])
    con.execute(f"DELETE FROM {table} WHERE EXISTS (SELECT 1 FROM staging WHERE {where})")
    cols_csv = ", ".join(df.columns)
    con.execute(f"INSERT INTO {table} ({cols_csv}) SELECT {cols_csv} FROM staging")
    con.unregister("staging")
    print(f"  -> upserted {len(df)} rows into {table}")


def load_rosters(con):
    print("Loading NFL rosters...")
    rosters = nfl.import_seasonal_rosters(NFL_YEARS)
    # Aggregate to one row per player (latest season info)
    rosters = rosters.sort_values("season")
    players = rosters.groupby("player_id", as_index=False).agg({
        "pfr_id": "last", "espn_id": "last", "sleeper_id": "last",
        "player_name": "last", "first_name": "last", "last_name": "last",
        "birth_date": "last", "height": "last", "weight": "last",
        "position": "last", "college": "last",
        "draft_number": "last", "season": "max",
        "rookie_year": "min", "status": "last",
    })
    players = players.rename(columns={
        "player_id": "gsis_id",
        "player_name": "full_name",
        "height": "height_inches",
        "weight": "weight_lbs",
        "season": "last_season",
        "draft_number": "draft_pick",
    })
    players["birth_date"] = pd.to_datetime(players["birth_date"], errors="coerce").dt.date
    # Draft round/team/year aren't in seasonal_rosters; pull from drafts
    drafts = nfl.import_draft_picks(NFL_YEARS)
    drafts = drafts[["gsis_id", "season", "round", "pick", "team"]].rename(columns={
        "season": "draft_year", "round": "draft_round",
        "pick": "draft_pick_d", "team": "draft_team",
    })
    players = players.merge(drafts, on="gsis_id", how="left")
    players["draft_pick"] = players["draft_pick_d"].fillna(players["draft_pick"])
    players = players.drop(columns=["draft_pick_d"])
    upsert(con, "nfl_players", players, ["gsis_id"])


def load_weekly_rosters(con):
    """Active roster snapshot per (player, team, season, week).

    This is the panel base — it includes every player on the active roster
    each week, regardless of whether they recorded any stats. Without it the
    panel collapses to skill-position players only (~80k rows / 15 seasons
    instead of ~600k)."""
    print("Loading NFL weekly rosters...")
    # nfl_data_py 0.3.x crashes on multi-year input due to a buggy age
    # reindex; pull season-by-season as a workaround.
    frames = []
    for y in NFL_YEARS:
        try:
            frames.append(nfl.import_weekly_rosters([y]))
            print(f"  {y}: ok")
        except Exception as e:
            print(f"  {y} failed: {e}")
    rosters = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    if rosters.empty:
        print("  no weekly rosters loaded")
        return
    rosters = rosters.rename(columns={
        "player_id": "gsis_id",
        "game_type": "season_type",
        "jersey_number": "jersey_number",
    })
    rosters["season_type"] = rosters["season_type"].fillna("REG")
    # Source distinguishes WC/CON/DIV/SB; nfl_player_stats only knows REG/POST.
    # Collapse postseason rounds so the panel can LEFT JOIN cleanly.
    rosters.loc[~rosters["season_type"].isin(["REG", "POST"]), "season_type"] = "POST"
    # jersey_number occasionally arrives as e.g. '36D' (legacy practice-squad
    # designation). Coerce to nullable Int.
    if "jersey_number" in rosters.columns:
        rosters["jersey_number"] = pd.to_numeric(
            rosters["jersey_number"], errors="coerce"
        ).astype("Int64")
    # Drop rows missing any PK component (a few stragglers each season)
    pk = ["gsis_id", "season", "week", "season_type", "team"]
    rosters = rosters.dropna(subset=pk)
    upsert(con, "nfl_weekly_rosters", rosters, pk)


def load_weekly_stats(con):
    if not INCLUDE_WEEKLY_NFL:
        return
    print("Loading NFL weekly player stats...")
    weekly = nfl.import_weekly_data(NFL_YEARS)
    weekly = weekly.rename(columns={"player_id": "gsis_id", "recent_team": "team"})
    weekly["season_type"] = weekly["season_type"].fillna("REG")
    upsert(con, "nfl_player_stats", weekly, ["gsis_id", "season", "week", "season_type"])


def load_seasonal_stats(con):
    print("Loading NFL seasonal player stats...")
    seasonal = nfl.import_seasonal_data(NFL_YEARS)
    seasonal = seasonal.rename(columns={"player_id": "gsis_id"})
    seasonal["week"] = 0      # 0 = season total
    seasonal["season_type"] = "REG"
    # Need team and position from rosters
    rosters = nfl.import_seasonal_rosters(NFL_YEARS)[["player_id", "season", "team", "position"]]
    rosters = rosters.rename(columns={"player_id": "gsis_id"})
    seasonal = seasonal.merge(rosters, on=["gsis_id", "season"], how="left")
    upsert(con, "nfl_player_stats", seasonal, ["gsis_id", "season", "week", "season_type"])


def load_combine(con):
    print("Loading NFL combine data...")
    combine = nfl.import_combine_data(NFL_YEARS)
    # Source already has `season`; `draft_ovr` maps to schema's `draft_pick`.
    combine = combine.rename(columns={"draft_ovr": "draft_pick"})
    # `ht` arrives as "6-3" (feet-inches) — convert to total inches as DOUBLE.
    def _ht_to_inches(v):
        if not isinstance(v, str) or "-" not in v:
            return None
        try:
            ft, inch = v.split("-", 1)
            return float(ft) * 12 + float(inch)
        except Exception:
            return None
    combine["ht"] = combine["ht"].map(_ht_to_inches)
    upsert(con, "nfl_combine", combine, ["season", "player_name", "pos"])


def load_nextgen(con):
    if not INCLUDE_NEXTGEN:
        return
    print("Loading NextGen Stats...")
    frames = []
    for stat_type in ["passing", "rushing", "receiving"]:
        try:
            ng = nfl.import_ngs_data(stat_type, [y for y in NFL_YEARS if y >= 2016])
            ng["stat_type"] = stat_type
            ng = ng.rename(columns={"player_gsis_id": "gsis_id"})
            frames.append(ng)
        except Exception as e:
            print(f"  NextGen {stat_type} failed: {e}")
    if frames:
        combined = pd.concat(frames, ignore_index=True)
        # Replace NaN week with 0 for season totals
        combined["week"] = combined["week"].fillna(0).astype(int)
        upsert(con, "nfl_nextgen_stats", combined,
               ["gsis_id", "season", "week", "stat_type"])


def load_team_stats(con):
    print("Loading NFL team-level stats from schedules...")
    schedules = nfl.import_schedules(NFL_YEARS)
    # Build per-team-per-week records
    home = schedules[["season", "week", "game_type", "home_team", "home_score",
                      "away_score", "away_team"]].copy()
    home.columns = ["season", "week", "season_type", "team", "points_for",
                    "points_against", "opponent"]
    away = schedules[["season", "week", "game_type", "away_team", "away_score",
                      "home_score", "home_team"]].copy()
    away.columns = ["season", "week", "season_type", "team", "points_for",
                    "points_against", "opponent"]
    games = pd.concat([home, away], ignore_index=True)
    games["wins"] = (games["points_for"] > games["points_against"]).astype(int)
    games["losses"] = (games["points_for"] < games["points_against"]).astype(int)
    games["ties"] = (games["points_for"] == games["points_against"]).astype(int)
    games["season_type"] = games["season_type"].map(
        lambda x: "REG" if x == "REG" else "POST"
    )
    upsert(con, "nfl_team_stats", games, ["team", "season", "week", "season_type"])

    # Season totals (week=0)
    season_totals = games.groupby(["team", "season", "season_type"], as_index=False).agg(
        points_for=("points_for", "sum"),
        points_against=("points_against", "sum"),
        wins=("wins", "sum"),
        losses=("losses", "sum"),
        ties=("ties", "sum"),
    )
    season_totals["week"] = 0
    upsert(con, "nfl_team_stats", season_totals,
           ["team", "season", "week", "season_type"])


def load_weekly_coaches(con):
    """One row per (team, season, week, season_type) carrying the head coach
    on duty for that game. Source: nflverse schedules, which already has
    home_coach / away_coach for every game.

    This is what makes mid-season HC changes detectable downstream — the
    nfl_coaches table is season-level, but this one is per game."""
    print("Loading NFL weekly head coaches from schedules...")
    sched = nfl.import_schedules(NFL_YEARS)
    keep = ["season", "week", "game_type", "home_team", "home_coach",
            "away_team", "away_coach"]
    sched = sched[[c for c in keep if c in sched.columns]].copy()

    home = sched.rename(columns={"home_team": "team", "home_coach": "full_name"})
    home = home[["team", "season", "week", "game_type", "full_name"]]
    away = sched.rename(columns={"away_team": "team", "away_coach": "full_name"})
    away = away[["team", "season", "week", "game_type", "full_name"]]
    rows = pd.concat([home, away], ignore_index=True)

    rows = rows.dropna(subset=["full_name", "team", "season", "week"])
    rows["season_type"] = rows["game_type"].map(
        lambda g: "REG" if g == "REG" else "POST"
    )
    rows = rows.drop(columns=["game_type"])

    # Split full name → first / last; build a stable coach_id.
    parts = rows["full_name"].str.strip().str.split(" ", n=1, expand=True)
    rows["first_name"] = parts[0]
    rows["last_name"] = parts[1].fillna("")
    rows["coach_id"] = (rows["full_name"].str.lower()
                                          .str.replace(r"\s+", "_", regex=True))

    upsert(con, "nfl_weekly_coaches", rows,
           ["team", "season", "week", "season_type"])


def load_team_descriptions(con):
    print("Loading NFL team descriptions...")
    teams = nfl.import_team_desc()
    teams = teams.rename(columns={
        "team_abbr": "team_abbr",
        "team_name": "team_name",
        "team_conf": "conference",
        "team_division": "division",
    })
    upsert(con, "nfl_teams", teams, ["team_abbr"])


def load_contracts(con):
    """Historical contracts from OverTheCap via nflverse.
    Not year-bounded — pulls everything in their dataset (~50k contracts back to 1990s)."""
    print("Loading NFL contracts (OverTheCap)...")
    try:
        contracts = nfl.import_contracts()
    except AttributeError:
        # Older nfl_data_py versions don't have it; fall back to direct CSV
        import pandas as pd
        url = "https://github.com/nflverse/nflverse-data/releases/download/contracts/historical_contracts.csv.gz"
        contracts = pd.read_csv(url, compression="gzip")
    # Normalize column names to match schema
    contracts = contracts.rename(columns={
        "draft_overall": "draft_overall",
        "date_of_birth": "date_of_birth",
    })
    # The dataset has rows per contract; otc_id + year_signed should be unique
    upsert(con, "nfl_contracts", contracts, ["otc_id", "year_signed"])


def main():
    con = duckdb.connect(str(DB_PATH))
    load_team_descriptions(con)
    load_rosters(con)
    load_weekly_rosters(con)
    load_weekly_stats(con)
    load_weekly_coaches(con)
    load_seasonal_stats(con)
    load_combine(con)
    load_nextgen(con)
    load_team_stats(con)
    load_contracts(con)
    con.close()
    print("NFL load complete.")


if __name__ == "__main__":
    main()

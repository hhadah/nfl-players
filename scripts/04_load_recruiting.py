"""Load recruiting profiles (247Sports composite via CFBD) and CFBD NFL draft picks.

Sources (common.CFBD; responses cached in data/raw/cfbd/<endpoint>/):
  /recruiting/players  one call per class year and classification:
                       RECRUIT_YEARS 2000-2025 x {HighSchool, JUCO}
  /draft/picks         one call per draft year, DRAFT_YEARS 2000-2026
  data/raw/nflverse/draft_picks.parquet (shared cache, as in 02_load_nfl.py)
                       gsis_id by draft slot
  data/raw/nflverse/players.parquet (shared cache) espn_id, to check which
                       CFBD draft id is an ESPN athlete id

Tables written (full refresh; every source field kept, nested hometown
objects flattened to hometown_info_* columns):
  recruits          one row per recruit profile (recruit_id = CFBD recruit id).
                    athlete_id is the CFBD college player id, i.e. the direct
                    link to college_players.player_id and college_player_stats;
                    college_players.recruit_ids links the other way. A
                    negative athlete_id is a placeholder roster identity with
                    no stats (18% of filled ids). recruit_type separates
                    high-school and JUCO profiles.
  cfbd_draft_picks  one row per pick. gsis_id comes from nflverse draft_picks
                    on the draft slot (season, overall = nflverse pick); it
                    agrees with the ESPN-id link below wherever both exist.
                    college_athlete_id = CFBD college player id (= ESPN
                    athlete id) from the 2009 draft on; negative values are
                    placeholders (about 20-30% of 2010-2016 picks). In the
                    2000-2008 drafts it is an ESPN draft-profile id that
                    matches no CFBD roster or stat row.
                    nfl_athlete_id is a separate CFBD/ESPN draft id, NOT the
                    nflverse espn_id (its numeric overlaps are other people).
                    franchise_id comes from nfl_team_id (the ESPN team id):
                    nfl_team is only the CURRENT location ('New York', 'Los
                    Angeles'; Raiders picks say 'Las Vegas' in every year).

Derived columns: first_name / last_name / name_suffix (recruits carry only a
full name), position_group (both 247 position vintages -- PRO/DUAL/WDE/SDE/
OC... before 2021, QB/EDGE/DL/IOL... after -- mapped to one set),
height_clean / weight_clean (NULL outside 60-84 in / 130-420 lb).

The recruiting layer is the only high-school information: CFBD has no
high-school game statistics. Class sizes are not comparable across cohorts:
2000-2001 hold only top recruits, and the 2021 class lacks most 2-star
recruits (237, against 923 in 2020 and 847 in 2022). PrepSchool profiles are
not pulled.

Usage: python 04_load_recruiting.py [--refresh]   (--refresh re-downloads
everything and spends 79 CFBD calls).
"""
import argparse

import nflreadpy as nfl
import pandas as pd
import polars as pl

from cfbd_utils import (as_id, clean_height, clean_weight, key_report,
                        position_group_cfbd, pull_years, split_name, stack)
from common import FRANCHISES, CFBD, cached_parquet, connect, to_franchise, write_table
from config import DRAFT_YEARS, RECRUIT_YEARS

RECRUIT_TYPES = ["HighSchool", "JUCO"]
SRC = "CFBD REST"

# ESPN NFL team id (CFBD nflTeamId) -> franchise_id. Validated on the picks
# whose college_athlete_id is an nflverse espn_id: for every team id the
# nflverse draft_team agrees with this map in at least 97% of picks.
ESPN_NFL_TEAM = {1: "ATL", 2: "BUF", 3: "CHI", 4: "CIN", 5: "CLE", 6: "DAL",
                 7: "DEN", 8: "DET", 9: "GB", 10: "TEN", 11: "IND", 12: "KC",
                 13: "LV", 14: "LA", 15: "MIA", 16: "MIN", 17: "NE", 18: "NO",
                 19: "NYG", 20: "NYJ", 21: "PHI", 22: "ARI", 23: "PIT", 24: "LAC",
                 25: "SF", 26: "SEA", 27: "TB", 28: "WAS", 29: "CAR", 30: "JAX",
                 33: "BAL", 34: "HOU"}


def load_recruits(con, api, refresh):
    parts = [stack(pull_years(api, "/recruiting/players", RECRUIT_YEARS, refresh,
                              classification=c), year_field="year")
             for c in RECRUIT_TYPES]
    df = pd.concat(parts, ignore_index=True).rename(columns={
        "id": "recruit_id", "season": "recruit_class"})
    df["recruit_id"] = as_id(df["recruit_id"])
    df["athlete_id"] = as_id(df["athlete_id"])
    # Recruits carry one name string; split it for name-based linkage
    names = df["name"].map(split_name)
    df["first_name"] = names.str[0]
    df["last_name"] = names.str[1]
    df["name_suffix"] = names.str[2]
    df["position_group"] = df["position"].map(position_group_cfbd)
    df["height_clean"] = clean_height(df["height"])
    df["weight_clean"] = clean_weight(df["weight"])
    lead = ["recruit_id", "athlete_id", "recruit_type", "recruit_class", "name",
            "first_name", "last_name", "name_suffix", "position", "position_group"]
    df = df[lead + [c for c in df.columns if c not in lead]]
    write_table(con, "recruits", df, source=f"{SRC} /recruiting/players",
                note="247 composite; HighSchool + JUCO; athlete_id = CFBD college player id")


def nflverse_draft_slots():
    """gsis_id by draft slot from nflverse draft_picks (shared cache)."""
    picks = cached_parquet("draft_picks", lambda: nfl.load_draft_picks(True))
    return (picks.select(pl.col("season").cast(pl.Int64),
                         pl.col("pick").cast(pl.Int64).alias("overall"), "gsis_id")
                 .to_pandas())


def espn_to_gsis():
    """ESPN athlete id -> gsis_id and nflverse draft slot (to validate the link)."""
    players = cached_parquet("players", nfl.load_players)   # shared nflverse cache
    ids = (players.filter(pl.col("espn_id").is_not_null())
                  .select(pl.col("espn_id").cast(pl.Int64).alias("college_athlete_id"),
                          "gsis_id", pl.col("draft_year").alias("nflverse_draft_year"),
                          pl.col("draft_pick").alias("nflverse_draft_pick"))
                  .to_pandas())
    if ids["college_athlete_id"].duplicated().any():
        raise ValueError("espn_id is not unique in nflverse players")
    return ids


def load_draft(con, api, refresh):
    # season = draft year, as in nflverse draft_picks
    df = stack(pull_years(api, "/draft/picks", DRAFT_YEARS, refresh), year_field="year")
    df["college_athlete_id"] = df["college_athlete_id"].astype("Int64")
    df["nfl_athlete_id"] = df["nfl_athlete_id"].astype("Int64")
    for c in ("hometown_info_latitude", "hometown_info_longitude"):
        df[c] = pd.to_numeric(df[c], errors="raise")
    # franchise_id from the ESPN team id; the location string must agree
    df.insert(df.columns.get_loc("nfl_team") + 1, "franchise_id",
              df["nfl_team_id"].map(ESPN_NFL_TEAM).map(to_franchise))
    if df["franchise_id"].isna().any():
        raise ValueError("unmapped nfl_team_id: "
                         f"{sorted(df.loc[df['franchise_id'].isna(), 'nfl_team_id'].unique())}")
    bad = [(t, f) for t, f in df[["nfl_team", "franchise_id"]].drop_duplicates().itertuples(
        index=False) if not FRANCHISES[f]["name"].startswith(t)]
    if bad:
        raise ValueError(f"nfl_team location disagrees with franchise: {bad}")
    df["position_group"] = df["position"].map(position_group_cfbd)
    ids = espn_to_gsis()
    # nfl_athlete_id is not an nflverse espn_id: show that its overlaps are
    # other players (draft slot disagrees), unlike college_athlete_id
    for col in ("nfl_athlete_id", "college_athlete_id"):
        m = df[["season", "overall", col]].merge(
            ids.rename(columns={"college_athlete_id": col}), on=col)
        same = (m["nflverse_draft_year"].eq(m["season"])
                & m["nflverse_draft_pick"].eq(m["overall"])).sum()
        print(f"  {col} found in nflverse espn_id: {len(m):,} picks; "
              f"same draft slot in nflverse: {same:,}")
    # gsis_id by draft slot (nflverse draft_picks is unique on season x pick)
    df = df.merge(nflverse_draft_slots(), on=["season", "overall"], how="left",
                  validate="one_to_one")
    # Cross-check: where the ESPN-id link lands on the same slot, it should
    # name the same gsis_id (the 2000-2001 CFBD ids that collide with other
    # players' ESPN ids land elsewhere and are not compared)
    m = df.merge(ids, on="college_athlete_id", suffixes=("", "_espn"))
    m = m[m["nflverse_draft_year"].eq(m["season"]) & m["nflverse_draft_pick"].eq(m["overall"])]
    print(f"  gsis_id attached by draft slot to {df['gsis_id'].notna().sum():,} of "
          f"{len(df):,} picks; equals the ESPN-id link's gsis_id in "
          f"{m['gsis_id'].eq(m['gsis_id_espn']).sum():,} of {len(m):,} same-slot matches")
    lead = ["season", "round", "pick", "overall", "name", "position", "position_group",
            "college_athlete_id", "nfl_athlete_id", "gsis_id"]
    df = df[lead + [c for c in df.columns if c not in lead]]
    write_table(con, "cfbd_draft_picks", df,
                source=f"{SRC} /draft/picks + nflverse draft_picks, players",
                note="season = draft year; gsis_id by draft slot (nflverse draft_picks); "
                     "college_athlete_id = CFBD player id from the 2009 draft; "
                     "nfl_athlete_id is not an nflverse id")


def coverage(con):
    print("\nCoverage summary")
    key_report(con, "recruits", ["recruit_id"], "recruit_class")
    key_report(con, "cfbd_draft_picks", ["season", "overall"], "season")

    # Shares are over all rows; negative ids are placeholders with no stats
    print("\nRecruits by class: rows; shares with an athlete_id (positive / "
          "placeholder), with college stats, geocode and committed_to")
    print(con.execute("""
        SELECT recruit_class, recruit_type, count(*) AS n,
               round(avg(coalesce(athlete_id > 0, false)::INT), 3) AS athlete_id,
               round(avg(coalesce(athlete_id < 0, false)::INT), 3) AS placeholder,
               round(avg(coalesce(athlete_id IN (SELECT player_id FROM college_player_stats),
                                  false)::INT), 3) AS with_stats,
               round(avg((hometown_info_fips_code IS NOT NULL)::INT), 3) AS fips,
               round(avg((committed_to IS NOT NULL)::INT), 3) AS committed,
               min(rating) AS min_rating
        FROM recruits GROUP BY 1, 2 ORDER BY 2, 1""").df().to_string(index=False))

    print("\nDraft picks by year: shares with a college_athlete_id (positive / "
          "placeholder), found in CFBD rosters, with college stats; pre-draft "
          "grade and ranking; gsis_id")
    con.execute("""
        CREATE OR REPLACE TEMP VIEW _picks AS
        SELECT *,
               coalesce(college_athlete_id IN (SELECT player_id FROM college_players),
                        false) AS in_rosters,
               coalesce(college_athlete_id IN (SELECT player_id FROM college_player_stats),
                        false) AS has_stats
        FROM cfbd_draft_picks""")
    print(con.execute("""
        SELECT season, count(*) AS picks,
               round(avg(coalesce(college_athlete_id > 0, false)::INT), 3) AS college_id,
               round(avg(coalesce(college_athlete_id < 0, false)::INT), 3) AS placeholder,
               round(avg(in_rosters::INT), 3) AS in_rosters,
               round(avg(has_stats::INT), 3) AS with_stats,
               round(avg((pre_draft_grade IS NOT NULL)::INT), 3) AS grade,
               round(avg((pre_draft_ranking IS NOT NULL)::INT), 3) AS ranking,
               round(avg((gsis_id IS NOT NULL)::INT), 3) AS gsis_id
        FROM _picks GROUP BY 1 ORDER BY 1""").df().to_string(index=False))

    n, k = con.execute("""SELECT count(*), count(*) FILTER (WHERE has_stats)
                          FROM _picks WHERE season BETWEEN 2005 AND 2026""").fetchone()
    print(f"\nDraftees 2005-2026 whose college_athlete_id has college stats: "
          f"{k:,} of {n:,} ({k / n:.1%})")


def main():
    ap = argparse.ArgumentParser(description="Load CFBD recruits and draft picks")
    ap.add_argument("--refresh", action="store_true",
                    help="re-download every CFBD response (spends quota)")
    args = ap.parse_args()

    api = CFBD()
    con = connect()
    print("Recruits (247 composite)...")
    load_recruits(con, api, args.refresh)
    print("Draft picks...")
    load_draft(con, api, args.refresh)
    coverage(con)
    con.close()
    print(f"\nCFBD live calls this run: {api.live_calls}")


if __name__ == "__main__":
    main()

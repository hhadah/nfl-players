"""Load college football data from the CollegeFootballData (CFBD) REST API.

Sources (common.CFBD; every response is cached in data/raw/cfbd/<endpoint>/,
so a rebuild spends no quota). One call per season (COLLEGE_SEASONS,
2004-2025) unless noted:
  /conferences/affiliations  1 call: team-conference spans, all divisions
  /teams                     1 call: static team attributes (alt. names, venue)
  /roster                    rosters, all divisions (FBS + FCS rosters)
  /stats/player/season       player season stats (regular + postseason)
  /stats/season              team season stats, classification=fbs and fcs
  /games                     every game with scores (all divisions)
  /records                   W-L splits with that season's classification
  /ratings/sp, /ratings/srs/expanded   SP+ (incl. SOS) and SRS (incl. FCS);
                             /ratings/srs for 2020 (expanded SRS is empty)
  /coaches                   1 call: head-coach seasons 2004-2025
  /ppa/players/season, /player/usage   2013-2025 (2012 returns no rows)
  /player/portal             transfer portal, 2021-2025 (2020 is empty)

Tables written (full refresh; every source field kept, nested objects
flattened to prefix_field columns):
  college_teams         team_id x season: conference and classification THAT
                        season (expanded affiliation spans) + static attributes
  college_players       roster rows: player_id x team x season, with class
                        year (0-6; filled for half of 2017 rows and 94-99% from
                        2020), hometown geocodes and recruit_ids
                        (-> recruits.recruit_id)
  college_player_stats  long: player_id, season, team, category, stat_type,
                        stat; stat_aggregation says how to combine seasons
  college_team_stats    team x season: box-score totals (wide), points and
                        per-game values, games vs FBS opponents, and the share
                        of team pass/rush attempts found in player rows
  college_games         one row per game
  college_team_records  team_id x season: W-L splits, classification
  college_team_ratings  team x season: SP+ (unit ratings, SOS) and SRS, with
                        the season's conference/classification from college_teams
  college_coaches       coach_id x team x season head-coach records; FBS
                        programs only (CFBD has no FCS or assistant coaches)
  college_player_ppa    player x team x season predicted points added (EPA)
                        per play and in total, by play type (2013+)
  college_player_usage  player x team x season share of team plays (2013+)
  college_transfers     transfer-portal entries (CFBD has no player id here)

Coverage caveats (numbers in the summary printed at the end):
  - FCS box scores before 2022 exist only for games against FBS opponents,
    so FCS player and team stat totals are truncated in 2004-2021.
    college_team_stats.box_score_complete flags the complete team-seasons.
  - FBS team totals are complete from 2004, but player stats are not.
    Before 2009 they cover mostly players still enrolled in 2009 or later
    (Georgia 2008 lacks Stafford and Moreno), so almost no 2004-2009 draftee
    has college stats. From 2009 the median FBS team is complete, but 2-15
    FBS team-seasons a year through 2016 miss a star (Auburn 2010 has no Cam
    Newton rows). college_team_stats.player_pass_att_share /
    player_rush_att_share measure completeness per team-season; use them.
  - Defensive and fumble stats start in 2016; interceptions exist every year.
  - SP+ labels every season with the team's current conference
    (sp_conference_current); the conference columns everywhere else are the
    conference of that season.
  - Player stats include postseason games and carry no games-played count.
    Rate and maximum stat types (PCT, YPA, YPC, YPR, YPP, AVG, LONG) must be
    recomputed from components or maxed, never summed across seasons.
  - Negative CFBD player ids are placeholder roster identities with no stats.

Usage: python 03_load_college.py [--refresh]   (--refresh re-downloads
everything and spends 211 CFBD calls).
"""
import argparse

import pandas as pd

from cfbd_utils import (as_id, clean_height, clean_weight, drop_repeats, frame,
                        key_report, position_group_cfbd, pull_years, snake, stack)
from common import CFBD, connect, write_table
from config import COLLEGE_SEASONS, LAST_SEASON

SEASONS = COLLEGE_SEASONS
PORTAL_SEASONS = list(range(2021, LAST_SEASON + 1))   # CFBD portal starts 2021
PPA_SEASONS = list(range(2013, LAST_SEASON + 1))      # 2012 returns no rows
SRC = "CFBD REST"

# How each player stat_type combines across seasons. Anything not listed
# raises, so a new CFBD stat type cannot be silently summed.
RATE_STATS = {"PCT", "YPA", "YPC", "YPR", "YPP", "AVG"}   # recompute from parts
MAX_STATS = {"LONG"}
SUM_STATS = {"ATT", "COMPLETIONS", "INT", "TD", "YDS", "CAR", "REC", "FGA",
             "FGM", "PTS", "XPA", "XPM", "In 20", "NO", "TB", "PD", "QB HUR",
             "SACKS", "SOLO", "TFL", "TOT", "FUM", "LOST"}


def load_teams(con, api, refresh):
    """Conference and classification by season, from affiliation spans."""
    spans = frame(api.get("/conferences/affiliations", refresh=refresh,
                          minYear=SEASONS[0], maxYear=SEASONS[-1]))
    # Expand each [start_year, end_year] span to one row per season in window
    spans["end"] = spans["end_year"].fillna(SEASONS[-1]).astype(int)
    rows = [dict(r, season=s) for r in spans.to_dict("records")
            for s in range(max(r["start_year"], SEASONS[0]),
                           min(r["end"], SEASONS[-1]) + 1)]
    teams = pd.DataFrame(rows).drop(columns="end").rename(columns={
        "start_year": "affiliation_start_year", "end_year": "affiliation_end_year"})
    # Static attributes; the current conference/classification are renamed
    # so they cannot be mistaken for the season-specific ones
    static = frame(api.get("/teams", refresh=refresh)).rename(columns={
        "id": "team_id", "conference": "current_conference",
        "classification": "current_classification", "division": "current_division"})
    teams = teams.merge(static.drop(columns="school"), on="team_id", how="left",
                        validate="many_to_one")
    lead = ["team_id", "team", "season", "conference", "classification"]
    teams = teams[lead + [c for c in teams.columns if c not in lead]]
    write_table(con, "college_teams", teams, source=f"{SRC} /conferences/affiliations + /teams",
                note="one row per team x season in the conference that season")


def load_players(con, api, refresh):
    """Rosters: one row per player x team x season, all divisions."""
    df = stack(pull_years(api, "/roster", SEASONS, refresh))
    df = df.rename(columns={"id": "player_id"})
    df["player_id"] = as_id(df["player_id"])
    # CFBD 'year' holds the class year (1-4, 5-6 for extra years) in most
    # rows from 2017 on, but repeats the season itself in older rows
    if not df.loc[df["year"] > 6, "year"].eq(df.loc[df["year"] > 6, "season"]).all():
        raise ValueError("/roster 'year' is neither a class year nor the season")
    df["class_year"] = df["year"].where(df["year"] <= 6).astype("Int64")
    df = df.drop(columns="year")
    df["recruit_ids"] = df["recruit_ids"].map(
        lambda ids: [int(i) for i in ids] if isinstance(ids, list) else None)
    df["placeholder_id"] = df["player_id"] < 0
    df["position_group"] = df["position"].map(position_group_cfbd)
    df["height_clean"] = clean_height(df["height"])
    df["weight_clean"] = clean_weight(df["weight"])
    lead = ["season", "player_id", "team", "first_name", "last_name", "position",
            "position_group", "class_year"]
    df = df[lead + [c for c in df.columns if c not in lead]]
    write_table(con, "college_players", df, source=f"{SRC} /roster",
                note="roster rows; class_year = eligibility year; negative ids are placeholders")


def load_player_stats(con, api, refresh):
    """Player season stats in the source's long format."""
    df = stack(pull_years(api, "/stats/player/season", SEASONS, refresh),
               year_field="season")
    df = df.rename(columns={"player": "player_name"})
    df["player_id"] = as_id(df["player_id"])
    df["stat"] = pd.to_numeric(df["stat"], errors="raise")
    unknown = set(df["stat_type"]) - RATE_STATS - MAX_STATS - SUM_STATS
    if unknown:
        raise ValueError(f"unclassified CFBD stat types: {sorted(unknown)}")
    df["stat_aggregation"] = df["stat_type"].map(
        lambda t: "rate" if t in RATE_STATS else "max" if t in MAX_STATS else "sum")
    lead = ["player_id", "season", "team", "conference", "category", "stat_type", "stat"]
    df = df[lead + [c for c in df.columns if c not in lead]]
    write_table(con, "college_player_stats", df, source=f"{SRC} /stats/player/season",
                note="season totals incl. postseason; rate/max stat types must not be summed")


def load_games(con, api, refresh):
    df = stack(pull_years(api, "/games", SEASONS, refresh), year_field="season")
    df = df.rename(columns={"id": "game_id"})
    write_table(con, "college_games", df, source=f"{SRC} /games",
                note="all divisions; regular + postseason")
    return df


def team_game_totals(games):
    """Per team-season: completed games, points for/against, games vs FBS."""
    done = games[games["home_points"].notna() & games["away_points"].notna()]
    sides = []
    for me, opp in (("home", "away"), ("away", "home")):
        sides.append(pd.DataFrame({
            "season": done["season"], "team": done[f"{me}_team"],
            "points_for": done[f"{me}_points"], "points_against": done[f"{opp}_points"],
            "vs_fbs": done[f"{opp}_classification"].eq("fbs")}))
    long = pd.concat(sides, ignore_index=True)
    return (long.groupby(["season", "team"], as_index=False)
                .agg(games_scored=("points_for", "size"),
                     games_vs_fbs=("vs_fbs", "sum"),
                     points_for=("points_for", "sum"),
                     points_against=("points_against", "sum")))


def load_team_stats(con, api, refresh, games):
    """Team season box-score totals (FBS and FCS calls), pivoted wide."""
    parts = [stack(pull_years(api, "/stats/season", SEASONS, refresh, classification=c),
                   year_field="season", stats_classification=c) for c in ("fbs", "fcs")]
    long = pd.concat(parts, ignore_index=True)
    # One column per statName (pivot raises if a team-season repeats a stat)
    idx = ["season", "team", "stats_classification"]
    wide = long.pivot(index=idx, columns="stat_name", values="stat_value")
    wide.columns = [snake(c) for c in wide.columns]
    wide = wide.apply(pd.to_numeric, errors="raise").reset_index()
    conf = long.drop_duplicates(idx)[idx + ["conference"]]
    df = conf.merge(wide, on=idx, validate="one_to_one").merge(
        team_game_totals(games), on=["season", "team"], how="left", validate="one_to_one")
    df["points_per_game"] = df["points_for"] / df["games_scored"]
    df["points_allowed_per_game"] = df["points_against"] / df["games_scored"]
    df["total_yards_per_game"] = df["total_yards"] / df["games"]
    df["total_yards_allowed_per_game"] = df["total_yards_opponent"] / df["games"]
    # FCS box scores before 2022 cover only games against FBS opponents
    df["box_score_complete"] = (df["stats_classification"].eq("fbs")
                                | (df["season"] >= 2022))
    # Share of the team's box-score attempts that appear in player rows:
    # ~1 when college_player_stats is complete for this team-season
    player = con.execute("""
        SELECT season, team,
               sum(stat) FILTER (WHERE category = 'passing' AND stat_type = 'ATT') AS p_att,
               sum(stat) FILTER (WHERE category = 'rushing' AND stat_type = 'CAR') AS p_car
        FROM college_player_stats GROUP BY ALL""").df()
    df = df.merge(player, on=["season", "team"], how="left", validate="one_to_one")
    df["player_pass_att_share"] = df.pop("p_att") / df["pass_attempts"]
    df["player_rush_att_share"] = df.pop("p_car") / df["rushing_attempts"]
    write_table(con, "college_team_stats", df, source=f"{SRC} /stats/season + /games",
                note="box-score totals wide; FCS totals before 2022 truncated "
                     "(box_score_complete=false); player_*_share = player-stat completeness")


def load_records(con, api, refresh):
    df = stack(pull_years(api, "/records", SEASONS, refresh), year_field="year")
    lead = ["team_id", "team", "season", "classification", "conference", "division"]
    df = df[lead + [c for c in df.columns if c not in lead]]
    write_table(con, "college_team_records", df, source=f"{SRC} /records",
                note="W-L splits; classification and conference as of that season")


def load_srs(api, refresh):
    """Expanded SRS (FBS + FCS); FBS-only /ratings/srs for seasons where the
    expanded endpoint is empty (2020)."""
    srs = stack(pull_years(api, "/ratings/srs/expanded", SEASONS, refresh),
                year_field="year", srs_source="expanded")
    missing = sorted(set(SEASONS) - set(srs["season"]))
    if missing:
        fbs = stack(pull_years(api, "/ratings/srs", missing, refresh),
                    year_field="year", srs_source="fbs_only")
        srs = pd.concat([srs, fbs], ignore_index=True)
    # The source lists some teams twice with the same rating, once without
    # classification/conference (Charlotte 2013-2025); keep the complete row
    srs = drop_repeats(srs, ["season", "team"],
                       ["classification", "conference", "division"], "SRS")
    return srs.rename(columns={c: f"srs_{c}" for c in srs.columns
                               if c not in ("season", "team", "srs_source")})


def load_ratings(con, api, refresh):
    """SP+ (FBS, with SOS and unit ratings) outer-joined to SRS (FBS + FCS)."""
    sp = stack(pull_years(api, "/ratings/sp", SEASONS, refresh), year_field="year")
    n_avg = int(sp["team"].eq("nationalAverages").sum())
    print(f"  dropped {n_avg} SP+ 'nationalAverages' pseudo-team rows")
    sp = sp[sp["team"].ne("nationalAverages")]
    # SP+ reports each team's CURRENT conference for every season
    sp = sp.rename(columns={"conference": "sp_conference_current"})
    sp = sp.rename(columns={c: f"sp_{c}" for c in sp.columns
                            if c not in ("season", "team", "sp_conference_current")})
    df = sp.merge(load_srs(api, refresh), on=["season", "team"], how="outer",
                  validate="one_to_one")
    # Conference and classification as of that season
    teams = con.execute("SELECT season, team, team_id, conference, classification "
                        "FROM college_teams").df()
    df = teams.merge(df, on=["season", "team"], how="right", validate="one_to_one")
    write_table(con, "college_team_ratings", df,
                source=f"{SRC} /ratings/sp + /ratings/srs/expanded (+ /ratings/srs 2020)",
                note="SP+ is FBS only; SRS covers FBS + FCS; sp_conference_current is "
                     "today's conference, use conference")


def load_coaches(con, api, refresh):
    coaches = api.get("/coaches", refresh=refresh, minYear=SEASONS[0], maxYear=SEASONS[-1])
    rows = [{"coach_id": c["id"], "first_name": c["firstName"], "last_name": c["lastName"],
             "hire_date": c["hireDate"], **s} for c in coaches for s in c["seasons"]]
    df = frame(rows).rename(columns={"year": "season"})
    if not df["season"].between(SEASONS[0], SEASONS[-1]).all():
        raise ValueError("/coaches returned seasons outside the requested window")
    lead = ["coach_id", "first_name", "last_name", "team_id", "school", "season"]
    df = df[lead + [c for c in df.columns if c not in lead]]
    write_table(con, "college_coaches", df, source=f"{SRC} /coaches",
                note="FBS head coaches only (CFBD has no FCS or assistant-coach data)")


def load_player_ppa(con, api, refresh):
    """Play-by-play player metrics: PPA (EPA per play) and usage shares."""
    for endpoint, table, note in (
            ("/ppa/players/season", "college_player_ppa",
             "average/total predicted points added by play type"),
            ("/player/usage", "college_player_usage",
             "share of team plays by play type")):
        df = stack(pull_years(api, endpoint, PPA_SEASONS, refresh), year_field="season")
        df = df.rename(columns={"id": "player_id", "name": "player_name"})
        df["player_id"] = as_id(df["player_id"])
        # Some player-seasons repeat with a blank conference (Troy 2013)
        df = drop_repeats(df, ["player_id", "season", "team"], ["conference"], endpoint)
        lead = ["player_id", "season", "team", "conference", "player_name", "position"]
        df = df[lead + [c for c in df.columns if c not in lead]]
        write_table(con, table, df, source=f"{SRC} {endpoint}", note=note)


def load_transfers(con, api, refresh):
    df = stack(pull_years(api, "/player/portal", PORTAL_SEASONS, refresh),
               year_field="season")
    write_table(con, "college_transfers", df, source=f"{SRC} /player/portal",
                note="no CFBD player id; link on name + origin school + season")


def coverage(con):
    """Print keys, seasons covered and the coverage facts the audit asked for."""
    print("\nCoverage summary")
    key_report(con, "college_teams", ["team_id", "season"], "season")
    key_report(con, "college_players", ["season", "player_id", "team"], "season")
    key_report(con, "college_player_stats",
               ["player_id", "season", "team", "category", "stat_type"], "season")
    key_report(con, "college_team_stats", ["season", "team"], "season")
    key_report(con, "college_games", ["game_id"], "season")
    key_report(con, "college_team_records", ["team_id", "season"], "season")
    key_report(con, "college_team_ratings", ["season", "team"], "season")
    key_report(con, "college_coaches", ["coach_id", "team_id", "season"], "season")
    key_report(con, "college_player_ppa", ["player_id", "season", "team"], "season")
    key_report(con, "college_player_usage", ["player_id", "season", "team"], "season")
    key_report(con, "college_transfers",
               ["season", "first_name", "last_name", "origin", "transfer_date"], "season")

    print("\nRoster rows by season (all / FCS teams / negative-id share)")
    print(con.execute("""
        SELECT p.season, count(*) AS rows,
               count(*) FILTER (WHERE t.classification = 'fcs') AS fcs_rows,
               round(avg(p.placeholder_id::INT), 3) AS neg_id_share,
               round(avg((len(p.recruit_ids) > 0)::INT), 3) AS has_recruit_id
        FROM college_players p
        LEFT JOIN college_teams t ON t.team = p.team AND t.season = p.season
        GROUP BY 1 ORDER BY 1""").df().to_string(index=False))

    print("\nPlayer-stat rows by season and category")
    print(con.execute("""
        PIVOT (SELECT season, category FROM college_player_stats)
        ON category USING count(*) GROUP BY season ORDER BY season""").df()
          .to_string(index=False))

    print("\nBox-score completeness by season (medians over team-seasons): team yards "
          "per game (all games in the denominator) and the share of team pass "
          "attempts found in player rows; SWAC+MEAC are the HBCU conferences; "
          "fbs_incomplete = FBS teams with < 90% of pass or rush attempts in player rows")
    print(con.execute("""
        SELECT season,
               round(median(total_yards_per_game) FILTER (WHERE stats_classification = 'fbs'), 1)
                   AS fbs_ypg,
               round(median(total_yards_per_game) FILTER (WHERE stats_classification = 'fcs'), 1)
                   AS fcs_ypg,
               round(median(total_yards_per_game) FILTER (WHERE conference IN ('SWAC', 'MEAC')), 1)
                   AS hbcu_ypg,
               round(median(total_yards / nullif(games_vs_fbs, 0))
                     FILTER (WHERE stats_classification = 'fcs'), 1) AS fcs_yds_per_fbs_game,
               round(median(player_pass_att_share) FILTER (WHERE stats_classification = 'fbs'), 3)
                   AS fbs_player_share,
               count(*) FILTER (WHERE stats_classification = 'fbs'
                                AND (coalesce(player_pass_att_share, 0) < 0.9
                                     OR coalesce(player_rush_att_share, 0) < 0.9))
                   AS fbs_incomplete,
               round(median(player_pass_att_share) FILTER (WHERE stats_classification = 'fcs'), 3)
                   AS fcs_player_share,
               count(player_pass_att_share) FILTER (WHERE stats_classification = 'fcs')
                   AS fcs_teams_with_players
        FROM college_team_stats GROUP BY 1 ORDER BY 1""").df().to_string(index=False))

    print("\nTeam-season fill rates")
    print(con.execute("""
        SELECT r.season,
               count(*) AS records,
               count(g.sp_rating) AS sp_plus, count(g.sp_sos) AS sp_sos,
               count(g.srs_rating) AS srs,
               count(s.points_per_game) AS ppg,
               count(s.total_yards_per_game) AS ypg
        FROM college_team_records r
        LEFT JOIN college_team_ratings g ON g.team = r.team AND g.season = r.season
        LEFT JOIN college_team_stats s ON s.team = r.team AND s.season = r.season
        WHERE r.classification IN ('fbs', 'fcs')
        GROUP BY 1 ORDER BY 1""").df().to_string(index=False))


def main():
    ap = argparse.ArgumentParser(description="Load CFBD college data")
    ap.add_argument("--refresh", action="store_true",
                    help="re-download every CFBD response (spends quota)")
    args = ap.parse_args()

    api = CFBD()
    con = connect()
    print("Teams (conference/classification by season)...")
    load_teams(con, api, args.refresh)
    print("Rosters...")
    load_players(con, api, args.refresh)
    print("Player season stats...")
    load_player_stats(con, api, args.refresh)
    print("Games...")
    games = load_games(con, api, args.refresh)
    print("Team season stats...")
    load_team_stats(con, api, args.refresh, games)
    print("Team records...")
    load_records(con, api, args.refresh)
    print("Team ratings (SP+, SRS)...")
    load_ratings(con, api, args.refresh)
    print("Head coaches...")
    load_coaches(con, api, args.refresh)
    print("Player PPA and usage...")
    load_player_ppa(con, api, args.refresh)
    print("Transfer portal...")
    load_transfers(con, api, args.refresh)
    coverage(con)
    con.close()
    print(f"\nCFBD live calls this run: {api.live_calls}")


if __name__ == "__main__":
    main()

"""Load NFL game-, team- and player-performance data from nflverse (nflreadpy).

Purpose: outcomes and market benchmarks for the coaching-diversity design
(team-game and team-season results, play-by-play efficiency, betting-market
expected wins, the head coach of every game) and position-neutral
productivity for the pay-discrimination design (box scores including
defense, kicking and punting; snap counts; Next Gen Stats; PFR advanced).

Sources (nflreadpy; every pull is cached in data/raw/nflverse/<name>.parquet,
so a second run makes no network calls; --refresh re-downloads everything)
  schedules                      load_schedules, games 1999-2025 (shared cache)
  players                        load_players, pfr_id -> gsis_id (shared cache)
  standings                      nflverse/nfldata standings.csv (2002+), used
                                 to validate W-L-T and to add division/seed
  pbp_<season>                   load_pbp, one file per season. The whole
                                 file is cached (~13 MB a season) so later
                                 unit- or player-level measures need no
                                 re-download; only team-game aggregates are
                                 written to DuckDB.
  player_stats_<level>_<season>  load_player_stats, level = week / reg / post
  snap_counts_<season>           load_snap_counts (PFR snap counts, 2013+)
  nextgen_stats_<type>           load_nextgen_stats, full release (2016+)
  pfr_advstats_season_<type>     load_pfr_advstats season release (2018+)

Tables written (every source column kept; franchise_id added right after
every team column; source ids renamed to the standard keys: player_id and
player_gsis_id -> gsis_id; PFR 'tm' -> team; the source's own position_group
-> position_group_nflverse, with position_group from positions.py)
  nfl_schedules             game (1999-2025) + home/away_franchise_id
  nfl_standings             team x season (2002-2025), nflverse standings
  nfl_team_games            team x game, REG and POST: result, team-
                            perspective market lines and implied win
                            probability, head coach, starting QB, rest, and
                            offensive (off_*) / defensive (def_*) pbp
                            aggregates
  nfl_team_seasons          franchise x season (REG) + playoff results
  nfl_team_week_head_coach  team x game: head coach and change flags
  nfl_player_stats_week     player x game box scores
  nfl_player_stats_season   player x season x season_type (REG, POST)
  nfl_snap_counts           player x game snaps (2013+) + gsis_id
  nfl_snap_counts_season    player x season x season_type x franchise
  nfl_nextgen_stats         NGS passing / rushing / receiving (stat_type)
  nfl_pfr_advstats_season   PFR pass / rush / rec / def (stat_type)

Play-by-play definitions (team-game, computed for the offense; def_* is the
same measure for the opponent's offense, i.e. allowed or forced by this
team's defense):
  plays          dropbacks (pass == 1, incl. sacks and scrambles) and designed
                 runs (rush == 1), excluding plays nullified by penalty
                 (play_type 'no_play'), two-point tries and plays without EPA
  epa_per_play, success_rate   means over plays (also by pass / rush)
  yards, yards_per_play        yards_gained summed over plays
  turnovers      interceptions + fumbles lost by the offense on plays
                 (special-teams fumbles are excluded)
  sacks          sacks on the offense's dropbacks
  third_down_*   nflverse third_down_converted / third_down_failed flags
  drives, points_per_drive     possessions (fixed_drive) with at least one
                 scrimmage play or kick from scrimmage; drive points are the
                 offense's score change over the drive (so TD + PAT = 7)
  pbp_available / pbp_score_mismatch   nflverse pbp lacks 3 early games
                 (aggregates null), and in a few games its running score
                 disagrees with the schedule (drive measures set to null)

Known source gaps (reported, not patched): the nflverse snap_counts_2012 file
is empty, so NFL_SNAP_SEASONS starts in 2013; player box scores have no QB hits in
2003-2005 and no tackles for loss in 2003-2011; the schedule's head coach
misses some in-season firings.

Usage: python 02c_load_nfl_stats.py [--refresh]
"""
import argparse
import io

import nflreadpy as nfl
import numpy as np
import polars as pl

from common import cached_parquet, connect, http_session, to_franchise, write_table
from config import (NFL_NGS_SEASONS, NFL_PFR_ADV_SEASONS, NFL_SEASONS,
                    NFL_SNAP_SEASONS)
from positions import position_group

STANDINGS_URL = ("https://raw.githubusercontent.com/nflverse/nfldata/master/"
                 "data/standings.csv")
PYTHAG_EXPONENT = 2.37   # NFL Pythagorean exponent (Football Outsiders)
DRIVE_PLAY_TYPES = ["pass", "run", "qb_kneel", "qb_spike", "field_goal", "punt"]
NGS_TYPES = ["passing", "rushing", "receiving"]
PFR_ADV_TYPES = ["pass", "rush", "rec", "def"]


# ============================================================================
# Helpers
# ============================================================================

def insert_after(df, anchor, expr):
    """Add the column defined by `expr` and place it right after `anchor`."""
    name = expr.meta.output_name()
    df = df.with_columns(expr)
    cols = [c for c in df.columns if c != name]
    i = cols.index(anchor) + 1
    return df.select(cols[:i] + [name] + cols[i:])


def map_unique(df, col, fn):
    """Apply a Python function to the distinct non-null values of `col`."""
    values = df[col].cast(pl.String).drop_nulls().unique().to_list()
    return {v: fn(v) for v in values}


def add_franchise(df, team_col, out_col="franchise_id"):
    """Add franchise_id (common.to_franchise) right after `team_col`.

    Codes that do not map to one franchise (e.g. PFR '2TM') are printed."""
    mapping = map_unique(df, team_col, to_franchise)
    unmapped = sorted(v for v, f in mapping.items() if f is None)
    if unmapped:
        n = df.filter(pl.col(team_col).cast(pl.String).is_in(unmapped)).height
        print(f"  note: {team_col} values with no franchise: {unmapped} ({n:,} rows)")
    expr = (pl.col(team_col).cast(pl.String)
            .replace_strict(mapping, default=None, return_dtype=pl.String)
            .alias(out_col))
    return insert_after(df, team_col, expr)


def add_position_group(df, pos_col, fn=position_group, out_col="position_group"):
    """Add the stable position group (positions.py) right after `pos_col`."""
    mapping = map_unique(df, pos_col, fn)
    expr = (pl.col(pos_col).cast(pl.String)
            .replace_strict(mapping, default=None, return_dtype=pl.String)
            .alias(out_col))
    return insert_after(df, pos_col, expr)


def per_season(name, fetch, seasons, refresh):
    """Stack one cached parquet per season (cache name = f'{name}_{season}')."""
    frames = [cached_parquet(f"{name}_{s}", lambda s=s: fetch(s), refresh)
              for s in seasons]
    empty = [s for s, f in zip(seasons, frames) if f.height == 0]
    if empty:
        print(f"  WARNING: {name}: the nflverse release has 0 rows for seasons {empty}")
    return pl.concat(frames, how="diagonal_relaxed")


def pfr_crosswalk(refresh):
    """pfr_id -> gsis_id (and nflverse position) from the players table."""
    players = cached_parquet("players", nfl.load_players, refresh)
    return (players.filter(pl.col("pfr_id").is_not_null())
            .select("pfr_id", "gsis_id", pl.col("position").alias("_players_position")))


def require(ok, message):
    """Fail loudly when a structural check does not hold."""
    if not ok:
        raise RuntimeError(message)


# ============================================================================
# Schedules and standings
# ============================================================================

def load_schedules(con, refresh):
    sched = cached_parquet("schedules", lambda: nfl.load_schedules(NFL_SEASONS), refresh)
    sched = sched.filter(pl.col("season").is_in(NFL_SEASONS))
    sched = add_franchise(sched, "away_team", "away_franchise_id")
    sched = add_franchise(sched, "home_team", "home_franchise_id")
    require(sched["game_id"].is_unique().all(), "nfl_schedules: duplicate game_id")
    write_table(con, "nfl_schedules", sched, source="nflreadpy.load_schedules",
                note="one row per game; spread_line > 0 means the home team is favored")
    return sched


def load_standings(con, refresh):
    def fetch():
        r = http_session().get(STANDINGS_URL, timeout=120)
        r.raise_for_status()
        return pl.read_csv(io.BytesIO(r.content), infer_schema_length=None)

    standings = cached_parquet("standings", fetch, refresh)
    standings = standings.filter(pl.col("season").is_in(NFL_SEASONS))
    standings = add_franchise(standings, "team")
    write_table(con, "nfl_standings", standings, source=STANDINGS_URL,
                note="nflverse/nfldata standings, 2002+ only")
    return standings


# ============================================================================
# Team games
# ============================================================================

# home_/away_ column pairs become team_/opp_ pairs; these get clearer names.
SIDE_RENAME = {
    "team_team": "team_code", "opp_team": "opponent_code",
    "team_franchise_id": "franchise_id", "opp_franchise_id": "opponent_franchise_id",
    "team_score": "points_for", "opp_score": "points_against",
    "team_coach": "head_coach", "opp_coach": "opponent_head_coach",
    "team_qb_id": "starting_qb_id", "opp_qb_id": "opponent_starting_qb_id",
    "team_qb_name": "starting_qb_name", "opp_qb_name": "opponent_starting_qb_name",
    "team_rest": "rest_days", "opp_rest": "opponent_rest_days",
    "team_moneyline": "moneyline", "opp_moneyline": "opponent_moneyline",
    "team_spread_odds": "spread_odds", "opp_spread_odds": "opponent_spread_odds",
}


def schedule_long(sched):
    """One row per team x game from the one-row-per-game schedule.

    Every home_/away_ pair becomes a team/opponent pair. The home-perspective
    game columns are re-signed: result -> margin, spread_line ->
    team_spread_line (the team's expected margin; > 0 = favored)."""
    sides = sorted(c.removeprefix("home_") for c in sched.columns
                   if c.startswith("home_")
                   and "away_" + c.removeprefix("home_") in sched.columns)
    game_cols = [c for c in sched.columns if not c.startswith(("home_", "away_"))]
    halves = []
    for me, opp, is_home in (("home", "away", True), ("away", "home", False)):
        halves.append(sched.select(
            *game_cols,
            pl.lit(is_home).alias("home"),
            *[pl.col(f"{me}_{s}").alias(f"team_{s}") for s in sides],
            *[pl.col(f"{opp}_{s}").alias(f"opp_{s}") for s in sides]))
    long = pl.concat(halves).rename(SIDE_RENAME, strict=False)

    sign = pl.when(pl.col("home")).then(1).otherwise(-1)
    pf, pa = pl.col("points_for"), pl.col("points_against")
    long = long.with_columns(
        season_type=pl.when(pl.col("game_type") == "REG").then(pl.lit("REG"))
        .otherwise(pl.lit("POST")),
        neutral_site=pl.col("location") == "Neutral",
        margin=pf - pa,
        win=(pf > pa).cast(pl.Int8),
        loss=(pf < pa).cast(pl.Int8),
        tie=(pf == pa).cast(pl.Int8),
        team_spread_line=sign * pl.col("spread_line"),
    ).drop("result", "spread_line")   # home perspective; kept in nfl_schedules
    return long


def add_implied_win_prob(long):
    """Pre-game win probability from the betting market.

    Moneylines -> vig-free probability p = q_team / (q_team + q_opp), where q
    is the American-odds implied probability. Where moneylines are missing
    (most of 1999-2005) the spread is mapped to a probability with
    logit(p) = b * team_spread_line, b fitted on the games that have both."""
    def implied(col):
        ml = pl.col(col).cast(pl.Float64)
        return pl.when(ml < 0).then(-ml / (100 - ml)).otherwise(100 / (ml + 100))

    long = long.with_columns(
        win_prob_moneyline=implied("moneyline")
        / (implied("moneyline") + implied("opponent_moneyline")))

    fit = long.filter(pl.col("win_prob_moneyline").is_not_null()
                      & pl.col("team_spread_line").is_not_null())
    x = fit["team_spread_line"].to_numpy().astype(float)
    p = fit["win_prob_moneyline"].to_numpy()
    b = float(x @ np.log(p / (1 - p)) / (x @ x))
    rmse = float(np.sqrt(np.mean((p - 1 / (1 + np.exp(-b * x))) ** 2)))
    print(f"  spread -> win prob: logit(p) = {b:.4f} x spread "
          f"(fit on {fit.height:,} team-games; RMSE vs moneyline {rmse:.4f})")

    has_ml = pl.col("win_prob_moneyline").is_not_null()
    long = long.with_columns(
        win_prob_spread=1 / (1 + (-b * pl.col("team_spread_line")).exp()))
    long = long.with_columns(
        implied_win_prob=pl.coalesce("win_prob_moneyline", "win_prob_spread"),
        implied_win_prob_source=pl.when(has_ml).then(pl.lit("moneyline"))
        .when(pl.col("win_prob_spread").is_not_null()).then(pl.lit("spread")),
    )
    return long, b, rmse


def pbp_team_game(season, refresh):
    """Offensive aggregates per game x posteam, and pbp final scores."""
    pbp = cached_parquet(f"pbp_{season}", lambda: nfl.load_pbp(season), refresh)
    final = pbp.group_by("game_id").agg(
        pbp_home_score=pl.col("total_home_score").max(),
        pbp_away_score=pl.col("total_away_score").max())
    # Rows without a possession team (timeouts, quarter ends; '' in 1999-2000)
    pbp = pbp.filter(pl.col("posteam").fill_null("") != "")
    keys = ["game_id", "posteam"]

    # Scrimmage plays: dropbacks and designed runs that count
    is_play = (((pl.col("pass") == 1) | (pl.col("rush") == 1))
               & (pl.col("play_type").fill_null("") != "no_play")
               & (pl.col("two_point_attempt").fill_null(0) == 0)
               & pl.col("epa").is_not_null())
    is_pass, is_rush = pl.col("pass") == 1, pl.col("rush") == 1
    offense = pbp.filter(is_play).group_by(keys).agg(
        plays=pl.len(),
        pass_plays=is_pass.sum(),
        rush_plays=is_rush.sum(),
        epa_total=pl.col("epa").sum(),
        epa_per_play=pl.col("epa").mean(),
        pass_epa_per_play=pl.col("epa").filter(is_pass).mean(),
        rush_epa_per_play=pl.col("epa").filter(is_rush).mean(),
        successes=pl.col("success").sum(),
        success_rate=pl.col("success").mean(),
        pass_success_rate=pl.col("success").filter(is_pass).mean(),
        rush_success_rate=pl.col("success").filter(is_rush).mean(),
        yards=pl.col("yards_gained").sum(),
        sacks=pl.col("sack").sum(),
        interceptions=pl.col("interception").sum(),
        fumbles_lost=((pl.col("fumble_lost") == 1)
                      & (pl.col("fumbled_1_team") == pl.col("posteam"))).sum(),
    ).with_columns(
        yards_per_play=pl.col("yards") / pl.col("plays"),
        pass_rate=pl.col("pass_plays") / pl.col("plays"),
        turnovers=pl.col("interceptions") + pl.col("fumbles_lost"),
    )

    # Third downs (nflverse conversion flags)
    third = pbp.group_by(keys).agg(
        third_down_conv=pl.col("third_down_converted").sum(),
        third_down_att=(pl.col("third_down_converted")
                        + pl.col("third_down_failed")).sum(),
    ).with_columns(third_down_rate=pl.col("third_down_conv") / pl.col("third_down_att"))

    # Drives: possessions with a scrimmage play or a kick from scrimmage
    drives = (pbp.filter(pl.col("fixed_drive").is_not_null())
              .group_by(keys + ["fixed_drive"])
              .agg(counts=(pl.col("play_type").is_in(DRIVE_PLAY_TYPES) | is_play).any(),
                   points=pl.col("posteam_score_post").max() - pl.col("posteam_score").min())
              .filter("counts")
              .group_by(keys)
              .agg(drives=pl.len(), drive_points=pl.col("points").sum())
              .with_columns(points_per_drive=pl.col("drive_points") / pl.col("drives")))

    agg = (offense.join(third, on=keys, how="full", coalesce=True)
           .join(drives, on=keys, how="full", coalesce=True))
    return agg, final


def load_pbp_aggregates(refresh):
    """Team-game offensive aggregates for every NFL season, keyed by franchise."""
    aggs, finals = [], []
    for season in NFL_SEASONS:
        agg, final = pbp_team_game(season, refresh)
        aggs.append(agg)
        finals.append(final)
        print(f"  pbp {season}: {agg.height:,} team-games")
    agg = pl.concat(aggs, how="diagonal_relaxed")
    agg = add_franchise(agg, "posteam").drop("posteam")
    require(agg["franchise_id"].null_count() == 0, "pbp: unmapped posteam")
    require(not agg.select("game_id", "franchise_id").is_duplicated().any(),
            "pbp aggregates: duplicate game_id x franchise_id")
    return agg, pl.concat(finals)


def build_team_games(sched, refresh):
    long = schedule_long(sched)
    long, slope, rmse = add_implied_win_prob(long)
    agg, final = load_pbp_aggregates(refresh)

    # pbp quality: games absent from pbp, and games whose pbp running score
    # disagrees with the schedule (drive points are unreliable there)
    check = sched.select("game_id", "home_score", "away_score").join(final, on="game_id", how="left")
    no_pbp = check.filter(pl.col("pbp_home_score").is_null())["game_id"]
    bad_score = check.filter((pl.col("home_score") != pl.col("pbp_home_score"))
                             | (pl.col("away_score") != pl.col("pbp_away_score")))["game_id"]
    print(f"  pbp coverage: {sched.height - no_pbp.len():,}/{sched.height:,} games; "
          f"missing: {no_pbp.to_list()}")
    print(f"  pbp final score differs from schedule in {bad_score.len()} games "
          f"(drive measures set to null): {bad_score.to_list()}")

    # Offense = this team's possessions; defense = the opponent's possessions
    stats = [c for c in agg.columns if c not in ("game_id", "franchise_id")]
    off = agg.rename({c: f"off_{c}" for c in stats})
    dfn = agg.rename({c: f"def_{c}" for c in stats} | {"franchise_id": "opponent_franchise_id"})
    games = (long.join(off, on=["game_id", "franchise_id"], how="left")
             .join(dfn, on=["game_id", "opponent_franchise_id"], how="left")
             .with_columns(turnover_margin=pl.col("def_turnovers") - pl.col("off_turnovers"),
                           pbp_available=~pl.col("game_id").is_in(no_pbp.implode()),
                           pbp_score_mismatch=pl.col("game_id").is_in(bad_score.implode())))
    drive_cols = [f"{u}_{m}" for u in ("off", "def")
                  for m in ("drives", "drive_points", "points_per_drive")]
    games = games.with_columns(
        pl.when(pl.col("pbp_score_mismatch")).then(None).otherwise(pl.col(c)).alias(c)
        for c in drive_cols)

    # Key column order: identifiers first, then everything else as built
    lead = ["game_id", "season", "game_type", "season_type", "week", "gameday",
            "franchise_id", "team_code", "opponent_franchise_id", "opponent_code",
            "home", "neutral_site", "points_for", "points_against", "margin",
            "win", "loss", "tie", "head_coach", "starting_qb_id", "starting_qb_name",
            "team_spread_line", "total_line", "implied_win_prob",
            "implied_win_prob_source"]
    games = games.select(lead + [c for c in games.columns if c not in lead])
    games = games.sort("season", "gameday", "game_id", "home")

    # Validation: exactly two rows per game, unique keys, franchises mapped
    per_game = games.group_by("game_id").len()
    require(per_game.height == sched.height and (per_game["len"] == 2).all(),
            "nfl_team_games: a game does not have exactly two team rows")
    require(games["franchise_id"].null_count() == 0
            and games["opponent_franchise_id"].null_count() == 0,
            "nfl_team_games: unmapped team code")
    require(not games.select("game_id", "franchise_id").is_duplicated().any(),
            "nfl_team_games: duplicate game_id x franchise_id")

    # Validation: points equal the schedule's scores
    home = games.filter(pl.col("home")).select("game_id", "points_for", "points_against")
    pts = sched.select("game_id", "home_score", "away_score").join(home, on="game_id")
    require(pts.height == sched.height
            and ((pts["home_score"] == pts["points_for"])
                 & (pts["away_score"] == pts["points_against"])).all(),
            "nfl_team_games: points do not match nfl_schedules")
    return games, slope, rmse


# ============================================================================
# Team seasons and weekly head coach
# ============================================================================

def build_team_seasons(games, standings):
    """REG-season aggregates per franchise x season, plus playoff results."""
    reg = games.filter(pl.col("season_type") == "REG").sort("gameday")
    by = ["franchise_id", "season"]
    seasons = reg.group_by(by).agg(
        team_code=pl.col("team_code").last(),
        games=pl.len(),
        wins=pl.col("win").sum(),
        losses=pl.col("loss").sum(),
        ties=pl.col("tie").sum(),
        points_for=pl.col("points_for").sum(),
        points_against=pl.col("points_against").sum(),
        expected_wins=pl.col("implied_win_prob").sum(),
        games_prob_from_spread=(pl.col("implied_win_prob_source") == "spread").sum(),
        head_coach_week1=pl.col("head_coach").first(),
        head_coach_last=pl.col("head_coach").last(),
        n_head_coaches=pl.col("head_coach").n_unique(),
        n_starting_qbs=pl.col("starting_qb_id").n_unique(),
        off_plays=pl.col("off_plays").sum(),
        off_epa_total=pl.col("off_epa_total").sum(),
        off_successes=pl.col("off_successes").sum(),
        off_yards=pl.col("off_yards").sum(),
        off_turnovers=pl.col("off_turnovers").sum(),
        off_drives=pl.col("off_drives").sum(),
        off_drive_points=pl.col("off_drive_points").sum(),
        def_plays=pl.col("def_plays").sum(),
        def_epa_total=pl.col("def_epa_total").sum(),
        def_successes=pl.col("def_successes").sum(),
        def_yards=pl.col("def_yards").sum(),
        def_turnovers=pl.col("def_turnovers").sum(),
        def_drives=pl.col("def_drives").sum(),
        def_drive_points=pl.col("def_drive_points").sum(),
    )
    pf, pa, x = pl.col("points_for"), pl.col("points_against"), PYTHAG_EXPONENT
    seasons = seasons.with_columns(
        win_pct=(pl.col("wins") + 0.5 * pl.col("ties")) / pl.col("games"),
        point_diff=pf - pa,
        pythag_win_pct=pf ** x / (pf ** x + pa ** x),
        wins_minus_expected=pl.col("wins") + 0.5 * pl.col("ties") - pl.col("expected_wins"),
        turnover_margin=pl.col("def_turnovers") - pl.col("off_turnovers"),
        **{f"{u}_{m}": pl.col(f"{u}_{num}") / pl.col(f"{u}_{den}")
           for u in ("off", "def")
           for m, num, den in (("epa_per_play", "epa_total", "plays"),
                               ("success_rate", "successes", "plays"),
                               ("yards_per_play", "yards", "plays"),
                               ("points_per_drive", "drive_points", "drives"))},
    ).with_columns(pythag_wins=pl.col("pythag_win_pct") * pl.col("games"))

    # Playoffs: any POST game = appearance
    post = games.filter(pl.col("season_type") == "POST").group_by(by).agg(
        playoff_games=pl.len(), playoff_wins=pl.col("win").sum())
    seasons = seasons.join(post, on=by, how="left").with_columns(
        made_playoffs=pl.col("playoff_games").is_not_null(),
        playoff_games=pl.col("playoff_games").fill_null(0),
        playoff_wins=pl.col("playoff_wins").fill_null(0))

    # Conference, division and seed from nflverse standings (2002+)
    extra = standings.select(by + ["conf", "division", "div_rank", "seed",
                                   pl.col("playoff").alias("playoff_result"),
                                   pl.col("wins").alias("_s_wins"),
                                   pl.col("losses").alias("_s_losses"),
                                   pl.col("ties").alias("_s_ties")])
    seasons = seasons.join(extra, on=by, how="left")
    compared = seasons.filter(pl.col("_s_wins").is_not_null())
    mismatch = compared.filter((pl.col("wins") != pl.col("_s_wins"))
                               | (pl.col("losses") != pl.col("_s_losses"))
                               | (pl.col("ties") != pl.col("_s_ties")))
    print(f"  W-L-T vs nflverse standings: {compared.height - mismatch.height:,}/"
          f"{compared.height:,} franchise-seasons agree")
    require(mismatch.height == 0, f"W-L-T disagree with standings:\n{mismatch}")
    seasons = seasons.drop("_s_wins", "_s_losses", "_s_ties")

    lead = ["franchise_id", "season", "team_code", "games", "wins", "losses", "ties",
            "win_pct", "points_for", "points_against", "point_diff",
            "pythag_win_pct", "pythag_wins", "expected_wins", "wins_minus_expected",
            "off_epa_per_play", "def_epa_per_play", "off_success_rate",
            "def_success_rate", "made_playoffs", "playoff_games", "playoff_wins"]
    seasons = seasons.select(lead + [c for c in seasons.columns if c not in lead])
    return seasons.sort(by)


def build_head_coach_weeks(games):
    """Head coach of every team-game, with in-season and new-season change flags.

    coach_change: the first game of a head coach who replaced another within
    the same season. new_hc_vs_prior_season: the season's first game has a
    different head coach than the franchise's last game of the prior season
    (null for a franchise's first season in the window)."""
    hc = (games.select("franchise_id", "team_code", "season", "game_type",
                       "season_type", "week", "gameday", "game_id", "head_coach")
          .sort("franchise_id", "season", "gameday"))
    prev = pl.col("head_coach").shift(1).over("franchise_id", "season")
    hc = hc.with_columns(
        coach_change=prev.is_not_null() & (pl.col("head_coach") != prev),
        game_in_season=pl.int_range(1, pl.len() + 1).over("franchise_id", "season"))
    last = (hc.group_by("franchise_id", "season")
            .agg(prior_season_last_hc=pl.col("head_coach").last())
            .with_columns(pl.col("season") + 1))
    hc = hc.join(last, on=["franchise_id", "season"], how="left").with_columns(
        new_hc_vs_prior_season=pl.when(pl.col("game_in_season") == 1)
        .then(pl.col("head_coach") != pl.col("prior_season_last_hc"))
        .otherwise(pl.lit(False)))
    return hc.sort("franchise_id", "season", "gameday")


# ============================================================================
# Player productivity
# ============================================================================

def load_player_stats_week(con, refresh):
    df = per_season("player_stats_week", lambda s: nfl.load_player_stats(s, "week"),
                    NFL_SEASONS, refresh)
    df = df.rename({"player_id": "gsis_id", "position_group": "position_group_nflverse"})
    df = add_position_group(df, "position")
    df = add_franchise(df, "team")
    df = add_franchise(df, "opponent_team", "opponent_franchise_id")
    write_table(con, "nfl_player_stats_week", df,
                source="nflreadpy.load_player_stats(summary_level='week')",
                note="one row per player x game, REG and POST (season_type)")
    return df


def load_player_stats_season(con, refresh):
    reg = per_season("player_stats_reg", lambda s: nfl.load_player_stats(s, "reg"),
                     NFL_SEASONS, refresh)
    post = per_season("player_stats_post", lambda s: nfl.load_player_stats(s, "post"),
                      NFL_SEASONS, refresh)
    require(set(reg["season_type"].unique()) == {"REG"}
            and set(post["season_type"].unique()) == {"POST"},
            "player season stats: unexpected source season_type values")
    df = pl.concat([reg, post], how="diagonal_relaxed")
    df = df.rename({"player_id": "gsis_id", "position_group": "position_group_nflverse"})
    df = add_position_group(df, "position")
    df = add_franchise(df, "recent_team")
    write_table(con, "nfl_player_stats_season", df,
                source="nflreadpy.load_player_stats(summary_level='reg' and 'post')",
                note="one row per player x season x season_type (source column)")
    return df


def load_snap_counts(con, xwalk, refresh):
    snaps = per_season("snap_counts", nfl.load_snap_counts, NFL_SNAP_SEASONS, refresh)
    ids = xwalk.select(pl.col("pfr_id").alias("pfr_player_id"), "gsis_id")
    snaps = snaps.join(ids, on="pfr_player_id", how="left")
    snaps = insert_after(snaps, "pfr_player_id", pl.col("gsis_id"))
    snaps = add_position_group(snaps, "position")   # PFR codes, e.g. 'G/T', 'LCB'
    snaps = add_franchise(snaps, "team")
    snaps = add_franchise(snaps, "opponent", "opponent_franchise_id")
    snaps = snaps.with_columns(
        season_type=pl.when(pl.col("game_type") == "REG").then(pl.lit("REG"))
        .otherwise(pl.lit("POST")))
    write_table(con, "nfl_snap_counts", snaps, source="nflreadpy.load_snap_counts",
                note="PFR snap counts; gsis_id via nflverse players.pfr_id")
    return snaps


def build_snap_counts_season(snaps):
    """Season snap totals per player x season x season_type x franchise.

    *_pct_mean is the mean game share over the games the player appears in;
    *_share_team is his snaps over all of the team's snaps that season (so it
    also reflects games missed). Team snaps per game are recovered from the
    most-used player: snaps / share, rounded. position and position_group are
    the modal game values, with ties broken alphabetically so that reruns
    give identical rows."""
    units = ("offense", "defense", "st")
    team_game = snaps.group_by("game_id", "franchise_id", "season", "season_type").agg(
        **{f"team_{u}_snaps": (pl.col(f"{u}_snaps") / pl.col(f"{u}_pct"))
           .sort_by(f"{u}_snaps").last().round() for u in units})
    by_team = ["franchise_id", "season", "season_type"]
    team_season = team_game.group_by(by_team).agg(
        team_games=pl.len(), **{f"team_{u}_snaps": pl.col(f"team_{u}_snaps").sum()
                                for u in units})

    keys = ["pfr_player_id", "season", "season_type", "franchise_id"]
    season = snaps.group_by(keys).agg(
        gsis_id=pl.col("gsis_id").drop_nulls().first(),
        player=pl.col("player").last(),
        position=pl.col("position").mode().sort().first(),
        position_group=pl.col("position_group").mode().sort().first(),
        team=pl.col("team").last(),
        games=pl.len(),
        **{f"games_{u}": (pl.col(f"{u}_snaps") > 0).sum() for u in units},
        **{f"{u}_snaps": pl.col(f"{u}_snaps").sum() for u in units},
        **{f"{u}_pct_mean": pl.col(f"{u}_pct").mean() for u in units},
    )
    season = season.join(team_season, on=by_team, how="left").with_columns(
        **{f"{u}_share_team": pl.col(f"{u}_snaps") / pl.col(f"team_{u}_snaps")
           for u in units})
    lead = ["gsis_id", "pfr_player_id", "season", "season_type", "franchise_id",
            "team", "player", "position", "position_group", "games"]
    return season.select(lead + [c for c in season.columns if c not in lead]).sort(keys)


def load_nextgen(con, refresh):
    frames = []
    for t in NGS_TYPES:
        d = cached_parquet(f"nextgen_stats_{t}",
                           lambda t=t: nfl.load_nextgen_stats(True, t), refresh)
        frames.append(d.filter(pl.col("season").is_in(NFL_NGS_SEASONS))
                      .with_columns(stat_type=pl.lit(t)))
    ngs = pl.concat(frames, how="diagonal_relaxed").rename({"player_gsis_id": "gsis_id"})
    ngs = ngs.select(["stat_type"] + [c for c in ngs.columns if c != "stat_type"])
    ngs = add_franchise(ngs, "team_abbr")
    ngs = add_position_group(ngs, "player_position")
    write_table(con, "nfl_nextgen_stats", ngs, source="nflreadpy.load_nextgen_stats",
                note="passing/rushing/receiving stacked; week 0 = season total")
    return ngs


def load_pfr_advstats(con, xwalk, refresh):
    frames = []
    for t in PFR_ADV_TYPES:
        d = cached_parquet(f"pfr_advstats_season_{t}",
                           lambda t=t: nfl.load_pfr_advstats(True, t, "season"), refresh)
        if "tm" in d.columns:
            d = d.rename({"tm": "team"})
        frames.append(d.filter(pl.col("season").is_in(NFL_PFR_ADV_SEASONS))
                      .with_columns(stat_type=pl.lit(t)))
    adv = pl.concat(frames, how="diagonal_relaxed")
    adv = adv.select(["stat_type"] + [c for c in adv.columns if c != "stat_type"])

    # gsis_id via pfr_id; position group from PFR 'pos', else nflverse players
    adv = adv.join(xwalk, on="pfr_id", how="left")
    adv = insert_after(adv, "pfr_id", pl.col("gsis_id"))
    groups = map_unique(adv, "pos", position_group)
    fallback = map_unique(adv, "_players_position", position_group)
    adv = adv.with_columns(position_group=pl.coalesce(
        pl.col("pos").replace_strict(groups, default=None, return_dtype=pl.String),
        pl.col("_players_position").replace_strict(fallback, default=None,
                                                   return_dtype=pl.String))
    ).drop("_players_position")
    adv = add_franchise(adv, "team")
    write_table(con, "nfl_pfr_advstats_season", adv,
                source="nflreadpy.load_pfr_advstats(summary_level='season')",
                note="pass/rush/rec/def stacked (stat_type); gsis_id via players.pfr_id")
    return adv


# ============================================================================
# Coverage summary
# ============================================================================

def summarize(con):
    """Rows, seasons, key uniqueness and franchise_id nulls for every table.

    'missing' lists configured seasons with no rows; 'null key' counts rows
    with a null key column (source 'Team' rows, PFR rows without pfr_id)."""
    specs = [
        ("nfl_schedules", "game_id", NFL_SEASONS,
         ["home_franchise_id", "away_franchise_id"]),
        ("nfl_standings", "season, team", NFL_SEASONS, ["franchise_id"]),
        ("nfl_team_games", "game_id, franchise_id", NFL_SEASONS,
         ["franchise_id", "opponent_franchise_id"]),
        ("nfl_team_seasons", "franchise_id, season", NFL_SEASONS, ["franchise_id"]),
        ("nfl_team_week_head_coach", "game_id, franchise_id", NFL_SEASONS, ["franchise_id"]),
        ("nfl_player_stats_week", "gsis_id, game_id", NFL_SEASONS,
         ["franchise_id", "opponent_franchise_id"]),
        ("nfl_player_stats_season", "gsis_id, season, season_type", NFL_SEASONS,
         ["franchise_id"]),
        ("nfl_snap_counts", "pfr_player_id, game_id", NFL_SNAP_SEASONS,
         ["franchise_id", "opponent_franchise_id"]),
        ("nfl_snap_counts_season", "pfr_player_id, season, season_type, franchise_id",
         NFL_SNAP_SEASONS, ["franchise_id"]),
        ("nfl_nextgen_stats", "stat_type, season, season_type, week, gsis_id",
         NFL_NGS_SEASONS, ["franchise_id"]),
        ("nfl_pfr_advstats_season", "stat_type, season, pfr_id, team",
         NFL_PFR_ADV_SEASONS, ["franchise_id"]),
    ]
    print("\nCoverage summary")
    print(f"  {'table':26s} {'rows':>9s} {'seasons':>11s} {'dup keys':>9s} {'null key':>9s}"
          "  franchise_id null; configured seasons missing")
    for table, key, window, fids in specs:
        n, present = con.execute(
            f'SELECT count(*), list(DISTINCT season) FROM "{table}"').fetchone()
        dups = con.execute(f'SELECT count(*) FROM (SELECT {key} FROM "{table}" '
                           f'GROUP BY ALL HAVING count(*) > 1)').fetchone()[0]
        null_key = con.execute(
            f'SELECT count(*) FROM "{table}" WHERE '
            + " OR ".join(f"{k.strip()} IS NULL" for k in key.split(","))).fetchone()[0]
        rates = con.execute("SELECT " + ", ".join(f"avg(({c} IS NULL)::INT)" for c in fids)
                            + f' FROM "{table}"').fetchone()
        nulls = ", ".join(f"{c} {r:.2%}" for c, r in zip(fids, rates))
        missing = sorted(set(window) - set(present)) or "none"
        print(f"  {table:26s} {n:9,d} {min(present)}-{max(present):>6} {dups:9,d} "
              f"{null_key:9,d}  {nulls}; missing: {missing}")
    print("  (key = " + "; ".join(f"{t}: {k}" for t, k, _, _ in specs) + ")")

    # Defensive, kicking and punting box scores: first season populated
    cols = ["def_tackles_solo", "def_tackle_assists", "def_sacks", "def_qb_hits",
            "def_tackles_for_loss", "def_pass_defended", "def_interceptions",
            "def_fumbles_forced", "fg_att", "pat_att", "pt_att"]
    print("\n  nfl_player_stats_week: player-weeks with a nonzero value; seasons with none")
    for c in cols:
        n, empty = con.execute(
            f"SELECT sum(n), list(season ORDER BY season) FILTER (WHERE n = 0) FROM "
            f"(SELECT season, count(*) FILTER (WHERE {c} > 0) AS n "
            "FROM nfl_player_stats_week GROUP BY season)").fetchone()
        print(f"    {c:24s} {n:9,d}  empty seasons: {empty or 'none'}")

    # Snap-count linkage to gsis_id by position group
    print("\n  nfl_snap_counts: gsis_id link rate by position_group (player-games)")
    for g, n, rate in con.execute(
            "SELECT position_group, count(*), avg((gsis_id IS NOT NULL)::INT) "
            "FROM nfl_snap_counts GROUP BY 1 ORDER BY 1").fetchall():
        print(f"    {str(g):5s} {n:8,d}  {rate:.1%}")

    # PFR advanced defense: pressures, missed tackles, coverage
    print("\n  nfl_pfr_advstats_season (def): rows with non-null measure")
    for s, n, prss, mtkl, tgt, yds in con.execute(
            "SELECT season, count(*), count(prss), count(m_tkl), count(tgt), count(yds) "
            "FROM nfl_pfr_advstats_season WHERE stat_type = 'def' GROUP BY 1 ORDER BY 1"
    ).fetchall():
        print(f"    {s}: rows {n:5,d}  pressures {prss:5,d}  missed tackles {mtkl:5,d}  "
              f"targets {tgt:5,d}  yards allowed {yds:5,d}")


# ============================================================================
# Main
# ============================================================================

def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--refresh", action="store_true",
                    help="re-download every nflverse file instead of using the cache")
    args = ap.parse_args()
    con = connect()

    print("Schedules and standings...")
    sched = load_schedules(con, args.refresh)
    standings = load_standings(con, args.refresh)

    print("Team games (schedules + play-by-play)...")
    games, slope, rmse = build_team_games(sched, args.refresh)
    write_table(con, "nfl_team_games", games,
                source="nflreadpy.load_schedules + load_pbp",
                note=f"team x game; spread prob logit slope {slope:.4f} (RMSE {rmse:.4f})")

    print("Team seasons and head coaches...")
    write_table(con, "nfl_team_seasons", build_team_seasons(games, standings),
                source="nfl_team_games + nflverse standings",
                note=f"REG season; Pythagorean exponent {PYTHAG_EXPONENT}")
    write_table(con, "nfl_team_week_head_coach", build_head_coach_weeks(games),
                source="nflreadpy.load_schedules (home_coach/away_coach)",
                note="nflverse misses some in-season firings; not corrected here")

    print("Player stats...")
    load_player_stats_week(con, args.refresh)
    load_player_stats_season(con, args.refresh)

    print("Snap counts, Next Gen Stats, PFR advanced stats...")
    xwalk = pfr_crosswalk(args.refresh)
    snaps = load_snap_counts(con, xwalk, args.refresh)
    write_table(con, "nfl_snap_counts_season", build_snap_counts_season(snaps),
                source="nfl_snap_counts", note="player x season x season_type x franchise")
    load_nextgen(con, args.refresh)
    load_pfr_advstats(con, xwalk, args.refresh)

    summarize(con)
    con.close()


if __name__ == "__main__":
    main()

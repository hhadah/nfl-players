"""Create the DuckDB schema. Idempotent — safe to re-run."""
import duckdb
from config import DB_PATH


SCHEMA = """
-- ============ PLAYER TABLES ============

CREATE TABLE IF NOT EXISTS nfl_players (
    gsis_id           VARCHAR PRIMARY KEY,
    pfr_id            VARCHAR,
    espn_id           VARCHAR,
    sleeper_id        VARCHAR,
    full_name         VARCHAR,
    first_name        VARCHAR,
    last_name         VARCHAR,
    birth_date        DATE,
    height_inches     INTEGER,
    weight_lbs        INTEGER,
    position          VARCHAR,
    college           VARCHAR,
    draft_year        INTEGER,
    draft_round       INTEGER,
    draft_pick        INTEGER,
    draft_team        VARCHAR,
    rookie_year       INTEGER,
    last_season       INTEGER,
    status            VARCHAR
);

CREATE TABLE IF NOT EXISTS nfl_weekly_rosters (
    gsis_id           VARCHAR,
    season            INTEGER,
    week              INTEGER,
    season_type       VARCHAR,           -- REG / POST
    team              VARCHAR,
    position          VARCHAR,
    depth_chart_position VARCHAR,
    jersey_number     INTEGER,
    status            VARCHAR,           -- ACT / INA / CUT / IR / PRA / etc.
    years_exp         INTEGER,
    PRIMARY KEY (gsis_id, season, week, season_type, team)
);

CREATE TABLE IF NOT EXISTS nfl_player_stats (
    gsis_id           VARCHAR,
    season            INTEGER,
    week              INTEGER,           -- NULL for season totals
    season_type       VARCHAR,           -- REG / POST
    team              VARCHAR,
    position          VARCHAR,
    -- Passing
    completions       INTEGER, attempts INTEGER, passing_yards INTEGER,
    passing_tds       INTEGER, interceptions INTEGER, sacks DOUBLE,
    sack_yards        DOUBLE, passing_air_yards DOUBLE, passing_yards_after_catch DOUBLE,
    passing_epa       DOUBLE, dakota DOUBLE,
    -- Rushing
    carries           INTEGER, rushing_yards INTEGER, rushing_tds INTEGER,
    rushing_fumbles   INTEGER, rushing_epa DOUBLE,
    -- Receiving
    receptions        INTEGER, targets INTEGER, receiving_yards INTEGER,
    receiving_tds     INTEGER, receiving_air_yards DOUBLE,
    receiving_yards_after_catch DOUBLE, receiving_epa DOUBLE,
    -- Fantasy
    fantasy_points    DOUBLE, fantasy_points_ppr DOUBLE,
    PRIMARY KEY (gsis_id, season, week, season_type)
);

CREATE TABLE IF NOT EXISTS nfl_combine (
    season            INTEGER,
    player_name       VARCHAR,
    pfr_id            VARCHAR,
    pos               VARCHAR,
    school            VARCHAR,
    ht                DOUBLE,
    wt                INTEGER,
    forty             DOUBLE,
    bench             INTEGER,
    vertical          DOUBLE,
    broad_jump        INTEGER,
    cone              DOUBLE,
    shuttle           DOUBLE,
    draft_team        VARCHAR,
    draft_round       INTEGER,
    draft_pick        INTEGER,
    PRIMARY KEY (season, player_name, pos)
);

CREATE TABLE IF NOT EXISTS nfl_nextgen_stats (
    gsis_id           VARCHAR,
    season            INTEGER,
    week              INTEGER,
    stat_type         VARCHAR,           -- passing / rushing / receiving
    -- Common
    avg_time_to_throw       DOUBLE,
    avg_completed_air_yards DOUBLE,
    avg_intended_air_yards  DOUBLE,
    aggressiveness          DOUBLE,
    avg_air_yards_to_sticks DOUBLE,
    expected_completion_pct DOUBLE,
    completion_pct_above_exp DOUBLE,
    -- Rushing
    efficiency              DOUBLE,
    pct_attempts_gte_eight_defenders DOUBLE,
    avg_time_to_los         DOUBLE,
    rush_yards_over_expected DOUBLE,
    -- Receiving
    avg_cushion             DOUBLE,
    avg_separation          DOUBLE,
    avg_yac                 DOUBLE,
    avg_expected_yac        DOUBLE,
    yac_above_expectation   DOUBLE,
    PRIMARY KEY (gsis_id, season, week, stat_type)
);

CREATE TABLE IF NOT EXISTS college_players (
    cfbd_id           VARCHAR PRIMARY KEY,
    first_name        VARCHAR,
    last_name         VARCHAR,
    full_name         VARCHAR,
    team              VARCHAR,
    position          VARCHAR,
    height_inches     INTEGER,
    weight_lbs        INTEGER,
    jersey            INTEGER,
    home_city         VARCHAR,
    home_state        VARCHAR,
    home_country      VARCHAR,
    seasons_played    VARCHAR             -- JSON array of season ints
);

CREATE TABLE IF NOT EXISTS college_player_stats (
    cfbd_id           VARCHAR,
    season            INTEGER,
    team              VARCHAR,
    category          VARCHAR,           -- passing/rushing/receiving/defensive/...
    stat_type         VARCHAR,           -- YDS, TD, ATT, etc.
    stat_value        DOUBLE,
    PRIMARY KEY (cfbd_id, season, team, category, stat_type)
);

CREATE TABLE IF NOT EXISTS recruits (
    recruit_id        VARCHAR PRIMARY KEY,
    year              INTEGER,
    name              VARCHAR,
    first_name        VARCHAR,
    last_name         VARCHAR,
    position          VARCHAR,
    height_inches     INTEGER,
    weight_lbs        INTEGER,
    stars             INTEGER,
    rating            DOUBLE,            -- 247 composite
    ranking           INTEGER,           -- national rank
    position_ranking  INTEGER,
    state_ranking     INTEGER,
    committed_to      VARCHAR,
    high_school       VARCHAR,
    hometown_city     VARCHAR,
    hometown_state    VARCHAR,
    hometown_country  VARCHAR
);

-- ============ TEAM TABLES ============

CREATE TABLE IF NOT EXISTS nfl_teams (
    team_abbr         VARCHAR PRIMARY KEY,
    team_name         VARCHAR,
    conference        VARCHAR,
    division          VARCHAR
);

CREATE TABLE IF NOT EXISTS nfl_team_stats (
    team              VARCHAR,
    season            INTEGER,
    week              INTEGER,           -- NULL for season totals
    season_type       VARCHAR,
    -- Offense
    points_for        INTEGER,
    total_yards       INTEGER,
    pass_yards        INTEGER,
    rush_yards        INTEGER,
    turnovers         INTEGER,
    -- Defense
    points_against    INTEGER,
    yards_allowed     INTEGER,
    -- Result
    wins              INTEGER,
    losses            INTEGER,
    ties              INTEGER,
    PRIMARY KEY (team, season, week, season_type)
);

CREATE TABLE IF NOT EXISTS college_teams (
    school            VARCHAR PRIMARY KEY,
    mascot            VARCHAR,
    abbreviation      VARCHAR,
    conference        VARCHAR,
    division          VARCHAR,
    classification    VARCHAR             -- fbs / fcs
);

CREATE TABLE IF NOT EXISTS college_team_stats (
    school            VARCHAR,
    season            INTEGER,
    wins              INTEGER,
    losses            INTEGER,
    points_per_game   DOUBLE,
    points_allowed_per_game DOUBLE,
    total_yards_pg    DOUBLE,
    yards_allowed_pg  DOUBLE,
    sp_plus_rating    DOUBLE,             -- if available
    srs               DOUBLE,
    sos               DOUBLE,
    PRIMARY KEY (school, season)
);

CREATE TABLE IF NOT EXISTS nfl_contracts (
    otc_id            INTEGER,
    gsis_id           VARCHAR,
    player            VARCHAR,
    position          VARCHAR,
    team              VARCHAR,
    is_active         BOOLEAN,
    year_signed       INTEGER,
    years             INTEGER,            -- contract length
    value             DOUBLE,             -- total $M
    apy               DOUBLE,             -- average per year $M
    guaranteed        DOUBLE,             -- guaranteed $M
    apy_cap_pct       DOUBLE,             -- APY as % of cap
    inflated_value    DOUBLE,             -- adjusted to current cap
    inflated_apy      DOUBLE,
    inflated_guaranteed DOUBLE,
    draft_year        INTEGER,
    draft_round       INTEGER,
    draft_overall     INTEGER,
    draft_team        VARCHAR,
    college           VARCHAR,
    PRIMARY KEY (otc_id, year_signed)
);

-- ============ COACHES ============

CREATE TABLE IF NOT EXISTS college_coaches (
    coach_id          VARCHAR,            -- synthesized: firstname_lastname
    first_name        VARCHAR,
    last_name         VARCHAR,
    full_name         VARCHAR,
    school            VARCHAR,
    season            INTEGER,
    games             INTEGER,
    wins              INTEGER,
    losses            INTEGER,
    ties              INTEGER,
    preseason_rank    INTEGER,
    postseason_rank   INTEGER,
    srs               DOUBLE,
    sp_overall        DOUBLE,
    sp_offense        DOUBLE,
    sp_defense        DOUBLE,
    PRIMARY KEY (coach_id, school, season)
);

-- Weekly NFL head coach: derived from import_schedules (one row per team-week
-- of a played game). Lets you detect mid-season HC changes — e.g. an interim
-- HC after a firing in week 8.
CREATE TABLE IF NOT EXISTS nfl_weekly_coaches (
    team              VARCHAR,
    season            INTEGER,
    week              INTEGER,
    season_type       VARCHAR,
    coach_id          VARCHAR,
    first_name        VARCHAR,
    last_name         VARCHAR,
    full_name         VARCHAR,
    PRIMARY KEY (team, season, week, season_type)
);

CREATE TABLE IF NOT EXISTS nfl_coaches (
    coach_id          VARCHAR,            -- synthesized: firstname_lastname
    first_name        VARCHAR,
    last_name         VARCHAR,
    full_name         VARCHAR,
    team              VARCHAR,
    season            INTEGER,
    role              VARCHAR,            -- HC / OC / DC / etc
    PRIMARY KEY (coach_id, team, season, role)
);

-- ============ JOIN TABLE ============

CREATE TABLE IF NOT EXISTS player_id_map (
    canonical_id      VARCHAR PRIMARY KEY,
    gsis_id           VARCHAR,
    pfr_id            VARCHAR,
    cfbd_id           VARCHAR,
    recruit_id        VARCHAR,
    full_name         VARCHAR,
    birth_date        DATE,
    college           VARCHAR,
    high_school       VARCHAR,
    confidence        VARCHAR              -- exact / fuzzy / manual
);

-- Helpful indexes
CREATE INDEX IF NOT EXISTS idx_nfl_stats_season ON nfl_player_stats(season);
CREATE INDEX IF NOT EXISTS idx_nfl_stats_player ON nfl_player_stats(gsis_id);
CREATE INDEX IF NOT EXISTS idx_cfb_stats_season ON college_player_stats(season);
CREATE INDEX IF NOT EXISTS idx_recruits_name ON recruits(name);
CREATE INDEX IF NOT EXISTS idx_nfl_players_name ON nfl_players(full_name);
CREATE INDEX IF NOT EXISTS idx_cfb_players_name ON college_players(full_name);
"""


# Race / ethnicity columns added via ALTER (so we can add them to existing
# databases without dropping data). Populated by 04c_infer_race.py.
_RACE_COLS = [
    ("race_white",        "DOUBLE"),
    ("race_black",        "DOUBLE"),
    ("race_api",          "DOUBLE"),
    ("race_aian",         "DOUBLE"),
    ("race_2prace",       "DOUBLE"),
    ("race_hispanic",     "DOUBLE"),
    ("race_pred",         "VARCHAR"),  # BIFSG argmax
    ("race_source",       "VARCHAR"),  # bifsg / surname / firstname / NULL
    ("race_pred_surname", "VARCHAR"),  # surname-only Census argmax
]
_RACE_TARGETS = [
    "nfl_players", "college_players", "recruits",
    "nfl_coaches", "college_coaches",
    "nfl_weekly_coaches",
]


def _add_race_columns(con):
    for table in _RACE_TARGETS:
        for col, dtype in _RACE_COLS:
            con.execute(
                f"ALTER TABLE {table} ADD COLUMN IF NOT EXISTS {col} {dtype}"
            )


def main():
    con = duckdb.connect(str(DB_PATH))
    con.execute(SCHEMA)
    _add_race_columns(con)
    tables = con.execute("SHOW TABLES").fetchall()
    print(f"Database ready at {DB_PATH}")
    print(f"Tables: {[t[0] for t in tables]}")
    con.close()


if __name__ == "__main__":
    main()

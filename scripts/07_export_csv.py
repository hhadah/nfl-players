"""Export merged datasets as CSVs to data/datasets/.

Each export joins the relevant raw tables so a downstream user can read one
flat file per topic without having to write SQL joins themselves. Re-run any
time the underlying DuckDB has been refreshed.

The headline export is `panel_player_team_week.csv` — a player-by-team-by-
season-by-week panel with weekly NFL player and team stats, NFL coaches,
contract info, and (constant within player) college career stats, last
college team-season context, college head coach, and recruiting profile.
"""
import re
import duckdb
from config import DB_PATH, DATA_DIR

OUT_DIR = DATA_DIR / "datasets"
OUT_DIR.mkdir(parents=True, exist_ok=True)


EXPORTS = {
    # ---- 1. Players master: recruit -> college -> NFL chain, one row per canonical_id ----
    "players_master.sql": """
        SELECT
            m.canonical_id,
            m.full_name,
            m.confidence,
            m.gsis_id, m.pfr_id, m.cfbd_id, m.recruit_id,
            -- Recruit tier
            r.year              AS recruit_year,
            r.stars             AS recruit_stars,
            r.rating            AS recruit_rating,
            r.ranking           AS recruit_national_rank,
            r.position          AS recruit_position,
            r.height_inches     AS recruit_height_in,
            r.weight_lbs        AS recruit_weight_lb,
            r.committed_to      AS recruit_committed_to,
            r.high_school,
            r.hometown_city,
            r.hometown_state,
            r.hometown_country,
            -- College tier
            cp.team             AS college_team,
            cp.position         AS college_position,
            cp.height_inches    AS college_height_in,
            cp.weight_lbs       AS college_weight_lb,
            cp.seasons_played   AS college_seasons,
            -- NFL tier
            np.position         AS nfl_position,
            np.college          AS nfl_listed_college,
            np.birth_date,
            np.height_inches    AS nfl_height_in,
            np.weight_lbs       AS nfl_weight_lb,
            np.draft_year,
            np.draft_round,
            np.draft_pick,
            np.draft_team,
            np.rookie_year,
            np.last_season,
            np.status           AS nfl_status,
            -- Combine
            nc.forty            AS combine_forty,
            nc.bench            AS combine_bench,
            nc.vertical         AS combine_vertical,
            nc.broad_jump       AS combine_broad_jump,
            nc.cone             AS combine_cone,
            nc.shuttle          AS combine_shuttle,
            -- Inferred race (Census 2010 surname). NFL row preferred, then
            -- recruit, then college player.
            COALESCE(np.race_pred,     r.race_pred,     cp.race_pred)     AS race_pred,
            COALESCE(np.race_white,    r.race_white,    cp.race_white)    AS race_white,
            COALESCE(np.race_black,    r.race_black,    cp.race_black)    AS race_black,
            COALESCE(np.race_api,      r.race_api,      cp.race_api)      AS race_api,
            COALESCE(np.race_aian,     r.race_aian,     cp.race_aian)     AS race_aian,
            COALESCE(np.race_2prace,   r.race_2prace,   cp.race_2prace)   AS race_2prace,
            COALESCE(np.race_hispanic, r.race_hispanic, cp.race_hispanic) AS race_hispanic
        FROM player_id_map m
        LEFT JOIN recruits r        ON r.recruit_id = m.recruit_id
        LEFT JOIN college_players cp ON cp.cfbd_id  = m.cfbd_id
        LEFT JOIN nfl_players np    ON np.gsis_id  = m.gsis_id
        LEFT JOIN nfl_combine nc    ON nc.pfr_id   = m.pfr_id
    """,

    # ---- 2. NFL player stats with name/college context ----
    "nfl_player_stats.sql": """
        SELECT
            s.gsis_id,
            np.full_name,
            np.college          AS nfl_listed_college,
            np.draft_year,
            np.draft_round,
            np.draft_pick,
            s.season, s.week, s.season_type, s.team, s.position,
            s.completions, s.attempts, s.passing_yards, s.passing_tds,
            s.interceptions, s.sacks, s.sack_yards,
            s.passing_air_yards, s.passing_yards_after_catch, s.passing_epa, s.dakota,
            s.carries, s.rushing_yards, s.rushing_tds, s.rushing_fumbles, s.rushing_epa,
            s.receptions, s.targets, s.receiving_yards, s.receiving_tds,
            s.receiving_air_yards, s.receiving_yards_after_catch, s.receiving_epa,
            s.fantasy_points, s.fantasy_points_ppr
        FROM nfl_player_stats s
        LEFT JOIN nfl_players np ON np.gsis_id = s.gsis_id
    """,

    # ---- 3. College player stats with name/hometown context ----
    "college_player_stats.sql": """
        SELECT
            s.cfbd_id,
            cp.full_name,
            cp.position,
            cp.home_city,
            cp.home_state,
            s.season, s.team, s.category, s.stat_type, s.stat_value
        FROM college_player_stats s
        LEFT JOIN college_players cp ON cp.cfbd_id = s.cfbd_id
    """,

    # ---- 4. Recruits with canonical_id and matched college/NFL IDs ----
    "recruits.sql": """
        SELECT
            r.recruit_id,
            m.canonical_id,
            m.cfbd_id, m.gsis_id, m.pfr_id,
            r.year, r.name, r.first_name, r.last_name, r.position,
            r.height_inches, r.weight_lbs,
            r.stars, r.rating, r.ranking, r.position_ranking, r.state_ranking,
            r.committed_to, r.high_school,
            r.hometown_city, r.hometown_state, r.hometown_country
        FROM recruits r
        LEFT JOIN player_id_map m ON m.recruit_id = r.recruit_id
    """,

    # ---- 5. NFL team-season records (week=0) with conference/division ----
    "nfl_team_seasons.sql": """
        SELECT
            ts.team,
            t.team_name,
            t.conference,
            t.division,
            ts.season, ts.season_type,
            ts.points_for, ts.points_against,
            (ts.points_for - ts.points_against) AS point_differential,
            ts.wins, ts.losses, ts.ties
        FROM nfl_team_stats ts
        LEFT JOIN nfl_teams t ON t.team_abbr = ts.team
        WHERE ts.week = 0
    """,

    # ---- 6. College team-season ratings with conference/division ----
    "college_team_seasons.sql": """
        SELECT
            cts.school,
            ct.mascot,
            ct.conference,
            ct.division,
            ct.classification,
            cts.season,
            cts.wins, cts.losses,
            cts.points_per_game, cts.points_allowed_per_game,
            cts.total_yards_pg, cts.yards_allowed_pg,
            cts.sp_plus_rating, cts.srs, cts.sos
        FROM college_team_stats cts
        LEFT JOIN college_teams ct ON ct.school = cts.school
    """,

    # ---- 7. NFL contracts with player name/position context ----
    "nfl_contracts.sql": """
        SELECT
            c.otc_id,
            c.gsis_id,
            c.player,
            np.full_name        AS nfl_full_name,
            c.position,
            c.team,
            c.is_active,
            c.year_signed, c.years,
            c.value, c.apy, c.guaranteed, c.apy_cap_pct,
            c.inflated_value, c.inflated_apy, c.inflated_guaranteed,
            c.draft_year, c.draft_round, c.draft_overall, c.draft_team,
            c.college
        FROM nfl_contracts c
        LEFT JOIN nfl_players np ON np.gsis_id = c.gsis_id
    """,
}


def _slug(s):
    """Snake_case-ify a stat label so it's safe as a SQL identifier."""
    s = re.sub(r"[^A-Za-z0-9]+", "_", str(s)).strip("_").lower()
    return s or "x"


def _college_pivot_sql(con):
    """Build a SUM-CASE pivot that turns (category, stat_type) long rows into
    wide columns named college_<category>_<stat_type>. The set of stat columns
    is read from the database so newly loaded categories show up automatically."""
    cats = con.execute(
        "SELECT DISTINCT category, stat_type FROM college_player_stats "
        "WHERE category IS NOT NULL AND stat_type IS NOT NULL "
        "ORDER BY category, stat_type"
    ).fetchall()
    cases = []
    for cat, stat in cats:
        col = f"college_{_slug(cat)}_{_slug(stat)}"
        cases.append(
            f"SUM(CASE WHEN category = '{cat}' AND stat_type = '{stat}' "
            f"THEN stat_value END) AS {col}"
        )
    body = ",\n            ".join(cases) if cases else "NULL AS no_stats"
    return f"""
        SELECT
            cfbd_id,
            {body}
        FROM college_player_stats
        WHERE cfbd_id IS NOT NULL
        GROUP BY cfbd_id
    """


def build_panel_sql(con):
    """Player-by-team-by-season-by-week panel.

    Base unit: one row of nfl_weekly_rosters — every player on the active
    roster each week (all positions, including OL / DL / DB / ST), regardless
    of whether they recorded any stats. Stats and everything else are LEFT
    JOINed so missing context becomes NULL rather than dropping the row.
    """
    college_pivot = _college_pivot_sql(con)
    return f"""
        WITH
        roster_base AS (
            SELECT
                gsis_id, season, week, season_type, team,
                position             AS roster_position,
                depth_chart_position AS roster_depth_chart_position,
                jersey_number,
                status               AS roster_status,
                years_exp
            FROM nfl_weekly_rosters
        ),
        weekly_player AS (
            SELECT *
            FROM nfl_player_stats
            WHERE week BETWEEN 1 AND 22
        ),
        weekly_team AS (
            SELECT
                team, season, week, season_type,
                points_for       AS team_points_for,
                points_against   AS team_points_against,
                total_yards      AS team_total_yards,
                pass_yards       AS team_pass_yards,
                rush_yards       AS team_rush_yards,
                turnovers        AS team_turnovers,
                yards_allowed    AS team_yards_allowed,
                wins             AS team_wins,
                losses           AS team_losses,
                ties             AS team_ties
            FROM nfl_team_stats
            WHERE week BETWEEN 1 AND 22
        ),
        -- nfl_coaches.team uses PFR 3-letter abbrs (GNB / KAN / LAR ...);
        -- nfl_player_stats uses nflverse 2-letter abbrs. Translate so the
        -- join hits. OC / DC are not on PFR's coaches.htm, so those columns
        -- stay NULL until a separate loader fills them.
        nfl_coach_pivot AS (
            SELECT
                CASE nc.team
                    WHEN 'GNB' THEN 'GB'
                    WHEN 'KAN' THEN 'KC'
                    WHEN 'NWE' THEN 'NE'
                    WHEN 'NOR' THEN 'NO'
                    WHEN 'SFO' THEN 'SF'
                    WHEN 'TAM' THEN 'TB'
                    WHEN 'SDG' THEN 'SD'
                    WHEN 'LVR' THEN 'LV'
                    WHEN 'LAR' THEN 'LA'
                    ELSE nc.team
                END AS team_abbr,
                nc.season,
                MAX(CASE WHEN nc.role = 'HC'  THEN nc.full_name END) AS team_head_coach,
                MAX(CASE WHEN nc.role = 'OC'  THEN nc.full_name END) AS team_offensive_coordinator,
                MAX(CASE WHEN nc.role = 'DC'  THEN nc.full_name END) AS team_defensive_coordinator,
                MAX(CASE WHEN nc.role = 'STC' THEN nc.full_name END) AS team_st_coordinator,
                MAX(CASE WHEN nc.role = 'GM'  THEN nc.full_name END) AS team_general_manager,
                MAX(CASE WHEN nc.role = 'HC'  THEN nc.race_pred END) AS team_head_coach_race_pred,
                MAX(CASE WHEN nc.role = 'OC'  THEN nc.race_pred END) AS team_offensive_coordinator_race_pred,
                MAX(CASE WHEN nc.role = 'DC'  THEN nc.race_pred END) AS team_defensive_coordinator_race_pred,
                MAX(CASE WHEN nc.role = 'STC' THEN nc.race_pred END) AS team_st_coordinator_race_pred,
                MAX(CASE WHEN nc.role = 'GM'  THEN nc.race_pred END) AS team_general_manager_race_pred
            FROM nfl_coaches nc
            GROUP BY 1, 2
        ),
        -- One contract per (gsis_id, season): the one whose term covers the
        -- season; tie-break to the most recently signed and then highest value.
        contract_active AS (
            SELECT
                p.gsis_id,
                p.season,
                c.year_signed   AS contract_year_signed,
                c.years         AS contract_years,
                c.value         AS contract_value,
                c.apy           AS contract_apy,
                c.guaranteed    AS contract_guaranteed,
                c.apy_cap_pct   AS contract_apy_cap_pct,
                c.team          AS contract_team,
                c.position      AS contract_position
            FROM (SELECT DISTINCT gsis_id, season FROM roster_base) p
            LEFT JOIN nfl_contracts c
                ON c.gsis_id = p.gsis_id
                AND c.year_signed <= p.season
                AND c.year_signed + COALESCE(c.years, 0) > p.season
            QUALIFY ROW_NUMBER() OVER (
                PARTITION BY p.gsis_id, p.season
                ORDER BY c.year_signed DESC NULLS LAST,
                         c.value       DESC NULLS LAST
            ) = 1
        ),
        college_career AS ({college_pivot}),
        -- player_id_map can have multiple rows per gsis_id (transfers, name
        -- collisions). Pick one: prefer rows with cfbd_id AND recruit_id,
        -- then any cfbd_id, then anything.
        id_map_unique AS (
            SELECT canonical_id, gsis_id, pfr_id, cfbd_id, recruit_id
            FROM player_id_map
            WHERE gsis_id IS NOT NULL
            QUALIFY ROW_NUMBER() OVER (
                PARTITION BY gsis_id
                ORDER BY (cfbd_id IS NOT NULL)    DESC,
                         (recruit_id IS NOT NULL) DESC,
                         canonical_id
            ) = 1
        ),
        -- Last college season per player (used to anchor team/coach context)
        college_last AS (
            SELECT
                cfbd_id,
                ARG_MAX(team, season) AS last_college_team,
                MAX(season)           AS last_college_season
            FROM college_player_stats
            WHERE cfbd_id IS NOT NULL
            GROUP BY cfbd_id
        ),
        college_team_ctx AS (
            SELECT
                cl.cfbd_id,
                cl.last_college_team,
                cl.last_college_season,
                ct.conference          AS college_team_conference,
                ct.division            AS college_team_division,
                ct.classification      AS college_team_classification,
                cts.wins               AS college_team_wins,
                cts.losses             AS college_team_losses,
                cts.points_per_game    AS college_team_points_per_game,
                cts.points_allowed_per_game AS college_team_points_allowed_per_game,
                cts.total_yards_pg     AS college_team_total_yards_pg,
                cts.yards_allowed_pg   AS college_team_yards_allowed_pg,
                cts.sp_plus_rating     AS college_team_sp_plus,
                cts.srs                AS college_team_srs,
                cts.sos                AS college_team_sos
            FROM college_last cl
            LEFT JOIN college_team_stats cts
                ON cts.school = cl.last_college_team
                AND cts.season = cl.last_college_season
            LEFT JOIN college_teams ct ON ct.school = cl.last_college_team
        ),
        college_coach_ctx AS (
            SELECT
                cl.cfbd_id,
                cc.full_name        AS college_head_coach,
                cc.wins             AS college_coach_wins,
                cc.losses           AS college_coach_losses,
                cc.preseason_rank   AS college_coach_preseason_rank,
                cc.postseason_rank  AS college_coach_postseason_rank,
                cc.srs              AS college_coach_srs,
                cc.sp_overall       AS college_coach_sp_overall,
                cc.sp_offense       AS college_coach_sp_offense,
                cc.sp_defense       AS college_coach_sp_defense
            FROM college_last cl
            LEFT JOIN college_coaches cc
                ON cc.school = cl.last_college_team
                AND cc.season = cl.last_college_season
            QUALIFY ROW_NUMBER() OVER (
                PARTITION BY cl.cfbd_id
                ORDER BY cc.games DESC NULLS LAST
            ) = 1
        )
        SELECT
            -- ---- Panel keys ----
            rb.gsis_id,
            rb.team,
            rb.season,
            rb.week,
            rb.season_type,
            -- ---- Roster snapshot (same week) ----
            rb.roster_position,
            rb.roster_depth_chart_position,
            rb.jersey_number,
            rb.roster_status,
            rb.years_exp,
            -- ---- Player bio (career-level) ----
            np.full_name,
            np.position           AS career_position,
            wp.position           AS stats_position,
            np.birth_date,
            np.height_inches,
            np.weight_lbs,
            np.college            AS nfl_listed_college,
            np.draft_year,
            np.draft_round,
            np.draft_pick,
            np.draft_team,
            np.rookie_year,
            np.last_season,
            -- ---- Inferred race (Census 2010 surname) ----
            np.race_pred,
            np.race_white, np.race_black, np.race_api,
            np.race_aian, np.race_2prace, np.race_hispanic,
            -- ---- NFL weekly player stats (NULL for non-skill positions) ----
            wp.completions, wp.attempts, wp.passing_yards, wp.passing_tds,
            wp.interceptions, wp.sacks, wp.sack_yards,
            wp.passing_air_yards, wp.passing_yards_after_catch,
            wp.passing_epa, wp.dakota,
            wp.carries, wp.rushing_yards, wp.rushing_tds,
            wp.rushing_fumbles, wp.rushing_epa,
            wp.receptions, wp.targets, wp.receiving_yards, wp.receiving_tds,
            wp.receiving_air_yards, wp.receiving_yards_after_catch,
            wp.receiving_epa,
            wp.fantasy_points, wp.fantasy_points_ppr,
            -- ---- NFL weekly team stats ----
            wt.team_points_for, wt.team_points_against,
            wt.team_total_yards, wt.team_pass_yards, wt.team_rush_yards,
            wt.team_turnovers, wt.team_yards_allowed,
            wt.team_wins, wt.team_losses, wt.team_ties,
            -- ---- Team coaches (season-level; same value for every week) ----
            ncp.team_head_coach,
            ncp.team_head_coach_race_pred,
            ncp.team_offensive_coordinator,
            ncp.team_offensive_coordinator_race_pred,
            ncp.team_defensive_coordinator,
            ncp.team_defensive_coordinator_race_pred,
            ncp.team_st_coordinator,
            ncp.team_st_coordinator_race_pred,
            ncp.team_general_manager,
            ncp.team_general_manager_race_pred,
            -- ---- Contract active that season ----
            con.contract_year_signed, con.contract_years,
            con.contract_value, con.contract_apy, con.contract_guaranteed,
            con.contract_apy_cap_pct, con.contract_team, con.contract_position,
            -- ---- ID bridges to college / recruiting ----
            m.canonical_id, m.cfbd_id, m.recruit_id,
            -- ---- Recruiting (one row per player) ----
            r.year             AS recruit_year,
            r.position         AS recruit_position,
            r.height_inches    AS recruit_height_in,
            r.weight_lbs       AS recruit_weight_lb,
            r.stars            AS recruit_stars,
            r.rating           AS recruit_rating,
            r.ranking          AS recruit_national_rank,
            r.position_ranking AS recruit_position_rank,
            r.state_ranking    AS recruit_state_rank,
            r.committed_to     AS recruit_committed_to,
            r.high_school,
            r.hometown_city,
            r.hometown_state,
            r.hometown_country,
            -- ---- College team-season context (last college season) ----
            ctx.last_college_team,
            ctx.last_college_season,
            ctx.college_team_conference,
            ctx.college_team_division,
            ctx.college_team_classification,
            ctx.college_team_wins, ctx.college_team_losses,
            ctx.college_team_points_per_game,
            ctx.college_team_points_allowed_per_game,
            ctx.college_team_total_yards_pg,
            ctx.college_team_yards_allowed_pg,
            ctx.college_team_sp_plus,
            ctx.college_team_srs,
            ctx.college_team_sos,
            -- ---- College head coach (last college season) ----
            cco.college_head_coach,
            cco.college_coach_wins, cco.college_coach_losses,
            cco.college_coach_preseason_rank, cco.college_coach_postseason_rank,
            cco.college_coach_srs, cco.college_coach_sp_overall,
            cco.college_coach_sp_offense, cco.college_coach_sp_defense,
            -- ---- College career stats (wide, summed across all college years) ----
            cc.* EXCLUDE (cfbd_id)
        FROM roster_base rb
        LEFT JOIN nfl_players      np  ON np.gsis_id   = rb.gsis_id
        LEFT JOIN weekly_player    wp  ON wp.gsis_id    = rb.gsis_id
                                       AND wp.season     = rb.season
                                       AND wp.week       = rb.week
                                       AND wp.season_type= rb.season_type
                                       AND wp.team       = rb.team
        LEFT JOIN weekly_team      wt  ON wt.team       = rb.team
                                       AND wt.season     = rb.season
                                       AND wt.week       = rb.week
                                       AND wt.season_type= rb.season_type
        LEFT JOIN nfl_coach_pivot  ncp ON ncp.team_abbr = rb.team
                                       AND ncp.season   = rb.season
        LEFT JOIN contract_active  con ON con.gsis_id   = rb.gsis_id
                                       AND con.season    = rb.season
        LEFT JOIN id_map_unique    m   ON m.gsis_id     = rb.gsis_id
        LEFT JOIN recruits         r   ON r.recruit_id  = m.recruit_id
        LEFT JOIN college_career   cc  ON cc.cfbd_id    = m.cfbd_id
        LEFT JOIN college_team_ctx ctx ON ctx.cfbd_id   = m.cfbd_id
        LEFT JOIN college_coach_ctx cco ON cco.cfbd_id  = m.cfbd_id
    """


def export_panel(con):
    out_path = OUT_DIR / "panel_player_team_week.csv"
    print(f"Exporting {out_path.name} ...", end=" ", flush=True)
    sql = build_panel_sql(con)
    con.execute(
        f"COPY ({sql}) TO '{out_path}' (FORMAT CSV, HEADER, DELIMITER ',')"
    )
    rows = con.execute(f"SELECT COUNT(*) FROM ({sql})").fetchone()[0]
    size_mb = out_path.stat().st_size / 1_000_000
    print(f"{rows:,} rows, {size_mb:.1f} MB")


def main():
    con = duckdb.connect(str(DB_PATH), read_only=True)
    for name, sql in EXPORTS.items():
        out_path = OUT_DIR / name.replace(".sql", ".csv")
        print(f"Exporting {out_path.name} ...", end=" ", flush=True)
        con.execute(
            f"COPY ({sql}) TO '{out_path}' (FORMAT CSV, HEADER, DELIMITER ',')"
        )
        rows = con.execute(f"SELECT COUNT(*) FROM ({sql})").fetchone()[0]
        size_mb = out_path.stat().st_size / 1_000_000
        print(f"{rows:,} rows, {size_mb:.1f} MB")
    export_panel(con)
    con.close()
    print(f"\nAll CSVs written to {OUT_DIR}")


if __name__ == "__main__":
    main()

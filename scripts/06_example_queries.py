"""Example queries demonstrating how to use the joined database.

Run after 01-05 have populated everything.
"""
import duckdb
from config import DB_PATH


def query(con, label, sql):
    print(f"\n=== {label} ===")
    print(con.execute(sql).df().to_string(index=False))


def main():
    con = duckdb.connect(str(DB_PATH))

    # 1. Full player profile: NFL + college + recruiting + combine
    query(con, "Full profile for one player", """
        SELECT
            m.full_name,
            m.high_school,
            r.stars             AS recruit_stars,
            r.rating            AS recruit_rating,
            r.committed_to      AS college,
            np.draft_year, np.draft_round, np.draft_pick,
            nc.forty            AS combine_forty,
            nc.vertical         AS combine_vertical,
            nc.bench            AS combine_bench
        FROM player_id_map m
        LEFT JOIN nfl_players np ON np.gsis_id = m.gsis_id
        LEFT JOIN recruits r     ON r.recruit_id = m.recruit_id
        LEFT JOIN nfl_combine nc ON nc.pfr_id = m.pfr_id
        WHERE m.full_name = 'Patrick Mahomes'
        LIMIT 5
    """)

    # 2. Compare college production to NFL production for QBs
    query(con, "QB college vs NFL passing yards (top 10 by NFL yards)", """
        WITH nfl_pass AS (
            SELECT gsis_id, SUM(passing_yards) AS nfl_yards
            FROM nfl_player_stats
            WHERE week = 0 AND season_type = 'REG'
            GROUP BY gsis_id
        ),
        cfb_pass AS (
            SELECT cfbd_id, SUM(stat_value) AS cfb_yards
            FROM college_player_stats
            WHERE category = 'passing' AND stat_type = 'YDS'
            GROUP BY cfbd_id
        )
        SELECT
            m.full_name, m.college,
            ROUND(c.cfb_yards) AS college_passing_yds,
            ROUND(n.nfl_yards) AS nfl_passing_yds
        FROM player_id_map m
        JOIN nfl_pass n ON n.gsis_id = m.gsis_id
        LEFT JOIN cfb_pass c ON c.cfbd_id = m.cfbd_id
        ORDER BY n.nfl_yards DESC
        LIMIT 10
    """)

    # 3. Recruit stars vs NFL outcome (do 5-stars actually pan out?)
    query(con, "NFL draft outcomes by recruit star rating", """
        SELECT
            r.stars,
            COUNT(*)                                          AS recruits,
            COUNT(np.gsis_id)                                 AS made_nfl,
            ROUND(100.0 * COUNT(np.gsis_id) / COUNT(*), 1)    AS pct_to_nfl,
            ROUND(AVG(np.draft_pick), 1)                      AS avg_draft_pick
        FROM recruits r
        LEFT JOIN player_id_map m ON m.recruit_id = r.recruit_id
        LEFT JOIN nfl_players np  ON np.gsis_id = m.gsis_id
        WHERE r.stars BETWEEN 2 AND 5
        GROUP BY r.stars
        ORDER BY r.stars DESC
    """)

    # 4. Player + their college team's strength (SP+) when they played
    query(con, "Top 2020 NFL rookies and their college team's SP+", """
        SELECT
            m.full_name, m.college,
            np.draft_year, np.draft_round, np.draft_pick,
            ROUND(cts.sp_plus_rating, 2) AS college_team_sp_plus
        FROM player_id_map m
        JOIN nfl_players np ON np.gsis_id = m.gsis_id
        LEFT JOIN college_team_stats cts
               ON cts.school = m.college AND cts.season = np.draft_year - 1
        WHERE np.draft_year = 2020 AND np.draft_round = 1
        ORDER BY np.draft_pick
    """)

    # 5. NFL team stats joined to player contributions
    query(con, "2023 team passing yards from QB stats vs team result", """
        SELECT
            np.team,
            ROUND(SUM(np.passing_yards)) AS qb_passing_yards,
            ts.wins, ts.losses
        FROM nfl_player_stats np
        JOIN nfl_team_stats ts
              ON ts.team = np.team AND ts.season = np.season
              AND ts.week = 0 AND ts.season_type = 'REG'
        WHERE np.season = 2023 AND np.week = 0 AND np.season_type = 'REG'
              AND np.passing_yards > 0
        GROUP BY np.team, ts.wins, ts.losses
        ORDER BY qb_passing_yards DESC
        LIMIT 10
    """)

    # 6. Contract value vs production: top QB contracts and their yardage
    query(con, "Top active QB contracts vs 2023 passing yards", """
        SELECT
            c.player, c.team, c.year_signed, c.years,
            ROUND(c.value, 1)     AS total_value_M,
            ROUND(c.apy, 1)       AS apy_M,
            ROUND(c.apy_cap_pct, 1) AS pct_of_cap,
            ROUND(SUM(s.passing_yards)) AS pass_yds_2023
        FROM nfl_contracts c
        LEFT JOIN nfl_player_stats s
               ON s.gsis_id = c.gsis_id
               AND s.season = 2023 AND s.week = 0 AND s.season_type = 'REG'
        WHERE c.is_active = TRUE AND c.position = 'QB'
        GROUP BY c.player, c.team, c.year_signed, c.years,
                 c.value, c.apy, c.apy_cap_pct
        ORDER BY c.apy DESC
        LIMIT 10
    """)

    con.close()


if __name__ == "__main__":
    main()

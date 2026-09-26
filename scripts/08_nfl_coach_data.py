"""Team-week stats × NFL coaching staff × inferred race × diversity index.

Produces data/datasets/nfl_team_coach_week.csv with one row per
(team, season, week, season_type). Each row carries:

  * Team weekly results (points / wins / losses / yards from nfl_team_stats).

  * Weekly head coach: pulled from nfl_weekly_coaches (one row per game,
    derived from import_schedules' home_coach/away_coach). This is what lets
    us detect mid-season HC changes — `head_coach` may differ from one row
    to the next within the same team-season. Race columns
    (`head_coach_race_pred`, `head_coach_race_white`, ...) come from the
    weekly row's BIFSG inference.

  * `coach_change` (boolean): 1 if this row's head coach is different from
    the previous played week's head coach for the same team-season; 0 if
    same; NULL on the first played week of the team-season. This is the
    column to look at when scanning for mid-season firings.

  * Senior staff for the season (off_coordinator, def_coordinator,
    st_coordinator, general_manager): season-level only because no free
    weekly source publishes coordinator changes mid-year. Each named role
    gets a `<role>_race_pred` column plus the six race-percentage columns.

  * `staff_size` — count of every coach we have on file for the team-season
    (HC / OC / DC / STC / GM / "Other Notable Asst." position coaches).

  * Diversity index over the FULL staff for that team-season:
      div_share_white      — share of staff predicted white (BIFSG argmax)
      div_share_nonwhite   — 1 - div_share_white, over staff with predictions
      div_blau             — Blau / Gini-Simpson, 1 - Σ p_i².  Computed from
                             BIFSG probability VECTORS averaged across the
                             staff so within-coach uncertainty is preserved.
                             Range [0, 1 - 1/k].
      div_shannon          — Shannon entropy in nats, -Σ p_i ln p_i.
      n_coaches_with_race  — denominator used for diversity calcs.
"""
import subprocess
import sys
from pathlib import Path

import duckdb

from config import DB_PATH, DATA_DIR
from race_utils import infer_race

OUT_DIR = DATA_DIR / "datasets"
OUT_DIR.mkdir(parents=True, exist_ok=True)
OUT_PATH = OUT_DIR / "nfl_team_coach_week.csv"

HERE = Path(__file__).resolve().parent

# PFR uses 3-letter abbrs; nflverse uses 2-letter. Inline as a CASE so we can
# reuse the same translation in two SQL CTEs.
PFR_TO_NFLVERSE = """
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
    END
"""


def _ensure_coaches_loaded():
    """If nfl_coaches is empty, run the PFR scraper script. Idempotent.
    Opens / closes its own short-lived connection so the scraper subprocess
    isn't blocked by a held write lock."""
    con = duckdb.connect(str(DB_PATH), read_only=True)
    n = con.execute("SELECT COUNT(*) FROM nfl_coaches").fetchone()[0]
    con.close()
    if n > 0:
        return
    print("nfl_coaches is empty — running 02b_load_nfl_coaches.py first...")
    rc = subprocess.run(
        [sys.executable, str(HERE / "02b_load_nfl_coaches.py")],
        cwd=HERE,
    ).returncode
    if rc != 0:
        print(f"Coach scrape exited rc={rc}; coach columns will be NULL where unmatched.")


def _refresh_coach_race(con):
    """Re-run BIFSG race inference on nfl_coaches (idempotent)."""
    df = con.execute(
        "SELECT coach_id, team, season, role, first_name, last_name "
        "FROM nfl_coaches"
    ).df()
    if df.empty:
        return
    enriched = infer_race(df, first_col="first_name", last_col="last_name")
    keep = ["coach_id", "team", "season", "role",
            "race_white", "race_black", "race_api", "race_aian",
            "race_2prace", "race_hispanic", "race_pred", "race_source"]
    con.register("coach_race_staging", enriched[keep])
    con.execute("""
        UPDATE nfl_coaches SET
          race_white    = coach_race_staging.race_white,
          race_black    = coach_race_staging.race_black,
          race_api      = coach_race_staging.race_api,
          race_aian     = coach_race_staging.race_aian,
          race_2prace   = coach_race_staging.race_2prace,
          race_hispanic = coach_race_staging.race_hispanic,
          race_pred     = coach_race_staging.race_pred,
          race_source   = coach_race_staging.race_source
        FROM coach_race_staging
        WHERE nfl_coaches.coach_id = coach_race_staging.coach_id
          AND nfl_coaches.team     = coach_race_staging.team
          AND nfl_coaches.season   = coach_race_staging.season
          AND nfl_coaches.role     = coach_race_staging.role
    """)
    con.unregister("coach_race_staging")


# Pivot one row per (team_abbr, season) carrying senior staff. HC is now
# pulled from nfl_weekly_coaches separately, so it's NOT in this list.
NAMED_ROLES = [
    ("OC",  "off_coordinator"),
    ("DC",  "def_coordinator"),
    ("STC", "st_coordinator"),
    ("GM",  "general_manager"),
]


def _named_pivot_columns():
    parts = []
    for role, alias in NAMED_ROLES:
        parts.append(
            f"MAX(CASE WHEN role = '{role}' THEN full_name         END) AS {alias}"
        )
        parts.append(
            f"MAX(CASE WHEN role = '{role}' THEN race_pred         END) AS {alias}_race_pred"
        )
        parts.append(
            f"MAX(CASE WHEN role = '{role}' THEN race_pred_surname END) AS {alias}_race_pred_surname"
        )
        for race in ["white", "black", "api", "aian", "2prace", "hispanic"]:
            parts.append(
                f"MAX(CASE WHEN role = '{role}' THEN race_{race} "
                f"END) AS {alias}_race_{race}"
            )
    return ",\n            ".join(parts)


# Diversity over the FULL staff, computed from BIFSG probability vectors.
# We average each race's per-coach probability across the staff (with the
# probability expressed as 0..1, not 0..100), giving a single race
# distribution for the team-season. Blau and Shannon are computed from that.
DIVERSITY_SQL = """
    WITH staff AS (
        SELECT
            {pfr_to_nfl} AS team_abbr,
            nc.season,
            nc.race_pred,
            nc.race_white    / 100.0 AS p_white,
            nc.race_black    / 100.0 AS p_black,
            nc.race_api      / 100.0 AS p_api,
            nc.race_aian     / 100.0 AS p_aian,
            nc.race_2prace   / 100.0 AS p_2prace,
            nc.race_hispanic / 100.0 AS p_hispanic
        FROM nfl_coaches nc
    ),
    -- Per (team, season): staff size, share-white, and the average
    -- per-race probability across coaches that have a prediction.
    agg AS (
        SELECT
            team_abbr, season,
            COUNT(*)                                            AS staff_size,
            SUM(CASE WHEN race_pred IS NOT NULL THEN 1 ELSE 0 END)
                                                                AS n_with_race,
            SUM(CASE WHEN race_pred = 'white'    THEN 1 ELSE 0 END) AS n_white,
            AVG(p_white)    FILTER (WHERE p_white    IS NOT NULL) AS avg_p_white,
            AVG(p_black)    FILTER (WHERE p_black    IS NOT NULL) AS avg_p_black,
            AVG(p_api)      FILTER (WHERE p_api      IS NOT NULL) AS avg_p_api,
            AVG(p_aian)     FILTER (WHERE p_aian     IS NOT NULL) AS avg_p_aian,
            AVG(p_2prace)   FILTER (WHERE p_2prace   IS NOT NULL) AS avg_p_2prace,
            AVG(p_hispanic) FILTER (WHERE p_hispanic IS NOT NULL) AS avg_p_hispanic
        FROM staff
        GROUP BY team_abbr, season
    )
    SELECT
        team_abbr,
        season,
        staff_size,
        n_with_race AS n_coaches_with_race,
        CAST(n_white AS DOUBLE) / NULLIF(n_with_race, 0) AS div_share_white,
        1.0 - CAST(n_white AS DOUBLE) / NULLIF(n_with_race, 0) AS div_share_nonwhite,
        -- Blau / Gini–Simpson:  1 - Σ p_i²   (using row-wise renormalised probs)
        1 - (
            POW(COALESCE(avg_p_white,    0), 2) +
            POW(COALESCE(avg_p_black,    0), 2) +
            POW(COALESCE(avg_p_api,      0), 2) +
            POW(COALESCE(avg_p_aian,     0), 2) +
            POW(COALESCE(avg_p_2prace,   0), 2) +
            POW(COALESCE(avg_p_hispanic, 0), 2)
        ) / NULLIF(POW(
            COALESCE(avg_p_white,    0) + COALESCE(avg_p_black,    0) +
            COALESCE(avg_p_api,      0) + COALESCE(avg_p_aian,     0) +
            COALESCE(avg_p_2prace,   0) + COALESCE(avg_p_hispanic, 0)
        , 2), 0)                                            AS div_blau,
        -- Shannon entropy in nats:  -Σ p_i ln p_i  (also row-wise renormalised)
        -1 * (
            CASE WHEN COALESCE(avg_p_white,    0) > 0 THEN avg_p_white    * LN(avg_p_white)    ELSE 0 END +
            CASE WHEN COALESCE(avg_p_black,    0) > 0 THEN avg_p_black    * LN(avg_p_black)    ELSE 0 END +
            CASE WHEN COALESCE(avg_p_api,      0) > 0 THEN avg_p_api      * LN(avg_p_api)      ELSE 0 END +
            CASE WHEN COALESCE(avg_p_aian,     0) > 0 THEN avg_p_aian     * LN(avg_p_aian)     ELSE 0 END +
            CASE WHEN COALESCE(avg_p_2prace,   0) > 0 THEN avg_p_2prace   * LN(avg_p_2prace)   ELSE 0 END +
            CASE WHEN COALESCE(avg_p_hispanic, 0) > 0 THEN avg_p_hispanic * LN(avg_p_hispanic) ELSE 0 END
        )                                                   AS div_shannon
    FROM agg
"""


def build_dataset_sql():
    named_pivot = _named_pivot_columns()
    diversity = DIVERSITY_SQL.format(pfr_to_nfl=PFR_TO_NFLVERSE)

    # Season-level pivot of OC / DC / STC / GM (HC is now weekly).
    coach_pivot = f"""
        WITH coaches_by_abbr AS (
            SELECT
                {PFR_TO_NFLVERSE} AS team_abbr,
                nc.season, nc.role, nc.full_name,
                nc.race_pred, nc.race_pred_surname,
                nc.race_white, nc.race_black,
                nc.race_api, nc.race_aian, nc.race_2prace, nc.race_hispanic
            FROM nfl_coaches nc
        )
        SELECT
            team_abbr,
            season,
            {named_pivot}
        FROM coaches_by_abbr
        GROUP BY team_abbr, season
    """

    # Weekly HC with coach_change flag.
    #
    # `coach_change`:
    #   1 if the current row's coach_id differs from the prior played-week's
    #   coach_id for the same team+season, 0 if same, NULL on the first row
    #   of the team-season (no prior to compare against). We order by week
    #   then season_type so REG weeks come before POST weeks within the same
    #   season.
    weekly_hc = """
        SELECT
            wc.team,
            wc.season,
            wc.week,
            wc.season_type,
            wc.full_name             AS head_coach,
            wc.coach_id              AS head_coach_id,
            wc.race_pred             AS head_coach_race_pred,
            wc.race_pred_surname     AS head_coach_race_pred_surname,
            wc.race_white            AS head_coach_race_white,
            wc.race_black            AS head_coach_race_black,
            wc.race_api              AS head_coach_race_api,
            wc.race_aian             AS head_coach_race_aian,
            wc.race_2prace           AS head_coach_race_2prace,
            wc.race_hispanic         AS head_coach_race_hispanic,
            CASE
                WHEN LAG(wc.coach_id) OVER (
                    PARTITION BY wc.team, wc.season
                    ORDER BY (wc.season_type = 'POST'), wc.week
                ) IS NULL THEN NULL
                WHEN LAG(wc.coach_id) OVER (
                    PARTITION BY wc.team, wc.season
                    ORDER BY (wc.season_type = 'POST'), wc.week
                ) = wc.coach_id THEN 0
                ELSE 1
            END AS coach_change
        FROM nfl_weekly_coaches wc
    """

    return f"""
        WITH coach_pivot AS ({coach_pivot}),
             staff_diversity AS ({diversity}),
             weekly_hc AS ({weekly_hc})
        SELECT
            ts.team,
            t.team_name,
            t.conference,
            t.division,
            ts.season,
            ts.week,
            ts.season_type,
            ts.points_for,
            ts.points_against,
            (ts.points_for - ts.points_against) AS point_differential,
            ts.total_yards,
            ts.pass_yards,
            ts.rush_yards,
            ts.turnovers,
            ts.yards_allowed,
            ts.wins,
            ts.losses,
            ts.ties,
            wh.head_coach,
            wh.head_coach_id,
            wh.coach_change,
            wh.head_coach_race_pred,
            wh.head_coach_race_pred_surname,
            wh.head_coach_race_white, wh.head_coach_race_black,
            wh.head_coach_race_api,   wh.head_coach_race_aian,
            wh.head_coach_race_2prace, wh.head_coach_race_hispanic,
            cp.* EXCLUDE (team_abbr, season),
            sd.staff_size,
            sd.n_coaches_with_race,
            sd.div_share_white,
            sd.div_share_nonwhite,
            sd.div_blau,
            sd.div_shannon
        FROM nfl_team_stats ts
        LEFT JOIN nfl_teams       t  ON t.team_abbr = ts.team
        LEFT JOIN weekly_hc       wh ON wh.team        = ts.team
                                    AND wh.season      = ts.season
                                    AND wh.week        = ts.week
                                    AND wh.season_type = ts.season_type
        LEFT JOIN coach_pivot     cp ON cp.team_abbr = ts.team
                                    AND cp.season    = ts.season
        LEFT JOIN staff_diversity sd ON sd.team_abbr = ts.team
                                    AND sd.season    = ts.season
        ORDER BY ts.season, ts.team, ts.season_type, ts.week
    """


def main():
    _ensure_coaches_loaded()
    con = duckdb.connect(str(DB_PATH))
    _refresh_coach_race(con)

    sql = build_dataset_sql()
    print(f"Exporting {OUT_PATH.name} ...", end=" ", flush=True)
    con.execute(
        f"COPY ({sql}) TO '{OUT_PATH}' (FORMAT CSV, HEADER, DELIMITER ',')"
    )
    rows = con.execute(f"SELECT COUNT(*) FROM ({sql})").fetchone()[0]
    size_mb = OUT_PATH.stat().st_size / 1_000_000
    print(f"{rows:,} rows, {size_mb:.1f} MB")

    coverage = con.execute(f"""
        SELECT
            COUNT(*) AS rows,
            SUM(CASE WHEN head_coach        IS NOT NULL THEN 1 ELSE 0 END) AS with_hc,
            SUM(CASE WHEN off_coordinator   IS NOT NULL THEN 1 ELSE 0 END) AS with_oc,
            SUM(CASE WHEN def_coordinator   IS NOT NULL THEN 1 ELSE 0 END) AS with_dc,
            SUM(CASE WHEN st_coordinator    IS NOT NULL THEN 1 ELSE 0 END) AS with_stc,
            SUM(CASE WHEN general_manager   IS NOT NULL THEN 1 ELSE 0 END) AS with_gm,
            SUM(CASE WHEN coach_change = 1  THEN 1 ELSE 0 END) AS coach_changes,
            AVG(staff_size)         AS avg_staff_size,
            AVG(div_share_nonwhite) AS avg_share_nonwhite,
            AVG(div_blau)           AS avg_blau
        FROM ({sql})
    """).df()
    print("\nCoverage / staff diversity summary:")
    print(coverage.to_string(index=False))

    # Show every team-season that had a mid-season coaching change so the
    # user can sanity-check the flag. The LAG must be computed BEFORE the
    # coach_change filter, otherwise it only sees the change rows themselves
    # and there's no previous row to lag onto.
    changes = con.execute(f"""
        WITH d AS (
            SELECT season, team, week, season_type, head_coach, coach_change,
                   LAG(head_coach) OVER (
                       PARTITION BY team, season
                       ORDER BY (season_type = 'POST'), week
                   ) AS prior_coach
            FROM ({sql})
        )
        SELECT season, team, week, season_type, prior_coach, head_coach
        FROM d
        WHERE coach_change = 1
        ORDER BY season, team, week
    """).df()
    if not changes.empty:
        print(f"\nDetected {len(changes)} mid-season HC change(s):")
        print(changes.to_string(index=False))
    else:
        print("\nNo mid-season HC changes detected (or nfl_weekly_coaches is empty).")
    con.close()


if __name__ == "__main__":
    main()

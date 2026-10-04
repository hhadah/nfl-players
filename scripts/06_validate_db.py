"""Cross-table validation of the DuckDB; the last step of 99_run_all.py.

Checks key uniqueness, franchise-season completeness, referential integrity
between tables, and value ranges. Prints every check; exits non-zero if any
CRITICAL check fails (WARN checks are reported but do not stop the run).
Tables built by later steps (player_xwalk, player_wiki_signals) are checked
only when present.
"""
import sys

from common import connect
from config import LAST_SEASON, STAFF_SEASONS

# table -> unique key
KEYS = {
    "franchise_seasons": "franchise_id, season",
    "nfl_players": "gsis_id",
    "nfl_draft_picks": "season, pick",
    "nfl_contracts": "contract_id",
    "nfl_contract_years": "otc_id, year",
    "nfl_schedules": "game_id",
    "nfl_team_games": "game_id, franchise_id",
    "nfl_team_seasons": "franchise_id, season",
    "nfl_player_stats_week": "gsis_id, game_id",
    "nfl_player_stats_season": "gsis_id, season, season_type",
    "staff_snapshots": "franchise_id, season, snapshot",
    "staff_team_season": "franchise_id, season, person_id, role_std",
    "staff_persons": "person_id",
    "nfl_hc_history": "franchise_id, season",
    "staff_person_wiki_signals": "person_id",
    "college_players": "season, player_id, team",
    "recruits": "recruit_id",
    "cfbd_draft_picks": "season, overall",
    "race_bifsg": "entity, entity_id",
    "player_xwalk": "gsis_id",
    "player_college_xwalk": "gsis_id, player_id",
    "player_recruit_xwalk": "gsis_id, recruit_id",
    "player_wiki_signals": "gsis_id",
}

# Team tables whose franchise_id must be fully populated
FRANCHISE_TABLES = ["nfl_team_games", "nfl_team_seasons", "nfl_rosters_weekly",
                    "nfl_player_stats_week", "staff_team_season", "staff_snapshots"]

results = []


def check(level, name, ok, detail=""):
    results.append((level, name, ok))
    status = "ok  " if ok else ("FAIL" if level == "CRITICAL" else "warn")
    print(f"  [{status}] {level:8s} {name}{': ' + detail if detail else ''}")


def exists(con, table):
    return con.execute("SELECT count(*) FROM information_schema.tables "
                       "WHERE table_name = ?", [table]).fetchone()[0] > 0


def scalar(con, sql):
    return con.execute(sql).fetchone()[0]


def main():
    con = connect(read_only=True)

    print("Tables and keys")
    for table, key in KEYS.items():
        if not exists(con, table):
            level = "WARN" if table.startswith("player_") else "CRITICAL"
            check(level, f"{table} exists", False)
            continue
        n = scalar(con, f"SELECT count(*) FROM {table}")
        dups = scalar(con, f"SELECT count(*) FROM (SELECT {key} FROM {table} "
                           f"GROUP BY {key} HAVING count(*) > 1)")
        check("CRITICAL", f"{table} non-empty and unique on ({key})",
              n > 0 and dups == 0, f"{n:,} rows, {dups:,} duplicate keys")

    print("Team keys")
    for table in FRANCHISE_TABLES:
        if not exists(con, table):
            continue
        cols = {r[0] for r in con.execute(f"DESCRIBE {table}").fetchall()}
        if "team" in cols:
            # An unmapped team code is a bug; a team missing in the source is not
            unmapped = scalar(con, f"SELECT count(*) FROM {table} "
                                   f"WHERE franchise_id IS NULL AND team IS NOT NULL")
            no_team = scalar(con, f"SELECT count(*) FROM {table} WHERE team IS NULL")
            check("CRITICAL", f"{table}: every team code maps to a franchise_id",
                  unmapped == 0, f"{unmapped:,} unmapped")
            check("WARN", f"{table}: source rows without a team", no_team == 0, f"{no_team:,}")
        else:
            nulls = scalar(con, f"SELECT count(*) FROM {table} WHERE franchise_id IS NULL")
            check("CRITICAL", f"{table}.franchise_id populated", nulls == 0, f"{nulls:,} NULL")

    print("Franchise-season completeness")
    missing = scalar(con, """SELECT count(*) FROM franchise_seasons f
                             ANTI JOIN nfl_team_seasons t USING (franchise_id, season)""")
    check("CRITICAL", "every franchise-season has team outcomes", missing == 0, f"{missing} missing")
    missing = scalar(con, f"""SELECT count(*) FROM franchise_seasons f
        WHERE f.season BETWEEN {STAFF_SEASONS[0]} AND {LAST_SEASON}
          AND NOT EXISTS (SELECT 1 FROM staff_team_season s
                          WHERE s.franchise_id = f.franchise_id AND s.season = f.season
                            AND s.role_std = 'HC')""")
    check("CRITICAL", f"every franchise-season {STAFF_SEASONS[0]}-{LAST_SEASON} has a staff "
                      "with a head coach", missing == 0, f"{missing} missing")
    thin = scalar(con, f"""SELECT count(*) FROM (
        SELECT franchise_id, season, count(DISTINCT person_id) n FROM staff_team_season
        WHERE season >= {STAFF_SEASONS[0]} GROUP BY 1, 2 HAVING n < 15)""")
    check("WARN", "template-era staffs list at least 15 people", thin == 0,
          f"{thin} team-seasons below 15")

    print("Snapshot timing")
    bad = scalar(con, """SELECT count(*) FROM staff_snapshots
        WHERE source = 'staff_template' AND
          (target_date IS NULL OR revision_timestamp IS NULL
           OR revision_timestamp > timezone('UTC', target_date::TIMESTAMP))""")
    check("CRITICAL", "template revisions precede their UTC target",
          bad == 0, f"{bad} future or undated snapshots")
    bad = scalar(con, """WITH openers AS (
        SELECT franchise_id, season, min(CAST(gameday AS DATE)) AS opener
        FROM nfl_team_games WHERE season_type = 'REG' GROUP BY 1, 2)
        SELECT count(*) FROM staff_snapshots s
        LEFT JOIN openers o USING (franchise_id, season)
        WHERE s.source = 'staff_template' AND s.snapshot = 'preseason'
          AND (o.opener IS NULL OR s.target_date IS DISTINCT FROM o.opener)""")
    check("CRITICAL", "opening staff targets equal each team's first REG game",
          bad == 0, f"{bad} mismatches")
    bad = scalar(con, """SELECT count(*) FROM staff_team_season s
        WHERE s.in_preseason AND NOT EXISTS (
          SELECT 1 FROM staff_entries e
          WHERE e.franchise_id = s.franchise_id AND e.season = s.season
            AND e.person_id = s.person_id AND e.role_std = s.role_std
            AND e.snapshot = 'preseason')""")
    check("CRITICAL", "opening roles have opening-snapshot provenance",
          bad == 0, f"{bad} roles without opening entries")

    if exists(con, "nfl_hc_history"):
        print("Historical head-coach coverage")
        bad = scalar(con, """WITH coverage AS (
            SELECT season, count(*) n FROM nfl_hc_history GROUP BY season)
            SELECT count(*) FROM range(1989, 1999) y(season)
            LEFT JOIN coverage c USING (season)
            WHERE c.n IS DISTINCT FROM CASE WHEN y.season < 1995
              THEN 28 ELSE 30 END""")
        check("CRITICAL", "historical coach panel covers all 1989-1998 teams",
              bad == 0, f"{bad} seasons with missing or excess teams")
        bad = scalar(con, """SELECT count(*) FROM nfl_hc_history
            WHERE wins IS NULL OR losses IS NULL OR ties IS NULL
              OR games IS NULL OR games != 16
              OR wins + losses + ties != games OR win_pct IS NULL
              OR abs(win_pct - (wins + 0.5 * ties) / games) > 1e-9
              OR opening_coach_name IS NULL OR trim(opening_coach_name) = ''
              OR n_head_coaches IS NULL OR n_head_coaches < 1
              OR source_url IS NULL OR coach_source_url IS NULL""")
        check("CRITICAL", "historical records and opening coaches reconcile",
              bad == 0, f"{bad} invalid or unsourced rows")

    print("Referential integrity")
    pairs = [
        ("nfl_rosters_weekly", "gsis_id", "nfl_players", "gsis_id", 0.98),
        # OverTheCap lists ~1,700 fringe players (never on an nflverse roster)
        ("nfl_contracts", "gsis_id", "nfl_players", "gsis_id", 0.95),
        ("nfl_player_stats_week", "gsis_id", "nfl_players", "gsis_id", 0.99),
        ("staff_team_season", "person_id", "staff_persons", "person_id", 1.0),
        ("staff_person_wiki_signals", "person_id", "staff_persons", "person_id", 1.0),
    ]
    for child, ccol, parent, pcol, floor in pairs:
        share = scalar(con, f"""SELECT avg((p.{pcol} IS NOT NULL)::INT) FROM {child} c
                                LEFT JOIN (SELECT DISTINCT {pcol} FROM {parent}) p
                                  ON c.{ccol} = p.{pcol} WHERE c.{ccol} IS NOT NULL""")
        check("CRITICAL" if floor == 1.0 else "WARN",
              f"{child}.{ccol} found in {parent} (>= {floor:.0%})", share >= floor,
              f"{share:.4f}")
    for entity, table, col in [("nfl_players", "nfl_players", "gsis_id"),
                               ("staff_persons", "staff_persons", "person_id")]:
        missing = scalar(con, f"""SELECT count(*) FROM {table} t WHERE NOT EXISTS (
            SELECT 1 FROM race_bifsg r WHERE r.entity = '{entity}' AND r.entity_id = t.{col})""")
        check("CRITICAL", f"race_bifsg covers every {table} row", missing == 0, f"{missing} missing")
    if exists(con, "player_xwalk"):
        gap = scalar(con, "SELECT count(*) FROM nfl_players ANTI JOIN player_xwalk USING (gsis_id)")
        check("CRITICAL", "player_xwalk has one row per NFL player", gap == 0, f"{gap} missing")
    if exists(con, "player_college_xwalk"):
        shared = scalar(con, """SELECT count(*) FROM (SELECT player_id FROM player_college_xwalk
                                GROUP BY 1 HAVING count(DISTINCT gsis_id) > 1)""")
        check("CRITICAL", "no college player id is linked to two NFL players", shared == 0,
              f"{shared} shared ids")
        late = scalar(con, """SELECT count(*) FROM player_college_xwalk
                              WHERE last_college_season >= entry_year""")
        check("CRITICAL", "linked college careers end before NFL entry", late == 0,
              f"{late} links end at or after entry")

    print("Value ranges")
    bad = scalar(con, """SELECT count(*) FROM race_bifsg WHERE race_bifsg IS NOT NULL AND
        abs(p_white + p_black + p_hispanic + p_api + p_aian + p_multi - 1) > 1e-6""")
    check("CRITICAL", "BIFSG probabilities sum to 1", bad == 0, f"{bad} rows off")
    if exists(con, "race_predicted"):
        for sfx in ["pred", "preddoc"]:
            bad = scalar(con, f"""SELECT count(*) FROM race_predicted WHERE
                p_white_{sfx} IS NULL OR abs(p_white_{sfx} + p_black_{sfx} + p_hispanic_{sfx}
                + p_api_{sfx} + p_aian_{sfx} + p_multi_{sfx} - 1) > 1e-6
                OR p_black_any_{sfx} < p_black_{sfx} - 1e-9 OR p_black_any_{sfx} > 1 + 1e-9""")
            check("CRITICAL", f"race_predicted {sfx} probabilities defined and sum to 1",
                  bad == 0, f"{bad} rows off")
        for entity, table, col in [("player", "nfl_players", "gsis_id"),
                                   ("staff", "staff_persons", "person_id")]:
            missing = scalar(con, f"""SELECT count(*) FROM {table} t WHERE NOT EXISTS (
                SELECT 1 FROM race_predicted r WHERE r.entity = '{entity}'
                AND r.entity_id = t.{col})""")
            check("CRITICAL", f"race_predicted covers every {table} row", missing == 0,
                  f"{missing} missing")
    bad = scalar(con, "SELECT count(*) FROM nfl_team_seasons WHERE win_pct < 0 OR win_pct > 1")
    check("CRITICAL", "team-season win_pct in [0, 1]", bad == 0, f"{bad} rows")
    bad = scalar(con, "SELECT count(*) FROM nfl_team_games WHERE points_for < 0")
    check("CRITICAL", "team-game points non-negative", bad == 0, f"{bad} rows")

    failed = [name for level, name, ok in results if level == "CRITICAL" and not ok]
    warned = [name for level, name, ok in results if level == "WARN" and not ok]
    print(f"\n{len(results)} checks: {len(failed)} critical failures, {len(warned)} warnings")
    con.close()
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

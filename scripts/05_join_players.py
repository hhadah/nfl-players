"""Crosswalk NFL players (gsis_id) to CFBD college players and 247 recruits.

Purpose: give every NFL player a validated link to his college career
(CFBD player_id, stable across transfers from 2020 on) and to his high-school
or JUCO recruiting profile, so that pay regressions can condition on
pre-NFL productivity. ID links come first; names are used only as a
validated, unique-candidate fallback.

NFL entry year = draft year, else nflverse rookie_season. College career
fields use only CFBD seasons before entry.

NFL -> college (player_college_xwalk), in priority order:
  (a) draft_id   cfbd_draft_picks.gsis_id (slot-validated) and
                 college_athlete_id = college_players.player_id; 2009+ drafts
                 only (earlier ids are ESPN draft-profile ids).
  (b) espn_id    college_players.player_id = nfl_players.espn_id (NFL entrants
                 ~2018+). ID links need the first or last name to agree
                 (waived when (a) and (b) agree) and the timing check below
                 with a 10-season career-start window; an espn_id link that
                 no draft id confirms also needs height within 3 in.
  (c) name       same normalized last name and first name (legal, common or
                 football name; A.J. = AJ), a CFBD team that the school
                 crosswalk maps from the nflverse college (or that the CFBD
                 draft pick lists), last pre-NFL college season 1-5 years
                 before entry, compatible position group, height within 3 in
                 and weight within 40 lb when both known, and a unique
                 candidate. Medium-confidence passes: a first-name prefix
                 (Greg/Gregory), and a relaxed tier (undrafted entrants 6-8
                 years after college; OL/DL/TE/LB 40-80 lb heavier than their
                 last college listing). Several CFBD ids per player are kept
                 when they are split identities of one career (CFBD
                 placeholder ids, 2009-2019). A CFBD id never links to two
                 NFL players.
  Timing everywhere: no college stat row in or after the NFL entry year and a
  first college season at most 8 (10 for ID links) years before entry
  (fathers and sons, e.g. Dre Kirkpatrick 2012 vs Dre Kirkpatrick Jr. 2024).
  CFBD player ids already span transfers; college_transfers (2021+, no id)
  only flags portal entrants (in_transfer_portal).

College -> recruit (college_recruit_xwalk):
  recruits.athlete_id and college_players.recruit_ids (direct ids, with a
  class-year check and a first-name-or-home-state identity check); fallback same name + class year 0-1 years before the
  first college season + compatible position and height + committed_to among
  the player's CFBD teams (or, with committed_to missing, the same home
  state), unique on both sides.
NFL -> recruit (player_recruit_xwalk): through the college link, else a
  name + school (committed_to) + class-year unique match.

Sources (all read from the DuckDB, built by 02/03/04):
  nfl_players, nfl_draft_picks, cfbd_draft_picks, college_players,
  college_player_stats, college_teams, college_transfers, recruits;
  data/hand_coded/school_crosswalk_overrides.csv (manual school names).

Tables written (full refresh):
  school_xwalk          every nflverse college string -> CFBD school, method
                        ('unmapped' with a NULL school when nothing matches)
  player_college_xwalk  one row per (gsis_id, college player_id) link
  college_recruit_xwalk one row per (college player_id, recruit_id)
  player_recruit_xwalk  one row per (gsis_id, recruit_id)
  player_xwalk          one row per nfl_players gsis_id (primary college id,
                        all college ids, recruit, flags)
"""
import pandas as pd

from common import connect, write_table
from config import HAND_CODED_DIR

OVERRIDES_CSV = HAND_CODED_DIR / "school_crosswalk_overrides.csv"

# Matching tolerances (see docstring)
MAX_HEIGHT_GAP_IN = 3
MAX_WEIGHT_GAP_LB = 40
MIN_YEARS_BEFORE_ENTRY = 1
MAX_YEARS_BEFORE_ENTRY = 5
MAX_CAREER_START_GAP = 8
ID_MAX_CAREER_START_GAP = 10   # ID links: JUCO/redshirt/CFL paths
MIN_PREFIX_LEN = 3             # Greg/Gregory, Walt/Walter
FIRST_NAME_JW_MIN = 0.85       # Micheal/Michael, Jamre/Jamare (recruit id check)
# Relaxed (medium-confidence) name tier: late NFL entrants (UFL/CFL/practice
# squads) and linemen whose last listed college weight predates NFL bulk
LATE_ENTRY_MAX_YEARS = 8
# First CFBD season minus recruit class year, accepted for direct id links
# (JUCO recruits may have played at a four-year school first)
RECRUIT_GAP_OK = {"HighSchool": (-1, 2), "JUCO": (-5, 1)}
# NFL entry year minus recruit class year, for the NFL -> recruit name path
RECRUIT_TO_NFL_YEARS = {"HighSchool": (3, 6), "JUCO": (1, 3)}
LINEMEN_MAX_WEIGHT_GAP_LB = 80

# College -> NFL position moves that are common enough to allow (unordered);
# a group is always compatible with itself, and a missing group with anything
COMPATIBLE_POSITIONS = [
    ("DL", "LB"), ("LB", "DB"), ("DL", "OL"), ("TE", "OL"), ("TE", "WR"),
    ("TE", "DL"), ("TE", "LB"), ("RB", "WR"), ("RB", "DB"), ("RB", "LB"),
    ("WR", "DB"), ("QB", "WR"), ("QB", "RB"), ("QB", "DB"), ("QB", "TE"),
    ("K", "P"), ("LS", "OL"), ("LS", "TE"), ("LS", "LB"), ("LS", "DL"),
]


# ============================================================================
# Normalization macros (SQL, so every join runs inside DuckDB)
# ============================================================================

def define_macros(con):
    """Name and school keys shared by every matching step."""
    # Last name: ASCII-fold, lowercase, drop generational suffixes, keep a-z
    con.execute(r"""CREATE OR REPLACE TEMP MACRO last_key(s) AS
        regexp_replace(regexp_replace(lower(strip_accents(coalesce(s, ''))),
            '\b(jr|sr|ii|iii|iv|v)\b\.?', '', 'g'), '[^a-z]', '', 'g')""")
    # First name: ASCII-fold, lowercase, keep a-z (A.J. -> aj, De'Von -> devon)
    con.execute(r"""CREATE OR REPLACE TEMP MACRO first_key(s) AS
        regexp_replace(lower(strip_accents(coalesce(s, ''))), '[^a-z]', '', 'g')""")
    # School, strict: HTML-unescape, '&' -> 'and', trailing 'St.' -> 'State'
    con.execute(r"""CREATE OR REPLACE TEMP MACRO school_key(s) AS
        regexp_replace(regexp_replace(regexp_replace(
            lower(strip_accents(replace(coalesce(s, ''), '&amp;', '&'))),
            '&', ' and ', 'g'),
            '([a-z]) st\.?$', '\1 state'), '[^a-z0-9]', '', 'g')""")
    # School, loose (nflverse side only): also drop 'University (of)',
    # 'Univ.', 'College', 'the', 'at'; parentheses are kept on purpose
    # ('Miami (Ohio)', 'Indiana (PA)' must not collapse to a bigger school)
    con.execute(r"""CREATE OR REPLACE TEMP MACRO school_key_loose(s) AS
        school_key(regexp_replace(lower(coalesce(s, '')),
            '\b(the|university|univ|of|college|at)\b\.?', ' ', 'g'))""")


# ============================================================================
# Person-level inputs
# ============================================================================

def build_people(con):
    """Temp tables: cp (one row per CFBD player id), cp_names (name keys),
    nfl (one row per gsis_id with entry year and name keys), nfl_names."""
    # College stat rows per player (stats never carry placeholder ids)
    con.execute("""CREATE OR REPLACE TEMP TABLE cp_stats AS
        SELECT player_id, count(*) AS n_stat_rows,
               min(season) AS stat_first_season, max(season) AS stat_last_season
        FROM college_player_stats GROUP BY player_id""")
    # One row per CFBD player id: career span, teams, last known size (ties
    # within a season, i.e. two teams, break on team name so runs agree)
    con.execute("""CREATE OR REPLACE TEMP TABLE cp AS
        SELECT c.player_id,
               arg_max(c.first_name, row(c.season, c.team)) AS first_name,
               arg_max(c.last_name, row(c.season, c.team)) AS last_name,
               min(c.season) AS first_season, max(c.season) AS last_season,
               list(DISTINCT c.season ORDER BY c.season) AS seasons,
               list(DISTINCT c.team ORDER BY c.team) AS teams,
               coalesce(list(DISTINCT c.position_group) FILTER (
                   WHERE c.position_group IS NOT NULL), []) AS pos_groups,
               arg_max(c.height_clean, row(c.season, c.team)) FILTER (
                   WHERE c.height_clean IS NOT NULL) AS height,
               arg_max(c.weight_clean, row(c.season, c.team)) FILTER (
                   WHERE c.weight_clean IS NOT NULL) AS weight,
               arg_max(c.home_state, row(c.season, c.team)) FILTER (
                   WHERE c.home_state IS NOT NULL) AS home_state,
               bool_or(c.player_id < 0) AS placeholder_id,
               coalesce(any_value(s.n_stat_rows), 0) AS n_stat_rows,
               any_value(s.stat_first_season) AS stat_first_season,
               any_value(s.stat_last_season) AS stat_last_season
        FROM college_players c LEFT JOIN cp_stats s USING (player_id)
        GROUP BY c.player_id""")
    # Every name spelling a CFBD id carried in any roster season
    con.execute("""CREATE OR REPLACE TEMP TABLE cp_names AS
        SELECT DISTINCT player_id, first_key(first_name) AS fk,
               last_key(last_name) AS lk
        FROM college_players WHERE last_key(last_name) <> ''""")
    # NFL players: entry year = draft year, else rookie season
    con.execute("""CREATE OR REPLACE TEMP TABLE nfl AS
        SELECT gsis_id, display_name, first_name, last_name, position_group,
               height, weight, college_name, rookie_season, draft_year,
               draft_round, try_cast(espn_id AS BIGINT) AS espn_id,
               coalesce(draft_year, rookie_season) AS entry_year
        FROM nfl_players""")
    # NFL name keys: legal, common and football first names
    con.execute("""CREATE OR REPLACE TEMP TABLE nfl_names AS
        SELECT DISTINCT gsis_id, first_key(f) AS fk, last_key(last_name) AS lk
        FROM (SELECT gsis_id, last_name,
                     unnest([first_name, common_first_name, football_name]) AS f
              FROM nfl_players)
        WHERE f IS NOT NULL AND first_key(f) <> '' AND last_key(last_name) <> ''""")


# ============================================================================
# (a) + (b): ID links NFL -> college
# ============================================================================

def timing_ok_sql(max_start_gap):
    """SQL predicate on aliases cp / n: the college career starts before NFL
    entry, not implausibly early, and has no stat row in or after entry."""
    return f"""(cp.first_season <= n.entry_year - {MIN_YEARS_BEFORE_ENTRY}
        AND cp.first_season >= n.entry_year - {max_start_gap}
        AND coalesce(cp.stat_last_season, 0) < n.entry_year)"""


def first_compatible_sql(a, b):
    """SQL predicate: first-name keys a and b are the same name (exact, one a
    prefix of the other, or a spelling variant with Jaro-Winkler >= 0.85)."""
    return f"""({a} = {b} OR jaro_winkler_similarity({a}, {b}) >= {FIRST_NAME_JW_MIN}
        OR (least(length({a}), length({b})) >= {MIN_PREFIX_LEN}
            AND (starts_with({a}, {b}) OR starts_with({b}, {a}))))"""


def build_id_links(con):
    """Temp table id_links: validated draft-id and ESPN-id links. The id is
    the evidence; the name check (first OR last name agrees) only guards
    against id collisions, and is waived when both id paths agree. Rejected
    candidates stay in id_candidates for the validation report."""
    # Candidate pairs from both id paths
    con.execute("""CREATE OR REPLACE TEMP TABLE id_pairs AS
        SELECT gsis_id, college_athlete_id AS player_id, 'draft_id' AS method
        FROM cfbd_draft_picks
        WHERE gsis_id IS NOT NULL AND season >= 2009
          AND college_athlete_id IS NOT NULL
        UNION ALL
        SELECT gsis_id, espn_id, 'espn_id' FROM nfl WHERE espn_id IS NOT NULL""")
    # Keep pairs whose college id exists; check name and timing
    con.execute(f"""CREATE OR REPLACE TEMP TABLE id_candidates AS
        SELECT p.gsis_id, p.player_id,
               string_agg(DISTINCT p.method, '+' ORDER BY p.method) AS method,
               bool_or(EXISTS (SELECT 1 FROM nfl_names a JOIN cp_names b
                       ON b.player_id = p.player_id AND a.gsis_id = p.gsis_id
                      AND (a.lk = b.lk OR a.fk = b.fk))) AS name_ok,
               bool_or({timing_ok_sql(ID_MAX_CAREER_START_GAP)}) AS timing_ok,
               any_value(abs(n.height - cp.height)) AS height_gap
        FROM id_pairs p JOIN nfl n USING (gsis_id) JOIN cp USING (player_id)
        GROUP BY p.gsis_id, p.player_id""")
    # An espn_id that no draft id confirms must also fit the player's height
    # (nflverse espn_id can point to a same-name player, e.g. DQ Thomas 2022)
    con.execute(f"""CREATE OR REPLACE TEMP TABLE id_links AS
        SELECT gsis_id, player_id, method AS link_method, 'high' AS confidence
        FROM id_candidates
        WHERE timing_ok AND (name_ok OR method = 'draft_id+espn_id')
          AND (method <> 'espn_id'
               OR coalesce(height_gap <= {MAX_HEIGHT_GAP_IN}, TRUE))""")


# ============================================================================
# School crosswalk: nflverse college string -> CFBD school
# ============================================================================

def build_school_xwalk(con):
    """Temp table school_xw (nflverse_college, cfbd_school, method).

    Priority: manual override > CFBD team name > CFBD alternate name >
    loose key (nflverse string without 'University of', 'College'). A key
    that points to two CFBD schools at the same level is not used. Strings
    with no match (junior colleges, Canadian schools, ...) are kept with
    method 'unmapped' and a NULL cfbd_school. Strings
    that need judgment (Mississippi -> Ole Miss, Miami (Fla.) -> Miami) go
    in data/hand_coded/school_crosswalk_overrides.csv; an override with a
    blank cfbd_school blocks a wrong rule match (cfbd_school NULL)."""
    # Every nflverse college string (players and draft picks; ';' = several)
    con.execute("""CREATE OR REPLACE TEMP TABLE nfl_college_part AS
        SELECT DISTINCT gsis_id, trim(unnest(string_split(college, ';'))) AS nflverse_college
        FROM (SELECT gsis_id, college_name AS college FROM nfl_players
              UNION ALL
              SELECT gsis_id, college FROM nfl_draft_picks WHERE gsis_id IS NOT NULL)
        WHERE college IS NOT NULL""")
    con.execute("DELETE FROM nfl_college_part WHERE nflverse_college = ''")
    # CFBD name keys: team name (level 1) and alternate names (level 2)
    con.execute("""CREATE OR REPLACE TEMP TABLE cfbd_keys AS
        WITH names AS (
            SELECT DISTINCT team, 1 AS level, team AS name FROM college_teams
            UNION ALL
            SELECT DISTINCT team, 2, unnest(alternate_names) FROM college_teams)
        SELECT level, school_key(name) AS k, min(team) AS cfbd_school
        FROM names WHERE school_key(name) <> ''
        GROUP BY level, k HAVING count(DISTINCT team) = 1""")
    # Manual overrides (tracked CSV)
    overrides = pd.read_csv(OVERRIDES_CSV, dtype=str)
    repeated = overrides.loc[overrides.nflverse_college.duplicated(), "nflverse_college"]
    if not repeated.empty:
        raise ValueError(f"{OVERRIDES_CSV.name}: repeated strings {list(repeated)}")
    con.register("_overrides", overrides)
    unknown = con.execute("""SELECT cfbd_school FROM _overrides
        WHERE cfbd_school IS NOT NULL
          AND cfbd_school NOT IN (SELECT team FROM college_teams)""").fetchall()
    if unknown:
        raise ValueError(f"{OVERRIDES_CSV.name}: not CFBD schools: {unknown}")
    # Rule-based mapping: best (lowest) level per string
    con.execute("""CREATE OR REPLACE TEMP TABLE school_xw AS
        WITH s AS (SELECT DISTINCT nflverse_college FROM nfl_college_part),
        cand AS (
            SELECT s.nflverse_college, coalesce(o.cfbd_school, '') AS cfbd_school, 0 AS level
            FROM s JOIN _overrides o USING (nflverse_college)
            UNION ALL
            SELECT s.nflverse_college, k.cfbd_school, k.level
            FROM s JOIN cfbd_keys k ON k.k = school_key(s.nflverse_college)
            UNION ALL
            SELECT s.nflverse_college, k.cfbd_school, 3
            FROM s JOIN cfbd_keys k ON k.k = school_key_loose(s.nflverse_college))
        SELECT s.nflverse_college,
               nullif(arg_min(c.cfbd_school, row(c.level, c.cfbd_school)), '') AS cfbd_school,
               CASE min(c.level) WHEN 0 THEN 'manual_override'
                    WHEN 1 THEN 'team_name' WHEN 2 THEN 'alternate_name'
                    WHEN 3 THEN 'loose_name' ELSE 'unmapped' END AS method
        FROM s LEFT JOIN cand c USING (nflverse_college)
        GROUP BY s.nflverse_college""")
    con.unregister("_overrides")


# ============================================================================
# (c): validated name + school + timing links NFL -> college
# ============================================================================

def build_name_candidates(con):
    """Temp table name_cand: every (gsis_id, player_id) pair with the same
    last name, a matching or prefix first name and a crosswalked school,
    plus the checks used to accept it."""
    # CFBD schools each NFL player may have attended
    con.execute("""CREATE OR REPLACE TEMP TABLE nfl_school AS
        SELECT DISTINCT p.gsis_id, x.cfbd_school
        FROM nfl_college_part p JOIN school_xw x USING (nflverse_college)
        WHERE x.cfbd_school IS NOT NULL
        UNION
        SELECT d.gsis_id, d.college_team FROM cfbd_draft_picks d
        WHERE d.gsis_id IS NOT NULL
          AND d.college_team IN (SELECT team FROM college_teams)""")
    pairs = ", ".join(f"('{a}', '{b}'), ('{b}', '{a}')"
                      for a, b in COMPATIBLE_POSITIONS)
    con.execute(f"""CREATE OR REPLACE TEMP TABLE pos_ok AS
        SELECT * FROM (VALUES {pairs}) t(nfl_pg, college_pg)""")
    # Same last name; exact first name, or one first name a prefix of the other
    con.execute(f"""CREATE OR REPLACE TEMP TABLE name_pairs AS
        SELECT a.gsis_id, b.player_id,
               bool_or(a.fk = b.fk) AS first_exact
        FROM nfl_names a JOIN cp_names b ON a.lk = b.lk
        WHERE a.fk = b.fk
           OR (least(length(a.fk), length(b.fk)) >= {MIN_PREFIX_LEN}
               AND (starts_with(a.fk, b.fk) OR starts_with(b.fk, a.fk)))
        GROUP BY a.gsis_id, b.player_id""")
    # Attach school, timing, position and size checks
    con.execute(f"""CREATE OR REPLACE TEMP TABLE name_cand AS
        SELECT np.gsis_id, np.player_id, np.first_exact,
               list_max(list_filter(cp.seasons, x -> x < n.entry_year)) AS last_pre_season,
               list_filter(cp.seasons, x -> x < n.entry_year) AS pre_seasons,
               EXISTS (SELECT 1 FROM nfl_school s WHERE s.gsis_id = np.gsis_id
                       AND list_contains(cp.teams, s.cfbd_school)) AS school_ok,
               n.entry_year - last_pre_season AS years_before,
               coalesce(cp.stat_last_season, 0) < n.entry_year AS no_stats_after,
               {timing_ok_sql(MAX_CAREER_START_GAP)}
                 AND years_before BETWEEN {MIN_YEARS_BEFORE_ENTRY}
                                      AND {MAX_YEARS_BEFORE_ENTRY} AS timing_ok,
               n.draft_year IS NULL AS undrafted, n.position_group,
               n.position_group IS NULL OR len(cp.pos_groups) = 0
                 OR list_contains(cp.pos_groups, n.position_group)
                 OR EXISTS (SELECT 1 FROM pos_ok q WHERE q.nfl_pg = n.position_group
                            AND list_contains(cp.pos_groups, q.college_pg)) AS pos_ok,
               coalesce(list_contains(cp.pos_groups, n.position_group), FALSE) AS pos_exact,
               abs(n.height - cp.height) AS height_gap,
               abs(n.weight - cp.weight) AS weight_gap,
               coalesce(height_gap <= {MAX_HEIGHT_GAP_IN}, TRUE)
                 AND coalesce(weight_gap <= {MAX_WEIGHT_GAP_LB}, TRUE) AS size_ok
        FROM name_pairs np JOIN nfl n USING (gsis_id) JOIN cp USING (player_id)""")


def build_name_links(con):
    """Temp table name_links: accepted name candidates.

    Tier 1 exact first name and every check; tier 2 prefix first name and
    every check; tier 3 (relaxed) exact first name, school, position and
    height, with either an undrafted late entrant (6-8 years) or a lineman
    whose college weight is 40-80 lb below his NFL weight. Only a player's
    best tier counts. Several CFBD ids are kept as split identities of one
    career when they fall within a 6-season span and the real (positive)
    ids among them have disjoint seasons; placeholder (negative) ids, which
    CFBD created alongside real ids in 2009-2019, may overlap. Otherwise
    an exact position-group match must make the choice unique. A CFBD id
    claimed by two NFL players, or ID-linked to another one, is dropped."""
    con.execute(f"""CREATE OR REPLACE TEMP TABLE name_tiered AS
        SELECT * FROM (
            SELECT *, CASE
                WHEN school_ok AND timing_ok AND pos_ok AND size_ok AND first_exact THEN 1
                WHEN school_ok AND timing_ok AND pos_ok AND size_ok THEN 2
                WHEN school_ok AND first_exact AND pos_ok AND no_stats_after
                     AND coalesce(height_gap <= {MAX_HEIGHT_GAP_IN}, TRUE)
                     AND ((undrafted AND size_ok
                           AND years_before > {MAX_YEARS_BEFORE_ENTRY}
                           AND years_before <= {LATE_ENTRY_MAX_YEARS})
                       OR (timing_ok AND position_group IN ('OL', 'DL', 'TE', 'LB')
                           AND weight_gap <= {LINEMEN_MAX_WEIGHT_GAP_LB})) THEN 3
                END AS tier
            FROM name_cand)
        WHERE tier IS NOT NULL
        QUALIFY tier = min(tier) OVER (PARTITION BY gsis_id)""")
    # Uniqueness: one candidate, disjoint fragments, or unique by position
    con.execute("""CREATE OR REPLACE TEMP TABLE name_unique AS
        WITH w AS (
            SELECT *,
                count(*) OVER g AS n_cand,
                flatten(list(pre_seasons) OVER g) AS all_s,
                coalesce(flatten(list(pre_seasons) FILTER (WHERE player_id > 0)
                                 OVER g), []) AS real_s,
                count(*) FILTER (WHERE pos_exact) OVER g AS n_pos,
                flatten(list(pre_seasons) FILTER (WHERE pos_exact) OVER g) AS pos_all_s,
                coalesce(flatten(list(pre_seasons) FILTER (
                    WHERE pos_exact AND player_id > 0) OVER g), []) AS pos_real_s
            FROM name_tiered WINDOW g AS (PARTITION BY gsis_id)),
        f AS (
            SELECT *,
                len(real_s) = len(list_distinct(real_s))
                    AND list_max(all_s) - list_min(all_s) < 6 AS frag_all,
                len(pos_real_s) = len(list_distinct(pos_real_s))
                    AND list_max(pos_all_s) - list_min(pos_all_s) < 6 AS frag_pos
            FROM w)
        SELECT gsis_id, player_id, tier, pos_exact, height_gap,
               NOT (n_cand = 1 OR frag_all) AS resolved_by_position
        FROM f
        WHERE n_cand = 1 OR frag_all
           OR (pos_exact AND (n_pos = 1 OR frag_pos))""")
    # Reverse uniqueness and consistency with the ID links
    con.execute("""CREATE OR REPLACE TEMP TABLE name_links AS
        SELECT gsis_id, player_id,
               CASE tier WHEN 1 THEN 'name' WHEN 2 THEN 'name_prefix'
                         ELSE 'name_relaxed' END AS link_method,
               CASE WHEN tier = 1 AND pos_exact AND height_gap IS NOT NULL
                         AND NOT resolved_by_position THEN 'high'
                    ELSE 'medium' END AS confidence
        FROM name_unique u
        WHERE NOT EXISTS (SELECT 1 FROM name_unique v
                          WHERE v.player_id = u.player_id AND v.gsis_id <> u.gsis_id)
          AND NOT EXISTS (SELECT 1 FROM id_links l
                          WHERE l.player_id = u.player_id AND l.gsis_id <> u.gsis_id)
          AND NOT EXISTS (SELECT 1 FROM id_links l
                          WHERE l.player_id = u.player_id AND l.gsis_id = u.gsis_id)""")


# ============================================================================
# Output: player_college_xwalk
# ============================================================================

def player_college_xwalk(con):
    """One row per (gsis_id, college player_id); career fields use only
    college seasons before the NFL entry year (stale later roster rows and
    re-used ids are ignored). in_transfer_portal: a college_transfers entry
    (2021+) with the same name leaves one of the player's CFBD teams."""
    return con.execute("""
        WITH links AS (
            SELECT gsis_id, player_id, link_method, confidence FROM id_links
            UNION ALL
            SELECT gsis_id, player_id, link_method, confidence FROM name_links),
        team_stats AS (
            SELECT DISTINCT player_id, season, team FROM college_player_stats),
        pre AS (
            SELECT l.gsis_id, l.player_id, c.season, c.team,
                   ts.player_id IS NOT NULL AS has_stats
            FROM links l JOIN nfl n USING (gsis_id)
            JOIN college_players c ON c.player_id = l.player_id
                                  AND c.season < n.entry_year
            LEFT JOIN team_stats ts ON ts.player_id = c.player_id
                                   AND ts.season = c.season AND ts.team = c.team),
        -- Last team: latest season; in a season listed at two schools, the
        -- one with stat rows, then alphabetical (deterministic)
        span AS (
            SELECT gsis_id, player_id,
                   min(season) AS first_college_season,
                   max(season) AS last_college_season,
                   arg_max(team, row(season, has_stats, team)) AS last_college_team,
                   list(DISTINCT team ORDER BY team) AS college_teams,
                   count(DISTINCT team) AS n_college_teams
            FROM pre GROUP BY gsis_id, player_id),
        stats AS (
            SELECT l.gsis_id, l.player_id, count(s.player_id) AS n_stat_rows
            FROM links l JOIN nfl n USING (gsis_id)
            LEFT JOIN college_player_stats s
              ON s.player_id = l.player_id AND s.season < n.entry_year
            GROUP BY l.gsis_id, l.player_id),
        portal AS (
            SELECT DISTINCT l.player_id
            FROM links l JOIN cp USING (player_id)
            JOIN college_transfers t
              ON last_key(t.last_name) = last_key(cp.last_name)
             AND first_key(t.first_name) = first_key(cp.first_name)
             AND list_contains(cp.teams, t.origin))
        SELECT l.gsis_id, l.player_id, l.link_method, l.confidence,
               cp.first_name AS college_first_name, cp.last_name AS college_last_name,
               s.first_college_season, s.last_college_season, s.last_college_team,
               s.college_teams, s.n_college_teams, st.n_stat_rows,
               l.player_id IN (SELECT player_id FROM portal) AS in_transfer_portal,
               cp.placeholder_id, n.entry_year,
               n.entry_year - s.last_college_season AS years_college_to_nfl
        FROM links l JOIN nfl n USING (gsis_id) JOIN cp USING (player_id)
        JOIN span s USING (gsis_id, player_id)
        JOIN stats st USING (gsis_id, player_id)
        ORDER BY l.gsis_id, l.player_id""").to_arrow_table()


# ============================================================================
# College -> recruit
# ============================================================================

def build_college_recruit(con):
    """Temp table cr_links (player_id, recruit_id, link_method, confidence).

    Direct ids first (recruits.athlete_id, college_players.recruit_ids),
    dropping pairs whose first CFBD season is implausible for the recruit
    class (RECRUIT_GAP_OK) and pairs with an incompatible first name and a
    different home state (a namesake). Fallback for ids and recruits with no
    accepted direct
    link: same name, class year 0-1 years before the first CFBD season,
    compatible position, height within 3 in, and either committed_to among
    the player's CFBD teams (name_school) or, with committed_to missing, the
    same home state (name_state, medium). Unique on both sides."""
    (hs_lo, hs_hi), (ju_lo, ju_hi) = RECRUIT_GAP_OK["HighSchool"], RECRUIT_GAP_OK["JUCO"]
    # Direct id pairs from both directions
    con.execute(f"""CREATE OR REPLACE TEMP TABLE cr_direct_all AS
        WITH d AS (
            SELECT recruit_id, athlete_id AS player_id, 'athlete_id' AS m
            FROM recruits WHERE athlete_id IS NOT NULL
            UNION ALL
            SELECT DISTINCT unnest(recruit_ids), player_id, 'recruit_ids'
            FROM college_players WHERE recruit_ids IS NOT NULL)
        SELECT d.player_id, d.recruit_id,
               string_agg(DISTINCT d.m, '+' ORDER BY d.m) AS link_method,
               any_value(cp.first_season - r.recruit_class) AS class_gap,
               any_value(CASE r.recruit_type
                   WHEN 'JUCO' THEN cp.first_season - r.recruit_class BETWEEN {ju_lo} AND {ju_hi}
                   ELSE cp.first_season - r.recruit_class BETWEEN {hs_lo} AND {hs_hi}
                   END) AS timing_ok
        FROM d JOIN recruits r USING (recruit_id) JOIN cp USING (player_id)
        GROUP BY d.player_id, d.recruit_id""")
    # Identity check on direct ids: CFBD athlete_id sometimes points to a
    # namesake or a brother at the same school (recruit Cameron Scarlett ->
    # Brennan Scarlett's Stanford id; Raheim Sanders -> Drew Sanders), so a
    # pair needs a compatible first name (exact, prefix, or Jaro-Winkler
    # >= 0.85 for spelling variants) or the same home state
    con.execute(f"""CREATE OR REPLACE TEMP TABLE cr_direct_chk AS
        SELECT d.*,
               EXISTS (SELECT 1 FROM cp_names b WHERE b.player_id = d.player_id
                       AND {first_compatible_sql('b.fk', 'first_key(r.first_name)')}) AS first_ok,
               EXISTS (SELECT 1 FROM college_players c WHERE c.player_id = d.player_id
                       AND c.home_state = r.state_province) AS state_ok
        FROM cr_direct_all d JOIN recruits r USING (recruit_id)""")
    con.execute("""CREATE OR REPLACE TEMP TABLE cr_direct AS
        SELECT player_id, recruit_id, link_method FROM cr_direct_chk
        WHERE timing_ok AND (first_ok OR state_ok)""")
    # Name fallback candidates among unlinked recruits and college ids
    con.execute(f"""CREATE OR REPLACE TEMP TABLE cr_name_cand AS
        SELECT cp.player_id, r.recruit_id, r.recruit_type,
               coalesce(list_contains(cp.teams, r.committed_to), FALSE) AS school_ok
        FROM recruits r
        JOIN cp_names b ON b.lk = last_key(r.last_name) AND b.fk = first_key(r.first_name)
        JOIN cp ON cp.player_id = b.player_id
        WHERE r.recruit_id NOT IN (SELECT recruit_id FROM cr_direct)
          AND cp.player_id NOT IN (SELECT player_id FROM cr_direct)
          AND cp.first_season - r.recruit_class BETWEEN 0 AND 1
          AND coalesce(abs(r.height_clean - cp.height) <= {MAX_HEIGHT_GAP_IN}, TRUE)
          AND (r.position_group IS NULL OR len(cp.pos_groups) = 0
               OR list_contains(cp.pos_groups, r.position_group)
               OR EXISTS (SELECT 1 FROM pos_ok q WHERE q.nfl_pg = r.position_group
                          AND list_contains(cp.pos_groups, q.college_pg)))
          AND (list_contains(cp.teams, r.committed_to)
               OR (r.committed_to IS NULL AND r.state_province = cp.home_state))""")
    con.execute("""CREATE OR REPLACE TEMP TABLE cr_links AS
        SELECT player_id, recruit_id, link_method, 'high' AS confidence
        FROM cr_direct
        UNION ALL
        SELECT player_id, recruit_id,
               CASE WHEN school_ok THEN 'name_school' ELSE 'name_state' END,
               CASE WHEN school_ok THEN 'high' ELSE 'medium' END
        FROM cr_name_cand c
        WHERE NOT EXISTS (SELECT 1 FROM cr_name_cand x WHERE x.recruit_id = c.recruit_id
                          AND x.player_id <> c.player_id)
          AND NOT EXISTS (SELECT 1 FROM cr_name_cand x WHERE x.player_id = c.player_id
                          AND x.recruit_type = c.recruit_type
                          AND x.recruit_id <> c.recruit_id)""")


# ============================================================================
# NFL -> recruit
# ============================================================================

def build_player_recruit(con):
    """Temp table pr_links (gsis_id, recruit_id, link_method, confidence).

    Through the college link ('via_college:<college-recruit method>'); for
    NFL players with no recruit that way, a direct name match: same name,
    committed_to = a crosswalked school or a team of a linked CFBD id, class year 3-6 (HS) or 1-3 (JUCO)
    years before NFL entry, compatible position, height within 3 in, unique
    on both sides, and not a recruit tied to another NFL player's college id."""
    (hs_lo, hs_hi), (ju_lo, ju_hi) = (RECRUIT_TO_NFL_YEARS["HighSchool"],
                                      RECRUIT_TO_NFL_YEARS["JUCO"])
    # Through the college link
    con.execute("""CREATE OR REPLACE TEMP TABLE pr_via AS
        SELECT DISTINCT l.gsis_id, c.recruit_id,
               'via_college:' || c.link_method AS link_method,
               CASE WHEN l.confidence = 'high' AND c.confidence = 'high'
                    THEN 'high' ELSE 'medium' END AS confidence
        FROM (SELECT gsis_id, player_id, confidence FROM id_links
              UNION ALL
              SELECT gsis_id, player_id, confidence FROM name_links) l
        JOIN cr_links c USING (player_id)""")
    # Schools: crosswalked nflverse colleges plus teams of linked CFBD ids
    con.execute("""CREATE OR REPLACE TEMP TABLE pr_school AS
        SELECT gsis_id, cfbd_school FROM nfl_school
        UNION
        SELECT l.gsis_id, unnest(cp.teams)
        FROM (SELECT gsis_id, player_id FROM id_links
              UNION ALL
              SELECT gsis_id, player_id FROM name_links) l JOIN cp USING (player_id)""")
    # Direct name candidates for players without a recruit so far
    con.execute(f"""CREATE OR REPLACE TEMP TABLE pr_name_cand AS
        SELECT DISTINCT a.gsis_id, r.recruit_id, r.recruit_type
        FROM recruits r
        JOIN nfl_names a ON a.lk = last_key(r.last_name) AND a.fk = first_key(r.first_name)
        JOIN nfl n ON n.gsis_id = a.gsis_id
        JOIN pr_school s ON s.gsis_id = a.gsis_id AND s.cfbd_school = r.committed_to
        WHERE a.gsis_id NOT IN (SELECT gsis_id FROM pr_via)
          AND r.recruit_id NOT IN (SELECT recruit_id FROM pr_via)
          AND CASE r.recruit_type
                WHEN 'JUCO' THEN n.entry_year - r.recruit_class BETWEEN {ju_lo} AND {ju_hi}
                ELSE n.entry_year - r.recruit_class BETWEEN {hs_lo} AND {hs_hi} END
          AND coalesce(abs(r.height_clean - n.height) <= {MAX_HEIGHT_GAP_IN}, TRUE)
          AND (r.position_group IS NULL OR n.position_group IS NULL
               OR r.position_group = n.position_group
               OR EXISTS (SELECT 1 FROM pos_ok q WHERE q.nfl_pg = n.position_group
                          AND q.college_pg = r.position_group))""")
    con.execute("""CREATE OR REPLACE TEMP TABLE pr_links AS
        SELECT gsis_id, recruit_id, link_method, confidence FROM pr_via
        UNION ALL
        SELECT gsis_id, recruit_id, 'name_school', 'medium'
        FROM pr_name_cand c
        WHERE NOT EXISTS (SELECT 1 FROM pr_name_cand x WHERE x.recruit_id = c.recruit_id
                          AND x.gsis_id <> c.gsis_id)
          AND NOT EXISTS (SELECT 1 FROM pr_name_cand x WHERE x.gsis_id = c.gsis_id
                          AND x.recruit_type = c.recruit_type
                          AND x.recruit_id <> c.recruit_id)""")


# ============================================================================
# Output: player_xwalk (one row per NFL player)
# ============================================================================

def player_xwalk(con):
    """One row per nfl_players gsis_id. Primary college id: most pre-NFL stat
    rows, then a real (positive) id, then the latest season, then the
    smallest id. Primary recruit: high-school profile before JUCO, then a
    first name compatible with the NFL player's (a brother's profile can ride
    on a shared CFBD id, e.g. Brennan/Cameron Scarlett), then a
    high-confidence link, then the smallest recruit_id."""
    return con.execute(f"""
        WITH pc AS (SELECT * FROM player_college_xwalk),
        prim AS (
            SELECT gsis_id,
                   arg_min(player_id, [-n_stat_rows, placeholder_id::INT,
                                       -last_college_season, player_id]) AS primary_player_id
            FROM pc GROUP BY gsis_id),
        coll AS (
            SELECT gsis_id,
                   list(player_id ORDER BY player_id) AS college_player_ids,
                   count(*) AS n_college_ids,
                   list(DISTINCT link_method ORDER BY link_method) AS college_link_methods,
                   min(first_college_season) AS first_college_season,
                   max(last_college_season) AS last_college_season,
                   arg_max(last_college_team, row(last_college_season, n_stat_rows,
                                               last_college_team)) AS last_college_team,
                   list_sort(list_distinct(flatten(list(college_teams)))) AS college_teams,
                   sum(n_stat_rows) AS n_college_stat_rows
            FROM pc GROUP BY gsis_id),
        rec AS (
            SELECT p.gsis_id, p.recruit_id, p.link_method, p.confidence,
                   r.recruit_type, r.recruit_class,
                   EXISTS (SELECT 1 FROM nfl_names a WHERE a.gsis_id = p.gsis_id
                           AND {first_compatible_sql('a.fk', 'first_key(r.first_name)')}) AS first_ok
            FROM pr_links p JOIN recruits r USING (recruit_id)),
        rec_g AS (
            SELECT gsis_id,
                   arg_min(recruit_id, [(recruit_type <> 'HighSchool')::INT,
                                        (NOT first_ok)::INT,
                                        (confidence <> 'high')::INT, recruit_id]) AS recruit_id,
                   list(recruit_id ORDER BY recruit_id) AS recruit_ids
            FROM rec GROUP BY gsis_id),
        mapped AS (
            SELECT DISTINCT p.gsis_id
            FROM nfl_college_part p JOIN school_xw x USING (nflverse_college)
            WHERE x.cfbd_school IS NOT NULL)
        SELECT n.gsis_id, n.display_name, n.position_group, n.entry_year,
               n.rookie_season, n.draft_year, n.draft_round, n.college_name,
               n.gsis_id IN (SELECT gsis_id FROM mapped) AS college_name_mapped,
               pr.primary_player_id AS college_player_id,
               pl.link_method AS college_link_method,
               pl.confidence AS college_link_confidence,
               c.college_player_ids, c.n_college_ids, c.college_link_methods,
               c.first_college_season, c.last_college_season,
               c.last_college_team, c.college_teams,
               coalesce(c.n_college_stat_rows, 0) AS n_college_stat_rows,
               g.recruit_id, rc.recruit_type, rc.recruit_class,
               rc.link_method AS recruit_link_method,
               rc.confidence AS recruit_link_confidence, g.recruit_ids,
               c.gsis_id IS NOT NULL AS has_college_link,
               coalesce(c.n_college_stat_rows, 0) > 0 AS has_college_stats,
               g.gsis_id IS NOT NULL AS has_recruit
        FROM nfl n
        LEFT JOIN prim pr USING (gsis_id)
        LEFT JOIN pc pl ON pl.gsis_id = n.gsis_id AND pl.player_id = pr.primary_player_id
        LEFT JOIN coll c ON c.gsis_id = n.gsis_id
        LEFT JOIN rec_g g ON g.gsis_id = n.gsis_id
        LEFT JOIN rec rc ON rc.gsis_id = n.gsis_id AND rc.recruit_id = g.recruit_id
        ORDER BY n.gsis_id""").to_arrow_table()


def check_keys(con):
    """Fail loudly if a written table breaks its documented key, or if a
    CFBD id / recruit is claimed by two NFL players."""
    keys = {"school_xwalk": "nflverse_college",
            "player_college_xwalk": "player_id",
            "college_recruit_xwalk": "recruit_id",
            "player_recruit_xwalk": "recruit_id",
            "player_xwalk": "gsis_id"}
    for table, key in keys.items():
        n, n_key = con.execute(f"""SELECT count(*), count(DISTINCT {key})
            FROM {table}""").fetchone()
        if n != n_key:
            raise ValueError(f"{table}: {n - n_key} repeated {key} values")
    n_nfl, n_x = con.execute("""SELECT (SELECT count(*) FROM nfl_players),
        (SELECT count(*) FROM player_xwalk)""").fetchone()
    if n_nfl != n_x:
        raise ValueError(f"player_xwalk has {n_x} rows for {n_nfl} NFL players")


def main():
    con = connect()
    define_macros(con)
    print("Building person tables and ID links...")
    build_people(con)
    build_id_links(con)
    build_school_xwalk(con)
    print("Name matching NFL -> college...")
    build_name_candidates(con)
    build_name_links(con)
    print("Recruit links...")
    build_college_recruit(con)
    build_player_recruit(con)

    # Write the crosswalk tables
    write_table(con, "school_xwalk", con.execute("""
        SELECT x.nflverse_college, x.cfbd_school, x.method,
               count(DISTINCT p.gsis_id) AS n_nfl_players
        FROM school_xw x JOIN nfl_college_part p USING (nflverse_college)
        GROUP BY ALL ORDER BY nflverse_college""").to_arrow_table(),
        source="nfl_players, nfl_draft_picks, college_teams, overrides CSV",
        note="nflverse college string -> CFBD school")
    write_table(con, "player_college_xwalk", player_college_xwalk(con),
                source="cfbd_draft_picks, nfl_players, college_players",
                note="one row per (gsis_id, CFBD player_id)")
    write_table(con, "college_recruit_xwalk", con.execute(
        "SELECT * FROM cr_links ORDER BY player_id, recruit_id").to_arrow_table(),
        source="recruits, college_players", note="one row per (player_id, recruit_id)")
    write_table(con, "player_recruit_xwalk", con.execute(
        "SELECT * FROM pr_links ORDER BY gsis_id, recruit_id").to_arrow_table(),
        source="player_college_xwalk, college_recruit_xwalk, recruits",
        note="one row per (gsis_id, recruit_id)")
    write_table(con, "player_xwalk", player_xwalk(con),
                source="all crosswalks", note="one row per nfl_players gsis_id")
    check_keys(con)
    con.close()


if __name__ == "__main__":
    main()

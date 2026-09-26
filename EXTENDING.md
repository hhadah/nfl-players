# Extending the data

What the 2026-09 rebuild does not cover, and how to add it. For each item,
add a loader in `scripts/` that caches raw responses under `data/raw/` and
writes tables with `common.write_table`. Then register the loader in
`scripts/99_run_all.py` and add checks to `scripts/06_validate_db.py`.

## Gaps that matter for the research designs

### Race (all designs)
Hand-coding is set up but not done (`data/hand_coded/race_coding/`). Code
tier 1 (176 head coaches) and tier 2 (coordinators, GMs, owners) first. The
team-level designs need tiers 1-4. The pay designs need tiers 5-6, about
8,200 players.

### Top football decision-maker
In 59 of 608 template-era team-seasons nobody holds a GM title (e.g. NE under
Belichick, CIN under Mike Brown). A rule based on titles picks the wrong
person too often, so this needs a hand-coded file (franchise, season,
person_id, source), for example `data/hand_coded/top_football_exec.csv`.

### Pro Bowl / All-Pro by season
nflverse draft picks carry career counts only. Season-level honors are on
Pro-Football-Reference award pages. PFR blocks direct requests, so use the
Wayback Machine at 3 seconds or more per request.

### PFF grades
PFF grades are the standard OL/DL/coverage quality measure. They require a
license and CSV export; join them on `pff_id`, which is in `nfl_players` and
`nfl_ids`.

### Contract signing dates, agents, cap space
OverTheCap gives no signing date. Spotrac and OverTheCap pages list dates,
agents and team cap space; check each site's terms before scraping.

### High-school game statistics
No source used here has them; the 247 composite summarizes the high-school
signal the market sees. MaxPreps is the only broad source; it has no API and
is protected by Cloudflare. Any sample from it would be partial and
non-random.

### Pre-2009 college box scores
CFBD has almost no player statistics before 2009. Sports-Reference college
pages (`cfbref_id` in `nfl_ids`) cover earlier seasons.

### College assistant coaches
CFBD lists FBS head coaches only. Coordinators and position coaches would come
from school media guides or Wikipedia season articles.

### Participation data
`nflreadpy.load_participation` gives the 22 players on the field for each play
(2016+). Use it for play-level OL and coverage exposure. The raw
play-by-play is already cached in `data/raw/nflverse/pbp_<season>.parquet`.

## Coverage gotchas

- **Team codes:** always join on `franchise_id`. The raw `team` columns mix
  GSIS (ARZ, BLT, CLV, HST, SL), historical (OAK, SD, STL) and current codes.
- **Rosters:** `nfl_rosters_weekly` keeps every status. Use `is_key_primary`
  for one row per player-team-week, and status `ACT`/`INA` for the game-day
  roster. Practice-squad (`DEV`) rows appear only from 2017.
- **College stats:** never sum rate or maximum stat types (PCT, YPA, AVG,
  LONG) across seasons; recompute rates from their components. Use
  `college_team_stats` completeness flags to set FCS (before 2022) and FBS
  (before 2009) production to missing.
- **Crosswalk confidence:** exclude `confidence = 'medium'` links in
  robustness checks. `first_college_season` can be early for players whose
  CFBD roster rows were backfilled.
- **Staff snapshots:** a person listed only in the late snapshot joined
  mid-season or was listed late by editors. Use the game-day snapshot
  assignment in `team_game` for timing-sensitive designs.
- **Contracts:** `guaranteed = 0` can mean unknown. Flag or exclude those rows
  for guarantee outcomes.

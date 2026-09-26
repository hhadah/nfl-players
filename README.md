# NFL coaching-staff diversity and player pay

Data infrastructure for two questions:

1. Does racial diversity of NFL coaching staffs and front offices improve team
   performance?
2. Is there racial discrimination in NFL player pay and contracts, and can it
   be distinguished from statistical discrimination by conditioning on
   high-school, college and NFL productivity?

The repository builds one DuckDB database from public sources (Python,
`scripts/`) and then the analytical samples and data exhibits from it (R,
`programs/`). Race is measured primarily by human hand-coding (protocol in
[notes/race-coding-protocol.md](notes/race-coding-protocol.md)); name-based
inference is kept only as a secondary measure.

## Layout

```
scripts/            Python: data acquisition, linkage, race inference, validation
programs/           R: analytical samples, summary tables, race-coding agreement
data/raw/           API caches (nflverse parquet, CFBD JSON, Wikipedia JSON, Census) — gitignored
data/datasets/      nfl_research.duckdb and analysis/ samples — gitignored, regenerated
data/hand_coded/    human inputs, tracked: race-coding sheets, school-name and
                    head-coach-date corrections
data/archive/       backup of the 2026-05-04 build — gitignored
output/             tables (.tex) and figures
notes/              race-coding protocol
```

## Setup

```bash
python3 -m venv .venv && .venv/bin/pip install -r requirements.txt
cp .env.example .env    # add a free CFBD key: https://collegefootballdata.com/key
```

R packages are loaded with `pacman::p_load()` in `programs/95-make-all.R`.

## Build

```bash
.venv/bin/python scripts/99_run_all.py
```

```bash
Rscript programs/95-make-all.R
```

The first Python run downloads everything (about 1 GB, mostly play-by-play)
and spends about 300 of the free CFBD tier's 1,000 monthly calls. Every
response is cached under `data/raw/`, so later rebuilds are offline and take
about 2 minutes. `--refresh` forces re-downloads; `--only <step>` runs one
step. `06_validate_db.py` runs last and fails the build on critical problems
(key uniqueness, franchise-season completeness, referential integrity, value
ranges).

| Step | Source | Main tables |
|---|---|---|
| `00_fetch_reference.py` | Census 2010 surnames; Tzioumis (2018) first names; Census county race shares | reference files |
| `01_build_db.py` | — | `franchise_seasons` (team key crosswalk) |
| `02_load_nfl.py` | nflverse (nflreadpy) | `nfl_players`, `nfl_rosters_weekly`/`_season`, `nfl_draft_picks` (1980-2026), `nfl_combine`, `nfl_contracts`, `nfl_contract_history`, `nfl_contract_years`, `nfl_injuries`, `nfl_depth_charts`, `nfl_trades`, `nfl_ids` |
| `02c_load_nfl_stats.py` | nflverse | `nfl_schedules`, `nfl_team_games`, `nfl_team_seasons`, `nfl_player_stats_week`/`_season`, `nfl_snap_counts`, `nfl_pfr_advstats_season`, `nfl_nextgen_stats`, `nfl_team_week_head_coach` |
| `02b_load_nfl_staff.py` | Wikipedia staff templates and season articles | `staff_team_season`, `staff_persons`, `staff_entries`, `staff_snapshots`, `staff_person_wiki_signals`, `staff_hc_reconciliation` |
| `03_load_college.py` | CollegeFootballData | `college_players`, `college_player_stats`, `college_teams`, `college_team_stats`/`_records`/`_ratings`, `college_games`, `college_coaches`, `college_player_ppa`/`_usage`, `college_transfers` |
| `04_load_recruiting.py` | CollegeFootballData (247 composite) | `recruits`, `cfbd_draft_picks` |
| `04c_infer_race.py` | reference files | `race_bifsg`, `race_bifsg_geo` |
| `05_join_players.py` | — | `player_xwalk`, `player_college_xwalk`, `player_recruit_xwalk`, `college_recruit_xwalk`, `school_xwalk` |
| `09_race_coding_sheets.py` | Wikidata/Wikipedia | coding sheets in `data/hand_coded/race_coding/`, `player_wiki_signals` |
| `06_validate_db.py` | — | cross-table checks |

Every team column carries `franchise_id` (current nflverse code; relocated
franchises keep one id, e.g. OAK/LV = `LV`), so tables join on
`(franchise_id, season)`. Tables are rebuilt in full on every run and keep
every source column.

## Coverage

| Data | Years | Notes |
|---|---|---|
| Team games, outcomes, EPA (play-by-play), point spreads | 1999-2025 | 14,552 team-games; market expected wins from moneylines |
| Coaching staff and front office | 2007-2025 full; 1999-2006 partial | Every franchise-season has a staff; 3,596 persons (HC, coordinators, position coaches, assistants, S&C, owners, presidents, GMs, personnel and scouting). Three snapshots per season (Sep 10, Nov 1, late season) capture in-season changes. |
| Rosters | 2002-2025 weekly | All statuses kept; `position_group` harmonizes the 2016 code change |
| Player box scores | 1999-2025 | Offense, defense and kicking |
| Snap counts | 2013-2025 | The nflverse 2012 file is empty |
| PFR advanced defense | 2018-2025 | Pressures, missed tackles, coverage |
| Contracts (OverTheCap) | signed 1983-2026, dense from 2011 | 52,944 contracts with type (drafted, UDFA, UFA, extension, tag, ...) and year-by-year cap hits and cash |
| Draft, combine | 1980-2026, 2000-2026 | Combine linked for 82% of invitees (95% of drafted OL) |
| College rosters and stats | 2004-2025 | Box-score stats usable for FBS from 2009 and FCS from 2022 |
| Recruits (high school) | classes 2000-2025 | 247 composite: stars, rating, rank, school, hometown. No high-school game statistics exist in any source used here. |
| NFL-college-recruit links | — | Rookies 2014-2024: 91% linked to college, 77% with college stats, 77% with a recruit profile |

## Race and ethnicity

- **Primary:** human hand-coding by two independent coders, with
  adjudication, following [notes/race-coding-protocol.md](notes/race-coding-protocol.md).
  `scripts/09_race_coding_sheets.py` writes tiered sheets (tier 1 = head
  coaches, then coordinators/GMs/owners, other coaches, front office, veteran
  contract signers, rookie signers, other players). It never overwrites
  entered codes. `programs/08-race-coding-agreement.R` reports Cohen's kappa
  and the adjudication list.
- **Screening aid:** Wikipedia category flags (`staff_person_wiki_signals`,
  `player_wiki_signals`). They are positive-only; a missing category is not
  evidence of race.
- **Secondary:** name-based BIFSG (`race_bifsg`; Elliott et al. 2009, Voicu
  2018), with a hometown-county variant (`race_bifsg_geo`). Names carry
  little information in this population, and BIFSG labels most Black coaches
  white. Use it only for misclassification-correction exercises.

All R samples take race from `load_person_race()` in
`programs/00-race-measures.R`, which keeps the three measures in separate
columns.

## Analytical samples

`Rscript programs/95-make-all.R` (about 1 minute) writes
`data/datasets/analysis/<name>.parquet` and `.csv`, each with a
`codebook_<name>.csv`. Some samples also write a `coverage_<name>.csv`
reporting signal coverage by season or class:

| Sample | Unit |
|---|---|
| `staff_person_season` | franchise x season x staff member (roles, tenure, race measures) |
| `team_season` | franchise x season: outcomes, market expectations, staff composition by group, HC/coordinator/GM race, turnover, Rooney Rule eras |
| `team_game` | franchise x game: outcomes, spreads, weekly HC, staff composition in force on game day |
| `player_season` | player x season: usage, snaps, production, pay, pre-NFL signals, race |
| `contracts` | contract: terms, market margin, pre-signing productivity, pre-NFL signals, race |
| `draft_prospects` | draft prospect: draft outcome, pre-draft signals, later NFL outcomes, race |

`programs/07-table-summary-statistics.R` writes summary-statistics and
coverage tables to `output/tables/` and `my_paper/tables/`.

## Known limitations

- Hand-coded race is empty until coding is done; every race-based variable in
  the samples is NA or provisional until then.
- Staff come from Wikipedia editors' records. Templates can lag changes (the
  median snapshot uses a revision 45-57 days old), and 1999-2006 boxes are
  retrospective. OC, DC and GM are title-based: a head coach who calls plays
  leaves the OC or DC slot empty.
- No college box scores before 2009 (FBS) or 2022 (FCS; before then only games
  against FBS opponents). This matters for HBCU alumni. No college assistant
  coaches.
- OverTheCap records unknown guarantees as 0; there is no signing date.
- Some pre-2002 NFL data are thin: weekly rosters start in 2002.

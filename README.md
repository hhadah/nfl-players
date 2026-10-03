# NFL coaching-staff diversity and player pay

Data infrastructure for two questions:

1. Does racial diversity of NFL coaching staffs and front offices improve team
   performance?
2. Is there racial discrimination in NFL player pay and contracts, and can it
   be distinguished from statistical discrimination by conditioning on
   high-school, college and NFL productivity?

The repository builds one DuckDB database from public sources (Python,
`scripts/`). From it, R (`programs/`) builds the analytical samples, the data
exhibits, and the estimation exhibits for the three questions:

- the pay gap at the same position;
- roster diversity and team performance;
- staff diversity and team performance.

Race is **predicted** (see
[notes/race-prediction-design.md](notes/race-prediction-design.md)). Each
person's probability of being Black combines names and hometown with an
NFL-specific prior estimated by EM, and the regressions use the probabilities
(regression calibration). Two further measures are kept:

- hand coding ([notes/race-coding-protocol.md](notes/race-coding-protocol.md)),
  which becomes primary automatically if the sheets are ever filled;
- the Wikipedia flag, as a sensitivity measure.

The estimation specifications are in
[notes/analysis-plan.md](notes/analysis-plan.md).

## Layout

```
scripts/            Python: data acquisition, linkage, race inference, validation
programs/           R: analytical samples, summary tables, race-coding agreement
data/raw/           API caches (nflverse parquet, CFBD JSON, Wikipedia JSON, Census) — gitignored
data/datasets/      nfl_research.duckdb and analysis/ samples — gitignored, regenerated
data/hand_coded/    human inputs, tracked: race-coding sheets, school-name and
                    head-coach-date corrections
data/derived/       LOCAL ONLY (gitignored; person-level race): race statements
                    classified once from Wikipedia article text
                    (race_text_labels.csv, race_text_classification.csv);
                    used by the preddoc variant and the validation only
data/reference/     tracked: published TIDES NFL race shares with citations
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
`fwildclusterboot` is not on CRAN:
`install.packages("fwildclusterboot", repos = c("https://s3alfisc.r-universe.dev", "https://cloud.r-project.org"))`.
`birdie` (CRAN) is used through `birdie::`.

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
| `04d_race_documented.py` | Wikidata (ethnic group, P172), Wikipedia article text | `race_wikidata_ethnicity`, `race_text_candidates`, `race_article_coverage` |
| `04e_predict_race.py` | `race_bifsg`, documented race, `data/derived/race_text_labels.csv`, TIDES | `race_predicted` (primary model-only and documented variants, draft-free, raked and Black-or-multiracial variants), `race_predicted_priors` |
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

All R samples take race from `load_person_race()` in
`programs/00-race-measures.R`. The estimation scripts pick a measure with
`choose_race_measure()` in `programs/00-analysis-functions.R`.

- **Primary: predicted race** (`race_predicted`; the design is in
  `notes/race-prediction-design.md`). P(race) combines two pieces:
  - the BIFSG name and hometown-county likelihood;
  - an NFL prior estimated by EM on the full population, using
    predetermined characteristics only:
    - players: position at entry, rookie era, draft round, college type,
      county availability;
    - staff: role, unit and era at first appearance.

  Documented race is not an input to the primary measure.

  **Pay regressions** regress on P(Black) and P(other race) and control for
  the prior's covariates (regression calibration). A BIRDiE cross-check is
  included (McCartan, Fisher, Goldin, Ho and Imai 2025).

  **Team regressions** use expected shares, the mean member probability.

  **Validation** (`programs/15`):
  - Predicted shares are compared with published TIDES shares
    (`data/reference/`).
  - On persons with documented race, the AUC is about 0.94 for players and
    0.97 for staff, Black versus white.
  - Assistant coaches are under-predicted by about 10 pp. Individual coaches'
    probabilities are noisy (reliability about 0.4 for head coaches).
- **Documented race** comes from three public sources:
  - Wikidata ethnic group;
  - Wikipedia categories;
  - statements in Wikipedia article text, classified by two independent
    model passes with adjudication (`data/derived/`).

  It is used for validation and for the `preddoc` sensitivity measure.
  Documentation depends on fame, so it is never the primary treatment.
- **Hand coding** follows
  [notes/race-coding-protocol.md](notes/race-coding-protocol.md). The sheets
  are empty. If hand codes cover at least 80% of a script's sample, that
  script uses them automatically. `programs/08-race-coding-agreement.R`
  reports agreement.
- **Sensitivity measures:**
  - `preddoc` (documented, else predicted);
  - `provisional` (the positive-only Wikipedia flag).

  Run either with `NFL_RACE_MEASURE=preddoc` or `NFL_RACE_MEASURE=provisional`.
  Their exhibits get a `-<measure>` suffix and are written to `output/` only.

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

Estimation samples and exhibits (`notes/analysis-plan.md`):

| Script | Output |
|---|---|
| `09-roster-composition-sample.R` | `roster_composition_team_season`, `roster_composition_team_game`: roster race composition by weighting scheme and unit (headcount, snaps, opening day), expected and residual shares given position mix, QB race, roster quality |
| `10-pay-analysis-sample.R` | `analysis_pay_contracts`, `analysis_pay_player_season`, `pay_control_blocks.csv` (position-specific quality controls, blocks A-F) |
| `11-team-analysis-sample.R` | `analysis_team_season` (with leads and lags), `analysis_team_unit_season` (offense/defense stack), `analysis_team_game` (with opponent composition) |
| `12-table-pay-gap.R` | Tables 7-13, 26 (BIRDiE), 27 (race measures): veteran pay gap, Gelbach decomposition, terms, by position, employer learning, draft margin |
| `13-table-roster-diversity-performance.R` | Tables 14-18b, 28: roster diversity and team performance, game-level design against the spread, placebo tests |
| `14-table-staff-diversity-performance.R` | Tables 19-23, 29: staff diversity, head-coach-spell FE, offense/defense stack, head-coach hires, placebo tests |
| `15-table-race-prediction-validation.R` | Tables 24-25 and figures: predicted race against TIDES and against documented race |

Each estimation script also writes tidy coefficients to `output/estimates/`.
Inference clusters by player (pay) or by franchise (teams). Team tables add
wild cluster bootstrap p-values (fwildclusterboot, Webb weights) because there
are only 32 clusters.

## Known limitations

- Race is predicted, not observed:
  - Names separate Black and white Americans imperfectly. About half of the
    variation in a player's P(Black) comes from the prior (position, draft,
    college).
  - Regression calibration needs the probabilities to be calibrated given
    each regression's controls. The pay script reports a diagnostic, and the
    full pre-NFL control set still predicts P(Black) within prior cells.
  - Coach-level probabilities are noisy.
- The team-level placebo tests fail: next-season composition predicts current
  outcomes, and past outcomes predict composition. Read the roster and staff
  associations as descriptive, not causal.
- Staff come from Wikipedia editors' records. Templates can lag changes (the
  median snapshot uses a revision 45-57 days old), and 1999-2006 boxes are
  retrospective. OC, DC and GM are title-based: a head coach who calls plays
  leaves the OC or DC slot empty.
- No college box scores before 2009 (FBS) or 2022 (FCS; before then only games
  against FBS opponents). This matters for HBCU alumni. No college assistant
  coaches.
- OverTheCap records unknown guarantees as 0; there is no signing date.
- Some pre-2002 NFL data are thin: weekly rosters start in 2002.

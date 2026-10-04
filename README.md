# NFL coaching-staff diversity and player pay

Data and empirical analyses for three questions:

1. Does racial diversity of NFL coaching staffs and front offices improve team
   performance?
2. Are there racial gaps in player pay, employment duration and access to
   freely bargained contracts, conditional on measured productivity?
3. How did coaching hires, retention and promotion change across the
   Rooney Rule's distinct hiring and staffing policies?

The repository builds one DuckDB database from public sources (Python,
`scripts/`). From it, R (`programs/`) builds the analytical samples, the data
exhibits, and estimation exhibits for:

- the pay gap at the same position;
- roster diversity and team performance;
- opening-staff diversity and team performance;
- policy-era hiring, retention and promotion;
- player retention and first observed bargained-contract access.

Race is **predicted** (see
[notes/race-prediction-design.md](notes/race-prediction-design.md)).
The model-only probability targets **non-Hispanic Black alone**, not Black
alone-or-in-combination. It combines names and hometown with an NFL-specific
prior estimated by EM. Regressions use probabilities, conditional on
calibration and outcome-model assumptions. Two further measures are kept:

- hand coding ([notes/race-coding-protocol.md](notes/race-coding-protocol.md)),
  which becomes primary automatically if the sheets are ever filled;
- the Wikipedia flag, as a sensitivity measure.

The estimation specifications are in
[notes/analysis-plan.md](notes/analysis-plan.md).

## Layout

```
scripts/            Python: data acquisition, linkage, race inference, validation
programs/           R: samples, estimates, exhibits, temporal checks and results memo
data/raw/           API caches (nflverse parquet, CFBD JSON, Wikipedia JSON, Census) — gitignored
data/datasets/      nfl_research.duckdb and analysis/ samples — gitignored, regenerated
data/hand_coded/    human inputs, tracked: race-coding sheets, school-name and
                    head-coach-date corrections and staff identity aliases
data/derived/       LOCAL ONLY (gitignored; person-level demographic inputs):
                    classified race statements, coach program participation,
                    sourced appointment dates and gender documentation
data/reference/     tracked: TIDES series, policy registry and program coverage
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

The R driver rebuilds the samples, runs `94-verify-analysis.R` before
estimation, then regenerates the tables, coefficient manifests and
`notes/results-memo.md`.

The first Python run downloads everything (about 1 GB, mostly play-by-play)
and spends about 300 of the free CFBD tier's 1,000 monthly calls. Every
response is cached under `data/raw/`; rebuilds reuse those caches. Runtime
depends on uncached pages and the EM fit. `--refresh` forces re-downloads;
`--only <step> ...` runs selected stages in dependency order.
`06_validate_db.py` runs last and fails on critical key, coverage, timing,
historical-record and value-range problems.

| Step | Source | Main tables |
|---|---|---|
| `00_fetch_reference.py` | Census 2010 surnames; Tzioumis (2018) first names; Census county race shares | reference files |
| `01_build_db.py` | — | `franchise_seasons` (team key crosswalk) |
| `02_load_nfl.py` | nflverse (nflreadpy) | `nfl_players`, `nfl_rosters_weekly`/`_season`, `nfl_draft_picks` (1980-2026), `nfl_combine`, `nfl_contracts`, `nfl_contract_history`, `nfl_contract_years`, `nfl_injuries`, `nfl_depth_charts`, `nfl_trades`, `nfl_ids` |
| `02c_load_nfl_stats.py` | nflverse | `nfl_schedules`, `nfl_team_games`, `nfl_team_seasons`, `nfl_player_stats_week`/`_season`, `nfl_snap_counts`, `nfl_pfr_advstats_season`, `nfl_nextgen_stats`, `nfl_team_week_head_coach` |
| `02b_load_nfl_staff.py` | Wikipedia staff templates and season articles | `staff_team_season`, `staff_persons`, `staff_entries`, `staff_snapshots`, `staff_person_wiki_signals`, `staff_hc_reconciliation` |
| `02d_load_coach_history.py` | archived PFR; Wikipedia team-season records | `nfl_hc_history`, `nfl_hc_history_coaches`, `nfl_hc_history_coverage` (1989-1998) |
| `02e_coach_policy_reference.py` | NFL program lists, Wikidata, Wikipedia biographies | local coach-policy participation, gender and appointment-date evidence; aggregate program coverage |
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
| Coaching staff and front office | 2007-2025 full; 1999-2006 partial | 3,598 persons. Opening snapshots are selected at midnight UTC on each team's first REG game date, followed by November 1 and late-season snapshots. Revision dates, staleness and damaged-box fallback flags are retained. |
| Historical head coaches and team records | 1989-1998 | 288 franchise-seasons; 1989 supplies the lag for 1990 hire and performance comparisons. Multi-coach order and documented interim status are retained; unknown appointment status is not imputed. |
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

  **Adjusted pay regressions** regress on P(Black) and P(other race) and
  control for the prior's covariates. Player FE absorb time-invariant race
  levels; their interactions describe pay evolution instead. The annual
  raw figure deliberately has no position, quality or prior-covariate
  controls and is labeled model-implied, not an observed-race mean gap.
  A BIRDiE cross-check is included (McCartan, Fisher, Goldin, Ho and Imai 2025).

  **Team regressions** use expected shares, the mean member probability.

  **Validation** (`programs/15`):
  - Predicted shares are compared with published TIDES series, retaining
    category and timing differences.
  - Model-event calibration uses documented non-Hispanic Black-alone
    labels; broad Black-any AUCs are reported separately.
  - Documentation is fame-selected. Positive-only head-coach bins are
    ascertainment lower bounds, not observed-race calibration.
  - Current estimates and uncertainty are in the generated
    [results memo](notes/results-memo.md), rather than duplicated here.
- **Documented race** comes from three public sources:
  - Wikidata ethnic group;
  - Wikipedia categories;
  - statements in Wikipedia article text, classified by two independent
    model passes with adjudication (`data/derived/`).

  It is used for validation, `preddoc` sensitivity, and explicitly
  documented-positive coaching-policy comparisons. Fame-selected
  documentation is not representative observed race.
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

`Rscript programs/95-make-all.R` writes
`data/datasets/analysis/<name>.parquet` and `.csv`, each with a
`codebook_<name>.csv`. Some samples also write a `coverage_<name>.csv`
reporting signal coverage by season or class:

| Sample | Unit |
|---|---|
| `staff_person_season` | franchise x season x staff member (roles, tenure, race measures) |
| `staff_person_opening_season` | franchise x season x member listed in the opening snapshot; opening roles, turnover and snapshot provenance |
| `team_season` | franchise x season: outcomes; opening `*Pre` and season-union composition; first-game and incumbent HC identities and spells; distinct policy indicators |
| `team_game` | franchise x game: outcomes, spreads, weekly HC, staff composition in force on game day |
| `player_season` | player x season: usage, snaps, production, pay, pre-NFL signals, race |
| `contracts` | contract: terms, market margin, pre-signing productivity, pre-NFL signals, race |
| `draft_prospects` | draft prospect: draft outcome, pre-draft signals, later NFL outcomes, race |
| `coach_policy_person_season` | opening coach x franchise x season: documented eligibility, participation, and one-/two-year transitions |
| `coach_policy_team_unit_season` | franchise x season x offense/defense: eligibility bounds, entries, retention and promotion |
| `coach_job_spells` | consecutive opening observations in a role: listing intervals, sourced appointment dates where available, censoring |
| `analysis_rooney_hires` | franchise x season, 1990-2025: opening HC, hire status, documented-positive demographics, inherited and subsequent outcomes |
| `analysis_player_retention` | player x season, 2002-2025: employment status, deduplicated weeks, risk sets and observed contract access, including players without pay records |

`programs/07-table-summary-statistics.R` writes summary-statistics and
coverage tables to `output/tables/` and `my_paper/tables/`.

Estimation samples and exhibits (`notes/analysis-plan.md`):

| Script | Output |
|---|---|
| `09-roster-composition-sample.R` | roster composition by weighting scheme and unit; position-mix expectations, quality, and `L1TeamCapShare` |
| `10-pay-analysis-sample.R` | `analysis_pay_contracts`, `analysis_pay_player_season`, `pay_control_blocks.csv` (position-specific quality controls, blocks A-F) |
| `11-team-analysis-sample.R` | `analysis_team_season` (with leads and lags), `analysis_team_unit_season` (offense/defense stack), `analysis_team_game` (with opponent composition) |
| `12-table-pay-gap.R` | Tables 7-13, 12b-12d, 26 (BIRDiE), 27 (race measures): veteran pay, career profiles with/without player FE, common-sample paying-franchise FE, successive contracts, Gelbach decomposition and draft margin |
| `12a-pay-gap-by-year.R` | Raw and adjusted annual White-minus-Black veteran-APY figures on identical contracts; `12-pay-gap-by-year` and support manifests; runs immediately after 12 |
| `13-table-roster-diversity-performance.R` | Tables 14-18b, 28: roster diversity and team performance, game-level design against the spread, placebo tests |
| `14-table-staff-diversity-performance.R` | Tables 19-23, 29: opening-staff composition, incumbent-HC-spell FE, unit stack and hires; season-union and conditional-exchangeability permutation sensitivities |
| `15-table-race-prediction-validation.R` | Tables 24-25 and figures: predicted race against TIDES and against documented race |
| `16-coach-policy-sample.R` | coach job and policy panels, eligibility/participation coverage, sourced hire dates and TIDES hire benchmark |
| `17-table-rooney-policy.R` | Tables 30-33 and figures: published trends, original-rule hires, race-by-era performance, offense/defense staffing and transitions; pretrends, placebo breaks, bounds and MDEs |
| `18-player-retention-sample.R` | player employment and first observed contract-access risk sets; season-t last under-contract employer, with tied employer listings left unknown |
| `19-table-player-retention.R` | Tables 34-35, 34b and 34c: retention, contract access, race/definition sensitivities and common-sample employer-by-season FE; sample flow and uncertainty |
| `94-verify-analysis.R` | temporal, opening-staff, incumbent-spell, lagged-control and transition invariants |
| `20-write-results-memo.R` | regenerates `notes/results-memo.md` from primary coefficient manifests after verification |

Coefficient manifests are exported as CSV and Stata DTA in
`output/estimates/`. Inference clusters by player or franchise. Existing
team-composition tables also report wild-cluster bootstrap inference
(`fwildclusterboot`, Webb weights); the new policy tables record their own
clustered inference, baselines and approximate MDEs.

LaTeX exhibits use `booktabs`, `graphicx`, `float`, `threeparttable` and
`natbib`. Estimation-table notes follow fixed-position bodies and can break
across pages; they are not squeezed into an oversized float.

### Pay profiles and annual gap figures

`figure-pay-gap-by-year-raw` and `figure-pay-gap-by-year-adjusted` are saved
as PDF and PNG in `output/figures/`. Both use the same freely bargained
UFA/extension contracts and race scores. Positive values mean higher White
APY. The axis is `100 * (exp(White-minus-Black log APY) - 1)`, relative to
Black geometric pay, not a difference in arithmetic mean wages. The adjusted
series uses Table 8 column 5 controls with common slopes across years and
position-by-signing-year FE. Intervals are pointwise 95 percent intervals,
clustered by player. The 2026 signing window is marked incomplete.

Table 12b and `figure-pay-career-fe` compare career profiles on a common
repeat-player sample, relative to experience 4-6. They do not identify a
race-level gap under player FE. Table 12c compares annual cap and cash
accounting amounts within paying-franchise seasons. Table 12d follows
distinct signed deals, truncates sequences at ambiguous same-year signings,
and separates contract-stage profiles from adjacent-deal changes. Neither
fixed effects nor repeated contracts resolve race measurement or selection.

## Known limitations

- Race is predicted, not observed:
  - Names separate Black and white Americans imperfectly. About half of the
    variation in a player's P(Black) comes from the prior (position, draft,
    college).
  - Regression calibration needs probabilities calibrated given each
    regression's controls. Controls predicting the score do not test that
    condition. Table 8 separates score overlap from a selected-documentation
    conditional check; neither validates population calibration.
  - Coach-level probabilities are noisy.
- Lead, reverse-selection and policy pretrend diagnostics are reported,
  not used to certify identification. A league-wide date supplies no
  untreated clubs, and offense versus defense is not a randomized design.
  Read the estimates as descriptive conditional associations.
- Wikipedia templates can be stale or omit roles even when their revision
  precedes kickoff. Pre-2007 boxes are retrospective. Listing intervals
  are not employment dates. OC, DC and GM are title-based.
- NFL policy eligibility means a woman or minority under league rules,
  not a predicted-Black threshold. Unknown eligibility stays unknown.
  The acquired public sources do not provide a complete designated-assistant
  roster, reimbursements, or exact non-HC appointment and departure dates.
- No college box scores before 2009 (FBS) or 2022 (FCS; before then only games
  against FBS opponents). This matters for HBCU alumni. No college assistant
  coaches.
- OverTheCap records unknown guarantees as zero and provides signing years,
  not exact dates. Roster weeks are not paid weeks. Contract-access outcomes
  are first observed contracts, not proof of complete historical coverage.
- The 2016 weekly-roster feed contains preseason spillover. Ambiguous-only
  future listings stay unknown. Primary retention excludes t=2015; alternative
  source-break treatments are reported as sensitivities.
- Some pre-2002 NFL data are thin: weekly rosters start in 2002.

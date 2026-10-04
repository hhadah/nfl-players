# Analysis plan: race, pay and team performance

Revised 2026-10-03. This file specifies the estimation samples and
specifications for five questions:

1. **Pay.** Conditional on every quality signal in the data, are Black NFL
   players paid more or less than white players at the same position?
2. **Roster diversity.** How do teams with more racially diverse rosters
   perform?
3. **Staff diversity.** How do teams with more racially diverse coaching
   staffs and front offices perform?
4. **Coaching policies.** How did hiring, inherited situations, relative
   performance, retention and promotion change across policy eras?
5. **Player employment.** Conditional on measured quality, who remains under
   contract and who reaches a first observed UFA or extension contract?

The code is in `programs/09`-`19` and runs from `programs/95-make-all.R`.
`94-verify-analysis.R` checks temporal and risk-set invariants; `20` generates
the results memo from coefficient manifests.

## Race measures

Revised 2026-10-02: the PI decided not to hand-code race. The **primary
measure is predicted race**, documented in `notes/race-prediction-design.md`.

**How it is built.** The model-only probability of non-Hispanic Black alone
combines:

- **Likelihood:** first name, surname and hometown county (BIFSG).
- **Prior:** an NFL-specific prior estimated by EM on predetermined
  characteristics, from `scripts/04e_predict_race.py` (table `race_predicted`).
  - Players: position at entry, rookie era, draft bucket, college type and
    county availability.
  - Staff: first role, unit and first-season era.

Documented statements are excluded from the primary prediction model. They
support event-matched validation, `preddoc` sensitivity and separately
labeled documented-positive coaching-policy comparisons.

**Measure switch.** `choose_race_measure()` in
`programs/00-analysis-functions.R` picks the measure:

- The primary measure is `hand` when hand codes cover at least 80% of the
  estimation sample, and `predicted` otherwise.
- `NFL_RACE_MEASURE=hand|predicted|preddoc|provisional` forces a measure.

**Output routing.**

- Primary-measure exhibits keep their base names and go to `output/tables`
  and `my_paper/tables`.
- Other measures get a `-<measure>` suffix and go to `output/` only.

**Person level.** `person_race_regressors()` builds the race regressors:

- **Black, model-only:** P(non-Hispanic Black alone), despite the legacy
  `p_black_any_pred` name.
- **OtherRace:** 1 - P(Black) - P(non-Hispanic white alone).
- **Hand/documented Black-any:** Black alone-or-in-combination. `preddoc`
  mixes this documented event with the model event for undocumented people.
- **Provisional:** a positive-only documented flag, not observed race for
  everybody without a positive flag.

The coefficient is a latent Black-white gap only under a linear, common-gap
outcome model, exclusion of names/hometown from the outcome conditional on
race and controls, and calibration given **all** controls. Including the
prior's covariates is not sufficient. Table 8 reports residual score SD
(overlap) and one documented-label control-index check. Controls predicting
the score do not establish miscalibration or mechanical attenuation.

**Team level.** `apply_race_measure()` maps the measure's columns to generic
names:

- `ShareBlackPred<G>`, the expected share (mean member probability), becomes
  `ShareBlack<G>`; likewise `ShareBlackPredDoc`, `ShareBlackHand` and
  `ShareBlackProv`.
- `HCBlackPred` becomes `HCBlack`.

The two-group Blau index is 2s(1 - s).

**Sensitivity measures.**

- `preddoc`: documented where a public source states race, otherwise
  predicted. Fame-dependent.
- `provisional`: the positive-only Wikipedia flag. A lower bound.

## 1. Pay gap at the same position

### Samples (`programs/10-pay-analysis-sample.R`)

**`analysis_pay_contracts`.** Unit: a contract. Source: `contracts`, with
`SampleMain == 1`, `InNflPlayers == 1`, `GsisLinkSuspect != 1`, a non-missing
OTC `position`, and `year_signed` 2014-2026. Contracts signed from 2014 on
have prior-season snap counts (snaps start in 2013).

- `VeteranMarket == 1` marks the main sample, the freely bargained margin
  (UFA, re-sign or extension, tag or tender; about 6,500 contracts).
- `NonRookieSample` adds `Other/SFA/Practice` contracts, for the
  minimum-salary margin.

The sample carries the full race vector from `load_person_race()`:

- the hand codes `race`, `hispanic`, `black_any` and `race_source`;
- `black_provisional`;
- every Wikipedia flag (`wiki_cat_black`, `wiki_cat_hispanic_latino`,
  `wiki_cat_asian`, `wiki_cat_pacific_islander`, `wiki_cat_native_american`);
- BIFSG `p_black_bifsg` (descriptive only).

It also carries the quality blocks below, built with `fill_missing()` so that
incomplete coverage does not drop observations.

**`analysis_pay_player_season`.** Unit: a player-season with realized pay.
Source: `player_season`, with `HasPay == 1`, seasons 2014-2025,
`InNflPlayers == 1` and Experience 0-15.

- Outcome: `LogCapPct = log(CapPercent)` (CapPercent > 0). Alternatives are
  `log(GoverningAPY / season cap)` and `log(CashPaid)`.
- One-season lagged production (`Lag*`), career production to date, pre-NFL
  signals, and the race vector.
- `OnRookieContract` separates slotted rookie pay from bargained pay.

The rookie margin uses `draft_prospects` directly (classes 2011-2022).

### Quality blocks (lagged by signing year)

Production enters with **position-specific slopes**. Each variable is
interacted with position group (QB, RB, WR, TE, OL, DL, LB, DB, K, P, LS),
but only for the groups where it measures performance:

| Block | Variables (prefix `Prior` = season before signing; `Career` = all seasons before signing) |
|---|---|
| A. Career stage | `ExperienceBin`, `AgeAtSigning` and its square, `ExperienceLeftCensored`, `MarketMargin` |
| B. Prior-season production | QB: pass att/yds/TD/INT, PassEPA, sacks taken, rush yds. RB: rush att/yds/TD, rec/yds/TD. WR, TE: targets, rec, yds, TD, RecEPA. DL, LB: tackles, sacks, QB hits, TFL, forced fumbles; PFR pressures and missed-tackle pct (2018+). DB: tackles, INT, passes defended; PFR targets and yards per target allowed (2018+). K: FG made/att, XP. P: punts, net yards. All: games played, injury weeks, `NoPriorSeason` |
| C. Career NFL production | career games, career starts (depth chart), career injury weeks, and career sums of the block B production variables for the same groups |
| D. Pre-NFL signals | log draft pick and `Undrafted`; draft round FE; 247 rating and stars; combine (forty, vertical, bench, broad, cone, shuttle, height, weight) with position-group slopes; `FinalCollegePower`, `FinalCollegeSRS`, `FinalCollegeHBCU`; missing indicators |
| E. Usage (coach-chosen; a possible bad control) | prior offensive, defensive and ST snaps; prior games started (snaps); career offense + defense snaps |
| F. Scout grade (may embed bias) | `PreDraftGrade` |

OTC gives signing years, not exact dates. A prior NFL season can extend into
January of the signing year; without dates, these controls cannot be
certified to precede every early-January signing. Usage and market signals
can also encode earlier selection.

### Changes made during implementation (2026-10-02 review)

- **Main sample:** freely bargained veteran contracts, i.e. UFA contracts and
  extensions (`BargainedMarket == 1`). Tags and tenders are priced by CBA
  formula and enter only the full veteran-market robustness columns.
- **Outcome:** log APY. Position x year FE absorb log cap_t, so this
  identifies the same beta_1 as log APY share of the cap, without OTC's
  rounding of `apy_cap_pct` to 0.001.
- **Player-season panel:** log cap number, because `CapPercent` is rounded.
- **Block E:** includes career depth-chart starts.
- **Athletic score:** the RAS-style score (`combine_athletic_scores()`) is in
  block D.

### Specifications (`programs/12-table-pay-gap.R`)

Equation (1), for contract c of player i at position p, signed in year t:

    Y_ipt = beta_1 Black_i + beta_2 Other_i + X_ipt pi + delta_{p,t} + e_ipt

- Main outcome: `LogAPY`, the log of APY in millions of dollars. Position
  by signing-year FE absorb the league cap without rounding its share.
- delta_{p,t}: OTC position x year-signed FE (18 market positions).
- Standard errors are clustered by player.

**Table: veteran-contract pay gap (progressive controls).**

| Column | Adds |
|---|---|
| (1) | position x year FE only (raw gap within position-year) |
| (2) | + A, career stage |
| (3) | + B, prior-season production |
| (4) | + C, career production |
| (5) | + D, pre-NFL signals (main specification) |
| (6) | + E, usage |
| (7) | + signing franchise x year FE |

**Table: Gelbach (2016) decomposition.** This decomposes the change in beta_1
from column (1) to column (5) into the contributions of blocks A-D. Blocks
enter as explicit interaction columns, not fixest slopes, so that each block's
fitted contribution can be computed. Standard errors come from a player-cluster
bootstrap (199 replications).

**Table: other terms and margins** (column (5) controls):

- `GuaranteeShare` (excluding `GuaranteedZero == 1`);
- log years;
- log APY in dollars;
- UFA contracts only; re-sign/extension only; UFA with a new team only;
- `NearMinimum` (LPM) in `NonRookieSample`, signed 2017-2026.

**Figure and table: gap by position group.** Column (5) with Black interacted
with position group.

**Table: player-season panel and employer learning** (Altonji and Pierret
2001):

- Outcome: `LogCapNumber`; position-group x season FE.
- Black x experience-bin interactions, with and without lagged and career
  production.
- Pre-NFL signals interacted with experience.
- Main sample: UFA/extension-governed seasons; broader non-rookie and
  all-contract samples are separate specifications.
- Standard errors are clustered by player.

**Career-profile and employer extensions, 2026-10-03.**

Table 12b compares position-group-by-season FE against player plus
position-group-by-season FE on the same repeat-observed veteran-pay players.
Single-season players are excluded from both fits. Interact both Black and
other-race scores with experience bins, using 4-6 as the common reference.
The player-FE specifications omit the static race main terms because those
are absorbed, not because their levels are zero. Each model pair requests
the same controls: A, A-C, A+D with signal-by-experience terms, or A-D with
those terms. Time-invariant covariates are absorbed; position-specific slopes
are retained when position changes make them vary within player. Report
cross-bin player support, player-clustered pointwise CIs and joint tests.
The difference between profiles is not a causal decomposition or a bound on
survivor selection, and experience also advances with calendar time.

Table 12c compares position-group-by-season FE with and without
`PayFranchise`-by-season FE on identical known-employer rows. Outcomes are
log cap number and, on its positive-cash sample, log cash paid. The paying
franchise comes from the selected OTC cap-table row, not necessarily the
primary roster team. Both outcomes remain annual accounting amounts.

Table 12d uses distinct signed veteran deals. Order is defined by signing
year; sequences stop before their first ambiguous same-year signing.
Never bridge that omitted year or call the first observed eligible deal the
first career contract. Compare the second and third-or-later deals with the
first, interacting stage with both race scores and absorbing player and
OTC-position-by-signing-year FE. Add career-stage and prior/career-production
controls on a common repeat-signer sample. Adjacent-deal change models
instead absorb separate position-by-year effects for both deals and compare
changes with and without differenced controls. These are per-deal changes,
not annual growth rates, and not algebraically identical first differences
of the level specifications. Exact signing dates are unavailable, so
prior-season statistics can overlap an early-January signing.

**Annual raw and adjusted figures (`programs/12a-pay-gap-by-year.R`).**

Use the same freely bargained veteran contracts and the same race scores
in both models. Drop position-year singleton cells from both. The raw model
includes signing-year FE and year-specific Black and other-race score
coefficients, with no position, quality or prior-covariate controls.
The adjusted model includes OTC-position-by-signing-year FE and Table 8
column 5 controls, including the race-prior covariates. Race coefficients
vary by year; quality-control slopes are pooled across years.

Reverse the Black coefficient and both CI endpoints to report
White-minus-Black log APY. Plot `100 * (exp(gap) - 1)`, relative to Black
geometric pay, with pointwise player-clustered 95 percent intervals.
Positive values mean higher White APY. This is not the percent difference
in arithmetic means. Under predicted race, both series are model-implied
contrasts; the raw series is not an observed-race group mean. Conditioning
changes calibration requirements as well as residual score variation.
Mark the partial 2026 signing window and use the same vertical axis in
both figures. Export annual sample counts and probability sums with the
estimates as CSV and DTA.

**Table: rookie margin.**

- `LogPick` on Black, among drafted prospects. Controls: combine, recruit
  rating, college production and quality, `AgeAtDraft`. FE: class x position
  group.
- An LPM of `Drafted` among combine invitees.
- Each with and without `PreDraftGrade`.
- Standard errors are clustered by final college team.

### Threats

- **Omitted quality.** There are no PFF grades and no Pro Bowl or All-Pro by
  season, and offensive linemen have no box-score production.
- **Bad controls.** Snaps and starts are chosen by coaches, so controlling for
  usage absorbs any usage discrimination. That is why block E enters last and
  separately.
- **Selection.** Only players who survive to a veteran contract are observed
  (see B6 in EXTENDING.md).
- **Measurement.** `GuaranteedZero` mixes zero and unknown guarantees. OTC has
  no signing date, so "prior season" means year_signed - 1.

## 2. Roster diversity and team performance

### Samples

`programs/09-roster-composition-sample.R` builds:

- **`roster_composition_team_season`** (franchise x season, 2002-2025). For
  each group, the Black share under each measure, using the existing naming
  pattern (`ShareBlackHand<G>`, `CodedShare<G>`, `ShareBlackProv<G>`,
  `MeanPBlackBifsg<G>`) plus `N<G>`. The groups are:
  - game-day roster, weighted by player-weeks (`Roster`);
  - offense + defense snap-weighted (`SnapW`, 2013+);
  - offense and defense units, by snaps (`OffenseSnapW`, `DefenseSnapW`) and
    by headcount;
  - each position group.

  It also builds:
  - expected Black shares given the team's position mix:
    `ExpectedShareBlack<Measure><G>` = sum over positions of (the team's
    position weight x the league-season Black share at that position,
    excluding the team);
  - the starting QB's race (`QBBlack<Measure>`, from the QB with the most
    starts).

  It also builds roster-quality controls from `player_season`:
  - annual total cap share and its consecutive-season lag, `L1TeamCapShare`;
  - snap-weighted mean of log draft pick (undrafted = log 300);
  - snap-weighted share of first-round picks;
  - mean age and experience;
  - the number of distinct starters.
- **`roster_composition_team_game`** (franchise x game, 2002-2025). The same
  shares for the game-day active roster of that week.

`programs/11-team-analysis-sample.R` builds:

- **`analysis_team_season`**: `team_season` + `roster_composition_team_season`,
  one-season leads of the composition measures (for placebo tests), and unit
  rows for the stacked offense/defense design.
- **`analysis_team_game`**: `team_game` + `roster_composition_team_game`.

### Specifications (`programs/13-table-roster-diversity-performance.R`)

Equation (2), for franchise f in season t:

    Y_ft = beta ShareBlackRoster_ft + X_ft pi + alpha_f + gamma_t + e_ft

Outcomes:

- `WinPct`, `PointDiffPerGame`, `WinsOverExpected`;
- `OffEPAPerPlay` and `-DefEPAPerPlay`.

Columns:

1. season FE;
2. + franchise FE;
3. + `LagWinPct` and roster quality;
4. + staff share Black and HC race;
5. diversity net of position mix (actual minus expected share);
6. two-group Blau index.

The snap-weighted estimation panel is 2014-2025 because `L1TeamCapShare`
first exists in 2014. All columns use that common window. Predetermined
controls use opening-roster quality and lagged cap totals; current annual
cap totals appear only in an explicitly contemporaneous sensitivity.
Opening-roster/headcount comparisons are separate. Franchise-clustered
SEs and wild-cluster bootstrap inference (Webb weights, 9,999 replications)
are reported. Snap weights remain same-season, not predetermined.

Placebo tests:

- the one-season lead of roster composition, conditional on current
  composition;
- the opponent's roster composition in the game-level design.

The game-level outcome is `margin`, with a freely estimated coefficient on
the market spread, franchise-by-season and opponent-by-season FE. The
game-day active-roster share varies with injuries, inactives and signings.
This is descriptive within-team-season variation, not random assignment.
The ATS residual is a sensitivity outcome. SEs cluster by franchise.

### Threats

- **Positional composition.** Black shares differ sharply by position, which
  is why the regressions include expected shares and position-mix controls.
- **Talent and selection.** Quality controls, lagged outcomes, leads and
  market spreads diagnose or condition on observed differences; they do not
  eliminate unobserved quality, reverse causality or selection into play.
- **Quarterback.** The QB's race enters separately.

## 3. Staff diversity and team performance

### Specifications (`programs/14-table-staff-diversity-performance.R`)

Equation (3) uses `OpeningStaffObserved` team-seasons, 2007-2025. Opening
roles and composition are rebuilt from raw opening-snapshot entries, not
filtered season-union roles:

    Y_ft = beta ShareBlackCoachesPre_ft + theta HCBlackPre_ft + X_ft pi + alpha_f + gamma_t + e_ft

Controls X:

- `LagWinPct` and `LagExpectedWins`;
- opening turnover, `ShareCoachesNewToFranchisePre` and
  `ShareCoachesPromotedPre`, with unknown first-period turnover flagged;
- lagged roster quality/composition, or opening-roster controls in the
  designated comparison. Current annual cap totals are not predetermined.

Tables:

1. **Main.** Opening coaches' share and role-holder race. Column 5 uses
   `HCIncumbentSpellId`, preserving incumbent tenure through temporary
   absences (including New Orleans 2012 and Indianapolis 2012).
2. **By staff group.** Coordinators, position coaches, assistants, front
   office, personnel and scouting; Blau indices.
3. **Unit stacked design** (A3). Stack offense and defense:
   - unit outcomes: offensive EPA/play and -defensive EPA/play, success
     rates;
   - treatment: the unit coordinator's race and the unit coaches' Black share;
   - FE: franchise x season and unit x season;
   - controls: unit roster quality and unit roster Black share.
4. **Head-coach hires.** Among `HCChange == 1` team-seasons, the change in
   outcomes on the race of the new head coach, controlling for `LagWinPct` and
   `LagExpectedWins`. Power is low.
5. **Timing sensitivity.** Table 19d substitutes season-union exposure. It
   is explicitly contemporaneous, since later hires and firings affect it.
6. **Permutation sensitivity.** Hire-race labels are permuted within season
   only under conditional exchangeability. There is no design-based
   randomization mechanism, and this permutation does not preserve
   franchise clustering.

Placebo: the one-season lead of staff composition. Standard errors are
clustered by franchise, with wild cluster bootstrap p-values for the key
coefficients.

### Threats

- **Firings follow bad seasons.** Coordinators are fired after bad seasons
  and replaced in bulk, so the regressions control for lagged outcomes and
  turnover, and test leads.
- **Staff coverage.** Staff boxes before 2007 are partial.
- **The glass cliff.** Lagged performance and expected wins describe the
  inherited situation but do not remove selection on owners' information.
  Hire-season market probabilities are measured after the hire, not before.

## 4. Coaching policies (`programs/16` and `17`)

### Institutional timing and samples

`data/reference/nfl_staff_policies.csv` records sources and timing.
`add_rooney_policies()` distinguishes opening-staff timing from the first
full offseason hiring cycle. Do not apply the March 2022 changes to earlier
2022 HC appointments.

The registry separates the 2003 HC interview rule, front-office expansion,
2020 interview and mobility changes, fellowships, compensatory draft-pick
incentives, 2021 in-person requirements, 2022 interview eligibility, the
2022-2024 offensive-assistant mandate and subsidy, and the voluntary
post-termination program from 2025. Ending the mandate does not end the
interview rules or compensatory-pick policy.

The coach panels contain opening roles, source-backed job-listing intervals,
verified appointment dates where available, policy eligibility,
participation, and one-/two-year transitions. NFL eligibility is a
documented woman or minority under league rules, not a score threshold.
Unknowns remain unknown. No public source acquired here identifies all
32 clubs' designated assistants or reimbursements. Published Accelerator
cohorts and individually sourced mandate participants are distinct.

`analysis_rooney_hires` extends the opening-HC panel to 1990 using archived
1989-1998 PFR records cross-checked against team-season sources. It separates
opening and in-season coaches, permanent versus interim appointments, and
expansion/re-entry censoring. A retained permanent in-season appointment is
not counted again the following year. A retained appointee with undocumented
permanent/interim status has unknown hire status, not an imputed offseason hire.

### Exhibits and estimands

- **Table 30:** source-selected TIDES series. Keep counts versus percentages,
  Black versus people-of-color categories, denominators and timing changes
  explicit; do not splice them into one homogeneous series.
- **Table 31:** documented-positive HC hires by policy era, hiring-cycle
  regressions, inherited performance/expectations, and a TIDES stock-count
  benchmark. Undocumented is not known white; any zero coding is an explicit
  ascertainment assumption.
- **Table 32:** hire-season performance on documented Black status,
  race-by-era interactions, freely estimated lagged-performance coefficients,
  and season/franchise controls. Wins-over-expected coverage starts later.
  Report prior-gap trends, placebo breaks, clustered intervals, baselines and
  approximate 80%-power MDEs. A change in the relative coaching-performance
  gap is not a change in aggregate league quality.
- **Table 33:** offense versus defense with franchise-by-unit and season FE,
  mandate and 2025 interactions, event-time coefficients, linear pretrends
  and joint leads. The regressions use the full observed panel; displayed
  baseline means use 2019-2021. Outcomes are opening counts, entries,
  eligibility bounds, next-opening retention and promotion. The
  Census-white-man upper-bound sensitivity adds an assumption not contained
  in the documented lower bound. Cohort-year transitions have later outcomes;
  the final cohorts are right-censored.

All clubs face the same dates; defense can have spillovers; interview,
mobility and incentive rules changed concurrently. Existing assistants could
satisfy the mandate. A rejected pretrend rules out a treatment-effect reading;
nonrejection is not identification. The one observed post-termination season
does not support a termination event study. Small HC-hire samples need
confidence intervals and MDEs, not a binary declaration that a rule worked.

## 5. Player employment (`programs/18` and `19`)

### Population and timing

The person-season panel preserves the full weekly-roster universe, including
players without pay records. Deduplicate franchise listings to person-weeks.
Separate active/inactive, reserve, suspended/exempt, practice-squad,
non-employed and ambiguous statuses. Game-week counts exclude byes.
Unverified 2016 preseason spillover is flagged rather than treated as
verified employment or exit. Roster weeks are not paid weeks; no wage rate
is constructed by dividing annual cap or cash totals by roster weeks.

Retention risk sets include under-contract players in seasons 2002-2024,
experience 0-15. The outcome is under-contract employment in the next NFL
season. Game-day, same-franchise, final-week and practice-squad-inclusive
definitions are separate; the latter starts in 2016. Incomplete future
seasons are censored.
The primary retention sample excludes t=2015 because the 2016 next-year
source has preseason spillover. Including that transition and additionally
excluding t=2016 are separate sensitivities. Ambiguous-only future evidence
remains unknown rather than establishing exit.

Contract access uses rookie cohorts 2011 onward and seasons 2013-2024 before
the first observed UFA/extension contract. It does not require an observed
salary. Missing historical coverage is not proof that no earlier contract
existed. Veteran-market and any-non-rookie definitions, retained-player
conditioning and experience 2-4 use their own risk sets. A 2026 signing
window is incomplete; signing years do not give exact dates.

### Models and interpretation

Tables 34-35 are linear-probability models with position-group-by-season FE,
prior covariates for predicted race, and progressively richer controls:
career stage, season-t and earlier production, pre-NFL signals, then usage
and employment. Cluster by player; report baselines, CIs, sample flow,
unknown-outcome exclusions and approximate MDEs. Table 34b changes race
measures. Logit score-plug-in AMEs are functional-form sensitivities, not
identified latent-race effects.

Table 34c adds employer-by-season FE to the Table 34 column 4 specification.
Employer is the last verified under-contract franchise during season t,
not necessarily the final-week employer and never a t+1 assignment.
Two-franchise under-contract listings in that final observed week leave the
employer unknown. Drop unknown employers and singleton employer-season cells
from both comparison columns, keeping the original full-sample model as a
separate column. Assert identical estimation rows. The outcome is retention
anywhere in the NFL, not retention by the same franchise. Report exclusions,
baselines, player-clustered CIs and MDEs. This is specification sensitivity
among selected employed players, not an employer treatment effect.

Employment and observed contract access are extensive-margin outcomes.
Conditional APY is a price conditional on reaching the bargained market;
annual cap and cash totals are accounting outcomes. The retention analysis
does not turn either into a wage rate. Survival to the risk set, voluntary
exit, injury, measurement error and unobserved quality remain selection
problems. Season-t statistics may overlap an early-January contract signing
in year t+1, so they are not certified pre-signing covariates.


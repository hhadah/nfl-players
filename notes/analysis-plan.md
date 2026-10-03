# Analysis plan: race, pay and team performance

Date: 2026-10-02. This file specifies the estimation samples and
specifications for three questions:

1. **Pay.** Conditional on every quality signal in the data, are Black NFL
   players paid more or less than white players at the same position?
2. **Roster diversity.** How do teams with more racially diverse rosters
   perform?
3. **Staff diversity.** How do teams with more racially diverse coaching
   staffs and front offices perform?

The code is in `programs/09`-`14` and runs from `programs/95-make-all.R`.

## Race measure (all three questions)

Revised 2026-10-02: the PI decided not to hand-code race. The **primary
measure is predicted race**, documented in `notes/race-prediction-design.md`.

**How it is built.** Each person's probability of being Black combines two
pieces:

- **Likelihood:** first name, surname and hometown county (BIFSG).
- **Prior:** an NFL-specific prior estimated by EM on predetermined
  characteristics, from `scripts/04e_predict_race.py` (table `race_predicted`).
  - Players: position group, era, draft round, college type.
  - Staff: first role, unit, era, former player.

Documented race statements are not inputs to the primary measure. They are
used for validation, and for the sensitivity measure `preddoc`.

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

- **Black:** P(Black alone or in combination).
- **OtherRace:** 1 - P(Black) - P(white).
- **Hand and provisional measures:** both are indicators instead.

The coefficient on Black is the Black-white gap under regression
calibration, if two conditions hold:

- names and hometown are unrelated to the outcome given race and the
  controls;
- the regression controls for the prior's covariates (`race_prior_controls()`).

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

### Quality blocks (all predetermined at signing)

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

- Main outcome: `LogAPYCapPct`, the log of APY as a share of the league
  salary cap.
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

- Outcome: `LogCapPct`; position-group x season FE.
- Black x experience-bin interactions, with and without lagged and career
  production.
- Pre-NFL signals interacted with experience.
- Sample: seasons on a non-rookie contract, plus a version with all seasons.
- Standard errors are clustered by player.

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
  - total cap share (sum of `CapPercent`);
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

Run the columns for the snap-weighted measure (2013-2025) and the headcount
measure (2002-2025). Standard errors are clustered by franchise (32 clusters),
with wild cluster bootstrap p-values (Webb weights, 9,999 replications) for
the diversity coefficient.

Placebo tests:

- the one-season lead of roster composition, conditional on current
  composition;
- the opponent's roster composition in the game-level design.

Within-season, game-level design. Outcome: `margin`. The regression controls
for the market expectation `team_spread_line` (positive = team favored; the
slope of margin on it is about 1.04). It includes franchise x season FE and
opponent x season FE. The treatment is the game-day active-roster Black share.
Identification comes from week-to-week roster changes (injuries, inactives,
signings) within a team-season, net of what the betting market prices. The ATS
residual `margin - team_spread_line` is an alternative outcome. Standard
errors are clustered by franchise.

### Threats

- **Positional composition.** Black shares differ sharply by position, which
  is why the regressions include expected shares and position-mix controls.
- **Talent.** Diverse rosters may simply be more talented. Roster-quality
  controls, LagWinPct and the market spread address this.
- **Reverse causality.** Winning teams may retain and acquire different
  players. The lead placebo and the within-season design address this.
- **Quarterback.** The QB's race enters separately.

## 3. Staff diversity and team performance

### Specifications (`programs/14-table-staff-diversity-performance.R`)

Equation (3) on `analysis_team_season`, with `FullStaffObserved` (2007-2025,
608 team-seasons):

    Y_ft = beta ShareBlackCoaches_ft + theta HCBlack_ft + X_ft pi + alpha_f + gamma_t + e_ft

Controls X:

- `LagWinPct` and `LagExpectedWins`;
- `ShareCoachesNewToFranchise` and `ShareCoachesPromoted`;
- roster quality and roster Black share (from 09).

Tables:

1. **Main.** Coaches' share Black, HC, OC, DC and GM race; FE as above; then
   franchise x HC-spell FE, so that identification comes from assistant
   turnover within a head coach's tenure.
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

Placebo: the one-season lead of staff composition. Standard errors are
clustered by franchise, with wild cluster bootstrap p-values for the key
coefficients.

### Threats

- **Firings follow bad seasons.** Coordinators are fired after bad seasons
  and replaced in bulk, so the regressions control for lagged outcomes and
  turnover, and test leads.
- **Staff coverage.** Staff boxes before 2007 are partial.
- **The glass cliff.** Minority coaches may be hired into worse situations,
  which biases naive comparisons downward. LagExpectedWins conditions on the
  situation they inherit.

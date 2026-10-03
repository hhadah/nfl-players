# Predicted race: design

Date: 2026-10-02. This replaces hand coding as the primary race measure; the
PI decided not to hand-code. The hand-coding protocol and sheets stay in the
repository. If hand codes are ever entered, the scripts switch to them
automatically (`choose_race_measure()`).

## Revision (2026-10-02, after the first fit): primary measure is model-only

The first fit gave documented persons P(Black) = 1 and estimated the prior
only on the undocumented. Two problems follow.

1. **Documentation depends on fame.** Wikipedia statements and categories
   exist mostly for famous people, so a documented Black player gets 1 while
   an equally Black, less famous player gets a model probability below 1. The
   error in P(Black) is then correlated with pay and team success. It biases
   the Black pay coefficient upward and makes measured roster diversity track
   talent.
2. **Two prior covariates are post-treatment.** Career length and having a
   Wikipedia article are partly determined after a contract is signed or a
   season is played. The same holds for a coach's most senior role ever held,
   which leaks future promotions.

**The primary measure (`pred`) is therefore model-only.** For every person,
documented or not:

- the likelihood comes from names and hometown;
- the prior is estimated by EM on the full population of players (or staff);
- the prior uses predetermined covariates only:
  - **players:** position group, rookie-season era, draft bucket, college
    type;
  - **staff:** role group at first appearance, unit, first-season era, former
    NFL player.

The documented race labels are used in two places only:

- validation, as the AUC on documented persons;
- the robustness variant `preddoc`, the documented one-hot plus a prior for
  the undocumented that conditions on fame proxies.

A side benefit: the primary measure does not depend on the language-model
text-classification step.

Regression calibration also requires the outcome regression to control for
the prior's covariates. Pay regressions under the predicted measure therefore
always include draft-bucket and college-type indicators along with the
position x year FE.

Sections 1-2 below describe the original fit, which is now the `preddoc`
variant.

## Revision 2 (2026-10-03, after adversarial review of the model)

**Changes to `scripts/04e_predict_race.py`:**

- **Linked staff-players.** Linked staff-player persons no longer inherit
  the staff posterior. Each player row keeps its own player-prior posterior,
  so player designs use the player prior.
- **Former player.** `former_player` is coded through a Wikidata link, which
  makes it a fame proxy, so it was dropped from the primary staff prior. It
  stays in `preddoc`.
- **Player prior covariates:**
  - The player prior now uses position at NFL entry (19 levels; K, P and LS
    pooled as ST) instead of the latest position group.
  - It adds `county_available`, because the county likelihood exists only for
    recruits with a hometown county.
- **Rare categories.** First-name likelihoods are neutralized for the
  AIAN and multiracial categories, whose Tzioumis cells contain one or two
  people.
- **HBCU list.** Matched on the nflverse college name, independent of CFBD
  coverage.
- **New variants:**
  - `p_*_pred_nodraft`: the prior without the draft bucket, for the draft
    table, where draft position is the outcome.
  - `p_black_or_multi_pred`: P(Black alone) + P(multiracial).
  - `p_*_pred_raked`: the TIDES-raked sensitivity. The raking windows end
    before 2017, when TIDES folded multiracial persons into one category, so
    the matched quantity is P(Black) + P(multi), corrected on 2026-10-03. For
    players the raking factor is then about 1.
- **Label correction.** `p_black_any_pred` is P(non-Hispanic Black
  **alone**). The name is kept for the R loaders.

**Validation after the revision:**

| Check | Players | Staff |
|---|---|---|
| AUC, documented Black vs white | 0.94 | 0.97 |
| Reliability | 0.63 | 0.48 |
| Reliability, head coaches | | 0.42 |

TIDES comparison:

- **Players:** 2010-2015 is -9 pp against "African-American", but +0.2 pp for
  P(Black or multi). 2019+ lies inside [Black alone, Black + two or more] in
  every season.
- **Head coaches:** -1 pp on average, with large season errors.
- **Assistant coaches:** -10 to -12 pp.

Players' logit calibration slope is 0.64: the model is overconfident in
relative odds.

**Remaining issues:**

- **Within-cell calibration.** The richest pay specification's pre-NFL
  controls predict P(Black) within prior cells. A `fullx` prior would fix
  this.
- **Role holders.** Probabilities are calibrated to the staff population at
  first appearance, not to current role.
- **BIRDiE.** The disagreement between BIRDiE and regression calibration in
  the pay table is unresolved.

## Principle

Race enters the analysis as a **probability**, never as a hard label. Each
person gets P(race = r | evidence), where the evidence is either:

- **documented:** a public source states the person's race or ethnicity; or
- **modelled:** names and hometown, combined with a prior estimated for NFL
  players or staff with the same observable characteristics.

Estimators are chosen to be consistent with probabilistic race (see
"Estimation" below). A hard label (argmax) is used only for descriptive
counts, and is flagged as such.

## 1. Documented race (`scripts/04d_race_documented.py`, then text extraction)

All sources are public. None of them infers race from a photograph.

- **Wikidata "ethnic group" (P172).** For staff items and for player items
  linked through P3561 (Pro Football Reference ID). Ethnic-group items are
  mapped to protocol categories with an explicit dictionary.
- **Wikipedia categories.** The existing `cat_*` flags, e.g. "African-American
  players of American football".
- **Wikipedia article text.**
  - Keyword search finds candidate sentences.
  - A language model then classifies each candidate: does the sentence state
    the **subject's own** race or ethnicity, as opposed to someone else's or a
    topic the subject was involved in?
  - The quote, revision id and classification are stored in
    `data/derived/race_text_labels.csv`. That file is tracked and produced
    once, so rebuilds do not call a model.
  - The current keyword evidence is noisy: Bill Walsh and Art Modell are
    flagged because their articles mention African-American coaches. This
    step removes those cases.

**Combining sources.** A person is documented Black if any source states
Black, alone or in combination. Statements of two races become multiracial.
Contradictory sources are flagged, and the person falls back to the model.

**Caveat.** Documentation is positive-only and depends on fame, so the absence
of a statement is never evidence of race.

## 2. Modelled race (`scripts/04e_predict_race.py`)

**Name and hometown likelihood.** L_i(r) is the existing BIFSG posterior
(`race_bifsg`, or `race_bifsg_geo` when a hometown county is known) divided by
the Census marginal P_C(r) embedded in P(r | surname):

    L_i(r) ∝ P(surname | r) · P(first name | r) · P(county | r)

**Prior, estimated by EM.**

    pi(r | X_i) = softmax(X_i gamma)

This is a multinomial logit with main effects. It is estimated only on
persons **without documented race**, by EM:

- E-step: posterior_i(r) ∝ pi(r | X_i) L_i(r).
- M-step: a weighted multinomial logit of the posteriors on X.

Covariates X:

- **Players:** position group; rookie-season era (≤2005, 2006-10, 2011-15,
  2016-20, 2021-25); draft bucket (rounds 1-2, 3-4, 5-7, undrafted); has a
  Wikipedia article; career-length bucket (fame proxy); college type (Power
  conference, other FBS, FCS or lower, HBCU, unknown).
- **Staff:** role group (head coach, coordinator, position coach,
  assistant/quality control, strength and support, GM/personnel/scouting,
  owner/executive); unit; first-season era; former NFL player; has a
  Wikipedia article.

Conditioning the prior on article existence and fame proxies matters.
Documented persons are removed, so the undocumented pool is negatively
selected on Black share. That selection depends on fame, which is correlated
with pay and team quality. Without these covariates, measured team diversity
would track talent mechanically.

**Posterior.**

    p_i(r) = documented one-hot, if documented
           = pi(r | X_i) L_i(r) / sum_r' pi(r' | X_i) L_i(r'),  otherwise

Categories follow the Census surname file: white, Black, Hispanic (of any
race), API, AIAN and multi (non-Hispanic). `p_black_pred` is P(Black). A
documented multiracial person with a Black component gets
`p_black_pred = 1` (Black alone or in combination).

**Optional TIDES raking (sensitivity only).** Rescale the season-level priors
so that the mean player P(Black) matches the published TIDES league share.
The main measure is not raked, so that TIDES remains an independent
validation.

## 3. Validation (`programs/15-table-race-prediction-validation.R`), no hand codes

1. **Aggregate calibration.** By season, compare the predicted Black share of
   game-day players, head coaches and assistant coaches with TIDES published
   shares (`data/reference/tides_nfl_race_shares.csv`).
2. **Discrimination on documented persons.** Use the model-only posterior,
   computed as if undocumented, for documented Black persons versus documented
   persons of other races. Report the AUC and the mean P(Black). These persons
   are famous, so this is indicative only.
3. **Informativeness.** Report the distribution of `p_black_pred`, the
   entropy, and the share of persons whose posterior moves away from their
   prior by more than 0.1.

## 4. Estimation with probabilistic race

**Person-level gaps (pay, draft).** Regression calibration: regress Y on
P(Black), P(other race) and the controls X. This is consistent for the
Black-white gap if:

- E[Y | R, X] is linear without race-by-X interactions, and Y is independent
  of name and hometown given (R, X); and
- p is calibrated given the information that the controls carry.

The prior conditions on the main covariates (position, era, draft, fame)
because of the second condition. Standard errors are clustered by player.
Power depends on Var(p | X). With weak name signals the estimates are
imprecise, and the tables say so.

**Cross-check.** BIRDiE (`birdie` package; McCartan, Fisher, Goldin, Ho and
Imai 2025, JASA), a Normal linear model with race-specific coefficients on a
parsimonious X. It reports:

- the marginal E[Y | R]; and
- the conditional gap, averaged over persons: X_i'(beta_Black - beta_white).

**Team shares (roster, staff).** `ShareBlackPred<G>` is the weighted mean of
`p_black_pred`, i.e. the expected Black share. `HCBlackPred` is the head
coach's P(Black). Team regressions use these expected values, which is
regression calibration at the team level.

**Measure switch.** `choose_race_measure()`:

- returns `hand` if hand codes cover at least 80% of the sample, else
  `predicted`;
- keeps `provisional` (the Wikipedia flag) as a robustness option.

The primary measure's exhibits keep the base file names and go to
`my_paper/tables`. Exhibits from a non-primary measure get a `-<measure>`
suffix and go to `output/` only.

## Not used

- **Face or photo classifiers.** They infer race biometrically, which this
  project rules out.
- **Hard BIFSG labels as a regressor.** The misclassification is
  non-classical.
- **Position as a predictor in team-diversity designs, without care.**
  Position is in the prior, so the expected share reflects position mix as
  true composition does. The "net of position mix" measures isolate
  within-position variation, which comes only from documentation and names.

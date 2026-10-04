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
   error in P(Black) can then correlate with pay and team success. This can
   confound the pay and composition comparisons; its direction is not
   established by the documentation mechanism alone.
2. **Two prior covariates are post-treatment.** Career length and having a
   Wikipedia article are partly determined after a contract is signed or a
   season is played. The same holds for a coach's most senior role ever held,
   which leaks future promotions.

**The primary measure (`pred`) is therefore model-only.** For every person,
documented or not:

- the likelihood comes from names and hometown;
- the prior is estimated by EM on the full population of players (or staff);
- the prior uses predetermined covariates only:
  - **players:** position at NFL entry, rookie-season era, draft bucket,
    college type and county availability;
  - **staff:** role group at first appearance, unit and first-season era.

Documented labels are not inputs to the primary model. They support
validation, the fame-dependent `preddoc` sensitivity, and explicitly
documented-positive coaching-policy comparisons. The latter retain unknown
eligibility and distinguish a documented-positive flag from observed race.

A side benefit: the primary measure does not depend on the language-model
text-classification step.

Pay regressions include the prior's covariates, but this is not sufficient
for regression calibration. The probability must be calibrated given all
the outcome regression's controls. The outcome model and exclusion
assumptions below are also required.

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

## Revision 3 (2026-10-03): event alignment and estimand comparisons

**The model-only target is non-Hispanic Black alone.** The legacy column
`p_black_any_pred` does not represent Black alone-or-in-combination. Hand
codes and documented Black-any labels target the broader event. `preddoc`
mixes the broad documented event with the narrower model-only event when
documentation is absent; it is a sensitivity, not an interchangeable
measure of the same estimand.

**Validation now matches the event.** `DocAloneY` is one for documented
non-Hispanic Black alone, zero for documented non-target groups (including
Hispanic and multiracial Black), and missing for undocumented or conflicting
labels. Absence of a stated Hispanic or additional racial component is
treated as absence of that component, a documentation assumption, not
self-identification. Broad Black-any AUCs are separate statistics. The old
player calibration slope used a different event and has been withdrawn.
Current estimates, standard errors and sample sizes are generated in
[results-memo.md](results-memo.md) and Table 25.

**Overlap is not calibration.** Regressing the race score on richer controls
does not test whether race is calibrated given those controls. The prior
`fullx` recommendation was therefore not justified. Table 8 instead reports
the residual SD of the score and a documented-label check: logit race on
logit score and prior fixed effects, then add one index trained to predict
the score from the controls. No pay outcome trains this index. This checks
one additional restriction in a selected documented sample, after allowing
recalibration; it is not a validation of the population's full conditional
race distribution.

**BIRDiE disagreement is decomposed, not declared repaired.** Table 26 uses
the same sample and control matrix to separate:

1. posterior versus prior averaging weights;
2. Normal-mixture EM versus interacted-OLS slopes at the same contrast;
3. a race-specific contrast versus a common-slope coefficient.

The interacted conditional-mean design is rank-deficient in sparse cells.
Table 26 keeps all observations and controls in every fit but averages the
matched EM/OLS contrasts only where a scaled-QR null-space check establishes
estimability. It reports the retained observation count and prior Black
probability mass. The unrestricted EM average is separate: it includes
contrasts not identified by that design. Bootstrap draws recompute support;
their SEs describe this support-adaptive diagnostic, not the unrestricted gap.

It also reports one-contract-per-player estimates and within-player
posterior variation. A contract-level mixture draws a separate latent race
for each contract; player-clustered standard errors do not repair that
person-level likelihood mismatch. Outcome-informed posterior weights are
not observed race.

**Role-holder calibration remains unestablished.** A prior fitted using
first-appearance roles does not establish calibration for subsequent
head-coach appointments or promotions. TIDES categories and observation
dates also differ from the model's event and opening-snapshot populations.


## Principle

Modelled race enters as a **probability**, never as a thresholded treatment.
Documented-positive flags are separate measures, not replacements for
unknown labels. The evidence consists of:

- **documented:** a public source states the person's race or ethnicity; or
- **modelled:** names and hometown, combined with a prior estimated for NFL
  players or staff with the same observable characteristics.

Estimators state the calibration and outcome-model assumptions they need.
Any argmax counts are descriptive and labeled. The coaching-policy analyses
use explicit documented eligibility and ascertainment assumptions instead
of classifying people by a predicted-Black cutoff.

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
2. **Documented-person discrimination and calibration.** Use model-only
   probabilities, never `preddoc` one-hot labels, as predictions. Report
   broad Black-any AUCs separately from model-event AUCs, logit calibration
   slopes, SEs and counts. These are selected-documentation checks.
3. **Informativeness.** Report score distributions, entropy, prior-to-posterior
   movement and residual score SD within each outcome specification.
4. **Positive-only head-coach comparisons.** Undocumented coaches are not
   observed non-Black people. Bins against documented positives are labeled
   ascertainment lower bounds, not calibration.

## 4. Estimation with probabilistic race

**Person-level gaps (pay, draft, retention and contract access).** Regress Y
on P(non-Hispanic Black alone), P(other) and controls X. The coefficient is a
latent Black-white gap only if:

- E[Y | R, X] is linear without race-by-X interactions, and Y is independent
  of name and hometown given (R, X); and
- p is calibrated given the information that the controls carry.

The primary prior uses entry position, era, draft, college type and county
availability, not fame. Prior controls alone do not ensure the second
condition. Standard errors cluster by player and hold predictions fixed.
Precision depends on Var(p | X); low residual variation is an overlap
problem, not proof of miscalibration.

**Cross-check.** BIRDiE (`birdie`; McCartan, Fisher, Goldin, Ho and Imai 2025,
JASA) fits a Normal linear model with race-specific slopes on a parsimonious
X. Table 26 reports marginal and conditional contrasts, prior/posterior
weight changes, same-X regression-calibration comparisons, and the
one-contract-per-player check described above. It does not identify the
same object as a common-slope coefficient merely because both are called a
Black-white gap.

**Team shares (roster, staff).** The weighted mean of member probabilities
is a model-implied share. A regression-calibration interpretation additionally
requires calibration given the team controls, selection and weights.
Same-season snap weights are endogenous. The Blau index computed from a
model-implied share is not a posterior expectation of the true Blau index.

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

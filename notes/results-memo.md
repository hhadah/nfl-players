# Results memo: race, pay and team performance

Date: 2026-10-03. Every number below comes from `Rscript programs/95-make-all.R`
(run on 2026-10-03). The exhibits are in `my_paper/tables`, and tidy
coefficients in `output/estimates`. Race is **predicted**, not observed
(`notes/race-prediction-design.md`). Read every estimate as conditional on
that measure.

## The race measure

- **Construction.**
  - Each person's probability of being non-Hispanic Black combines first
    name, surname and hometown county (BIFSG) with an NFL-specific prior.
  - The prior is estimated by EM on predetermined characteristics:
    - players: position at entry, rookie era, draft round, college type,
      county availability;
    - staff: role, unit and era at first appearance.
  - Documented race statements (Wikidata, Wikipedia categories and article
    text) are kept out of the measure. Documentation depends on fame, which
    is correlated with pay and team success.
- **Validation** (Tables 24-25):

  | Check | Result |
  |---|---|
  | AUC, documented Black vs white | players 0.94, staff 0.97 |
  | AUC, documented Black vs non-Black | players 0.89, staff 0.91 |
  | Players vs TIDES, 2019-2023 | inside [Black alone, Black + two or more races] every season |
  | Players vs TIDES, 2010-2015 | P(Black or multiracial) within 0.2 pp of "African-American" |
  | Head coaches vs TIDES | about 1 pp low on average; large season errors |
  | Assistant coaches vs TIDES | 10-12 pp low |
  | Reliability | players 0.63, staff 0.48, head coaches 0.42 |
  | Players' logit calibration slope | 0.64 (overconfident in relative odds) |

- **Estimation.** I use regression calibration: outcomes are regressed on
  P(Black) and P(other race), with fixed effects for the prior's covariates.
  The coefficient on P(Black) is the Black-white gap if two conditions hold:
  - names and hometown are unrelated to the outcome given race and the
    controls;
  - the probabilities are calibrated given the controls.

  The pay script's diagnostic shows the second condition is not fully met for
  the richest specification. The pre-NFL controls still predict P(Black)
  within prior cells (F = 3.4). Team regressions use expected shares, which
  carry Berkson-type error and therefore lose precision.

## 1. Do Black players earn less than white players at the same position?

**Main sample.** 5,062 freely bargained veteran contracts (UFA and
extensions) signed 2014-2026, for 2,789 players. Outcome: log APY.
Equation (1) includes OTC position x year-signed FE, and standard errors are
clustered at the player level (Table 8).

**Finding: no detectable gap.** The coefficient on P(Black) by column:

| Column | Controls added | P(Black) |
|---|---|---|
| (1) | position-year FE only, within the race-prior cells | -0.023 (0.070) |
| (2) | + career stage | -0.015 (0.058) |
| (3) | + prior-season production, position-specific | -0.026 (0.049) |
| (4) | + career production | -0.035 (0.049) |
| (5) | + pre-NFL signals: draft slot, 247 rating, combine, RAS-style athletic score, college quality (main specification) | -0.004 (0.051) |
| (6) | + usage: snaps, starts | -0.039 (0.045) |
| (7) | + signing-team x year FE | -0.039 (0.046) |

- Column (5) has a 95% CI of [-0.10, 0.10]. The data rule out Black-white
  gaps larger than about 10% in either direction. They cannot rule out small
  ones.
- **Draft-free benchmark.** A raw gap without conditioning on draft round
  is +0.098 (0.073). The Gelbach decomposition (Table 9) attributes that
  benchmark gap mainly to career stage (0.051) and prior-season production
  (0.038). Conditional on these, Black and white veterans are paid alike.
- **By position** (Table 11): no group gap is significant. Wild cluster
  bootstrap p-values are 0.32 or higher.
- **Other contract terms** (Table 10): guarantee share, length, and the UFA,
  re-sign and new-team margins show no gaps.
- **Robustness to the race measure** (Table 27). Column (5) is between -0.017
  and 0.008 under the documented, raked, draft-free and Black-or-multiracial
  variants.
  - The old Wikipedia flag gives a raw "Black premium" of +0.153***. That is
    the fame bias of a positive-only flag.
- **BIRDiE cross-check** (Table 26):
  - The marginal Black-white difference in log APY is +0.11 (0.05). This
    is a composition effect: Black players are concentrated at higher-paid
    positions.
  - BIRDiE's conditional gap is -0.16 (0.10), against -0.05 (0.05) from
    regression calibration with the same X. The two estimators disagree, and
    this is unresolved (`notes/race-prediction-design.md`).
- **Bottom of the market** (Table 12, player-season panel, log cap number).
  - Across all non-rookie seasons, Black players' cap numbers are 12.5%
    lower (-0.133, SE 0.043).
  - The gap is concentrated in street-free-agent (-0.100) and practice-squad
    (-0.149) seasons. In UFA and extension seasons it is -0.032 (0.043).
  - It shrinks to -0.085 (0.045) in seasons with games played.
  - Cap numbers in these deals partly reflect weeks on the roster. This is a
    retention or roster-churn margin (B6 in `EXTENDING.md`) as much as a
    wage margin, and it needs follow-up.
- **Draft position** (Table 13, draft-free race prediction, class x position
  FE, clustered by college).
  - Black prospects go earlier unconditionally: log pick -0.29 (0.09).
  - Conditional on combine, recruit and college signals the gap is -0.13
    (0.08). Adding the pre-draft grade gives 0.02 (0.05).
  - The "drafted" column is selected on later NFL employment.

## 2. How do teams with more diverse rosters perform?

**Specification.** Equation (2): team-season, franchise and season FE, with
lagged outcomes, roster quality, opening-day staff and QB race, and the team
mean of the race prior (the calibration control). Standard errors are
clustered by franchise (32 clusters), with wild cluster bootstrap p-values.

- **Association.** A higher snap-weighted Black share goes with fewer
  wins: -0.59 (0.24), WCB p = 0.026, 2013-2025 (Table 15). A one-SD change in
  the expected share corresponds to about -2.6 win-percentage points. Under
  headcount weights over 2002-2025 the estimate is -0.42 (WCB p = 0.10). The
  three-group Blau index enters positively, 0.53 (p < 0.1).
- **Not causal.** The placebo test fails (Table 18).
  - Next season's roster share predicts this season's win percentage
    strongly: -0.63, WCB p = 0.013. Conditional on it, the current share is
    -0.17 (p = 0.57).
  - Past win percentage predicts the current share (WCB p = 0.035).
  - These patterns fit roster turnover after losing seasons (who plays and
    who is signed), not an effect of diversity.
- **Game level** (Table 17): within team-season, against the betting
  spread, there is no effect. ATS margin -2.3 (8.4), WCB p = 0.79.

## 3. How do teams with more diverse staffs perform?

**Specification.** Equation (3), full staff boxes 2007-2025 (608
team-seasons): franchise and season FE, predetermined controls, and the
staff prior. Standard errors are clustered by franchise, with WCB p-values.

- **Coaches.** The coaches' expected Black share goes with lower win
  percentage: -0.32 (0.16), WCB p = 0.067 (Table 19, column (4)).
  - Within head-coach spells: -0.21 (0.20), p = 0.30.
  - The estimate does not survive conditioning on the documented-race
    share.
  - By group (Table 20), assistants are -0.17 (WCB p = 0.08). Coordinators,
    position coaches, front office and personnel are all near zero.
- **Offense vs defense** (Table 21): within team-season, the unit
  coordinator's race and the unit coaches' share have no effect.
- **Head-coach hires** (Table 22).
  - With a proper LagWinPct control, a Black hire's P(Black) on win
    percentage is -0.10 (0.07), WCB p = 0.16.
  - The earlier "Black hires do worse" result came from a DeltaWinPct
    specification that forces the coefficient on lagged wins to one. The data
    reject that restriction (p < 0.001).
- **Selection** (Table 23): past win percentage predicts the current
  coaches' share (-0.43, WCB p = 0.03). Losing teams hire relatively more
  Black coaches, or Black coaches are hired into worse situations (the glass
  cliff). The negative cross-sectional association should not be read as a
  causal effect of staff diversity.
- **Measurement.** Coaches' predicted race is noisy. Black coaches with
  common surnames get low probabilities, so coach-level estimates are
  imprecise.

## What would sharpen these answers

1. **Hand-check a validation sample.** Coding 200-300 people by hand, split
   between coaches and players, would measure the misclassification directly
   and allow a misclassification-corrected estimator. That matters most for
   coaches.
2. **Fix within-cell calibration.** A prior that also conditions on the
   pre-NFL controls (`fullx`) would address the calibration violation in the
   richest pay specification.
3. **Study the minimum-salary margin directly.** Weeks on the roster, cuts
   and retention for street-free-agent and practice-squad players
   (`EXTENDING.md`, B6).
4. **Build the team designs' missing pieces.** An HC spell and hire file with
   preseason win totals (for the glass cliff), and opening-day composition as
   the treatment, which is already reported in Table 18b.
5. **Real RAS data.** Request an export from its creator. The RAS-style score
   used here has no pro-day results.

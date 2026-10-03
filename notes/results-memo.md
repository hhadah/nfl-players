# Results memo: race, pay, employment and coaching policies

Generated 2026-10-03 by `programs/20-write-results-memo.R` after the pipeline's temporal checks.
The numbers come from `output/estimates/`. Tables are in `my_paper/tables/`. Race scores and the measured controls are held fixed for inference. These are conditional associations, not estimates of discrimination or of a league-wide policy's causal effect.

## What changed

Primary staff models now use opening-snapshot roles, composition, priors and turnover. The cutoff is midnight UTC on each team's first regular-season game date. Season-long staff unions appear only as labeled sensitivity analyses. Incumbent-coach fixed effects preserve tenure across temporary absences. A vandalized opening template uses an earlier parsable revision, with its timestamp and fallback flag retained.

Current annual cap totals are not predetermined controls. The snap-weighted model uses last season's cap total and starts in 2014. Same-season snap weights remain descriptive. Opening-roster specifications are reported separately.

The old test of whether controls predict the race score did not test calibration. It has been removed. A richer prior is not established as a fix. The replacement reports score overlap and a documented-label check in one control direction.

## Race measurement and calibration

The model-only prediction event is **non-Hispanic Black alone**, despite the legacy column name `p_black_any_pred`. Hand/documented Black-any measures use the broader alone-or-in-combination event; the coefficient tables record their measure. Documented Hispanic and multiracial Black people are non-target observations for the model-event validation. Ambiguous or conflicting documentation remains unknown. Absence of a Hispanic or second-race statement is treated as absence of that component, an ascertainment assumption rather than self-identification. Broad Black-any AUCs remain separate discrimination statistics.

Population | Logit calibration slope | SE | N
--- | --- | --- | ---
players | 0.576 | 0.028 | 2268
staff | 0.827 | 0.107 |  391
head_coaches | 1.490 | 0.690 |   28

These are selected-documentation diagnostics, not population validation. The prior memo's player slope used a Black-any label against a Black-alone probability. Head-coach deciles that count undocumented people as zero are documented-positive lower bounds, not observed-race calibration.

Model | Residual score SD | N
--- | --- | ---
(1) | 0.275 | 5062
(5) | 0.243 | 5062

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
slope | LogitP08 | 0.647 | 0.064 | [0.522, 0.772] | 0.000 | 1409 | clustered | DocAlone08 | predicted
index | LogitP08 | 0.626 | 0.068 | [0.492, 0.759] | 0.000 | 1409 | clustered | DocAlone08 | predicted
index | RaceIndex08 | 0.733 | 1.108 | [-1.439, 2.905] | 0.508 | 1409 | clustered | DocAlone08 | predicted

The extra index uses observed controls and the race score, never salary outcomes. This is one restriction on a fame-selected documented sample. Rejection does not establish population miscalibration; nonrejection does not validate calibration given every control.

## Veteran contracts and realized annual pay

Table 8's main estimate is -0.004 log points, SE 0.051. The model does not distinguish the conditional gap from zero. Its 95 percent interval permits -9.8 to 10.1 percent relative to equal pay, conditional on the race-calibration and outcome-model assumptions.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | Black | -0.023 | 0.070 | [-0.161, 0.115] | 0.745 | 5062 | clustered | LogAPY | predicted
(2) | Black | -0.015 | 0.058 | [-0.128, 0.098] | 0.793 | 5062 | clustered | LogAPY | predicted
(3) | Black | -0.026 | 0.049 | [-0.121, 0.070] | 0.596 | 5062 | clustered | LogAPY | predicted
(4) | Black | -0.035 | 0.049 | [-0.131, 0.061] | 0.472 | 5062 | clustered | LogAPY | predicted
(5) | Black | -0.004 | 0.051 | [-0.104, 0.096] | 0.944 | 5062 | clustered | LogAPY | predicted
(6) | Black | -0.039 | 0.045 | [-0.127, 0.050] | 0.391 | 5062 | clustered | LogAPY | predicted
(7) | Black | -0.039 | 0.046 | [-0.129, 0.051] | 0.391 | 5062 | clustered | LogAPY | predicted
(8) | Black | -0.001 | 0.055 | [-0.110, 0.108] | 0.985 | 5062 | clustered | LogAPY | predicted
(9) | Black | 0.098 | 0.073 | [-0.045, 0.240] | 0.181 | 5062 | clustered | LogAPY | predicted

The sample contains freely bargained UFA and extension contracts. It conditions on reaching that market. Annual cap charges in Table 12 are accounting totals, not wage rates; a cap gap cannot by itself separate employment duration from prices paid for labor.

### Annual White-minus-Black pay gaps

The raw and adjusted figures use identical freely bargained veteran contracts and identical race scores. Positive values mean higher White APY. Values are 100 times exp(White-minus-Black log APY) minus 100, relative to Black geometric pay, not differences in arithmetic mean pay. Intervals are pointwise 95 percent intervals with player-clustered SEs; the race scores are held fixed.

Year | Raw gap, percent | Raw 95% CI | Adjusted gap, percent | Adjusted 95% CI | Contracts | Window
--- | --- | --- | --- | --- | --- | ---
2014 | -19.2 | [-39.1, 7.2] | -2.9 | [-30.8, 36.0] | 234 | Completed
2015 | -13.4 | [-36.3, 17.7] | -0.8 | [-28.8, 38.1] | 272 | Completed
2016 | 16.5 | [-13.2, 56.4] | -1.3 | [-27.7, 34.7] | 336 | Completed
2017 | -25.3 | [-42.0, -3.8] | 11.3 | [-19.7, 54.4] | 352 | Completed
2018 | -6.1 | [-28.8, 23.8] | 39.8 | [5.1, 86.0] | 350 | Completed
2019 | -28.5 | [-46.4, -4.7] | -14.4 | [-34.1, 11.3] | 371 | Completed
2020 | -8.5 | [-32.6, 24.1] | 24.6 | [-4.3, 62.4] | 366 | Completed
2021 | -14.4 | [-34.0, 11.0] | 3.2 | [-20.5, 33.9] | 408 | Completed
2022 | -14.1 | [-32.9, 10.0] | 7.7 | [-17.7, 40.8] | 437 | Completed
2023 | -20.0 | [-36.7, 1.0] | -5.5 | [-22.6, 15.5] | 458 | Completed
2024 | -27.1 | [-43.2, -6.6] | -24.3 | [-39.9, -4.7] | 501 | Completed
2025 | -12.6 | [-31.2, 11.1] | 14.9 | [-7.7, 43.1] | 521 | Completed
2026 | -34.4 | [-48.9, -15.8] | -20.4 | [-37.0, 0.6] | 454 | Partial

Among the 12 completed signing years, 10 adjusted 95 percent intervals include zero. These are pointwise intervals, not a joint confidence band or a test of a time trend.

The raw regression includes year-specific Black and other-race scores and signing-year fixed effects, with no position, quality or prior-covariate controls. The adjusted regression adds position-by-signing-year fixed effects and Table 8 column 5 controls, including the prior covariates. Control slopes are pooled across years; racial gaps vary by year. Position-year singleton cells are removed from both models. Both series are model-implied under predicted race, not observed-race group means; changing controls also changes calibration requirements. The 2026 signing window is incomplete. These are descriptive gaps among players reaching the veteran market, not causal effects.

[Raw annual figure](../output/figures/figure-pay-gap-by-year-raw.png) | [Adjusted annual figure](../output/figures/figure-pay-gap-by-year-adjusted.png)

### Within-player career pay profiles

Table 12b compares position-by-season fixed effects with player plus position-by-season fixed effects on a common veteran-pay sample. Black and other-race experience interactions use the same reference category. The reported Black interactions describe changes in the Black-white gap relative to that category, not the gap's level. Player fixed effects absorb the time-invariant race score. Differences between the profiles are specification sensitivity, not a decomposition identifying survivor selection. Career stage also changes with calendar time; the profiles do not isolate employer learning.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | BlackExp_0_3 | 0.387 | 0.167 | [0.060, 0.714] | 0.020 | 5284 | clustered | LogCapNumber | predicted
(1) | BlackExp_7plus | -0.138 | 0.072 | [-0.279, 0.004] | 0.056 | 5284 | clustered | LogCapNumber | predicted
(2) | BlackExp_0_3 | -0.120 | 0.179 | [-0.471, 0.232] | 0.505 | 5284 | clustered | LogCapNumber | predicted
(2) | BlackExp_7plus | -0.020 | 0.067 | [-0.152, 0.112] | 0.770 | 5284 | clustered | LogCapNumber | predicted
(3) | BlackExp_0_3 | 0.275 | 0.123 | [0.034, 0.516] | 0.026 | 5284 | clustered | LogCapNumber | predicted
(3) | BlackExp_7plus | -0.135 | 0.067 | [-0.266, -0.003] | 0.045 | 5284 | clustered | LogCapNumber | predicted
(4) | BlackExp_0_3 | -0.056 | 0.167 | [-0.383, 0.272] | 0.739 | 5284 | clustered | LogCapNumber | predicted
(4) | BlackExp_7plus | -0.022 | 0.065 | [-0.150, 0.105] | 0.731 | 5284 | clustered | LogCapNumber | predicted
(5) | BlackExp_0_3 | 0.247 | 0.162 | [-0.071, 0.565] | 0.127 | 5284 | clustered | LogCapNumber | predicted
(5) | BlackExp_7plus | -0.143 | 0.073 | [-0.286, 0.000] | 0.050 | 5284 | clustered | LogCapNumber | predicted
(6) | BlackExp_0_3 | -0.127 | 0.182 | [-0.485, 0.230] | 0.485 | 5284 | clustered | LogCapNumber | predicted
(6) | BlackExp_7plus | -0.016 | 0.068 | [-0.150, 0.117] | 0.813 | 5284 | clustered | LogCapNumber | predicted
(7) | BlackExp_0_3 | 0.152 | 0.134 | [-0.112, 0.415] | 0.259 | 5284 | clustered | LogCapNumber | predicted
(7) | BlackExp_7plus | -0.124 | 0.069 | [-0.260, 0.011] | 0.072 | 5284 | clustered | LogCapNumber | predicted
(8) | BlackExp_0_3 | -0.042 | 0.170 | [-0.375, 0.291] | 0.805 | 5284 | clustered | LogCapNumber | predicted
(8) | BlackExp_7plus | -0.017 | 0.066 | [-0.146, 0.113] | 0.799 | 5284 | clustered | LogCapNumber | predicted

In the fully controlled pair, columns (7)-(8), the Black-by-7+ experience coefficient is -0.124 log points without player FE and -0.017 with player FE (95 percent CI [-0.146, 0.113]), relative to 4-6 seasons. This is a change in the career profile, not a pay-gap level.

### Annual pay within franchise and season

Table 12c holds the sample and quality controls fixed while adding paying-franchise-by-season fixed effects. The comparison concerns conditional Black-white gaps in annual accounting pay, not an employer treatment effect or a wage rate.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | Black | 0.002 | 0.056 | [-0.108, 0.112] | 0.974 | 5949 | clustered | LogCapNumber | predicted
(2) | Black | 0.007 | 0.056 | [-0.104, 0.117] | 0.905 | 5949 | clustered | LogCapNumber | predicted
(3) | Black | -0.014 | 0.057 | [-0.126, 0.098] | 0.811 | 5948 | clustered | LogCashPaid | predicted
(4) | Black | 0.012 | 0.058 | [-0.102, 0.126] | 0.837 | 5948 | clustered | LogCashPaid | predicted

For log cap number, the Black-minus-White coefficient is 0.002 without employer FE and 0.007 with employer FE (95 percent CI [-0.104, 0.117]).

### Successive observed veteran contracts

Table 12d compares log APY across newly signed contracts, with player and position-by-signing-year fixed effects. Race-by-contract-stage coefficients describe differential pay evolution relative to the first observed eligible deal; static race levels are absorbed. The sample selects players reaching repeated deals. Observed order is not complete career order, and tied signing years do not supply a defensible within-year sequence. Prior-year controls can overlap early-January signings because exact signing dates are unavailable.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | BlackOrd_2 | 0.022 | 0.070 | [-0.116, 0.159] | 0.756 | 3491 | clustered | LogAPY | predicted
(1) | BlackOrd_3plus | 0.063 | 0.118 | [-0.169, 0.295] | 0.593 | 3491 | clustered | LogAPY | predicted
(2) | BlackOrd_2 | 0.044 | 0.069 | [-0.093, 0.180] | 0.531 | 3491 | clustered | LogAPY | predicted
(2) | BlackOrd_3plus | 0.078 | 0.113 | [-0.143, 0.299] | 0.490 | 3491 | clustered | LogAPY | predicted
(3) | BlackOrd_2 | 0.028 | 0.059 | [-0.089, 0.144] | 0.642 | 3491 | clustered | LogAPY | predicted
(3) | BlackOrd_3plus | 0.006 | 0.094 | [-0.178, 0.191] | 0.946 | 3491 | clustered | LogAPY | predicted
(4) | BlackChange | 0.030 | 0.066 | [-0.099, 0.160] | 0.644 | 2157 | clustered | DeltaLogAPY | predicted
(5) | BlackChange | -0.003 | 0.056 | [-0.112, 0.106] | 0.955 | 2157 | clustered | DeltaLogAPY | predicted

With prior-production controls, the Black-minus-White difference in adjacent-contract log-APY changes is -0.003 (95 percent CI [-0.112, 0.106]). The interval does not establish equal pay progression; the comparison includes only players receiving another eligible deal.

### BIRDiE comparison

Table 26 separates posterior versus prior weights, EM versus interacted-OLS slopes at the same estimable contrasts, and a supported race-specific average versus the full-sample common-slope coefficient. All observations and covariates remain in the fitted models. The unrestricted EM average is retained separately because it includes contrasts not identified by the interacted design. The bootstrap recomputes support, so its intervals describe a support-adaptive diagnostic, not the unrestricted heterogeneous gap. The one-contract-per-player comparison addresses a separate limitation: the contract-level mixture allows a person's latent race to change across contracts, which player-clustered uncertainty does not repair.

The full-sample interacted design supports 5055 of 5062 contract-level Black–white contrasts, accounting for 99.918 percent of prior Black probability mass. Rank-deficient coefficients are not interpreted as zero effects.

Contrast | Log-point estimate | SE | SE method
--- | --- | --- | ---
cond_gap_full | -0.162 | 0.096 | bootstrap
cond_gap | -0.162 | 0.096 | bootstrap
cond_gap_prior | -0.159 | 0.096 | bootstrap
rc_int_prior | -0.158 | 2.237 | bootstrap
rc_same_x | -0.048 | 0.048 | cluster
diff_weights | -0.002 | 0.002 | bootstrap
diff_slopes | -0.001 | 2.240 | bootstrap
diff_contrast | -0.110 | 2.239 | bootstrap
diff_total | -0.113 | 0.092 | bootstrap
cond_gap_first | -0.132 | 0.118 | bootstrap
rc_int_prior_first | -0.122 | 5.508 | bootstrap
rc_same_x_first | -0.055 | 0.061 | bootstrap

Near-equal point estimates do not establish a precise reconciliation. The interacted OLS average has a player-bootstrap SE of 2.237 log points, and 5.508 in the one-contract-per-player sample. This comparison is unstable under resampling; its small point difference from EM is not evidence of equivalence.

Any remaining model disagreement at the same contrast is a substantive limitation. The decomposition explains which choices move the answer; it does not force agreement.

## Roster and staff composition

The following estimates are win-percentage-point associations per 10-percentage-point increase in Black share under each table's recorded measure. The league team-season baseline win percentage is 50.0 percent. Reported intervals use wild-cluster bootstrap inversion where available; SEs are franchise-clustered.

### Rosters

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(4) | ShareBlackSnapW | -5.663 | 2.540 | [-11.125, -0.496] | 0.032 | 384 | wild cluster bootstrap | WinPct | predicted

Same-season snap weights depend on coaching decisions and performance. Lagged cap totals do not make these weights predetermined. Table 18b contains the opening-roster comparison.

### Opening staffs

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(4) | ShareBlackCoachesPre | -3.052 | 1.686 | [-6.566, 0.609] | 0.095 | 608 | wild cluster bootstrap | WinPct | predicted
(5) | ShareBlackCoachesPre | -1.722 | 2.086 | [-6.039, 2.780] | 0.422 | 581 | wild cluster bootstrap | WinPct | predicted

Column 5 uses incumbent-coach spell fixed effects. Turnover before the first opening snapshot is unknown, not zero. Table 19d reports the old season-union exposure as a contemporaneous sensitivity.

### Selection diagnostics

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | F1ShareBlackSnapW | -0.514 | 0.240 | [-0.999, -0.030] | 0.039 | 352 | wild cluster bootstrap | WinPct | predicted
(1) | LagWinPct | -0.186 | 0.077 | [-0.343, -0.029] | 0.022 | 352 | clustered | WinPct | predicted
(2) | F1ShareBlackSnapW | -5.194 | 2.684 | [-10.710, 0.345] | 0.063 | 352 | wild cluster bootstrap | WinsOverExpected | predicted
(2) | LagWinPct | -3.677 | 0.795 | [-5.298, -2.056] | 0.000 | 352 | clustered | WinsOverExpected | predicted
(3) | F1ShareBlackRoster | -0.412 | 0.193 | [-0.808, -0.010] | 0.044 | 735 | wild cluster bootstrap | WinPct | predicted
(3) | LagWinPct | -0.039 | 0.061 | [-0.164, 0.085] | 0.524 | 735 | clustered | WinPct | predicted
(4) | LagWinPct | -0.021 | 0.010 | [-0.040, -0.001] | 0.035 | 384 | wild cluster bootstrap | ShareBlackSnapW | predicted
(5) | LagWinPct | -0.016 | 0.008 | [-0.032, 0.000] | 0.055 | 736 | wild cluster bootstrap | ShareBlackRoster | predicted
(6) | LagWinPct | -0.021 | 0.010 | [-0.040, -0.001] | 0.041 | 384 | wild cluster bootstrap | ShareBlackSnapW | predicted
(7) | LagWinPct | -0.009 | 0.007 | [-0.022, 0.005] | 0.208 | 736 | wild cluster bootstrap | ShareBlackRoster | predicted

These lead/reverse-selection checks are descriptive. A small p-value rejects the corresponding no-selection restriction; a large p-value does not establish causal identification.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | F1ShareBlackCoachesPre | -0.265 | 0.131 | [-0.523, -0.009] | 0.042 | 576 | wild cluster bootstrap | WinPct | predicted
(2) | F1ShareBlackCoachesPre | -0.278 | 0.141 | [-0.554, -0.002] | 0.049 | 576 | wild cluster bootstrap | WinPct | predicted
(3) | F1ShareBlackCoachesPre | -0.226 | 0.236 | [-0.714, 0.253] | 0.346 | 492 | wild cluster bootstrap | WinPct | predicted
(3) | F1ShareBlackCoordinatorsPre | -0.139 | 0.211 | [-0.569, 0.291] | 0.514 | 492 | clustered | WinPct | predicted
(3) | F1ShareBlackPositionCoachesPre | 0.005 | 0.151 | [-0.304, 0.313] | 0.976 | 492 | clustered | WinPct | predicted
(3) | F1ShareBlackFrontOfficePre | 0.093 | 0.314 | [-0.546, 0.733] | 0.768 | 492 | clustered | WinPct | predicted

These lead/reverse-selection checks are descriptive. A small p-value rejects the corresponding no-selection restriction; a large p-value does not establish causal identification.

Within-season hire-race permutations are labeled conditional-exchangeability sensitivities. There is no random assignment mechanism, and the permutation does not preserve franchise clustering. It is not design-based randomization inference.

## Rooney policies and published diversity trends

The [policy registry](../data/reference/nfl_staff_policies.csv) separates interviews, mobility, fellowships, compensatory picks, the offensive-assistant mandate and its subsidy. The mandate and reimbursement operated in 2022–2024. They ended before 2025; the voluntary program and interview rules continued. Existing assistants could satisfy the mandate. Most 2022 head-coach hires preceded the March 28 announcement.

Table 30 selects one published observation per population, category, unit and season. Counts and percentages are not interchangeable. The endpoint comparisons below retain each series' definition and timing caveats in the CSV; they are not policy effects.

Series | First | First value | Last | Last value | Value basis
--- | --- | --- | --- | --- | ---
ac_black | 1994 | 23.0 | 2023 | 36.6 | printed percent
ac_poc | 2003 | 33.0 | 2023 | 43.6 | printed percent
coord_black | 2002 | 12.0 | 2017 | 13.0 | printed count
gm_black | 1995 | 13.0 | 2023 | 26.7 | printed percent
hc_black | 1992 | 7.0 | 2024 | 18.8 | count / number of teams
hc_poc | 2011 | 25.0 | 2024 | 28.1 | count / number of teams
player_black | 1990 | 61.0 | 2023 | 53.5 | printed percent

Black, people-of-color and multiracial categories differ. TIDES changed some category definitions and observation dates. In particular, the GM series changes to people of color, so its endpoints are not a within-definition Black-share change.

### Head-coach hiring

Era | Hires | Documented Black hires | Share, percent | Model SE, pp | Undocumented hires
--- | --- | --- | --- | --- | ---
pre_rule |  86 |  7 | 8.1 | 2.9 | 78
rule_2003 | 120 | 20 | 16.7 | 3.4 | 88
amend_2020 |   7 |  1 | 14.3 | 13.2 |  5
amend_2022 |  23 |  7 | 30.4 | 9.6 | 13
post_mandate_2025 |   7 |  1 | 14.3 | 13.2 |  6

The linked opening-coach panel covers 1990–2025. The historical extension uses archived PFR records cross-checked against Wikipedia team-season records. Expansion franchises and re-entry are left-censored. A permanent in-season appointee retained the following year is not a second hire.

The head-coach comparison is documented Black versus all other hires under an explicit ascertainment assumption. Undocumented does not mean white. The opening-head-coach counts are benchmarked against TIDES in `17-rooney-hire-benchmark.csv`.

### Inherited situations

Table 31 compares the previous season's outcomes of teams hiring documented-Black versus other head coaches, with season effects and a retained-interim indicator. The era interactions describe changes in that inherited-situation gap; they do not measure post-hire performance.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure | Outcome baseline | Estimate / baseline, percent | MDE, 80% power
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(5) | BlackDoc | 0.151 | 0.054 | [0.041, 0.261] | 0.009 | 243 | clustered | LagWinPct | documented_positive | 0.354 | 42.7 | 0.151
(5) | BlackXRule | -0.057 | 0.084 | [-0.228, 0.114] | 0.499 | 243 | clustered | LagWinPct | documented_positive | 0.354 | -16.2 | 0.235
(5) | BlackXPost | -0.100 | 0.096 | [-0.296, 0.096] | 0.306 | 243 | clustered | LagWinPct | documented_positive | 0.354 | -28.3 | 0.269
(6) | BlackDoc | 0.169 | 0.098 | [-0.031, 0.370] | 0.095 | 187 | clustered | LagWinPct | documented_positive | 0.348 | 48.6 | 0.275
(6) | BlackXRule | -0.076 | 0.124 | [-0.328, 0.177] | 0.545 | 187 | clustered | LagWinPct | documented_positive | 0.348 | -21.8 | 0.347
(6) | BlackXPost | -0.118 | 0.125 | [-0.373, 0.136] | 0.350 | 187 | clustered | LagWinPct | documented_positive | 0.348 | -34.0 | 0.350
(7) | BlackDoc | 0.997 | 0.590 | [-0.205, 2.200] | 0.101 | 179 | clustered | LagExpectedWins | documented_positive | 7.002 | 14.2 | 1.652
(7) | BlackXRule | -0.599 | 0.809 | [-2.249, 1.051] | 0.464 | 179 | clustered | LagExpectedWins | documented_positive | 7.002 | -8.6 | 2.266
(7) | BlackXPost | -0.363 | 1.275 | [-2.963, 2.237] | 0.778 | 179 | clustered | LagExpectedWins | documented_positive | 7.002 | -5.2 | 3.572
(8) | BlackDoc | 0.181 | 1.158 | [-2.182, 2.544] | 0.877 | 179 | clustered | LagWinsOverExpected | documented_positive | -1.393 | not applicable | 3.246
(8) | BlackXRule | 0.921 | 1.412 | [-1.958, 3.801] | 0.519 | 179 | clustered | LagWinsOverExpected | documented_positive | -1.393 | not applicable | 3.955
(8) | BlackXPost | 0.050 | 1.471 | [-2.951, 3.050] | 0.973 | 179 | clustered | LagWinsOverExpected | documented_positive | -1.393 | not applicable | 4.122

Lagged win percentage is a fraction; lagged expected wins and wins over expected are in wins. Game-time market probabilities are not preseason forecasts. The table cannot measure owners' private information or expectations at the appointment date.

### Relative performance

Table 32 compares the documented-Black outcome gap across hiring eras while allowing the coefficient on lagged win percentage to differ from one. Season effects absorb the common policy level. The interaction is a change in a conditional gap, not the Rooney Rule's aggregate effect on team quality.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure | Outcome baseline | Estimate / baseline, percent | MDE, 80% power
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | BlackDoc | 0.126 | 0.063 | [-0.002, 0.254] | 0.054 | 243 | clustered | WinPct | documented_positive | 0.443 | 28.4 | 0.176
(1) | BlackXRule | -0.198 | 0.074 | [-0.349, -0.046] | 0.012 | 243 | clustered | WinPct | documented_positive | 0.443 | -44.6 | 0.208
(1) | BlackXPost | -0.230 | 0.102 | [-0.439, -0.021] | 0.032 | 243 | clustered | WinPct | documented_positive | 0.443 | -51.9 | 0.287
(2) | BlackDoc | 0.094 | 0.083 | [-0.075, 0.262] | 0.266 | 243 | clustered | WinPct | documented_positive | 0.443 | 21.2 | 0.232
(2) | BlackXRule | -0.183 | 0.102 | [-0.391, 0.024] | 0.081 | 243 | clustered | WinPct | documented_positive | 0.443 | -41.4 | 0.285
(2) | BlackXPost | -0.251 | 0.129 | [-0.513, 0.012] | 0.060 | 243 | clustered | WinPct | documented_positive | 0.443 | -56.6 | 0.360
(3) | BlackDoc | 0.192 | 0.050 | [0.089, 0.294] | 0.001 | 236 | clustered | LeadWinPct | documented_positive | 0.463 | 41.4 | 0.141
(3) | BlackXRule | -0.205 | 0.054 | [-0.315, -0.096] | 0.001 | 236 | clustered | LeadWinPct | documented_positive | 0.463 | -44.4 | 0.151
(3) | BlackXPost | -0.169 | 0.091 | [-0.355, 0.016] | 0.072 | 236 | clustered | LeadWinPct | documented_positive | 0.463 | -36.6 | 0.255
(4) | BlackDoc | 0.148 | 0.084 | [-0.023, 0.319] | 0.088 | 179 | clustered | WinPct | documented_positive | 0.437 | 33.8 | 0.235
(4) | BlackXRule | -0.198 | 0.096 | [-0.393, -0.003] | 0.047 | 179 | clustered | WinPct | documented_positive | 0.437 | -45.2 | 0.268
(4) | BlackXPost | -0.257 | 0.131 | [-0.523, 0.009] | 0.058 | 179 | clustered | WinPct | documented_positive | 0.437 | -58.8 | 0.366
(5) | BlackDoc | 1.084 | 1.114 | [-1.188, 3.356] | 0.338 | 179 | clustered | WinsOverExpected | documented_positive | -0.014 | not applicable | 3.121
(5) | BlackXRule | -1.669 | 1.300 | [-4.320, 0.983] | 0.209 | 179 | clustered | WinsOverExpected | documented_positive | -0.014 | not applicable | 3.642
(5) | BlackXPost | -1.847 | 1.572 | [-5.052, 1.358] | 0.249 | 179 | clustered | WinsOverExpected | documented_positive | -0.014 | not applicable | 4.403
(6) | BlackDoc | 2.575 | 0.601 | [1.348, 3.802] | 0.000 | 172 | clustered | LeadWinsOverExpected | documented_positive | 0.031 | not applicable | 1.685
(6) | BlackXRule | -2.474 | 0.688 | [-3.876, -1.072] | 0.001 | 172 | clustered | LeadWinsOverExpected | documented_positive | 0.031 | not applicable | 1.926
(6) | BlackXPost | -2.492 | 0.930 | [-4.389, -0.596] | 0.012 | 172 | clustered | LeadWinsOverExpected | documented_positive | 0.031 | not applicable | 2.605

Win-percentage outcomes are fractions, and wins-over-expected outcomes are wins. Table 32 reports each specification's baseline and MDE; the coefficient manifest also records them. Expected wins are sums of game-time market probabilities, not preseason win totals.

Model | Check | p | Verdict
--- | --- | --- | ---
(1) | pre-rule Black x linear trend | 0.218 | not rejected; not proof of stability
(1) | pre-rule placebo break 1997 | 0.446 | not rejected; not proof of stability
(1) | rule-era Black x linear trend | 0.160 | not rejected; not proof of stability
(2) | pre-rule Black x linear trend | 0.218 | not rejected; not proof of stability
(2) | pre-rule placebo break 1997 | 0.446 | not rejected; not proof of stability
(2) | rule-era Black x linear trend | 0.160 | not rejected; not proof of stability
(3) | pre-rule Black x linear trend | 0.672 | not rejected; not proof of stability
(3) | pre-rule placebo break 1997 | 0.961 | not rejected; not proof of stability
(3) | rule-era Black x linear trend | 0.621 | not rejected; not proof of stability
(4) | pre-rule Black x linear trend | 0.115 | not rejected; not proof of stability
(4) | rule-era Black x linear trend | 0.123 | not rejected; not proof of stability
(5) | pre-rule Black x linear trend | 0.007 | reject: incompatible with a stable prior gap
(5) | rule-era Black x linear trend | 0.060 | not rejected; not proof of stability
(6) | pre-rule Black x linear trend | 0.465 | not rejected; not proof of stability
(6) | rule-era Black x linear trend | 0.559 | not rejected; not proof of stability

Madden and Ruther's [2011 study](https://doi.org/10.1177/1527002510379641) examines a change in relative coaching performance consistent with a changed hiring bar. It does not establish an aggregate league productivity effect. These hire-season comparisons are related descriptive estimands, not an exact replication.

### Offensive assistants, retention and promotion

Outcome | Offense baseline, 2019–2021 | 2022–2024 relative change | SE | Pretrend p | Pretrend verdict | MDE, 80% power
--- | --- | --- | --- | --- | --- | ---
NCoaches | 9.490 | 0.552 | 0.161 | 0.220 | not rejected | 0.452
NNewCoaches | 3.271 | 0.257 | 0.270 | 0.676 | not rejected | 0.756
EligibleShareLower | 0.213 | 0.033 | 0.027 | 0.911 | not rejected | 0.075
EligibleShareUpperCensus | 0.997 | 0.008 | 0.007 | 0.333 | not rejected | 0.020
NewEligibleShareLower | 0.222 | 0.025 | 0.036 | 0.639 | not rejected | 0.101
NewEligibleShareUpperCensus | 0.999 | 0.010 | 0.007 | 0.069 | not rejected | 0.019
RetentionRate | 0.662 | 0.034 | 0.030 | 0.307 | not rejected | 0.084
PromotionRate | 0.077 | -0.015 | 0.012 | 0.794 | not rejected | 0.034
MeanPBlackPred | 0.197 | 0.015 | 0.020 | 0.326 | not rejected | 0.056

The estimates compare offense with defense using franchise-by-unit and season fixed effects over the observed panel; 2019–2021 supplies the displayed descriptive baseline, not the entire regression's pre-period. All clubs faced the same policy, other rules changed concurrently, and defense is not randomly assigned. Counts do not reveal whether a club designated an existing employee or made an additional hire. A rejected pretrend disqualifies a treatment-effect reading; a nonrejection does not identify one. Only one observed season follows termination.

Eligibility follows the NFL's woman-or-minority definition, not a predicted-Black threshold or the Census white/nonwhite boundary. Unknown eligibility remains unknown. Complete 32-club mandate participant identities and reimbursements are unavailable in the acquired public sources. Named mandate participants and published Accelerator cohorts have separate provenance and coverage; nomination itself is selected.

Of 5339 observed coach-role spells, 61 have a source-stated appointment date and 9 have a source-stated departure date. Other boundaries describe Wikipedia listing intervals, not exact employment dates. Announcement dates are separate from appointment dates and neither establishes a contract's effective date.

Season | Opening coaches | Eligibility known | Documented eligible | Unknown eligibility
--- | --- | --- | --- | ---
2021 | 673 | 151 | 151 | 522
2024 | 761 | 153 | 153 | 608
2025 | 777 | 149 | 149 | 628

## Player retention and contract access

Table 34 starts with 46115 under-contract player-seasons in its risk set; 17021 have no observed cap-table year. It does not select the sample on pay. Current-season employment and next-season retention are separate from the annual cap and cash totals.

The main employment definition includes active, inactive, reserve and suspended/exempt statuses, but not practice squads, released players or ambiguous listings. Practice-squad-inclusive retention is separate and begins in 2016. Person-week deduplication prevents trades from doubling employment; game-week counts exclude byes. Roster weeks are not paid weeks, and pay is never divided by these counts.

The main retention sample excludes t=2015 because the next-year source has preseason spillover. Ambiguous-only future listings remain unknown, not exits. The manifests report the sensitivity including 2015 and another excluding t=2016 as well.

The following Black coefficients and confidence intervals are in percentage points; baseline outcomes are percentages. Predicted-race models target Black alone; hand/documented measures use their stated broader definitions. Column 4 controls for career stage, season-t and earlier production, and pre-NFL signals. Column 5 adds usage and employment measures. Later columns change the employment definition or risk set. SEs cluster by player; MDEs are the 2.8-SE approximation.

### Retention into the next NFL season

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure | Outcome baseline | Estimate / baseline, percent | MDE, 80% power
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | Black | -3.621 | 0.896 | [-5.378, -1.864] | 0.000 | 46083 | clustered | RetainedNextSeason | predicted | 77.758 | -4.7 | 2.509
(2) | Black | -4.761 | 0.902 | [-6.529, -2.993] | 0.000 | 46083 | clustered | RetainedNextSeason | predicted | 77.758 | -6.1 | 2.525
(3) | Black | -3.292 | 0.771 | [-4.804, -1.779] | 0.000 | 46083 | clustered | RetainedNextSeason | predicted | 77.758 | -4.2 | 2.160
(4) | Black | -3.195 | 0.807 | [-4.777, -1.614] | 0.000 | 46083 | clustered | RetainedNextSeason | predicted | 77.758 | -4.1 | 2.259
(5) | Black | -2.938 | 0.794 | [-4.495, -1.382] | 0.000 | 46083 | clustered | RetainedNextSeason | predicted | 77.758 | -3.8 | 2.224
(6) | Black | -3.997 | 0.918 | [-5.796, -2.198] | 0.000 | 46083 | clustered | RetainedNextSeasonGameDay | predicted | 70.575 | -5.7 | 2.570
(7) | Black | -5.405 | 0.994 | [-7.353, -3.457] | 0.000 | 46083 | clustered | RetainedNextSeasonSameFranchise | predicted | 59.282 | -9.1 | 2.783
(8) | Black | -4.326 | 0.936 | [-6.162, -2.491] | 0.000 | 46083 | clustered | RetainedNextSeasonFinalWeek | predicted | 69.470 | -6.2 | 2.622
(9) | Black | -1.001 | 1.098 | [-3.154, 1.151] | 0.362 | 23881 | clustered | RetainedNextSeasonEmployed | predicted | 79.310 | -1.3 | 3.074

### Retention within employer and season

Table 34c compares the column-4 specification before and after adding employer-by-season fixed effects on identical known-employer rows. Employer means the last verified under-contract franchise during season t, not necessarily the final-week employer. Tied franchise listings remain unknown and are excluded; singleton employer-season cells are removed from both specifications. The outcome remains retention anywhere in the NFL. The fixed-effect comparison is descriptive among selected employed players, not a causal decomposition or an employer treatment effect.

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure | Outcome baseline | Estimate / baseline, percent | MDE, 80% power
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | Black | -3.195 | 0.807 | [-4.777, -1.614] | 0.000 | 46083 | clustered | RetainedNextSeason | predicted | 77.758 | -4.1 | 2.259
(2) | Black | -3.196 | 0.810 | [-4.784, -1.607] | 0.000 | 46082 | clustered | RetainedNextSeason | predicted | 77.757 | -4.1 | 2.269
(3) | Black | -2.829 | 0.808 | [-4.413, -1.244] | 0.000 | 46082 | clustered | RetainedNextSeason | predicted | 77.757 | -3.6 | 2.263

The common-sample Black-minus-White retention gap is -3.20 percentage points without employer-season FE and -2.83 with them; the latter's 95 percent interval is [-4.41, -1.24] percentage points.

### First observed freely bargained contract

Model | Term | Estimate | SE | 95% CI | p | N | Inference | Outcome | Race measure | Outcome baseline | Estimate / baseline, percent | MDE, 80% power
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
(1) | Black | -3.491 | 0.983 | [-5.419, -1.564] | 0.000 | 17282 | clustered | BargainedEventNext | predicted | 12.175 | -28.7 | 2.753
(2) | Black | -2.687 | 0.897 | [-4.445, -0.928] | 0.003 | 17282 | clustered | BargainedEventNext | predicted | 12.175 | -22.1 | 2.512
(3) | Black | -1.348 | 0.916 | [-3.144, 0.449] | 0.141 | 17282 | clustered | BargainedEventNext | predicted | 12.175 | -11.1 | 2.566
(4) | Black | -0.710 | 0.939 | [-2.550, 1.130] | 0.449 | 17282 | clustered | BargainedEventNext | predicted | 12.175 | -5.8 | 2.628
(5) | Black | -0.399 | 0.927 | [-2.217, 1.419] | 0.667 | 17282 | clustered | BargainedEventNext | predicted | 12.175 | -3.3 | 2.596
(6) | Black | -0.474 | 1.192 | [-2.811, 1.863] | 0.691 | 15702 | clustered | VeteranMarketEventNext | predicted | 16.565 | -2.9 | 3.338
(7) | Black | 0.457 | 1.591 | [-2.661, 3.575] | 0.774 | 11071 | clustered | NonRookieEventNext | predicted | 27.206 | 1.7 | 4.454
(8) | Black | -0.635 | 1.123 | [-2.836, 1.566] | 0.572 | 13586 | clustered | BargainedEventNext | predicted | 14.728 | -4.3 | 3.144
(9) | Black | -1.089 | 2.003 | [-5.016, 2.838] | 0.587 |  7398 | clustered | BargainedEventNext | predicted | 25.858 | -4.2 | 5.609

The main contract-access risk set uses 2011-and-later entrant cohorts and seasons 2013–2024, before a first observed UFA or extension contract. Tags/tenders, other non-rookie deals, retention-conditioned access, and experience 2–4 have separate columns. These are observed-contract outcomes: dense coverage for drafted cohorts does not establish complete historical coverage for every undrafted player.

Season 2025 is right-censored for next-season retention and has only a partial 2026 contract-signing window, so it is not a completed outcome year. Signing years do not locate exact dates. Season-t statistics can overlap an early-January signing in calendar year t+1, so contract-access controls are not guaranteed to precede every signing. The models describe survival and observed contract access among already employed players, not racial effects or wage rates.

## Remaining limits

Opening templates can be stale or incomplete even when their revision precedes the game. Pre-2007 full staff histories remain incomplete. Public demographic documentation is selective. A representative independent race-validation sample, a complete designated-assistant roster, verified non-HC appointment dates, and historical preseason win totals are not supplied by these sources.

All machine-readable estimates retain their sample and inference fields. The complete exhibits, rather than selected statistically significant coefficients, are the empirical record.


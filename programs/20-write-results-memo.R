#!/usr/bin/env Rscript
# Regenerate the existing results memo from empirical manifests after 94 passes.
# Never type coefficient values into the memo or claim policy identification.
# Results -> notes/results-memo.md. Sourced last by 95-make-all.R.

if (!exists("root")) root <- Sys.getenv("NFL_PLAYERS_ROOT", unset = getwd())
if (!exists("analysis")) analysis <- file.path(root, "data", "datasets", "analysis")
estimate_dir <- file.path(root, "output", "estimates")

read_estimate <- function(name) {
  read.csv(file.path(estimate_dir, paste0(name, ".csv")),
           stringsAsFactors = FALSE, check.names = FALSE)
}
fmt_memo <- function(x, digits = 3L) {
  ifelse(is.na(x), "not estimable", formatC(x, format = "f", digits = digits))
}
md_table <- function(x) {
  clean <- function(y) gsub("|", "\\|", as.character(y), fixed = TRUE)
  rows <- apply(x, 1L, function(y) paste(clean(y), collapse = " | "))
  c(paste(names(x), collapse = " | "),
    paste(rep("---", ncol(x)), collapse = " | "), rows, "")
}
result_rows <- function(x, table, model = NULL, term = NULL) {
  keep <- x$table == table
  if (!is.null(model)) keep <- keep & x$model %in% model
  if (!is.null(term)) keep <- keep & x$term %in% term
  out <- x[which(keep), , drop = FALSE]
  if (!nrow(out)) stop(sprintf("Memo has no results for %s.", table), call. = FALSE)
  out
}
coef_table <- function(x, scale = 1, baseline_scale = 1) {
  ci_l <- x$ci_low
  ci_h <- x$ci_high
  pv <- x$p_value
  inference <- rep("clustered", nrow(x))
  if ("p_wcb" %in% names(x)) {
    use <- !is.na(x$p_wcb)
    pv[use] <- x$p_wcb[use]
    inference[use] <- "wild cluster bootstrap"
    interval <- use & !is.na(x$wcb_ci_low) & !is.na(x$wcb_ci_high)
    ci_l[interval] <- x$wcb_ci_low[interval]
    ci_h[interval] <- x$wcb_ci_high[interval]
  }
  model <- if ("model" %in% names(x)) x$model else x$column
  out <- data.frame(Model = model, Term = x$term,
    Estimate = fmt_memo(scale * x$estimate),
    SE = fmt_memo(scale * x$std_error),
    `95% CI` = sprintf("[%s, %s]", fmt_memo(scale * ci_l), fmt_memo(scale * ci_h)),
    p = fmt_memo(pv), N = x$nobs, Inference = inference,
    check.names = FALSE)
  if ("dep_var" %in% names(x)) out[["Outcome"]] <- x$dep_var
  if ("race_measure" %in% names(x)) out[["Race measure"]] <- x$race_measure
  if ("baseline_mean" %in% names(x)) {
    out[["Outcome baseline"]] <- fmt_memo(baseline_scale * x$baseline_mean)
    meaningful <- is.finite(x$baseline_mean) & x$baseline_mean > 0
    # A percentage of a zero-centered market residual has no useful meaning.
    if ("dep_var" %in% names(x)) {
      meaningful <- meaningful & !grepl("WinsOverExpected", x$dep_var)
    }
    relative <- rep(NA_real_, nrow(x))
    relative[meaningful] <- 100 * x$estimate[meaningful] / x$baseline_mean[meaningful]
    out[["Estimate / baseline, percent"]] <- ifelse(
      meaningful, fmt_memo(relative, 1L), "not applicable")
  }
  if ("mde_80" %in% names(x)) out[["MDE, 80% power"]] <- fmt_memo(scale * x$mde_80)
  out
}

write_results_memo <- function() {
  forced_measure <- Sys.getenv("NFL_RACE_MEASURE", "")
  if (nzchar(forced_measure) && forced_measure != "predicted") {
    message("20: sensitivity run leaves the primary results memo unchanged.")
    return(invisible(NULL))
  }
  pay <- read_estimate("12-pay-gap")
  annual_pay <- read_estimate("12-pay-gap-by-year")
  roster <- read_estimate("13-roster-diversity")
  staff <- read_estimate("14-staff-diversity")
  employment <- read_estimate("19-player-retention")
  employment$baseline_mean <- employment$outcome_mean
  player_risk <- as.data.frame(arrow::read_parquet(
    file.path(analysis, "analysis_player_retention.parquet"),
    col_select = c("season", "HasPay", "InRiskSetRetention", "InRiskSetBargained")))
  birdie <- read_estimate("12-pay-gap-birdie")
  validation <- read_estimate("15-race-prediction-validation")
  policy <- read_estimate("17-rooney-policy")
  eras <- read_estimate("17-rooney-era-descriptives")
  tides <- read_estimate("17-rooney-tides-table30-series")
  team <- as.data.frame(arrow::read_parquet(file.path(analysis, "team_season.parquet")))
  hires <- as.data.frame(arrow::read_parquet(file.path(analysis, "analysis_rooney_hires.parquet")))
  spells <- as.data.frame(arrow::read_parquet(file.path(analysis, "coach_job_spells.parquet")))
  coverage <- read.csv(file.path(analysis, "coach_policy_coverage.csv"),
                       stringsAsFactors = FALSE)

  main_pay <- result_rows(pay, "table-08", "(5)", "Black")
  stopifnot(nrow(main_pay) == 1L)
  verdict <- if (main_pay$p_value < 0.05) {
    "The model rejects a zero conditional gap at five percent."
  } else {
    "The model does not distinguish the conditional gap from zero."
  }
  log_interval <- 100 * expm1(c(main_pay$ci_low, main_pay$ci_high))
  lines <- c(
    "# Results memo: race, pay, employment and coaching policies", "",
    sprintf("Generated %s by `programs/20-write-results-memo.R` after the pipeline's temporal checks.", Sys.Date()),
    "The numbers come from `output/estimates/`. Tables are in `my_paper/tables/`. Race scores and the measured controls are held fixed for inference. These are conditional associations, not estimates of discrimination or of a league-wide policy's causal effect.", "",
    "## What changed", "",
    "Primary staff models now use opening-snapshot roles, composition, priors and turnover. The cutoff is midnight UTC on each team's first regular-season game date. Season-long staff unions appear only as labeled sensitivity analyses. Incumbent-coach fixed effects preserve tenure across temporary absences. A vandalized opening template uses an earlier parsable revision, with its timestamp and fallback flag retained.", "",
    "Current annual cap totals are not predetermined controls. The snap-weighted model uses last season's cap total and starts in 2014. Same-season snap weights remain descriptive. Opening-roster specifications are reported separately.", "",
    "The old test of whether controls predict the race score did not test calibration. It has been removed. A richer prior is not established as a fix. The replacement reports score overlap and a documented-label check in one control direction.", "",
    "## Race measurement and calibration", "",
    "The model-only prediction event is **non-Hispanic Black alone**, despite the legacy column name `p_black_any_pred`. Hand/documented Black-any measures use the broader alone-or-in-combination event; the coefficient tables record their measure. Documented Hispanic and multiracial Black people are non-target observations for the model-event validation. Ambiguous or conflicting documentation remains unknown. Absence of a Hispanic or second-race statement is treated as absence of that component, an ascertainment assumption rather than self-identification. Broad Black-any AUCs remain separate discrimination statistics.", "")

  cal <- validation[validation$score == "Pred" & validation$stat == "cal_slope" &
                      validation$entity %in% c("players", "staff", "head_coaches"), ]
  cal_se <- validation[validation$score == "Pred" & validation$stat == "cal_slope_se",
                       c("entity", "value")]
  cal_n <- validation[validation$score == "Pred" & validation$stat == "cal_slope_n",
                      c("entity", "value")]
  stopifnot(nrow(cal) == 3L, !anyDuplicated(cal$entity))
  lines <- c(lines, md_table(data.frame(
    Population = cal$entity, `Logit calibration slope` = fmt_memo(cal$value),
    SE = fmt_memo(cal_se$value[match(cal$entity, cal_se$entity)]),
    N = cal_n$value[match(cal$entity, cal_n$entity)], check.names = FALSE)),
    "These are selected-documentation diagnostics, not population validation. The prior memo's player slope used a Black-any label against a Black-alone probability. Head-coach deciles that count undocumented people as zero are documented-positive lower bounds, not observed-race calibration.", "")
  if (main_pay$race_measure == "predicted") {
    overlap <- result_rows(pay, "table-08-overlap", c("(1)", "(5)"), "resid_sd_pblack")
    lines <- c(lines, md_table(data.frame(Model = overlap$model,
      `Residual score SD` = fmt_memo(overlap$estimate), N = overlap$nobs,
      check.names = FALSE)))
  }
  doc <- pay[pay$table == "table-08-calibration-check", ]
  if (nrow(doc)) {
    lines <- c(lines, md_table(coef_table(doc)),
      "The extra index uses observed controls and the race score, never salary outcomes. This is one restriction on a fame-selected documented sample. Rejection does not establish population miscalibration; nonrejection does not validate calibration given every control.", "")
  } else {
    lines <- c(lines, "This primary measure/sample has no documented-label control-index check.", "")
  }

  career_pay <- result_rows(pay, "table-12b")
  career_pay <- career_pay[grepl("^BlackExp_", career_pay$term), ]
  repeat_pay <- result_rows(pay, "table-12d")
  repeat_pay <- repeat_pay[grepl("^Black", repeat_pay$term), ]
  career_between <- result_rows(pay, "table-12b", "(7)", "BlackExp_7plus")
  career_within <- result_rows(pay, "table-12b", "(8)", "BlackExp_7plus")
  cap_between <- result_rows(pay, "table-12c", "(1)", "Black")
  cap_within <- result_rows(pay, "table-12c", "(2)", "Black")
  adjacent_pay <- result_rows(pay, "table-12d", "(5)", "BlackChange")
  retention_between <- result_rows(employment, "table-34c", "(2)", "Black")
  retention_within <- result_rows(employment, "table-34c", "(3)", "Black")
  annual_raw <- annual_pay[annual_pay$model == "Raw", ]
  annual_adjusted <- annual_pay[annual_pay$model == "Adjusted", ]
  annual_raw <- annual_raw[order(annual_raw$year_signed), ]
  annual_adjusted <- annual_adjusted[order(annual_adjusted$year_signed), ]
  stopifnot(!anyDuplicated(annual_raw$year_signed),
            identical(annual_raw$year_signed, annual_adjusted$year_signed),
            identical(annual_raw$n_year, annual_adjusted$n_year))
  annual_summary <- data.frame(
    Year = annual_raw$year_signed,
    `Raw gap, percent` = fmt_memo(annual_raw$gap_percent, 1L),
    `Raw 95% CI` = sprintf("[%s, %s]",
      fmt_memo(annual_raw$ci_percent_low, 1L),
      fmt_memo(annual_raw$ci_percent_high, 1L)),
    `Adjusted gap, percent` = fmt_memo(annual_adjusted$gap_percent, 1L),
    `Adjusted 95% CI` = sprintf("[%s, %s]",
      fmt_memo(annual_adjusted$ci_percent_low, 1L),
      fmt_memo(annual_adjusted$ci_percent_high, 1L)),
    Contracts = annual_raw$n_year,
    Window = ifelse(annual_raw$partial_year == 1L, "Partial", "Completed"),
    check.names = FALSE)

  lines <- c(lines, "## Veteran contracts and realized annual pay", "",
    sprintf("Table 8's main estimate is %s log points, SE %s. %s Its 95 percent interval permits %s to %s percent relative to equal pay, conditional on the race-calibration and outcome-model assumptions.",
      fmt_memo(main_pay$estimate), fmt_memo(main_pay$std_error), verdict,
      fmt_memo(log_interval[1], 1L), fmt_memo(log_interval[2], 1L)), "",
    md_table(coef_table(result_rows(pay, "table-08", paste0("(", 1:9, ")"), "Black"))),
    "The sample contains freely bargained UFA and extension contracts. It conditions on reaching that market. Annual cap charges in Table 12 are accounting totals, not wage rates; a cap gap cannot by itself separate employment duration from prices paid for labor.", "",
    "### Annual White-minus-Black pay gaps", "",
    "The raw and adjusted figures use identical freely bargained veteran contracts and identical race scores. Positive values mean higher White APY. Values are 100 times exp(White-minus-Black log APY) minus 100, relative to Black geometric pay, not differences in arithmetic mean pay. Intervals are pointwise 95 percent intervals with player-clustered SEs; the race scores are held fixed.", "",
    md_table(annual_summary),
    sprintf("Among the %s completed signing years, %s adjusted 95 percent intervals include zero. These are pointwise intervals, not a joint confidence band or a test of a time trend.",
      sum(annual_adjusted$partial_year == 0L),
      sum(annual_adjusted$partial_year == 0L &
            annual_adjusted$ci_low <= 0 & annual_adjusted$ci_high >= 0)), "",
    "The raw regression includes year-specific Black and other-race scores and signing-year fixed effects, with no position, quality or prior-covariate controls. The adjusted regression adds position-by-signing-year fixed effects and Table 8 column 5 controls, including the prior covariates. Control slopes are pooled across years; racial gaps vary by year. Position-year singleton cells are removed from both models. Both series are model-implied under predicted race, not observed-race group means; changing controls also changes calibration requirements. The 2026 signing window is incomplete. These are descriptive gaps among players reaching the veteran market, not causal effects.", "",
    "[Raw annual figure](../output/figures/figure-pay-gap-by-year-raw.png) | [Adjusted annual figure](../output/figures/figure-pay-gap-by-year-adjusted.png)", "",
    "### Within-player career pay profiles", "",
    "Table 12b compares position-by-season fixed effects with player plus position-by-season fixed effects on a common veteran-pay sample. Black and other-race experience interactions use the same reference category. The reported Black interactions describe changes in the Black-white gap relative to that category, not the gap's level. Player fixed effects absorb the time-invariant race score. Differences between the profiles are specification sensitivity, not a decomposition identifying survivor selection. Career stage also changes with calendar time; the profiles do not isolate employer learning.", "",
    md_table(coef_table(career_pay)),
    sprintf("In the fully controlled pair, columns (7)-(8), the Black-by-7+ experience coefficient is %s log points without player FE and %s with player FE (95 percent CI [%s, %s]), relative to 4-6 seasons. This is a change in the career profile, not a pay-gap level.",
      fmt_memo(career_between$estimate), fmt_memo(career_within$estimate),
      fmt_memo(career_within$ci_low), fmt_memo(career_within$ci_high)), "",
    "### Annual pay within franchise and season", "",
    "Table 12c holds the sample and quality controls fixed while adding paying-franchise-by-season fixed effects. The comparison concerns conditional Black-white gaps in annual accounting pay, not an employer treatment effect or a wage rate.", "",
    md_table(coef_table(result_rows(pay, "table-12c", term = "Black"))),
    sprintf("For log cap number, the Black-minus-White coefficient is %s without employer FE and %s with employer FE (95 percent CI [%s, %s]).",
      fmt_memo(cap_between$estimate), fmt_memo(cap_within$estimate),
      fmt_memo(cap_within$ci_low), fmt_memo(cap_within$ci_high)), "",
    "### Successive observed veteran contracts", "",
    "Table 12d compares log APY across newly signed contracts, with player and position-by-signing-year fixed effects. Race-by-contract-stage coefficients describe differential pay evolution relative to the first observed eligible deal; static race levels are absorbed. The sample selects players reaching repeated deals. Observed order is not complete career order, and tied signing years do not supply a defensible within-year sequence. Prior-year controls can overlap early-January signings because exact signing dates are unavailable.", "",
    md_table(coef_table(repeat_pay)),
    sprintf("With prior-production controls, the Black-minus-White difference in adjacent-contract log-APY changes is %s (95 percent CI [%s, %s]). The interval does not establish equal pay progression; the comparison includes only players receiving another eligible deal.",
      fmt_memo(adjacent_pay$estimate), fmt_memo(adjacent_pay$ci_low),
      fmt_memo(adjacent_pay$ci_high)), "",
    "### BIRDiE comparison", "",
    "Table 26 separates posterior versus prior weights, EM versus interacted-OLS slopes at the same estimable contrasts, and a supported race-specific average versus the full-sample common-slope coefficient. All observations and covariates remain in the fitted models. The unrestricted EM average is retained separately because it includes contrasts not identified by the interacted design. The bootstrap recomputes support, so its intervals describe a support-adaptive diagnostic, not the unrestricted heterogeneous gap. The one-contract-per-player comparison addresses a separate limitation: the contract-level mixture allows a person's latent race to change across contracts, which player-clustered uncertainty does not repair.", "")
  birdie_terms <- c("cond_gap_full", "cond_gap", "cond_gap_prior", "rc_int_prior", "rc_same_x",
                    "diff_weights", "diff_slopes", "diff_contrast", "diff_total",
                    "cond_gap_first", "rc_int_prior_first", "rc_same_x_first")
  bd <- birdie[match(birdie_terms, birdie$term), ]
  stopifnot(!anyNA(bd$term))
  support_n <- birdie$estimate[match("support_n", birdie$term)]
  support_share <- birdie$estimate[match("support_pblack_share", birdie$term)]
  lines <- c(lines, sprintf("The full-sample interacted design supports %s of %s contract-level Black–white contrasts, accounting for %s percent of prior Black probability mass. Rank-deficient coefficients are not interpreted as zero effects.",
    support_n, birdie$nobs[1], fmt_memo(100 * support_share, 3L)), "")
  lines <- c(lines, md_table(data.frame(Contrast = bd$term,
    `Log-point estimate` = fmt_memo(bd$estimate), SE = fmt_memo(bd$std_error),
    `SE method` = bd$se_type, check.names = FALSE)),
    sprintf("Near-equal point estimates do not establish a precise reconciliation. The interacted OLS average has a player-bootstrap SE of %s log points, and %s in the one-contract-per-player sample. This comparison is unstable under resampling; its small point difference from EM is not evidence of equivalence.",
      fmt_memo(birdie$std_error[match("rc_int_prior", birdie$term)]),
      fmt_memo(birdie$std_error[match("rc_int_prior_first", birdie$term)])), "",
    "Any remaining model disagreement at the same contrast is a substantive limitation. The decomposition explains which choices move the answer; it does not force agreement.", "")

  lines <- c(lines, "## Roster and staff composition", "",
    sprintf("The following estimates are win-percentage-point associations per 10-percentage-point increase in Black share under each table's recorded measure. The league team-season baseline win percentage is %s percent. Reported intervals use wild-cluster bootstrap inversion where available; SEs are franchise-clustered.",
            fmt_memo(100 * mean(team$WinPct), 1L)), "",
    "### Rosters", "",
    md_table(coef_table(result_rows(roster, "table-15", "(4)", "ShareBlackSnapW"), 10)),
    "Same-season snap weights depend on coaching decisions and performance. Lagged cap totals do not make these weights predetermined. Table 18b contains the opening-roster comparison.", "",
    "### Opening staffs", "",
    md_table(coef_table(result_rows(staff, "table-19", c("(4)", "(5)"),
                                     "ShareBlackCoachesPre"), 10)),
    "Column 5 uses incumbent-coach spell fixed effects. Turnover before the first opening snapshot is unknown, not zero. Table 19d reports the old season-union exposure as a contemporaneous sensitivity.", "",
    "### Selection diagnostics", "")
  for (x in list(roster, staff)) {
    diagnostics <- x[x$table %in% c("table-18", "table-23") &
                       (grepl("^F1ShareBlack", x$term) | x$term == "LagWinPct"), ]
    if (nrow(diagnostics)) {
      lines <- c(lines, md_table(coef_table(diagnostics)),
        "These lead/reverse-selection checks are descriptive. A small p-value rejects the corresponding no-selection restriction; a large p-value does not establish causal identification.", "")
    }
  }
  lines <- c(lines, "Within-season hire-race permutations are labeled conditional-exchangeability sensitivities. There is no random assignment mechanism, and the permutation does not preserve franchise clustering. It is not design-based randomization inference.", "",
    "## Rooney policies and published diversity trends", "",
    "The [policy registry](../data/reference/nfl_staff_policies.csv) separates interviews, mobility, fellowships, compensatory picks, the offensive-assistant mandate and its subsidy. The mandate and reimbursement operated in 2022–2024. They ended before 2025; the voluntary program and interview rules continued. Existing assistants could satisfy the mandate. Most 2022 head-coach hires preceded the March 28 announcement.", "",
    "Table 30 selects one published observation per population, category, unit and season. Counts and percentages are not interchangeable. The endpoint comparisons below retain each series' definition and timing caveats in the CSV; they are not policy effects.", "")
  endpoint <- do.call(rbind, lapply(split(tides, tides$series_id), function(x) {
    x <- x[!is.na(x$plot_value), ]
    x <- x[order(x$season), ]
    data.frame(Series = x$series_id[1], First = x$season[1],
      `First value` = fmt_memo(x$plot_value[1], 1L), Last = x$season[nrow(x)],
      `Last value` = fmt_memo(x$plot_value[nrow(x)], 1L),
      `Value basis` = x$value_basis[nrow(x)], check.names = FALSE)
  }))
  lines <- c(lines, md_table(endpoint),
    "Black, people-of-color and multiracial categories differ. TIDES changed some category definitions and observation dates. In particular, the GM series changes to people of color, so its endpoints are not a within-definition Black-share change.", "")
  hd <- eras[eras$panel_table == "table-31A" & eras$era_var == "rooney_era", ]
  stopifnot(nrow(hd) > 0L)
  lines <- c(lines, "### Head-coach hiring", "",
    md_table(data.frame(Era = hd$era, Hires = hd$hires,
      `Documented Black hires` = hd$black_doc_hires,
      `Share, percent` = fmt_memo(100 * hd$share_black_hires, 1L),
      `Model SE, pp` = fmt_memo(100 * hd$se_share_black, 1L),
      `Undocumented hires` = hd$hires_undocumented, check.names = FALSE)),
    sprintf("The linked opening-coach panel covers %s–%s. The historical extension uses archived PFR records cross-checked against Wikipedia team-season records. Expansion franchises and re-entry are left-censored. A permanent in-season appointee retained the following year is not a second hire.",
            min(hires$season), max(hires$season)), "",
    "The head-coach comparison is documented Black versus all other hires under an explicit ascertainment assumption. Undocumented does not mean white. The opening-head-coach counts are benchmarked against TIDES in `17-rooney-hire-benchmark.csv`.", "",
    "### Inherited situations", "",
    "Table 31 compares the previous season's outcomes of teams hiring documented-Black versus other head coaches, with season effects and a retained-interim indicator. The era interactions describe changes in that inherited-situation gap; they do not measure post-hire performance.", "")
  inherited <- policy[policy$table == "table-31" & policy$term %in%
                        c("BlackDoc", "BlackXRule", "BlackXPost"), ]
  lines <- c(lines, md_table(coef_table(inherited)),
    "Lagged win percentage is a fraction; lagged expected wins and wins over expected are in wins. Game-time market probabilities are not preseason forecasts. The table cannot measure owners' private information or expectations at the appointment date.", "",
    "### Relative performance", "",
    "Table 32 compares the documented-Black outcome gap across hiring eras while allowing the coefficient on lagged win percentage to differ from one. Season effects absorb the common policy level. The interaction is a change in a conditional gap, not the Rooney Rule's aggregate effect on team quality.", "")
  perf <- policy[policy$table == "table-32" & policy$term %in%
                   c("BlackDoc", "BlackXRule", "BlackXPost"), ]
  lines <- c(lines, md_table(coef_table(perf)),
    "Win-percentage outcomes are fractions, and wins-over-expected outcomes are wins. Table 32 reports each specification's baseline and MDE; the coefficient manifest also records them. Expected wins are sums of game-time market probabilities, not preseason win totals.", "")
  pd <- policy[policy$table == "table-32-diagnostics", ]
  lines <- c(lines, md_table(data.frame(Model = pd$column, Check = pd$design,
    p = fmt_memo(pd$p_value), Verdict = ifelse(pd$p_value < 0.05,
      "reject: incompatible with a stable prior gap", "not rejected; not proof of stability"))),
    "Madden and Ruther's [2011 study](https://doi.org/10.1177/1527002510379641) examines a change in relative coaching performance consistent with a changed hiring bar. It does not establish an aggregate league productivity effect. These hire-season comparisons are related descriptive estimands, not an exact replication.", "",
    "### Offensive assistants, retention and promotion", "")
  dd <- eras[eras$panel_table == "table-33A-did", ]
  eligibility <- coverage[coverage$block == "eligibility" &
                            coverage$item == "PolicyEligible" &
                            coverage$unit == "all_coaches" &
                            coverage$season %in% c(2021L, 2024L, 2025L), ]
  lines <- c(lines, md_table(data.frame(Outcome = dd$outcome,
    `Offense baseline, 2019–2021` = fmt_memo(dd$baseline_offense_2019_21),
    `2022–2024 relative change` = fmt_memo(dd$did_mandate), SE = fmt_memo(dd$se_mandate),
    `Pretrend p` = fmt_memo(dd$pretrend_p),
    `Pretrend verdict` = ifelse(is.na(dd$pretrend_p), "not estimable",
                               ifelse(dd$pretrend_p < 0.05, "reject", "not rejected")),
    `MDE, 80% power` = fmt_memo(dd$mde_mandate), check.names = FALSE)),
    "The estimates compare offense with defense using franchise-by-unit and season fixed effects over the observed panel; 2019–2021 supplies the displayed descriptive baseline, not the entire regression's pre-period. All clubs faced the same policy, other rules changed concurrently, and defense is not randomly assigned. Counts do not reveal whether a club designated an existing employee or made an additional hire. A rejected pretrend disqualifies a treatment-effect reading; a nonrejection does not identify one. Only one observed season follows termination.", "",
    "Eligibility follows the NFL's woman-or-minority definition, not a predicted-Black threshold or the Census white/nonwhite boundary. Unknown eligibility remains unknown. Complete 32-club mandate participant identities and reimbursements are unavailable in the acquired public sources. Named mandate participants and published Accelerator cohorts have separate provenance and coverage; nomination itself is selected.", "",
    sprintf("Of %s observed coach-role spells, %s have a source-stated appointment date and %s have a source-stated departure date. Other boundaries describe Wikipedia listing intervals, not exact employment dates. Announcement dates are separate from appointment dates and neither establishes a contract's effective date.",
      nrow(spells), sum(!is.na(spells$ActualHireDate)), sum(!is.na(spells$ActualDepartureDate))), "",
    md_table(data.frame(Season = eligibility$season,
      `Opening coaches` = eligibility$n_total,
      `Eligibility known` = eligibility$n_known,
      `Documented eligible` = eligibility$n_positive,
      `Unknown eligibility` = eligibility$n_total - eligibility$n_known,
      check.names = FALSE)),
    "## Player retention and contract access", "",
    sprintf("Table 34 starts with %s under-contract player-seasons in its risk set; %s have no observed cap-table year. It does not select the sample on pay. Current-season employment and next-season retention are separate from the annual cap and cash totals.",
      sum(player_risk$InRiskSetRetention),
      sum(player_risk$InRiskSetRetention == 1L & player_risk$HasPay == 0L)), "",
    "The main employment definition includes active, inactive, reserve and suspended/exempt statuses, but not practice squads, released players or ambiguous listings. Practice-squad-inclusive retention is separate and begins in 2016. Person-week deduplication prevents trades from doubling employment; game-week counts exclude byes. Roster weeks are not paid weeks, and pay is never divided by these counts.", "",
    "The main retention sample excludes t=2015 because the next-year source has preseason spillover. Ambiguous-only future listings remain unknown, not exits. The manifests report the sensitivity including 2015 and another excluding t=2016 as well.", "",
    "The following Black coefficients and confidence intervals are in percentage points; baseline outcomes are percentages. Predicted-race models target Black alone; hand/documented measures use their stated broader definitions. Column 4 controls for career stage, season-t and earlier production, and pre-NFL signals. Column 5 adds usage and employment measures. Later columns change the employment definition or risk set. SEs cluster by player; MDEs are the 2.8-SE approximation.", "",
    "### Retention into the next NFL season", "",
    md_table(coef_table(result_rows(employment, "table-34", paste0("(", 1:9, ")"), "Black"), 100, 100)),
    "### Retention within employer and season", "",
    "Table 34c compares the column-4 specification before and after adding employer-by-season fixed effects on identical known-employer rows. Employer means the last verified under-contract franchise during season t, not necessarily the final-week employer. Tied franchise listings remain unknown and are excluded; singleton employer-season cells are removed from both specifications. The outcome remains retention anywhere in the NFL. The fixed-effect comparison is descriptive among selected employed players, not a causal decomposition or an employer treatment effect.", "",
    md_table(coef_table(result_rows(employment, "table-34c", term = "Black"),
                        100, 100)),
    sprintf("The common-sample Black-minus-White retention gap is %s percentage points without employer-season FE and %s with them; the latter's 95 percent interval is [%s, %s] percentage points.",
      fmt_memo(100 * retention_between$estimate, 2L),
      fmt_memo(100 * retention_within$estimate, 2L),
      fmt_memo(100 * retention_within$ci_low, 2L),
      fmt_memo(100 * retention_within$ci_high, 2L)), "",
    "### First observed freely bargained contract", "",
    md_table(coef_table(result_rows(employment, "table-35", paste0("(", 1:9, ")"), "Black"), 100, 100)),
    "The main contract-access risk set uses 2011-and-later entrant cohorts and seasons 2013–2024, before a first observed UFA or extension contract. Tags/tenders, other non-rookie deals, retention-conditioned access, and experience 2–4 have separate columns. These are observed-contract outcomes: dense coverage for drafted cohorts does not establish complete historical coverage for every undrafted player.", "",
    "Season 2025 is right-censored for next-season retention and has only a partial 2026 contract-signing window, so it is not a completed outcome year. Signing years do not locate exact dates. Season-t statistics can overlap an early-January signing in calendar year t+1, so contract-access controls are not guaranteed to precede every signing. The models describe survival and observed contract access among already employed players, not racial effects or wage rates.", "",
    "## Remaining limits", "",
    "Opening templates can be stale or incomplete even when their revision precedes the game. Pre-2007 full staff histories remain incomplete. Public demographic documentation is selective. A representative independent race-validation sample, a complete designated-assistant roster, verified non-HC appointment dates, and historical preseason win totals are not supplied by these sources.", "",
    "All machine-readable estimates retain their sample and inference fields. The complete exhibits, rather than selected statistically significant coefficients, are the empirical record.", "")
  writeLines(lines, file.path(root, "notes", "results-memo.md"))
  message(sprintf("20: wrote results memo from %d pay, %d roster, %d staff and %d policy estimate rows.",
                  nrow(pay), nrow(roster), nrow(staff), nrow(policy)))
}

write_results_memo()

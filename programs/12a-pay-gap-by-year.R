#!/usr/bin/env Rscript
# Annual White-minus-Black gaps in newly signed veteran-contract APY.
# Sourced immediately after 12 by 95-make-all.R in an isolated environment;
# reuses its checked VetData, A-D control design and primary race measure.
# Never treats annual cap charges as wage rates or predicted race as observed.
# Results -> output/figures/figure-pay-gap-by-year-{raw,adjusted}.{pdf,png}
#         -> output/estimates/12-pay-gap-by-year.{csv,dta}
#         -> output/estimates/12-pay-gap-by-year-support.{csv,dta}
# Non-primary measures use the existing -<measure> output suffix.
# Author decision 2026-10-03: both plots use the same race scores and contracts;
# raw means no position, quality or race-prior covariate controls. The adjusted
# model uses Table 8(5) controls with common slopes across signing years.

needed12a <- c("VetData", "VetCols", "measure", "prior_cols", "make_fml",
               "save_estimates", "save_exhibit_figure", "theme_customs")
missing12a <- needed12a[!vapply(needed12a, exists, logical(1), inherits = TRUE)]
if (length(missing12a)) {
  stop(sprintf("12a [498]: run after 12; missing %s",
               paste(missing12a, collapse = ", ")), call. = FALSE)
}

annual_controls <- unique(c(unlist(VetCols[c("A", "B", "C", "D")],
                                   use.names = FALSE),
                            prior_cols(measure, VetData)))
annual_required <- c("contract_id", "gsis_id", "year_signed", "position",
                     "LogAPY", "Black", "OtherRace", "PWhite", annual_controls)
if (!all(annual_required %in% names(VetData))) {
  stop("12a [498]: the Table 8 design is incomplete.", call. = FALSE)
}
AnnualInput <- data.table::as.data.table(VetData[, annual_required])
if (anyDuplicated(AnnualInput$contract_id)) {
  stop("12a [498]: duplicate contract ids in the veteran sample.", call. = FALSE)
}
annual_numeric <- c("LogAPY", "Black", "OtherRace", "PWhite", annual_controls)
annual_complete <- complete.cases(AnnualInput)
for (v in annual_numeric) {
  annual_complete <- annual_complete & is.finite(AnnualInput[[v]])
}
AnnualData <- data.table::copy(AnnualInput[annual_complete])
# Remove position-year singleton cells from BOTH specifications. They carry
# no within-position-year information for the adjusted race contrast.
AnnualData[, PositionYearN := .N, by = .(position, year_signed)]
AnnualData <- AnnualData[PositionYearN > 1L]
if (!nrow(AnnualData)) {
  stop("12a [498]: no common annual-gap sample.", call. = FALSE)
}
AnnualData[, PositionYearN := NULL]
annual_years <- sort(unique(AnnualData$year_signed))
if (length(annual_years) < 2L) {
  stop("12a [498]: annual gaps require at least two signing years.",
       call. = FALSE)
}

# Freeze the data-window definition, not the execution clock. Contracts signed
# in 2026 are observed only through the current 2026 source snapshot.
last_complete_signing_year <- 2025L
AnnualSupport <- AnnualInput[, .(n_input = .N), by = year_signed]
AnnualComplete <- AnnualInput[annual_complete,
  .(n_complete = .N), by = year_signed]
AnnualUsed <- AnnualData[, .(
  n_year = .N, players_year = data.table::uniqueN(gsis_id),
  black_mass = sum(Black), white_mass = sum(PWhite), other_mass = sum(OtherRace),
  sd_black = sd(Black), mean_log_apy = mean(LogAPY)), by = year_signed]
AnnualSupport <- merge(AnnualSupport, AnnualComplete, by = "year_signed",
                       all.x = TRUE)
AnnualSupport <- merge(AnnualSupport, AnnualUsed, by = "year_signed",
                       all.x = TRUE)
AnnualSupport[is.na(n_complete), n_complete := 0L]
AnnualSupport[is.na(n_year), `:=`(n_year = 0L, players_year = 0L)]
AnnualSupport[, `:=`(
  n_missing = n_input - n_complete,
  n_singleton = n_complete - n_year,
  partial_year = as.integer(year_signed > last_complete_signing_year),
  sample = "Freely bargained UFA/extension contracts; positive APY",
  race_counts = if (measure %in% c("predicted", "preddoc")) {
    "Probability sums, not observed racial counts"
  } else "Counts under the selected race classification")]
if (any(AnnualSupport$n_year == 0L) || any(AnnualSupport$black_mass <= 0) ||
    any(AnnualSupport$white_mass <= 0) || any(AnnualSupport$sd_black <= 0)) {
  stop("12a [498]: an annual racial contrast lacks common-sample support.",
       call. = FALSE)
}

annual_black_terms <- paste0("AnnualBlack_", annual_years)
annual_other_terms <- paste0("AnnualOther_", annual_years)
for (j in seq_along(annual_years)) {
  in_year <- AnnualData$year_signed == annual_years[j]
  AnnualData[[annual_black_terms[j]]] <- AnnualData$Black * in_year
  AnnualData[[annual_other_terms[j]]] <- AnnualData$OtherRace * in_year
}
annual_race_terms <- c(annual_black_terms, annual_other_terms)
annual_fit <- function(rhs, fe, label) {
  m <- tryCatch(fixest::feols(
    make_fml("LogAPY", rhs, fe), data = AnnualData, vcov = ~gsis_id,
    fixef.rm = "none", notes = FALSE), error = function(e) {
      stop(sprintf("12a [498]: %s failed: %s", label, conditionMessage(e)),
           call. = FALSE)
    })
  if (!identical(as.integer(fixest::obs(m)), seq_len(nrow(AnnualData)))) {
    stop(sprintf("12a [498]: %s changed the common sample.", label),
         call. = FALSE)
  }
  missing_terms <- setdiff(annual_black_terms, names(stats::coef(m)))
  if (length(missing_terms)) {
    stop(sprintf("12a [498]: %s has unidentified annual contrasts: %s",
                 label, paste(missing_terms, collapse = ", ")),
         call. = FALSE)
  }
  m
}
AnnualModels <- list(
  Raw = annual_fit(annual_race_terms, "year_signed", "raw"),
  Adjusted = annual_fit(c(annual_race_terms, annual_controls),
                        "position^year_signed", "adjusted"))

AnnualEstimates <- data.table::rbindlist(lapply(names(AnnualModels), function(nm) {
  m <- AnnualModels[[nm]]
  ct <- fixest::coeftable(m)[annual_black_terms, , drop = FALSE]
  ci <- stats::confint(m)[annual_black_terms, , drop = FALSE]
  # White-minus-Black reverses BOTH the coefficient and interval endpoints.
  out <- data.table::data.table(
    table = paste0("figure-pay-gap-by-year-", tolower(nm)),
    model = nm, term = "WhiteMinusBlack", year_signed = annual_years,
    estimate = -ct[, 1L], std_error = ct[, 2L], p_value = ct[, 4L],
    ci_low = -ci[, 2L], ci_high = -ci[, 1L],
    source_black_estimate = ct[, 1L],
    source_black_ci_low = ci[, 1L], source_black_ci_high = ci[, 2L],
    nobs = stats::nobs(m),
    n_players = data.table::uniqueN(AnnualData$gsis_id[fixest::obs(m)]),
    dep_var = "LogAPY", scale = "White-minus-Black log points",
    estimator = "OLS; annual race-score interactions",
    estimand = "Model-implied White-minus-Black gap in log veteran-contract APY",
    fixed_effects = if (nm == "Raw") "Signing year" else "OTC position x signing year",
    controls = if (nm == "Raw") "None beyond annual race-score terms" else
      "Table 8(5): A-D quality controls and race-prior covariate dummies",
    shared_control_slopes = as.integer(nm == "Adjusted"),
    weighting = "Equal contract weights",
    inference = "Player-clustered SE; pointwise 95% CI; race scores held fixed",
    limitation = paste(
      "Selected veteran contracts; predicted-race calibration and outcome-model assumptions;",
      "raw retains position/quality composition; adjusted controls have pooled year slopes;",
      "2026 signing window is partial; neither series is a causal effect"))
  out[, `:=`(
    gap_percent = 100 * expm1(estimate),
    ci_percent_low = 100 * expm1(ci_low),
    ci_percent_high = 100 * expm1(ci_high),
    mde_80_log = 2.8 * std_error,
    percent_baseline = "Black APY; geometric-pay contrast, not arithmetic mean APY")]
  out
}))
AnnualEstimates <- merge(AnnualEstimates, AnnualSupport, by = "year_signed",
                         all.x = TRUE)
if (any(!is.finite(AnnualEstimates$std_error)) ||
    any(AnnualEstimates$ci_low > AnnualEstimates$estimate) ||
    any(AnnualEstimates$ci_high < AnnualEstimates$estimate) ||
    any(abs(AnnualEstimates$estimate +
            AnnualEstimates$source_black_estimate) > 1e-12)) {
  stop("12a [498]: invalid White-minus-Black estimates or intervals.",
       call. = FALSE)
}
data.table::setorder(AnnualEstimates, model, year_signed)
save_estimates(as.data.frame(AnnualEstimates), "12-pay-gap-by-year", measure)
save_estimates(as.data.frame(AnnualSupport), "12-pay-gap-by-year-support", measure)

# Both figures use identical axes and units. Percent is exp(log White APY -
# log Black APY) - 1, not the percent difference in arithmetic mean wages.
annual_ylim <- range(c(0, AnnualEstimates$ci_percent_low,
                        AnnualEstimates$ci_percent_high))
annual_race_label <- if (measure %in% c("predicted", "preddoc")) {
  "Model-implied gap using predicted race"
} else sprintf("Gap under the %s race classification", measure)
for (nm in names(AnnualModels)) {
  z <- AnnualEstimates[model == nm]
  color <- if (nm == "Raw") "#b2182b" else "#1f4e79"
  subtitle <- if (nm == "Raw") {
    "No position or quality controls"
  } else {
    "Position-year FE; career stage, prior production and pre-NFL controls"
  }
  chart <- ggplot2::ggplot(z, ggplot2::aes(x = year_signed, y = gap_percent)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_line(data = z[partial_year == 0L], color = color,
                      linewidth = 0.55) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = ci_percent_low, ymax = ci_percent_high,
                   shape = factor(partial_year)),
      color = color, linewidth = 0.45) +
    ggplot2::scale_shape_manual(values = c("0" = 16, "1" = 1), guide = "none") +
    ggplot2::scale_x_continuous(breaks = annual_years,
      labels = paste0(annual_years,
                       ifelse(annual_years > last_complete_signing_year, "*", ""))) +
    ggplot2::scale_y_continuous(limits = annual_ylim,
      labels = function(x) sprintf("%g%%", x),
      expand = ggplot2::expansion(mult = c(0.07, 0.08))) +
    ggplot2::labs(
      title = sprintf("White-minus-Black veteran pay gap: %s", tolower(nm)),
      subtitle = paste(subtitle, annual_race_label, sep = "\n"),
      x = "Contract signing year",
      y = "White APY relative to Black APY\nPercent, from log-pay contrast",
      caption = sprintf(paste0(
        "Positive = higher White APY. Pointwise 95%% CIs; SEs clustered by player.\n",
        "Same %s UFA/extension contracts in both plots. *2026 is incomplete.\n",
        "Predicted-race and selection assumptions apply; not a causal effect."),
        format(nrow(AnnualData), big.mark = ","))) +
    theme_customs() +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9),
                    plot.subtitle = ggplot2::element_text(size = 10),
                    axis.text.x = ggplot2::element_text(size = 9))
  save_exhibit_figure(chart, paste0("figure-pay-gap-by-year-", tolower(nm)),
                       measure, width = 8.5, height = 5.5)
}
message(sprintf(paste0("12a: %d years, %d common contracts, %d players; ",
                       "%d incomplete and %d singleton rows excluded."),
                length(annual_years), nrow(AnnualData),
                data.table::uniqueN(AnnualData$gsis_id),
                sum(AnnualSupport$n_missing), sum(AnnualSupport$n_singleton)))

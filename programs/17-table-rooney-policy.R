# ============================================================================
# 17-table-rooney-policy.R
# Rooney Rule and diversity-policy exhibits: descriptive staff-wide trends
# from the published TIDES series, head-coach hiring and hire performance by
# policy era, and offense/defense coaching-staff changes around the 2022-24
# minority/women offensive-assistant mandate. Nothing here is a causal
# estimate: every policy change hit all 32 franchises in the same offseason,
# so season fixed effects absorb the common policy level and the era
# interactions are descriptive comparisons across time regimes with 32
# franchise clusters and a handful of common shocks.
# Exhibits:
#   - table-30-rooney-diversity-trends: TIDES series (one authoritative value
#     per group x category x unit x season; counts and percentages kept
#     apart, head-coach percentages derived from counts over the number of
#     teams and labelled as derived) with era means and coverage
#   - table-31-rooney-hiring: head-coach hire rates and the documented-Black
#     share of hires by era; linear probability of a documented-Black hire
#     on era; inherited situation (lagged win percentage, lagged market
#     expected wins) of Black vs other hires by era
#   - table-32-rooney-hire-performance: first- and second-season team
#     outcomes after a hire on the hire's documented race x era, lagged win
#     percentage freely estimated; betting outcomes 1999+ only; pre-trend
#     and placebo-break diagnostics, MDEs
#   - table-33-coach-advancement: offense vs defense opening-staff size,
#     hiring, eligibility bounds, retention and promotion before, during and
#     after the 2022-24 mandate (difference-in-differences with pre-trend
#     test, MDE), person-level advancement by documented group, and the
#     eligibility-ascertainment statement that decides whether a baseline
#     exposure design is estimable (it is reported only when it is)
#   - output/estimates/17-rooney-policy.csv and .dta (every coefficient),
#     17-rooney-tides-series.csv (the selected TIDES values with timing and
#     definition caveats), 17-rooney-hire-benchmark.csv (own documented
#     opening-day Black head-coach counts vs TIDES by season),
#     17-rooney-era-descriptives.csv (table 30/31/33 descriptive cells)
# Inputs: data/reference/tides_nfl_race_shares.csv (+ README),
#   analysis/analysis_rooney_hires, analysis/coach_policy_team_unit_season,
#   analysis/coach_policy_person_season, analysis/coach_policy_coverage.csv
#   (programs/16), programs/00-policy-functions.R (add_rooney_policies(),
#   rooney_era()).
# Race labels: "documented Black" / "documented minority" are positive
# documentation from public sources (16); a head coach without documentation
# is "not documented Black", never "white". The predicted score P(Black)
# appears only as a labelled descriptive association (tidy output), not as
# a calibrated race measure.
# Timing: HC hires for season t are made in the offseason before t. The
# March 28, 2022 interview expansion and offensive-assistant mandate came
# after most 2022 head-coach hires, so hires are classed by the first full
# hiring cycle (add_rooney_policies(timing = "offseason_hire")); opening
# staffs by the opening-staff convention. 2022 offense staffing can respond
# to the March 2022 mandate; 2022 head-coach hires cannot.
# Sourced by 95-make-all.R after 16; standalone: Rscript programs/17-...R.
# Base R + data.table + fixest + ggplot2 (no tidyverse verbs).
# Date: October 3rd, 2026
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

# ---------------------------------------------------------------------------
# Paths (runner globals when sourced by 95-make-all.R, else from the root)
# ---------------------------------------------------------------------------

if (!exists("root")) {
  env_root <- Sys.getenv("NFL_PLAYERS_ROOT", "")
  root <- normalizePath(if (nzchar(env_root)) env_root else getwd(), mustWork = TRUE)
}
if (!exists("programs")) programs <- file.path(root, "programs")
if (!exists("analysis")) analysis <- file.path(root, "data", "datasets", "analysis")
if (!exists("tables_wd")) tables_wd <- file.path(root, "output", "tables")
if (!exists("figures_wd")) figures_wd <- file.path(root, "output", "figures")
if (!exists("paper_tables")) paper_tables <- file.path(root, "my_paper", "tables")
if (!exists("estimates_wd")) estimates_wd <- file.path(root, "output", "estimates")
for (d in c(tables_wd, figures_wd, paper_tables, estimates_wd)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
if (!exists("add_rooney_policies") || !exists("rooney_era")) {
  policy_file <- file.path(programs, "00-policy-functions.R")
  if (!file.exists(policy_file)) {
    stop("17: programs/00-policy-functions.R (add_rooney_policies, rooney_era) is required",
         call. = FALSE)
  }
  source(policy_file)
}
if (!"timing" %in% names(formals(add_rooney_policies))) {
  stop("17: add_rooney_policies() must expose timing = 'opening_staff' | 'offseason_hire'",
       call. = FALSE)
}

read_input <- function(name) {
  path <- file.path(analysis, paste0(name, ".parquet"))
  if (!file.exists(path)) stop(sprintf("17: missing input %s (run programs/16)", path), call. = FALSE)
  as.data.table(arrow::read_parquet(path))
}
need_cols <- function(dt, cols, name) {
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0) {
    stop(sprintf("17: %s lacks columns: %s", name, paste(miss, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

T0Script <- Sys.time()

# ---------------------------------------------------------------------------
# Formatting and output helpers
# ---------------------------------------------------------------------------

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}
fmt_int <- function(x) ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ","))
stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.01, "$^{***}$", ifelse(p < 0.05, "$^{**}$",
                                                           ifelse(p < 0.1, "$^{*}$", ""))))
}
tex_escape <- function(x) gsub("%", "\\%", gsub("&", "\\&", x, fixed = TRUE), fixed = TRUE)
# Minimum detectable effect at 80% power, 5% two-sided (2.80 standard errors)
MdeMultiplier <- qnorm(0.975) + qnorm(0.8)

# Scale the tabular body only; notes retain page-width wrapping and readable type.
write_tex_table <- function(name, caption, label, colspec, header, body, notes,
                            scale_down = TRUE, font_size = "footnotesize") {
  note_text <- paste(notes, collapse = " ")
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s \\label{tab:%s}}", caption, label),
    sprintf("\\%s", font_size),
    "\\setlength{\\tabcolsep}{3pt}",
    if (scale_down) "\\resizebox{\\textwidth}{!}{" else NULL,
    sprintf("\\begin{tabular}{%s}", colspec),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    if (scale_down) "}" else NULL,
    "\\par\\smallskip",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    sprintf("\\textit{Notes:} %s", note_text),
    "\\end{minipage}",
    "\\end{table}")
  for (d in c(tables_wd, paper_tables)) writeLines(lines, file.path(d, paste0(name, ".tex")))
  message(sprintf("17: wrote %s.tex", name))
  invisible(lines)
}

# Save a ggplot as pdf and png (theme_customs() from 00-setup-functions when
# sourced by the runner, else theme_minimal)
plot_theme <- function() {
  if (exists("theme_customs")) theme_customs() else
    theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
}
save_figure <- function(plot, name, width = 7, height = 4.5, dpi = 300) {
  use_showtext <- requireNamespace("showtext", quietly = TRUE)
  for (ext in c("pdf", "png")) {
    if (use_showtext) showtext::showtext_opts(dpi = if (ext == "png") dpi else 96)
    ggsave(file.path(figures_wd, paste0(name, ".", ext)), plot,
           width = width, height = height, dpi = dpi, bg = "white")
  }
  if (use_showtext) showtext::showtext_opts(dpi = 96)
  message(sprintf("17: wrote %s.pdf/.png", name))
  invisible(plot)
}

# Guarded fixest fit: any failure is fatal with the exhibit and column named
fit17 <- function(fml, data, label, vcov = ~franchise_id) {
  m <- tryCatch(feols(fml, data = data, vcov = vcov, notes = FALSE),
                error = function(e) stop(sprintf("17: model %s failed: %s", label,
                                                 conditionMessage(e)), call. = FALSE))
  if (nobs(m) < 10) stop(sprintf("17: model %s has only %d observations", label, nobs(m)),
                         call. = FALSE)
  # Cluster count stored on the model (fixest keeps no cluster variable)
  m$InputN <- nrow(data)
  m$ClusterN <- length(unique(data$franchise_id[obs(m)]))
  if (length(m$collin.var) > 0) {
    message(sprintf("17: model %s dropped collinear term(s): %s (blank in the table)", label,
                    paste(m$collin.var, collapse = ", ")))
  }
  m
}

# Tidy rows (one per term) with clustered SE, p, 95% CI, MDE and sample sizes
Estimates <- list()
tidy17 <- function(m, table, column, terms = NULL, design = "", note = "") {
  ct <- as.data.frame(coeftable(m))
  keep <- if (is.null(terms)) rownames(ct) else intersect(terms, rownames(ct))
  if (length(keep) == 0) return(invisible(NULL))
  ci <- confint(m)[keep, , drop = FALSE]
  n_cl <- m$ClusterN
  out <- data.table(table = table, column = column, term = keep,
                    estimate = ct[keep, 1], std_error = ct[keep, 2],
                    p_value = ct[keep, 4], ci_low = ci[, 1], ci_high = ci[, 2],
                    mde_80 = MdeMultiplier * ct[keep, 2],
                    baseline_mean = mean(fitted(m) + resid(m)),
                    n_input = m$InputN, nobs = nobs(m),
                    n_dropped = m$InputN - nobs(m),
                    n_clusters = n_cl, dep_var = as.character(m$fml[[2]]),
                    weighting = "equal observations",
                    fixed_effects = paste(m$fixef_vars, collapse = "+"),
                    inference = "cluster franchise_id (CR1, t with G-1 df)",
                    design = design, note = note)
  Estimates[[length(Estimates) + 1]] <<- out
  invisible(out)
}
n_clusters_of <- function(m, data) m$ClusterN
# SE of a term, NA when the term was dropped (e.g. no documented-Black hire
# in an era makes its interaction collinear)
se_of <- function(m, term) if (term %in% names(se(m))) se(m)[[term]] else NA_real_
p_of <- function(m, term) {
  ct <- coeftable(m)
  if (term %in% rownames(ct)) ct[term, 4] else NA_real_
}
coef_cell <- function(m, term, digits = 3) {
  ct <- coeftable(m)
  if (!term %in% rownames(ct)) return(c("", ""))
  c(paste0(fmt(ct[term, 1], digits), stars(ct[term, 4])),
    paste0("(", fmt(ct[term, 2], digits), ")"))
}
# Body lines of a coefficient block: one estimate row and one SE row per term
coef_block <- function(models, coef_map, digits = 3) {
  unlist(lapply(names(coef_map), function(term) {
    cells <- vapply(models, function(m) coef_cell(m, term, digits), character(2))
    if (all(cells[1, ] == "")) return(character())
    c(paste(c(coef_map[[term]], cells[1, ]), collapse = " & "),
      paste(c("", cells[2, ]), collapse = " & "))
  }))
}
row_line <- function(label, cells) paste0(paste(c(label, cells), collapse = " & "), " \\\\")
coef_lines <- function(models, coef_map, digits = 3) paste0(coef_block(models, coef_map, digits), " \\\\")
# Joint cluster-robust Wald p-value of the terms matching `pattern`
wald_p <- function(m, pattern) {
  w <- tryCatch(fixest::wald(m, keep = pattern, print = FALSE), error = function(e) NULL)
  if (is.null(w)) NA_real_ else unname(w$p)
}

# ---------------------------------------------------------------------------
# Policy eras
# ---------------------------------------------------------------------------

# Number of NFL teams by season (denominator for derived head-coach shares):
# 28 through 1994, 30 with Carolina and Jacksonville (1995), 31 with Cleveland
# (1999), 32 with Houston (2002)
n_teams <- function(season) {
  ifelse(season >= 2002, 32L, ifelse(season >= 1999, 31L, ifelse(season >= 1995, 30L, 28L)))
}
EraLevels <- c("pre_rule", "rule_2003", "amend_2020", "amend_2022", "post_mandate_2025")
EraLabels <- c(pre_rule = "Pre-rule ($<$2003)", rule_2003 = "Rule (2003--20)",
               amend_2020 = "2021", amend_2022 = "2022--24", post_mandate_2025 = "2025")
# Coarse eras for the TIDES era means (2025 has no TIDES report) and the
# hire regressions (2021, 2022-24 and 2025 are one to three seasons each)
coarse_era <- function(season) {
  ifelse(season < 2003, "pre_rule", ifelse(season <= 2020, "rule_2003", "post_2021"))
}
CoarseLabels <- c(pre_rule = "Pre-rule (1990--2002)", rule_2003 = "Rule (2003--2020)",
                  post_2021 = "Amended (2021--)")

# ===========================================================================
# Table 30: TIDES staff-wide trends
# ===========================================================================

TidesPath <- file.path(root, "data", "reference", "tides_nfl_race_shares.csv")
if (!file.exists(TidesPath)) stop("17: missing ", TidesPath, call. = FALSE)
Tides <- fread(TidesPath, colClasses = "character", na.strings = c("", "NA"))
need_cols(Tides, c("report_year", "season", "as_of", "group", "category", "value", "unit",
                   "base_n", "source_type", "source_url", "page_or_section", "notes"),
          "tides_nfl_race_shares.csv")
Tides[, `:=`(season = as.integer(season), report_year = as.integer(report_year),
             value = as.numeric(value), base_n = as.numeric(base_n))]
Tides <- Tides[!is.na(season) & !is.na(value)]
Tides[is.na(as_of), as_of := ""]
Tides[is.na(notes), notes := ""]

# Selection of one authoritative value per group x category x unit x season.
# Author decision Oct 2026 (README "Conflicts"): (1) primary TIDES over the
# secondary AP rows; (2) never a value dated "as of publication" (in-season
# interim hires, not a season-start figure); (3) the report closest in time
# to the season (contemporaneous text or current-season table over the 2023
# retrospective Appendix II, which relabels 1999/2000); (4) a row with a
# stated as-of date over one without; (5) the earlier report (the original
# statement over a later restatement). Alternatives are kept in alt_values.
Tides[, `:=`(publication_dated = grepl("publication", as_of, fixed = TRUE),
             retrospective = grepl("Appendix II", page_or_section, fixed = TRUE),
             report_distance = abs(report_year - season),
             dated = nzchar(as_of) & !grepl("publication", as_of, fixed = TRUE),
             primary = source_type == "primary")]
setorder(Tides, group, category, unit, season, -primary, publication_dated,
         report_distance, retrospective, -dated, report_year)
Tides[, `:=`(n_alternatives = .N - 1L,
             alt_values = {
               v <- value[-1]
               if (length(v) == 0) "" else paste(sprintf("%s (%s)", v, report_year[-1]), collapse = "; ")
             }),
      by = .(group, category, unit, season)]
TidesSel <- Tides[, .SD[1], by = .(group, category, unit, season)]
TidesSel <- TidesSel[publication_dated == FALSE]
stopifnot(!anyDuplicated(TidesSel[, .(group, category, unit, season)]))

# Timing and definition caveats, from the README, carried on every value
TidesSel[, timing_basis := ifelse(!primary, "secondary (AP), not TIDES",
                           ifelse(retrospective, "retrospective (2023 Appendix II)",
                                  "contemporaneous report"))]
TidesSel[, as_of_date := as_of]
TidesSel[, definition_caveat := ""]
TidesSel[retrospective & season %in% c(1999L, 2000L),
         definition_caveat := "appendix 1999/2000 season labels unreliable (README)"]
TidesSel[group == "assistant_coaches" & season == 2002L & report_year == 2003L,
         definition_caveat := "season label ambiguous (2001 or 2002)"]
TidesSel[group == "players" & season >= 2019L,
         definition_caveat := "self-identified categories from 2019 (break in series)"]
TidesSel[group == "assistant_coaches" & season >= 2018L,
         definition_caveat := paste0(definition_caveat,
                                     ifelse(nzchar(definition_caveat), "; ", ""),
                                     "two-or-more-races and not-disclosed categories from 2018")]
TidesSel[group == "coordinators" & season >= 2013L,
         definition_caveat := "coordinator list broadened from the 2013 season"]
TidesSel[group == "coordinators" & season == 2002L & report_year == 2003L,
         definition_caveat := "season label ambiguous (2001 or 2002)"]
TidesSel[group == "general_managers" & season >= 2013L & category == "black",
         definition_caveat := "GM counts are people of color from the 2014 report"]
TidesSel[group == "head_coaches" & nzchar(as_of) & substr(as_of, 6, 7) %in% c("11", "12"),
         definition_caveat := paste0(definition_caveat,
                                     ifelse(nzchar(definition_caveat), "; ", ""),
                                     "as-of date late in the season")]

# Derived percentages: head-coach counts over the number of teams (the
# denominator TIDES itself uses), assistant-coach counts only where the
# report prints the total. Derived values never replace a printed percent in
# the same series; they are a separate column.
TidesSel[, derived_base := NA_real_]
TidesSel[group == "head_coaches" & unit == "count", derived_base := n_teams(season)]
TidesSel[group == "assistant_coaches" & unit == "count" & !is.na(base_n), derived_base := base_n]
TidesSel[, derived_percent := ifelse(is.na(derived_base), NA_real_, 100 * value / derived_base)]
TidesSel[, coarse_era := coarse_era(season)]
TidesSel[, rooney_era := rooney_era(season)]

TidesOut <- TidesSel[, .(group, category, unit, season, value, base_n, derived_percent,
                         derived_base, report_year, as_of = as_of_date, timing_basis,
                         definition_caveat, n_alternatives, alt_values, rooney_era,
                         coarse_era, source_type, source_url, page_or_section)]
setorder(TidesOut, group, category, unit, season)
fwrite(TidesOut, file.path(estimates_wd, "17-rooney-tides-series.csv"))
message(sprintf("17: TIDES: %d selected values from %d rows", nrow(TidesOut), nrow(Tides)))

# Series shown in table 30 and the figure. Head-coach series are percent of
# teams: the count over the number of teams where TIDES prints a count,
# else the printed percentage (TIDES's own denominator is the number of
# teams; 2020-2023 and the pre-2003 appendix rows print only percentages);
# value_basis records which. Assistant-coach, GM and player series are the
# printed percentages; coordinators are printed counts (no total printed).
TidesSeries <- data.table(
  series_id = c("hc_black", "hc_poc", "ac_black", "ac_poc", "coord_black", "gm_black",
                "player_black"),
  group = c("head_coaches", "head_coaches", "assistant_coaches", "assistant_coaches",
            "coordinators", "general_managers", "players"),
  category = c("black", "people_of_color", "black", "people_of_color", "black", "black",
               "black"),
  unit = c("percent_of_teams", "percent_of_teams", "percent", "percent", "count", "percent",
           "percent"),
  label = c("Head coaches, Black (\\% of teams; count-derived or printed)",
            "Head coaches, people of color (\\% of teams; count-derived or printed)",
            "Assistant coaches, Black (printed \\%)",
            "Assistant coaches, people of color (printed \\%)",
            "Coordinators, Black (printed count; no total)",
            "General managers, Black (printed \\%; people of color from 2014 report)",
            "Players, Black (printed \\%; reference)"))
series_values <- function(s) {
  if (s$unit == "percent_of_teams") {
    cnt <- TidesSel[group == s$group & category == s$category & unit == "count"]
    cnt[, `:=`(plot_value = derived_percent, value_basis = "count / number of teams")]
    pct <- TidesSel[group == s$group & category == s$category & unit == "percent" &
                      !season %in% cnt$season]
    pct[, `:=`(plot_value = value, value_basis = "printed percent")]
    d <- rbind(cnt, pct)
  } else {
    d <- TidesSel[group == s$group & category == s$category & unit == s$unit]
    d[, `:=`(plot_value = value, value_basis = paste("printed", s$unit))]
  }
  d[, series_id := s$series_id]
  setorder(d, season)
  stopifnot(!anyDuplicated(d$season))
  d
}
TidesPlot <- rbindlist(lapply(seq_len(nrow(TidesSeries)), function(i) series_values(TidesSeries[i])))
fwrite(TidesPlot[, .(series_id, season, plot_value, value_basis, unit, value, derived_base,
                     report_year, timing_basis, definition_caveat, n_alternatives, alt_values)],
       file.path(estimates_wd, "17-rooney-tides-table30-series.csv"))

era_cell <- function(d, era, what = c("mean", "n", "range")) {
  what <- match.arg(what)
  x <- d[coarse_era == era & !is.na(plot_value)]
  if (nrow(x) == 0) return(if (what == "n") "0" else "--")
  switch(what,
         mean = fmt(mean(x$plot_value), 1),
         n = sprintf("%d", nrow(x)),
         range = sprintf("%s--%s", fmt(min(x$plot_value), 1), fmt(max(x$plot_value), 1)))
}
EraOrder <- c("pre_rule", "rule_2003", "post_2021")
EraSpan <- c(pre_rule = 13L, rule_2003 = 18L, post_2021 = 4L)  # 1990-2002, 2003-20, 2021-24
Body30 <- character()
Desc30 <- list()
for (i in seq_len(nrow(TidesSeries))) {
  s <- TidesSeries[i]
  d <- TidesPlot[series_id == s$series_id]
  cells <- character()
  for (e in EraOrder) {
    cells <- c(cells, era_cell(d, e, "mean"), sprintf("%s/%d", era_cell(d, e, "n"), EraSpan[[e]]),
               era_cell(d, e, "range"))
    x <- d[coarse_era == e & !is.na(plot_value)]
    Desc30[[length(Desc30) + 1]] <- data.table(
      table = "table-30", panel = "tides", series_id = s$series_id, unit = s$unit,
      scale = ifelse(s$unit == "percent_of_teams", "percent of teams (count-derived or printed)",
                     ifelse(s$unit == "count", "count", "percent")),
      era = e, mean = if (nrow(x) > 0) mean(x$plot_value) else NA_real_,
      n_seasons = nrow(x), seasons_in_era = EraSpan[[e]],
      min = if (nrow(x) > 0) min(x$plot_value) else NA_real_,
      max = if (nrow(x) > 0) max(x$plot_value) else NA_real_,
      n_retrospective = sum(x$retrospective), n_secondary = sum(!x$primary))
  }
  Body30 <- c(Body30, row_line(s$label, cells))
}

# ---------------------------------------------------------------------------
# Inputs from 16 (opening-day panel benchmark rows of table 30 use them)
# ---------------------------------------------------------------------------

Hires <- read_input("analysis_rooney_hires")
need_cols(Hires, c("franchise_id", "season", "HistoricalCoverage", "HeadCoachName",
                   "HCSpellId", "HCSpellSeason", "IsHire", "HireType", "RetainedInterim",
                   "InSeasonHCChange", "WinPct", "ExpectedWins", "WinsOverExpected",
                   "LagWinPct", "LagExpectedWins", "LagWinsOverExpected", "DocumentedBlack",
                   "DocumentedMinority", "HCRaceDocumented", "HCDocPositiveBlack",
                   "HCDocPositiveMinority", "PBlackPred"), "analysis_rooney_hires")
UnitSeason <- read_input("coach_policy_team_unit_season")
need_cols(UnitSeason, c("franchise_id", "season", "Unit", "NCoaches", "NDocumentedEligible",
                        "NCensusWhiteMen", "ShareEligibleCensus", "EligibleShareLower",
                        "EligibleShareUpperCensus", "NNewCoaches", "NNewEligible",
                        "NewEligibleShareLower", "NewEligibleShareUpperCensus",
                        "NRetainedNextSeason", "NRetainedKnown", "NPromotedNextSeason",
                        "NPromotedKnown", "NextOpeningObserved", "MeanPBlackPred",
                        "NDocumentedBlack", "NDocumentedMinority", "NDocumentedWomen"),
          "coach_policy_team_unit_season")
PersonSeason <- read_input("coach_policy_person_season")
need_cols(PersonSeason, c("franchise_id", "season", "person_id", "Unit", "RoleTier",
                          "IsOffenseCoach", "IsDefenseCoach", "IsHeadCoach", "IsCoordinator",
                          "IsPositionCoach", "NewToFranchise", "Next1Observed", "RetainedNext1",
                          "PromotedNext1", "OnAnyStaffNext1", "AdvancedAnyNext1",
                          "DocumentedBlack", "DocumentedMinority", "DocumentedWoman",
                          "PolicyEligible", "CensusWhiteMan", "PBlackPred"),
          "coach_policy_person_season")
CoveragePath <- file.path(analysis, "coach_policy_coverage.csv")
if (!file.exists(CoveragePath)) stop("17: missing ", CoveragePath, call. = FALSE)
Coverage16 <- fread(CoveragePath)
stopifnot(!anyDuplicated(Hires[, .(franchise_id, season)]),
          !anyDuplicated(UnitSeason[, .(franchise_id, season, Unit)]),
          !anyDuplicated(PersonSeason[, .(franchise_id, season, person_id)]),
          all(UnitSeason$Unit %in% c("offense", "defense")))

# Own opening-day panel rows for table 30 (2007+, coaches on the offense and
# defense staffs): the documented-Black lower bound (documented / all
# coaches) and the mean predicted score
OwnPanel <- UnitSeason[, .(NCoaches = sum(NCoaches), NDocBlack = sum(NDocumentedBlack, na.rm = TRUE),
                           ExpBlack = sum(MeanPBlackPred * NCoaches, na.rm = TRUE),
                           NPred = sum(NCoaches[!is.na(MeanPBlackPred)])),
                       by = season]
OwnPanel[, `:=`(DocBlackLower = 100 * NDocBlack / NCoaches,
                PredBlackShare = 100 * ExpBlack / NPred,
                coarse_era = coarse_era(season))]
# Own panel spans 2007-2020 and 2021-2025 (TIDES ends in 2024)
OwnSpan <- c(pre_rule = 0L, rule_2003 = 14L, post_2021 = 5L)
own_cells <- function(var) {
  unlist(lapply(EraOrder, function(e) {
    x <- OwnPanel[coarse_era == e & !is.na(get(var))]
    if (nrow(x) == 0) return(c("--", sprintf("0/%d", OwnSpan[[e]]), "--"))
    c(fmt(mean(x[[var]]), 1), sprintf("%d/%d", nrow(x), OwnSpan[[e]]),
      sprintf("%s--%s", fmt(min(x[[var]]), 1), fmt(max(x[[var]]), 1)))
  }))
}
for (v in c("DocBlackLower", "PredBlackShare")) {
  for (e in EraOrder) {
    x <- OwnPanel[coarse_era == e & !is.na(get(v))]
    Desc30[[length(Desc30) + 1]] <- data.table(
      table = "table-30", panel = "own_panel", series_id = v, unit = "percent",
      scale = "percent of opening-day offense+defense coaches", era = e,
      mean = if (nrow(x) > 0) mean(x[[v]]) else NA_real_, n_seasons = nrow(x),
      seasons_in_era = OwnSpan[[e]], min = if (nrow(x) > 0) min(x[[v]]) else NA_real_,
      max = if (nrow(x) > 0) max(x[[v]]) else NA_real_, n_retrospective = 0L, n_secondary = 0L)
  }
}
Body30 <- c(Body30, "\\midrule",
            row_line("Own opening-day coaches, documented Black (\\%, lower bound)", own_cells("DocBlackLower")),
            row_line("Own opening-day coaches, predicted P(Black) mean (\\%)", own_cells("PredBlackShare")))

Header30 <- c(
  paste0(" & \\multicolumn{3}{c}{", CoarseLabels[["pre_rule"]], "} & \\multicolumn{3}{c}{",
         CoarseLabels[["rule_2003"]], "} & \\multicolumn{3}{c}{Amended (2021--2024; own panel 2021--2025)} \\\\"),
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
  paste0("Series & Mean & Seasons & Range & Mean & Seasons & Range & Mean & Seasons & Range \\\\"))
Notes30 <- c(
  "Published TIDES Racial and Gender Report Card values (\\texttt{data/reference/tides\\_nfl\\_race\\_shares.csv}), one value per group, category, unit and season: the primary TIDES report closest in time to the season, excluding values dated at publication (in-season interim hires); seasons before 2000 come only from the retrospective Appendix II of the 2023 report, whose 1999 and 2000 labels conflict with the contemporaneous 2001 report.",
  "Counts and percentages are never mixed within a series: head-coach rows are shares of teams, the printed count over the number of teams (28, 30, 31, 32) where TIDES prints a count and the printed percentage otherwise (2020--2023 and the pre-2003 appendix rows; the tidy series file records the basis of every value); assistant-coach, GM and player rows are printed percentages (TIDES prints no assistant-coach total for most years; GM figures are people of color from the 2014 report); coordinator rows are printed counts without a total (the list broadens from the 2013 season).",
  "Mean is the unweighted mean over the seasons with a value; Seasons is values available over seasons in the era; Range is the minimum and maximum.",
  "Head-coach figures are start-of-season counts (as-of dates August--September; 2018 is a December count). Assistant-coach seasons are the season the report assigns. Player shares break in 2019 (self-identification, separate multiracial and not-disclosed categories).",
  "No TIDES NFL report exists for 2024 or 2025; the 2024 head-coach values are an AP count (secondary). The last two rows are this project's opening-day offense and defense coaches (2007--2025; programs/16): the documented-Black lower bound treats every undocumented coach as not documented Black, and the predicted share is the mean model P(Black), a measurement with error that the validation exhibit benchmarks against TIDES.",
  "Descriptive. Era means compare calendar cohorts; they are not policy effects.")
write_tex_table("table-30-rooney-diversity-trends",
                "Racial composition of NFL coaching and football operations by policy era: TIDES series",
                "rooney-diversity-trends", "lrrrrrrrrr", Header30, Body30, Notes30)

# Figure: shares over time with policy dates
PolicyDates <- data.table(season = c(2003, 2021, 2022, 2025),
                          label = c("Rooney Rule", "2020 amendments", "2022 expansion / mandate",
                                    "mandate ends"))
FigTides <- TidesPlot[series_id %in% c("hc_black", "hc_poc", "ac_black", "ac_poc") & !is.na(plot_value)]
FigTides[, series_label := c(hc_black = "Head coaches: Black (% of teams)",
                             hc_poc = "Head coaches: people of color (% of teams)",
                             ac_black = "Assistant coaches: Black (%)",
                             ac_poc = "Assistant coaches: people of color (%)")[series_id]]
FigTides[, basis := ifelse(!primary, "Secondary (AP)",
                           ifelse(retrospective, "Retrospective appendix", "Contemporaneous report"))]
FigOwn <- OwnPanel[!is.na(DocBlackLower), .(season, plot_value = DocBlackLower,
                                             series_label = "Own panel: documented-Black coaches (lower bound)",
                                             basis = "Own panel (programs/16)")]
FigData30 <- rbind(FigTides[, .(season, plot_value, series_label, basis)], FigOwn)
Fig30 <- ggplot(FigData30, aes(x = season, y = plot_value, colour = series_label)) +
  geom_vline(data = PolicyDates, aes(xintercept = season), linetype = "dashed", colour = "grey55") +
  geom_line() +
  geom_point(aes(shape = basis), size = 1.8) +
  geom_text(data = PolicyDates, aes(x = season, y = 48, label = label), inherit.aes = FALSE,
            angle = 90, vjust = -0.4, hjust = 1, size = 2.6, colour = "grey30") +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Season", y = "Percent", colour = NULL, shape = NULL,
       caption = paste0("TIDES values, one per series and season (see table notes); head-coach shares are counts\n",
                        "over the number of teams, or the printed percent where no count is printed. ",
                        "Own panel: programs/16 opening staffs.")) +
  guides(colour = guide_legend(ncol = 2, order = 1), shape = guide_legend(ncol = 4, order = 2)) +
  plot_theme() +
  theme(legend.box = "vertical", legend.text = element_text(size = rel(0.8)))
save_figure(Fig30, "figure-rooney-tides-trends", width = 8, height = 6)

# ===========================================================================
# Table 31: head-coach hiring by era
# ===========================================================================

# Hire-cycle policy timing (first full offseason hiring cycle): the parent's
# registry; the opening-staff flags 16 attached are kept under a suffix for
# the record
HiresOpen <- copy(Hires)
hire_flag_cols <- intersect(c(StaffPolicyRegistry$indicator, "RooneyEra", "PolicyTimingConvention"),
                            names(Hires))
if (length(hire_flag_cols) > 0) setnames(Hires, hire_flag_cols, paste0(hire_flag_cols, "OpeningStaff"))
Hires <- as.data.table(add_rooney_policies(as.data.frame(Hires), season_col = "season",
                                           timing = "offseason_hire"))
need_cols(Hires, c("RooneyRule", "RooneyAmend2020"), "add_rooney_policies(timing='offseason_hire')")
Hires[, HireEra := ifelse(RooneyRule == 0L, "pre_rule",
                         ifelse(RooneyAmend2020 == 0L, "rule_2003", "post_2021"))]
Hires[, RooneyEra := rooney_era(season)]
Hires[, coarse_era := coarse_era(season)]
# The hire-cycle eras coincide with the coarse calendar cohorts; both are
# kept because the registry can move a first-full-cycle season
print(table(Hires$HireEra, Hires$coarse_era))

# Documented-positive Black indicator (undocumented = not documented Black,
# never "white") and the explicit unknown flag
Hires[, `:=`(BlackDoc = as.integer(HCDocPositiveBlack == 1L),
             MinorityDoc = as.integer(HCDocPositiveMinority == 1L),
             DocUnknown = as.integer(HCRaceDocumented == 0L))]
setorder(Hires, franchise_id, season)
Hires[, `:=`(LeadWinPct = shift(WinPct, -1L), LeadWinsOverExpected = shift(WinsOverExpected, -1L),
             LeadSeason = shift(season, -1L)), by = franchise_id]
Hires[is.na(LeadSeason) | LeadSeason != season + 1L, `:=`(LeadWinPct = NA_real_,
                                                           LeadWinsOverExpected = NA_real_)]

# Benchmark of the documented-positive convention against TIDES: opening-day
# documented-Black head coaches per season vs the TIDES count
OwnHC <- Hires[, .(own_black_doc = sum(BlackDoc, na.rm = TRUE),
                   own_minority_doc = sum(MinorityDoc, na.rm = TRUE),
                   own_undocumented = sum(DocUnknown, na.rm = TRUE), n_teams = .N), by = season]
TidesHC <- TidesSel[group == "head_coaches" & unit == "count" & category %in% c("black", "people_of_color"),
                    .(season, category, tides = value, tides_basis = timing_basis)]
TidesHCw <- dcast(TidesHC, season ~ category, value.var = "tides")
setnames(TidesHCw, c("black", "people_of_color"), c("tides_black", "tides_poc"), skip_absent = TRUE)
Bench <- merge(OwnHC, TidesHCw, by = "season", all.x = TRUE)
if (!"tides_poc" %in% names(Bench)) Bench[, tides_poc := NA_real_]
Bench[, `:=`(diff_black = own_black_doc - tides_black, diff_poc = own_minority_doc - tides_poc)]
setorder(Bench, season)
fwrite(Bench, file.path(estimates_wd, "17-rooney-hire-benchmark.csv"))
BenchBlack <- Bench[!is.na(tides_black)]
BenchTxt <- sprintf("%d seasons compared, exact match in %d, mean absolute difference %s head coaches",
                    nrow(BenchBlack), sum(BenchBlack$diff_black == 0),
                    fmt(mean(abs(BenchBlack$diff_black)), 2))
message("17: documented-Black opening HC vs TIDES: ", BenchTxt)

# Panel A: descriptive by RooneyEra (calendar cohorts) and coarse era
HireSample <- Hires[IsHire == 1L]
desc_hires <- function(d, era_var, era_val) {
  all_rows <- d
  h <- d[IsHire == 1L]
  nb <- sum(h$BlackDoc)
  share <- if (nrow(h) > 0) nb / nrow(h) else NA_real_
  se <- if (nrow(h) > 1) sqrt(share * (1 - share) / nrow(h)) else NA_real_
  data.table(era_var = era_var, era = era_val,
             seasons = uniqueN(all_rows$season), franchise_seasons = nrow(all_rows),
             franchise_seasons_hire_known = sum(!is.na(all_rows$IsHire)),
             hires = nrow(h), hire_rate = nrow(h) / sum(!is.na(all_rows$IsHire)),
             retained_interim = sum(h$RetainedInterim == 1L, na.rm = TRUE),
             black_doc_hires = nb, share_black_hires = share, se_share_black = se,
             minority_doc_hires = sum(h$MinorityDoc), share_minority_hires = mean(h$MinorityDoc),
             hires_undocumented = sum(h$DocUnknown),
             seat_share_black_doc = mean(all_rows$BlackDoc),
             seat_share_minority_doc = mean(all_rows$MinorityDoc),
             mean_pblack_pred_hires = mean(h$PBlackPred, na.rm = TRUE),
             n_pblack_pred_hires = sum(!is.na(h$PBlackPred)),
             historical_rows = sum(all_rows$HistoricalCoverage != "current"))
}
DescA <- rbind(
  rbindlist(lapply(EraLevels, function(e) desc_hires(Hires[RooneyEra == e], "rooney_era", e))),
  rbindlist(lapply(EraOrder, function(e) desc_hires(Hires[coarse_era == e], "coarse_era", e))))
DescA <- DescA[seasons > 0]
# TIDES seat share for the same seasons (derived from counts)
TidesSeat <- TidesPlot[series_id == "hc_black" & !is.na(plot_value), .(season, tides_seat = plot_value)]
DescA[, tides_seat_share_black := vapply(seq_len(.N), function(i) {
  s <- if (era_var[i] == "rooney_era") Hires[RooneyEra == era[i], unique(season)] else
    Hires[coarse_era == era[i], unique(season)]
  x <- TidesSeat[season %in% s, tides_seat]
  if (length(x) == 0) NA_real_ else mean(x) / 100
}, numeric(1))]
print(DescA[era_var == "rooney_era"])

BodyA31 <- character()
for (i in seq_len(nrow(DescA[era_var == "rooney_era"]))) {
  r <- DescA[era_var == "rooney_era"][i]
  BodyA31 <- c(BodyA31, row_line(EraLabels[[r$era]], c(
    fmt_int(r$seasons), fmt_int(r$franchise_seasons_hire_known), fmt_int(r$hires),
    fmt(r$hire_rate, 3), fmt_int(r$black_doc_hires),
    sprintf("%s (%s)", fmt(r$share_black_hires, 3), fmt(r$se_share_black, 3)),
    fmt_int(r$minority_doc_hires), fmt_int(r$hires_undocumented),
    fmt(r$seat_share_black_doc, 3), fmt(r$tides_seat_share_black, 3))))
}

# Panel B: linear probability of a documented-Black (or minority) hire on
# hire-cycle era, hires only. Panel C: inherited situation on the hired
# coach's documented race x era with season FE (the season FE absorb the
# common policy level; the interactions compare Black with other hires
# within era).
HireSample[, `:=`(Rule2003 = as.integer(HireEra == "rule_2003"),
                  Post2021 = as.integer(HireEra == "post_2021"))]
HireSample[, `:=`(BlackXRule = BlackDoc * Rule2003, BlackXPost = BlackDoc * Post2021,
                  RetainedInterim = as.integer(RetainedInterim == 1L))]
Hist <- HireSample[!is.na(WinPct)]
Cur <- HireSample[season >= 1999L & !is.na(WinPct)]
HistComplete <- Hires[season %in% 1990:1998, all(HistoricalCoverage == "historical")]
message(sprintf("17: hires 1990+: %d (historical rows complete: %s); 1999+: %d",
                nrow(Hist), HistComplete, nrow(Cur)))
if (!HistComplete) message("17: 1990-1998 historical coverage incomplete; 1990+ columns use available seasons")

ModelsB31 <- list(
  "(1)" = fit17(BlackDoc ~ Rule2003 + Post2021, Hist, "31B1"),
  "(2)" = fit17(BlackDoc ~ Rule2003 + Post2021, Cur, "31B2"),
  "(3)" = fit17(MinorityDoc ~ Rule2003 + Post2021, Hist, "31B3"),
  "(4)" = fit17(BlackDoc ~ Rule2003 + Post2021 | franchise_id, Hist, "31B4"))
ModelsC31 <- list(
  "(5)" = fit17(LagWinPct ~ BlackDoc + BlackXRule + BlackXPost + RetainedInterim | season, Hist, "31C5"),
  "(6)" = fit17(LagWinPct ~ BlackDoc + BlackXRule + BlackXPost + RetainedInterim | season, Cur, "31C6"),
  "(7)" = fit17(LagExpectedWins ~ BlackDoc + BlackXRule + BlackXPost + RetainedInterim | season, Cur, "31C7"),
  "(8)" = fit17(LagWinsOverExpected ~ BlackDoc + BlackXRule + BlackXPost + RetainedInterim | season, Cur, "31C8"))
for (k in names(ModelsB31)) tidy17(ModelsB31[[k]], "table-31", k, design = "LPM hire race on hire-cycle era, cluster franchise")
for (k in names(ModelsC31)) tidy17(ModelsC31[[k]], "table-31", k, design = "inherited situation, Black x era, season FE, cluster franchise")

CoefMapB <- c(Rule2003 = "Rule era (2003--2020)", Post2021 = "Amended era (2021--)")
CoefMapC <- c(BlackDoc = "Documented Black hire", BlackXRule = "$\\times$ Rule era (2003--2020)",
              BlackXPost = "$\\times$ Amended era (2021--)", RetainedInterim = "Retained interim")
stat_rows <- function(models, data_list) {
  list(
    row_line("Observations (hires)", vapply(models, function(m) fmt_int(nobs(m)), character(1))),
    row_line("Documented Black hires", vapply(seq_along(models), function(i)
      fmt_int(sum(data_list[[i]]$BlackDoc[obs(models[[i]])])), character(1))),
    row_line("Hires without documented race", vapply(seq_along(models), function(i)
      fmt_int(sum(data_list[[i]]$DocUnknown[obs(models[[i]])])), character(1))),
    row_line("Franchise clusters", vapply(seq_along(models), function(i)
      fmt_int(n_clusters_of(models[[i]], data_list[[i]])), character(1))),
    row_line("Seasons", vapply(seq_along(models), function(i)
      fmt_int(uniqueN(data_list[[i]]$season[obs(models[[i]])])), character(1))))
}
DataB31 <- list(Hist, Cur, Hist, Hist)
DataC31 <- list(Hist, Cur, Cur, Cur)
BaseB <- vapply(seq_along(ModelsB31), function(i) {
  d <- DataB31[[i]][obs(ModelsB31[[i]])]
  y <- as.character(ModelsB31[[i]]$fml[[2]])
  fmt(mean(d[[y]][d$HireEra == "pre_rule"]), 3)
}, character(1))
BaseC <- vapply(seq_along(ModelsC31), function(i) {
  d <- DataC31[[i]][obs(ModelsC31[[i]])]
  y <- as.character(ModelsC31[[i]]$fml[[2]])
  fmt(mean(d[[y]][d$BlackDoc == 0 & d$HireEra == "pre_rule"], na.rm = TRUE), 3)
}, character(1))
MdeB <- vapply(ModelsB31, function(m) fmt(MdeMultiplier * se_of(m, "Rule2003"), 3), character(1))
MdeC <- vapply(ModelsC31, function(m) fmt(MdeMultiplier * se_of(m, "BlackXRule"), 3), character(1))

HeaderA31 <- c("\\multicolumn{11}{p{0.95\\textwidth}}{\\textit{Panel A: hires and the documented race of hires by calendar era (opening-staff cohorts)}} \\\\",
               "Era & Seasons & \\shortstack{Franchise-\\\\seasons} & Hires & \\shortstack{Hire\\\\rate} & \\shortstack{Black\\\\hires} & \\shortstack{Share Black\\\\(SE)} & \\shortstack{Minority\\\\hires} & Undocumented & \\shortstack{Black\\\\seats} & \\shortstack{TIDES Black\\\\seats} \\\\")
# Panels B and C: one column per model in table columns 2-5, blanks after
pad_lines <- function(lines, pad = 6) {
  vapply(lines, function(l) {
    l <- sub(" \\\\\\\\$", "", l)
    paste0(l, paste(rep(" &", pad), collapse = ""), " \\\\")
  }, character(1), USE.NAMES = FALSE)
}
# Documented Black hires in the reference (pre-rule) era of each sample: the
# support of the era interactions (an interaction is dropped as collinear
# when it is zero)
ref_black_row <- function(models, data_list) {
  row_line("Documented Black hires, pre-rule (reference)", vapply(seq_along(models), function(i) {
    d <- data_list[[i]][obs(models[[i]])]
    fmt_int(sum(d$BlackDoc[d$HireEra == "pre_rule"]))
  }, character(1)))
}
BodyB31 <- c(
  "\\midrule",
  "\\multicolumn{11}{p{0.95\\textwidth}}{\\textit{Panel B: documented race of the hire on hire-cycle era (linear probability; reference: pre-rule hires)}} \\\\",
  " & (1) & (2) & (3) & (4) & & & & & & \\\\",
  " & \\shortstack{Black\\\\1990--} & \\shortstack{Black\\\\1999--} & \\shortstack{Minority\\\\1990--} & \\shortstack{Black, 1990--\\\\franchise FE} & & & & & & \\\\",
  "\\cmidrule(lr){2-5}",
  pad_lines(c(coef_lines(ModelsB31, CoefMapB),
              row_line("Pre-rule mean of the outcome", BaseB),
              row_line("MDE (80\\% power) on the Rule-era term", MdeB),
              stat_rows(ModelsB31, DataB31)[[1]], ref_black_row(ModelsB31, DataB31),
              stat_rows(ModelsB31, DataB31)[[2]], stat_rows(ModelsB31, DataB31)[[3]],
              stat_rows(ModelsB31, DataB31)[[4]], stat_rows(ModelsB31, DataB31)[[5]])))
BodyC31 <- c(
  "\\midrule",
  "\\multicolumn{11}{l}{\\textit{Panel C: inherited situation of the hire (season FE)}} \\\\",
  " & (5) & (6) & (7) & (8) & & & & & & \\\\",
  " & \\shortstack{Lag win \\%\\\\1990--} & \\shortstack{Lag win \\%\\\\1999--} & \\shortstack{Lag expected\\\\wins, 1999--} & \\shortstack{Lag wins over\\\\expected, 1999--} & & & & & & \\\\",
  "\\cmidrule(lr){2-5}",
  pad_lines(c(coef_lines(ModelsC31, CoefMapC),
              row_line("Pre-rule mean, hires not documented Black", BaseC),
              row_line("MDE (80\\% power) on the Rule-era interaction", MdeC),
              stat_rows(ModelsC31, DataC31)[[1]], ref_black_row(ModelsC31, DataC31),
              stat_rows(ModelsC31, DataC31)[[2]], stat_rows(ModelsC31, DataC31)[[3]],
              stat_rows(ModelsC31, DataC31)[[4]], stat_rows(ModelsC31, DataC31)[[5]])))
Notes31 <- c(
  "A hire is a franchise-season whose first-regular-season-game head coach differs from the previous season's last-game head coach, or a retained interim coach; returns after an absence of at most two seasons and stand-ins are not hires (programs/16, the same convention as table 22). Hire rate = hires over franchise-seasons with an observed previous season.",
  "Documented Black and minority are positive documentation from public sources (16); Undocumented counts hires with no documented race, who are treated as not documented Black, not as white. Black seats is the share of franchise-seasons whose opening head coach is documented Black; TIDES Black seats is the TIDES start-of-season count over the number of teams, averaged over the era's seasons with a value.",
  sprintf("Benchmark of the documented convention: %s.", BenchTxt),
  "Panel A eras are calendar cohorts of the opening staff (rooney\\_era()). Panels B and C class hires by the first full offseason hiring cycle under each policy (add\\_rooney\\_policies(timing = \"offseason\\_hire\")): the 2003 cycle for the original rule and the 2021 cycle for the May and November 2020 amendments. The March 28, 2022 interview expansion and the offensive-assistant mandate came after most 2022 head-coach hires, so 2022 hires are classed with the 2021 amendments, and no exhibit reads 2022 hires as a response to the March 2022 changes.",
  "1990--1998 rows come from documented historical head-coach records (scripts/02d); betting-market expected wins exist from 1999 only. Season fixed effects absorb the common level of each policy, so the era interactions compare documented-Black hires with other hires within era; they are descriptive and not treatment effects: every policy changed for all 32 franchises at once, leaving no cross-sectional exposure, and the pre-rule comparison spans a different league.",
  "Standard errors clustered by franchise (32 clusters) in parentheses; MDE is the effect detectable with 80 percent power at the five percent level (2.80 SE). $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$.")
write_tex_table("table-31-rooney-hiring",
                "Head-coach hires and the race of hires by Rooney Rule era",
                "rooney-hiring", "lrrrrrrrrrr", HeaderA31,
                c(BodyA31, BodyB31, BodyC31), Notes31)

# ===========================================================================
# Table 32: hire performance by documented race x era
# ===========================================================================

Spec32 <- list(
  "(1)" = list(y = "WinPct", d = Hist, fe = "season", lag = "LagWinPct", label = "Win \\%, season $t$, 1990--"),
  "(2)" = list(y = "WinPct", d = Hist, fe = "season + franchise_id", lag = "LagWinPct", label = "Win \\%, $t$, 1990--, franchise FE"),
  "(3)" = list(y = "LeadWinPct", d = Hist, fe = "season", lag = "LagWinPct", label = "Win \\%, season $t+1$, 1990--"),
  "(4)" = list(y = "WinPct", d = Cur, fe = "season", lag = "LagWinPct + LagExpectedWins", label = "Win \\%, $t$, 1999--"),
  "(5)" = list(y = "WinsOverExpected", d = Cur, fe = "season", lag = "LagWinPct + LagExpectedWins", label = "Wins over expected, $t$, 1999--"),
  "(6)" = list(y = "LeadWinsOverExpected", d = Cur, fe = "season", lag = "LagWinPct + LagExpectedWins", label = "Wins over expected, $t+1$, 1999--"))
Models32 <- lapply(names(Spec32), function(k) {
  s <- Spec32[[k]]
  fml <- as.formula(sprintf("%s ~ BlackDoc + BlackXRule + BlackXPost + %s + RetainedInterim | %s",
                            s$y, s$lag, s$fe))
  fit17(fml, s$d, paste0("32", k))
})
names(Models32) <- names(Spec32)
for (k in names(Models32)) tidy17(Models32[[k]], "table-32", k,
                                  design = "hire outcome on documented Black x hire-cycle era, lag freely estimated, cluster franchise",
                                  note = Spec32[[k]]$label)
Data32 <- lapply(Spec32, function(s) s$d)

# Descriptive check: the predicted score P(Black) in place of the documented
# indicator. Without the prior's covariate controls this is the association
# of outcomes with the predicted score, not the latent Black/non-Black gap
# (no calibration or attenuation claim is made); tidy output only.
for (k in c("(1)", "(4)")) {
  s <- Spec32[[k]]
  d <- copy(s$d)[!is.na(PBlackPred)]
  d[, `:=`(PBlackXRule = PBlackPred * Rule2003, PBlackXPost = PBlackPred * Post2021)]
  fml <- as.formula(sprintf("%s ~ PBlackPred + PBlackXRule + PBlackXPost + %s + RetainedInterim | %s",
                            s$y, s$lag, s$fe))
  tidy17(fit17(fml, d, paste0("32 sensitivity ", k)), "table-32-sensitivity", k,
         design = "association with predicted P(Black) score, not the latent Black/non-Black gap (no prior controls; descriptive)", note = s$label)
}

# Diagnostics. (a) Pre-trend: within pre-rule hires, the Black gap on a
# linear trend in the hire season (a gap already moving before 2003 would
# make the Rule-era interaction a continuation, not a break). (b) Placebo
# break at 1997 within the pre-rule era. (c) Within-rule trend 2003-2020.
# (d) Test that the coefficient on LagWinPct equals one (the restriction a
# change-in-win-percentage outcome imposes).
diag32 <- function(m_spec, k) {
  s <- Spec32[[k]]
  pre <- s$d[HireEra == "pre_rule"]
  rule <- s$d[HireEra == "rule_2003"]
  out <- c(pretrend_p = NA_real_, placebo_1997_p = NA_real_, rule_trend_p = NA_real_, lag_one_p = NA_real_)
  if (nrow(pre) >= 20 && sum(pre$BlackDoc) >= 3) {
    pre[, `:=`(Trend = season - 1990L, BlackXTrend = BlackDoc * (season - 1990L),
               BlackX1997 = BlackDoc * as.integer(season >= 1997L))]
    mt <- fit17(as.formula(sprintf("%s ~ BlackDoc + BlackXTrend + %s + RetainedInterim | season", s$y, s$lag)),
                pre, paste0("32 pretrend ", k))
    mp <- fit17(as.formula(sprintf("%s ~ BlackDoc + BlackX1997 + %s + RetainedInterim | season", s$y, s$lag)),
                pre, paste0("32 placebo ", k))
    out[["pretrend_p"]] <- p_of(mt, "BlackXTrend")
    out[["placebo_1997_p"]] <- p_of(mp, "BlackX1997")
    tidy17(mt, "table-32-diagnostics", k, "BlackXTrend", design = "pre-rule Black x linear trend", note = s$label)
    tidy17(mp, "table-32-diagnostics", k, "BlackX1997", design = "pre-rule placebo break 1997", note = s$label)
  }
  if (nrow(rule) >= 20 && sum(rule$BlackDoc) >= 3) {
    rule[, BlackXTrend := BlackDoc * (season - 2003L)]
    mr <- fit17(as.formula(sprintf("%s ~ BlackDoc + BlackXTrend + %s + RetainedInterim | season", s$y, s$lag)),
                rule, paste0("32 rule trend ", k))
    out[["rule_trend_p"]] <- p_of(mr, "BlackXTrend")
    tidy17(mr, "table-32-diagnostics", k, "BlackXTrend", design = "rule-era Black x linear trend", note = s$label)
  }
  m <- m_spec
  if ("LagWinPct" %in% names(coef(m)) && s$y %in% c("WinPct", "LeadWinPct")) {
    t <- (coef(m)[["LagWinPct"]] - 1) / se(m)[["LagWinPct"]]
    out[["lag_one_p"]] <- 2 * pt(-abs(t), df = fixest::degrees_freedom(m, "t"))
  }
  out
}
Diag32 <- vapply(names(Models32), function(k) diag32(Models32[[k]], k), numeric(4))
print(round(Diag32, 3))

CoefMap32 <- c(BlackDoc = "Documented Black hire", BlackXRule = "$\\times$ Rule era (2003--2020)",
               BlackXPost = "$\\times$ Amended era (2021--)", LagWinPct = "Lagged win \\%",
               LagExpectedWins = "Lagged expected wins", RetainedInterim = "Retained interim")
Base32 <- vapply(seq_along(Models32), function(i) {
  d <- Data32[[i]][obs(Models32[[i]])]
  y <- Spec32[[i]]$y
  fmt(mean(d[[y]][d$BlackDoc == 0], na.rm = TRUE), 3)
}, character(1))
Header32 <- c(paste0("Outcome & ", paste(vapply(Spec32, function(s) s$label, character(1)), collapse = " & "), " \\\\"),
              paste0(" & ", paste(names(Spec32), collapse = " & "), " \\\\"))
Body32 <- c(
  coef_lines(Models32, CoefMap32),
  "\\midrule",
  row_line("Mean outcome, hires not documented Black", Base32),
  row_line("MDE (80\\% power), Black $\\times$ Rule era", vapply(Models32, function(m) fmt(MdeMultiplier * se_of(m, "BlackXRule"), 3), character(1))),
  row_line("MDE (80\\% power), Black $\\times$ Amended era", vapply(Models32, function(m) fmt(MdeMultiplier * se_of(m, "BlackXPost"), 3), character(1))),
  row_line("Pre-rule Black $\\times$ trend, $p$", fmt(Diag32["pretrend_p", ], 3)),
  row_line("Pre-rule placebo break 1997, $p$", fmt(Diag32["placebo_1997_p", ], 3)),
  row_line("Rule-era Black $\\times$ trend, $p$", fmt(Diag32["rule_trend_p", ], 3)),
  row_line("$H_0$: lagged win \\% coefficient $= 1$, $p$", fmt(Diag32["lag_one_p", ], 3)),
  stat_rows(Models32, Data32)[[1]], ref_black_row(Models32, Data32),
  stat_rows(Models32, Data32)[[2]], stat_rows(Models32, Data32)[[3]],
  stat_rows(Models32, Data32)[[4]], stat_rows(Models32, Data32)[[5]])
Notes32 <- c(
  "Hire sample of table 31. Outcomes are the franchise's regular-season results in the hire season $t$ (and $t+1$), whoever coached the games: intention-to-initial-hire, not games under the hired coach. Wins over expected is regular-season wins minus the sum of pre-game market win probabilities (1999--).",
  "The lagged win percentage enters freely; the bottom panel tests the unit-coefficient restriction that a change-in-win-percentage outcome would impose. Retained interims' lagged outcomes partly reflect the new coach's own games.",
  "Documented Black hire is positive public documentation (undocumented hires are not documented Black, never assumed white). Era is the first full offseason hiring cycle: Rule era hires 2003--2020, Amended era hires 2021 onward (the March 2022 expansion postdates most 2022 hires). Season fixed effects absorb the common policy level; the interactions compare documented-Black with other hires within era.",
  "These are not treatment effects. Every policy changed for all franchises in the same offseason, so there is no cross-sectional exposure, 32 clusters carry few common policy shocks, and a race-by-era interaction can move with the composition of hires, the teams that hire, and the league. The pre-rule trend and the 1997 placebo break test whether the Black gap was already moving before 2003 (a rejection disqualifies the Rule-era interaction as a break); the rule-era trend tests drift within 2003--2020. A null is informative only up to the MDE.",
  "Standard errors clustered by franchise in parentheses; $p$-values use $t$ with 31 degrees of freedom. $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$.")
write_tex_table("table-32-rooney-hire-performance",
                "Team performance after a head-coach hire by the hire's documented race and policy era",
                "rooney-hire-performance", "lrrrrrr", Header32, Body32, Notes32)

# ===========================================================================
# Table 33: coach advancement around the 2022-24 offensive-assistant mandate
# ===========================================================================

UnitSeason[, `:=`(Offense = as.integer(Unit == "offense"),
                  RetentionRate = ifelse(NextOpeningObserved == 1L & NRetainedKnown > 0,
                                         NRetainedNextSeason / NRetainedKnown, NA_real_),
                  PromotionRate = ifelse(NextOpeningObserved == 1L & NPromotedKnown > 0,
                                         NPromotedNextSeason / NPromotedKnown, NA_real_),
                  Period = ifelse(season <= 2021L, "pre", ifelse(season <= 2024L, "mandate", "post")),
                  Mandate = as.integer(season %in% 2022:2024),
                  Post2025 = as.integer(season >= 2025L),
                  FranchiseUnit = paste(franchise_id, Unit, sep = "-"))]
UnitSeason[, `:=`(OffXMandate = Offense * Mandate, OffXPost = Offense * Post2025)]
stopifnot(all(UnitSeason$NCoaches > 0))

Outcomes33 <- c(NCoaches = "Coaches on the opening staff", NNewCoaches = "New to the franchise",
                EligibleShareLower = "Eligible share, documented (lower bound)",
                EligibleShareUpperCensus = "Eligible share, Census-white-man assumption (upper bound)",
                NewEligibleShareLower = "Eligible share of new coaches, documented (lower bound)",
                NewEligibleShareUpperCensus = "Eligible share of new coaches, assumption (upper bound)",
                RetentionRate = "Retained on the staff next season",
                PromotionRate = "Promoted within the franchise next season",
                MeanPBlackPred = "Mean predicted P(Black)")
# Means by unit and period (2019-21 pre, 2022-24 mandate, 2025 post)
MeanWin <- list(pre = 2019:2021, mandate = 2022:2024, post = 2025L)
Desc33 <- rbindlist(lapply(names(Outcomes33), function(v) {
  rbindlist(lapply(names(MeanWin), function(p) {
    rbindlist(lapply(c("offense", "defense"), function(u) {
      x <- UnitSeason[Unit == u & season %in% MeanWin[[p]] & !is.na(get(v))]
      data.table(table = "table-33", panel = "unit_means", outcome = v, unit = u, period = p,
                 seasons = paste(range(MeanWin[[p]]), collapse = "-"),
                 mean = if (nrow(x) > 0) mean(x[[v]]) else NA_real_, n_cells = nrow(x),
                 n_franchises = uniqueN(x$franchise_id))
    }))
  }))
}))

# Difference-in-differences: offense (mandated unit) vs defense, franchise x
# unit and season FE, 2007-2025; event study with 2021 as the reference
# season. Pre-trend diagnostics: (a) the offense x linear-trend slope on the
# pre-2022 seasons (one restriction, cluster t with 31 df; the table's
# pre-trend p); (b) the joint cluster-robust Wald test of the 2017-2020
# leads (four restrictions) and (c) of all 2008-2020 leads (13 restrictions
# on 32 clusters, over-sized; tidy output only).
Did33 <- list()
Es33 <- list()
PreSeasons <- 2008:2020
ShortLeads <- 2017:2020
UnitSeason[, SeasonF := factor(season)]
for (v in names(Outcomes33)) {
  d <- UnitSeason[!is.na(get(v))]
  if (uniqueN(d$season) < 5) next
  m <- fit17(as.formula(sprintf("%s ~ OffXMandate + OffXPost | FranchiseUnit + season", v)), d,
             paste0("33 DiD ", v))
  tidy17(m, "table-33", v, design = "offense vs defense DiD, franchise x unit + season FE, cluster franchise",
         note = Outcomes33[[v]])
  # Event study: offense x season dummies, 2021 omitted
  seasons_v <- sort(unique(d$season))
  es_terms <- setdiff(seasons_v, 2021L)
  for (s in es_terms) d[, (paste0("OffX", s)) := Offense * as.integer(season == s)]
  es_fml <- as.formula(sprintf("%s ~ %s | FranchiseUnit + season", v,
                               paste(paste0("OffX", es_terms), collapse = " + ")))
  me <- fit17(es_fml, d, paste0("33 event study ", v))
  lead_terms <- paste0("OffX", intersect(PreSeasons, es_terms))
  short_terms <- paste0("OffX", intersect(ShortLeads, es_terms))
  pre_p_all <- if (length(lead_terms) >= 2) wald_p(me, paste0("^(", paste(lead_terms, collapse = "|"), ")$")) else NA_real_
  pre_p_short <- if (length(short_terms) >= 2) wald_p(me, paste0("^(", paste(short_terms, collapse = "|"), ")$")) else NA_real_
  dpre <- d[season <= 2021L]
  dpre[, OffXTrend := Offense * (season - 2021L)]
  mt <- fit17(as.formula(sprintf("%s ~ OffXTrend | FranchiseUnit + season", v)), dpre,
              paste0("33 pretrend ", v))
  pre_p <- coeftable(mt)["OffXTrend", 4]
  tidy17(mt, "table-33-pretrend", v, "OffXTrend",
         design = "offense x linear trend, pre-2022 seasons, franchise x unit + season FE",
         note = sprintf("joint leads p: %s (%d leads 2008-20), %s (%d leads 2017-20)",
                        fmt(pre_p_all, 4), length(lead_terms), fmt(pre_p_short, 4), length(short_terms)))
  es <- tidy17(me, "table-33-event-study", v, design = "offense x season, ref 2021, franchise x unit + season FE",
               note = Outcomes33[[v]])
  es[, season := as.integer(sub("^OffX", "", term))]
  es[, outcome := v]
  Es33[[v]] <- es
  ct <- coeftable(m)
  base_off <- UnitSeason[Unit == "offense" & season %in% 2019:2021 & !is.na(get(v)), mean(get(v))]
  Did33[[v]] <- data.table(
    outcome = v, label = Outcomes33[[v]],
    did_mandate = ct["OffXMandate", 1], se_mandate = ct["OffXMandate", 2], p_mandate = ct["OffXMandate", 4],
    did_post = if ("OffXPost" %in% rownames(ct)) ct["OffXPost", 1] else NA_real_,
    se_post = if ("OffXPost" %in% rownames(ct)) ct["OffXPost", 2] else NA_real_,
    p_post = if ("OffXPost" %in% rownames(ct)) ct["OffXPost", 4] else NA_real_,
    mde_mandate = MdeMultiplier * ct["OffXMandate", 2],
    pretrend_p = pre_p, pretrend_joint_all_p = pre_p_all, pretrend_joint_2017_20_p = pre_p_short,
    n_leads = length(lead_terms), baseline_offense_2019_21 = base_off,
    nobs = nobs(m), n_clusters = n_clusters_of(m, d), seasons = paste(range(d$season), collapse = "-"))
  for (s in es_terms) d[, (paste0("OffX", s)) := NULL]
}
Did33 <- rbindlist(Did33)
print(Did33[, .(outcome, did_mandate, se_mandate, did_post, se_post, pretrend_p, mde_mandate)])

BodyA33 <- character()
for (v in names(Outcomes33)) {
  m_of <- function(p, u) {
    x <- Desc33[outcome == v & period == p & unit == u]
    if (nrow(x) == 0 || is.na(x$mean)) "--" else fmt(x$mean, ifelse(v %in% c("NCoaches", "NNewCoaches"), 2, 3))
  }
  r <- Did33[outcome == v]
  dd <- if (nrow(r) == 1) {
    dig <- ifelse(v %in% c("NCoaches", "NNewCoaches"), 2, 3)
    c(paste0(fmt(r$did_mandate, dig), stars(r$p_mandate), " (", fmt(r$se_mandate, dig), ")"),
      ifelse(is.na(r$did_post), "--", paste0(fmt(r$did_post, dig), stars(r$p_post), " (", fmt(r$se_post, dig), ")")),
      fmt(r$pretrend_p, 3), fmt(r$mde_mandate, dig), fmt_int(r$nobs))
  } else rep("--", 5)
  BodyA33 <- c(BodyA33, row_line(Outcomes33[[v]], c(m_of("pre", "offense"), m_of("pre", "defense"),
                                                   m_of("mandate", "offense"), m_of("mandate", "defense"),
                                                   m_of("post", "offense"), m_of("post", "defense"), dd)))
}

# Eligibility ascertainment at the 2021 baseline (offense staffs).
# Eligibility is positive-only in 16 (documented woman, documented
# ancestry minority or league designation); the only ineligibility marker
# is the assumption-based CensusWhiteMan (ancestry-mapped white, Wikidata
# male, not league-designated), so "status known" = documented eligible OR
# Census-white man. Author decision Oct 2026: a baseline-exposure design
# (mandate x baseline eligible share under that assumption, offense units)
# is estimated only when every franchise's 2021 offense staff has a status
# for at least 90% of coaches and the pooled share is at least 95%.
# Otherwise the baseline share is reported as the identified lower bound
# and the assumption-based upper bound, and no exposure effect is
# estimated: the NFL stated in March 2022 that all 32 clubs already employed
# a qualifying assistant, so exposure would have to come from how many, and
# a share built from documented-positive status cannot stand in for that.
Base21 <- UnitSeason[season == 2021L & Unit == "offense"]
stopifnot(nrow(Base21) > 0)
Base21[, NStatusKnown := NDocumentedEligible + NCensusWhiteMen]
Base21[, ShareKnown := NStatusKnown / NCoaches]
AscertainedBaseline <- nrow(Base21) == 32 && min(Base21$ShareKnown) >= 0.9 &&
  sum(Base21$NStatusKnown) / sum(Base21$NCoaches) >= 0.95 && !anyNA(Base21$ShareEligibleCensus)
BaselineTxt <- sprintf(paste("2021 offense baseline: %d franchises; eligibility status (documented eligible or",
                             "Census-white-man assumption) for %s of coaches (franchise minimum %s);",
                             "eligible share: documented lower bound %s, assumption-based upper bound %s",
                             "(franchise means)"),
                       nrow(Base21), fmt(sum(Base21$NStatusKnown) / sum(Base21$NCoaches), 3),
                       fmt(min(Base21$ShareKnown), 3), fmt(mean(Base21$EligibleShareLower), 3),
                       fmt(mean(Base21$EligibleShareUpperCensus), 3))
message("17: ", BaselineTxt, "; ascertained = ", AscertainedBaseline)
ExposureRows <- character()
if (AscertainedBaseline) {
  Off <- merge(UnitSeason[Unit == "offense"],
               Base21[, .(franchise_id, BaseEligibleShare = ShareEligibleCensus)], by = "franchise_id")
  Off[, `:=`(MandateXBase = Mandate * (BaseEligibleShare - mean(Base21$ShareEligibleCensus)),
             PostXBase = Post2025 * (BaseEligibleShare - mean(Base21$ShareEligibleCensus)))]
  for (v in c("NCoaches", "NNewCoaches", "EligibleShareLower", "RetentionRate", "PromotionRate")) {
    d <- Off[!is.na(get(v))]
    m <- fit17(as.formula(sprintf("%s ~ MandateXBase + PostXBase | franchise_id + season", v)), d,
               paste0("33 exposure ", v))
    tidy17(m, "table-33-exposure", v,
           design = "offense units: mandate x 2021 eligible share under the Census-white-man assumption (centered), franchise + season FE",
           note = "status (documented eligible or Census-white man) >=90% per franchise, >=95% pooled")
    cell_of <- function(term) {
      cc <- coef_cell(m, term)
      if (cc[1] == "") "--" else paste0(cc[1], " ", cc[2])
    }
    ExposureRows <- c(ExposureRows, row_line(paste0(Outcomes33[[v]], " $\\times$ baseline eligible share"),
                                             c(rep("", 6), cell_of("MandateXBase"), cell_of("PostXBase"), "",
                                               fmt(MdeMultiplier * se_of(m, "MandateXBase"), 3), fmt_int(nobs(m)))))
  }
}

# Panel B: person-level advancement by documented group, offense vs defense
# coaches, 2019-21 vs 2022-24 (year+1 outcomes; 2025 has no t+1 and is
# excluded; coaches whose franchise's next opening staff is unobserved are
# excluded from the retention and promotion rates)
# Groups are positive documentation or league designation first; the
# Census-white-man marker is an assumption (ancestry-mapped white, Wikidata
# male, not league-designated), not documentation of ineligibility
PersonSeason[, Group := ifelse(DocumentedBlack == 1L & !is.na(DocumentedBlack), "black",
                        ifelse(DocumentedWoman == 1L & !is.na(DocumentedWoman), "woman",
                        ifelse(PolicyEligible == 1L & !is.na(PolicyEligible), "eligible_other",
                        ifelse(CensusWhiteMan == 1L & !is.na(CensusWhiteMan), "census_white_man",
                               "undocumented"))))]
GroupLabels <- c(black = "Documented Black", woman = "Documented woman (not Black)",
                 eligible_other = "Other eligible (documented minority or league-designated)",
                 census_white_man = "Census-white man (assumption)",
                 undocumented = "No documentation")
PersonSeason[, Period := ifelse(season %in% 2019:2021, "pre", ifelse(season %in% 2022:2024, "mandate",
                                                                     ifelse(season == 2025L, "post", "earlier")))]
PersonUnits <- PersonSeason[Unit %in% c("offense", "defense")]
PersonUnits[, UnitLabel := Unit]
if (nrow(PersonUnits) == 0) {
  PersonUnits <- PersonSeason[IsOffenseCoach == 1L | IsDefenseCoach == 1L]
  PersonUnits[, UnitLabel := ifelse(IsOffenseCoach == 1L, "offense", "defense")]
}
rate <- function(x) if (sum(!is.na(x)) == 0) NA_real_ else mean(x, na.rm = TRUE)
DescB33 <- PersonUnits[Period %in% c("pre", "mandate", "post"),
                       .(n_coach_seasons = .N, n_persons = uniqueN(person_id),
                         share_new = rate(NewToFranchise),
                         n_next1_observed = sum(Next1Observed == 1L, na.rm = TRUE),
                         retained_next1 = rate(RetainedNext1), promoted_next1 = rate(PromotedNext1),
                         on_any_staff_next1 = rate(OnAnyStaffNext1), advanced_any_next1 = rate(AdvancedAnyNext1)),
                       by = .(unit = UnitLabel, Group, Period)]
setorder(DescB33, unit, Group, Period)
DescB33[, `:=`(table = "table-33", panel = "person_advancement")]
BodyB33 <- c("\\midrule",
             "\\multicolumn{12}{l}{\\textit{Panel B: coach-seasons and year $t+1$ outcomes by documented group (offense / defense)}} \\\\",
             " & \\multicolumn{2}{c}{Coach-seasons} & \\multicolumn{2}{c}{New to franchise} & \\multicolumn{2}{c}{Retained $t+1$} & \\multicolumn{2}{c}{Promoted $t+1$} & \\multicolumn{2}{c}{Any staff $t+1$} & \\\\",
             "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9} \\cmidrule(lr){10-11}",
             "Group, period & Off. & Def. & Off. & Def. & Off. & Def. & Off. & Def. & Off. & Def. & \\\\")
for (g in names(GroupLabels)) {
  for (p in c("pre", "mandate", "post")) {
    cell <- function(u, var, dig = 3) {
      x <- DescB33[Group == g & Period == p & unit == u]
      if (nrow(x) == 0 || is.na(x[[var]])) "--" else if (var == "n_coach_seasons") fmt_int(x[[var]]) else fmt(x[[var]], dig)
    }
    plab <- c(pre = "2019--21", mandate = "2022--24", post = "2025")[[p]]
    BodyB33 <- c(BodyB33, row_line(paste0(GroupLabels[[g]], ", ", plab),
                                   c(cell("offense", "n_coach_seasons"), cell("defense", "n_coach_seasons"),
                                     cell("offense", "share_new"), cell("defense", "share_new"),
                                     cell("offense", "retained_next1"), cell("defense", "retained_next1"),
                                     cell("offense", "promoted_next1"), cell("defense", "promoted_next1"),
                                     cell("offense", "on_any_staff_next1"), cell("defense", "on_any_staff_next1"), "")))
  }
}

HeaderA33 <- c(
  "\\multicolumn{12}{p{0.95\\textwidth}}{\\textit{Panel A: offense vs defense opening staffs (means) and difference-in-differences}} \\\\",
  " & \\multicolumn{2}{c}{2019--21} & \\multicolumn{2}{c}{\\shortstack{2022--24\\\\(mandate)}} & \\multicolumn{2}{c}{\\shortstack{2025\\\\(ended)}} & \\shortstack{Off.\\,$\\times$\\\\2022--24} & \\shortstack{Off.\\,$\\times$\\\\2025} & \\shortstack{Pre-trend\\\\$p$} & MDE & $N$ \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "Outcome & Off. & Def. & Off. & Def. & Off. & Def. & (SE) & (SE) & \\shortstack{linear,\\\\pre-2022} & (80\\%) & \\\\")
BodyC33 <- c("\\midrule",
             "\\multicolumn{12}{l}{\\textit{Panel C: baseline eligibility ascertainment and exposure design}} \\\\",
             paste0("\\multicolumn{12}{p{0.95\\textwidth}}{", tex_escape(BaselineTxt), "} \\\\"),
             if (AscertainedBaseline) ExposureRows else
               "\\multicolumn{12}{p{0.95\\textwidth}}{Baseline eligibility is not sufficiently ascertained: no exposure-based policy effect is identified; the offense--defense contrasts above are descriptive.} \\\\")
Notes33 <- c(
  "Opening-day (preseason snapshot) offense and defense coaching staffs of all franchises, 2007--2025 (programs/16); New to the franchise needs the previous opening staff (2008--); retention and promotion need the next (through 2024, so 2025 is right-censored and shown as --). Means are over franchise-unit-seasons.",
  "The 2022--24 rule required each club to employ at least one offensive assistant who is a woman or a member of an ethnic or racial minority, with a league reimbursement; the NFL stated in March 2022 that all 32 clubs already met it, and ended the requirement and the reimbursement before the 2025 season (voluntary employment continues). Defense staffs were not covered.",
  "Difference-in-differences: outcome on offense $\\times$ 2022--24 and offense $\\times$ 2025 with franchise-by-unit and season fixed effects; the pre-trend $p$ tests the offense $\\times$ linear-trend slope over the pre-2022 seasons (one restriction, cluster $t$ with 31 degrees of freedom); joint tests of the 2017--2020 and 2008--2020 offense $\\times$ season leads from the event-study version (figure) are in the tidy estimates, the 13-lead test being over-sized with 32 clusters; the MDE is 2.80 SE. A rejected pre-trend disqualifies the row's contrast as a break. With no cross-franchise exposure (every club covered, every club compliant), the offense--defense contrast is a descriptive change in the mandated unit relative to the other unit, not an identified policy effect; a 2025 contrast is one post-termination season.",
  "Documented eligible = documented woman, documented ancestry minority or league designation (the NFL's minority category is wider than the Census non-white category), all positive documentation; the lower bound counts only documented-eligible coaches, the upper bound treats every coach not marked as a Census-white man (ancestry-mapped white, Wikidata male, not league-designated) as eligible, an assumption rather than documentation. Shares of eligible coaches are never built from predicted race, and no threshold on a model score defines treatment.",
  if (AscertainedBaseline) paste(
    "Panel C: with an eligibility status (documented eligible or the Census-white-man assumption) for at least 90 percent of every franchise's 2021 offense coaches (95 percent pooled), the exposure rows regress each offense-unit outcome on 2022--24 and 2025 indicators times the franchise's 2021 assumption-based eligible share (eligible over eligible plus Census-white men, centered; one unit = the whole staff), with franchise and season fixed effects: a negative coefficient means units that started with fewer eligible coaches moved more. Exposure is a continuous baseline share resting on the Census-white-man assumption, not a threshold, and the design still compares franchises that were all covered and all compliant.")
  else paste(
    "Panel C: eligibility is not ascertained for enough 2021 offense coaches to measure baseline exposure, so no exposure design is estimated; the bounds state what the documentation supports, and no policy effect is identified."),
  "Panel B groups coaches by positive documentation or league designation; the No documentation row is neither white nor ineligible, and the Census-white-man row is an assumption-based marker. Rates are over coach-seasons with the outcome observed. Standard errors clustered by franchise (32 clusters). $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$.")
write_tex_table("table-33-coach-advancement",
                "Offense and defense coaching staffs around the 2022--2024 offensive-assistant mandate",
                "coach-advancement", "lrrrrrrrrrrr", HeaderA33, c(BodyA33, BodyB33, BodyC33), Notes33,
                font_size = "scriptsize")

# Figure: event-study offense - defense gaps for staff size and hiring
EsPlot <- rbindlist(Es33[intersect(c("NCoaches", "NNewCoaches", "EligibleShareLower", "RetentionRate"), names(Es33))])
EsPlot <- rbind(EsPlot, EsPlot[, .(estimate = 0, ci_low = 0, ci_high = 0, season = 2021L), by = outcome], fill = TRUE)
EsPlot[, label := Outcomes33[outcome]]
Fig33 <- ggplot(EsPlot, aes(x = season, y = estimate)) +
  geom_vline(xintercept = c(2021.5, 2024.5), linetype = "dashed", colour = "grey55") +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, colour = "grey50") +
  geom_point(size = 1.6) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(x = "Season", y = "Offense minus defense, relative to 2021",
       caption = paste0("Offense x season coefficients (2021 omitted) with franchise-by-unit and season fixed effects;\n",
                        "95% confidence intervals clustered by franchise. Dashed lines bound the 2022-24 mandate.")) +
  plot_theme()
save_figure(Fig33, "figure-rooney-unit-event-study", width = 8, height = 5.5)

# ---------------------------------------------------------------------------
# Tidy outputs and run summary
# ---------------------------------------------------------------------------

EstimatesAll <- rbindlist(Estimates, fill = TRUE)
EstimatesAll[, race_measure := "documented_positive"]
EstimatesAll[grepl("sensitivity", table), race_measure := "predicted_score_descriptive"]
fwrite(EstimatesAll, file.path(estimates_wd, "17-rooney-policy.csv"))
# Stata companion of the coefficient manifest (same rows and columns)
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("17: package haven is required for output/estimates/17-rooney-policy.dta", call. = FALSE)
}
haven::write_dta(as.data.frame(EstimatesAll), file.path(estimates_wd, "17-rooney-policy.dta"))
DescAll <- rbindlist(list(
  rbindlist(Desc30)[, panel_table := "table-30"],
  DescA[, panel_table := "table-31A"],
  Desc33[, panel_table := "table-33A"],
  Did33[, panel_table := "table-33A-did"],
  DescB33[, panel_table := "table-33B"]), fill = TRUE)
fwrite(DescAll, file.path(estimates_wd, "17-rooney-era-descriptives.csv"))
message(sprintf("17: %d tidy estimates; %d descriptive rows; %.1f s",
                nrow(EstimatesAll), nrow(DescAll),
                as.numeric(difftime(Sys.time(), T0Script, units = "secs"))))

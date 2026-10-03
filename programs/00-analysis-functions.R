# ============================================================================
# 00-analysis-functions.R
# Shared helpers for the estimation scripts (12-14), sourced by 95-make-all.R
# after 00-setup-functions.R and 00-race-measures.R:
#   - choose_race_measure(): the primary measure is hand-coded race when it
#     covers enough of the estimation sample, else predicted race;
#     NFL_RACE_MEASURE=hand|predicted|preddoc|provisional forces a measure
#   - apply_race_measure(): copies the *BlackHand* / *BlackPred* /
#     *BlackPredDoc* / *BlackProv* columns of a team-level sample to generic
#     *Black* names for the chosen measure
#   - person_race_group(): Black / White / Other (hand, provisional)
#   - person_race_regressors(): Black / OtherRace regressors for any measure
#     (probabilities under the predicted measures)
#   - race_prior_controls(): covariates of the predicted-race prior
#   - race_measure_note(), race_reference_note(): table-note text by measure
#   - save_exhibit_tex(), save_exhibit_figure(): non-primary measures get a
#     -<measure> suffix and are never written to my_paper/tables
#   - tidy_terms(), save_estimates(): tidy coefficient files in output/estimates
#   - write_model_table(): modelsummary -> kableExtra LaTeX with project defaults
#   - add_notes(): threeparttable notes that keep LaTeX backslashes
#   - wild_cluster_test(): wild cluster bootstrap-t (fwildclusterboot)
#   - fill_missing(), blau_two_group(), fmt_p(): small helpers
# Expects the directory objects defined in 95-make-all.R.
# Date: 2026-10-02 (predicted race added the same day)
# ============================================================================

# modelsummary returns kableExtra (not tinytable) LaTeX tables
options(modelsummary_factory_latex = "kableExtra",
        modelsummary_format_numeric_latex = "plain")

# ---------------------------------------------------------------------------
# Race measure
# ---------------------------------------------------------------------------

# Measures:
#   hand        hand-coded race (notes/race-coding-protocol.md)
#   predicted   model-only predicted race (race_predicted *_pred; design in
#               notes/race-prediction-design.md): the primary measure while
#               hand codes are absent
#   preddoc     documented race where a public source states it, else the
#               model (fame-dependent documentation; sensitivity only)
#   provisional hand code where coded, else the Wikipedia category flag
#               (positive-only; sensitivity only)
RaceMeasures <- c("hand", "predicted", "preddoc", "provisional")
RaceMeasureTags <- c(hand = "Hand", predicted = "Pred", preddoc = "PredDoc",
                     provisional = "Prov")

# Minimum share of the estimation sample (or of a team group's members) with a
# hand code before hand-coded race is used
MinHandCoverage <- as.numeric(Sys.getenv("NFL_MIN_HAND_COVERAGE", "0.8"))

# The primary measure is "hand" when hand codes cover at least MinHandCoverage
# of the sample, else "predicted". NFL_RACE_MEASURE (hand, predicted, preddoc
# or provisional) forces the measure used; exhibits of a measure other than
# the primary one get a -<measure> suffix (see exhibit_name()). The primary
# measure is stored in options(nfl.race_primary).
choose_race_measure <- function(coverage, label = "") {
  forced <- Sys.getenv("NFL_RACE_MEASURE", "auto")
  if (!forced %in% c("auto", RaceMeasures)) {
    stop("NFL_RACE_MEASURE must be auto or one of ",
         paste(RaceMeasures, collapse = ", "), ", not ", forced)
  }
  primary <- if (isTRUE(coverage >= MinHandCoverage)) "hand" else "predicted"
  options(nfl.race_primary = primary)
  measure <- if (forced != "auto") forced else primary
  message(glue("{label}: hand-coded race covers {round(100 * coverage, 1)}% ",
               "of the sample; primary measure {primary}; using {measure}"))
  measure
}

# Copy the measure's team-level columns to generic names: ShareBlackPredCoaches
# -> ShareBlackCoaches, HCBlackPred -> HCBlack, ExpectedShareBlackHandSnapW ->
# ExpectedShareBlackSnapW, etc. ("BlackPred" never matches "BlackPredDoc").
# Under "hand", a group share is set to NA when its CodedShare<Group> is below
# MinHandCoverage.
apply_race_measure <- function(df, measure) {
  tag <- RaceMeasureTags[[measure]]
  pattern <- paste0("Black", tag, if (tag == "Pred") "(?!Doc)" else "")
  src <- grep(pattern, names(df), value = TRUE, perl = TRUE)
  for (s in src) {
    x <- df[[s]]
    coverage_col <- sub(paste0("^ShareBlack", tag), "CodedShare", s)
    if (measure == "hand" && startsWith(s, "ShareBlackHand") &&
        coverage_col %in% names(df)) {
      x <- if_else(df[[coverage_col]] >= MinHandCoverage, x, NA_real_)
    }
    df[[sub(pattern, "Black", s, perl = TRUE)]] <- x
  }
  df
}

# Person-level race group: "Black" (Black alone or in combination), "White"
# (white, not Hispanic or Hispanic unknown) or "Other"; NA when uncoded.
# Under "provisional", uncoded persons with a Wikipedia article are "Black"
# when a category flags them Black, "Other" when another race or Hispanic
# category flags them, and "White" (= not flagged, the reference group)
# otherwise; persons without an article stay NA. Needs race, hispanic,
# black_any and the wiki_cat_* columns from load_person_race(). Not defined
# for the predicted measures (use person_race_regressors()).
person_race_group <- function(df, measure) {
  if (measure %in% c("predicted", "preddoc")) {
    stop("person_race_group(): predicted race is a probability; use person_race_regressors()")
  }
  hand <- case_when(df$black_any == 1L ~ "Black",
                    df$race == "white" & coalesce(df$hispanic, "unknown") != "yes" ~ "White",
                    !is.na(df$race) & df$race != "unknown" ~ "Other",
                    TRUE ~ NA_character_)
  if (measure == "hand") return(hand)
  other_flag <- (coalesce(df$wiki_cat_hispanic_latino, 0L) +
                   coalesce(df$wiki_cat_asian, 0L) +
                   coalesce(df$wiki_cat_pacific_islander, 0L) +
                   coalesce(df$wiki_cat_native_american, 0L)) > 0
  wiki <- case_when(df$wiki_cat_black == 1L ~ "Black",
                    other_flag ~ "Other",
                    df$wiki_cat_black == 0L ~ "White",
                    TRUE ~ NA_character_)
  coalesce(hand, wiki)
}

# Person-level race regressors for any measure, added to df:
#   Black      1/0 (hand: Black alone or in combination; provisional: flagged)
#              or a probability: predicted = p_black_any_pred, which is
#              P(non-Hispanic Black ALONE) despite its name (Black Hispanic and
#              multiracial persons are in the Hispanic and multi categories);
#              preddoc = p_black_any_preddoc (documented multiracial persons
#              with a Black component count as Black)
#   OtherRace  1/0, or P(neither Black nor white) = 1 - P(Black) - P(white)
#   PWhite     1/0, or P(white)
#   RaceKnown  TRUE when the regressors are defined
# With probabilities, the coefficient on Black is the Black-white gap under
# regression calibration (notes/race-prediction-design.md, section 4).
person_race_regressors <- function(df, measure) {
  if (measure %in% c("hand", "provisional")) {
    group <- person_race_group(df, measure)
    df$Black <- as.integer(group == "Black")
    df$OtherRace <- as.integer(group == "Other")
    df$PWhite <- as.integer(group == "White")
  } else {
    sfx <- c(predicted = "pred", preddoc = "preddoc")[[measure]]
    df$Black <- df[[paste0("p_black_any_", sfx)]]
    df$PWhite <- df[[paste0("p_white_", sfx)]]
    df$OtherRace <- pmax(1 - df$Black - df$PWhite, 0)
  }
  df$RaceKnown <- !is.na(df$Black)
  df
}

# Predetermined covariates of the primary predicted-race prior (04e
# PRIMARY_LEVELS) that regressions under a predicted measure must control for
# (regression calibration), as fixest FE terms; character(0) for the other
# measures. draft = FALSE drops the draft bucket (for the draft-free posterior
# p_*_pred_nodraft). pred_county_available is not returned by
# load_person_race(); scripts read it from race_predicted.county_available.
# The preddoc prior also uses fame proxies (has_wiki, career_bucket; players)
# and former player (staff), which scripts add themselves.
race_prior_controls <- function(measure, entity = c("player", "staff"), draft = TRUE) {
  entity <- match.arg(entity)
  if (!measure %in% c("predicted", "preddoc")) return(character())
  v <- if (entity == "player") {
    c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket", "pred_college_type",
      "pred_county_available")
  } else {
    c("pred_role_group_first", "pred_unit_first", "pred_first_era")
  }
  if (!draft) v <- setdiff(v, "pred_draft_bucket")
  v
}

# Table-note sentence on the race measure. design = "person" (race of the
# person on the left- or right-hand side) or "team" (team shares).
race_measure_note <- function(measure, design = c("person", "team")) {
  design <- match.arg(design)
  switch(measure,
    hand = "Race is hand-coded by two independent coders with adjudication (notes/race-coding-protocol.md).",
    predicted = paste(
      "Race is predicted, not observed: each person's probability of being non-Hispanic",
      "Black combines first name, surname and hometown county (BIFSG) with an NFL-specific",
      "prior estimated by EM on predetermined characteristics (players: position at entry,",
      "rookie era, draft round, college type, county availability; staff: role, unit and era",
      "at first appearance); documented race statements are not used",
      "(notes/race-prediction-design.md).",
      if (design == "person") paste(
        "Regressions use the probabilities (regression calibration) and control for the",
        "prior's covariates; the coefficient on P(Black) is the Black-white gap if the",
        "probabilities are calibrated given the regression's controls and names and hometown",
        "are unrelated to the outcome given race and the controls.")
      else paste(
        "Team shares are expected shares (mean member probability). Their error is",
        "Berkson-type: estimates are consistent if the probabilities are calibrated given the",
        "regression's controls (the team mean of the prior is one of them), but less precise",
        "than with observed race; individual coaches' probabilities are noisy because names",
        "carry little information for many Black coaches."),
      "The validation exhibit compares the predictions with published TIDES shares."),
    preddoc = paste(
      "\\textbf{Sensitivity measure.} Race is documented where a public source",
      "(Wikidata, Wikipedia categories or article text) states it, and predicted",
      "otherwise. Documentation depends on fame, so measurement error may be",
      "correlated with the outcome."),
    provisional = paste(
      "\\textbf{Sensitivity measure.} Race is the Wikipedia category flag. The flag",
      "is positive-only and misses most Black players and staff,",
      if (design == "person") "so the comparison group mixes white and unflagged Black persons and the estimates are attenuated."
      else "so team shares are lower bounds whose coverage varies with Wikipedia editing."))
}

# Table-note sentence naming the omitted (reference) group of a person-level
# race regressor
race_reference_note <- function(measure) {
  switch(measure,
    hand = "The omitted group is white players (white, not Hispanic or Hispanic unknown).",
    predicted = ,
    preddoc = "P(Black) and P(other race) enter together, so the coefficient on P(Black) compares a Black with a white player.",
    provisional = "The omitted group is players with a Wikipedia article and no race or Hispanic category flag.")
}

# ---------------------------------------------------------------------------
# Exhibit output
# ---------------------------------------------------------------------------

# TRUE when `measure` is the primary measure set by choose_race_measure()
is_primary_measure <- function(measure) {
  measure == getOption("nfl.race_primary", "predicted")
}

# File stem by measure: the primary measure keeps the base name; any other
# measure gets a -<measure> suffix (e.g. -provisional, -preddoc)
exhibit_name <- function(name, measure) {
  if (is_primary_measure(measure)) name else paste0(name, "-", measure)
}

# Write a LaTeX table to tables_wd (and, for the primary measure only, to
# paper_tables) as <name>[-<measure>].tex
save_exhibit_tex <- function(tab, name, measure) {
  dirs <- if (is_primary_measure(measure)) c(tables_wd, paper_tables) else tables_wd
  file <- paste0(exhibit_name(name, measure), ".tex")
  for (d in dirs) writeLines(as.character(tab), file.path(d, file))
  message(glue("wrote {file}"))
  invisible(tab)
}

# Save a ggplot to figures_wd as <name>[-<measure>].pdf and .png
# (showtext renders text at its own dpi, so match it to the png dpi)
save_exhibit_figure <- function(plot, name, measure, width = 7, height = 4.5,
                                dpi = 300) {
  stem <- exhibit_name(name, measure)
  use_showtext <- requireNamespace("showtext", quietly = TRUE)
  for (ext in c("pdf", "png")) {
    if (use_showtext) showtext::showtext_opts(dpi = if (ext == "png") dpi else 96)
    ggsave(file.path(figures_wd, paste0(stem, ".", ext)), plot,
           width = width, height = height, dpi = dpi)
  }
  if (use_showtext) showtext::showtext_opts(dpi = 96)
  message(glue("wrote {stem}.pdf/.png"))
  invisible(plot)
}

# Tidy coefficients of `terms` from a named list of fixest models: one row per
# model x term with estimate, clustered SE, p-value, 95% CI, N and the model
# name (the table column); terms absent from a model are skipped
tidy_terms <- function(models, terms) {
  imap_dfr(models, \(m, name) {
    ct <- as.data.frame(coeftable(m))
    keep <- intersect(terms, rownames(ct))
    if (length(keep) == 0) return(tibble())
    ci <- confint(m)[keep, , drop = FALSE]
    tibble(model = name, term = keep, estimate = ct[keep, 1],
           std_error = ct[keep, 2], p_value = ct[keep, 4],
           ci_low = ci[, 1], ci_high = ci[, 2], nobs = nobs(m),
           dep_var = as.character(m$fml[[2]]))
  })
}

# Write a tidy estimates table to <output>/estimates/<name>[-provisional].csv
# (next to tables_wd), with the race measure as a column
save_estimates <- function(df, name, measure) {
  dir <- file.path(dirname(tables_wd), "estimates")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file <- paste0(exhibit_name(name, measure), ".csv")
  readr::write_csv(mutate(df, race_measure = measure), file.path(dir, file), na = "")
  message(glue("wrote estimates/{file}"))
  invisible(df)
}

# ---------------------------------------------------------------------------
# Regression tables
# ---------------------------------------------------------------------------

Stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

GofMap <- tribble(
  ~raw,            ~clean,           ~fmt,
  "nobs",          "Observations",   0,
  "r.squared",     "R$^2$",          3,
  "r2.within",     "Within R$^2$",   3
)

# modelsummary table of a named list of models (names become column headers),
# with the project's stars and goodness-of-fit rows, a \label{tab:<label>}
# in the title, and threeparttable notes that end with the race-measure note.
# Saved through save_exhibit_tex(); returns the kableExtra object.
write_model_table <- function(models, coef_map, title, label, notes, name,
                              measure, add_rows = NULL, font_size = NULL,
                              scale_down = TRUE, gof_map = GofMap,
                              design = "person") {
  caption <- paste0(title, if (!is_primary_measure(measure))
                      paste0(" (", measure, " race measure)"),
                    " \\label{tab:", label, "}")
  tab <- modelsummary(models, output = "latex", coef_map = coef_map,
                      gof_map = gof_map, stars = Stars, escape = FALSE,
                      add_rows = add_rows, title = caption) |>
    kable_styling(latex_options = c("hold_position", if (scale_down) "scale_down"),
                  font_size = font_size) |>
    add_notes(c(notes, race_measure_note(measure, design)))
  save_exhibit_tex(tab, name, measure)
}

# threeparttable notes for any kableExtra LaTeX table. Write notes as normal
# LaTeX in R strings ("\\textbf{x}", "$<$"); kableExtra's footnote() strips
# one level of backslashes, so they are doubled here.
add_notes <- function(tab, notes) {
  text <- gsub("\\", "\\\\", paste(notes, collapse = " "), fixed = TRUE)
  footnote(tab, general = text, general_title = "Notes:",
           footnote_as_chunk = TRUE, threeparttable = TRUE, escape = FALSE)
}

# ---------------------------------------------------------------------------
# Inference
# ---------------------------------------------------------------------------

# Wild cluster bootstrap-t (restricted, WCR) test of H0: coef(param) = 0 for a
# fixest OLS model estimated on `data`. Webb six-point weights suit few
# clusters (e.g. 32 franchises). fwildclusterboot 0.14 cannot read fixest
# 0.12 objects, so the model is refit by lm() on its estimation sample with
# each fixed effect as dummies (a^b as an interaction); the point estimate is
# identical. Not for models with varying slopes, weights or i() terms; the
# formula's right-hand side must be plain variables. `cluster` is a column
# name. Returns a one-row tibble with the bootstrap p-value and the
# confidence interval from test inversion.
wild_cluster_test <- function(model, data, param, cluster, B = 9999,
                              type = "webb", seed = 20261002) {
  if (!is.null(model$slope_flag) && any(model$slope_flag != 0)) {
    stop("wild_cluster_test: varying slopes are not supported")
  }
  fe_terms <- map_chr(model$fixef_vars %||% character(), \(f) {
    parts <- strsplit(f, "^", fixed = TRUE)[[1]]
    if (length(parts) == 1) glue("factor({f})") else
      glue("interaction({paste(parts, collapse = ', ')}, drop = TRUE)")
  })
  fml <- model$fml
  if (length(fe_terms) > 0) {
    fml <- update(fml, as.formula(paste(". ~ . +", paste(fe_terms, collapse = " + "))))
  }
  d <- as.data.frame(data)[obs(model), , drop = FALSE]
  # do.call embeds the data in the call, which boottest() re-evaluates
  refit <- do.call("lm", list(formula = fml, data = d))
  if (!isTRUE(all.equal(unname(coef(refit)[param]), unname(coef(model)[param]),
                        tolerance = 1e-6))) {
    stop("wild_cluster_test: lm refit does not reproduce the fixest estimate of ", param)
  }
  set.seed(seed)
  dqrng::dqset.seed(seed)
  bt <- suppressMessages(suppressWarnings(
    fwildclusterboot::boottest(refit, param = param, clustid = cluster, B = B,
                               type = type)))
  tibble(term = param, estimate = unname(coef(model)[param]),
         p_wcb = bt$p_val, ci_low = bt$conf_int[1], ci_high = bt$conf_int[2],
         n_clusters = unname(bt$N_G[1]), B = B)
}

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

# Zero-fill each variable and add a <var>Miss indicator (1 = was missing), so
# controls with incomplete coverage do not drop observations
fill_missing <- function(df, vars, suffix = "Miss") {
  for (v in vars) {
    df[[paste0(v, suffix)]] <- as.integer(is.na(df[[v]]))
    df[[v]] <- coalesce(as.numeric(df[[v]]), 0)
  }
  df
}

# Two-group (Black vs non-Black) Blau index from a Black share s: the
# probability that two members drawn with replacement differ, 2 s (1 - s)
blau_two_group <- function(s) 2 * s * (1 - s)

# p-value as text for add_rows ("" for NA)
fmt_p <- function(p, digits = 3) {
  if_else(is.na(p), "", formatC(p, format = "f", digits = digits))
}

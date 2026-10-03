# ============================================================================
# 12-table-pay-gap.R
# Estimates the pay-gap specifications of notes/analysis-plan.md, section 1
# ("Specifications"): conditional on every quality signal in the data, are
# Black NFL players paid more or less than white players at the same
# position? Equation (1), for contract c of player i at position p signed in
# year t:
#   Y_ipt = beta_1 Black_i + beta_2 Other_i + X_ipt pi + delta_{p,t} + e_ipt
# Y is log APY (OTC rounds apy_cap_pct to 0.001 of the cap; position x year
# FE absorb log cap_t, so log APY identifies the same beta_1 without the
# rounding). Main sample: freely bargained veteran contracts (UFA and
# extensions, BargainedMarket == 1); tags and tenders (CBA-formula pay) enter
# only the full veteran-market robustness columns of Table 10.
# Under the predicted race measures, Black_i and Other_i are the probabilities
# P(Black) and P(other race) (regression calibration, notes/race-prediction-
# design.md, section 4) and every specification adds the covariates of the
# race prior (race_prior_controls(), plus county availability) as fixed
# effects.
# Exhibits (-<measure> suffix under a non-primary race measure):
#   - table-07-pay-sumstats-by-race: veteran-contract means by race group
#     (probability-weighted under the predicted measures)
#   - table-08-pay-gap-veteran-contracts: progressive controls, columns (1)-(8)
#     (and, under the predicted measure, column (9): the raw benchmark on the
#     draft-free posterior, since the primary prior conditions on draft round)
#   - table-09-pay-gap-gelbach: Gelbach (2016) decomposition of b(1) - b(5)
#     with a player-cluster bootstrap (NFL_BOOT_REPS replications, default 199);
#     under the predicted measure on the draft-free posterior, so that draft
#     capital enters through block D
#   - table-10-pay-gap-terms-margins: guarantees, length, dollars, margins
#   - table-11-pay-gap-by-position + figure-pay-gap-by-position
#   - table-12-pay-gap-player-season + figure-pay-gap-by-experience
#     (log cap number; Altonji and Pierret 2001 signal x experience terms;
#     columns (9)-(11) locate the non-rookie gap by contract type)
#   - table-13-draft-margin-race: rookie (draft) margin
#   - table-26-pay-gap-birdie: BIRDiE cross-check (McCartan et al. 2025) of
#     the raw and conditional gaps, player-cluster bootstrap
#   - table-27-pay-gap-race-measures: columns (1) and (5) of table 08 under
#     every race measure and predicted-race variant
# Position-group slopes are built as EXPLICIT interaction columns from
# analysis/pay_control_blocks.csv, and categorical controls (ExperienceBin,
# MarketMargin, DraftRound) as dummy columns, so that each block's fitted
# contribution can be computed for the Gelbach decomposition.
# Race: one measure for the whole script, from choose_race_measure() on the
# hand-code coverage of the veteran-market contract sample (table 27 loops
# over the other measures without changing it).
# Inputs: analysis/analysis_pay_contracts.parquet,
# analysis/analysis_pay_player_season.parquet (10),
# analysis/pay_control_blocks.csv (10), analysis/draft_prospects.parquet (06),
# load_person_race() (DuckDB + data/hand_coded), and DuckDB race_predicted
# (read-only): the columns not yet in load_person_race() (county
# availability, raked, draft-free and Black-or-multiracial variants; attached
# by script 10, read here for older samples and for the draft prospects) and
# a check that the samples come from the current 04e fit.
# Outputs: output/tables/table-07 ... table-13, table-26, table-27 (.tex; also
# my_paper/tables for the primary measure), output/figures/
# figure-pay-gap-by-position and figure-pay-gap-by-experience (.pdf/.png),
# output/estimates/12-pay-gap[-<measure>].csv,
# 12-pay-gap-gelbach[-<measure>].csv, 12-pay-gap-birdie[-<measure>].csv and
# 12-pay-gap-race-measures[-<measure>].csv.
# Date: 2026-10-02
# ============================================================================

T0Script <- Sys.time()

# Forked workers for the wild cluster bootstrap and the BIRDiE bootstrap
# (NFL_CORES; default: up to 8, leaving two cores free; 1 on Windows). Each
# job sets its own seed or uses draws made up front, so results do not
# depend on the number of workers
Cores <- if (.Platform$OS.type == "windows") 1L else
  as.integer(Sys.getenv("NFL_CORES", max(1L, min(8L, parallel::detectCores() - 2L))))

# Elapsed-time message at the start of each section
tick <- function(label) {
  message(glue("12: {label} at {round(difftime(Sys.time(), T0Script, units = 'secs'))}s"))
}

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

PayContracts <- read_parquet(file.path(analysis, "analysis_pay_contracts.parquet"))
PayPanel <- read_parquet(file.path(analysis, "analysis_pay_player_season.parquet"))
ControlBlocks <- read_csv(file.path(analysis, "pay_control_blocks.csv"),
                          show_col_types = FALSE)
DraftProspects <- read_parquet(file.path(analysis, "draft_prospects.parquet"))

# Every dictionary variable must exist in its sample
stopifnot(all(filter(ControlBlocks, sample == "contracts")$variable %in% names(PayContracts)),
          all(filter(ControlBlocks, sample == "player_season")$variable %in% names(PayPanel)))

# ---------------------------------------------------------------------------
# Race measure (one for the whole script)
# ---------------------------------------------------------------------------

# Coverage = share of main-sample (UFA and extension) contracts with a hand
# code, computed before any restriction to known race
VeteranAll <- filter(PayContracts, BargainedMarket == 1)
HandCoverage <- mean(!is.na(VeteranAll$black_any))
measure <- choose_race_measure(HandCoverage, "12 pay gap")
PredMeasures <- c("predicted", "preddoc")
is_pred <- function(m) m %in% PredMeasures

# race_predicted columns that load_person_race() does not (yet) return, read
# directly (read-only): county availability (a covariate of the primary
# player prior), the preddoc prior's extra covariates (article found, career
# length), and the raked, draft-free and Black-or-multiracial variants of the
# posterior. Columns absent from the table are skipped
ExtraPredCols <- c(pred_county_available = "county_available",
                   pred_has_wiki = "has_wiki", pred_career_bucket = "career_bucket",
                   p_white_pred_raked = "p_white_pred_raked",
                   p_black_pred_raked = "p_black_pred_raked",
                   p_white_pred_nodraft = "p_white_pred_nodraft",
                   p_black_pred_nodraft = "p_black_pred_nodraft",
                   p_black_or_multi_pred = "p_black_or_multi_pred")
con <- db_connect()
PredExtras <- tibble(gsis_id = character())
if (dbExistsTable(con, "race_predicted")) {
  avail <- ExtraPredCols[ExtraPredCols %in% dbListFields(con, "race_predicted")]
  PredExtras <- tbl(con, "race_predicted") |>
    filter(entity == "player") |>
    select(person_uid, all_of(avail), db_p_black_any_pred = p_black_any_pred,
           db_pos_group = pos_group) |>
    collect() |>
    transmute(gsis_id = sub("^player:", "", person_uid),
              across(any_of(c("pred_county_available", "pred_has_wiki",
                              "pred_career_bucket")),
                     \(x) coalesce(as.character(x), "unknown")),
              across(starts_with("p_")),
              db_p_black_any_pred,
              db_pos_group = coalesce(as.character(db_pos_group), "unknown"))
}
db_disconnect(con)
stopifnot(!anyDuplicated(PredExtras$gsis_id))
message(glue("12: race_predicted extra columns: {paste(setdiff(names(PredExtras), 'gsis_id'), collapse = ', ')}"))

# Attach the extra columns a sample does not already carry (script 10 now
# attaches them; this covers older samples)
add_pred_extras <- function(df) {
  new <- setdiff(names(PredExtras), c(names(df), "db_p_black_any_pred", "db_pos_group"))
  left_join(df, PredExtras[c("gsis_id", new)], by = "gsis_id") |>
    mutate(across(any_of(intersect(new, c("pred_county_available", "pred_has_wiki",
                                          "pred_career_bucket"))),
                  \(x) coalesce(x, "unknown")))
}
# The sample's race columns (script 10's snapshot of race_predicted) and the
# columns read here must come from the same 04e fit: a refit of 04e without a
# rerun of script 10 would otherwise combine the posterior of one fit with the
# prior covariates and variants of another. Stop if they differ
check_pred_fit <- function(df, label) {
  if (!"db_p_black_any_pred" %in% names(PredExtras) || !"p_black_any_pred" %in% names(df)) return(invisible(df))
  chk <- left_join(select(df, gsis_id, p_black_any_pred, any_of("pred_pos_group")),
                   select(PredExtras, gsis_id, db_p_black_any_pred, db_pos_group), by = "gsis_id")
  gap <- max(abs(chk$p_black_any_pred - chk$db_p_black_any_pred), na.rm = TRUE)
  pos_bad <- if ("pred_pos_group" %in% names(chk)) sum(chk$pred_pos_group != coalesce(chk$db_pos_group, "unknown")) else 0L
  if (gap > 1e-9 || pos_bad > 0) {
    stop(glue("12: {label}: race columns do not match the current race_predicted ",
              "(max |p_black_any_pred diff| {signif(gap, 3)}, pred_pos_group differs in {pos_bad} rows); ",
              "rerun programs/10-pay-analysis-sample.R"))
  }
  invisible(df)
}
check_pred_fit(PayContracts, "pay contracts")
check_pred_fit(PayPanel, "pay player-seasons")
PayContracts <- add_pred_extras(PayContracts)
PayPanel <- add_pred_extras(PayPanel)

# Race regressors Black, OtherRace, PWhite and RaceKnown under the chosen
# measure (indicators under hand/provisional, probabilities under the
# predicted measures)
add_race <- function(df, m = measure) person_race_regressors(df, m)
PayContracts <- add_race(PayContracts)
PayPanel <- add_race(PayPanel)

# Draft-free posterior (prior without the draft bucket), used under the
# predicted measure wherever the specification omits draft slot (table 08
# column (8), table 13): the primary prior conditions on draft round, which
# the draft-margin outcome and column (8)'s omitted controls record
HasNoDraft <- all(c("p_black_pred_nodraft", "p_white_pred_nodraft") %in% names(PredExtras))
UseNoDraft <- measure == "predicted" && HasNoDraft
nodraft_race <- function(df) {
  df |> mutate(Black = p_black_pred_nodraft, PWhite = p_white_pred_nodraft,
               OtherRace = pmax(1 - Black - PWhite, 0), RaceKnown = !is.na(Black))
}

# Fixed effects for the covariates of the race prior under measure m:
# race_prior_controls() plus county availability (in the primary player prior
# since the 2026-10-02 refit) and, for preddoc, its article and career-length
# covariates; draft = FALSE drops the draft bucket (draft-free posterior).
# Only columns present in df are used
prior_fe_vars <- function(m, df, draft = TRUE) {
  v <- race_prior_controls(m, "player")
  if (is_pred(m)) v <- c(v, "pred_county_available")
  if (m == "preddoc") v <- c(v, "pred_has_wiki", "pred_career_bucket")
  if (!draft) v <- setdiff(v, "pred_draft_bucket")
  intersect(v, names(df))
}
PriorFE <- prior_fe_vars(measure, PayContracts)
stopifnot(!is_pred(measure) || length(PriorFE) >= 4)

# The prior-covariate FE enter as dummy columns Prior_<var>_<level>
# (reference: the modal level) rather than as fixest FE, so that the only
# absorbed FE is position x year: one-way demeaning is exact, whereas
# fixest's alternating projections with several FE stop at a tolerance,
# which breaks the Gelbach adding-up identity and the lm refit of the wild
# cluster bootstrap. add_prior_dummies() uses add_dummies() (defined below)
PriorVarsAll <- c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket",
                  "pred_college_type", "pred_county_available", "pred_has_wiki",
                  "pred_career_bucket")
add_prior_dummies <- function(df) {
  for (v in intersect(PriorVarsAll, names(df))) {
    df <- add_dummies(df, v, paste0("Prior_", v))$df
  }
  df
}
# Prior dummy columns for measure m that vary in the estimation sample df
prior_cols <- function(m, df, draft = TRUE) {
  vars <- prior_fe_vars(m, df, draft)
  cols <- unlist(map(vars, \(v) grep(paste0("^Prior_", v, "_"), names(df), value = TRUE)))
  varying(df, cols %||% character())
}

# Labels: "Black" for indicators, "P(Black)" for probabilities
BlackLab <- if (is_pred(measure)) "P(Black)" else "Black"
OtherLab <- if (is_pred(measure)) "P(other race)" else "Other race"
RefNote <- race_reference_note(measure)
PriorNote <- if (is_pred(measure)) {
  paste0("Every column also includes fixed effects for the covariates of the race prediction's prior ",
         "(position at NFL entry, entry era, draft-round bucket, college type and whether a home county is known",
         if (measure == "preddoc") ", plus whether a Wikipedia article was found and career length, which are outcomes of the career",
         "), as regression calibration requires.")
} else ""
# Definition and level calibration of the predicted P(Black): the primary
# posterior is non-Hispanic Black alone (p_black_any_pred = p_black_pred) and
# is not calibrated in levels (04e; Table 24)
PredDefNote <- if (is_pred(measure)) {
  paste("P(Black) is the predicted probability of being non-Hispanic Black alone; Black Hispanic and multiracial Black players enter P(other race), and Table \\ref{tab:pay-gap-race-measures} also reports estimates with P(Black) + P(multiracial) as the Black regressor.",
        "The predicted Black share is not calibrated in levels: it lies below the TIDES African-American player share through 2016 and above the self-identified Black-alone share (below the Black-or-multiracial share) from 2019 (Table \\ref{tab:race-prediction-tides}); Table \\ref{tab:pay-gap-race-measures} reports estimates with the TIDES-raked posterior.",
        "Standard errors treat the predicted probabilities as known; the prior is estimated by EM on the whole player population of the database, so the omitted first-stage variance is likely small.")
} else ""
# Conditions of regression calibration beyond the names exclusion, for tables
# whose columns add controls to the prior covariates (the diagnostic sentence
# is added where it is computed, in table 08)
RCNote <- if (is_pred(measure)) {
  paste("Regression calibration identifies the Black-white gap only if, in addition to names and hometown being unrelated to pay given race and the controls, (i) P(Black) is calibrated given every control in the column, not only given the prior's covariates, and (ii) the gap does not vary with the controls (otherwise the coefficient is a variance-weighted average of gaps).",
        "Controls that predict race beyond the prior violate (i) and attenuate the coefficient on P(Black) even when they do not affect pay, so part of any change in the coefficient as controls are added is mechanical.")
} else ""

# Text for "known race" by measure: under the provisional measure a person has
# a race group exactly when he has a Wikipedia article; under the predicted
# measures every player with a name has a prediction
KnownRace <- switch(measure, hand = "known (hand-coded) race",
                    provisional = "a Wikipedia article (race classified by the provisional measure)",
                    "a race prediction")
UnknownRace <- switch(measure, hand = "unknown race", provisional = "no Wikipedia article",
                      "no race prediction")

# Rows dropped for unknown race, reported for each estimation sample
DroppedRace <- list()
keep_known_race <- function(df, label) {
  n_na <- sum(!df$RaceKnown)
  DroppedRace[[label]] <<- c(n = nrow(df), dropped = n_na)
  message(glue("12: {label}: dropping {n_na} of {nrow(df)} rows with unknown race"))
  filter(df, RaceKnown)
}

# Drop empty note strings (they would leave double spaces in the notes)
drop_empty <- function(x) x[!is.na(x) & x != ""]


# ---------------------------------------------------------------------------
# Design helpers: explicit slope interactions, dummies, formulas
# ---------------------------------------------------------------------------

# Syntactic tag for a factor level ("4-6" -> "4_6", "7+" -> "7plus")
level_tag <- function(x) {
  x |> str_replace_all("\\+", "plus") |> str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_remove("_$")
}

# Dummy columns <prefix>_<level> for every level of `var` except the
# reference (the modal level unless `ref` is given); returns list(df, cols)
add_dummies <- function(df, var, prefix, ref = NULL) {
  x <- as.character(df[[var]])
  levs <- sort(unique(x[!is.na(x)]))
  if (is.null(ref)) ref <- names(which.max(table(x)))
  cols <- character()
  for (l in setdiff(levs, ref)) {
    nm <- paste0(prefix, "_", level_tag(l))
    df[[nm]] <- as.integer(coalesce(x == l, FALSE))
    cols <- c(cols, nm)
  }
  list(df = df, cols = cols)
}

# Build the control design for `dict_sample` ("contracts" or "player_season")
# from pay_control_blocks.csv: for each variable and each listed group g a
# column x * (PositionGroup == g) named <x>_x_<g> ('all' = every group present
# in df; 'none' = x enters once, linearly), plus the categorical dummies in
# `cat_vars` (a named list: block -> variables). `dict` defaults to the pay
# control dictionary (any tibble with the same columns works). Returns list(df, cols) where
# cols is a named list of column names by block.
build_design <- function(df, dict_sample, blocks = c("A", "B", "C", "D", "E", "F"),
                         cat_vars = list(), dict = ControlBlocks) {
  dict <- filter(dict, sample == dict_sample, block %in% blocks)
  groups <- sort(unique(df$PositionGroup))
  cols <- list()
  new <- list()
  for (b in blocks) {
    rows <- filter(dict, block == b)
    bc <- character()
    for (i in seq_len(nrow(rows))) {
      v <- rows$variable[i]
      sg <- rows$slope_groups[i]
      if (sg == "none") {
        bc <- c(bc, v)
        next
      }
      gs <- if (sg == "all") groups else intersect(strsplit(sg, ";")[[1]], groups)
      for (g in gs) {
        nm <- paste0(v, "_x_", g)
        new[[nm]] <- as.numeric(df[[v]]) * (df$PositionGroup == g)
        bc <- c(bc, nm)
      }
    }
    # Categorical controls of the block as dummies
    for (cv in cat_vars[[b]] %||% character()) {
      d <- add_dummies(df, cv, cv)
      df <- d$df
      bc <- c(bc, d$cols)
    }
    cols[[b]] <- bc
  }
  df <- bind_cols(df, as_tibble(new))
  list(df = df, cols = cols)
}

# Drop columns that are constant in the estimation sample
varying <- function(df, cols) cols[map_lgl(cols, \(v) n_distinct(df[[v]]) > 1)]

# Formula y ~ rhs | fe (fe = "" for none)
make_fml <- function(y, rhs, fe = "") {
  f <- paste(y, "~", paste(rhs, collapse = " + "))
  if (fe != "") f <- paste(f, "|", fe)
  as.formula(f)
}

# Number formatting for hand-built tables
fmt_pg <- function(x, digits = 3) {
  if_else(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}
fmt_se_pg <- function(x, digits = 3) if_else(is.na(x), "", paste0("(", fmt_pg(x, digits), ")"))

# ---------------------------------------------------------------------------
# Veteran-contract sample (main sample) and its control design
# ---------------------------------------------------------------------------

# Categorical controls entered as dummies: block A career stage, block D
# draft round
ContractCats <- list(A = c("ExperienceBin", "MarketMargin"), D = "DraftRound")

# Outcome of equation (1): log APY ($ millions). OTC's apy_cap_pct is rounded
# to 0.001 of the cap; the position x year-signed FE absorb log cap_t, so log
# APY gives the same beta_1 as log(APY / cap_t) without the rounding error
YVet <- "LogAPY"

# Prior-covariate dummy columns (used only under the predicted measures)
PayContracts <- add_prior_dummies(PayContracts)
PayPanel <- add_prior_dummies(PayPanel)

# Main sample: freely bargained veteran contracts (UFA and extensions), known
# race. Tags and tenders (CBA-formula pay; ERFA tenders are minimum deals)
# are left to the full veteran-market columns of Table 10
Veteran <- PayContracts |>
  filter(BargainedMarket == 1) |>
  keep_known_race("veteran contracts")

# Control design (explicit slope interactions and dummies)
VetDesign <- Veteran |>
  mutate(DraftRound = as.character(DraftRound)) |>
  build_design("contracts", cat_vars = ContractCats)
VetData <- VetDesign$df |> filter(!is.na(.data[[YVet]]))
VetCols <- map(VetDesign$cols, \(cc) varying(VetData, cc))
message(glue("12: veteran estimation sample {nrow(VetData)} contracts; ",
             "controls by block: {paste(names(VetCols), lengths(VetCols), sep = '=', collapse = ', ')}"))

# ---------------------------------------------------------------------------
# Table 07: veteran-contract means by race group
# ---------------------------------------------------------------------------

tick("table 07")

# Variables (NA where unknown, so means use known values only)
SumData <- Veteran |>
  mutate(ApyCapPct100 = 100 * apy_cap_pct,
         GuaranteeShareKnown = if_else(GuaranteedZero == 0, GuaranteeShare, NA_real_),
         AgeKnown = if_else(AgeAtSigningMiss == 0, AgeAtSigning, NA_real_),
         PriorOffDefSnaps = PriorOffSnaps + PriorDefSnaps,
         Round12 = as.integer(DraftRound %in% 1:2),
         Rating247 = if_else(RecruitRatingMiss == 0, RecruitRating, NA_real_),
         FortyKnown = if_else(FortyMiss == 0, Forty, NA_real_))

SumVars <- tribble(
  ~var,                  ~label,                                   ~digits,
  "apy",                 "APY (\\$ millions)",                      2,
  "ApyCapPct100",        "APY share of cap (\\%)",                  2,
  "GuaranteeShareKnown", "Guarantee share (known guarantees)",      3,
  "years",               "Contract length (years)",                2,
  "AgeKnown",            "Age at signing",                         2,
  "ExperienceAtSigning", "Experience (seasons)",                   2,
  "PriorGamesPlayed",    "Prior-season games played",              2,
  "PriorOffDefSnaps",    "Prior-season offense + defense snaps",   0,
  "Round12",             "Drafted in rounds 1-2",                  3,
  "Undrafted",           "Undrafted",                              3,
  "Rating247",           "247 composite rating",                   3,
  "FortyKnown",          "40-yard dash (seconds)",                 2
)

# Group means: means weighted by the race regressors (0/1 indicators give the
# group means; under the predicted measures, P(Black), P(white) and P(other
# race) give probability-weighted means). Black - White difference: the
# coefficient on Black in a regression on Black and OtherRace (SE clustered by
# player); under the predicted measures with position x year-signed FE and the
# prior-covariate FE (regression calibration), else without controls, so it
# equals the difference between the first two columns
FE07 <- if (is_pred(measure)) "position^year_signed" else ""
Prior07 <- prior_cols(measure, SumData)
Prior07NoDraft <- prior_cols(measure, SumData, draft = FALSE)
DraftVars07 <- c("Round12", "Undrafted")
SumRows <- pmap_dfr(SumVars, \(var, label, digits) {
  x <- SumData[[var]]
  ok <- !is.na(x)
  means <- map_dbl(c("Black", "PWhite", "OtherRace"),
                   \(w) weighted.mean(x[ok], SumData[[w]][ok]))
  # Draft outcomes are absorbed by the prior's draft-round bucket: use the
  # draft-free posterior and its prior instead
  draft_var <- UseNoDraft && var %in% DraftVars07
  fit <- feols(make_fml(var, c("Black", "OtherRace", if (draft_var) Prior07NoDraft else Prior07), FE07),
               data = if (draft_var) nodraft_race(SumData) else SumData,
               vcov = ~gsis_id, notes = FALSE)
  ct <- coeftable(fit)
  tibble(Variable = label,
         Black = fmt_pg(means[1], digits), White = fmt_pg(means[2], digits),
         Other = fmt_pg(means[3], digits),
         Diff = fmt_pg(ct["Black", 1], digits), SE = fmt_se_pg(ct["Black", 2], digits),
         estimate = ct["Black", 1], std_error = ct["Black", 2], term = var)
})
# Counts (expected counts under the predicted measures): sums of the weights
# over contracts and over players
player_sum <- function(w) sum(distinct(SumData, gsis_id, .data[[w]])[[w]])
CountRows <- tibble(
  Variable = paste(if (is_pred(measure)) "Expected N" else "N", c("contracts", "players")),
  Black = c(sum(SumData$Black), player_sum("Black")),
  White = c(sum(SumData$PWhite), player_sum("PWhite")),
  Other = c(sum(SumData$OtherRace), player_sum("OtherRace"))) |>
  mutate(across(c(Black, White, Other), \(x) fmt_pg(x, 0)), Diff = "", SE = "")

# Column headers of the race groups under the chosen measure
RaceHeaders <- switch(measure,
  hand = c(Black = "Black", White = "White", Other = "Other"),
  provisional = c(Black = "Flagged Black", White = "Not flagged", Other = "Other flag"),
  c(Black = "Black (P-weighted)", White = "White (P-weighted)", Other = "Other (P-weighted)"))
# Attenuation factor of two-group probability-weighted differences when names
# are unrelated to the variable given race: Var(p) / (pbar (1 - pbar))
Shrink07 <- var(SumData$Black) / (mean(SumData$Black) * (1 - mean(SumData$Black)))
DiffNote07 <- if (is_pred(measure)) {
  paste("Under the predicted race measure, the group columns are means weighted by each player's P(Black), P(white) and P(other race), and the counts are expected counts (sums of the probabilities).",
        glue("Weighted means are consistent for E[Y $|$ race] only if the variable is unrelated to race given names, hometown and the prior covariates; if instead names are unrelated to the variable given race, they shrink the group difference toward zero (by a factor of about {fmt_pg(Shrink07, 2)} here, the variance of P(Black) over its Bernoulli maximum), and a variable related to the prior covariates (draft round, position) also moves them. They are descriptive only (Table \\ref{{tab:pay-gap-birdie}} reports BIRDiE estimates of E[Y $|$ race])."),
        "The difference column is the coefficient on P(Black) in a regression of the variable on P(Black) and P(other race) with OTC position $\\times$ year-signed fixed effects and fixed effects for the covariates of the race prior (position at NFL entry, entry era, draft-round bucket, college type, home county known), i.e. the regression-calibration estimate of the Black-white difference within position and year; it therefore need not equal the difference between the first two columns.",
        if (UseNoDraft) "For the two draft rows, which the draft-round bucket would absorb, the regression uses the draft-free prediction and omits the bucket from the prior fixed effects." else "",
        "Standard errors are clustered at the player level.")
} else {
  paste("The difference column is the coefficient on the Black indicator in a regression of the variable on Black and Other indicators, so it equals the difference between the first two columns.",
        "Standard errors are clustered at the player level.")
}
SampleNote07 <- if (DroppedRace[["veteran contracts"]][["dropped"]] > 0) {
  glue("The sample excludes {DroppedRace[['veteran contracts']][['dropped']]} contracts of players with {UnknownRace}.")
} else "Every contract in the sample has a race measure."

SumTable <- bind_rows(select(SumRows, Variable, Black, White, Other, Diff, SE),
                      CountRows)
Tab07 <- kbl(SumTable, format = "latex", booktabs = TRUE, escape = FALSE,
             linesep = "", align = "lrrrrr",
             col.names = c("", RaceHeaders[["Black"]], RaceHeaders[["White"]],
                           RaceHeaders[["Other"]],
                           if (is_pred(measure)) "Black $-$ White" else
                             paste(RaceHeaders[["Black"]], "$-$", RaceHeaders[["White"]]),
                           "SE"),
             caption = paste0("Veteran contracts: means by race group",
                              if (!is_primary_measure(measure)) paste0(" (", measure, " race measure)"),
                              " \\label{tab:pay-sumstats-race}")) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_notes(drop_empty(c(
    glue("This table reports means of freely bargained veteran contracts (unrestricted free ",
         "agents, including those who re-sign with their own team, and extensions) signed 2014-2026 by ",
         "race group, among players with {KnownRace}. Tags and tenders are excluded. {SampleNote07}"),
    "APY is the average annual value; the cap share is OTC's APY over the league salary cap of the signing year, which OTC rounds to 0.1\\% of the cap (the rounding is immaterial for these means).",
    "The guarantee share is guaranteed over total value, among contracts with nonzero recorded guarantees (OTC records unknown guarantees as zero).",
    "Prior-season games and snaps refer to year signed minus one and are zero for players not on a roster that season; snaps start in 2013.",
    "The 247 rating and the 40-yard dash are means over players with a linked recruit profile or combine time.",
    DiffNote07,
    PredDefNote,
    RefNote,
    race_measure_note(measure, "person"))))
save_exhibit_tex(Tab07, "table-07-pay-sumstats-by-race", measure)

# Estimates collected for the tidy coefficient file
Estimates <- list(SumRows |> transmute(table = "table-07",
                                       model = if_else(UseNoDraft & term %in% DraftVars07,
                                                       "Black - White (draft-free posterior)",
                                                       "Black - White"),
                                       term, estimate, std_error,
                                       nobs = nrow(SumData)))

# ---------------------------------------------------------------------------
# Table-note text shared by the regression tables
# ---------------------------------------------------------------------------

BlockNotes <- c(
  A = "Career stage (A): experience-bin dummies (0, 1, 2, 3, 4-6, 7+ seasons), age at signing and its square, a left-censored-experience indicator and an extension dummy (reference: unrestricted free agents). Whether a team extends a player is itself a team decision.",
  B = "Prior-season production (B): season-before-signing box-score and PFR measures with position-group-specific slopes (passing for QBs, rushing and receiving for RBs, receiving for WRs and TEs, tackles, sacks, QB hits, tackles for loss and pressures for DL and LB, tackles, interceptions, passes defended and coverage allowed for DBs, kicking and punting), games played and injury weeks interacted with position group, and a no-prior-season indicator.",
  C = "Career production (C): career games and injury weeks interacted with position group, and career sums of the block B production measures with the same position-group slopes.",
  D = "Pre-NFL signals (D): log draft pick, an undrafted indicator, draft-round dummies, 247 rating and stars, combine measures (40, vertical, bench, broad jump, cone, shuttle, height, weight) and athletic scores (a composite and size, speed, explosion and agility subscores built from the combine measures) interacted with position group, and the final college team's Power-conference, SRS and HBCU indicators.",
  E = "Usage (E): prior-season offensive, defensive and special-teams snaps, prior-season games started, career depth-chart starts and career snaps, interacted with position group. Snaps and starts are chosen by coaches, so the usage controls may absorb discrimination in playing time.",
  Miss = "Controls with incomplete coverage are set to zero with a missing indicator (interacted like the control).",
  Volume = "Games played and the box-score volume counts (attempts, targets, tackles) also depend on playing time, and draft slot and the extension dummy record earlier team decisions, so columns (3)-(5) estimate a gap conditional on earlier allocation decisions, which discrimination could also affect.",
  Identify = "The identifying assumption is that, conditional on the controls and fixed effects, race is uncorrelated with unobserved productivity. Omitted quality would bias the estimate: there are no PFF-type grades, and offensive linemen have no production measure, so their quality rests on games, injury weeks and pre-NFL signals."
)
NoteCluster <- "Standard errors are clustered at the player level."

# Implied percent gap 100 (exp(b) - 1)
pct_gap <- function(b) 100 * (exp(b) - 1)

# ---------------------------------------------------------------------------
# Table 08: veteran-contract pay gap, progressive controls (equation (1))
# ---------------------------------------------------------------------------

tick("table 08")

Rhs0 <- c("Black", "OtherRace")
CoefMap <- c(Black = BlackLab, OtherRace = OtherLab)
# Position x year-signed FE; under the predicted measures the prior-covariate
# dummies (all of them, or without the draft bucket for the draft-free
# posterior) enter every column
FE08 <- "position^year_signed"
PriorCols08 <- prior_cols(measure, VetData)
PriorCols08NoDraft <- prior_cols(measure, VetData, draft = FALSE)
# Column (8): column (5) without the controls that record earlier team
# decisions (draft slot, draft round, the extension dummy)
DecisionCols <- c("LogDraftPick", "LogDraftPickMiss", "Undrafted",
                  grep("^(DraftRound|MarketMargin)_", unlist(VetCols, use.names = FALSE),
                       value = TRUE))
Spec08 <- list(
  "(1)" = list(blocks = character(), fe = FE08),
  "(2)" = list(blocks = "A", fe = FE08),
  "(3)" = list(blocks = c("A", "B"), fe = FE08),
  "(4)" = list(blocks = c("A", "B", "C"), fe = FE08),
  "(5)" = list(blocks = c("A", "B", "C", "D"), fe = FE08),
  "(6)" = list(blocks = c("A", "B", "C", "D", "E"), fe = FE08),
  "(7)" = list(blocks = c("A", "B", "C", "D", "E"),
               fe = paste(FE08, "+ SigningFranchise^year_signed")),
  "(8)" = list(blocks = c("A", "B", "C", "D"), drop = DecisionCols,
               fe = FE08, nodraft = UseNoDraft))
# Under the predicted measure, column (9) is the raw benchmark: the draft-free
# posterior with its prior's covariates and position x year FE only, so no
# regressor records the draft (column (1) conditions on the draft-round
# bucket through the primary prior)
if (UseNoDraft) Spec08[["(9)"]] <- list(blocks = character(), fe = FE08, nodraft = TRUE)
# Under the predicted measure, column (8) uses the draft-free posterior and
# drops the draft bucket from the prior FE, so no regressor records the draft
Models08 <- map(Spec08, \(s) {
  controls <- setdiff(unlist(VetCols[s$blocks], use.names = FALSE), s$drop %||% character())
  d <- if (isTRUE(s$nodraft)) nodraft_race(VetData) else VetData
  prior <- if (isTRUE(s$nodraft)) PriorCols08NoDraft else PriorCols08
  feols(make_fml(YVet, c(Rhs0, prior, controls), s$fe),
        data = d, vcov = ~gsis_id, notes = FALSE)
})
stopifnot(all(map_int(Models08, nobs) == nrow(VetData)))

# Yes/No rows for the blocks, the outcome mean and the implied percent gap
yes_no <- function(cond) if_else(cond, "Yes", "No")
Rows08 <- tibble(
  term = c("Position $\\times$ year FE", "Career stage (A)", "Prior-season production (B)",
           "Career production (C)", "Pre-NFL signals (D)", "Usage (E)",
           "Signing team $\\times$ year FE", "Draft slot, round, extension dummy",
           "Race-prior covariate FE", "Mean of outcome", "Implied Black gap (\\%)"))
for (nm in names(Spec08)) {
  spec <- Spec08[[nm]]
  Rows08[[nm]] <- c("Yes", yes_no(c("A", "B", "C", "D", "E") %in% spec$blocks),
                    yes_no(str_detect(spec$fe, "SigningFranchise")),
                    if (length(spec$blocks) == 0) "No" else
                      yes_no(is.null(spec$drop) && "D" %in% spec$blocks),
                    if (!is_pred(measure)) "No" else
                      if (isTRUE(spec$nodraft)) "No draft" else "Yes",
                    fmt_pg(mean(VetData[[YVet]]), 3),
                    fmt_pg(pct_gap(coef(Models08[[nm]])[["Black"]]), 1))
}
# Columns (2)-(4) have the extension dummy (block A) but not draft slot (block
# D); under the predicted measures columns (1)-(4) condition on the draft-round
# bucket through the prior FE
DraftCell <- if (is_pred(measure)) c("Bucket (prior)", "Ext. + bucket") else c("No", "Ext. only")
Rows08[["(1)"]][8] <- DraftCell[1]
for (nm in c("(2)", "(3)", "(4)")) Rows08[[nm]][8] <- DraftCell[2]
# Standard error of the race coefficient with two-way clustering by player and
# signing franchise (teams set pay; robustness of the player clustering)
se_twoway <- function(m) {
  se(summary(m, vcov = ~gsis_id + SigningFranchise))[["Black"]]
}
Rows08 <- bind_rows(Rows08, as_tibble(c(
  list(term = paste("SE of", BlackLab, "clustered by player and signing team")),
  map(Models08, \(m) fmt_se_pg(se_twoway(m), 3)))))

tick("table 08 models and two-way SEs done")
# Precision of the main estimate: 95% CI of column (5) and, under the
# predicted measures, the residual SD of P(Black) given the fixed effects and
# prior dummies (the identifying variation)
CI5 <- confint(Models08[["(5)"]])["Black", ]
ResidSD08 <- if (is_pred(measure)) {
  sd(resid(feols(make_fml("Black", PriorCols08, FE08), data = VetData, notes = FALSE)))
} else NA_real_
PrecisionNote08 <- paste0(
  glue("The 95\\% confidence interval for {BlackLab} in column (5) is [{fmt_pg(CI5[[1]], 3)}, {fmt_pg(CI5[[2]], 3)}] log points"),
  if (is_pred(measure)) glue("; the residual standard deviation of P(Black) given the position $\\times$ year and prior fixed effects is {fmt_pg(ResidSD08, 3)}, so the estimates are imprecise") else "",
  ".")

# Diagnostic of condition (i) of regression calibration: do the column-(5)
# controls predict P(Black) within position x year x prior-covariate cells?
# Joint Wald tests (player-clustered) of all block A-D controls and of block D
CalibDiag <- NULL
if (is_pred(measure)) {
  Cols5Diag <- unlist(VetCols[c("A", "B", "C", "D")], use.names = FALSE)
  DiagFit <- feols(make_fml("Black", c(PriorCols08, Cols5Diag), FE08), data = VetData,
                   vcov = ~gsis_id, notes = FALSE)
  # The clustered covariance of several hundred coefficients is singular
  # (fewer independent cluster scores than restrictions in some directions),
  # so the Wald statistic uses its pseudo-inverse on the scaled matrix; F has
  # rank(V) and G - 1 degrees of freedom (G = player clusters)
  NClust08 <- n_distinct(VetData$gsis_id)
  diag_wald <- function(cols) {
    kept <- intersect(cols, names(coef(DiagFit)))
    b <- coef(DiagFit)[kept]
    V <- vcov(DiagFit)[kept, kept]
    sc <- 1 / sqrt(diag(V))
    e <- eigen(V * outer(sc, sc), symmetric = TRUE)
    pos <- e$values > 1e-8 * max(e$values)
    stat <- sum(drop(crossprod(e$vectors[, pos], b * sc))^2 / e$values[pos])
    r <- sum(pos)
    f <- stat / r
    tibble(n_terms = length(kept), rank = r, stat = f,
           p = pf(f, r, NClust08 - 1, lower.tail = FALSE))
  }
  CalibDiag <- bind_rows(all = diag_wald(Cols5Diag),
                         blockD = diag_wald(VetCols[["D"]]), .id = "set")
  print(CalibDiag)
  tick("table 08 calibration diagnostic done")
}
DiagNote <- if (!is.null(CalibDiag)) {
  glue("Within position $\\times$ year and prior-covariate cells the column-(5) controls predict P(Black): a joint Wald test of the block A-D controls in a regression of P(Black) on them gives F = {fmt_pg(CalibDiag$stat[1], 2)} (p = {fmt_p(CalibDiag$p[1])}), and of the block D controls alone F = {fmt_pg(CalibDiag$stat[2], 2)} (p = {fmt_p(CalibDiag$p[2])}) (player-clustered covariance, inverted by pseudo-inverse; numerator degrees of freedom are its rank, {CalibDiag$rank[1]} of {CalibDiag$n_terms[1]} and {CalibDiag$rank[2]} of {CalibDiag$n_terms[2]} coefficients).")
} else ""

# Note on column (8)'s race measure under the predicted measures
NoDraftNote08 <- if (UseNoDraft) {
  "In column (8), P(Black) and P(other race) come from the draft-free prediction (a prior without the draft-round bucket), and the draft-round bucket is dropped from the prior fixed effects, so no regressor records the draft."
} else if (is_pred(measure)) {
  "Column (8) keeps the draft-round bucket among the prior fixed effects, because the draft-free prediction is not available for this measure; it therefore still conditions on draft round."
} else ""

write_model_table(
  Models08, CoefMap,
  title = "Race gap in veteran-contract pay",
  label = "pay-gap-veteran",
  notes = drop_empty(c(
    glue("This table includes the estimation results of equation (1). The outcome is log APY (\\$ millions); ",
         "with OTC position $\\times$ year-signed fixed effects this equals log APY over the league salary cap ",
         "of the signing year, without OTC's rounding of the cap share. The sample is freely bargained veteran ",
         "contracts (unrestricted free agents, including those who re-sign with their own team, and extensions) ",
         "signed 2014-2026 by players with {KnownRace}; tags and tenders, whose pay is set by a CBA formula, ",
         "are in Table \\ref{{tab:pay-gap-terms}}. {DroppedRace[['veteran contracts']][['dropped']]} contracts ",
         "of players with {UnknownRace} and {sum(is.na(Veteran[[YVet]]))} with missing APY are excluded."),
    "Every column includes OTC position (18 market positions) $\\times$ year-signed fixed effects; column (7) adds signing franchise $\\times$ year-signed fixed effects. Column (8) is column (5) without draft slot, draft-round dummies and the extension dummy.",
    PriorNote, NoDraftNote08,
    if (UseNoDraft) "Because the prior's covariates include the draft-round bucket, columns (1)-(4) already condition on draft round and column (1) is not a raw gap. Column (9) is the raw benchmark: P(Black) and P(other race) from the draft-free prediction, with its prior's covariates (no draft bucket) and position $\\times$ year-signed fixed effects only; the difference between columns (9) and (1) is the part of the raw gap that conditioning on draft round removes." else "",
    BlockNotes[c("A", "B", "C", "D", "E", "Miss", "Volume", "Identify")],
    RCNote, DiagNote, PredDefNote,
    "Column (5) is the main specification. The implied gap is $100(e^{\\hat\\beta_1}-1)$.",
    PrecisionNote08,
    NoteCluster,
    glue("The row 'SE of {BlackLab} clustered by player and signing team' reports the standard error of the race coefficient with two-way clustering by player and signing franchise (teams set pay)."),
    RefNote)),
  name = "table-08-pay-gap-veteran-contracts", measure = measure,
  add_rows = Rows08)
Estimates <- c(Estimates, list(tidy_terms(Models08, Rhs0) |> mutate(table = "table-08")))

# ---------------------------------------------------------------------------
# Table 09: Gelbach (2016) decomposition of b(1) - b(5)
# ---------------------------------------------------------------------------

tick("table 09")

# Decomposition on data d: b(1) from Y on Black, OtherRace and the base FE;
# b(5) from the column-(5) model; delta_k = coefficient on Black from
# regressing H_k = X_k beta_k (beta_k from column (5)) on the column-(1)
# regressors. The deltas sum to b(1) - b(5) on the same sample.
GelbachBlocks <- c("A", "B", "C", "D")
gelbach <- function(d, cols = VetCols[GelbachBlocks], fe = FE08, prior = PriorCols08) {
  base <- feols(make_fml(YVet, c(Rhs0, prior), fe), data = d, vcov = "iid",
                notes = FALSE, warn = FALSE)
  full <- feols(make_fml(YVet, c(Rhs0, prior, unlist(cols, use.names = FALSE)), fe),
                data = d, vcov = "iid", notes = FALSE, warn = FALSE)
  b <- coef(full)
  # Fitted contribution of each block (collinear columns dropped by fixest
  # contribute zero)
  for (k in names(cols)) {
    cc <- intersect(cols[[k]], names(b))
    d[[paste0("H_", k)]] <- if (length(cc) == 0) 0 else
      drop(as.matrix(d[cc]) %*% b[cc])
  }
  aux <- feols(as.formula(paste0("c(", paste0("H_", names(cols), collapse = ", "),
                                 ") ~ ", paste(c(Rhs0, prior), collapse = " + "), " | ", fe)),
               data = d, vcov = "iid", notes = FALSE, warn = FALSE)
  delta <- map_dbl(seq_along(names(cols)), \(i) coef(aux[[i]])[["Black"]])
  c(b1 = coef(base)[["Black"]], b5 = b[["Black"]], set_names(delta, names(cols)))
}

# Base of the decomposition. Under the predicted measure it uses the
# draft-free posterior and its prior (no draft bucket) in both b(1) and b(5):
# the primary prior conditions on the draft-round bucket, so with the primary
# posterior b(1) would already condition on draft round and the draft-capital
# part of the pre-NFL signals would be left out of block D. b(1) is then
# Table 08 column (9) and b(5) the column-(5) controls on the draft-free
# posterior (not Table 08 column (5))
GelbachPrior <- if (UseNoDraft) PriorCols08NoDraft else PriorCols08
GelbachBase <- if (UseNoDraft) nodraft_race(VetData) else VetData
# Point estimates on the column-(5) estimation sample (all of VetData)
GelbachCols <- c("gsis_id", "position", "year_signed", GelbachPrior, YVet, Rhs0,
                 unlist(VetCols[GelbachBlocks], use.names = FALSE))
GelbachData <- as.data.frame(GelbachBase[GelbachCols])
# feols version (cross-check of the matrix version below)
GelbachFeols <- gelbach(GelbachData, prior = GelbachPrior)
# Models whose race coefficients b(1) and b(5) must reproduce
GelbachRef1 <- if (UseNoDraft) Models08[["(9)"]] else Models08[["(1)"]]
GelbachRef5 <- if (UseNoDraft) {
  feols(make_fml(YVet, c(Rhs0, GelbachPrior, unlist(VetCols[GelbachBlocks], use.names = FALSE)), FE08),
        data = GelbachBase, vcov = ~gsis_id, notes = FALSE)
} else Models08[["(5)"]]

# The same decomposition with explicit matrices, for the bootstrap (feols with
# about 470 regressors takes over a second per replication): demean every
# column within position x year cells, then OLS from the normal equations.
# Under the predicted measures the prior-covariate FE enter as dummy columns
# of the base regressors W = [Black, OtherRace, prior dummies], so b(1), b(5)
# and the deltas condition on them and the decomposition stays exact.
# Collinear columns are found by a pivoted Cholesky of the scaled
# cross-product matrix (the race regressors, the first two columns, must
# survive); dropped columns get a zero coefficient. y may be a matrix
ols_normal <- function(Z, y, tol = 1e-10) {
  y <- as.matrix(y)
  A <- crossprod(Z)
  zy <- crossprod(Z, y)
  d <- diag(A)
  cand <- which(d > 1e-12 * max(d))
  sc <- sqrt(d[cand])
  As <- A[cand, cand] / outer(sc, sc)
  # Pivoted Cholesky (LAPACK dpstrf) finds the rank of the scaled system
  ch <- suppressWarnings(chol(As, pivot = TRUE, tol = tol))
  r <- attr(ch, "rank")
  kept <- attr(ch, "pivot")[seq_len(r)]
  if (!all(c(1L, 2L) %in% cand[kept])) stop("ols_normal: a race regressor was dropped as collinear")
  U <- ch[seq_len(r), seq_len(r), drop = FALSE]
  b <- matrix(0, ncol(Z), ncol(y))
  # Solve the scaled system (the columns' scales differ by many orders)
  bs <- backsolve(U, forwardsolve(t(U), zy[cand[kept], , drop = FALSE] / sc[kept]))
  b[cand[kept], ] <- bs / sc[kept]
  if (ncol(y) == 1) drop(b) else b
}
GelbachY <- GelbachData[[YVet]]
# Base regressors: race regressors, then the prior-covariate dummies
# (reference level of each covariate omitted)
GelbachR <- as.matrix(GelbachData[c(Rhs0, GelbachPrior)])
GelbachX <- as.matrix(GelbachData[unlist(VetCols[GelbachBlocks], use.names = FALSE)])
GelbachBlockOf <- rep(GelbachBlocks, lengths(VetCols[GelbachBlocks]))
GelbachCell <- as.integer(factor(paste(GelbachData$position, GelbachData$year_signed)))
demean_cells <- function(M, cell) {
  cell <- as.integer(factor(cell))
  M - (rowsum(M, cell) / tabulate(cell))[cell, , drop = FALSE]
}
gelbach_fast <- function(idx) {
  cell <- GelbachCell[idx]
  y <- demean_cells(matrix(GelbachY[idx]), cell)
  R <- demean_cells(GelbachR[idx, , drop = FALSE], cell)
  X <- demean_cells(GelbachX[idx, , drop = FALSE], cell)
  nr <- ncol(R)
  b1 <- ols_normal(R, y)[[1]]
  full <- ols_normal(cbind(R, X), y)
  pi <- full[-seq_len(nr)]
  # Demeaned block contributions regressed on the demeaned base regressors
  H <- sapply(GelbachBlocks, \(k) X[, GelbachBlockOf == k, drop = FALSE] %*%
                pi[GelbachBlockOf == k])
  delta <- ols_normal(R, H)[1, ]
  c(b1 = b1, b5 = full[[1]], set_names(delta, GelbachBlocks))
}
# Reported point estimates: the matrix version, whose normal equations are
# solved on the scaled system. With about 380 partly collinear regressors,
# feols' solution can leave residuals that are not orthogonal to Black (seen
# with the sandbox codes), which breaks the adding-up identity; the matrix
# version satisfies it. b(1) and b(5) must match the Table 08 estimates.
GelbachPoint <- gelbach_fast(seq_len(nrow(GelbachData)))
stopifnot(abs(sum(GelbachPoint[GelbachBlocks]) -
                (GelbachPoint[["b1"]] - GelbachPoint[["b5"]])) < 1e-6,
          abs(GelbachPoint[["b1"]] - coef(GelbachRef1)[["Black"]]) < 1e-6,
          abs(GelbachPoint[["b5"]] - coef(GelbachRef5)[["Black"]]) < 1e-6)
GelbachGap <- max(abs(GelbachPoint - GelbachFeols))
message(glue("12: Gelbach matrix vs feols decomposition: max abs difference {signif(GelbachGap, 3)}; ",
             "feols adding-up error {signif(sum(GelbachFeols[GelbachBlocks]) - (GelbachFeols[['b1']] - GelbachFeols[['b5']]), 3)}"))

# Player-cluster bootstrap: resample players with replacement and repeat the
# whole decomposition
BootReps <- as.integer(Sys.getenv("NFL_BOOT_REPS", "199"))
set.seed(20261002)
RowsByPlayer <- split(seq_len(nrow(GelbachData)), GelbachData$gsis_id)
T0Boot <- Sys.time()
GelbachBoot <- map_dfr(seq_len(BootReps), \(r) {
  draw <- sample(names(RowsByPlayer), length(RowsByPlayer), replace = TRUE)
  as_tibble_row(gelbach_fast(unlist(RowsByPlayer[draw], use.names = FALSE)))
})
message(glue("12: Gelbach bootstrap, {BootReps} replications in ",
             "{round(difftime(Sys.time(), T0Boot, units = 'secs'))}s"))

# Table rows: b(1), b(5), the block contributions and the total change
GelbachTotal <- GelbachPoint[["b1"]] - GelbachPoint[["b5"]]
BootTotal <- GelbachBoot$b1 - GelbachBoot$b5
GelbachRes <- tibble(
  term = c("b1", "b5", GelbachBlocks, "Total"),
  Row = c(if (UseNoDraft) paste0(BlackLab, " (draft-free), Table \\ref{tab:pay-gap-veteran} column (9): position $\\times$ year and prior FE (no draft bucket)")
          else if (is_pred(measure)) paste0(BlackLab, ", column (1): position $\\times$ year and race-prior FE (incl. draft-round bucket)")
          else paste0(BlackLab, ", column (1): position $\\times$ year FE only"),
          if (UseNoDraft) paste0(BlackLab, " (draft-free), column-(5) controls")
          else paste0(BlackLab, ", column (5): main specification"),
          "Career stage (A)", "Prior-season production (B)",
          "Career production (C)", "Pre-NFL signals (D)",
          "Total change, $\\hat\\beta_1^{(1)} - \\hat\\beta_1^{(5)}$"),
  estimate = c(GelbachPoint[c("b1", "b5", GelbachBlocks)], GelbachTotal),
  std_error = c(map_dbl(c("b1", "b5", GelbachBlocks), \(k) sd(GelbachBoot[[k]])),
                sd(BootTotal)),
  share = c(NA, NA, GelbachPoint[GelbachBlocks] / GelbachTotal, 1))
# Shares are reported only when the total change is distinguishable from zero
# (|total| > 2 bootstrap SEs); otherwise the ratios are not informative
ShowShares09 <- abs(GelbachTotal) > 2 * sd(BootTotal)
if (!ShowShares09) GelbachRes$share <- NA_real_

Tab09 <- GelbachRes |>
  transmute(Row, Contribution = fmt_pg(estimate, 3), SE = fmt_se_pg(std_error, 3),
            Share = if_else(is.na(share), "", fmt_pg(share, 3))) |>
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
      align = "lrrr",
      col.names = c("", "Log points", "Bootstrap SE", "Share of total change"),
      caption = paste0("Gelbach decomposition of the change in the Black pay gap",
                       if (!is_primary_measure(measure)) paste0(" (", measure, " race measure)"),
                       " \\label{tab:pay-gap-gelbach}")) |>
  kable_styling(latex_options = c("hold_position")) |>
  pack_rows(paste("Coefficient on", BlackLab), 1, 2) |>
  pack_rows("Contribution of each block to the change", 3, 7) |>
  add_notes(drop_empty(c(
    if (UseNoDraft) paste0("This table decomposes the change in the coefficient on ", BlackLab, " in equation (1) between the raw benchmark (column (9) of Table \\ref{tab:pay-gap-veteran}) and the column-(5) controls following \\citet{gelbach2016covariates}. ",
                           "Both regressions use the draft-free prediction and its prior's covariates (position at NFL entry, entry era, college type, whether a home county is known; no draft-round bucket), so draft slot and draft round enter only through block D. ",
                           glue("The column-(5) coefficient on the draft-free prediction ({fmt_pg(GelbachPoint[['b5']], 3)}) therefore differs from the main estimate in column (5) of Table \\ref{{tab:pay-gap-veteran}} ({fmt_pg(coef(Models08[['(5)']])[['Black']], 3)}), which uses the primary prediction."))
    else paste0("This table decomposes the change in the coefficient on ", BlackLab, " in equation (1) between column (1) and column (5) of Table \\ref{tab:pay-gap-veteran} following \\citet{gelbach2016covariates}."),
    glue("The sample is the column-(5) estimation sample of {fmt_pg(nrow(GelbachData), 0)} freely bargained veteran contracts (UFA and extensions) signed 2014-2026; the outcome is log APY."),
    glue("For each block $k$, the contribution is the coefficient on {BlackLab} from a regression of the block's fitted contribution $X_k\\hat\\pi_k$ (coefficients from the column-(5) model) on {BlackLab} and {OtherLab} and the fixed effects of the base regression (position $\\times$ year signed{if (is_pred(measure)) ' and the race-prior covariates' else ''}); the contributions sum to the total change. A positive contribution means the block's controls account for part of a positive raw gap."),
    if (!ShowShares09) "Shares of the total change are not reported because the total change is not distinguishable from zero (its absolute value is below two bootstrap standard errors)." else "",
    if (UseNoDraft) "" else PriorNote,
    BlockNotes[c("A", "B", "C", "D", "Miss")],
    if (is_pred(measure)) paste(RCNote, "The block contributions therefore mix the controls' relation to pay with their information on race; block D (combine, recruit and college signals) is the most likely to carry race information beyond the prior.") else "",
    DiagNote, PredDefNote,
    glue("Standard errors come from a player-cluster bootstrap with {BootReps} replications: players are resampled with replacement and the whole decomposition is repeated in each replication."),
    RefNote, race_measure_note(measure))))
save_exhibit_tex(Tab09, "table-09-pay-gap-gelbach", measure)
save_estimates(mutate(GelbachRes, boot_reps = BootReps,
                      nobs = nrow(GelbachData)) |> select(-Row),
               "12-pay-gap-gelbach", measure)

# ---------------------------------------------------------------------------
# Table 10: other contract terms and margins (column-(5) controls)
# ---------------------------------------------------------------------------

tick("table 10")

# Column (5) of equation (1) on a subsample: race indicators, blocks A-D
# (columns constant in the subsample dropped) and position x year FE
Cols5 <- c("A", "B", "C", "D")
# (plus the prior-covariate dummies under the predicted measures)
fit_col5 <- function(df, cols, y, rhs = Rhs0, fe = FE08, blocks = Cols5, extra = character()) {
  d <- filter(df, !is.na(.data[[y]]))
  controls <- unlist(map(cols[blocks], \(cc) varying(d, cc)), use.names = FALSE)
  feols(make_fml(y, c(rhs, prior_cols(measure, d), controls, extra), fe), data = d,
        vcov = ~gsis_id, notes = FALSE)
}

# Main-sample contracts with known race (all outcomes)
VetAll <- VetDesign$df

# Full veteran market (adds tags and tenders), with its own design: its
# market-margin dummies are re-sign/extension and tag/tender (reference UFA).
# Black x margin indicators (no main Black term) give the gap within each
# margin from one pooled regression, so the small margins borrow the common
# control slopes instead of fitting about 550 parameters on 859 contracts
VetFullDesign <- PayContracts |>
  filter(VeteranMarket == 1) |>
  keep_known_race("full veteran market") |>
  mutate(DraftRound = as.character(DraftRound),
         Margin4 = case_when(ContractType == "UFA" & coalesce(NewTeam, 0L) == 1 ~ "UFANew",
                             ContractType == "UFA" ~ "UFAOwn",
                             ContractType == "Extension" ~ "Ext",
                             TRUE ~ "TagTender"),
         UFANewTeam = as.integer(Margin4 == "UFANew"),
         BlackM_UFANew = Black * (Margin4 == "UFANew"),
         BlackM_UFAOwn = Black * (Margin4 == "UFAOwn"),
         BlackM_Ext = Black * (Margin4 == "Ext"),
         BlackM_TagTender = Black * (Margin4 == "TagTender")) |>
  build_design("contracts", blocks = Cols5, cat_vars = ContractCats)
BlackMarginTerms <- c("BlackM_UFANew", "BlackM_UFAOwn", "BlackM_Ext", "BlackM_TagTender")
# Black contracts by margin (expected counts under the predicted measures)
MarginCounts <- VetFullDesign$df |>
  group_by(Margin4) |>
  summarise(n = round(sum(Black)), .groups = "drop") |>
  deframe()

# Minimum-salary margin: NonRookieSample signed 2017-2026, with its own
# design (its MarketMargin dummies include Other/SFA/Practice)
MinDesign <- PayContracts |>
  filter(NonRookieSample == 1, year_signed >= 2017) |>
  keep_known_race("non-rookie contracts 2017+") |>
  mutate(DraftRound = as.character(DraftRound)) |>
  build_design("contracts", blocks = Cols5, cat_vars = ContractCats)

Models10 <- list(
  "Guarantee share" = fit_col5(filter(VetAll, GuaranteedZero == 0), VetDesign$cols, "GuaranteeShare"),
  "Zero guarantee" = fit_col5(VetAll, VetDesign$cols, "GuaranteedZero"),
  "Log years" = fit_col5(VetAll, VetDesign$cols, "LogYears"),
  "Log value" = fit_col5(VetAll, VetDesign$cols, "LogValue"),
  "Log APY" = fit_col5(VetFullDesign$df, VetFullDesign$cols, YVet),
  "Log APY" = fit_col5(VetFullDesign$df, VetFullDesign$cols, YVet,
                       rhs = c(BlackMarginTerms, "OtherRace"), extra = "UFANewTeam"),
  "Near minimum" = fit_col5(MinDesign$df, MinDesign$cols, "NearMinimum"))
names(Models10) <- paste0("(", seq_along(Models10), ") ", names(Models10))

# Outcome means, implied percent gaps (log outcomes) and residual degrees of
# freedom on each estimation sample
mean_dep <- function(m) mean(fitted(m) + resid(m))
Rows10 <- tibble(term = c("Sample", "Mean of outcome", "Implied Black gap (\\%)",
                          "Residual df"))
Samples10 <- c("Bargained, known guar.", "Bargained", "Bargained", "Bargained",
               "All veteran", "All veteran", "Non-rookie 2017+")
LogOutcome10 <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
for (i in seq_along(Models10)) {
  m <- Models10[[i]]
  Rows10[[names(Models10)[i]]] <- c(
    Samples10[i], fmt_pg(mean_dep(m), 3),
    if (LogOutcome10[i]) fmt_pg(pct_gap(coef(m)[["Black"]]), 1) else "",
    fmt_pg(degrees_freedom(m, type = "resid"), 0))
}

CoefMap10 <- c(CoefMap,
               BlackM_UFANew = paste(BlackLab, "$\\times$ UFA, new team"),
               BlackM_UFAOwn = paste(BlackLab, "$\\times$ UFA, re-signs with own team"),
               BlackM_Ext = paste(BlackLab, "$\\times$ extension"),
               BlackM_TagTender = paste(BlackLab, "$\\times$ tag or tender"))
write_model_table(
  Models10, CoefMap10,
  title = "Race gaps in contract terms and across market margins",
  label = "pay-gap-terms",
  notes = drop_empty(c(
    "This table includes the estimation results of equation (1) with the column-(5) controls of Table \\ref{tab:pay-gap-veteran} (blocks A-D) and OTC position $\\times$ year-signed fixed effects.",
    glue("Columns (1)-(4) use the main sample of freely bargained veteran contracts (unrestricted free agents and extensions) signed 2014-2026. ",
         "Column (1): guaranteed money over total value, excluding contracts with zero recorded guarantees; a few contracts record guarantees above total value. ",
         "Column (2): linear probability model of zero recorded guarantees (OTC records unknown guarantees as zero, so this mixes zero and unknown guarantees); it shows whether the sample of column (1) is selected by race. ",
         "Column (3): log contract length in years. Column (4): log total contract value."),
    glue("Columns (5)-(6) use the full veteran market, which adds franchise and transition tags and RFA and ERFA tenders (pay set by a CBA formula; ERFA tenders are minimum deals); the outcome is log APY and the market-margin dummies are re-sign/extension and tag/tender (reference: UFA). ",
         "Column (6) replaces {BlackLab} with its interactions with margin indicators, with a dummy for UFAs who sign with a team other than their end-of-season team of the previous year, so each coefficient is the gap within that margin with common control slopes. ",
         "{if (is_pred(measure)) 'Expected Black contracts (sums of P(Black))' else 'Black contracts'} by margin: UFA new team {MarginCounts[['UFANew']] %||% 0}, UFA own team {MarginCounts[['UFAOwn']] %||% 0}, extension {MarginCounts[['Ext']] %||% 0}, tag or tender {MarginCounts[['TagTender']] %||% 0}."),
    "Column (7) is a linear probability model of signing within 10\\% of the reference minimum APY among non-rookie contracts (veteran market plus other, street free agent and practice-squad deals) signed 2017-2026; its market-margin dummies include the other/SFA/practice margin.",
    PriorNote,
    BlockNotes[c("A", "B", "C", "D", "Miss", "Identify")],
    RCNote, PredDefNote,
    "The implied gap is $100(e^{\\hat\\beta_1}-1)$ for log outcomes. Residual degrees of freedom are observations minus estimated coefficients and fixed effects.",
    NoteCluster, RefNote)),
  name = "table-10-pay-gap-terms-margins", measure = measure,
  add_rows = Rows10)
Estimates <- c(Estimates, list(tidy_terms(Models10, c(Rhs0, BlackMarginTerms)) |>
                                 mutate(table = "table-10")))

# ---------------------------------------------------------------------------
# Table 11 and figure: gap by position group (column (5), Black x group)
# ---------------------------------------------------------------------------

tick("table 11")

# Black players and contracts by position group (expected counts, i.e. sums
# of P(Black), under the predicted measures); groups without a Black contract
# get no interaction
PosCounts <- VetData |>
  group_by(PositionGroup) |>
  summarise(BlackContracts = sum(Black),
            BlackPlayers = sum(Black[!duplicated(gsis_id)]), .groups = "drop") |>
  filter(BlackContracts > 0)

# Black x position-group indicators (no main Black term) for groups with at
# least MinBlackPos Black players; Black players in the remaining groups share
# one pooled interaction, so every Black contract has exactly one indicator
MinBlackPos <- 10
PosGroups <- sort(PosCounts$PositionGroup[PosCounts$BlackPlayers >= MinBlackPos])
SmallGroups <- sort(setdiff(PosCounts$PositionGroup, PosGroups))
for (g in PosGroups) {
  VetData[[paste0("BlackPos_", g)]] <- VetData$Black * (VetData$PositionGroup == g)
}
BlackPosTerms <- paste0("BlackPos_", PosGroups)
PosLabels <- paste(BlackLab, "$\\times$", PosGroups)
if (length(SmallGroups) > 0) {
  VetData$BlackPos_Small <- VetData$Black * (VetData$PositionGroup %in% SmallGroups)
  BlackPosTerms <- c(BlackPosTerms, "BlackPos_Small")
  PosLabels <- c(PosLabels, paste0(BlackLab, " $\\times$ ", paste(SmallGroups, collapse = ", ")))
}
# The pooled indicator is reported only when it covers at least MinBlackPos
# Black players (it stays in the regression either way)
SmallPlayers <- sum(PosCounts$BlackPlayers[PosCounts$PositionGroup %in% SmallGroups])
ReportPosTerms <- if (length(SmallGroups) > 0 && SmallPlayers < MinBlackPos) {
  setdiff(BlackPosTerms, "BlackPos_Small")
} else BlackPosTerms
SmallNote <- if (length(SmallGroups) > 0 && SmallPlayers < MinBlackPos) {
  glue("The pooled indicator for Black players in {paste(SmallGroups, collapse = ', ')} ",
       "({fmt_pg(SmallPlayers, 0)} {if (is_pred(measure)) 'expected ' else ''}players) is included but not reported.")
} else ""
Model11 <- list("(1)" = fit_col5(VetData, VetCols, YVet,
                                 rhs = c(BlackPosTerms, "OtherRace")))
# Refit without the regressors fixest dropped as collinear (e.g. a prior
# dummy nested in the position x year cells), so that the lm refit of the
# wild cluster bootstrap has no aliased coefficients
if (length(Model11[["(1)"]]$collin.var) > 0) {
  Model11[["(1)"]] <- feols(make_fml(YVet, names(coef(Model11[["(1)"]])), FE08),
                            data = VetData, vcov = ~gsis_id, notes = FALSE)
}

CoefMap11 <- c(set_names(PosLabels, BlackPosTerms)[ReportPosTerms],
               OtherRace = OtherLab)

# Wild cluster restricted bootstrap (player clusters) p-values for each
# reported group interaction: some groups have few Black players (treated
# clusters), where cluster-robust SEs can over-reject
WcbReps <- as.integer(Sys.getenv("NFL_WCB_REPS", "9999"))
T0Wcb <- Sys.time()
# The bootstrap refits the model by lm() with the FE as dummies; controls
# measured in yards or snaps next to 0/1 dummies make the cross-product matrix
# numerically singular, so the controls are divided by their standard
# deviation first (this leaves the coefficients on the race terms unchanged)
WcbControls <- setdiff(names(coef(Model11[["(1)"]])), c(BlackPosTerms, "OtherRace"))
WcbData <- VetData |>
  mutate(across(all_of(WcbControls), \(x) x / sd(x)))
Model11Wcb <- feols(make_fml(YVet, names(coef(Model11[["(1)"]])), FE08),
                    data = WcbData, vcov = ~gsis_id, notes = FALSE)
stopifnot(max(abs(coef(Model11Wcb)[BlackPosTerms] - coef(Model11[["(1)"]])[BlackPosTerms])) < 1e-6)
# One term per worker of a socket cluster (forked workers crash inside
# boottest); wild_cluster_test() sets its own seed, so the p-values do not
# depend on the number of workers
WcbTerms <- setdiff(ReportPosTerms, "OtherRace")
if (Cores > 1) {
  WcbCluster <- parallel::makePSOCKcluster(min(Cores, length(WcbTerms)))
  parallel::clusterEvalQ(WcbCluster, suppressMessages({
    library(tidyverse); library(glue); library(fixest); library(fwildclusterboot)
  }))
  parallel::clusterExport(WcbCluster, c("Model11Wcb", "WcbData", "WcbReps",
                                        "wild_cluster_test"))
  Wcb11 <- parallel::parLapply(WcbCluster, WcbTerms, \(t)
    wild_cluster_test(Model11Wcb, WcbData, t, "gsis_id", B = WcbReps))
  parallel::stopCluster(WcbCluster)
  Wcb11 <- bind_rows(Wcb11)
} else {
  Wcb11 <- map_dfr(WcbTerms, \(t) wild_cluster_test(Model11Wcb, WcbData, t, "gsis_id", B = WcbReps))
}
stopifnot(nrow(Wcb11) == length(WcbTerms))
message(glue("12: Table 11 wild cluster bootstrap, {nrow(Wcb11)} terms x {WcbReps} reps in ",
             "{round(difftime(Sys.time(), T0Wcb, units = 'secs'))}s"))
Rows11 <- tibble(term = paste("WCR bootstrap $p$,", set_names(PosLabels, BlackPosTerms)[Wcb11$term]),
                 "(1)" = fmt_p(Wcb11$p_wcb))
write_model_table(
  Model11, CoefMap11,
  title = "Race gap in veteran-contract pay by position group",
  label = "pay-gap-position",
  notes = drop_empty(c(
    glue("This table includes the estimation results of equation (1) with {BlackLab} interacted with position-group indicators (no main term), so each coefficient is the gap within that group."),
    "The outcome is log APY; the sample is freely bargained veteran contracts (UFA and extensions) signed 2014-2026.",
    "Controls are those of column (5) of Table \\ref{tab:pay-gap-veteran} (blocks A-D) with OTC position $\\times$ year-signed fixed effects.",
    PriorNote,
    glue(paste0(if (is_pred(measure)) "Expected Black contracts (players), sums of P(Black), by group: " else "Black contracts (players) by group: ",
           paste0(PosCounts$PositionGroup, " ", fmt_pg(PosCounts$BlackContracts, 0), " (",
                  fmt_pg(PosCounts$BlackPlayers, 0), ")", collapse = ", "),
           ". Groups with fewer than {MinBlackPos} {if (is_pred(measure)) 'expected ' else ''}Black players share one pooled interaction; groups without a Black contract have none.")),
    SmallNote,
    BlockNotes[c("A", "B", "C", "D", "Miss", "Identify")],
    RCNote, PredDefNote,
    "OL quality is measured only by games, injury weeks and pre-NFL signals, so the OL estimate rests on those controls.",
    NoteCluster,
    if (is_pred(measure)) glue("The WCR bootstrap $p$-values are from a wild cluster restricted bootstrap-t with player clusters, Webb weights and {WcbReps} replications; every player enters each interaction with his P(Black), but within some groups (e.g. K, P, LS, QB, OL) P(Black) is mostly near zero and its effective variation comes from few players, where cluster-robust standard errors can over-reject.")
    else glue("The WCR bootstrap $p$-values are from a wild cluster restricted bootstrap-t with player clusters, Webb weights and {WcbReps} replications; with few Black players in a group (few treated clusters) they are more reliable than the cluster-robust standard errors."),
    RefNote)),
  name = "table-11-pay-gap-by-position", measure = measure, add_rows = Rows11)
Estimates <- c(Estimates, list(Wcb11 |> transmute(table = "table-11-wcb", model = "(1)", term,
                                                  estimate, p_value = p_wcb, ci_low, ci_high,
                                                  nobs = nobs(Model11[["(1)"]]))))
Estimates <- c(Estimates, list(tidy_terms(Model11, c(ReportPosTerms, "OtherRace")) |>
                                 mutate(table = "table-11")))

# Figure: coefficients with 95% CIs for groups with their own interaction
FigPos <- tidy_terms(Model11, BlackPosTerms) |>
  mutate(PositionGroup = str_remove(term, "^BlackPos_")) |>
  left_join(PosCounts, by = "PositionGroup") |>
  filter(BlackPlayers >= MinBlackPos) |>
  mutate(Label = fct_reorder(paste0(PositionGroup, "\n(N = ", round(BlackPlayers), ")"), estimate))
# Subtitle line on the race measure
FigMeasureLine <- switch(measure,
  hand = NULL,
  provisional = "\nProvisional race measure (Wikipedia category flag)",
  predicted = "\nPredicted race (P(Black)); race-prior covariate FE included",
  preddoc = "\nDocumented-or-predicted race (sensitivity); race-prior covariate FE included")
FigPosPlot <- ggplot(FigPos, aes(x = Label, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high), colour = "#1f4e79") +
  labs(x = if (is_pred(measure)) "Position group (expected N Black players)" else "Position group (N Black players)",
       y = "Black gap in log APY (95% CI)",
       title = "Race gap in veteran-contract pay by position group",
       subtitle = paste("Equation (1), column (5) controls; SEs clustered by player",
                        FigMeasureLine),
       caption = glue("Groups with at least {MinBlackPos} {if (is_pred(measure)) 'expected ' else ''}Black players.")) +
  theme_customs()
save_exhibit_figure(FigPosPlot, "figure-pay-gap-by-position", measure)

# ---------------------------------------------------------------------------
# Table 12 and figure: player-season panel and the signal x experience test
# ---------------------------------------------------------------------------

tick("table 12")

# Player-season analogue of equation (1), for player i in position group g
# and season t:
#   Y_igt = beta_1 Black_i + beta_2 Other_i + X_igt pi + delta_{g,t} + e_igt
# Outcome: log cap number ($ millions). OTC's CapPercent is rounded to 0.001
# of the league cap (low cap hits are 0), so log(CapPercent) would truncate
# low-pay seasons; delta_{g,t} absorbs the league cap.
YPanel <- "LogCapNumber"

# Player-seasons with a governing contract and known race; control design
# from the player_season rows of the dictionary
PanelGov <- PayPanel |>
  filter(!is.na(OnRookieContract)) |>
  keep_known_race("player-seasons")
NoPositiveCap <- sum(is.na(PanelGov[[YPanel]]))
PanelKnown <- PanelGov |>
  filter(!is.na(.data[[YPanel]])) |>
  mutate(DraftRound = as.character(DraftRound))
PanelDesign <- build_design(PanelKnown, "player_season", blocks = c("A", "B", "C", "D"),
                            cat_vars = list(A = "ExperienceBin", D = "DraftRound"))
PanelAll <- PanelDesign$df

# Black x experience-bin indicators (no main Black term), Black x rookie
# contract, and pre-NFL signals x Experience (Altonji-Pierret terms)
# Black x experience bins: 0-3 pooled, because few players are on a UFA or
# extension contract in their first four seasons (a handful of Black players
# per single-season bin gave spurious estimates with random sandbox codes)
ExpBins <- c("0-3", "4-6", "7+")
ExpBinOf <- c("0" = "0-3", "1" = "0-3", "2" = "0-3", "3" = "0-3", "4-6" = "4-6", "7+" = "7+")
add_panel_terms <- function(df) {
  for (b in ExpBins) {
    df[[paste0("BlackExp_", level_tag(b))]] <- df$Black * (ExpBinOf[df$ExperienceBin] == b)
  }
  df |>
    mutate(BlackRookie = Black * OnRookieContract,
           LogDraftPick_x_Exp = LogDraftPick * Experience,
           Undrafted_x_Exp = Undrafted * Experience,
           RecruitRating_x_Exp = RecruitRating * Experience,
           RecruitRatingMiss_x_Exp = RecruitRatingMiss * Experience)
}
PanelAll <- add_panel_terms(PanelAll)
# Main panel sample: seasons governed by a freely bargained veteran contract
# (UFA or extension); the plan's broader non-rookie sample (adds SFA,
# practice-squad, ERFA, tag/tender and other minimum-scale deals) is column (7)
PanelVet <- filter(PanelAll, VeteranBargainedPay == 1)
PanelNonRookie <- filter(PanelAll, BargainedPay == 1)
BlackExpTerms <- paste0("BlackExp_", level_tag(ExpBins))
BlackExpTerms <- BlackExpTerms[map_lgl(BlackExpTerms, \(v) sum(PanelVet[[v]]) > 0)]
SignalExpTerms <- c("LogDraftPick_x_Exp", "Undrafted_x_Exp", "RecruitRating_x_Exp",
                    "RecruitRatingMiss_x_Exp")

# Controls by block (columns constant in the sample dropped)
panel_controls <- function(df, blocks) {
  unlist(map(PanelDesign$cols[blocks], \(cc) varying(df, cc)), use.names = FALSE)
}
# Position group x season FE (plus the prior-covariate dummies under the
# predicted measures)
FE12 <- "PositionGroup^season"
fit_panel <- function(df, rhs) {
  feols(make_fml(YPanel, c(rhs, prior_cols(measure, df)), FE12), data = df,
        vcov = ~gsis_id, notes = FALSE)
}
Models12 <- list(
  "(1)" = fit_panel(PanelVet, Rhs0),
  "(2)" = fit_panel(PanelVet, c(Rhs0, panel_controls(PanelVet, "A"))),
  "(3)" = fit_panel(PanelVet, c(Rhs0, panel_controls(PanelVet, c("A", "B", "C")))),
  "(4)" = fit_panel(PanelVet, c(BlackExpTerms, "OtherRace",
                                panel_controls(PanelVet, c("A", "B", "C")))),
  # Altonji-Pierret specification: entry signals and their experience
  # interactions without the production employers learn from
  "(5)" = fit_panel(PanelVet, c(BlackExpTerms, "OtherRace",
                                panel_controls(PanelVet, c("A", "D")), SignalExpTerms)),
  "(6)" = fit_panel(PanelVet, c(BlackExpTerms, "OtherRace",
                                panel_controls(PanelVet, c("A", "B", "C", "D")),
                                SignalExpTerms)),
  "(7)" = fit_panel(PanelNonRookie, c(Rhs0, panel_controls(PanelNonRookie, c("A", "B", "C")))),
  "(8)" = fit_panel(PanelAll, c(Rhs0, "OnRookieContract", "BlackRookie",
                                panel_controls(PanelAll, c("A", "B", "C")))))

# Locating the non-rookie gap of column (7): (9) adds the pre-NFL signals
# (block D); (10) replaces P(Black) with its interactions with the governing
# contract type (UFA or extension; tender or tag; street free agent;
# practice squad; other), with contract-type dummies, so each coefficient is
# the gap within that contract type; (11) is column (7) on seasons with at
# least one game played (partial-season cap numbers of released or
# practice-squad players)
ContractGroups12 <- c(UFAExt = "UFA or extension", Tender = "tender or tag",
                      SFA = "street free agent", Practice = "practice squad",
                      Other = "other")
PanelNonRookie <- PanelNonRookie |>
  mutate(ContractGroup = case_when(
    GoverningContractType %in% c("UFA", "Extension") ~ "UFAExt",
    GoverningContractType %in% c("ERFA", "RFA", "Franchise", "Transition") ~ "Tender",
    GoverningContractType == "SFA" ~ "SFA",
    GoverningContractType == "Practice" ~ "Practice",
    TRUE ~ "Other"))
for (g in names(ContractGroups12)) {
  PanelNonRookie[[paste0("BlackCT_", g)]] <- PanelNonRookie$Black * (PanelNonRookie$ContractGroup == g)
}
CTDummies <- add_dummies(PanelNonRookie, "ContractGroup", "CT", ref = "UFAExt")
PanelNonRookie <- CTDummies$df
BlackCTTerms <- paste0("BlackCT_", names(ContractGroups12))
# Seasons (expected Black seasons under the predicted measures) by contract
# type, for the notes
CTCounts <- PanelNonRookie |>
  group_by(ContractGroup) |>
  summarise(n = n(), NBlack = sum(Black), .groups = "drop")
print(CTCounts)
Models12[["(9)"]] <- fit_panel(PanelNonRookie, c(Rhs0, panel_controls(PanelNonRookie, c("A", "B", "C", "D"))))
Models12[["(10)"]] <- fit_panel(PanelNonRookie, c(BlackCTTerms, "OtherRace", CTDummies$cols,
                                                  panel_controls(PanelNonRookie, c("A", "B", "C"))))
PanelPlayed <- filter(PanelNonRookie, coalesce(GamesPlayed, 0) > 0)
Models12[["(11)"]] <- fit_panel(PanelPlayed, c(Rhs0, panel_controls(PanelPlayed, c("A", "B", "C"))))

ExpLabels <- c("0-3" = "0-3", "4-6" = "4-6", "7+" = "7+")
# Black players and seasons by pooled bin (UFA/extension seasons), for the notes
# (expected counts, sums of P(Black), under the predicted measures)
BlackByBin <- PanelVet |>
  mutate(Bin = ExpBinOf[ExperienceBin]) |>
  group_by(Bin) |>
  summarise(Players = sum(Black[!duplicated(gsis_id)]), Seasons = sum(Black),
            .groups = "drop")
CoefMap12 <- c(CoefMap,
               set_names(paste0(BlackLab, " $\\times$ experience ", ExpLabels), paste0("BlackExp_", level_tag(ExpBins))),
               OnRookieContract = "Rookie contract",
               BlackRookie = paste(BlackLab, "$\\times$ rookie contract"),
               LogDraftPick_x_Exp = "Log draft pick $\\times$ experience",
               Undrafted_x_Exp = "Undrafted $\\times$ experience",
               RecruitRating_x_Exp = "247 rating $\\times$ experience",
               set_names(paste(BlackLab, "$\\times$", ContractGroups12), BlackCTTerms))
# Under the predicted measures the prior FE include the draft-round bucket,
# so columns without block D still condition on it
NoD12 <- if (is_pred(measure)) "Bucket (prior)" else "No"
Rows12 <- tibble(
  term = c("Position group $\\times$ season FE", "Experience bins, age (A)",
           "Lagged and career production (B, C)", "Pre-NFL signals (D)",
           "Signals $\\times$ experience", "Contract-type dummies", "Seasons",
           "Mean of outcome", "Implied Black gap (\\%)"),
  "(1)" = c("Yes", "No", "No", NoD12, "No", "No", "UFA/ext.", "", ""),
  "(2)" = c("Yes", "Yes", "No", NoD12, "No", "No", "UFA/ext.", "", ""),
  "(3)" = c("Yes", "Yes", "Yes", NoD12, "No", "No", "UFA/ext.", "", ""),
  "(4)" = c("Yes", "Yes", "Yes", NoD12, "No", "No", "UFA/ext.", "", ""),
  "(5)" = c("Yes", "Yes", "No", "Yes", "Yes", "No", "UFA/ext.", "", ""),
  "(6)" = c("Yes", "Yes", "Yes", "Yes", "Yes", "No", "UFA/ext.", "", ""),
  "(7)" = c("Yes", "Yes", "Yes", NoD12, "No", "No", "Non-rookie", "", ""),
  "(8)" = c("Yes", "Yes", "Yes", NoD12, "No", "No", "All", "", ""),
  "(9)" = c("Yes", "Yes", "Yes", "Yes", "No", "No", "Non-rookie", "", ""),
  "(10)" = c("Yes", "Yes", "Yes", NoD12, "No", "Yes", "Non-rookie", "", ""),
  "(11)" = c("Yes", "Yes", "Yes", NoD12, "No", "No", "Non-rookie, played", "", ""))
for (nm in names(Models12)) {
  Rows12[[nm]][8] <- fmt_pg(mean_dep(Models12[[nm]]), 3)
  b <- coef(Models12[[nm]])
  if ("Black" %in% names(b)) Rows12[[nm]][9] <- fmt_pg(pct_gap(b[["Black"]]), 1)
}

write_model_table(
  Models12, CoefMap12,
  title = "Race gap in realized pay over the career: player-season panel",
  label = "pay-gap-learning",
  notes = drop_empty(c(
    "This table includes the estimation results of a player-season analogue of equation (1), $Y_{igt} = \\beta_1 Black_i + \\beta_2 Other_i + X_{igt}\\pi + \\delta_{g,t} + \\varepsilon_{igt}$ for player $i$ in position group $g$ and season $t$, where $\\delta_{g,t}$ are position group $\\times$ season fixed effects.",
    glue("The outcome is the log of the player's cap number in season $t$ (\\$ millions); the fixed effects absorb the league salary cap, so this is the log cap number relative to the league cap. ",
         "OTC's cap share is rounded to 0.1\\% of the cap and is zero for low cap hits, so it is not used. ",
         "The sample is player-seasons 2014-2025 with realized pay, a governing contract, experience 0-15 and {KnownRace} ",
         "({DroppedRace[['player-seasons']][['dropped']]} player-seasons of players with {UnknownRace} and {NoPositiveCap} with a non-positive cap number are excluded). ",
         "Columns (1)-(6) use seasons governed by a freely bargained veteran contract (UFA or extension). ",
         "Column (7) uses all seasons on a non-rookie contract, which adds street free agent, practice-squad, ERFA, tag, tender and other deals, mostly on the experience-scaled minimum. ",
         "Column (8) adds seasons on rookie (drafted or UDFA) contracts and controls for a rookie-contract indicator and its interaction with {BlackLab}."),
    glue("Columns (9)-(11) locate the non-rookie gap of column (7). Column (9) adds the pre-NFL signals (block D). ",
         "Column (10) replaces {BlackLab} with its interactions with the governing contract type and adds contract-type dummies (reference: UFA or extension), so each coefficient is the gap within that contract type; ",
         "{if (is_pred(measure)) 'expected Black seasons (sums of P(Black))' else 'Black seasons'} and all seasons by type: ",
         paste0(ContractGroups12[CTCounts$ContractGroup], " ", fmt_pg(CTCounts$NBlack, 0), " of ", fmt_pg(CTCounts$n, 0), collapse = ", "),
         ". Tenders and tags include ERFA and RFA tenders and franchise and transition tags; other includes the remaining non-rookie deal types. ",
         "Column (11) is column (7) on seasons with at least one game played, which drops partial-season cap numbers of players who never played (released or practice-squad players); {fmt_pg(nrow(PanelNonRookie) - nrow(PanelPlayed), 0)} seasons are dropped. ",
         "Column (10) shows where the column-(7) estimate comes from: the within-type coefficients are ",
         paste0(ContractGroups12, " ", fmt_pg(coef(Models12[["(10)"]])[BlackCTTerms], 3), " (", fmt_pg(se(Models12[["(10)"]])[BlackCTTerms], 3), ")", collapse = ", "),
         "."),
    PriorNote,
    "Block A: experience-bin dummies, experience, age and its square. Blocks B and C: one-season lagged production and career production to date with position-group-specific slopes, defined as in Table \\ref{tab:pay-gap-veteran}. Block D: pre-NFL signals as in Table \\ref{tab:pay-gap-veteran}.",
    glue("Columns (4)-(6) replace {BlackLab} with its interactions with experience-bin indicators (0-3, 4-6 and 7+ seasons; ",
         "{if (is_pred(measure)) 'expected Black seasons (players), sums of P(Black),' else 'Black seasons (players)'} by bin: ",
         paste0(BlackByBin$Bin, " ", fmt_pg(BlackByBin$Seasons, 0), " (", fmt_pg(BlackByBin$Players, 0), ")", collapse = ", "), ")."),
    "Column (4) includes the controls of column (3). Column (5) is the \\citet{altonji2001employer} specification: pre-NFL signals and the interactions of log draft pick, the undrafted indicator and the 247 rating (and its missing indicator) with experience, without the lagged and career production employers learn from; column (6) adds that production.",
    "The signal $\\times$ experience terms are consistent with employer learning but also with selective survival (late picks and undrafted players still in the league at high experience are positively selected), and the cap number in season $t$ comes from a contract signed earlier, so lagged production can postdate the pay decision. They are not a test of employer learning on their own.",
    BlockNotes[["Miss"]], BlockNotes[["Identify"]],
    RCNote, PredDefNote,
    "The implied gap is $100(e^{\\hat\\beta_1}-1)$.",
    NoteCluster, RefNote)),
  name = "table-12-pay-gap-player-season", measure = measure,
  add_rows = Rows12, font_size = 8)
Estimates <- c(Estimates, list(
  tidy_terms(Models12, c(Rhs0, BlackExpTerms, "OnRookieContract", "BlackRookie",
                         SignalExpTerms, BlackCTTerms)) |> mutate(table = "table-12")))

# Figure: Black x experience-bin coefficients with and without the lagged and
# career production controls
Model12NoProd <- fit_panel(PanelVet, c(BlackExpTerms, "OtherRace",
                                       panel_controls(PanelVet, "A")))
FigExp <- bind_rows(
  tidy_terms(list("Without production controls" = Model12NoProd), BlackExpTerms),
  tidy_terms(list("With lagged and career production" = Models12[["(4)"]]), BlackExpTerms)) |>
  mutate(Bin = factor(ExpLabels[match(term, paste0("BlackExp_", level_tag(ExpBins)))],
                      levels = ExpLabels))
Estimates <- c(Estimates, list(FigExp |> select(-Bin) |> mutate(table = "figure-pay-gap-by-experience")))
FigExpPlot <- ggplot(FigExp, aes(x = Bin, y = estimate, colour = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = c("Without production controls" = "#b2182b",
                                 "With lagged and career production" = "#1f4e79")) +
  labs(x = "Experience (seasons)", y = "Black gap in log cap number (95% CI)",
       colour = NULL,
       title = "Race gap in realized pay by experience",
       subtitle = paste("UFA/extension seasons 2014-2025; position group x season FE; SEs clustered by player",
                        FigMeasureLine)) +
  theme_customs()
save_exhibit_figure(FigExpPlot, "figure-pay-gap-by-experience", measure)

# ---------------------------------------------------------------------------
# Table 13: rookie (draft) margin
# ---------------------------------------------------------------------------

tick("table 13")

# Race vector of every player, joined on gsis_id (prospects without a gsis_id
# have no race)
con <- db_connect()
ProspectRace <- load_person_race(con, hand_coded) |>
  filter(entity == "player") |>
  select(gsis_id = person_id, race, hispanic, black_any, wiki_cat_black,
         wiki_cat_hispanic_latino, wiki_cat_asian, wiki_cat_pacific_islander,
         wiki_cat_native_american, any_of(c("p_black_any_pred", "p_white_pred",
                                            "p_black_any_preddoc", "p_white_preddoc")),
         any_of(c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket",
                  "pred_college_type"))) |>
  mutate(across(starts_with("pred_"), \(x) coalesce(as.character(x), "unknown")))
db_disconnect(con)
stopifnot(!anyDuplicated(ProspectRace$gsis_id))

# Final-season college production with position-group slopes, and the other
# pre-draft controls
DraftCombine <- c("Forty", "Vertical", "Bench", "BroadJump", "Cone", "Shuttle",
                  "Height", "Weight")
DraftProduction <- tribble(
  ~variable,                 ~slope_groups,
  "CollFinalPassYds",        "QB",
  "CollFinalPassTD",         "QB",
  "CollFinalPassInt",        "QB",
  "CollFinalRushYds",        "QB;RB",
  "CollFinalRushTD",         "RB",
  "CollFinalRec",            "RB;WR;TE",
  "CollFinalRecYds",         "RB;WR;TE",
  "CollFinalRecTD",          "RB;WR;TE",
  "CollFinalTackles",        "DL;LB;DB",
  "CollFinalTFL",            "DL;LB",
  "CollFinalSacks",          "DL;LB",
  "CollFinalDefInt",         "DB",
  "CollFinalPassesDefended", "DB",
  "CollFinalFGMade",         "K",
  "CollFinalFGAtt",          "K",
  "CollFinalPunts",          "P",
  "CollFinalPuntYds",        "P")
DraftLinear <- c("AgeAtDraft", "FinalCollegePower", "FinalCollegeSRS", "FinalCollegeHBCU")
DraftDict <- bind_rows(
  tibble(block = "D", variable = c(DraftCombine, paste0(DraftCombine, "Miss"),
                                   "RecruitRating", "RecruitRatingMiss"),
         slope_groups = "all"),
  tibble(block = "D", variable = c(DraftLinear, paste0(DraftLinear, "Miss")),
         slope_groups = "none"),
  DraftProduction |> mutate(block = "D"),
  DraftProduction |> mutate(block = "D", variable = paste0(variable, "Miss")),
  tibble(block = "F", variable = c("PreDraftGrade", "PreDraftGradeMiss"),
         slope_groups = "none")) |>
  mutate(sample = "draft")

# Classes 2011-2022 with known race; missing controls zero-filled with a Miss
# indicator; prospects without a college get their own cluster
Prospects <- DraftProspects |>
  filter(DraftClass %in% 2011:2022) |>
  select(-any_of(c("race", "hispanic", "black_any", "nonwhite", "race_source",
                   "black_provisional", "black_provisional_source",
                   setdiff(names(ProspectRace), "gsis_id")))) |>
  left_join(ProspectRace, by = "gsis_id") |>
  add_pred_extras() |>
  add_race()
# Under the predicted measure, the draft-free posterior: the primary prior
# conditions on the draft-round bucket, which is the outcome here
if (UseNoDraft) Prospects <- nodraft_race(Prospects)

# Race is attached through gsis_id, which an undrafted prospect has only if
# he later signed with an NFL team: the invitee sample of column (4) is
# selected on post-draft NFL employment. Counts by draft status
InviteeDrops <- Prospects |>
  filter(CombineInvite == 1) |>
  group_by(Drafted) |>
  summarise(n = n(), NoGsis = sum(is.na(gsis_id)), NoRace = sum(!RaceKnown),
            .groups = "drop")
print(InviteeDrops)
drop_n <- function(d, col) InviteeDrops[[col]][InviteeDrops$Drafted == d] %||% 0L
InviteeDrafted <- Prospects |>
  filter(CombineInvite == 1) |>
  summarise(All = mean(Drafted), Known = mean(Drafted[RaceKnown]))

Prospects <- Prospects |>
  keep_known_race("draft prospects 2011-2022") |>
  add_prior_dummies() |>
  fill_missing(c(DraftCombine, "RecruitRating", DraftLinear,
                 DraftProduction$variable, "PreDraftGrade")) |>
  mutate(ClusterCollege = if_else(is.na(College) | College == "",
                                  paste0("missing:", ProspectId), College))
DraftDesign <- build_design(Prospects, "draft", blocks = c("D", "F"), dict = DraftDict)

# Models: draft slot among drafted; drafted among combine invitees
# Class x position group FE (plus, under the predicted measures, the
# prior-covariate dummies without the draft-round bucket, which records the
# outcome)
FE13 <- "DraftClass^PositionGroup"
fit_draft <- function(df, y, blocks) {
  controls <- unlist(map(DraftDesign$cols[blocks], \(cc) varying(df, cc)), use.names = FALSE)
  feols(make_fml(y, c(Rhs0, prior_cols(measure, df, draft = FALSE), controls), FE13),
        data = df, vcov = ~ClusterCollege,
        notes = FALSE)
}
DraftedOnly <- filter(DraftDesign$df, Drafted == 1)
Invitees <- filter(DraftDesign$df, CombineInvite == 1)
Models13 <- list(
  "(1) Log pick" = fit_draft(DraftedOnly, "LogPick", character()),
  "(2) Log pick" = fit_draft(DraftedOnly, "LogPick", "D"),
  "(3) Log pick" = fit_draft(DraftedOnly, "LogPick", c("D", "F")),
  "(4) Drafted" = fit_draft(Invitees, "Drafted", "D"))
Rows13 <- tibble(
  term = c("Class $\\times$ position group FE", "Combine, recruit, college (D)",
           "Pre-draft grade (F)", "Sample", "Mean of outcome"),
  "(1) Log pick" = c("Yes", "No", "No", "Drafted", ""),
  "(2) Log pick" = c("Yes", "Yes", "No", "Drafted", ""),
  "(3) Log pick" = c("Yes", "Yes", "Yes", "Drafted", ""),
  "(4) Drafted" = c("Yes", "Yes", "No", "Invitees, NFL id", ""))
for (nm in names(Models13)) Rows13[[nm]][5] <- fmt_pg(mean_dep(Models13[[nm]]), 3)

# Note on the race measure of the draft margin
NoteRace13 <- if (UseNoDraft) {
  "P(Black) and P(other race) come from the draft-free race prediction, whose prior omits the draft-round bucket (the primary prior conditions on it, and it records the outcome); every column includes fixed effects for that prior's covariates (position at NFL entry, entry era, college type, whether a home county is known), as regression calibration requires."
} else if (is_pred(measure)) {
  "Every column includes fixed effects for the race prior's covariates except the draft-round bucket, which records the outcome; because the probabilities condition on draft round, the estimates may be biased toward the draft-round means (no draft-free prediction exists for this measure)."
} else ""

write_model_table(
  Models13, CoefMap,
  title = "Race gap at the draft margin",
  label = "draft-margin",
  notes = drop_empty(c(
    "This table includes the estimation results of a prospect-level analogue of equation (1), $Y_{icg} = \\beta_1 Black_i + \\beta_2 Other_i + X_i\\pi + \\delta_{c,g} + \\varepsilon_{icg}$ for prospect $i$ of draft class $c$ in position group $g$, where $\\delta_{c,g}$ are draft class $\\times$ position group fixed effects.",
    glue("The sample is NFL draft prospects of the 2011-2022 classes (drafted players and combine invitees) with {KnownRace}; ",
         "{DroppedRace[['draft prospects 2011-2022']][['dropped']]} prospects without one, including every prospect without a GSIS id, are excluded. ",
         "Columns (1)-(3): log overall pick among drafted players (a positive coefficient is a later pick)."),
    glue("Column (4): linear probability model of being drafted among combine invitees. Race is attached through the GSIS id, which an undrafted invitee has only if he later joined an NFL team, so this sample conditions on later NFL employment, an outcome of the draft: ",
         "{drop_n(0L, 'NoGsis')} of {drop_n(0L, 'n')} undrafted invitees and {drop_n(1L, 'NoGsis')} of {drop_n(1L, 'n')} drafted invitees have no GSIS id, ",
         "and {drop_n(0L, 'NoRace')} undrafted and {drop_n(1L, 'NoRace')} drafted invitees have no race measure. ",
         "The drafted share is {fmt_pg(InviteeDrafted$All, 3)} among all invitees and {fmt_pg(InviteeDrafted$Known, 3)} among invitees with a race measure. ",
         "Column (4) is therefore not an estimate of the race gap in the probability of being drafted. ",
         "The pre-draft grade is not used in column (4): it exists only for drafted players, so its missing indicator would determine the outcome."),
    "Block D: combine measures (40, vertical, bench, broad jump, cone, shuttle, height, weight) and the 247 rating interacted with position group; age at draft; the final college team's Power-conference, SRS and HBCU indicators; and final-season college production with position-group slopes (passing for QBs, rushing and receiving for RBs, receiving for WRs and TEs, tackles, tackles for loss and sacks for DL and LB, tackles, interceptions and passes defended for DBs, kicking and punting; defensive statistics start in 2016).",
    "Block F: the CFBD pre-draft grade, which may itself embed scouts' bias.",
    NoteRace13,
    BlockNotes[["Miss"]],
    "Standard errors are clustered at the college level (final college team as listed in the draft or combine record; prospects without a college form their own cluster).",
    RefNote)),
  name = "table-13-draft-margin-race", measure = measure,
  add_rows = Rows13)
Estimates <- c(Estimates, list(tidy_terms(Models13, Rhs0) |> mutate(table = "table-13")))

# ---------------------------------------------------------------------------
# Table 26: BIRDiE cross-check of the raw and conditional gaps
# ---------------------------------------------------------------------------

tick("table 26")

# BIRDiE (McCartan, Fisher, Goldin, Ho and Imai 2025): a Normal linear model of
# log APY with race-specific coefficients on a parsimonious X, fit by EM with
# race as a latent variable whose prior is the race prediction (P(white),
# P(Black), P(other race)). It identifies E[Y | R] and E[Y | R, X] if names
# and hometown are independent of pay given race and X and the probabilities
# are calibrated given X. Main sample (table 08)
YearBreaks <- c(2013, 2016, 2019, 2022, 2026)
BirdieData <- VetData |>
  mutate(YearBucket = cut(year_signed, YearBreaks,
                          labels = c("2014-16", "2017-19", "2020-22", "2023-26")),
         pr_white = PWhite, pr_black = Black, pr_other = OtherRace) |>
  mutate(across(c(PositionGroup, ExperienceBin, all_of(prior_fe_vars(measure, VetData))),
                as.factor))
# Parsimonious X: market position group, signing-year bucket, experience bin,
# the race prior's covariates under the measure (prior_fe_vars(); under
# preddoc also article found and career length), and quality summaries
# (prior-season games and starts, career games, draft slot, athletic score
# with its missing indicator)
BirdieX <- c("PositionGroup", "YearBucket", "ExperienceBin", prior_fe_vars(measure, BirdieData),
             "PriorGamesPlayed", "PriorGamesStartedSnaps", "CareerGames",
             "LogDraftPick", "LogDraftPickMiss", "Undrafted", "AthleticScore",
             "AthleticScoreMiss")
BirdieX <- BirdieX[map_lgl(BirdieX, \(v) n_distinct(BirdieData[[v]]) > 1)]
BirdieFml <- make_fml(YVet, BirdieX)
# Intercept-only model: E[Y | R] without the race-specific X model (needs
# names independent of pay given race alone)
BirdieFml0 <- as.formula(paste(YVet, "~ 1"))

# One BIRDiE fit on rows idx: E[Y | R] by race (finite-population estimates,
# coef()), the Black-white difference, the conditional gap (mean over players,
# weighted by P(Black), of X_i'(beta_Black - beta_white), from the
# race-specific linear predictors) and, for comparison, probability-weighted
# means. The r_probs columns are ordered white, Black, other
birdie_fit <- function(idx) {
  d <- BirdieData[idx, ]
  rp <- as.data.frame(d[c("pr_white", "pr_black", "pr_other")])
  rp <- rp / rowSums(rp)
  fit <- suppressMessages(suppressWarnings(
    birdie::birdie(rp, BirdieFml, data = as.data.frame(d), family = gaussian(),
                   algorithm = "em")))
  fit0 <- suppressMessages(suppressWarnings(
    birdie::birdie(rp, BirdieFml0, data = as.data.frame(d), family = gaussian(),
                   algorithm = "em")))
  ey <- as.numeric(coef(fit))
  ey0 <- as.numeric(coef(fit0))
  lp <- fit$linpred
  # Weights: BIRDiE's posterior P(R = Black | Y, X, names, county), which
  # estimates the mean over Black players under the model; the prior P(Black)
  # would do so only if it were calibrated given X
  w <- as.data.frame(fit$p_ryxs)[[2]]
  c(ey_white = ey[1], ey_black = ey[2], ey_other = ey[3], marg_gap = ey[2] - ey[1],
    marg_gap0 = ey0[2] - ey0[1],
    cond_gap = weighted.mean(lp[, 2] - lp[, 1], w),
    pw_gap = weighted.mean(d[[YVet]], rp$pr_black) - weighted.mean(d[[YVet]], rp$pr_white))
}
BirdiePoint <- birdie_fit(seq_len(nrow(BirdieData)))

# Player-cluster bootstrap (birdie's em_boot does not return replicate
# coefficients, so the conditional gap needs its own resampling): resample
# players with replacement and refit by EM; failed replications are dropped.
# The draws are made up front, so the results do not depend on the number of
# forked workers (Cores)
set.seed(20261003)
BirdieRowsByPlayer <- split(seq_len(nrow(BirdieData)), BirdieData$gsis_id)
BirdieDraws <- map(seq_len(BootReps), \(r) {
  draw <- sample(names(BirdieRowsByPlayer), length(BirdieRowsByPlayer), replace = TRUE)
  unlist(BirdieRowsByPlayer[draw], use.names = FALSE)
})
T0Birdie <- Sys.time()
BirdieBoot <- parallel::mclapply(BirdieDraws, \(idx) {
  tryCatch(birdie_fit(idx), error = \(e) NULL)
}, mc.cores = Cores) |>
  compact() |>
  map_dfr(as_tibble_row)
message(glue("12: BIRDiE bootstrap, {nrow(BirdieBoot)} of {BootReps} replications in ",
             "{round(difftime(Sys.time(), T0Birdie, units = 'secs'))}s"))

# Regression calibration with the same X (common slopes; SE clustered by
# player), and the main specification (table 08, column (5)) for reference
BirdieRC <- feols(make_fml(YVet, c("Black", "OtherRace", BirdieX)), data = BirdieData,
                  vcov = ~gsis_id, notes = FALSE)
rc_row <- function(m) coeftable(m)["Black", 1:2]
BirdieTerms <- c("ey_white", "ey_black", "ey_other", "marg_gap", "marg_gap0", "pw_gap",
                 "cond_gap")
Res26 <- tibble(
  term = c(BirdieTerms, "rc_same_x", "rc_main"),
  Row = c("White", "Black", "Other race", "Black $-$ white",
          "Black $-$ white, intercept-only BIRDiE (no X)",
          "Black $-$ white, probability-weighted means",
          "BIRDiE conditional gap",
          "Regression calibration, same X",
          "Regression calibration, main specification (Table \\ref{tab:pay-gap-veteran}, column (5))"),
  estimate = c(BirdiePoint[BirdieTerms],
               rc_row(BirdieRC)[[1]], rc_row(Models08[["(5)"]])[[1]]),
  std_error = c(map_dbl(BirdieTerms, \(k) sd(BirdieBoot[[k]], na.rm = TRUE)),
                rc_row(BirdieRC)[[2]], rc_row(Models08[["(5)"]])[[2]]),
  se_type = c(rep("bootstrap", length(BirdieTerms)), "cluster", "cluster"))

Tab26 <- Res26 |>
  transmute(Row, Estimate = fmt_pg(estimate, 3), SE = fmt_se_pg(std_error, 3)) |>
  kbl(format = "latex", booktabs = TRUE, escape = FALSE, linesep = "", align = "lrr",
      col.names = c("", "Log APY", "SE"),
      caption = paste0("Race gap in veteran-contract pay: BIRDiE cross-check",
                       if (!is_primary_measure(measure)) paste0(" (", measure, " race measure)"),
                       " \\label{tab:pay-gap-birdie}")) |>
  kable_styling(latex_options = c("hold_position")) |>
  pack_rows("A. Mean log APY by race, E[Y $|$ R] (BIRDiE)", 1, 6, escape = FALSE) |>
  pack_rows("B. Conditional Black-white gap", 7, 9) |>
  add_notes(drop_empty(c(
    glue("This table compares the regression-calibration estimates of equation (1) with BIRDiE \\citep{{mccartan2025birdie}}, which treats race as a latent variable whose prior is the race prediction (P(white), P(Black), P(other race)) and fits a Normal linear model of log APY with race-specific coefficients by EM. ",
         "The sample is the main sample of Table \\ref{{tab:pay-gap-veteran}}: {fmt_pg(nrow(BirdieData), 0)} freely bargained veteran contracts (UFA and extensions) of {fmt_pg(n_distinct(BirdieData$gsis_id), 0)} players signed 2014-2026."),
    glue("X is parsimonious: OTC market position group, signing-year bucket (2014-16, 2017-19, 2020-22, 2023-26), experience bin, the race prior's covariates (position at NFL entry, entry era, draft-round bucket, college type, whether a home county is known{if (measure == 'preddoc') ', whether a Wikipedia article was found, career length' else ''}), prior-season games played and games started, career games, log draft pick with missing and undrafted indicators, and the athletic score with its missing indicator."),
    "Panel A reports BIRDiE's finite-sample estimates of mean log APY by race and their difference (the raw gap, without conditioning on position or year), from the model with X and from an intercept-only model, which does not depend on the race-specific X model; and, for comparison, the difference between means weighted by P(Black) and by P(white), which mixes the groups when the probabilities are not 0 or 1 and also picks up any relation between the prior covariates and pay, so its bias has no definite sign.",
    "Panel B: the BIRDiE conditional gap is the mean over contracts, weighted by BIRDiE's posterior probability of being Black given pay, X, names and county, of $X_i'(\\hat\\beta_{Black} - \\hat\\beta_{white})$, the gap at the characteristics of Black players allowing every coefficient to differ by race. The regression-calibration rows regress log APY on P(Black), P(other race) and controls with common slopes: the same X, and the main specification (position $\\times$ year-signed fixed effects, blocks A-D and the prior-covariate fixed effects).",
    "Both estimators assume that first name, surname and home county are independent of pay given race and X, and both treat the predicted probabilities as P(race $|$ names, county, X), i.e. as calibrated given X, although X includes characteristics (games, draft slot, athletic score) that are not in the prior; regression calibration in addition assumes common slopes.",
    if (is_pred(measure)) "With correctly specified models and probabilities calibrated given X, the BIRDiE raw gaps with and without X would agree, as would the conditional BIRDiE and regression-calibration gaps up to the difference between race-specific and common slopes; where they differ, the differences point to sensitivity to the race-specific X model or to probabilities that are not calibrated given X, so the table is a diagnostic rather than an independent estimate of the gap." else "",
    if (!is_pred(measure)) glue("Under the {measure} measure the race regressors are 0/1 indicators, so BIRDiE reduces to race-specific regressions and the probability-weighted means are group means.") else "",
    glue("BIRDiE standard errors come from a player-cluster bootstrap with {nrow(BirdieBoot)} successful replications of {BootReps} (players resampled with replacement; BIRDiE refit by EM in each); regression-calibration standard errors are clustered at the player level."),
    RefNote, race_measure_note(measure, "person"))))
save_exhibit_tex(Tab26, "table-26-pay-gap-birdie", measure)
save_estimates(mutate(Res26, boot_reps = nrow(BirdieBoot), nobs = nrow(BirdieData)) |>
                 select(-Row), "12-pay-gap-birdie", measure)

# ---------------------------------------------------------------------------
# Table 27: columns (1) and (5) of table 08 under each race measure
# ---------------------------------------------------------------------------

tick("table 27")

# Main sample before any race restriction, with the column-(5) design
Base27 <- PayContracts |>
  filter(BargainedMarket == 1, !is.na(.data[[YVet]])) |>
  mutate(DraftRound = as.character(DraftRound)) |>
  build_design("contracts", blocks = Cols5, cat_vars = ContractCats)

# Measures: each base measure of person_race_regressors(), and variants of the
# predicted posterior read from race_predicted (raked to the 2010-2015 TIDES
# player shares, draft-free prior, Black or multiracial); hand codes only
# when they exist. A variant whose columns are absent is skipped
HasHand <- any(!is.na(PayContracts$black_any))
Measures27 <- list(
  list(key = "predicted", head = "Predicted", base = "predicted"),
  list(key = "raked", head = "Raked", base = "predicted",
       black = "p_black_pred_raked", white = "p_white_pred_raked"),
  list(key = "nodraft", head = "No draft", base = "predicted",
       black = "p_black_pred_nodraft", white = "p_white_pred_nodraft", draft = FALSE),
  list(key = "blackmulti", head = "Black+multi", base = "predicted",
       black = "p_black_or_multi_pred", white = "p_white_pred"),
  list(key = "preddoc", head = "PredDoc", base = "preddoc"),
  list(key = "provisional", head = "Prov.", base = "provisional"),
  list(key = "hand", head = "Hand", base = "hand"))
Available27 <- map_lgl(Measures27, \(m) {
  if (m$key == "hand") return(HasHand)
  all(c(m$black, m$white) %in% names(Base27$df))
})
Skipped27 <- map_chr(Measures27[!Available27], "key")
Measures27 <- Measures27[Available27]
if (length(Skipped27) > 0) message(glue("12: table 27 skips {paste(Skipped27, collapse = ', ')}"))

# Race regressors of one measure on the main sample (rows with a race measure)
race_data27 <- function(m) {
  d <- person_race_regressors(Base27$df, m$base)
  if (!is.null(m$black)) {
    d <- d |> mutate(Black = .data[[m$black]], PWhite = .data[[m$white]],
                     OtherRace = pmax(1 - Black - PWhite, 0), RaceKnown = !is.na(Black))
  }
  filter(d, RaceKnown)
}
Models27 <- list()
Info27 <- list()
for (m in Measures27) {
  d <- race_data27(m)
  prior <- prior_cols(m$base, d, draft = m$draft %||% TRUE)
  controls <- unlist(map(Base27$cols[Cols5], \(cc) varying(d, cc)), use.names = FALSE)
  Models27[[paste(m$head, "(1)")]] <- feols(make_fml(YVet, c(Rhs0, prior), FE08), data = d,
                                            vcov = ~gsis_id, notes = FALSE)
  Models27[[paste(m$head, "(5)")]] <- feols(make_fml(YVet, c(Rhs0, prior, controls), FE08),
                                            data = d, vcov = ~gsis_id, notes = FALSE)
  Info27[[m$key]] <- tibble(key = m$key, head = m$head, base = m$base, mean_black = mean(d$Black),
                            n_players = n_distinct(d$gsis_id),
                            prior = if (length(prior) == 0) "No" else
                              if (isFALSE(m$draft)) "No draft" else "Yes")
}
Info27 <- bind_rows(Info27)
# The script's measure's main-specification estimate must match table 08
# column (5) whenever that measure is among the table-27 columns
Idx27 <- which(map_chr(Measures27, "key") == measure)
if (length(Idx27) == 1) {
  stopifnot(abs(coef(Models27[[paste(Measures27[[Idx27]]$head, "(5)")]])[["Black"]] -
                  coef(Models08[["(5)"]])[["Black"]]) < 1e-6)
}

Rows27 <- tibble(term = c("Race measure", "Specification", "Race-prior covariate FE",
                          "Mean of race regressor", "Players"))
for (i in seq_len(nrow(Info27))) {
  for (sp in c("(1)", "(5)")) {
    Rows27[[paste(Info27$head[i], sp)]] <- c(
      Info27$head[i],
      if (sp == "(5)") "Main" else if (is_pred(Info27$base[i])) "Prior FE only" else "Raw",
      Info27$prior[i], fmt_pg(Info27$mean_black[i], 3), fmt_pg(Info27$n_players[i], 0))
  }
}
# Column headers: (1), (2), ... (the measure is in the first added row)
Heads27 <- names(Models27)
names(Models27) <- paste0("(", seq_along(Models27), ")")
names(Rows27) <- c("term", names(Models27))

MeasureNotes27 <- c(
  predicted = "Predicted: P(non-Hispanic Black alone) and P(white) from the primary model-only prediction (BIFSG name and county likelihood times an NFL prior estimated by EM on predetermined characteristics).",
  raked = "Raked: the predicted posterior with the Black log-odds shifted so that the mean matches published TIDES player shares for 2010-2015 (from race\\_predicted, attached by the sample script since load\\_person\\_race() does not return it); it fixes the level of the prediction, not its ranking.",
  nodraft = "No draft: the predicted posterior with a prior that omits the draft-round bucket; its prior fixed effects omit the bucket too.",
  blackmulti = "Black+multi: P(Black) + P(multiracial) as the Black regressor, since the multiracial category is weakly separated from Black by names.",
  preddoc = "PredDoc: documented race where a public source states it (Black alone or in combination), else the model prediction (non-Hispanic Black alone); documentation depends on fame, and its prior adds whether a Wikipedia article exists and career length.",
  provisional = "Prov.: the Wikipedia category flag (positive-only), on players with an article; the comparison group mixes white and unflagged Black players, so the estimates are attenuated.",
  hand = "Hand: hand-coded race (notes/race-coding-protocol.md).")
write_model_table(
  Models27, c(Black = "Black / P(Black)", OtherRace = "Other / P(other race)"),
  title = "Race gap in veteran-contract pay under alternative race measures",
  label = "pay-gap-race-measures",
  notes = drop_empty(c(
    "This table re-estimates columns (1) and (5) of Table \\ref{tab:pay-gap-veteran} (equation (1)) under each race measure. Odd columns include OTC position $\\times$ year-signed fixed effects and, under the predicted measures, the fixed effects of the measure's prior covariates; even columns add the control blocks A-D of column (5). The sample is the main sample of freely bargained veteran contracts (UFA and extensions) signed 2014-2026, restricted to players with the measure.",
    "Under the predicted measures (all but Prov. and Hand) the race regressors are probabilities (regression calibration) and every column, odd columns included, conditions on the covariates of the measure's prior (position at NFL entry, entry era, draft-round bucket, college type, whether a home county is known; the No-draft prior omits the draft-round bucket). The odd columns are therefore raw gaps only for Prov. and Hand, and they differ across measures in their conditioning set as well as in the race measure: the Predicted, Raked and Black+multi odd columns condition on draft round, the No-draft odd column does not, and the Prov. odd column has no prior fixed effects.",
    "The PredDoc prior also includes whether a Wikipedia article was found and career length, which are outcomes of the career (career length is partly an outcome of pay), so both PredDoc columns condition on post-treatment variables.",
    MeasureNotes27[Info27$key],
    if ("raked" %in% Skipped27) "The raked variant is not reported because race\\_predicted has no raked column." else "",
    if (!HasHand) "Hand-coded race is not yet available, so no hand-coded column is reported." else "",
    "The mean of the race regressor is the mean P(Black) (or share flagged or coded Black) in the estimation sample.",
    NoteCluster)),
  name = "table-27-pay-gap-race-measures", measure = measure,
  add_rows = Rows27, font_size = 8)
save_estimates(tidy_terms(set_names(Models27, Heads27), Rhs0) |>
                 mutate(table = "table-27"), "12-pay-gap-race-measures", measure)

# ---------------------------------------------------------------------------
# Tidy coefficient file and runtime
# ---------------------------------------------------------------------------

save_estimates(bind_rows(Estimates) |> relocate(table), "12-pay-gap", measure)
message(glue("12: rows dropped for unknown race: ",
             paste(names(DroppedRace), map_chr(DroppedRace, \(x) glue("{x[['dropped']]} of {x[['n']]}")),
                   sep = " ", collapse = "; ")))
tick("done")

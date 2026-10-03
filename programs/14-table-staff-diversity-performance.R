# ============================================================================
# 14-table-staff-diversity-performance.R
# Estimates the staff-diversity specifications of notes/analysis-plan.md,
# section 3: how do teams with more racially diverse coaching staffs and
# front offices perform? Equation (3), for franchise f in season t:
#   Y_ft = beta ShareBlackCoaches_ft + theta HCBlack_ft + X_ft pi
#          + alpha_f + gamma_t + e_ft
# Exhibits ([-<measure>] suffix under a non-primary race measure):
#   - table-19-staff-diversity-team-season: main specification, columns
#     (1)-(7), incl. franchise x head-coach-spell FE (singletons removed);
#     (8) column (4) with the opposite prior-control choice; (9) column (4)
#     plus the other probability measure's share and head coach
#   - table-19b-staff-diversity-outcomes: point differential, offensive and
#     defensive EPA per play under the column-(4) and column-(5) specifications
#   - table-19c-staff-diversity-controls: predetermined (lagged) roster
#     controls vs. opening-day and contemporaneous (potentially post-treatment)
#     roster controls, with and without the coaching-turnover shares
#   - table-20-staff-diversity-by-group: staff groups one at a time, jointly,
#     and the two-group Blau index of coaches
#   - table-21-staff-diversity-unit-stacked: offense and defense stacked,
#     unit coordinator and unit coaches (franchise x season and unit x season FE)
#   - table-22-hc-hire-race: head-coach hires (between-season changes and
#     retained interim coaches; returns and stand-in seasons excluded),
#     outcomes on the race of the hired (first-game) head coach; columns
#     (7)-(8) the hired coach's documented race (validation); the lagged
#     win percentage enters freely in the headline columns (2) and (5)
#   - table-23-staff-diversity-placebo: one-season leads and selection on
#     past performance
#   - table-29-staff-diversity-race-measures: the headline specifications
#     (table 19 column (4) without and with prior controls, columns (5) and
#     (9), table 22 column (2), table 21 column (2)) under each race measure
#     (primary run only)
#   - figure-staff-diversity-coefficients: staff-group coefficients (table 20)
# Race: one measure for tables 19-23, from choose_race_measure() on the mean
# CodedShareCoaches over FullStaffObserved team-seasons; without hand codes
# the primary measure is predicted race, so shares are expected shares and
# role-holder regressors (HCBlack, OCBlack, ...) are the holder's P(Black).
# Under the model-only predicted measure every specification controls for
# the model-only priors matched to each race regressor (regression
# calibration; add_prior()): MeanPriorBlackPred<G> for a share,
# <role>PriorBlackPred for a role holder.
# Inference: clustered by franchise (32 clusters) with wild cluster
# bootstrap-t p-values (wild_cluster_test(), Webb weights, NFL_WCB_REPS
# replications, default 9999).
# Inputs: analysis/analysis_team_season.parquet,
# analysis/analysis_team_unit_season.parquet,
# analysis/analysis_team_game.parquet (programs/11; game head coaches and
# opening-day starting QBs), analysis/player_season.parquet (programs/04;
# the opening-day QB's draft pick), load_person_race() (DuckDB, read-only:
# staff documented race and model-only P(Black), for validation).
# Outputs: output/tables/table-19, 19b, 19c, 20 ... 23, 29 (.tex), output/figures/
# figure-staff-diversity-coefficients (.pdf/.png),
# output/estimates/14-staff-diversity[-provisional].csv.
# Date: 2026-10-02 (predicted race and table 29 added the same day)
# ============================================================================

T0Script <- Sys.time()

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

TeamSeasonAll <- read_parquet(file.path(analysis, "analysis_team_season.parquet"))
UnitSeasonAll <- read_parquet(file.path(analysis, "analysis_team_unit_season.parquet"))
stopifnot(!anyDuplicated(TeamSeasonAll[c("franchise_id", "season")]),
          !anyDuplicated(UnitSeasonAll[c("franchise_id", "season", "Unit")]))
# REG games: game head coach (for the hire design) and starting QB (for the
# unit design's opening-day QB)
TeamGameReg <- read_parquet(file.path(analysis, "analysis_team_game.parquet"),
                            col_select = c(franchise_id, season, season_type, week, gameday,
                                           HeadCoachPersonId, HeadCoachName, starting_qb_id)) |>
  filter(season_type == "REG")
QBDraft <- read_parquet(file.path(analysis, "player_season.parquet"),
                        col_select = c(gsis_id, season, DraftPick))
stopifnot(!anyDuplicated(QBDraft[c("gsis_id", "season")]))
# Staff persons' documented race (validation of the predicted measure only:
# the calibration of head-coach probabilities and the documented-race
# columns of table 22) and model-only P(Black), from load_person_race()
con <- db_connect()
StaffDoc <- load_person_race(con, hand_coded) |>
  filter(entity == "staff") |>
  transmute(PersonId = person_id, DocBlack = documented_black_any,
            PBlackPred = p_black_any_pred)
db_disconnect(con)
stopifnot(!anyDuplicated(StaffDoc$PersonId))

# ---------------------------------------------------------------------------
# Race measure (one for tables 19-23)
# ---------------------------------------------------------------------------

# Coverage = mean share of on-field coaches with a hand code over the
# FullStaffObserved team-seasons (2007-2025)
HandCoverage <- TeamSeasonAll |>
  filter(FullStaffObserved) |>
  pull(CodedShareCoaches) |>
  mean(na.rm = TRUE)
measure <- choose_race_measure(HandCoverage, "14 staff diversity")
# IsPredMeasure: race regressors are probabilities (predicted or preddoc);
# IsModelPred: the model-only predicted measure, the only one for which the
# model-only prior is the regression-calibration control (under preddoc a
# documented person's probability is 1 or 0 whatever the prior)
IsPredMeasure <- measure %in% c("predicted", "preddoc")
IsModelPred <- measure == "predicted"
# Unmapped inputs, kept for the race-measure comparison (table 29)
TeamSeasonRaw <- TeamSeasonAll
UnitSeasonRaw <- UnitSeasonAll

# Wild cluster bootstrap replications
WcbReps <- as.integer(Sys.getenv("NFL_WCB_REPS", "9999"))
stopifnot("NFL_WCB_REPS must be at least 100 (fwildclusterboot minimum)" = WcbReps >= 100)

# ---------------------------------------------------------------------------
# Derived variables on the full panel (1999-2025)
# ---------------------------------------------------------------------------

# Map one race measure to the generic names and build the derived variables
# of the team-season panel (a function, so that table 29 can rebuild the
# panel under each measure)
prep_team_season <- function(df, m) {
  df <- apply_race_measure(df, m)

  # Head-coach spell: a run of consecutive seasons of one HCPersonId with a
  # franchise (a coach who returns after a gap starts a new spell); built on
  # the full panel so that spells starting before 2007 keep their identity
  df <- df |>
    arrange(franchise_id, season) |>
    group_by(franchise_id) |>
    mutate(NewSpell = coalesce(HCPersonId != lag(HCPersonId) |
                                 season != lag(season) + 1L, TRUE),
           HCSpellId = paste(franchise_id, cumsum(NewSpell), sep = "-")) |>
    ungroup() |>
    select(-NewSpell)

  # Two-group Blau index of on-field coaches under the measure (under a
  # predicted measure, the plug-in 2s(1-s) of the expected share, not the
  # expected Blau index)
  df <- mutate(df, BlauBlackCoaches = blau_two_group(ShareBlackCoaches))

  # The other probability measure, for the horse-race column (9) of table 19:
  # the documented-race (preddoc) share and head coach under the predicted
  # measure, and the model-only predicted ones under every other measure
  alt <- if (m == "predicted") "PredDoc" else "Pred"
  df <- mutate(df, ShareBlackAltCoaches = .data[[paste0("ShareBlack", alt, "Coaches")]],
               HCBlackAlt = .data[[paste0("HCBlack", alt)]])

  # Role holders can be missing (no GM title, no OC listed, or an uncoded
  # holder under the hand measure): zero-fill each role regressor (an
  # indicator, or the holder's P(Black) under a predicted measure) and add a
  # <role>BlackMiss indicator rather than dropping the team-season. The raw
  # values are kept as <role>BlackRaw for the hire design and the counts.
  # (The F1* leads stay unfilled: a missing lead drops the row in the placebo.)
  # The holders' model-only priors (prior controls) are zero-filled the same way.
  for (v in RoleVars) df[[paste0(v, "Raw")]] <- df[[v]]
  df <- fill_missing(df, c(RoleVars, "HCBlackAlt", RolePriors))

  # TeamCapShare is NA before 2013: zero-fill it so that 2007-2012 stay in.
  # Its missing indicator is a function of season and is absorbed by the
  # season fixed effects, so it never enters a formula. ShareBlackRoster is
  # zero-filled with a missing indicator (it can be gated to NA under the
  # hand measure).
  df <- fill_missing(df, c("TeamCapShare", "ShareBlackRoster"))

  # Predetermined roster controls: season t-1 values of the game-day roster's
  # mean log draft pick, mean age and cap share (the t-1 roster Black share
  # is L1ShareBlackRoster from 11, coverage-gated there under the hand
  # measure). The contemporaneous season-t versions respond to in-season
  # call-ups, benchings and signings and are chosen by the staff, so they
  # are kept only for a robustness table. LagTeamCapShare is NA before 2014
  # and zero-filled; its missing indicator is a function of season (absorbed
  # by season FE).
  df <- df |>
    group_by(franchise_id) |>
    mutate(across(c(MeanLogPickRoster, MeanAgeRoster, TeamCapShare),
                  \(x) lag_within(x, season), .names = "Lag{.col}")) |>
    ungroup()
  # (the roster shares' model-only priors are zero-filled with them)
  fill_missing(df, c("LagTeamCapShare", "LagMeanLogPickRoster", "LagMeanAgeRoster",
                     "L1ShareBlackRoster", "ShareBlackWeek1", "MeanLogPickWeek1",
                     "MeanAgeWeek1", "L1MeanPriorBlackPredRoster",
                     "MeanPriorBlackPredWeek1", "MeanPriorBlackPredRoster"))
}
RoleVars <- c("HCBlack", "OCBlack", "DCBlack", "STCBlack", "GMBlack")
RolePriors <- c("HCPriorBlackPred", "OCPriorBlackPred", "DCPriorBlackPred",
                "STCPriorBlackPred", "GMPriorBlackPred")
TeamSeasonAll <- prep_team_season(TeamSeasonRaw, measure)

# Estimation sample: full staff observed (template era, 2007-2025)
TeamSeason <- filter(TeamSeasonAll, FullStaffObserved)
NClusters <- n_distinct(TeamSeason$franchise_id)
message(glue("14: {nrow(TeamSeason)} FullStaffObserved team-seasons, ",
             "{min(TeamSeason$season)}-{max(TeamSeason$season)}, {NClusters} franchises"))
stopifnot(NClusters == 32, all(table(TeamSeason$season) == 32), min(TeamSeason$season) == 2007)

# Within-franchise SD of the coaches' Black share (net of franchise means)
WithinSDCoaches <- TeamSeason |>
  filter(!is.na(ShareBlackCoaches)) |>
  group_by(franchise_id) |>
  mutate(Dev = ShareBlackCoaches - mean(ShareBlackCoaches)) |>
  ungroup() |>
  pull(Dev) |>
  sd()
message(glue("14: within-franchise SD of ShareBlackCoaches = {round(WithinSDCoaches, 4)}"))

# ---------------------------------------------------------------------------
# Estimation helpers
# ---------------------------------------------------------------------------

# Formula y ~ rhs | fe
make_fml14 <- function(y, rhs, fe = "") {
  f <- paste(y, "~", paste(rhs, collapse = " + "))
  if (fe != "") f <- paste(f, "|", fe)
  as.formula(f)
}

# Fit equation (3)-type models with feols, clustered by franchise, on the
# complete cases of the outcome, the regressors and the FE variables.
# Regressors without variation in that sample (e.g. a missing indicator that
# is always 0) and regressors fixest reports as collinear are dropped, so the
# lm refit in wild_cluster_test() has no aliased coefficients. The estimation
# data are kept in m$EstData (after singleton removal when
# drop_singletons = TRUE, so that N and the outcome mean exclude
# observations perfectly fit by a fixed effect).
# prior = TRUE appends the model-only prior controls of the race regressors
# in rhs (add_prior()); m$PriorUsed records it.
fit14 <- function(y, rhs, fe, data, cluster = "franchise_id", drop_singletons = FALSE,
                  prior = FALSE) {
  if (prior) rhs <- add_prior(rhs, data)
  fe_vars <- unique(unlist(strsplit(fe, "[\\^+ ]+")))
  fe_vars <- fe_vars[fe_vars != ""]
  d <- data |> filter(if_all(all_of(unique(c(y, rhs, fe_vars, cluster))), \(x) !is.na(x)))
  # Iteratively drop observations whose FE level has a single observation
  if (drop_singletons) {
    repeat {
      single <- map(fe_vars, \(f) d[[f]] %in% names(which(table(d[[f]]) == 1))) |>
        reduce(`|`)
      if (!any(single)) break
      d <- d[!single, , drop = FALSE]
    }
  }
  rhs <- rhs[map_lgl(rhs, \(v) n_distinct(d[[v]]) > 1)]
  m <- feols(make_fml14(y, rhs, fe), data = d, vcov = as.formula(paste0("~", cluster)),
             notes = FALSE)
  if (length(m$collin.var) > 0) {
    message("14: dropping collinear regressors: ", paste(m$collin.var, collapse = ", "))
    rhs <- setdiff(rhs, m$collin.var)
    m <- feols(make_fml14(y, rhs, fe), data = d, vcov = as.formula(paste0("~", cluster)),
               notes = FALSE)
  }
  stopifnot(nobs(m) == nrow(d))
  m$EstData <- d
  m$FeSpec <- fe
  m$PriorUsed <- prior
  m
}

# Prior controls (regression calibration under the predicted measure,
# notes/race-prediction-design.md, section 4): the model-only prior P(Black)
# matched to each race regressor. A group share (ShareBlack<G>, its lead
# F1ShareBlack<G> or lag L1ShareBlack<G>) gets the members' mean prior
# MeanPriorBlackPred<G> (the team-level summary of the prior's covariates:
# first role, unit and first-season era for staff); a role holder (HC, OC,
# DC, GM, unit coordinator, opening-day QB and their unit interactions) gets
# the holder's prior <role>PriorBlackPred. The Blau index and the column-(9)
# alternative-measure regressors get the coaches' and head coach's priors.
# The Pred-named columns are the model-only prior under every measure.
prior_of <- function(v) {
  case_when(grepl("Miss$", v) ~ NA_character_,
            v %in% c("BlauBlackCoaches", "ShareBlackAltCoaches") ~ "MeanPriorBlackPredCoaches",
            v == "HCBlackAlt" ~ "HCPriorBlackPred",
            grepl("^(F1|L1)?ShareBlack[A-Z]", v) ~
              sub("^(F1|L1)?ShareBlack(.*)$", "\\1MeanPriorBlackPred\\2", v),
            grepl("^(F1)?(HC|OC|DC|STC|GM|QBWeek1|UnitCoord|OffQBWeek1|DefHC)Black$", v) ~
              sub("Black$", "PriorBlackPred", v),
            TRUE ~ NA_character_)
}
# rhs plus the prior controls of its race regressors (and their zero-fill
# missing indicators, where they exist). A race regressor without a prior
# column (the F1 role-holder leads) is reported and left without one.
PriorMissingLog <- character()
add_prior <- function(rhs, data) {
  v <- rhs[!is.na(prior_of(rhs))]
  p <- prior_of(v)
  absent <- setdiff(p, names(data))
  if (length(absent) > 0) PriorMissingLog <<- union(PriorMissingLog, absent)
  keep <- p %in% names(data)
  # A prior's missing indicator is skipped when it duplicates the race
  # regressor's own (the usual case: both are missing without a holder)
  miss <- map2_chr(v[keep], p[keep], \(vv, pp) {
    pm <- paste0(pp, "Miss")
    vm <- paste0(vv, "Miss")
    if (!pm %in% names(data) ||
        (vm %in% names(data) && isTRUE(all(data[[pm]] == data[[vm]])))) NA_character_ else pm
  })
  unique(c(rhs, p[keep], na.omit(miss)))
}

# Use the prior controls in the tables 19-23 specifications: under the
# model-only predicted measure only (UsePrior); column (8) of table 19
# reverses this choice for comparison
UsePrior <- IsModelPred

# SD of the coaches' Black share net of a model's fixed effects in its
# estimation sample (the identifying variation of that column)
resid_sd <- function(m, x = "ShareBlackCoaches") {
  sd(resid(feols(make_fml14(x, "1", m$FeSpec), data = m$EstData, notes = FALSE)))
}

# Effect of a 1-SD within change of the coaches' share, in win percentage
# points for WinPct and in the outcome's own units otherwise
sd_effect <- function(m, x = "ShareBlackCoaches") {
  y <- as.character(m$fml[[2]])
  scale <- if (y == "WinPct") 100 else 1
  fmt14(coef(m)[[x]] * resid_sd(m, x) * scale, if (grepl("EPA", y)) 4 else 3)
}

# Wild cluster bootstrap-t for `param` in model m (NA row when the term is
# not estimated). A failed bootstrap stops the script, so that no table
# reports blank WCB cells under a note that claims them. The elapsed time
# is recorded.
WcbLog <- list()
wcb <- function(m, param, model_name = "", table = "") {
  empty <- tibble(term = param, estimate = NA_real_, p_wcb = NA_real_,
                  ci_low = NA_real_, ci_high = NA_real_, n_clusters = NA_integer_,
                  B = WcbReps)
  if (!param %in% names(coef(m))) return(mutate(empty, model = model_name, table = table))
  t0 <- Sys.time()
  res <- tryCatch(wild_cluster_test(m, m$EstData, param, "franchise_id", B = WcbReps),
                  error = \(e) {
                    stop(glue("14: WCB failed for {param} ({table} {model_name}): ",
                              conditionMessage(e)), call. = FALSE)
                  })
  stopifnot(!is.na(res$p_wcb))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  WcbLog[[length(WcbLog) + 1]] <<- tibble(table = table, model = model_name,
                                          term = param, seconds = secs)
  mutate(res, model = model_name, table = table)
}

# WCB results for each model x param (params absent from a model give NA)
wcb_all <- function(models, params, table) {
  imap_dfr(models, \(m, nm) map_dfr(params, \(p) wcb(m, p, nm, table)))
}

# add_rows lines "WCB p-value: <label>" for each param, one column per model
wcb_rows <- function(wcb_res, models, labels) {
  map_dfr(names(labels), \(p) {
    row <- tibble(term = paste0("WCB $p$-value: ", labels[[p]]))
    for (nm in names(models)) {
      pv <- filter(wcb_res, model == nm, term == p)$p_wcb
      row[[nm]] <- fmt_p(if (length(pv) == 0) NA_real_ else pv[1])
    }
    row
  })
}

# One add_rows line built from a function of each model
model_row <- function(models, label, f) {
  row <- tibble(term = label)
  for (nm in names(models)) row[[nm]] <- f(models[[nm]])
  row
}

# Number formatting
fmt14 <- function(x, digits = 3) {
  if_else(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}
yes_no14 <- function(cond) if_else(cond, "Yes", "No")
fmt_p14 <- function(p) if_else(!is.na(p) & p < 0.001, "$<$0.001", fmt_p(p))

# Mean of the outcome in a model's estimation sample
outcome_mean <- function(m, digits = 3) {
  fmt14(mean(m$EstData[[as.character(m$fml[[2]])]]), digits)
}

# Shared table-note text
NoteCluster <- glue("Standard errors, in parentheses, are clustered at the franchise level ({NClusters} clusters).")
NoteWcb <- glue("Wild cluster bootstrap-t $p$-values (restricted, Webb six-point weights, ",
                "{format(WcbReps, big.mark = ',')} replications, clustered by franchise) are ",
                "reported in the bottom panel because 32 clusters are few.")
# Role-holder note by measure: role regressors are 0/1 under hand and
# provisional race and the holder's P(Black) under a predicted measure
role_note <- function(m) {
  pred <- m %in% c("predicted", "preddoc")
  paste(if (pred) "Role-holder regressors (HC, OC, DC, GM) are the holder's predicted probability of being Black;"
        else "Role-holder indicators (HC, OC, DC, GM)",
        if (pred) "they are zero-filled when the role has no listed holder (e.g. teams without a GM title)"
        else "are zero-filled when the role has no listed holder (e.g. teams without a GM title) or the holder's race is unknown",
        "and each enters with a missing indicator, so no team-season is dropped for a missing role holder.")
}
NoteRoles <- role_note(measure)
NotePredShort <- if (IsModelPred) paste(
  "Under the predicted measure, staff shares are expected shares (mean member P(Black)),",
  "role-holder regressors are the holder's P(Black), and every specification controls for",
  "the matching model-only priors (the members' mean prior for each share, the holder's",
  "prior for each role holder); see the notes to Table 19 on their measurement and Table 29",
  "for the other race measures.") else if (IsPredMeasure) paste(
  "Under the preddoc measure, staff shares are expected shares and role-holder regressors are",
  "the holder's probability (1 or 0 when documented); see the notes to Table 19 and Table 29.") else character()

# Head-coach measurement diagnostics, computed from the data (model-only
# P(Black) under every measure): among the distinct head coaches of the
# estimation sample, the reliability Var(p)/(Var(p) + E[p(1-p)]) of P(Black)
# (the share of its variance that is signal if p is calibrated) and the mean
# P(Black) of documented Black head coaches
HCDiag <- TeamSeason |>
  distinct(HCPersonId) |>
  filter(!is.na(HCPersonId)) |>
  left_join(StaffDoc, by = c("HCPersonId" = "PersonId"))
stopifnot(!anyNA(HCDiag$PBlackPred))
HCReliability <- with(HCDiag, var(PBlackPred) / (var(PBlackPred) + mean(PBlackPred * (1 - PBlackPred))))
NHCDiag <- nrow(HCDiag)
NHCDocBlack <- sum(HCDiag$DocBlack == 1L, na.rm = TRUE)
MeanPHCDocBlack <- mean(HCDiag$PBlackPred[coalesce(HCDiag$DocBlack == 1L, FALSE)])
message(glue("14: {NHCDiag} head coaches; reliability of P(Black) {round(HCReliability, 3)}; ",
             "{NHCDocBlack} documented Black, mean P(Black) {round(MeanPHCDocBlack, 3)}"))
NoteHCDiag <- glue(
  "Validation (Tables 24 and 25): predicted assistant-coach Black shares fall below the published ",
  "TIDES shares, and head-coach probabilities are noisy at the individual level: among the {NHCDiag} head coaches ",
  "of these team-seasons (each counted once), the reliability Var(p)/(Var(p) + E[p(1-p)]) of the ",
  "model-only P(Black) is {fmt14(HCReliability, 2)}, and the {NHCDocBlack} documented Black head ",
  "coaches average P(Black) of {fmt14(MeanPHCDocBlack, 2)}.")

# Measurement of the coaches' shares and role holders, by measure
NotePredStaff <- if (IsModelPred) paste(
  "Under the predicted measure the coaches' share is an expected share (mean member P(Black))",
  "and role holders enter as their P(Black). Under regression calibration the coefficient on",
  "an expected share estimates the effect of the true share if two conditions hold. First,",
  "the probabilities are calibrated: the measurement error is then of the Berkson type and does",
  "not attenuate the coefficient (uninformative names cost precision only), whereas",
  "miscalibration rescales the coefficient by the slope of the true on the expected share,",
  "which is not estimated here, so magnitudes are not bounds. Second, the information used to",
  "predict race (first name, surname, hometown county and the prior's covariates) is unrelated",
  "to performance given race and the controls. The prior's covariates (first role, unit and",
  "first-season era) proxy experience, so every column except (8) controls for the model-only",
  "priors matched to each race regressor (the coaches' mean prior for the share, the holder's",
  "prior for each role holder); column (8) drops them for comparison. P(Black) is computed person",
  "by person, while race is likely correlated within staffs (homophily in hiring), so the",
  "head-coach and share coefficients are not separately identified under this measure. Column",
  "(9) adds the documented-race (preddoc) share and head-coach regressor: if the predicted",
  "share's coefficient persists conditional on them, it rests on the part of P(Black) that is",
  "unrelated to documented race, which regression calibration cannot read as a race effect.",
  NoteHCDiag) else if (IsPredMeasure) paste(
  "Under the preddoc measure a documented person's probability is 1 or 0 and an undocumented",
  "person's is a model probability with a prior that conditions on fame proxies; because",
  "documentation depends on fame, the measurement error may be correlated with performance.",
  "The model-only prior is not a regression-calibration control for this measure: column (8)",
  "adds the model-only priors as ordinary predetermined controls. Column (9) adds the model-only",
  "predicted share and head-coach P(Black) (with their priors).", NoteHCDiag) else paste(
  "Column (8) adds the model-only priors of the predicted-race model (an ordinary predetermined",
  "control under this measure); column (9) adds the model-only predicted share and head-coach",
  "P(Black) with their priors.")
# Glass cliff: level outcomes (tables 19 and 23)
NoteGlassCliff <- paste("HC race coefficients are subject to the glass-cliff threat: if Black",
                        "head coaches are hired into persistently worse situations than the",
                        "controls capture, the coefficient on a level outcome is biased downward.",
                        "LagWinPct and LagExpectedWins condition on the situation the coach inherits.")

# ---------------------------------------------------------------------------
# Regressor sets
# ---------------------------------------------------------------------------

RoleRhs <- c("HCBlack", "OCBlack", "DCBlack", "GMBlack",
             "HCBlackMiss", "OCBlackMiss", "DCBlackMiss", "GMBlackMiss")
# Main (column 4) controls: predetermined with respect to season t. Lagged
# outcomes, the coaching-turnover shares (staff listed at the start of the
# season) and season t-1 roster quality, cap share and roster Black share.
TurnoverShares <- c("ShareCoachesNewToFranchise", "ShareCoachesPromoted")
LagRoster <- c("LagTeamCapShare", "LagMeanLogPickRoster", "LagMeanAgeRoster",
               "LagMeanLogPickRosterMiss", "LagMeanAgeRosterMiss",
               "L1ShareBlackRoster", "L1ShareBlackRosterMiss")
Controls4 <- c("LagWinPct", "LagExpectedWins", TurnoverShares, LagRoster)
# Robustness: opening-day active roster (predetermined with respect to the
# season's results but chosen by the staff) and the contemporaneous
# game-day-week-weighted roster (potentially post-treatment)
Week1Roster <- c("LagTeamCapShare", "MeanLogPickWeek1", "MeanAgeWeek1",
                 "ShareBlackWeek1", "ShareBlackWeek1Miss")
ContempRoster <- c("TeamCapShare", "MeanLogPickRoster", "MeanAgeRoster",
                   "ShareBlackRoster", "ShareBlackRosterMiss")

# Regressor labels by measure: under a probability measure, shares are
# expected shares and role holders enter as their P(Black)
share_lab <- function(group) paste0(group, if (IsPredMeasure) " expected share Black" else " share Black")
role_lab <- function(role) if (IsPredMeasure) paste0("P(Black): ", role) else paste("Black", role)
AltLab <- if (IsModelPred) "documented (preddoc)" else "predicted (model only)"
CoefMapMain <- c(
  ShareBlackCoaches = share_lab("Coaches'"),
  HCBlack = role_lab("head coach"),
  OCBlack = role_lab("offensive coordinator"),
  DCBlack = role_lab("defensive coordinator"),
  GMBlack = role_lab("general manager"),
  ShareBlackAltCoaches = glue("Coaches' share Black, {AltLab}"),
  HCBlackAlt = glue("P(Black): head coach, {AltLab}"),
  MeanPriorBlackPredCoaches = "Coaches' mean prior P(Black)",
  HCPriorBlackPred = "Head coach's prior P(Black)",
  L1ShareBlackRoster = "Roster share Black ($t-1$)",
  LagWinPct = "Lagged win percentage",
  LagExpectedWins = "Lagged market expected wins",
  ShareCoachesNewToFranchise = "Share of coaches new to franchise",
  ShareCoachesPromoted = "Share of coaches promoted")

# Shared note sentences for table 19 and its companions
NoteShareDef <- paste("The coaches' share includes the head coach and the coordinators.",
                      "Conditional on the role-holder regressors (from column 3 on), its",
                      "coefficient is identified from the composition of the remaining",
                      "coaches, and the role coefficients are net of the role holder's",
                      "own contribution to the share.")
NoteControls4 <- paste("The column-(4) controls are predetermined: lagged win percentage",
                       "and market expected wins, the shares of coaches new to the",
                       "franchise and promoted from within (staff listed at the start of",
                       "the season), and the previous season's cap share (observed from",
                       "2013, so from 2014 here, zero-filled before; its missing",
                       "indicator is absorbed by the season FE), game-day roster mean",
                       "log draft pick, mean age and Black share. Contemporaneous roster",
                       "measures respond to in-season results and to the staff's own",
                       "choices, so they appear only in Table 19c.")
NoteMarket <- paste("Because pre-game betting lines price staff quality and update",
                    "during the season, the wins-over-expected column estimates",
                    "performance relative to market expectations (mispricing), not the",
                    "total effect of staff composition.")

Estimates <- list()
WcbResults <- list()

# ---------------------------------------------------------------------------
# Table 19: main specification (equation (3))
# ---------------------------------------------------------------------------

RoleNoHC <- setdiff(RoleRhs, c("HCBlack", "HCBlackMiss"))
# Columns (1)-(7) and (9) use the prior controls under the predicted measure
# (UsePrior); column (8) is column (4) with the opposite choice (no prior
# controls under the predicted measure, the model-only priors as ordinary
# controls under the others). Column (9) adds the other probability measure's
# coaches' share and head coach to column (4), always with the priors (they
# are the calibration controls of the model-only regressors).
AltRhs <- c("ShareBlackAltCoaches", "HCBlackAlt", "HCBlackAltMiss")
Spec19 <- list(
  "(1)" = list(y = "WinPct", rhs = "ShareBlackCoaches", fe = "season"),
  "(2)" = list(y = "WinPct", rhs = "ShareBlackCoaches", fe = "franchise_id + season"),
  "(3)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", RoleRhs),
               fe = "franchise_id + season"),
  "(4)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", RoleRhs, Controls4),
               fe = "franchise_id + season"),
  "(5)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", RoleNoHC, Controls4),
               fe = "HCSpellId + season", single = TRUE),
  "(6)" = list(y = "WinPct",
               rhs = c("ShareBlackCoaches", RoleNoHC, setdiff(Controls4, "LagWinPct")),
               fe = "HCSpellId + season", single = TRUE),
  "(7)" = list(y = "WinsOverExpected", rhs = c("ShareBlackCoaches", RoleRhs, Controls4),
               fe = "franchise_id + season"),
  "(8)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", RoleRhs, Controls4),
               fe = "franchise_id + season", prior = !UsePrior),
  "(9)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", RoleRhs, Controls4, AltRhs),
               fe = "franchise_id + season", prior = TRUE))
fit_spec19 <- function(s, data, use_prior) {
  fit14(s$y, s$rhs, s$fe, data, drop_singletons = isTRUE(s$single),
        prior = if (is.null(s$prior)) use_prior else s$prior)
}
Models19 <- map(Spec19, \(s) fit_spec19(s, TeamSeason, UsePrior))
map_int(Models19, nobs) |> print()

# Wild cluster bootstrap for the coaches' share and the head coach (and the
# alternative-measure share in column 9, the GM in the spell-FE columns)
Wcb19 <- bind_rows(wcb_all(Models19, c("ShareBlackCoaches", "HCBlack"), "table-19"),
                   wcb_all(Models19["(9)"], "ShareBlackAltCoaches", "table-19"),
                   wcb_all(Models19[c("(5)", "(6)")], "GMBlack", "table-19"))
WcbResults <- c(WcbResults, list(Wcb19))

# Head-coach spells in the column (5) sample, and singleton spells removed
SpellCounts <- TeamSeason |> filter(!is.na(WinPct)) |> count(HCSpellId)
NSpellsAll <- nrow(SpellCounts)
NSingletons <- sum(SpellCounts$n == 1)
NSpells <- n_distinct(Models19[["(5)"]]$EstData$HCSpellId)
message(glue("14: {NSpellsAll} HC spells, {NSingletons} singletons; column (5) keeps ",
             "{NSpells} spells, {nobs(Models19[['(5)']])} team-seasons"))
# Spells in the column (5) sample in which the GM regressor changes (the
# only variation that identifies the GM coefficient with spell FE)
GMSpellVar <- Models19[["(5)"]]$EstData |>
  summarise(Range = max(GMBlack) - min(GMBlack), .by = c(HCSpellId, franchise_id)) |>
  filter(Range > 0.01)
NGMSpells <- nrow(GMSpellVar)
NGMFranchises <- n_distinct(GMSpellVar$franchise_id)
message(glue("14: GM regressor varies within {NGMSpells} spells at {NGMFranchises} franchises"))

SdLab <- if (IsPredMeasure) "expected share" else "coaches' share"
Rows19 <- bind_rows(
  tibble(term = c("Season FE", "Franchise FE", "Franchise $\\times$ HC-spell FE",
                  "Controls (column 4)")) |>
    bind_cols(tibble("(1)" = c("Yes", "No", "No", "No"), "(2)" = c("Yes", "Yes", "No", "No"),
                     "(3)" = c("Yes", "Yes", "No", "No"), "(4)" = c("Yes", "Yes", "No", "Yes"),
                     "(5)" = c("Yes", "No", "Yes", "Yes"),
                     "(6)" = c("Yes", "No", "Yes", "Yes, excl. lag. win \\%"),
                     "(7)" = c("Yes", "Yes", "No", "Yes"),
                     "(8)" = c("Yes", "Yes", "No", "Yes"),
                     "(9)" = c("Yes", "Yes", "No", "Yes"))),
  model_row(Models19, "Model-only prior controls", \(m) yes_no14(m$PriorUsed)),
  model_row(Models19, "Outcome", \(m) if_else(as.character(m$fml[[2]]) == "WinPct",
                                              "Win \\%", "Wins over exp.")),
  model_row(Models19, "Mean of outcome", outcome_mean),
  model_row(Models19, glue("SD of {SdLab} net of FE"), \(m) fmt14(resid_sd(m), 3)),
  model_row(Models19, glue("Effect of 1 SD of {SdLab} (win pp; wins in col. 7)"), sd_effect),
  model_row(Models19, "Spells with a within-spell GM change",
            \(m) if (grepl("HCSpellId", m$FeSpec)) as.character(NGMSpells) else ""),
  wcb_rows(Wcb19, Models19, c(ShareBlackCoaches = "coaches' share Black",
                              HCBlack = "head coach",
                              ShareBlackAltCoaches = "coaches' share, other measure",
                              GMBlack = "general manager")))

write_model_table(
  Models19, CoefMapMain,
  title = "Coaching-staff racial composition and team performance",
  label = "staff-diversity-main",
  notes = c(
    glue("This table includes the estimation results of equation (3). The unit of observation is a ",
         "franchise-season; the sample is the {nrow(TeamSeason)} team-seasons with the full staff ",
         "observed (Wikipedia staff templates, {min(TeamSeason$season)}-{max(TeamSeason$season)}). ",
         "The outcome is the regular-season win percentage in columns (1)-(6), (8) and (9) and wins ",
         "minus market expected wins (the sum of pre-game implied win probabilities) in column (7)."),
    paste("The model-only prior controls are the team mean of the coaches' prior probability of",
          "being Black from the predicted-race model (a function of first role, unit and",
          "first-season era, no names) for the coaches' share and the holder's prior for each",
          "role holder (row \\textit{Model-only prior controls}); only the coaches' and head",
          "coach's priors are displayed. Column (8) reverses the column-(4) choice:",
          if (IsModelPred) "it drops the prior controls."
          else "it adds the model-only priors, an ordinary predetermined control under this measure.",
          glue("Column (9) adds to column (4) the coaches' share and head-coach regressor under the ",
               "{AltLab} measure, with the priors.")),
    "Coaches are on-field coaches (head coach, coordinators, position coaches, assistants and quality control).",
    NoteShareDef, NoteControls4,
    glue("Columns (5) and (6) replace franchise FE with franchise $\\times$ head-coach-spell FE ",
         "(a spell is a run of consecutive seasons of one season head coach with a franchise), so ",
         "the head coach's race is absorbed. Of {NSpellsAll} spells, {NSingletons} last one season; ",
         "these team-seasons are perfectly fit by their spell FE and are removed, leaving {NSpells} ",
         "spells. The identifying assumption is that, within a head coach's tenure, changes in the ",
         "composition of the other coaches (assistant and coordinator turnover) are not timed to ",
         "shocks in team quality. Spells are short, so a lagged outcome with spell FE is subject to ",
         "Nickell bias; column (6) drops the lagged win percentage. With spell FE the GM coefficient ",
         "is identified only from the {NGMSpells} spells (at {NGMFranchises} franchises) in which the ",
         "GM regressor changes within the spell; with so few effective clusters neither the clustered ",
         "standard error nor the bootstrap is reliable, and the GM coefficient in columns (5) and (6) ",
         "is not interpreted (the GM enters as a control)."),
    "Columns (2)-(4) and (7)-(9) assume that, conditional on franchise and season FE and the controls, staff composition is uncorrelated with unobserved team quality.",
    NoteMarket,
    if (IsPredMeasure) {
      "The SD row is the SD of the coaches' expected Black share net of each column's fixed effects in its estimation sample, and the effect row multiplies it by the coefficient (in percentage points of win percentage, and in wins in column 7). The SD of the true share is not observed; if the probabilities are calibrated it is at least as large, because the expected share is shrunk toward the prior."
    } else {
      "The SD row is the SD of the coaches' Black share net of each column's fixed effects in its estimation sample; the effect row multiplies it by the coefficient (in percentage points of win percentage, and in wins in column 7)."
    },
    NotePredStaff, NoteRoles, NoteCluster, NoteWcb, NoteGlassCliff),
  name = "table-19-staff-diversity-team-season", measure = measure, design = "team",
  add_rows = Rows19)
Estimates <- c(Estimates, list(tidy_terms(Models19, names(CoefMapMain)) |>
                                 mutate(table = "table-19")))

# ---------------------------------------------------------------------------
# Table 19b: other team-season outcomes (columns 4 and 5 of table 19)
# ---------------------------------------------------------------------------

# Point differential per game (a less noisy measure of team quality than win
# percentage), offensive EPA per play and minus defensive EPA per play, each
# under the franchise FE (column 4) and HC-spell FE (column 5) specifications
Outcomes19b <- c(PointDiffPerGame = "Point diff. per game", OffEPAPerPlay = "Off. EPA per play",
                 NegDefEPAPerPlay = "$-$Def. EPA per play")
Spec19b <- list()
for (y in names(Outcomes19b)) {
  Spec19b[[paste0(Outcomes19b[[y]], ", franchise FE")]] <-
    list(y = y, rhs = c("ShareBlackCoaches", RoleRhs, Controls4), fe = "franchise_id + season")
  Spec19b[[paste0(Outcomes19b[[y]], ", spell FE")]] <-
    list(y = y, rhs = c("ShareBlackCoaches", RoleNoHC, Controls4), fe = "HCSpellId + season",
         single = TRUE)
}
names(Spec19b) <- paste0("(", seq_along(Spec19b), ")")
Models19b <- map(Spec19b, \(s) fit14(s$y, s$rhs, s$fe, TeamSeason,
                                     drop_singletons = isTRUE(s$single), prior = UsePrior))
map_int(Models19b, nobs) |> print()

Wcb19b <- wcb_all(Models19b, c("ShareBlackCoaches", "HCBlack"), "table-19b")
WcbResults <- c(WcbResults, list(Wcb19b))

Rows19b <- bind_rows(
  model_row(Models19b, "Outcome", \(m) Outcomes19b[[as.character(m$fml[[2]])]]),
  model_row(Models19b, "Franchise FE", \(m) yes_no14(grepl("franchise_id", m$FeSpec))),
  model_row(Models19b, "Franchise $\\times$ HC-spell FE", \(m) yes_no14(grepl("HCSpellId", m$FeSpec))),
  tibble(term = "Season FE, controls (column 4)") |>
    bind_cols(as_tibble(set_names(rep(list("Yes"), length(Models19b)), names(Models19b)))),
  model_row(Models19b, "Mean of outcome", outcome_mean),
  model_row(Models19b, glue("Effect of 1 SD of {SdLab} net of FE"), sd_effect),
  model_row(Models19b, "Model-only prior controls", \(m) yes_no14(m$PriorUsed)),
  wcb_rows(Wcb19b, Models19b, c(ShareBlackCoaches = "coaches' share Black",
                                HCBlack = "head coach")))

write_model_table(
  Models19b, CoefMapMain[c("ShareBlackCoaches", "HCBlack", "OCBlack", "DCBlack", "GMBlack",
                           "L1ShareBlackRoster")],
  title = "Coaching-staff racial composition and other team outcomes",
  label = "staff-diversity-outcomes",
  notes = c(
    glue("This table includes the estimation results of equation (3) for three further outcomes: ",
         "the regular-season point differential per game (columns 1-2), offensive EPA per play ",
         "(3-4) and minus defensive EPA per play allowed (5-6; higher is better). Odd columns use ",
         "the column-(4) specification of Table 19 (franchise and season FE); even columns use ",
         "the column-(5) specification (franchise $\\times$ head-coach-spell and season FE, ",
         "singleton spells removed, the head coach's race absorbed). The sample is ",
         "FullStaffObserved team-seasons, {min(TeamSeason$season)}-{max(TeamSeason$season)}."),
    NoteShareDef, NoteControls4,
    glue("The effect row is the coefficient times the SD of the coaches' ",
         if (IsPredMeasure) "expected " else "", "Black share net of the column's fixed effects, in the outcome's units."),
    NotePredShort, NoteRoles, NoteCluster, NoteWcb),
  name = "table-19b-staff-diversity-outcomes", measure = measure, design = "team",
  add_rows = Rows19b)
Estimates <- c(Estimates, list(tidy_terms(Models19b, names(CoefMapMain)) |>
                                 mutate(table = "table-19b")))

# ---------------------------------------------------------------------------
# Table 19c: roster controls (column 4 of table 19)
# ---------------------------------------------------------------------------

# Baseline (predetermined, season t-1 roster), without the turnover shares
# (mechanically linked to changes in staff composition), opening-day roster,
# and contemporaneous game-day-week-weighted roster (potentially
# post-treatment), the last two also without the turnover shares
LagOnly <- c("LagWinPct", "LagExpectedWins")
Spec19c <- list(
  "(1)" = c(LagOnly, TurnoverShares, LagRoster),
  "(2)" = c(LagOnly, LagRoster),
  "(3)" = c(LagOnly, TurnoverShares, Week1Roster),
  "(4)" = c(LagOnly, Week1Roster),
  "(5)" = c(LagOnly, TurnoverShares, ContempRoster),
  "(6)" = c(LagOnly, ContempRoster))
Models19c <- map(Spec19c, \(ctrl) fit14("WinPct", c("ShareBlackCoaches", RoleRhs, ctrl),
                                         "franchise_id + season", TeamSeason, prior = UsePrior))
map_int(Models19c, nobs) |> print()

Wcb19c <- wcb_all(Models19c, c("ShareBlackCoaches", "HCBlack"), "table-19c")
WcbResults <- c(WcbResults, list(Wcb19c))

CoefMap19c <- c(CoefMapMain[c("ShareBlackCoaches", "HCBlack", "OCBlack", "DCBlack", "GMBlack")],
                LagMeanLogPickRoster = "Roster mean log draft pick ($t-1$)",
                MeanLogPickWeek1 = "Opening-day mean log draft pick",
                MeanLogPickRoster = "Roster mean log draft pick ($t$)",
                L1ShareBlackRoster = "Roster share Black ($t-1$)",
                ShareBlackWeek1 = "Opening-day roster share Black",
                ShareBlackRoster = "Roster share Black ($t$)",
                ShareCoachesNewToFranchise = "Share of coaches new to franchise",
                ShareCoachesPromoted = "Share of coaches promoted")
Rows19c <- bind_rows(
  tibble(term = c("Roster controls", "Turnover shares", "Season FE, franchise FE")) |>
    bind_cols(tibble("(1)" = c("$t-1$", "Yes", "Yes"), "(2)" = c("$t-1$", "No", "Yes"),
                     "(3)" = c("Opening day", "Yes", "Yes"), "(4)" = c("Opening day", "No", "Yes"),
                     "(5)" = c("Season $t$", "Yes", "Yes"), "(6)" = c("Season $t$", "No", "Yes"))),
  model_row(Models19c, glue("Effect of 1 SD of {SdLab} (win pp)"), sd_effect),
  model_row(Models19c, "Model-only prior controls", \(m) yes_no14(m$PriorUsed)),
  wcb_rows(Wcb19c, Models19c, c(ShareBlackCoaches = "coaches' share Black",
                                HCBlack = "head coach")))

write_model_table(
  Models19c, CoefMap19c,
  title = "Coaching-staff racial composition and win percentage: roster controls",
  label = "staff-diversity-controls",
  notes = c(
    glue("This table includes the estimation results of equation (3) (column 4 of Table 19) with ",
         "alternative roster controls. Every column includes franchise and season FE, the ",
         "role-holder regressors and the lagged win percentage and market expected wins. Column (1) ",
         "is the baseline: the previous season's cap share, game-day roster mean log draft pick, ",
         "mean age and Black share, and the shares of coaches new to the franchise and promoted. ",
         "Columns (3)-(4) use the opening-day active roster (first regular-season game), which is ",
         "predetermined with respect to the season's results but chosen by the staff. Columns ",
         "(5)-(6) use the season-$t$ game-day-week-weighted roster and the season-$t$ cap share, ",
         "which respond to in-season results (call-ups, benchings, signings) and to the staff's ",
         "choices: these are potentially post-treatment controls that may absorb part of the ",
         "staff effect. Even columns drop the turnover shares, which move mechanically with ",
         "changes in staff composition. Sample: FullStaffObserved team-seasons, ",
         "{min(TeamSeason$season)}-{max(TeamSeason$season)}."),
    "Cap shares are observed from 2013 and zero-filled before; their missing indicators are absorbed by the season FE.",
    NoteShareDef, NotePredShort, NoteRoles, NoteCluster, NoteWcb),
  name = "table-19c-staff-diversity-controls", measure = measure, design = "team",
  add_rows = Rows19c)
Estimates <- c(Estimates, list(tidy_terms(Models19c, names(CoefMap19c)) |>
                                 mutate(table = "table-19c")))

# ---------------------------------------------------------------------------
# Table 20: staff groups (column-(4) specification of table 19)
# ---------------------------------------------------------------------------

# Group shares, one at a time, with the column-(4) controls and role
# indicators of table 19. The coordinators' share contains the OC and DC,
# and the front-office share contains the GM, so those columns drop the
# indicators of their own members (the personnel and scouting group does
# not include the GM)
GroupVars <- c(ShareBlackCoordinators = share_lab("Coordinators'"),
               ShareBlackPositionCoaches = share_lab("Position coaches'"),
               ShareBlackAssistants = share_lab("Assistants'"),
               ShareBlackFrontOffice = share_lab("Front office"),
               ShareBlackPersonnel = share_lab("Personnel and scouting"))
CoachingGroups <- c("ShareBlackCoordinators", "ShareBlackPositionCoaches",
                    "ShareBlackAssistants")
RoleNoCoord <- setdiff(RoleRhs, c("OCBlack", "DCBlack", "OCBlackMiss", "DCBlackMiss"))
RoleNoGM <- setdiff(RoleRhs, c("GMBlack", "GMBlackMiss"))
Spec20 <- list(
  "Coord." = c("ShareBlackCoordinators", RoleNoCoord),
  "Position" = c("ShareBlackPositionCoaches", RoleRhs),
  "Assistants" = c("ShareBlackAssistants", RoleRhs),
  "Front office" = c("ShareBlackFrontOffice", RoleNoGM),
  "Personnel" = c("ShareBlackPersonnel", RoleRhs),
  "Coaching jointly" = c(CoachingGroups, RoleNoCoord),
  "Blau" = c("BlauBlackCoaches", RoleRhs))
Models20 <- map(Spec20, \(g) fit14("WinPct", c(g, Controls4), "franchise_id + season", TeamSeason,
                                   prior = UsePrior))
map_int(Models20, nobs) |> print()

# Wild cluster bootstrap for every group coefficient
Wcb20 <- wcb_all(Models20, c(names(GroupVars), "BlauBlackCoaches"), "table-20")
WcbResults <- c(WcbResults, list(Wcb20))

# Largest coaches' share in the sample (below 0.5, the Blau index is
# increasing in the share)
MaxShareCoaches <- max(Models20[["Blau"]]$EstData$ShareBlackCoaches)

# Missing group shares in the sample (a group with no listed member)
MissGroups <- map_int(c(names(GroupVars)), \(g) sum(is.na(TeamSeason[[g]]))) |>
  set_names(names(GroupVars))
MissPersonnelYears <- TeamSeason |> filter(is.na(ShareBlackPersonnel)) |>
  count(season)
print(MissGroups)
print(MissPersonnelYears)

CoefMap20 <- c(GroupVars, BlauBlackCoaches = "Blau index of coaches (Black/non-Black)",
               CoefMapMain[c("HCBlack", "OCBlack", "DCBlack", "GMBlack")])
Rows20 <- bind_rows(
  tibble(term = c("Season FE", "Franchise FE", "Controls (column 4 of Table 19)")) |>
    bind_cols(as_tibble(set_names(rep(list(rep("Yes", 3)), length(Models20)), names(Models20)))),
  model_row(Models20, "Role-holder regressors", \(m) case_when(
    !"OCBlack" %in% names(coef(m)) ~ "HC, GM",
    !"GMBlack" %in% names(coef(m)) ~ "HC, OC, DC",
    TRUE ~ "HC, OC, DC, GM")),
  model_row(Models20, "Model-only prior controls", \(m) yes_no14(m$PriorUsed)),
  model_row(Models20, "Mean of outcome", outcome_mean),
  wcb_rows(Wcb20, Models20, c(set_names(c("coordinators", "position coaches", "assistants",
                                          "front office", "personnel"), names(GroupVars)),
                              BlauBlackCoaches = "Blau index")))

MissGroups <- MissGroups[MissGroups > 0]
PersYears <- MissPersonnelYears$season[MissPersonnelYears$n >= 5]
PersYearsText <- if (length(PersYears) == 0) "" else
  glue(" (personnel: mostly in {paste(PersYears, collapse = ', ')})")
MissText <- if (length(MissGroups) == 0) "none" else paste(glue("{GroupVars[names(MissGroups)]} ({MissGroups})"),
                  collapse = "; ")
write_model_table(
  Models20, CoefMap20,
  title = "Racial composition by staff group and team performance",
  label = "staff-diversity-groups",
  notes = c(
    glue("This table includes the estimation results of equation (3) with the coaches' Black share ",
         "replaced by the Black share of one staff group at a time (columns 1-5), the three ",
         "coaching groups jointly (column 6) and the two-group Blau index of on-field coaches, ",
         "$2s(1-s)$ with $s$ the coaches' Black share (column 7). The outcome is the regular-season ",
         "win percentage; the sample is FullStaffObserved team-seasons, ",
         "{min(TeamSeason$season)}-{max(TeamSeason$season)}."),
    paste(if (IsPredMeasure) "Under this measure $s$ is the expected share, so column (7) uses $2s(1-s)$ of the expected share, not the expected Blau index $E[2S(1-S)] = 2s(1-s) - 2\\mathrm{Var}(S)$, and regression calibration does not carry over to this nonlinear function.",
          if (MaxShareCoaches < 0.5) glue("Because $s$ is below 0.5 in every team-season (maximum {fmt14(MaxShareCoaches, 3)}), the index is a monotone transform of the share, not an independent diversity measure.")),
    "Coordinators are the OC, DC and special-teams coordinator; the front office includes owners, executives, the GM and personnel and scouting staff.",
    glue("Every column includes franchise and season FE, the predetermined column-(4) controls of ",
         "Table 19 (lagged win percentage and market expected wins, the shares of coaches new to ",
         "the franchise and promoted, and the previous season's cap share, roster mean log draft ",
         "pick, mean age and roster Black share) and the role-holder regressors of Table 19, ",
         "except that the coordinator columns (1 and 6) omit the OC and DC regressors and the ",
         "front-office column (4) omits the GM regressor, because those persons are members of ",
         "the group whose share enters (row \\textit{{Role-holder regressors}})."),
    glue("Team-seasons with no listed member of a group have a missing share and drop from that ",
         "column, so the number of observations differs across columns. Team-seasons with a ",
         "missing share: {MissText}{PersYearsText}."),
    NotePredShort, NoteCluster, NoteWcb),
  name = "table-20-staff-diversity-by-group", measure = measure, design = "team",
  add_rows = Rows20)
Estimates <- c(Estimates, list(tidy_terms(Models20, names(CoefMap20)) |>
                                 mutate(table = "table-20")))

# ---------------------------------------------------------------------------
# Figure: staff-group coefficients (table 20)
# ---------------------------------------------------------------------------

# 95% cluster-robust CIs from the models; WCB CIs from test inversion
FigData <- tidy_terms(Models20[c(names(Spec20)[1:5], "Coaching jointly")], names(GroupVars)) |>
  mutate(Spec = if_else(model == "Coaching jointly", "Coaching groups jointly",
                        "One group at a time")) |>
  left_join(Wcb20 |> select(model, term, wcb_low = ci_low, wcb_high = ci_high),
            by = c("model", "term")) |>
  mutate(Group = factor(GroupVars[term], levels = rev(GroupVars)))

FigCoef <- ggplot(FigData, aes(x = estimate, y = Group, colour = Spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = wcb_low, xmax = wcb_high), width = 0, orientation = "y", linewidth = 2.2,
                 alpha = 0.3, position = position_dodge(width = 0.5), na.rm = TRUE) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.15, orientation = "y",
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("One group at a time" = "#1b4f72",
                                 "Coaching groups jointly" = "#c0392b")) +
  labs(x = "Effect on win percentage of a staff group's Black share (0 to 1)", y = NULL,
       colour = NULL,
       title = "Staff-group racial composition and win percentage",
       subtitle = paste0("Thin bars: 95% cluster-robust CI; shaded bars: wild cluster bootstrap CI",
                         switch(measure, hand = "",
                                predicted = "\nPredicted race: expected shares (mean member P(Black))",
                                paste0("\n", str_to_sentence(measure), " race measure (sensitivity)")))) +
  theme_customs() +
  theme(legend.position = "bottom", plot.title.position = "plot")
save_exhibit_figure(FigCoef, "figure-staff-diversity-coefficients", measure,
                    width = 8, height = 5)

# ---------------------------------------------------------------------------
# Table 21: offense and defense stacked (unit design)
# ---------------------------------------------------------------------------

# Opening-day starting QB (starter of the franchise's first REG game; the
# race is QBWeek1Black from 11) and his log overall draft pick (undrafted =
# log 300, as in the roster measures; NA when the QB has no player_season row)
QBWeek1 <- TeamGameReg |>
  arrange(franchise_id, season, gameday, week) |>
  summarise(QBWeek1Id = first(starting_qb_id), .by = c(franchise_id, season)) |>
  left_join(mutate(QBDraft, InPlayerSeason = TRUE), by = c("QBWeek1Id" = "gsis_id", "season")) |>
  mutate(QBWeek1LogPick = if_else(coalesce(InPlayerSeason, FALSE),
                                  log(coalesce(as.numeric(DraftPick), 300)), NA_real_)) |>
  select(franchise_id, season, QBWeek1LogPick)

# Unit sample: FullStaffObserved franchise-seasons, one row per unit;
# outcomes are signed so that higher = better for both units. Team-level
# factors that load on one unit enter as unit interactions: the opening-day
# QB's race and draft pick (offense rows) and the head coach's race
# (defense rows; its offense-row level is absorbed by franchise x season FE)
# Built by a function of the race measure, so that table 29 can rebuild it.
# Unit x season FE also enter as explicit UnitDefense x season dummies, one
# per season (franchise x season FE absorb the offense level of each season),
# for the bootstrap refits: together they span the same space as
# Unit^season, but avoid aliased dummies in the lm refit. The displayed
# models use franchise_id^season + Unit^season FE, so the within R^2 is
# meaningful; the coefficients are identical (checked below).
UnitSeasons <- sort(unique(UnitSeasonRaw$season[UnitSeasonRaw$FullStaffObserved]))
UnitSeasonDummies <- paste0("DefenseX", UnitSeasons)
prep_unit_season <- function(unit_raw, team_all, m) {
  d <- apply_race_measure(unit_raw, m) |>
    filter(FullStaffObserved) |>
    left_join(team_all |> select(franchise_id, season, QBWeek1Black, QBWeek1PriorBlackPred,
                                 HCPriorBlackPred),
              by = c("franchise_id", "season")) |>
    left_join(QBWeek1, by = c("franchise_id", "season")) |>
    mutate(UnitCoordBlackRaw = UnitCoordBlack) |>
    fill_missing(c("UnitCoordBlack", "ShareBlackUnitRoster", "ShareBlackUnitRosterSnapW",
                   "QBWeek1Black", "QBWeek1LogPick", "HCBlack",
                   # model-only priors (prior controls), zero-filled the same way
                   "UnitCoordPriorBlackPred", "MeanPriorBlackPredUnitRoster",
                   "MeanPriorBlackPredUnitRosterSnapW", "QBWeek1PriorBlackPred",
                   "HCPriorBlackPred")) |>
    mutate(UnitOffense = 1L - UnitDefense,
           OffQBWeek1Black = UnitOffense * QBWeek1Black,
           OffQBWeek1BlackMiss = UnitOffense * QBWeek1BlackMiss,
           OffQBWeek1LogPick = UnitOffense * QBWeek1LogPick,
           OffQBWeek1LogPickMiss = UnitOffense * QBWeek1LogPickMiss,
           DefHCBlack = UnitDefense * HCBlack,
           DefHCBlackMiss = UnitDefense * HCBlackMiss,
           OffQBWeek1PriorBlackPred = UnitOffense * QBWeek1PriorBlackPred,
           OffQBWeek1PriorBlackPredMiss = UnitOffense * QBWeek1PriorBlackPredMiss,
           DefHCPriorBlackPred = UnitDefense * HCPriorBlackPred,
           DefHCPriorBlackPredMiss = UnitDefense * HCPriorBlackPredMiss)
  for (i in seq_along(UnitSeasonDummies)) {
    d[[UnitSeasonDummies[i]]] <- d$UnitDefense * (d$season == UnitSeasons[i])
  }
  d
}
UnitSeason <- prep_unit_season(UnitSeasonRaw, TeamSeasonAll, measure)
stopifnot(nrow(UnitSeason) == 2 * nrow(TeamSeason))
message(glue("14: opening-day QB draft pick missing for ",
             "{sum(UnitSeason$QBWeek1LogPickMiss[UnitSeason$UnitOffense == 1])} offense unit-seasons; ",
             "QB race missing for {sum(UnitSeason$QBWeek1BlackMiss[UnitSeason$UnitOffense == 1])}"))

UnitBase <- c("UnitCoordBlack", "UnitCoordBlackMiss", "ShareBlackUnitCoaches")
UnitTeamSide <- c("OffQBWeek1Black", "OffQBWeek1BlackMiss", "OffQBWeek1LogPick",
                  "OffQBWeek1LogPickMiss", "DefHCBlack", "DefHCBlackMiss")
UnitHeadcount <- c("UnitMeanLogPickRoster", "UnitMeanAgeRoster", "ShareBlackUnitRoster",
                   "ShareBlackUnitRosterMiss", UnitTeamSide)
UnitSnap <- c("UnitMeanLogPickSnapW", "UnitMeanAgeSnapW", "UnitCapShare",
              "ShareBlackUnitRosterSnapW", "ShareBlackUnitRosterSnapWMiss", UnitTeamSide)
UnitSnapSample <- filter(UnitSeason, season >= 2013)

# Columns (1)-(3) unit EPA per play, (4)-(6) unit success rate; the controls
# include the unit's own lagged outcome
Spec21 <- list(
  "(1)" = list(y = "UnitEPAPerPlay", rhs = UnitBase, d = UnitSeason),
  "(2)" = list(y = "UnitEPAPerPlay", rhs = c(UnitBase, UnitHeadcount, "LagUnitEPAPerPlay"),
               d = UnitSeason),
  "(3)" = list(y = "UnitEPAPerPlay", rhs = c(UnitBase, UnitSnap, "LagUnitEPAPerPlay"),
               d = UnitSnapSample),
  "(4)" = list(y = "UnitSuccessRate", rhs = UnitBase, d = UnitSeason),
  "(5)" = list(y = "UnitSuccessRate", rhs = c(UnitBase, UnitHeadcount, "LagUnitSuccessRate"),
               d = UnitSeason),
  "(6)" = list(y = "UnitSuccessRate", rhs = c(UnitBase, UnitSnap, "LagUnitSuccessRate"),
               d = UnitSnapSample))
Models21 <- map(Spec21, \(s) fit14(s$y, s$rhs, "franchise_id^season + Unit^season", s$d,
                                   prior = UsePrior))
Models21Wcb <- map(Spec21, \(s) fit14(s$y, c(s$rhs, UnitSeasonDummies), "franchise_id^season", s$d,
                                      prior = UsePrior))
map_int(Models21, nobs) |> print()
# Same coefficients in the displayed and bootstrap versions
for (nm in names(Models21)) {
  b <- coef(Models21[[nm]])
  stopifnot(nobs(Models21[[nm]]) == nobs(Models21Wcb[[nm]]),
            isTRUE(all.equal(b, coef(Models21Wcb[[nm]])[names(b)], tolerance = 1e-6)))
}

# Wild cluster bootstrap on the dummy version: time one test first; skip
# the rest when a test takes more than about a minute (lm refit with ~600
# franchise-season dummies)
T0Unit <- Sys.time()
Wcb21First <- wcb(Models21Wcb[["(1)"]], "UnitCoordBlack", "(1)", "table-21")
SecsUnitTest <- as.numeric(difftime(Sys.time(), T0Unit, units = "secs"))
message(glue("14: one unit-design WCB test took {round(SecsUnitTest, 1)}s"))
UnitWcbRun <- SecsUnitTest <= 60
Wcb21 <- if (UnitWcbRun) {
  bind_rows(Wcb21First,
            wcb(Models21Wcb[["(1)"]], "ShareBlackUnitCoaches", "(1)", "table-21"),
            wcb_all(Models21Wcb[-1], c("UnitCoordBlack", "ShareBlackUnitCoaches"), "table-21"))
} else {
  Wcb21First
}
WcbResults <- c(WcbResults, list(Wcb21))

# Outcome mean by unit (the stacked outcome has opposite-signed unit levels,
# so the pooled mean is about zero)
unit_mean <- function(m, defense) {
  d <- m$EstData
  fmt14(mean(d[[as.character(m$fml[[2]])]][d$UnitDefense == defense]), 3)
}

CoefMap21 <- c(UnitCoordBlack = role_lab("unit coordinator"),
               ShareBlackUnitCoaches = share_lab("Unit coaches'"),
               ShareBlackUnitRoster = "Unit roster share Black (headcount)",
               ShareBlackUnitRosterSnapW = "Unit roster share Black (snap-weighted)",
               OffQBWeek1Black = paste("Offense $\\times$", role_lab("opening-day QB")),
               OffQBWeek1LogPick = "Offense $\\times$ opening-day QB log draft pick",
               DefHCBlack = paste("Defense $\\times$", role_lab("head coach")),
               UnitMeanLogPickRoster = "Unit mean log draft pick (headcount)",
               UnitMeanLogPickSnapW = "Unit mean log draft pick (snap-weighted)",
               UnitCapShare = "Unit cap share",
               LagUnitEPAPerPlay = "Lagged unit EPA per play",
               LagUnitSuccessRate = "Lagged unit success rate")
Rows21 <- bind_rows(
  tibble(term = c("Franchise $\\times$ season FE", "Unit $\\times$ season FE",
                  "Unit roster and QB/HC controls", "Seasons")) |>
    bind_cols(tibble("(1)" = c("Yes", "Yes", "No", "2007-2025"),
                     "(2)" = c("Yes", "Yes", "Headcount", "2007-2025"),
                     "(3)" = c("Yes", "Yes", "Snap-weighted", "2013-2025"),
                     "(4)" = c("Yes", "Yes", "No", "2007-2025"),
                     "(5)" = c("Yes", "Yes", "Headcount", "2007-2025"),
                     "(6)" = c("Yes", "Yes", "Snap-weighted", "2013-2025"))),
  model_row(Models21, "Model-only prior controls", \(m) yes_no14(m$PriorUsed)),
  model_row(Models21, "Mean of outcome, offense", \(m) unit_mean(m, 0L)),
  model_row(Models21, "Mean of outcome, defense", \(m) unit_mean(m, 1L)),
  if (UnitWcbRun) wcb_rows(Wcb21, Models21, c(UnitCoordBlack = "unit coordinator",
                                               ShareBlackUnitCoaches = "unit coaches' share")))

NoteWcb21 <- if (UnitWcbRun) {
  glue("{NoteWcb} The bootstrap refits each model by OLS with franchise $\\times$ season dummies ",
       "and defense $\\times$ season dummies (one test took {round(SecsUnitTest)} seconds).")
} else {
  glue("Wild cluster bootstrap $p$-values are not reported: the OLS refit with about 600 ",
       "franchise $\\times$ season dummies took {round(SecsUnitTest)} seconds per test, so ",
       "inference is cluster-robust only.")
}
write_model_table(
  Models21, CoefMap21,
  title = "Unit coordinators, unit coaches and unit performance: offense and defense stacked",
  label = "staff-diversity-unit",
  notes = c(
    glue("This table includes the estimation results of the stacked unit version of equation (3): ",
         "each FullStaffObserved franchise-season contributes an offense and a defense row ",
         "({nrow(UnitSeason)} unit-seasons, {min(UnitSeason$season)}-{max(UnitSeason$season)}; ",
         "columns 3 and 6 start in 2013, when snap counts and cap tables are observed). The outcome ",
         "is EPA per play (columns 1-3) or success rate (columns 4-6), signed so that higher is ",
         "better for both units (defense: minus the opponent's EPA per play or success rate). ",
         "Because the two units' outcomes have opposite-signed levels, the outcome mean is ",
         "reported by unit."),
    glue("The unit coordinator is the OC for the offense and the DC for the defense; it is ",
         "zero-filled with a missing indicator when no coordinator is listed",
         if (IsPredMeasure) "" else " or his race is unknown",
         " ({sum(is.na(UnitSeason$UnitCoordBlackRaw))} unit-seasons). Unit coaches are the ",
         "coordinator and the unit's position and assistant coaches.",
         if (UsePrior) paste(" Under the predicted measure every column controls for the model-only",
                             "priors matched to each race regressor: the unit coordinator's prior,",
                             "the unit coaches' and unit roster's mean priors, and the opening-day",
                             "QB's and head coach's priors in their unit interactions.") else ""),
    "The design compares a franchise-season's offense with its defense. Franchise $\\times$ season FE absorb factors that shift both units equally; unit $\\times$ season FE absorb league-wide offense-defense differences in each season. Factors that load on one unit are controlled through unit interactions in columns (2), (3), (5) and (6): the opening-day starting QB's race and log draft pick (undrafted = log 300) interacted with the offense indicator, and the head coach's race interacted with the defense indicator (each zero-filled with an interacted missing indicator). The head coach's side of the ball is not observed, and QB quality is proxied only by draft position, so the differential influence of the play-calling head coach and of in-season QB changes remains a threat. The identifying assumption is that, within a franchise-season, the racial composition of one unit's staff relative to the other's is uncorrelated with the unit's unobserved quality, conditional on these interactions and the unit's roster controls (mean log draft pick, mean age, cap share in columns 3 and 6, the roster's Black share and the unit's lagged outcome).",
    NotePredShort, NoteCluster, NoteWcb21),
  name = "table-21-staff-diversity-unit-stacked", measure = measure, design = "team",
  add_rows = Rows21)
Estimates <- c(Estimates, list(tidy_terms(Models21, names(CoefMap21)) |>
                                 mutate(table = "table-21")))

# ---------------------------------------------------------------------------
# Table 22: head-coach hires and the new head coach's race
# ---------------------------------------------------------------------------

# First-game and last-game head coach of each franchise-season (REG games)
GameHC <- TeamGameReg |>
  arrange(franchise_id, season, gameday, week) |>
  summarise(FirstHC = first(HeadCoachPersonId), LastHC = last(HeadCoachPersonId),
            FirstHCName = first(HeadCoachName), .by = c(franchise_id, season))
stopifnot(!anyDuplicated(GameHC[c("franchise_id", "season")]), !anyNA(GameHC$FirstHC))
# Seasons in which each person coached at least one REG game for a franchise
CoachSeasons <- TeamGameReg |> distinct(franchise_id, HeadCoachPersonId, season)

# Hire indicators on the full panel:
# - BetweenHire: the first-game HC differs from the previous season's
#   last-game HC (= HCChange from 11)
# - RetainedInterim: the first-game HC took over during season t-1 and was
#   kept (he coached the last game of t-1 but not the first)
# - ReturnHire: the hired coach comes back after a short absence: he
#   coached the franchise in one of the two previous seasons (e.g. NO 2013,
#   Payton after his suspension). A coach rehired after a longer gap (LV 2018
#   Gruden; former interims such as ATL 2024 Morris) is a genuine new hire.
# - StandIn: the previous season's last-game HC is back at the first game of
#   t+1, so the season-t coach stood in for an absent HC (NO 2012)
HireFlags <- TeamSeasonAll |>
  select(franchise_id, season, HCChange, HCWeek1Black) |>
  left_join(GameHC, by = c("franchise_id", "season")) |>
  arrange(franchise_id, season) |>
  group_by(franchise_id) |>
  mutate(LagLastHC = lag_within(LastHC, season), LagFirstHC = lag_within(FirstHC, season),
         LeadFirstHC = if_else(lead(season) == season + 1L, lead(FirstHC), NA_character_)) |>
  ungroup() |>
  mutate(BetweenHire = as.integer(FirstHC != LagLastHC),
         RetainedInterim = as.integer(FirstHC == LagLastHC & FirstHC != LagFirstHC))
stopifnot(identical(coalesce(HireFlags$BetweenHire, -1L), coalesce(HireFlags$HCChange, -1L)))

# Recent earlier spell: the hired coach coached the franchise in t - 2 or
# t - 1 (between-season hires), or in t - 2 (retained interims, whose t - 1
# games are the interim spell itself)
EarlierSpell <- HireFlags |>
  select(franchise_id, season, FirstHC, RetainedInterim) |>
  inner_join(CoachSeasons, by = c("franchise_id", "FirstHC" = "HeadCoachPersonId"),
             suffix = c("", "Coached"), relationship = "many-to-many") |>
  filter(seasonCoached >= season - 2L, seasonCoached < season - coalesce(RetainedInterim, 0L)) |>
  distinct(franchise_id, season) |>
  mutate(EarlierSpell = 1L)
HireFlags <- HireFlags |>
  left_join(EarlierSpell, by = c("franchise_id", "season")) |>
  mutate(EarlierSpell = coalesce(EarlierSpell, 0L),
         ReturnHire = as.integer((BetweenHire == 1 | RetainedInterim == 1) & EarlierSpell == 1),
         StandIn = as.integer(BetweenHire == 1 & coalesce(LagLastHC == LeadFirstHC, FALSE)),
         Hire = as.integer((BetweenHire == 1 | RetainedInterim == 1) & ReturnHire == 0 &
                             StandIn == 0))

# Hire-sample bookkeeping for the notes
HireLog <- HireFlags |>
  filter(BetweenHire == 1 | RetainedInterim == 1) |>
  mutate(Type = case_when(StandIn == 1 ~ "stand-in", ReturnHire == 1 ~ "return",
                          RetainedInterim == 1 ~ "retained interim", TRUE ~ "between-season"))
print(count(HireLog, Type))
print(HireLog |> filter(Type != "between-season") |>
        select(franchise_id, season, FirstHCName, Type, HCWeek1Black), n = Inf)
fmt_cases <- function(df) paste(glue("{df$franchise_id} {df$season}"), collapse = ", ")
NBetweenAll <- sum(HireLog$BetweenHire == 1 & !is.na(HireLog$BetweenHire))
CasesInterim <- HireLog |> filter(Type == "retained interim")
CasesDropped <- HireLog |> filter(Type %in% c("return", "stand-in"))

# Hire sample: the hired (first-game) head coach's race under the measure,
# not zero-filled (hires with unknown race drop out); the season head coach
# (most games) can differ when the hire was replaced during his first season
# (a function of the measure's team-season panel, reused by table 29).
# Under a predicted measure HCBlack is the hired coach's P(Black), and its
# prior control is the hired coach's model-only prior (HCWeek1PriorBlackPred,
# stored as HCPriorBlackPred so that add_prior() finds it). HCDoc is the
# hired coach's documented race (documented_black_any), a validation
# regressor: documentation is mostly positive (public sources state that a
# person is Black far more often than that he is white), so a hire without a
# documented race counts as non-Black (0), as in Table 25; HCDocUndoc flags
# those hires for the counts.
build_hires <- function(team_all) {
  team_all |>
    inner_join(HireFlags |> filter(Hire == 1) |>
                 select(franchise_id, season, RetainedInterim, FirstHC),
               by = c("franchise_id", "season")) |>
    left_join(select(StaffDoc, FirstHC = PersonId, HCDoc = DocBlack), by = "FirstHC") |>
    mutate(HCBlack = HCWeek1Black, HCPriorBlackPred = HCWeek1PriorBlackPred,
           HCDocUndoc = as.integer(is.na(HCDoc)),
           HCDoc = coalesce(as.numeric(HCDoc), 0))
}
Hires <- build_hires(TeamSeasonAll)
Hires0725 <- filter(Hires, season >= 2007)
NFirstNotSeasonHC <- sum(Hires$FirstHC != Hires$HCPersonId)
message(glue("14: {nrow(Hires)} hires; the first-game HC is not the season HC in {NFirstNotSeasonHC}; ",
             "no documented race for {sum(Hires$HCDocUndoc)}"))

# Columns (2), (3), (5), (6): outcomes with the lagged win percentage as a
# control. Columns (1) and (4): DeltaWinPct = WinPct - LagWinPct with only
# LagExpectedWins, which restricts the coefficient on LagWinPct in the
# win-percentage equation to one (tested in the bottom panel; shown for
# comparison only). Columns (7) and (8): the hired coach's documented race
# alone and jointly with HCBlack (validation of the measure). RetainedInterim
# enters every column: for a retained interim the lagged outcomes partly
# reflect the new coach's own record.
LagCtrl22 <- c("LagWinPct", "LagExpectedWins")
DocRhs <- "HCDoc"
Spec22 <- list(
  "(1)" = list(y = "DeltaWinPct", rhs = c("HCBlack", "LagExpectedWins"), d = Hires),
  "(2)" = list(y = "WinPct", rhs = c("HCBlack", LagCtrl22), d = Hires),
  "(3)" = list(y = "WinsOverExpected", rhs = c("HCBlack", LagCtrl22), d = Hires),
  "(4)" = list(y = "DeltaWinPct", rhs = c("HCBlack", "LagExpectedWins"), d = Hires0725),
  "(5)" = list(y = "WinPct", rhs = c("HCBlack", LagCtrl22), d = Hires0725),
  "(6)" = list(y = "WinsOverExpected", rhs = c("HCBlack", LagCtrl22), d = Hires0725),
  "(7)" = list(y = "WinPct", rhs = c(DocRhs, LagCtrl22), d = Hires),
  "(8)" = list(y = "WinPct", rhs = c("HCBlack", DocRhs, LagCtrl22), d = Hires))
Models22 <- map(Spec22, \(s) fit14(s$y, c(s$rhs, "RetainedInterim"), "season", s$d,
                                   prior = UsePrior))
map_int(Models22, nobs) |> print()

Wcb22 <- wcb_all(Models22, c("HCBlack", "HCDoc"), "table-22")
WcbResults <- c(WcbResults, list(Wcb22))

# Test of the restriction behind columns (1) and (4): H0 coefficient on
# LagWinPct = 1 in the win-percentage columns (cluster-robust t test with
# the model's t degrees of freedom)
lag_one_p <- function(m) {
  if (!"LagWinPct" %in% names(coef(m)) || as.character(m$fml[[2]]) != "WinPct") return(NA_real_)
  t <- (coef(m)[["LagWinPct"]] - 1) / se(m)[["LagWinPct"]]
  2 * pt(-abs(t), df = fixest::degrees_freedom(m, "t"))
}
LagOneP <- map_dbl(Models22, lag_one_p)
print(round(LagOneP, 4))

# Inherited situation: the lagged win percentage on the hired coach's race
# given lagged market expected wins (and the prior control under the
# predicted measure), season FE, in the column (2) and (5) samples. A
# positive coefficient means hires with higher HCBlack inherit teams that beat
# their market expectation the year before, so mean reversion biases the
# change-in-win-percentage columns downward.
Select22 <- map(list("2000" = Hires, "2007" = Hires0725), \(d)
  fit14("LagWinPct", c("HCBlack", "LagExpectedWins", "RetainedInterim"), "season", d,
        prior = UsePrior))
Select22Txt <- map_chr(Select22, \(m) glue("{fmt14(coef(m)[['HCBlack']], 3)} ",
                                           "(SE {fmt14(se(m)[['HCBlack']], 3)})"))
print(Select22Txt)

# Calibration of HCBlack among the column-(2) hires: slope of the
# documented race (undocumented = 0) on HCBlack, season FE (a slope of one
# means HCBlack is calibrated for documented Black race)
Calib22 <- feols(HCDoc ~ HCBlack | season, data = Models22[["(2)"]]$EstData,
                 vcov = ~franchise_id, notes = FALSE)
Calib22Txt <- glue("{fmt14(coef(Calib22)[['HCBlack']], 3)} (SE {fmt14(se(Calib22)[['HCBlack']], 3)}, ",
                   "{nobs(Calib22)} hires)")
print(Calib22Txt)

# Randomization inference: permute the race regressor (HCBlack, or HCDoc in
# column 7) across hires within season and re-estimate (Frisch-Waugh:
# residualize on the other regressors and the season dummies once, then only
# the permuted treatment). Two-sided p-value on the coefficient, counting the
# observed assignment.
RiReps <- 9999
ri_pvalue <- function(m, param = "HCBlack", reps = RiReps, seed = 20261002) {
  d <- m$EstData
  y <- d[[as.character(m$fml[[2]])]]
  others <- setdiff(names(coef(m)), param)
  Z <- cbind(model.matrix(~ factor(season), d), as.matrix(d[others]))
  qz <- qr(Z)
  ry <- qr.resid(qz, y)
  b_of <- \(x) { rx <- qr.resid(qz, x); sum(rx * ry) / sum(rx^2) }
  b0 <- b_of(d[[param]])
  stopifnot(isTRUE(all.equal(b0, unname(coef(m)[param]), tolerance = 1e-6)))
  set.seed(seed)
  idx <- split(seq_len(nrow(d)), d$season)
  bperm <- replicate(reps, {
    x <- d[[param]]
    for (g in idx) x[g] <- x[g][sample.int(length(g))]
    b_of(x)
  })
  (1 + sum(abs(bperm) >= abs(b0) - 1e-12)) / (reps + 1)
}
RiParam22 <- map_chr(Models22, \(m) if ("HCBlack" %in% names(coef(m))) "HCBlack" else "HCDoc")
RiP22 <- map2_dbl(Models22, RiParam22, \(m, p) ri_pvalue(m, p))
print(round(RiP22, 3))

# Black hires, franchises with a Black hire and residual degrees of freedom.
# Under a predicted measure HCBlack is a probability: the expected number of
# Black hires is sum(P(Black)); hires with P(Black) >= 0.5 are counted as a
# descriptive statistic only (the regressions use the probabilities). Under
# the 0/1 measures both counts equal the number of Black hires.
ExpBlackHires <- map_dbl(Models22, \(m) sum(m$EstData$HCBlack, na.rm = TRUE))
NBlackHires <- map_int(Models22, \(m) sum(m$EstData$HCBlack >= 0.5, na.rm = TRUE))
NBlackFranchises <- map_int(Models22, \(m) n_distinct(m$EstData$franchise_id[coalesce(m$EstData$HCBlack >= 0.5, FALSE)]))
NDocBlackHires <- map_int(Models22, \(m) sum(m$EstData$HCDoc == 1))
print(rbind(expected = round(ExpBlackHires, 1), p_ge_half = NBlackHires, documented = NDocBlackHires))
BlackHireRows <- if (IsPredMeasure) {
  bind_rows(
    model_row(Models22, "Expected Black hires (sum of P(Black))", \(m) fmt14(sum(m$EstData$HCBlack, na.rm = TRUE), 1)),
    model_row(Models22, "Hires with P(Black) $\\geq$ 0.5 (descriptive)",
              \(m) as.character(sum(m$EstData$HCBlack >= 0.5, na.rm = TRUE))),
    model_row(Models22, "Franchises with such a hire",
              \(m) as.character(n_distinct(m$EstData$franchise_id[coalesce(m$EstData$HCBlack >= 0.5, FALSE)]))))
} else {
  bind_rows(
    model_row(Models22, "Black head-coach hires", \(m) as.character(sum(m$EstData$HCBlack == 1, na.rm = TRUE))),
    model_row(Models22, "Franchises with a Black hire",
              \(m) as.character(n_distinct(m$EstData$franchise_id[coalesce(m$EstData$HCBlack == 1, FALSE)]))))
}
Cols22 <- names(Models22)
Rows22 <- bind_rows(
  tibble(term = c("Hires from", "Season FE")) |>
    bind_cols(as_tibble(set_names(map(Cols22, \(cl) c(if (cl %in% c("(4)", "(5)", "(6)")) "2007" else "2000",
                                                   "Yes")), Cols22))),
  model_row(Models22, "Hires", \(m) as.character(nobs(m))),
  model_row(Models22, "Retained interim hires", \(m) as.character(sum(m$EstData$RetainedInterim))),
  BlackHireRows,
  model_row(Models22, "Documented Black hires", \(m) as.character(sum(m$EstData$HCDoc == 1))),
  model_row(Models22, "Model-only prior control", \(m) yes_no14(m$PriorUsed && "HCBlack" %in% names(coef(m)))),
  model_row(Models22, "Mean of outcome", outcome_mean),
  model_row(Models22, "Residual degrees of freedom",
            \(m) as.character(nobs(m) - m$nparams)),
  tibble(term = "$p$-value, lagged win \\% coefficient = 1") |>
    bind_cols(as_tibble(as.list(set_names(fmt_p14(LagOneP), Cols22)))),
  wcb_rows(Wcb22, Models22, c(HCBlack = "new head coach", HCDoc = "documented race")),
  tibble(term = "Randomization inference $p$-value") |>
    bind_cols(as_tibble(as.list(set_names(fmt_p(RiP22), Cols22)))))
# First hire season actually used (LagWinPct is NA in 1999)
FirstHire22 <- as.character(min(Models22[["(2)"]]$EstData$season))
for (cl in setdiff(Cols22, c("(4)", "(5)", "(6)"))) Rows22[[cl]][1] <- FirstHire22

CoefMap22 <- c(HCBlack = role_lab("new head coach"),
               HCDoc = "Documented Black new head coach",
               HCPriorBlackPred = "New head coach's prior P(Black)",
               RetainedInterim = "Retained interim coach",
               LagWinPct = "Lagged win percentage",
               LagExpectedWins = "Lagged market expected wins")
LagOneRange <- range(LagOneP[c("(2)", "(5)")])
write_model_table(
  Models22, CoefMap22,
  title = "Head-coach hires: team performance and the new head coach's race",
  label = "hc-hire-race",
  notes = c(
    glue("This table includes the estimation results of a version of equation (3) on head-coach ",
         "hires, {FirstHire22}-{max(Hires$season)} in columns (1)-(3), (7) and (8) and ",
         "2007-{max(Hires$season)} in columns (4)-(6). A hire is the first season of a new head ",
         "coach: either the first-game head coach differs from the previous season's last-game ",
         "head coach ({NBetweenAll} franchise-seasons in 2000-{max(Hires$season)}), or an interim ",
         "head coach who took over during the previous season is retained ({nrow(CasesInterim)}: ",
         "{fmt_cases(CasesInterim)}). Returns of a coach who coached the franchise in one of the ",
         "two previous seasons and stand-in seasons in which the previous head coach returns the next season are ",
         "excluded ({nrow(CasesDropped)}: {fmt_cases(CasesDropped)}). Retained interim hires are ",
         "flagged by an indicator, because their lagged outcomes partly reflect their own record."),
    glue("The new head coach's race is that of the first-game head coach (the hired coach), not ",
         "of the coach with the most games (they differ for {NFirstNotSeasonHC} of the hires); it is not ",
         "zero-filled, so hires with unknown race are excluded",
         if (IsPredMeasure) " (under the predicted measure every hire has a prediction)." else "."),
    glue("The outcomes are the win percentage (columns 2, 5, 7 and 8), wins minus market expected ",
         "wins (3 and 6) and the change in win percentage from the previous season (1 and 4). ",
         "Columns (1) and (4) control only for lagged market expected wins, which restricts the ",
         "coefficient on the lagged win percentage in the win-percentage equation to one (bottom ",
         "panel: $p$-values of {fmt_p14(LagOneP[['(2)']])} and {fmt_p14(LagOneP[['(5)']])} for this ",
         "restriction in columns 2 and 5). ",
         if (max(LagOneRange) < 0.05) "The restriction is rejected, so columns (1) and (4) can attribute part of the inherited team's mean reversion to the new coach's race; they are shown for comparison only, and columns (2) and (5) are the headline specifications."
         else "Columns (2) and (5), which leave the coefficient free, are the headline specifications."),
    "Pre-game betting lines price the new hire, so columns (3) and (6) test whether the market misprices Black hires, not the total effect of the hire's race.",
    "Season FE are used rather than Rooney Rule era FE because the residual degrees of freedom remain ample (bottom panel). The identifying assumption is that, conditional on the inherited situation (lagged win percentage and market expected wins) and the season, the new head coach's race is unrelated to unobserved determinants of the team's subsequent performance.",
    glue("Columns (7) and (8) use the hired coach's documented race (1 when a public source states ",
         "that he is Black; documentation is mostly positive, so the ",
         "{sum(Models22[['(7)']]$EstData$HCDocUndoc)} hires without a documented race count as ",
         "non-Black, as in Table 25), alone and jointly with the column-(2) regressor, as a check on ",
         "the race measure."),
    if (IsModelPred) {
      glue("Under the predicted measure the new head coach's race is his model-only probability of ",
           "being Black, and every column with it controls for his model-only prior (first role, unit and ",
           "first-season era). Regression calibration reads the coefficient as a race effect only if ",
           "the information behind the prediction (names, hometown county and the prior's ",
           "covariates) is unrelated to performance given race and the controls. Among the ",
           "hires of column (2), the slope of documented race on P(Black) (season FE) is ",
           "{Calib22Txt}. If the P(Black) coefficient survives conditioning on documented race ",
           "(column 8) while the documented-race coefficient does not, it rests on variation in names ",
           "and hometowns unrelated to race, and it is not a race effect. The hire sample contains an expected ",
           "{fmt14(ExpBlackHires[['(2)']], 1)} Black hires in columns (1)-(3) and ",
           "{fmt14(ExpBlackHires[['(5)']], 1)} in columns (4)-(6) (sum of P(Black)), and ",
           "{NBlackHires[['(2)']]} and {NBlackHires[['(5)']]} hires with P(Black) $\\geq$ 0.5 at ",
           "{NBlackFranchises[['(2)']]} and {NBlackFranchises[['(5)']]} franchises (descriptive); ",
           "{NDocBlackHires[['(2)']]} hires in columns (1)-(3) are documented as Black. Table 29 repeats ",
           "column (2) under the other race measures. ", NoteHCDiag)
    } else if (IsPredMeasure) {
      glue("Under the preddoc measure the new head coach's race is 1 or 0 when documented and a model ",
           "probability otherwise. The hire sample contains an expected ",
           "{fmt14(ExpBlackHires[['(2)']], 1)} Black hires in columns (1)-(3) and ",
           "{fmt14(ExpBlackHires[['(5)']], 1)} in columns (4)-(6) (sum of the probabilities). ",
           "Among the hires of column (2), the slope of documented race on the ",
           "regressor (season FE) is {Calib22Txt}.")
    } else {
      glue("Power is low: the coefficient is identified from {NBlackHires[['(2)']]} Black hires at ",
           "{NBlackFranchises[['(2)']]} franchises in columns (1)-(3) and {NBlackHires[['(5)']]} at ",
           "{NBlackFranchises[['(5)']]} franchises in columns (4)-(6) under this race measure.")
    },
    glue("With few effectively treated clusters the wild cluster bootstrap can be mis-sized, so the ",
         "bottom row adds a randomization-inference $p$-value that permutes the race regressor ",
         "(column 7: documented race) across hires within season, holding the other regressors fixed ",
         "({format(RiReps, big.mark = ',')} permutations, two-sided, on the coefficient)."),
    glue("The glass-cliff threat has opposite signs by outcome. If Black head coaches are hired into ",
         "persistently worse situations than the controls capture, the level coefficients are biased ",
         "downward; if the inherited record is transiently good or bad, the change in win percentage ",
         "(columns 1 and 4) mean-reverts. Regressing the lagged win percentage on the race regressor, ",
         "lagged market expected wins and the retained-interim indicator (season FE",
         if (UsePrior) ", prior control" else "", ") gives {Select22Txt[['2000']]} for the hires from ",
         "{FirstHire22} and {Select22Txt[['2007']]} for those from 2007: a positive coefficient means ",
         "that such hires inherit teams that beat their market expectation, which biases columns (1) ",
         "and (4) downward."),
    glue("Standard errors, in parentheses, are clustered at the franchise level ",
         "({n_distinct(Models22[['(2)']]$EstData$franchise_id)} clusters)."),
    NoteWcb),
  name = "table-22-hc-hire-race", measure = measure, design = "team",
  add_rows = Rows22)
Estimates <- c(Estimates, list(tidy_terms(Models22, names(CoefMap22)) |>
                                 mutate(table = "table-22") |>
                                 left_join(tibble(model = names(RiP22), term = RiParam22,
                                                  p_ri = unname(RiP22)),
                                           by = c("model", "term"))))

# ---------------------------------------------------------------------------
# Table 23: placebo leads and selection on past performance
# ---------------------------------------------------------------------------

# Columns (1)-(3): current win percentage on current and next-season staff
# composition (lead shares are not zero-filled; 2025 has no lead). The
# next-season OC/DC/GM indicators are zero-filled with missing indicators
# (no listed holder, e.g. no GM title), like the current ones. Columns
# (4)-(5): lagged win percentage on current composition (selection)
TeamSeason <- TeamSeason |>
  mutate(across(c(F1OCBlack, F1DCBlack, F1GMBlack), \(x) x, .names = "{.col}Raw")) |>
  fill_missing(c("F1OCBlack", "F1DCBlack", "F1GMBlack"))
TeamSeason <- TeamSeason |>
  mutate(across(c(F1OCBlackMiss, F1DCBlackMiss, F1GMBlackMiss),
                \(x) if_else(is.na(F1ShareBlackCoaches), NA_integer_, x)))
F1Roles <- c("F1OCBlack", "F1DCBlack", "F1GMBlack", "F1OCBlackMiss", "F1DCBlackMiss",
             "F1GMBlackMiss")
F1Groups <- c("F1ShareBlackCoordinators", "F1ShareBlackPositionCoaches", "F1ShareBlackFrontOffice")
SelectionRoster <- c("TeamCapShare", "MeanLogPickRoster", "MeanAgeRoster",
                     "ShareBlackRoster", "ShareBlackRosterMiss")
Spec23 <- list(
  "(1)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", "F1ShareBlackCoaches",
                                     RoleRhs, Controls4)),
  "(2)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", "F1ShareBlackCoaches", "F1HCBlack",
                                     RoleRhs, Controls4)),
  "(3)" = list(y = "WinPct", rhs = c("ShareBlackCoaches", "F1ShareBlackCoaches", "F1HCBlack",
                                     F1Roles, F1Groups, RoleRhs, Controls4)),
  "(4)" = list(y = "LagWinPct", rhs = c("ShareBlackCoaches", "HCBlack", "HCBlackMiss")),
  "(5)" = list(y = "LagWinPct", rhs = c("ShareBlackCoaches", RoleRhs, SelectionRoster)))
Models23 <- map(Spec23, \(s) fit14(s$y, s$rhs, "franchise_id + season", TeamSeason,
                                   prior = UsePrior))
map_int(Models23, nobs) |> print()

Wcb23 <- wcb_all(Models23, c("ShareBlackCoaches", "F1ShareBlackCoaches", "HCBlack", "F1HCBlack"),
                 "table-23")
# Joint test of all leads in column (3) (cluster-robust Wald)
Wald23 <- wald(Models23[["(3)"]], keep = "^F1(ShareBlack|HCBlack$|OCBlack$|DCBlack$|GMBlack$)",
               print = FALSE)
print(Wald23)
WcbResults <- c(WcbResults, list(Wcb23))

CoefMap23 <- c(ShareBlackCoaches = share_lab("Coaches'"),
               F1ShareBlackCoaches = paste0(share_lab("Coaches'"), ", season $t+1$"),
               HCBlack = role_lab("head coach"),
               F1HCBlack = paste0(role_lab("head coach"), ", season $t+1$"),
               F1OCBlack = paste0(role_lab("OC"), ", season $t+1$"),
               F1DCBlack = paste0(role_lab("DC"), ", season $t+1$"),
               F1GMBlack = paste0(role_lab("GM"), ", season $t+1$"),
               F1ShareBlackCoordinators = paste0(share_lab("Coordinators'"), ", $t+1$"),
               F1ShareBlackPositionCoaches = paste0(share_lab("Position coaches'"), ", $t+1$"),
               F1ShareBlackFrontOffice = paste0(share_lab("Front office"), ", $t+1$"),
               CoefMapMain[c("OCBlack", "DCBlack", "GMBlack")])
Rows23 <- bind_rows(
  tibble(term = c("Season FE", "Franchise FE", "Controls (column 4 of Table 19)",
                  "Roster controls")) |>
    bind_cols(tibble("(1)" = c("Yes", "Yes", "Yes", "Lagged"), "(2)" = c("Yes", "Yes", "Yes", "Lagged"),
                     "(3)" = c("Yes", "Yes", "Yes", "Lagged"),
                     "(4)" = c("Yes", "Yes", "No", "No"), "(5)" = c("Yes", "Yes", "No", "Season $t$"))),
  model_row(Models23, "Outcome", \(m) if_else(as.character(m$fml[[2]]) == "WinPct",
                                              "Win \\% ($t$)", "Win \\% ($t-1$)")),
  model_row(Models23, "Model-only prior controls", \(m) yes_no14(m$PriorUsed)),
  model_row(Models23, "Mean of outcome", outcome_mean),
  model_row(Models23, "Joint test of all leads, $p$-value",
            \(m) if (identical(m, Models23[["(3)"]])) fmt_p(Wald23$p) else ""),
  wcb_rows(Wcb23, Models23, c(ShareBlackCoaches = "coaches' share", 
                              F1ShareBlackCoaches = "coaches' share, $t+1$",
                              HCBlack = "head coach", F1HCBlack = "head coach, $t+1$")))

write_model_table(
  Models23, CoefMap23,
  title = "Staff composition: placebo leads and selection on past performance",
  label = "staff-diversity-placebo",
  notes = c(
    glue("This table includes placebo and selection versions of equation (3) on FullStaffObserved ",
         "team-seasons ({min(TeamSeason$season)}-{max(TeamSeason$season)}). Columns (1)-(3) add ",
         "next-season staff composition to the column-(4) specification of Table 19: the coaches' ",
         "Black share (column 1), the head coach's race (column 2), and the OC, DC and GM ",
         "regressors and the coordinators', position coaches' and front-office shares (column 3; ",
         "role regressors zero-filled with missing indicators, group shares not filled). The ",
         "{max(TeamSeason$season)} season and seasons without an observed next season drop out. ",
         "Conditional on current composition, next season's composition should not predict this ",
         "season's performance unless unobserved franchise trends drive both. A nonzero lead is ",
         "also expected if staff are replaced in response to season-$t$ results (firings after bad ",
         "seasons, poaching after good ones) even when current composition has a causal effect, so ",
         "the test is informative mainly when the leads are small. The joint test of all leads in ",
         "column (3) is a cluster-robust Wald test."),
    "Columns (4) and (5) regress the previous season's win percentage on current composition: a nonzero coefficient means that past performance predicts current staff composition (selection), the threat that the lagged-outcome controls address. Column (5) adds the role-holder regressors and season-$t$ roster controls (cap share, mean log draft pick, mean age, roster Black share).",
    if (UsePrior) glue("Under the predicted measure the next-season shares are controlled by the ",
                       "next-season mean priors; no prior exists for the next-season role holders ",
                       "(HC, OC, DC, GM), so those leads enter without one.") else character(),
    NotePredShort, NoteRoles, NoteCluster, NoteWcb, NoteGlassCliff),
  name = "table-23-staff-diversity-placebo", measure = measure, design = "team",
  add_rows = Rows23)
Estimates <- c(Estimates, list(tidy_terms(Models23, names(CoefMap23)) |>
                                 mutate(table = "table-23")))

# ---------------------------------------------------------------------------
# Table 29: headline specifications under each race measure
# ---------------------------------------------------------------------------

# Columns, each estimated under every measure from the unmapped inputs with
# the same functions as above:
#   (1) table 19 column (4) specification without prior controls
#   (2) the same with the model-only prior controls
#   (3) table 19 column (5) (HC-spell FE)
#   (4) table 22 column (2) (hires, win percentage with the lagged win
#       percentage free)
#   (5) table 21 column (2) (unit-stacked, headcount controls)
#   (6) table 19 column (9) (horse race with the other probability measure)
# Columns (3)-(5) use the prior controls in the predicted panel only (as in
# tables 19-23); column (6) always does. Under the predicted measure column
# (2) is table 19 column (4) and column (1) is its column (8); under the
# other measures the reverse. The table compares measures, so it is written
# only in the primary-measure run (its content does not depend on
# NFL_RACE_MEASURE).
RunTable29 <- is_primary_measure(measure)
Cols29 <- tribble(
  ~col,  ~params,
  "(1)", c("ShareBlackCoaches", "HCBlack"),
  "(2)", c("ShareBlackCoaches", "HCBlack"),
  "(3)", "ShareBlackCoaches",
  "(4)", "HCBlack",
  "(5)", c("UnitCoordBlack", "ShareBlackUnitCoaches"),
  "(6)", c("ShareBlackCoaches", "ShareBlackAltCoaches"))

# Models (displayed and, for the unit design, the dummy version for the
# bootstrap) and WCB results of one measure
fit_measure29 <- function(m) {
  pr <- m == "predicted"
  ts_all <- prep_team_season(TeamSeasonRaw, m)
  ts <- filter(ts_all, FullStaffObserved)
  us <- prep_unit_season(UnitSeasonRaw, ts_all, m)
  hires <- build_hires(ts_all)
  s4 <- Spec19[["(4)"]]
  models <- list(
    "(1)" = fit_spec19(modifyList(s4, list(prior = FALSE)), ts, pr),
    "(2)" = fit_spec19(modifyList(s4, list(prior = TRUE)), ts, pr),
    "(3)" = fit_spec19(Spec19[["(5)"]], ts, pr),
    "(4)" = fit14(Spec22[["(2)"]]$y, c(Spec22[["(2)"]]$rhs, "RetainedInterim"), "season", hires,
                  prior = pr),
    "(5)" = fit14(Spec21[["(2)"]]$y, Spec21[["(2)"]]$rhs, "franchise_id^season + Unit^season", us,
                  prior = pr),
    "(6)" = fit_spec19(Spec19[["(9)"]], ts, pr))
  unit_wcb_model <- fit14(Spec21[["(2)"]]$y, c(Spec21[["(2)"]]$rhs, UnitSeasonDummies),
                          "franchise_id^season", us, prior = pr)
  stopifnot(isTRUE(all.equal(coef(models[["(5)"]]),
                             coef(unit_wcb_model)[names(coef(models[["(5)"]]))], tolerance = 1e-6)))
  wcb_res <- map_dfr(seq_len(nrow(Cols29)), \(i) {
    cl <- Cols29$col[i]
    if (cl == "(5)" && !UnitWcbRun) return(tibble())
    mod <- if (cl == "(5)") unit_wcb_model else models[[cl]]
    map_dfr(Cols29$params[[i]], \(p) wcb(mod, p, cl, paste0("table-29-", m)))
  }) |>
    rename(col = model)
  list(models = models, wcb = wcb_res)
}

if (RunTable29) {
  # Measures: the primary one first, then the predicted, documented and
  # provisional measures, and hand-coded race when it is coded
  Measures29 <- unique(c(measure, "predicted", "preddoc", "provisional",
                         if (isTRUE(HandCoverage >= MinHandCoverage)) "hand"))
  Fits29 <- map(set_names(Measures29), fit_measure29)

  # Coefficient cells: estimate with stars from the clustered p-value, the
  # clustered SE in parentheses and the WCB p-value in brackets
  star29 <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
  Terms29 <- c(ShareBlackCoaches = "Coaches' share Black", HCBlack = "Head coach (new head coach in col. 4)",
               UnitCoordBlack = "Unit coordinator",
               ShareBlackUnitCoaches = "Unit coaches' share Black",
               ShareBlackAltCoaches = "Coaches' share Black, other measure$^a$")
  Est29 <- imap_dfr(Fits29, \(f, m) tidy_terms(f$models, names(Terms29)) |>
                      rename(col = model) |>
                      left_join(select(f$wcb, col, term, p_wcb, wcb_ci_low = ci_low,
                                       wcb_ci_high = ci_high),
                                by = c("col", "term")) |>
                      mutate(measure29 = m))
  # Keep each column's headline terms
  Est29 <- Est29 |>
    filter(map2_lgl(col, term, \(cl, tm) tm %in% Cols29$params[[match(cl, Cols29$col)]]))
  cell29 <- function(m, tm, cl, what) {
    r <- filter(Est29, measure29 == m, term == tm, col == cl)
    if (nrow(r) == 0) return("")
    switch(what,
           est = paste0(fmt14(r$estimate, 3), star29(r$p_value)),
           se = paste0("(", fmt14(r$std_error, 3), ")"),
           wcb = if (is.na(r$p_wcb)) "" else paste0("[", fmt_p(r$p_wcb), "]"))
  }
  Body29 <- map_dfr(Measures29, \(m) {
    rows <- map_dfr(names(Terms29), \(tm) {
      map_dfr(c("est", "se", "wcb"), \(w) {
        row <- tibble(Row = if (w == "est") Terms29[[tm]] else "")
        for (cl in Cols29$col) row[[cl]] <- cell29(m, tm, cl, w)
        row
      })
    }) |>
      filter(if_any(all_of(Cols29$col), \(x) x != ""))
    obs <- tibble(Row = "Observations")
    for (cl in Cols29$col) obs[[cl]] <- fmt14(nobs(Fits29[[m]]$models[[cl]]), 0)
    bind_rows(rows, obs) |> mutate(Measure = m)
  })
  # Specification rows (identical across measures)
  Spec29 <- tribble(
    ~Row,                ~`(1)`,        ~`(2)`,        ~`(3)`,                ~`(4)`,     ~`(5)`,     ~`(6)`,
    "Outcome",           "Win \\%",     "Win \\%",     "Win \\%",             "Win \\%",  "Unit EPA/play", "Win \\%",
    "Fixed effects",     "Fr., season", "Fr., season", "Fr. $\\times$ spell, season", "Season",
    "Fr. $\\times$ season, unit $\\times$ season", "Fr., season",
    "Model-only prior controls", "No",  "Yes",         "Predicted panel",     "Predicted panel", "Predicted panel", "Yes",
    "Source (predicted panel)", "T19 (8)", "T19 (4)",  "T19 (5)",             "T22 (2)",  "T21 (2)",  "T19 (9)",
    "Source (other panels)", "T19 (4)", "T19 (8)",     "T19 (5)",             "T22 (2)",  "T21 (2)",  "T19 (9)")
  MeasureLabels29 <- c(predicted = "Predicted race (model only)",
                       preddoc = "Documented race, else predicted (preddoc)",
                       provisional = "Wikipedia category flag (provisional)",
                       hand = "Hand-coded race")
  PanelLabels29 <- paste0(
    "Panel ", LETTERS[seq_along(Measures29)], ": ", MeasureLabels29[Measures29],
    if_else(Measures29 == getOption("nfl.race_primary"), " (primary)", ""))
  PanelSizes29 <- as.integer(table(factor(Body29$Measure, levels = Measures29)))

  # Data-driven summary of the coaches' share across measures: the headline
  # column of each panel (column 2 for the predicted measure, column 1
  # otherwise) and the horse race of column (6)
  get29 <- function(m, cl, tm) filter(Est29, measure29 == m, col == cl, term == tm)
  Share29 <- map_dfr(Measures29, \(m) get29(m, if (m == "predicted") "(2)" else "(1)",
                                            "ShareBlackCoaches"))
  Share29Txt <- paste(glue("{Share29$measure29} {fmt14(Share29$estimate, 3)} ",
                           "(WCB $p$ = {fmt_p(Share29$p_wcb)})"), collapse = "; ")
  Horse29 <- get29("predicted", "(6)", "ShareBlackCoaches")
  HorseAlt29 <- get29("predicted", "(6)", "ShareBlackAltCoaches")
  Horse29Txt <- glue("In the predicted panel, column (6) gives {fmt14(Horse29$estimate, 3)} ",
                     "(SE {fmt14(Horse29$std_error, 3)}, WCB $p$ = {fmt_p(Horse29$p_wcb)}) for the ",
                     "predicted share and {fmt14(HorseAlt29$estimate, 3)} (SE ",
                     "{fmt14(HorseAlt29$std_error, 3)}, WCB $p$ = {fmt_p(HorseAlt29$p_wcb)}) for the ",
                     "documented-race (preddoc) share entered jointly.")

  Tab29 <- bind_rows(select(Body29, -Measure), Spec29) |>
    kbl(format = "latex", booktabs = TRUE, escape = FALSE, linesep = "", align = "lcccccc",
        col.names = c("", paste0("(", 1:6, ")")),
        caption = paste0("Staff composition and team performance under alternative race measures",
                         " \\label{tab:staff-diversity-race-measures}")) |>
    kable_styling(latex_options = c("hold_position", "scale_down")) |>
    add_header_above(c(" " = 1, "Franchise FE" = 2, "Spell FE" = 1, "Hires" = 1,
                       "Unit stacked" = 1, "Joint" = 1))
  Start29 <- cumsum(c(1, head(PanelSizes29, -1)))
  for (i in seq_along(Measures29)) {
    Tab29 <- pack_rows(Tab29, PanelLabels29[i], Start29[i], Start29[i] + PanelSizes29[i] - 1,
                       escape = FALSE)
  }
  Tab29 <- pack_rows(Tab29, "Specification", sum(PanelSizes29) + 1, sum(PanelSizes29) + nrow(Spec29))
  Tab29 <- add_notes(Tab29, c(
    glue("This table re-estimates the headline specifications of equation (3) under each race ",
         "measure. Column (1) is the Table 19 column-(4) specification (franchise and season FE, ",
         "predetermined controls) without the model-only prior controls and column (2) the same ",
         "with them (the members' mean prior for each share and the holder's prior for each role ",
         "holder); in the predicted panel column (2) is Table 19 column (4) and column (1) its ",
         "column (8), in the other panels the reverse. Column (3) is Table 19 column (5) ",
         "(franchise $\\times$ head-coach-spell and season FE, singleton spells removed); column (4) ",
         "is Table 22 column (2) (head-coach hires, win percentage with the lagged win percentage ",
         "and market expected wins, season FE); column (5) is Table 21 column (2) (offense and ",
         "defense stacked, franchise $\\times$ season and unit $\\times$ season FE, unit roster and ",
         "QB/HC controls); column (6) is Table 19 column (9). Columns (3)-(5) include the prior ",
         "controls in the predicted panel only, where they are the regression-calibration controls; ",
         "elsewhere the model-only prior is an ordinary predetermined control (under preddoc a ",
         "documented person's probability does not depend on it). Each panel rebuilds the samples ",
         "and every race regressor (shares, role holders, roster share controls) under its measure."),
    paste("$^a$ Column (6) adds to column (2) the coaches' share and head-coach regressor of another",
          "probability measure: the documented-race (preddoc) measure in the predicted panel and the",
          "model-only predicted measure in the other panels."),
    paste("Fr.\\ = franchise. Predicted race: each person's probability of being Black from first name,",
          "surname and hometown county combined with an NFL-specific prior on predetermined",
          "characteristics; shares are expected shares and role holders enter as probabilities.",
          "Documented race (preddoc): the documented race where a public source states it, else",
          "a prediction; documentation depends on fame, so its error may be correlated with",
          "outcomes. Provisional: the positive-only Wikipedia category flag, a lower bound whose",
          "coverage varies with Wikipedia editing; non-flagged persons count as non-Black."),
    glue("The coaches' share coefficient in each panel's prior-consistent franchise-FE column ",
         "(column 2 in the predicted panel, column 1 elsewhere) is: {Share29Txt}. {Horse29Txt} ",
         "A predicted-share coefficient that persists conditional on the documented share rests on ",
         "the part of P(Black) that departs from documented race, which regression calibration ",
         "cannot read as a race effect; magnitudes across measures are not comparable without a ",
         "calibration slope of the true on the expected share, which is not estimated here. ",
         "Head-coach validation for the predicted measure: see the notes to Tables 19 and 22."),
    glue("Each coefficient is followed by its standard error, clustered at the franchise level ",
         "({NClusters} clusters), in parentheses and by its wild cluster bootstrap-t $p$-value ",
         "(restricted, Webb six-point weights, {format(WcbReps, big.mark = ',')} replications, ",
         "clustered by franchise) in brackets; stars use the clustered $p$-value."),
    if (!UnitWcbRun) "Wild cluster bootstrap $p$-values are not reported for the unit design (column 5 of this table); see the notes to Table 21."))
  save_exhibit_tex(Tab29, "table-29-staff-diversity-race-measures", measure)
  save_estimates(Est29, "14-staff-diversity-race-measures", measure)
  print(select(Est29, measure29, col, term, estimate, std_error, p_value, p_wcb, nobs) |>
          mutate(across(c(estimate, std_error, p_value, p_wcb), \(x) round(x, 4))), n = Inf)
} else {
  message("14: table 29 (race measures) is written only in the primary-measure run")
}

if (length(PriorMissingLog) > 0) {
  message("14: race regressors without a prior column (entered without one): ",
          paste(PriorMissingLog, collapse = ", "))
}

# ---------------------------------------------------------------------------
# Tidy estimates (clustered and WCB) and run summary
# ---------------------------------------------------------------------------

# One row per table x model x term, with the WCB p-value and CI where run
EstimatesAll <- bind_rows(Estimates) |>
  left_join(bind_rows(WcbResults) |>
              select(table, model, term, p_wcb, wcb_ci_low = ci_low, wcb_ci_high = ci_high,
                     wcb_reps = B),
            by = c("table", "model", "term")) |>
  mutate(within_sd_coaches = WithinSDCoaches, hand_coverage = HandCoverage)
save_estimates(EstimatesAll, "14-staff-diversity", measure)

# Main coefficients and bootstrap timing
EstimatesAll |>
  filter(term %in% c("ShareBlackCoaches", "HCBlack", "UnitCoordBlack", "ShareBlackUnitCoaches",
                     "F1ShareBlackCoaches", names(GroupVars), "BlauBlackCoaches")) |>
  select(table, model, term, estimate, std_error, p_wcb, nobs) |>
  mutate(across(c(estimate, std_error, p_wcb), \(x) round(x, 4))) |>
  print(n = Inf)
bind_rows(WcbLog) |> summarise(tests = n(), total_secs = sum(seconds),
                               max_secs = max(seconds), .by = table) |> print()
message(glue("14: done in {round(as.numeric(difftime(Sys.time(), T0Script, units = 'secs')))}s"))

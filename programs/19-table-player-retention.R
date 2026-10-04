# ============================================================================
# 19-table-player-retention.R
# Who stays employed and who reaches a freely bargained contract? Linear
# probability models on the player-season risk sets of 18, for player i in
# position group g and season t:
#   Y_{i,t+1} = beta_1 Black_i + beta_2 Other_i + X_it pi + delta_{g,t} + e_it
# with position group x season fixed effects and standard errors clustered
# by player. Under the predicted race measures Black_i and Other_i are
# probabilities (regression calibration) and the prior's covariates enter as
# fixed effects, as in 12.
# Outcomes
#   Table 34 (retention): under contract to any franchise in a game week of
#   t+1, among players under contract in a game week of t (1 - Y is the
#   discrete-time exit hazard after season t); game-day, same-franchise,
#   final-week and practice-squad-inclusive variants.
#   Table 35 (access): first UFA or Extension contract signed in year t+1,
#   among players under contract in t who have not signed one by year t
#   (rookie cohorts 2011+, seasons 2013-2024); veteran-market and any-non-
#   rookie variants; access among players retained in t+1; the rookie-
#   contract expiry window (experience 2-4).
#   Table 34b: both headline specifications under every race measure and
#   predicted-race variant.
#   Table 34c: the column-(4) retention specification with and without
#   employer x season FE on identical rows (employer = 18 RetentionEmployer,
#   the last verified under-contract franchise of t; tied final weeks and
#   singleton employer-season cells dropped from both columns).
# Controls follow 10's quality blocks measured in season t and before
# (analysis/player_retention_control_blocks.csv): A career stage, B season-t
# production, C career production, D pre-NFL signals, E season-t usage, F
# scout grade, G season-t employment. Position-group slopes are explicit
# interaction columns as in 12.
# Everything here is a conditional association among players already
# employed in season t; survival to t is itself selected. The estimates
# describe who stays and who reaches a bargained contract given observed
# quality, not a causal effect of race. Pay is never an outcome or a control.
# Inputs: analysis/analysis_player_retention.parquet,
# analysis/player_retention_control_blocks.csv (18).
# Outputs: output/tables/table-34-player-retention,
# table-35-player-contract-access, table-34b-player-retention-race-measures,
# table-34c-player-retention-employer-fe
# (.tex; my_paper/tables for the primary measure),
# output/figures/figure-player-retention-by-experience (.pdf/.png),
# output/estimates/19-player-retention[-<measure>].{csv,dta} (tidy
# coefficients with CI, outcome mean, MDE, sample flow and estimator/
# estimand/FE/inference/limitation columns; save_estimates() writes both)
# and 19-player-retention-sample-flow[-<measure>].{csv,dta}.
# Run after 18 (and after 15) in 95-make-all.R.
# Date: 2026-10-03
# ============================================================================

T0Script19 <- Sys.time()
msg19 <- function(...) message(sprintf("19: %s", sprintf(...)))

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

Ret <- as.data.table(arrow::read_parquet(file.path(analysis, "analysis_player_retention.parquet")))
Blocks19 <- fread(file.path(analysis, "player_retention_control_blocks.csv"))
stopifnot(all(Blocks19$variable %in% names(Ret)))
NInput <- nrow(Ret)

# ---------------------------------------------------------------------------
# Race measure (one for the whole script), regressors and prior FE
# ---------------------------------------------------------------------------

HandCoverage <- mean(!is.na(Ret$black_any[Ret$InRiskSetRetention == 1L]))
measure <- choose_race_measure(HandCoverage, "19 player retention")
is_pred <- function(m) m %in% c("predicted", "preddoc")

# Fixed effects for the covariates of the race prior. Under preddoc the
# prior also conditions on the Wikipedia article and a career-length bucket;
# career length is the outcome here, so those two are NOT controlled and the
# preddoc column of Table 34b is a mis-specified sensitivity, labelled so
prior_fe_vars <- function(m) {
  v <- race_prior_controls(m, "player")
  if (is_pred(m)) v <- c(v, "pred_county_available")
  intersect(v, names(Ret))
}
PriorFE <- prior_fe_vars(measure)
stopifnot(!is_pred(measure) || length(PriorFE) >= 4)
for (v in intersect(c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket",
                      "pred_college_type", "pred_county_available"), names(Ret))) {
  Ret[is.na(get(v)), (v) := "unknown"]
}
Ret <- as.data.table(person_race_regressors(as.data.frame(Ret), measure))
BlackLab <- if (is_pred(measure)) "P(Black)" else "Black"
OtherLab <- if (is_pred(measure)) "P(other race)" else "Other race"
Rhs0 <- c("Black", "OtherRace")
# The prior covariates enter as dummy columns Prior_<var>_<level> (reference:
# the modal level) rather than as extra fixest FE, as in 12: one absorbed FE
# (position group x season) is demeaned exactly in one pass, whereas six FE
# dimensions with 450 control columns took about a minute per model
FE19 <- "PositionGroup^season"
FE19Label <- if (is_pred(measure)) "PositionGroup^season + prior-covariate dummies" else FE19

# ---------------------------------------------------------------------------
# Design helpers (explicit slope interactions, as in 12)
# ---------------------------------------------------------------------------

level_tag <- function(x) sub("_$", "", gsub("[^A-Za-z0-9]+", "_", gsub("\\+", "plus", x)))
add_dummies <- function(df, var, prefix, ref = NULL) {
  x <- as.character(df[[var]])
  levs <- sort(unique(x[!is.na(x)]))
  if (is.null(ref)) ref <- names(which.max(table(x)))
  cols <- character()
  for (l in setdiff(levs, ref)) {
    nm <- paste0(prefix, "_", level_tag(l))
    df[[nm]] <- as.integer(!is.na(x) & x == l)
    cols <- c(cols, nm)
  }
  list(df = df, cols = cols)
}
# Control columns by block: x * 1{PositionGroup == g} for each listed group
# (all = every group in df; none = x linearly), plus the categorical dummies
build_design <- function(df, dict, cat_vars = list()) {
  df <- as.data.frame(df)
  groups <- sort(unique(df$PositionGroup))
  cols <- list()
  new <- list()
  for (b in sort(unique(dict$block))) {
    rows <- dict[dict$block == b, ]
    bc <- character()
    for (i in seq_len(nrow(rows))) {
      v <- rows$variable[i]
      sg <- rows$slope_groups[i]
      if (sg == "none") { bc <- c(bc, v); next }
      gs <- if (sg == "all") groups else intersect(strsplit(sg, ";")[[1]], groups)
      for (g in gs) {
        nm <- paste0(v, "_x_", g)
        new[[nm]] <- as.numeric(df[[v]]) * (df$PositionGroup == g)
        bc <- c(bc, nm)
      }
    }
    for (cv in cat_vars[[b]]) {
      d <- add_dummies(df, cv, cv)
      df <- d$df
      bc <- c(bc, d$cols)
    }
    cols[[b]] <- bc
  }
  df <- cbind(df, as.data.frame(new, check.names = FALSE))
  list(df = as.data.table(df), cols = cols)
}
varying <- function(df, cols) cols[vapply(cols, function(v) length(unique(df[[v]])) > 1L, logical(1))]
make_fml <- function(y, rhs, fe = "") {
  f <- paste(y, "~", paste(rhs, collapse = " + "))
  if (nzchar(fe)) f <- paste(f, "|", fe)
  as.formula(f)
}
fmt_n <- function(x, digits = 3) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
drop_empty <- function(x) x[!is.na(x) & nzchar(x)]

# One design over every row in any risk set; experience bins and draft
# round enter as dummies (blocks A and D)
AnyRisk <- Ret[InRiskSetRetentionIncl2015 == 1L | InRiskSetEmployed == 1L | InRiskSetBargained == 1L |
                 InRiskSetVeteranMarket == 1L | InRiskSetNonRookie == 1L]
AnyRisk[, DraftRound := as.character(DraftRound)]
Design <- build_design(AnyRisk, Blocks19, cat_vars = list(A = "ExperienceBin", D = "DraftRound"))
Data19 <- Design$df
DesignCols <- Design$cols
PriorVarsAll <- c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket", "pred_college_type",
                  "pred_county_available")
PriorDummyCols <- list()
for (v in intersect(PriorVarsAll, names(Data19))) {
  d <- add_dummies(as.data.frame(Data19), v, paste0("Prior_", v))
  Data19 <- as.data.table(d$df)
  PriorDummyCols[[v]] <- d$cols
}
# Prior dummy columns for the prior variables `prior` that vary in df
prior_cols <- function(df, prior = PriorFE) {
  varying(df, unlist(PriorDummyCols[intersect(prior, names(PriorDummyCols))], use.names = FALSE))
}
msg19("design: %s rows; controls by block: %s", format(nrow(Data19), big.mark = ","),
      paste(names(DesignCols), lengths(DesignCols), sep = "=", collapse = ", "))
controls_for <- function(df, blocks) {
  unlist(lapply(DesignCols[blocks], function(cc) varying(df, cc)), use.names = FALSE)
}

# ---------------------------------------------------------------------------
# Estimation helpers, sample flow and tidy output
# ---------------------------------------------------------------------------

KnownRaceText <- switch(measure, hand = "known (hand-coded) race",
                        provisional = "a Wikipedia article (race classified by the provisional measure)",
                        "a race prediction")
Flow <- list()
Estimates19 <- list()

# Estimation sample: a risk set, the outcome observed, known race
risk_sample <- function(df, risk_flag, outcome, label, extra = rep(TRUE, nrow(df))) {
  in_risk <- df[[risk_flag]] == 1L & extra
  d <- df[in_risk]
  n_outcome <- sum(!is.na(d[[outcome]]))
  d <- d[!is.na(d[[outcome]]) & RaceKnown]
  Flow[[label]] <<- data.table(sample = label, risk_flag = risk_flag, outcome = outcome,
                               n_input = NInput, n_risk_set = sum(in_risk),
                               n_outcome_observed = n_outcome, n_known_race = nrow(d),
                               n_players = uniqueN(d$gsis_id))
  d
}

# Guarded fixest fit; failure is fatal with the column key in the message.
# fixest drops collinear columns silently (the coefficient is absent, so the
# tidy file would carry no row rather than an aliased zero); a dropped Black
# regressor is fatal, a dropped OtherRace (possible under the indicator
# measures in small 34b variants) is reported
fit_lpm <- function(df, y, rhs, prior = PriorFE, key = y, fe = FE19) {
  m <- tryCatch(feols(make_fml(y, c(rhs, prior_cols(df, prior)), fe), data = df,
                      vcov = ~gsis_id, notes = FALSE),
                error = function(e) e)
  if (inherits(m, "error")) {
    stop(sprintf("19: model %s failed: %s", key, conditionMessage(m)), call. = FALSE)
  }
  if (!"Black" %in% names(coef(m))) {
    stop(sprintf("19: model %s dropped Black as collinear", key), call. = FALSE)
  }
  if ("OtherRace" %in% rhs && !"OtherRace" %in% names(coef(m))) {
    msg19("model %s dropped OtherRace as collinear (no row in the tidy file)", key)
  }
  m
}
mean_dep <- function(m, df) mean(df[[as.character(m$fml[[2]])]][obs(m)])
n_players <- function(m, df) uniqueN(df$gsis_id[obs(m)])

# Tidy rows of a named model list with baseline, MDE (80% power, 5% size:
# 2.8 x SE) and sample flow; fe_labels (named by model) records a model's
# own fixed effects when they differ from FE19Label
tidy_block <- function(models, datas, table, terms = Rhs0, flows = names(models),
                       fe_labels = NULL) {
  out <- lapply(names(models), function(nm) {
    m <- models[[nm]]
    d <- datas[[nm]]
    t <- tidy_terms(setNames(list(m), nm), terms)
    if (nrow(t) == 0) return(NULL)
    fl <- Flow[[flows[[match(nm, names(models))]]]]
    t$outcome_mean <- mean_dep(m, d)
    t$n_players <- n_players(m, d)
    t$mde_80 <- 2.8 * t$std_error
    t$n_input <- fl$n_input
    t$n_risk_set <- fl$n_risk_set
    t$n_known_race <- fl$n_known_race
    t$n_estimated <- nobs(m)
    t$table <- table
    if (!is.null(fe_labels)) t$fixed_effects <- fe_labels[[nm]]
    t
  })
  rbindlist(out, use.names = TRUE)
}

# Notes shared by the tables
RefNote <- race_reference_note(measure)
NoteCluster <- "Standard errors, clustered by player, in parentheses. $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$."
PriorNote <- if (is_pred(measure)) {
  "Every column also includes fixed effects for the covariates of the race prediction's prior (position at NFL entry, entry era, draft-round bucket, college type and whether a home county is known), as regression calibration requires."
} else ""
RCNote <- if (is_pred(measure)) {
  paste("Under the predicted measure the coefficient on P(Black) equals the Black-white gap only under two assumptions beyond names and hometown being unrelated to the outcome given race and the controls:",
        "(i) P(Black) is calibrated conditional on every control in the column, not only on the prior's covariates, and (ii) the gap is the same across values of the controls (otherwise the coefficient is a variance-weighted average of gaps).",
        "Adding controls changes which conditional calibration is required; it also changes the overlap of P(Black) within cells, which affects precision rather than the estimand. Changes in the coefficient across columns therefore cannot be attributed to the controls' effect on the gap without checking (i) and (ii) in each column.")
} else ""
NoteBlocks <- "Block A: experience-bin dummies, experience, age and its square. Block B: season-$t$ games played, injury weeks and position-appropriate production with position-group-specific slopes (as the lagged block of Table \\ref{tab:pay-gap-learning}, measured in $t$). Block C: career production before $t$. Block D: log draft pick, undrafted, draft-round dummies, 247 rating and stars, combine measurables and athletic scores, college programme. Block F: CFBD pre-draft grade. Block E: season-$t$ snaps, snap-based starts and career snaps and starts (2013+; zero before with an indicator). Block G: weeks under contract in $t$, under contract in the final week of $t$ and more than one spell. Controls with incomplete coverage are zero-filled with a missing indicator, so no player-season is dropped for a missing control."
NoteSelection <- "The estimates are conditional associations among players already employed in season $t$; survival to $t$ is itself selected on the same unobservables that drive the outcome, so the coefficients describe who stays and who reaches a bargained contract given observed quality, not a causal effect of race. Pay is neither an outcome nor a control; roster weeks are not paid weeks."
NoteWeeks <- "Employment is measured from the weekly rosters, one row per person-week (a player listed by two franchises in a week counts once), in game weeks of the listing franchise (bye weeks excluded, so counts are comparable across eras); under contract means on the 53-man roster, a reserve list or suspended/exempt, which excludes practice-squad, released, retired and free-agent listings. No listing at all in $t+1$ is an exit. The 2016 source leaks preseason rosters into weeks 1-2; listings of players who neither played that week nor stayed under contract from week 3 are unverified and do not count as employment, and a $t+1$ whose only listings are unverified, trade-pending or of unknown status leaves the outcome unknown (excluded) rather than recording an exit."

# ---------------------------------------------------------------------------
# Table 34: next-season retention
# ---------------------------------------------------------------------------

msg19("table 34 at %ds", round(as.numeric(difftime(Sys.time(), T0Script19, units = "secs"))))
RetData <- risk_sample(Data19, "InRiskSetRetention", "RetainedNextSeason", "retention")
RetSame <- risk_sample(Data19, "InRiskSetRetention", "RetainedNextSeasonSameFranchise", "retention same franchise")
EmpData <- risk_sample(Data19, "InRiskSetEmployed", "RetainedNextSeasonEmployed", "employed 2016+")
msg19("retention risk set: %s player-seasons, %s with known race, %s players",
      format(Flow$retention$n_risk_set, big.mark = ","),
      format(Flow$retention$n_known_race, big.mark = ","),
      format(Flow$retention$n_players, big.mark = ","))
c34 <- function(df, blocks) c(Rhs0, controls_for(df, blocks))
Models34 <- list(
  "(1)" = fit_lpm(RetData, "RetainedNextSeason", Rhs0, key = "34 (1)"),
  "(2)" = fit_lpm(RetData, "RetainedNextSeason", c34(RetData, "A"), key = "34 (2)"),
  "(3)" = fit_lpm(RetData, "RetainedNextSeason", c34(RetData, c("A", "B", "C")), key = "34 (3)"),
  "(4)" = fit_lpm(RetData, "RetainedNextSeason", c34(RetData, c("A", "B", "C", "D", "F")), key = "34 (4)"),
  "(5)" = fit_lpm(RetData, "RetainedNextSeason", c34(RetData, c("A", "B", "C", "D", "F", "E", "G")), key = "34 (5)"),
  "(6)" = fit_lpm(RetData, "RetainedNextSeasonGameDay", c34(RetData, c("A", "B", "C", "D", "F")), key = "34 (6)"),
  "(7)" = fit_lpm(RetSame, "RetainedNextSeasonSameFranchise", c34(RetSame, c("A", "B", "C", "D", "F")), key = "34 (7)"),
  "(8)" = fit_lpm(RetData, "RetainedNextSeasonFinalWeek", c34(RetData, c("A", "B", "C", "D", "F")), key = "34 (8)"),
  "(9)" = fit_lpm(EmpData, "RetainedNextSeasonEmployed",
                  c(c34(EmpData, c("A", "B", "C", "D", "F")), "PracticeSquadOnly"), key = "34 (9)"))
Datas34 <- list("(1)" = RetData, "(2)" = RetData, "(3)" = RetData, "(4)" = RetData, "(5)" = RetData,
                "(6)" = RetData, "(7)" = RetSame, "(8)" = RetData, "(9)" = EmpData)
Flows34 <- c(rep("retention", 6), "retention same franchise", "retention", "employed 2016+")
NoD <- if (is_pred(measure)) "\\shortstack{Bucket\\\\(prior)}" else "No"
Rows34 <- data.frame(
  term = c("Position group $\\times$ season FE", "Career stage (A)",
           "\\shortstack[l]{Season-$t$ and career\\\\production (B, C)}",
           "Pre-NFL signals (D, F)",
           "\\shortstack[l]{Season-$t$ usage and\\\\employment (E, G)}", "Outcome",
           "Risk set", "Mean of outcome", "Players", paste(BlackLab, "gap (pp)")),
  stringsAsFactors = FALSE)
OutLab34 <- c(rep("\\shortstack{Under\\\\contract}", 5), "\\shortstack{Game\\\\day}",
              "\\shortstack{Same\\\\franchise}", "\\shortstack{Final\\\\week}",
              "\\shortstack{Under\\\\contract\\\\or PS}")
Risk34 <- c(rep("\\shortstack{Contract, $t$\\\\2002--2024\\\\excl. 2015}", 8),
             "\\shortstack{Employed, $t$\\\\2016--2024}")
Ctrl34 <- list(c("No", "No", NoD, "No"), c("Yes", "No", NoD, "No"), c("Yes", "Yes", NoD, "No"),
               c("Yes", "Yes", "Yes", "No"), c("Yes", "Yes", "Yes", "Yes"), c("Yes", "Yes", "Yes", "No"),
               c("Yes", "Yes", "Yes", "No"), c("Yes", "Yes", "Yes", "No"), c("Yes", "Yes", "Yes", "No"))
for (i in seq_along(Models34)) {
  nm <- names(Models34)[i]
  m <- Models34[[nm]]
  Rows34[[nm]] <- c("Yes", Ctrl34[[i]], OutLab34[i], Risk34[i],
                    fmt_n(mean_dep(m, Datas34[[nm]]), 3), fmt_n(n_players(m, Datas34[[nm]]), 0),
                    fmt_n(100 * coef(m)[["Black"]], 1))
}
CoefMap34 <- c(Black = BlackLab, OtherRace = OtherLab, PracticeSquadOnly = "Practice squad only in $t$")
write_model_table(
  Models34, CoefMap34,
  title = "Race and next-season roster retention: player-season linear probability models",
  label = "player-retention",
  notes = drop_empty(c(
    "This table includes the estimation results of $Y_{i,t+1} = \\beta_1 Black_i + \\beta_2 Other_i + X_{it}\\pi + \\delta_{g,t} + \\varepsilon_{it}$ for player $i$ in position group $g$ and season $t$, estimated by OLS, where $\\delta_{g,t}$ are position group $\\times$ season fixed effects.",
    sprintf("The outcome in columns (1)-(5) is 1 if the player is under contract to any franchise in at least one game week of season $t+1$; one minus the outcome is the discrete-time hazard of leaving NFL employment after season $t$. The risk set is every player-season 2002-2024 with at least one under-contract game week in $t$, experience 0-15 and %s (%s player-seasons; %s of players without %s are excluded; 2025 is right-censored and excluded; $t = 2015$ is excluded because the 2016 rosters leak preseason listings, which makes the 2016 evidence on 2015 players unreliable in both directions, and the tidy estimates report the two bounding treatments of that transition).",
            KnownRaceText, fmt_n(Flow$retention$n_known_race, 0),
            fmt_n(Flow$retention$n_risk_set - Flow$retention$n_known_race, 0),
            sub("^a ", "a ", KnownRaceText)),
    "Column (6) requires a game-day (active or inactive) week in $t+1$; column (7) requires retention by a franchise the player was under contract to in $t$; column (8) requires being under contract in the final calendar week of $t+1$. Column (9) counts practice-squad weeks as employment in both seasons and starts in 2016, when the practice squad is observed in the rosters, with an indicator for practice-squad-only seasons.",
    NoteWeeks, NoteBlocks, PriorNote, RCNote, NoteSelection,
    sprintf("The %s gap is $100\\hat\\beta_1$, in percentage points of the outcome.", BlackLab),
    NoteCluster, RefNote)),
  name = "table-34-player-retention", measure = measure, add_rows = Rows34, font_size = 8)
Estimates19[["table-34"]] <- tidy_block(Models34, Datas34, "table-34", c(Rhs0, "PracticeSquadOnly"), Flows34)

# ---------------------------------------------------------------------------
# Table 34c: employer x season fixed effects on a common sample
# ---------------------------------------------------------------------------

msg19("table 34c at %ds", round(as.numeric(difftime(Sys.time(), T0Script19, units = "secs"))))
# Season-t employer: the franchise of the last verified under-contract week
# of t (18 RetentionEmployer; NA when two franchises listed the player under
# contract in that week, where 18's person-week deduplication picked one by
# a tie-break rather than by evidence). Author decision 2026-10-03: tied
# seasons are dropped, not assigned to either franchise, and never to a
# dummy team. feols keeps singleton and perfect-fit cells (checked, fixest
# 0.12.1), so columns (2) and (3) would use identical rows anyway; singleton
# employer x season cells (one estimation row) are nevertheless dropped from
# both, since such a row is fitted exactly by the employer FE in (3) and
# would carry weight in (2) only. Losses are reported in the notes
EmpKnown <- risk_sample(Data19, "InRiskSetRetention", "RetainedNextSeason", "retention known employer",
                        extra = !is.na(Data19$RetentionEmployer))
cell_key <- function(d) paste(d$RetentionEmployer, d$season)
SingletonCells <- EmpKnown[, .N, by = .(RetentionEmployer, season)][N == 1L]
EmpCommon <- risk_sample(Data19, "InRiskSetRetention", "RetainedNextSeason", "retention employer common sample",
                         extra = !is.na(Data19$RetentionEmployer) &
                           !cell_key(Data19) %in% cell_key(SingletonCells))
NTied34c <- Flow$retention$n_known_race - Flow[["retention known employer"]]$n_known_race
NSingleton34c <- Flow[["retention known employer"]]$n_known_race -
  Flow[["retention employer common sample"]]$n_known_race
msg19("table 34c: %s retention rows -> %s with a known employer (%s tied dropped) -> %s after dropping %d singleton employer x season cells",
      format(Flow$retention$n_known_race, big.mark = ","),
      format(Flow[["retention known employer"]]$n_known_race, big.mark = ","), NTied34c,
      format(Flow[["retention employer common sample"]]$n_known_race, big.mark = ","), nrow(SingletonCells))
stopifnot(NTied34c >= 0L, NSingleton34c == nrow(SingletonCells),
          nrow(EmpCommon) == Flow[["retention employer common sample"]]$n_known_race,
          !anyNA(EmpCommon$RetentionEmployer))
FE34cEmployer <- paste(FE19, "RetentionEmployer^season", sep = " + ")
Ctrl34c <- c("A", "B", "C", "D", "F")
Models34c <- list(
  "(1)" = Models34[["(4)"]],
  "(2)" = fit_lpm(EmpCommon, "RetainedNextSeason", c34(EmpCommon, Ctrl34c), key = "34c (2)"),
  "(3)" = fit_lpm(EmpCommon, "RetainedNextSeason", c34(EmpCommon, Ctrl34c), key = "34c (3)",
                  fe = FE34cEmployer))
Datas34c <- list("(1)" = RetData, "(2)" = EmpCommon, "(3)" = EmpCommon)
Flows34c <- c("retention", "retention employer common sample", "retention employer common sample")
# Both models use the same rows and requested controls. Additional fixed
# effects may legitimately absorb a control, but never the focal race term.
stopifnot(identical(obs(Models34c[["(2)"]]), obs(Models34c[["(3)"]])),
          nobs(Models34c[["(2)"]]) == nrow(EmpCommon),
          nobs(Models34c[["(3)"]]) == nrow(EmpCommon))
Obs34c <- obs(Models34c[["(3)"]])
NEmployers34c <- uniqueN(EmpCommon$RetentionEmployer[Obs34c])
NCells34c <- uniqueN(cell_key(EmpCommon)[Obs34c])
stopifnot(all(EmpCommon[Obs34c, .N, by = .(RetentionEmployer, season)]$N >= 2L))
Rows34c <- data.frame(
  term = c("Position group $\\times$ season FE", "Employer $\\times$ season FE", "Career stage (A)",
           "\\shortstack[l]{Season-$t$ and career\\\\production (B, C)}",
           "Pre-NFL signals (D, F)", "Sample", "Employer $\\times$ season cells",
           "Mean of outcome", "Players", paste(BlackLab, "gap (pp)"), "MDE (80\\% power, pp)"),
  stringsAsFactors = FALSE)
Sample34c <- c("\\shortstack{All\\\\(Table \\ref{tab:player-retention}, col. 4)}",
               "\\shortstack{Known\\\\employer}", "\\shortstack{Known\\\\employer}")
EmpFE34c <- c("No", "No", "Yes")
Cells34c <- c("", "", fmt_n(NCells34c, 0))
for (i in seq_along(Models34c)) {
  nm <- names(Models34c)[i]
  m <- Models34c[[nm]]
  Rows34c[[nm]] <- c("Yes", EmpFE34c[i], "Yes", "Yes", "Yes", Sample34c[i], Cells34c[i],
                     fmt_n(mean_dep(m, Datas34c[[nm]]), 3), fmt_n(n_players(m, Datas34c[[nm]]), 0),
                     fmt_n(100 * coef(m)[["Black"]], 1), fmt_n(100 * 2.8 * se(m)[["Black"]], 1))
}
write_model_table(
  Models34c, CoefMap34[Rhs0],
  title = "Race and next-season roster retention within employer and season",
  label = "player-retention-employer-fe",
  notes = drop_empty(c(
    "This table includes the estimation results of the column-(4) specification of Table \\ref{tab:player-retention} (position group $\\times$ season fixed effects and control blocks A, B, C, D and F) with, in column (3), employer $\\times$ season fixed effects added, so that the race coefficients compare players of the same franchise in the same season. The outcome is 1 if the player is under contract to any franchise in at least one game week of season $t+1$ (retention anywhere in the league, not by the same franchise).",
    "The employer is the franchise of the player's last verified under-contract week of season $t$, measured from the season-$t$ weekly rosters only (never from $t+1$). When two franchises list a player under contract in that week (duplicate listings and trade weeks), the person-week deduplication picks one by a tie-break rather than by evidence; those player-seasons have no known employer and are excluded rather than assigned. Unverified 2016 preseason listings never set the employer.",
    sprintf("Sample flow: column (1) is the Table \\ref{tab:player-retention} column (4) sample (%s player-seasons). Of these, %s have a known employer (%s tied player-seasons dropped); %s employer $\\times$ season cells with a single player-season are dropped from columns (2) and (3) alike (%s player-seasons), because such a row is fitted exactly by the employer fixed effect in (3) and would carry weight in (2) only. Columns (2) and (3) are estimated on the identical %s player-seasons (asserted on the estimation rows of both fits), %s players, %s employers and %s employer $\\times$ season cells.",
            fmt_n(Flow$retention$n_known_race, 0), fmt_n(Flow[["retention known employer"]]$n_known_race, 0),
            fmt_n(NTied34c, 0), fmt_n(nrow(SingletonCells), 0), fmt_n(NSingleton34c, 0),
            fmt_n(nobs(Models34c[["(3)"]]), 0), fmt_n(n_players(Models34c[["(3)"]], EmpCommon), 0),
            fmt_n(NEmployers34c, 0), fmt_n(NCells34c, 0)),
    "Columns (2) and (3) compare the race coefficient with and without employer fixed effects on fixed rows. This is specification sensitivity, not a causal decomposition of the gap: adding fixed effects also changes the residual race-score variation. The employer is the last recorded under-contract franchise during season $t$, not necessarily employment in the final week. Conditioning on that employer and observed season-$t$ employment leaves a descriptive comparison among selected players, not an employer treatment effect.",
    "The MDE row is the gap the column could detect with 80 percent power at the 5 percent level ($2.8$ times the clustered standard error), in percentage points.",
    NoteWeeks, NoteBlocks, PriorNote, RCNote, NoteSelection,
    sprintf("The %s gap is $100\\hat\\beta_1$, in percentage points of the outcome.", BlackLab),
    NoteCluster, RefNote)),
  name = "table-34c-player-retention-employer-fe", measure = measure, add_rows = Rows34c, font_size = 8)
FELabels34c <- c("(1)" = FE19Label, "(2)" = FE19Label,
                 "(3)" = if (is_pred(measure)) paste(FE34cEmployer, "+ prior-covariate dummies") else FE34cEmployer)
Estimates19[["table-34c"]] <- tidy_block(Models34c, Datas34c, "table-34c", Rhs0, Flows34c,
                                         fe_labels = FELabels34c)
Estimates19[["table-34c"]][, `:=`(
  employer_definition = "franchise of the last verified under-contract week of season t (18 RetentionEmployer); tied final weeks have no employer and are dropped",
  n_tied_employer_dropped = NTied34c,
  n_singleton_cells_dropped = nrow(SingletonCells),
  n_singleton_rows_dropped = NSingleton34c,
  n_employers = fifelse(model == "(1)", NA_integer_, NEmployers34c),
  n_employer_cells = fifelse(model == "(1)", NA_integer_, NCells34c),
  common_sample = fifelse(model == "(1)", "Table 34 column (4) sample", "columns (2) and (3): identical estimation rows (fixest::obs asserted)"))]

# ---------------------------------------------------------------------------
# Table 35: access to a first freely bargained contract
# ---------------------------------------------------------------------------

msg19("table 35 at %ds", round(as.numeric(difftime(Sys.time(), T0Script19, units = "secs"))))
AccData <- risk_sample(Data19, "InRiskSetBargained", "BargainedEventNext", "access bargained")
VetData <- risk_sample(Data19, "InRiskSetVeteranMarket", "VeteranMarketEventNext", "access veteran market")
NRData <- risk_sample(Data19, "InRiskSetNonRookie", "NonRookieEventNext", "access non-rookie")
AccRetained <- risk_sample(Data19, "InRiskSetBargained", "BargainedEventNext", "access bargained, retained",
                           extra = Data19$RetainedNextSeason %in% 1L)
AccExpiry <- risk_sample(Data19, "InRiskSetBargained", "BargainedEventNext", "access bargained, experience 2-4",
                         extra = Data19$Experience %in% 2:4)
msg19("access risk set: %s player-seasons, %s with known race, %s players, %s events",
      format(Flow[["access bargained"]]$n_risk_set, big.mark = ","),
      format(Flow[["access bargained"]]$n_known_race, big.mark = ","),
      format(Flow[["access bargained"]]$n_players, big.mark = ","),
      format(sum(AccData$BargainedEventNext), big.mark = ","))
Models35 <- list(
  "(1)" = fit_lpm(AccData, "BargainedEventNext", Rhs0, key = "35 (1)"),
  "(2)" = fit_lpm(AccData, "BargainedEventNext", c34(AccData, "A"), key = "35 (2)"),
  "(3)" = fit_lpm(AccData, "BargainedEventNext", c34(AccData, c("A", "B", "C")), key = "35 (3)"),
  "(4)" = fit_lpm(AccData, "BargainedEventNext", c34(AccData, c("A", "B", "C", "D", "F")), key = "35 (4)"),
  "(5)" = fit_lpm(AccData, "BargainedEventNext", c34(AccData, c("A", "B", "C", "D", "F", "E", "G")), key = "35 (5)"),
  "(6)" = fit_lpm(VetData, "VeteranMarketEventNext", c34(VetData, c("A", "B", "C", "D", "F")), key = "35 (6)"),
  "(7)" = fit_lpm(NRData, "NonRookieEventNext", c34(NRData, c("A", "B", "C", "D", "F")), key = "35 (7)"),
  "(8)" = fit_lpm(AccRetained, "BargainedEventNext", c34(AccRetained, c("A", "B", "C", "D", "F")), key = "35 (8)"),
  "(9)" = fit_lpm(AccExpiry, "BargainedEventNext", c34(AccExpiry, c("A", "B", "C", "D", "F")), key = "35 (9)"))
Datas35 <- list("(1)" = AccData, "(2)" = AccData, "(3)" = AccData, "(4)" = AccData, "(5)" = AccData,
                "(6)" = VetData, "(7)" = NRData, "(8)" = AccRetained, "(9)" = AccExpiry)
Flows35 <- c(rep("access bargained", 5), "access veteran market", "access non-rookie",
             "access bargained, retained", "access bargained, experience 2-4")
Rows35 <- data.frame(
  term = c("Position group $\\times$ season FE", "Career stage (A)",
           "\\shortstack[l]{Season-$t$ and career\\\\production (B, C)}",
           "Pre-NFL signals (D, F)",
           "\\shortstack[l]{Season-$t$ usage and\\\\employment (E, G)}", "First contract of type",
           "Risk set", "Mean of outcome", "Players", paste(BlackLab, "gap (pp)")),
  stringsAsFactors = FALSE)
OutLab35 <- c(rep("\\shortstack{UFA/\\\\extension}", 5), "\\shortstack{Veteran\\\\market}",
              "\\shortstack{Any non-\\\\rookie}", rep("\\shortstack{UFA/\\\\extension}", 2))
Risk35 <- c(rep("All", 7), "\\shortstack{Retained\\\\in $t+1$}",
            "\\shortstack{Experience\\\\2--4}")
for (i in seq_along(Models35)) {
  nm <- names(Models35)[i]
  m <- Models35[[nm]]
  Rows35[[nm]] <- c("Yes", Ctrl34[[i]], OutLab35[i], Risk35[i],
                    fmt_n(mean_dep(m, Datas35[[nm]]), 3), fmt_n(n_players(m, Datas35[[nm]]), 0),
                    fmt_n(100 * coef(m)[["Black"]], 1))
}
NEventsBarg <- sum(AccData$BargainedEventNext)
NFirstBargPlayers <- Ret[AccessCohortObservable == 1L & !is.na(FirstBargainedYear) &
                           FirstBargainedYear > 2013L, uniqueN(gsis_id)]
NFirstBargReached <- Ret[BargainedAtRisk == 1L & BargainedEventNext %in% 1L, uniqueN(gsis_id)]
# Contract-history coverage behind the risk set (one row per player)
Players19 <- Ret[InNflPlayers == 1L & !is.na(RookieSeason), .SD[1L], by = gsis_id,
                 .SDcols = c("RookieSeason", "Undrafted", "EntryContractObserved", "HasAnyContractRow")]
CovDrafted2011 <- Players19[RookieSeason >= 2011L & Undrafted == 0L, mean(EntryContractObserved)]
CovUdfa2011_16 <- Players19[RookieSeason %in% 2011:2016 & Undrafted == 1L, mean(EntryContractObserved)]
CovUdfa2017 <- Players19[RookieSeason >= 2017L & Undrafted == 1L, mean(EntryContractObserved)]
ShareNoOtcRisk <- mean(AccData$HasAnyContractRow == 0L)
fmt_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), " percent")
msg19("entry-contract coverage: drafted 2011+ %s, undrafted 2011-2016 %s, undrafted 2017+ %s; risk-set rows without any OTC row %s",
      fmt_pct(CovDrafted2011), fmt_pct(CovUdfa2011_16), fmt_pct(CovUdfa2017), fmt_pct(ShareNoOtcRisk))
write_model_table(
  Models35, CoefMap34[Rhs0],
  title = "Race and access to a first freely bargained contract: discrete-time risk sets",
  label = "player-contract-access",
  notes = drop_empty(c(
    "This table includes the estimation results of the specification of Table \\ref{tab:player-retention} with the outcome equal to 1 if the player's first observed UFA or Extension contract (OverTheCap contract type; the freely bargained veteran contracts of Table \\ref{tab:pay-gap-veteran}) is signed in year $t+1$, the offseason after season $t$ or during season $t+1$.",
    sprintf("The risk set is every player-season of rookie cohorts 2011 and later, seasons 2013-2024, with at least one under-contract game week in $t$, no UFA or Extension contract observed in a year up to $t$, experience 0-15 and %s (%s player-seasons, %s players, %s first contracts; %s player-seasons without %s are excluded). A player leaves the risk set after his first such contract or when he is no longer on a REG roster; a first contract signed after a season out of the league is not reachable from any risk-set row (%s of %s first contracts of these cohorts after 2013). Signings of 2026 are observed only through the OverTheCap scrape, so $t = 2025$ is excluded.",
            KnownRaceText, fmt_n(Flow[["access bargained"]]$n_known_race, 0),
            fmt_n(Flow[["access bargained"]]$n_players, 0), fmt_n(NEventsBarg, 0),
            fmt_n(Flow[["access bargained"]]$n_risk_set - Flow[["access bargained"]]$n_known_race, 0),
            KnownRaceText, fmt_n(NFirstBargPlayers - NFirstBargReached, 0), fmt_n(NFirstBargPlayers, 0)),
    "OverTheCap records the signing year only. Season $t$ runs into January of calendar year $t+1$, so a contract signed in early January of $t+1$ can precede the last game or two of season $t$ and the season-$t$ controls (blocks B, E and G) are not strictly pre-signing in those cases; most UFA signings fall in March and most extensions between March and September. Retention into season $t+1$ (Table \\ref{tab:player-retention}) is not affected.",
    sprintf("The outcome is the first OBSERVED qualifying contract. OverTheCap lists an entry contract for %s of drafted entrants of the 2011-2025 classes but for only %s of undrafted entrants of 2011-2016 (%s from 2017), and %s of risk-set player-seasons belong to players with no OverTheCap contract row at all (coverage\\_player\\_contract\\_history.csv). Dense entry coverage is evidence that drafted players' histories are well covered, not proof that they are complete: an OverTheCap page can omit a past deal, and a missing entry deal does not by itself hide a later UFA or extension. Cohorts before 2011 are excluded because entry contracts are observed for 7-44 percent of their drafted entrants, a survivor selection. The tidy estimates report the drafted-only, OverTheCap-page-only and observed-entry-contract sensitivities.",
            fmt_pct(CovDrafted2011), fmt_pct(CovUdfa2011_16), fmt_pct(CovUdfa2017), fmt_pct(ShareNoOtcRisk)),
    "Column (6) widens the first contract to the veteran market (adds franchise and transition tags and RFA and ERFA tenders) on its own risk set; column (7) to any non-rookie contract (adds street free agent and other deals, mostly minimum-salary). Column (8) restricts the risk set to players under contract again in $t+1$, so it separates access from exit; column (9) restricts it to experience 2-4, the window in which drafted (four-year) and undrafted (three-year) rookie contracts expire.",
    NoteBlocks, PriorNote, RCNote,
    "Street free agent and practice-squad contracts are bargained too but priced at or near the minimum; the UFA/extension outcome follows the pay-gap samples. The experience-bin dummies of block A are the baseline hazard of the discrete-time model.",
    NoteSelection,
    sprintf("The %s gap is $100\\hat\\beta_1$, in percentage points of the outcome.", BlackLab),
    NoteCluster, RefNote)),
  name = "table-35-player-contract-access", measure = measure, add_rows = Rows35, font_size = 8)
Estimates19[["table-35"]] <- tidy_block(Models35, Datas35, "table-35", Rhs0, Flows35)

# ---------------------------------------------------------------------------
# Table 34b: race measures and predicted-race variants (columns (4))
# ---------------------------------------------------------------------------

msg19("table 34b at %ds", round(as.numeric(difftime(Sys.time(), T0Script19, units = "secs"))))
# Each variant: a function that sets Black/PWhite/OtherRace/RaceKnown on a
# copy of the design, its prior FE and a label
set_prob <- function(df, pb, pw) {
  df <- copy(df)
  df[, Black := get(pb)]
  df[, PWhite := get(pw)]
  df[, OtherRace := pmax(1 - Black - PWhite, 0)]
  df[, RaceKnown := !is.na(Black)]
  df
}
Variants <- list()
add_variant <- function(key, label, data_fn, prior, note = "") {
  Variants[[key]] <<- list(label = label, data_fn = data_fn, prior = prior, note = note)
}
add_variant("predicted", "Predicted (model only)",
            function(df) as.data.table(person_race_regressors(as.data.frame(df), "predicted")),
            prior_fe_vars("predicted"))
if (all(c("p_black_pred_raked", "p_white_pred_raked") %in% names(Data19))) {
  add_variant("raked", "Predicted, TIDES-raked",
              function(df) set_prob(df, "p_black_pred_raked", "p_white_pred_raked"),
              prior_fe_vars("predicted"))
}
if (all(c("p_black_pred_nodraft", "p_white_pred_nodraft") %in% names(Data19))) {
  add_variant("nodraft", "Predicted, draft-free prior",
              function(df) set_prob(df, "p_black_pred_nodraft", "p_white_pred_nodraft"),
              setdiff(prior_fe_vars("predicted"), "pred_draft_bucket"))
}
if ("p_black_or_multi_pred" %in% names(Data19)) {
  add_variant("blackmulti", "Predicted, Black or multiracial",
              function(df) set_prob(df, "p_black_or_multi_pred", "p_white_pred"),
              prior_fe_vars("predicted"))
}
add_variant("preddoc", "Documented where available, else predicted",
            function(df) as.data.table(person_race_regressors(as.data.frame(df), "preddoc")),
            prior_fe_vars("preddoc"),
            "the preddoc prior conditions on career length, an outcome here, which is not controlled")
add_variant("provisional", "Wikipedia category flag (provisional)",
            function(df) as.data.table(person_race_regressors(as.data.frame(df), "provisional")),
            character())
if (HandCoverage > 0) {
  add_variant("hand", "Hand-coded",
              function(df) as.data.table(person_race_regressors(as.data.frame(df), "hand")),
              character())
}
fit_variant <- function(v, df_all, risk_flag, outcome, label) {
  d <- v$data_fn(df_all)
  d <- d[d[[risk_flag]] == 1L & !is.na(d[[outcome]]) & d$RaceKnown]
  if (nrow(d) < 100 || sum(d$Black, na.rm = TRUE) < 5) return(NULL)
  m <- fit_lpm(d, outcome, c(Rhs0, controls_for(d, c("A", "B", "C", "D", "F"))),
               prior = v$prior, key = paste("34b", label))
  list(model = m, data = d)
}
Res34b <- list()
for (key in names(Variants)) {
  v <- Variants[[key]]
  r <- fit_variant(v, Data19, "InRiskSetRetention", "RetainedNextSeason", key)
  a <- fit_variant(v, Data19, "InRiskSetBargained", "BargainedEventNext", key)
  if (is.null(r) || is.null(a)) {
    msg19("variant %s skipped (too few classified players)", key)
    next
  }
  Res34b[[key]] <- list(label = v$label, note = v$note, ret = r, acc = a)
}
stopifnot(length(Res34b) >= 2)
row34b <- function(key, which) {
  x <- Res34b[[key]][[which]]
  m <- x$model
  ct <- coeftable(m)
  c(fmt_n(ct["Black", 1], 3), paste0("(", fmt_n(ct["Black", 2], 3), ")"),
    fmt_n(mean_dep(m, x$data), 3), fmt_n(nobs(m), 0))
}
Tab34b <- do.call(rbind, lapply(names(Res34b), function(k) {
  c(Res34b[[k]]$label, row34b(k, "ret"), row34b(k, "acc"))
}))
colnames(Tab34b) <- c("Race measure", "Coef.", "(SE)", "Mean", "N", "Coef.", "(SE)", "Mean", "N")
Tab34bTex <- kbl(Tab34b, format = "latex", booktabs = TRUE, escape = FALSE, align = "lrrrrrrrr",
                 caption = paste0("Race and retention and contract access under alternative race measures",
                                  if (!is_primary_measure(measure)) paste0(" (", measure, " race measure)"),
                                  " \\label{tab:player-retention-race-measures}")) |>
  add_header_above(c(" " = 1, "Retained in $t+1$ (Table 34, col. 4)" = 4,
                     "First UFA/extension in $t+1$ (Table 35, col. 4)" = 4), escape = FALSE) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_notes(drop_empty(c(
    "This table includes the coefficient on the Black regressor from the column-(4) specifications of Tables \\ref{tab:player-retention} and \\ref{tab:player-contract-access} under each race measure, with its clustered standard error, the mean of the outcome and the number of player-seasons.",
    "Predicted measures use probabilities (regression calibration) with the prior's covariates as fixed effects; the TIDES-raked variant shifts the Black log-odds so the mean matches published TIDES player shares; the draft-free variant drops the draft-round bucket from the prior and its fixed effects; the Black-or-multiracial variant adds P(multiracial) to the Black regressor.",
    paste0("Documented-where-available (preddoc) and the Wikipedia flag are sensitivity measures whose coverage depends on fame",
           if ("preddoc" %in% names(Res34b)) paste0("; ", Res34b[["preddoc"]]$note) else "", "."),
    "The Wikipedia flag is positive-only: its comparison group mixes white and unflagged Black players, and whether a Black player is flagged rises with the length and visibility of his career, which is the outcome here; its retention coefficient is therefore not a race gap and is shown only to document the measure's behaviour.",
    NoteSelection, NoteCluster)))
save_exhibit_tex(Tab34bTex, "table-34b-player-retention-race-measures", measure)
Estimates19[["table-34b"]] <- rbindlist(lapply(names(Res34b), function(k) {
  rbind(
    tidy_terms(setNames(list(Res34b[[k]]$ret$model), paste0("retention ", Res34b[[k]]$label)), Rhs0) |>
      as.data.table() |> (\(t) { t[, `:=`(outcome_mean = mean_dep(Res34b[[k]]$ret$model, Res34b[[k]]$ret$data),
                                         n_players = n_players(Res34b[[k]]$ret$model, Res34b[[k]]$ret$data),
                                         race_variant = k)]; t })(),
    tidy_terms(setNames(list(Res34b[[k]]$acc$model), paste0("access ", Res34b[[k]]$label)), Rhs0) |>
      as.data.table() |> (\(t) { t[, `:=`(outcome_mean = mean_dep(Res34b[[k]]$acc$model, Res34b[[k]]$acc$data),
                                         n_players = n_players(Res34b[[k]]$acc$model, Res34b[[k]]$acc$data),
                                         race_variant = k)]; t })())
}), use.names = TRUE)
Estimates19[["table-34b"]][, `:=`(table = "table-34b", mde_80 = 2.8 * std_error)]

# ---------------------------------------------------------------------------
# Further sensitivities (tidy file only): logit, era, 2016 leak
# ---------------------------------------------------------------------------

msg19("sensitivities at %ds", round(as.numeric(difftime(Sys.time(), T0Script19, units = "secs"))))
# Logit with the column-(4) controls, summarised as the score plug-in average
# marginal effect of the Black regressor: beta x mean(p(1-p)) over the
# estimation sample, with the mean treated as fixed. Under the predicted
# measures P(Black) enters the index as a score, so this is a functional-form
# sensitivity of the linear probability model, not an identified average
# marginal effect of latent race
logit_ame <- function(df, y, label) {
  rhs <- c(Rhs0, controls_for(df, c("A", "B", "C", "D", "F")), prior_cols(df))
  m <- tryCatch(feglm(make_fml(y, rhs, FE19), data = df, family = binomial(), vcov = ~gsis_id,
                      notes = FALSE), error = function(e) e)
  if (inherits(m, "error")) stop(sprintf("19: logit %s failed: %s", label, conditionMessage(m)), call. = FALSE)
  p <- predict(m, type = "response")
  scale <- mean(p * (1 - p))
  b <- coef(m)[["Black"]]
  s <- se(m)[["Black"]]
  data.table(table = paste0(label, "-sensitivity"), model = "logit, score plug-in AME (functional-form sensitivity)", term = "Black",
             estimate = b * scale, std_error = s * scale, p_value = pvalue(m)[["Black"]],
             ci_low = (b - qnorm(0.975) * s) * scale, ci_high = (b + qnorm(0.975) * s) * scale,
             nobs = nobs(m), dep_var = y, outcome_mean = mean(df[[y]][obs(m)]),
             n_players = uniqueN(df$gsis_id[obs(m)]), mde_80 = 2.8 * s * scale)
}
sens_lpm <- function(df, y, label, model) {
  m <- fit_lpm(df, y, c(Rhs0, controls_for(df, c("A", "B", "C", "D", "F"))), key = paste(label, model))
  t <- as.data.table(tidy_terms(setNames(list(m), model), Rhs0))
  t[, `:=`(table = paste0(label, "-sensitivity"), outcome_mean = mean_dep(m, df),
           n_players = n_players(m, df), mde_80 = 2.8 * std_error)]
  t
}
Sens <- rbindlist(list(
  logit_ame(RetData, "RetainedNextSeason", "table-34"),
  logit_ame(AccData, "BargainedEventNext", "table-35"),
  sens_lpm(RetData[season >= 2017L], "RetainedNextSeason", "table-34", "seasons 2017-2024"),
  sens_lpm(RetData[season != 2016L], "RetainedNextSeason", "table-34", "drop t = 2016 as well"),
  sens_lpm(RetData[season <= 2014L], "RetainedNextSeason", "table-34", "seasons 2002-2014"),
  sens_lpm(Data19[InRiskSetRetentionIncl2015 == 1L & !is.na(RetainedNextSeason) & RaceKnown == TRUE],
           "RetainedNextSeason", "table-34", "include t = 2015, ambiguous t+1 listings unknown"),
  sens_lpm(Data19[InRiskSetRetentionIncl2015 == 1L & !is.na(RetainedNextSeasonAmbiguousAsExit) & RaceKnown == TRUE],
           "RetainedNextSeasonAmbiguousAsExit", "table-34", "include t = 2015, ambiguous t+1 listings as exit"),
  sens_lpm(RetData[Experience <= 3L], "RetainedNextSeason", "table-34", "experience 0-3"),
  sens_lpm(RetData[Experience >= 4L], "RetainedNextSeason", "table-34", "experience 4-15"),
  sens_lpm(AccData[Undrafted == 0L], "BargainedEventNext", "table-35", "drafted players"),
  sens_lpm(AccData[Undrafted == 1L], "BargainedEventNext", "table-35", "undrafted players"),
  sens_lpm(AccData[season >= 2017L], "BargainedEventNext", "table-35", "seasons 2017-2024"),
  sens_lpm(AccData[HasAnyContractRow == 1L], "BargainedEventNext", "table-35", "players with an OTC contract history"),
  sens_lpm(AccData[EntryContractObserved == 1L], "BargainedEventNext", "table-35", "players with an observed entry contract")),
  use.names = TRUE, fill = TRUE)
Estimates19[["sensitivity"]] <- Sens

# ---------------------------------------------------------------------------
# Figure: retention and first-contract rates by experience and race
# ---------------------------------------------------------------------------

# Means weighted by the race regressors (group means under indicators;
# probability-weighted means under the predicted measures)
wmean_by <- function(df, y, w, exp_max = 10L) {
  d <- df[Experience <= exp_max & !is.na(get(y))]
  d[, .(Rate = sum(get(w) * get(y)) / sum(get(w)), N = sum(get(w))), by = Experience]
}
FigData <- rbindlist(list(
  wmean_by(RetData, "RetainedNextSeason", "Black")[, `:=`(Group = BlackLab, Outcome = "Under contract in t+1")],
  wmean_by(RetData, "RetainedNextSeason", "PWhite")[, `:=`(Group = if (is_pred(measure)) "P(white)" else "White", Outcome = "Under contract in t+1")],
  wmean_by(AccData, "BargainedEventNext", "Black")[, `:=`(Group = BlackLab, Outcome = "First UFA/extension in t+1")],
  wmean_by(AccData, "BargainedEventNext", "PWhite")[, `:=`(Group = if (is_pred(measure)) "P(white)" else "White", Outcome = "First UFA/extension in t+1")]))
FigData[, Outcome := factor(Outcome, levels = c("Under contract in t+1", "First UFA/extension in t+1"))]
FigRet <- ggplot(FigData, aes(x = Experience, y = Rate, colour = Group, shape = Group)) +
  geom_line() + geom_point(size = 2) +
  facet_wrap(~Outcome, scales = "free_y") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "NFL experience in season t (seasons since rookie season)", y = "Share",
       colour = NULL, shape = NULL,
       caption = paste0("Means over the Table 34 and Table 35 risk sets, weighted by the race regressors (",
                        if (is_pred(measure)) "predicted probabilities" else "group indicators",
                        "); no controls.")) +
  theme_customs()
save_exhibit_figure(FigRet, "figure-player-retention-by-experience", measure, width = 8, height = 4.5)
Estimates19[["figure"]] <- FigData[, .(table = "figure-player-retention-by-experience",
                                       model = paste(Outcome, Group, sep = " | "), term = "Experience",
                                       experience = Experience, estimate = Rate, nobs = N)]

# ---------------------------------------------------------------------------
# Tidy coefficient file, sample flow and runtime
# ---------------------------------------------------------------------------

Est <- rbindlist(Estimates19, use.names = TRUE, fill = TRUE)
# Provenance and interpretation columns (one manifest, self-describing)
Est[, estimator := fifelse(grepl("^logit", model), "fixest feglm logit; beta x mean(p(1-p)) with P(Black) plugged into the index (functional-form sensitivity, not an identified latent-race AME)",
                   fifelse(table == "figure-player-retention-by-experience", "weighted mean", "fixest feols linear probability model"))]
Est[, estimand := fifelse(term == "Black",
                          sprintf("%s coefficient: Black-white difference in P(outcome) given controls (regression calibration under predicted measures); conditional association, not a causal effect", BlackLab),
                   fifelse(term == "OtherRace", sprintf("%s coefficient relative to white", OtherLab),
                   fifelse(term == "PracticeSquadOnly", "practice-squad-only season in t relative to under contract",
                           "share of the risk set with the outcome, by experience")))]
Est[, scale := fifelse(table == "figure-player-retention-by-experience", "share (0-1)", "probability (0-1); x100 = percentage points")]
# Models that carry their own fixed-effect label (Table 34c) keep it; the
# blanket label applies to the rest
Est[is.na(fixed_effects), fixed_effects := fifelse(table == "figure-player-retention-by-experience", "none", FE19Label)]
stopifnot(all(grepl("RetentionEmployer", Est$fixed_effects[Est$table == "table-34c" & Est$model == "(3)"])))
Est[, inference := fifelse(table == "figure-player-retention-by-experience", "none",
                           "cluster-robust by player (gsis_id); 95% CI; mde_80 = 2.8 x SE")]
Est[, outcome_timing := fifelse(grepl("^table-34|figure", table), "season t+1 roster, right-censored at 2025",
                                "contract year t+1, 2026 observed through the OTC scrape")]
Est[, limitations := paste("Risk sets condition on employment in season t (selected survival); roster weeks are not paid weeks;",
                           "under-contract status from weekly rosters (2016 weeks 1-2 unverified listings dropped);",
                           "access risk set limited to rookie cohorts 2011+ and seasons 2013+ (OTC contract-history coverage);",
                           if (is_pred(measure)) "race is predicted, not observed (see race_measure_note)" else "race measure as labelled")]
Est[table == "table-34c", limitations := paste(limitations,
  "employer = last verified under-contract franchise of season t (an end point of season-t roster decisions), tied final weeks dropped;",
  "within-employer comparison is descriptive among selected survivors, not an employer treatment effect", sep = " ")]
setcolorder(Est, intersect(c("table", "model", "term", "estimate", "std_error", "p_value", "ci_low",
                             "ci_high", "mde_80", "nobs", "n_players", "outcome_mean", "dep_var",
                             "n_input", "n_risk_set", "n_known_race", "n_estimated", "race_variant",
                             "experience", "estimator", "estimand", "scale", "fixed_effects",
                             "inference", "outcome_timing", "limitations"), names(Est)))
save_estimates(Est, "19-player-retention", measure)
FlowOut <- rbindlist(Flow, use.names = TRUE)
FlowOut[, `:=`(race_measure = measure,
               n_dropped_unknown_race = n_outcome_observed - n_known_race,
               note = "n_input = all player-seasons of 18; n_risk_set = rows with the risk flag (and the column's extra restriction); n_outcome_observed = of those, outcome not censored; n_known_race = estimation rows")]
save_estimates(FlowOut, "19-player-retention-sample-flow", measure)
print(FlowOut)
print(Est[table %in% c("table-34", "table-34c", "table-35") & term == "Black",
          .(table, model, estimate = round(estimate, 4), std_error = round(std_error, 4),
            p_value = round(p_value, 3), outcome_mean = round(outcome_mean, 3), nobs)])
msg19("done at %ds", round(as.numeric(difftime(Sys.time(), T0Script19, units = "secs"))))

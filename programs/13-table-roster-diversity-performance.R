# ============================================================================
# 13-table-roster-diversity-performance.R
# Estimates the roster-diversity specifications of notes/analysis-plan.md,
# section 2 ("Specifications"): how do teams with more racially diverse
# rosters perform? Equation (2), for franchise f in season t:
#   Y_ft = beta ShareBlackRoster_ft + X_ft pi + alpha_f + gamma_t + e_ft
# and the within-season game-level design (equation (3)), for franchise f in
# game g of season t:
#   AtsMargin_fgt = beta ShareBlackActiveRoster_fg + alpha_{f,t} + e_fgt
# where AtsMargin = margin - team_spread_line (the market-adjusted margin).
# Exhibits (a -<measure> suffix, output/ only, under a non-primary race
# measure; see save_exhibit_tex()):
#   - table-14-roster-diversity-sumstats + figure-roster-share-black-by-season
#   - table-15-roster-diversity-team-season: equation (2), WinPct,
#     snap-weighted share, 2014-2025, columns (1)-(9)
#   - table-15b-roster-diversity-team-season-headcount: the same columns for
#     the headcount share, 2002-2025 (the plan runs both measures)
#   - table-16-roster-diversity-outcomes (snap-weighted) and
#     table-16b-roster-diversity-outcomes-headcount: column-(4) specification
#     for the other outcomes
#   - table-17-roster-diversity-team-game: within-season game-level design
#     with a fixed-effects sensitivity panel and the symmetry check
#   - table-18-roster-diversity-placebo: one-season leads, the
#     reverse-causality test (composition on past performance) and its
#     decomposition net of the mean prior (estimated before Table 15, whose
#     notes report the results)
#   - table-18b-roster-diversity-robustness: predetermined (opening-day)
#     treatments, contemporaneous (possible bad) controls including the
#     annual cap share, no lagged outcome, trimming the largest share of each
#     season, and the game-script channel of the snap weights (unit-balanced
#     share, defensive snap share control, offense and defense shares jointly)
#   - table-28-roster-diversity-race-measures: the headline specifications
#     (column (4) of Tables 15 and 15b and column (1) of Table 17, without and
#     with the calibration/composition controls) under every available race
#     measure and under three variants of the predicted measure (prior
#     without draft round, Black or multiracial, raked to TIDES), with the
#     Berkson-implied SD of the true share
# Deviations from the plan (documented in the table notes):
#   - Controls are predetermined: opening-day (first REG game) roster quality,
#     starting QB and head coach, and the prior season's cap share
#     (L1TeamCapShare, 09). The plan's in-season (snap- or roster-week-
#     weighted) quality, season QB (most starts) and season HC (most games)
#     respond to injuries, benching, tanking and firings that also drive
#     results, so they enter only in a robustness column; so does the annual
#     cap share of the season (TeamCapShare), which in-season signings,
#     releases and restructures change, so it is not fixed by opening day.
#   - The snap-weighted treatment is measured during the season (playing time
#     responds to injuries, benching and game script), so its coefficients
#     are descriptive conditional associations; the opening-day roster
#     treatments of Table 18b are the ones fixed before the season's results.
#   - The snap-weighted estimation panel is 2014-2025: snap counts start in
#     2013 and the prior season's cap share is first observed in 2014 (the
#     2013 cap table is the first kept). Table 14 describes 2013-2025.
#   - Under the predicted measures the headline column (4), columns (6)-(9)
#     and the other tables include the calibration controls: the roster
#     group's mean prior (MeanPriorBlack<G>), the weighted shares of the
#     levels of the prior's non-position covariates (Share<CovVar><G>, 09) and
#     the priors of the coaches, head coach and QB whose race enters as a
#     control (regression calibration conditions on the prior's covariates);
#     column (5) drops them. Under the other measures column (5) adds the mean
#     prior and covariate shares as composition controls.
#   - Column (6) of the plan (two-group Blau index 2s(1-s)) is nested in the
#     share + share^2 column (8); column (9) adds a three-group (Black / white
#     / other) Blau index, the diversity measure proper. Column (7) here
#     controls for the position-mix expected share (the plan's "expected
#     shares").
#   - Game level: the market-adjusted margin (unit slope on the spread) with
#     franchise x season FE is the main specification; the plan's margin +
#     spread with opponent x season FE is a robustness column, because the
#     spread's within team-season slope is distorted (it updates on earlier
#     results of the same team-season).
#   - Staff composition is used only where the full staff is observed
#     (FullStaffObserved, 2007+), with a missing indicator otherwise; the
#     predetermined control is the Black share of the coaches listed in the
#     preseason snapshot (ShareBlack<M>CoachesPre, 02), the share over all
#     snapshots enters only the contemporaneous-controls column.
# Inference: standard errors clustered by franchise (32 clusters); wild
# cluster bootstrap-t p-values (Webb weights, NFL_WCB_REPS replications,
# default 9,999) for the diversity coefficients.
# Race: one measure for Tables 14-18b, from choose_race_measure() on the
# hand-code coverage of the roster treatments (the lower of the mean
# CodedShareRoster, 2002-2025, and CodedShareSnapW, 2013-2025): predicted
# race (notes/race-prediction-design.md) while hand codes are absent.
# Under the predicted measures a team share is the mean member P(Black), the
# expected share. Staff and QB race controls are zero-filled with missing
# indicators, so uncoded persons or unobserved staff never drop team-seasons.
# Inputs: analysis/analysis_team_season.parquet and
#   analysis/analysis_team_game.parquet (11).
# Outputs: output/tables/table-14 ... table-18b, table-28 (.tex; primary
#   measure also in my_paper/tables), output/figures/
#   figure-roster-share-black-by-season (.pdf/.png), output/estimates/
#   13-roster-diversity[-<measure>].csv and
#   13-roster-diversity-sumstats[-<measure>].csv.
# Date: 2026-10-02 (predicted race, Table 28, calibration controls and
# variants added the same day; lagged cap share, 2014+ snap panel and
# Black-alone labels 2026-10-03)
# ============================================================================

T0Script13 <- Sys.time()

# Wild cluster bootstrap replications (override with NFL_WCB_REPS for tests)
WcbReps <- as.integer(Sys.getenv("NFL_WCB_REPS", "9999"))

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

TeamSeasonAll <- read_parquet(file.path(analysis, "analysis_team_season.parquet"))
TeamGameAll <- read_parquet(file.path(analysis, "analysis_team_game.parquet"))

# Keys must be unique
stopifnot(!anyDuplicated(TeamSeasonAll[c("franchise_id", "season")]),
          !anyDuplicated(TeamGameAll[c("franchise_id", "game_id")]))

# ---------------------------------------------------------------------------
# Race measure (one for Tables 14-18b; Table 28 loops over all measures)
# ---------------------------------------------------------------------------

# Coverage = the lower of the mean hand-coded weight share of the headcount
# roster (2002-2025) and of the snap-weighted roster (2013-2025); without hand
# codes the primary measure is predicted race
HandCoverage13 <- min(
  TeamSeasonAll |> filter(season >= 2002) |> pull(CodedShareRoster) |> mean(na.rm = TRUE),
  TeamSeasonAll |> filter(season >= 2013) |> pull(CodedShareSnapW) |> mean(na.rm = TRUE))
measure <- choose_race_measure(HandCoverage13, "13 roster diversity")

# Every generic column the script uses must exist after the mapping
NeedSeason <- c("ShareBlackRoster", "ShareBlackSnapW", "ExpectedShareBlackRoster",
                "ResidualShareBlackRoster", "ExpectedShareBlackSnapW",
                "ResidualShareBlackSnapW", "ShareBlackOffenseSnapW", "ShareBlackDefenseSnapW",
                "ShareBlackOffense", "ShareBlackDefense", "ShareBlackWeek1",
                "ShareBlackWeek1PriorSnapW", "QBBlack", "QBWeek1Black", "ShareBlackCoaches",
                "ShareBlackCoachesPre", "HCBlack", "HCWeek1Black", "F1ShareBlackRoster", "F1ShareBlackSnapW",
                "L1ShareBlackRoster", "L1ShareBlackSnapW")
stopifnot(c("TeamCapShare", "L1TeamCapShare") %in% names(TeamSeasonAll))

# Season bounds. Snap counts start in 2013 (descriptive period of Table 14
# and the figure); the snap-weighted ESTIMATION panel starts in 2014, the
# first season with the prior season's cap share (L1TeamCapShare, 09: the
# 2013 cap table is the first kept). The bound is set here, rather than left
# to listwise deletion by the controls, so that every snap-weighted column,
# including those without controls and the contemporaneous-controls column
# of Table 18b, is estimated on the same franchise-seasons.
FirstSnapSeason13 <- 2013L
FirstSnapEstSeason13 <- 2014L
LastSeason13 <- 2025L
SnapSeasons13 <- paste0(FirstSnapEstSeason13, "-", LastSeason13)
L1CapCoverage13 <- range(TeamSeasonAll$season[!is.na(TeamSeasonAll$L1TeamCapShare)])
if (L1CapCoverage13[1] != FirstSnapEstSeason13) {
  stop("13: L1TeamCapShare is first observed in ", L1CapCoverage13[1], ", not ",
       FirstSnapEstSeason13, "; update FirstSnapEstSeason13")
}
NeedGame <- c("ShareBlackActiveRoster", "ResidualShareBlackActiveRoster",
              "OppShareBlackActiveRoster", "GameQBBlack", "OppGameQBBlack")

# Three-group (Black / white / other) Blau index from Black and other shares
blau_three_group <- function(b, o) 1 - b^2 - o^2 - (1 - b - o)^2

# Predicted measures (team shares are expected shares; regressions condition
# on the prior's covariates, notes/race-prediction-design.md, section 4)
PredMeasures <- c("predicted", "preddoc")
is_pred <- function(m) m %in% PredMeasures

# Levels of the primary player prior's non-position covariates whose weighted
# team shares 09 builds (Share<CovVar><G>; omitted levels: undrafted, power
# conference, rookie era <= 2005, no county likelihood)
CovVars13 <- c("DraftR12", "DraftR34", "DraftR5", "CollegeHBCU", "CollegeFCS",
               "CollegeOtherFBS", "CollegeUnknown", "Era0610", "Era1115", "Era1620",
               "Era21", "CountyAvail")
cov_terms <- function(g, drop_draft = FALSE) {
  paste0("Share", if (drop_draft) CovVars13[!str_starts(CovVars13, "Draft")] else CovVars13, g)
}

# Mean-prior source column of group g under measure m: the preddoc prior
# (MeanPriorBlackPredDoc<g>, 09) where built, else the model-only prior
prior_source <- function(df, g, m, prefix = "") {
  doc <- glue("{prefix}MeanPriorBlackPredDoc{g}")
  if (m == "preddoc" && doc %in% names(df)) doc else glue("{prefix}MeanPriorBlackPred{g}")
}

# Treatment variants of the predicted measure for Table 28 (09 columns):
# nodraft = prior without draft round (shares that do not proxy draft
# capital); multi = P(Black or multiracial); raked = raked to TIDES margins
Variants13 <- c(nodraft = "NoDraft", multi = "OrMulti", raked = "Raked")

# Estimation samples under race measure m (and, under "predicted", treatment
# variant `variant`): list(snap = snap-weighted team-seasons 2014-2025, head =
# headcount team-seasons 2002-2025, game = REG team-games 2002-2025), with
# generic race names (ShareBlackRoster, QBBlack, HCBlack, ...), the
# other-race shares, the mean prior of each roster group (MeanPriorBlack<G>)
# and of the staff and QB race controls (MeanPriorBlackCoachesPre,
# HCWeek1PriorBlack, QBWeek1PriorBlack, ...), Blau indices, squared shares and
# zero-filled staff and QB race controls
build_samples13 <- function(m, variant = "none") {
  season_df <- apply_race_measure(TeamSeasonAll, m)
  game_df <- apply_race_measure(TeamGameAll, m)
  stopifnot(all(NeedSeason %in% names(season_df)), all(NeedGame %in% names(game_df)))
  tag <- RaceMeasureTags[[m]]
  # Other-race (neither Black nor white) shares for the three-group Blau
  # index: apply_race_measure() maps only *Black* names, so the mapping (with
  # the same coverage gate under the hand measure) is done here
  for (g in c("Roster", "SnapW")) {
    other <- season_df[[glue("ShareOther{tag}{g}")]]
    season_df[[glue("ShareOther{g}")]] <- if (m == "hand") {
      if_else(coalesce(season_df[[glue("CodedShare{g}")]], 0) >= MinHandCoverage, other, NA_real_)
    } else other
  }
  # Team mean of the race prior for each roster group, its lead and lag
  # (preddoc prior under preddoc where built, else the model-only prior):
  # under the predicted measures a calibration control, otherwise a summary
  # of the roster's predetermined position, draft and college mix
  for (g in c("Roster", "SnapW", "Week1", "Week1PriorSnapW", "Offense", "Defense",
              "OffenseSnapW", "DefenseSnapW")) {
    season_df[[glue("MeanPriorBlack{g}")]] <- season_df[[prior_source(season_df, g, m)]]
  }
  for (g in c("Roster", "SnapW")) {
    for (p in c("F1", "L1")) {
      season_df[[glue("{p}MeanPriorBlack{g}")]] <- season_df[[prior_source(season_df, g, m, p)]]
    }
  }
  game_df$MeanPriorBlackActiveRoster <- game_df[[prior_source(game_df, "ActiveRoster", m)]]
  game_df$OppMeanPriorBlackActiveRoster <- game_df[[prior_source(game_df, "ActiveRoster", m, "Opp")]]
  # Priors of the persons whose predicted race enters as a control (model-only
  # priors: no preddoc prior is built for staff or QBs)
  season_df <- season_df |>
    mutate(MeanPriorBlackCoachesPre = MeanPriorBlackPredCoachesPre,
           MeanPriorBlackCoaches = MeanPriorBlackPredCoaches,
           HCWeek1PriorBlack = HCWeek1PriorBlackPred, HCPriorBlack = HCPriorBlackPred,
           QBWeek1PriorBlack = QBWeek1PriorBlackPred, QBPriorBlack = QBPriorBlackPred)
  game_df <- game_df |>
    mutate(GameQBPriorBlack = GameQBPriorBlackPred, OppGameQBPriorBlack = OppGameQBPriorBlackPred)
  # Treatment variant (predicted measure only): replace the roster shares
  # (and, for nodraft, the mean prior) by the variant's
  if (variant != "none") {
    stopifnot(m == "predicted", variant %in% names(Variants13))
    vt <- Variants13[[variant]]
    for (g in c("Roster", "SnapW", "Week1", "Week1PriorSnapW")) {
      season_df[[glue("ShareBlack{g}")]] <- season_df[[glue("ShareBlack{vt}{g}")]]
      if (variant == "nodraft") {
        season_df[[glue("MeanPriorBlack{g}")]] <- season_df[[glue("MeanPriorBlackNoDraft{g}")]]
      }
    }
    game_df$ShareBlackActiveRoster <- game_df[[glue("ShareBlack{vt}ActiveRoster")]]
    if (variant == "nodraft") {
      game_df$MeanPriorBlackActiveRoster <- game_df$MeanPriorBlackNoDraftActiveRoster
    }
  }
  # Blau indices, squared shares, unit-balanced snap share, numeric
  # indicators. The coaches' shares are used only where the full staff is
  # observed (2007+; 2002-2006 staff boxes are partial season-article boxes),
  # so they are set to NA before.
  season_df <- season_df |>
    mutate(BlauRoster = blau_two_group(ShareBlackRoster),
           BlauSnapW = blau_two_group(ShareBlackSnapW),
           Blau3Roster = blau_three_group(ShareBlackRoster, ShareOtherRoster),
           Blau3SnapW = blau_three_group(ShareBlackSnapW, ShareOtherSnapW),
           ShareBlackRosterSq = ShareBlackRoster^2,
           ShareBlackSnapWSq = ShareBlackSnapW^2,
           ShareBlackBalancedSnapW = 0.5 * (ShareBlackOffenseSnapW + ShareBlackDefenseSnapW),
           Playoffs = as.numeric(Playoffs),
           across(c(ShareBlackCoaches, ShareBlackCoachesPre, MeanPriorBlackCoaches,
                    MeanPriorBlackCoachesPre), \(x) if_else(FullStaffObserved, x, NA_real_)),
           across(c(HCBlack, HCWeek1Black, QBBlack, QBWeek1Black), as.numeric))
  # Staff and QB race controls: zero-filled with <var>Miss indicators, so
  # that uncoded persons (hand, provisional) or unobserved staff never drop
  # team-seasons; each prior is zero-filled where its race control is
  # missing (it shares the race control's missing indicator)
  season_df <- fill_missing(season_df, c("ShareBlackCoaches", "ShareBlackCoachesPre", "HCBlack",
                                         "HCWeek1Black", "QBBlack", "QBWeek1Black"))
  PriorPairs <- c(MeanPriorBlackCoachesPre = "ShareBlackCoachesPre",
                  MeanPriorBlackCoaches = "ShareBlackCoaches", HCWeek1PriorBlack = "HCWeek1Black",
                  HCPriorBlack = "HCBlack", QBWeek1PriorBlack = "QBWeek1Black",
                  QBPriorBlack = "QBBlack")
  for (pv in names(PriorPairs)) {
    miss <- season_df[[paste0(PriorPairs[[pv]], "Miss")]] == 1
    season_df[[pv]] <- if_else(miss, 0, coalesce(season_df[[pv]], 0))
  }
  # Game-level sample: outcome net of the market's pre-game win probability;
  # team-season identifier; game starting-QB race (and prior) zero-filled
  # likewise
  game_df <- game_df |>
    filter(game_type == "REG", season >= 2002, season <= 2025) |>
    mutate(GameQBBlack = as.numeric(GameQBBlack), OppGameQBBlack = as.numeric(OppGameQBBlack),
           WinOverMarket = WinOrHalfTie - implied_win_prob,
           TeamSeasonId = paste(franchise_id, season, sep = "_")) |>
    fill_missing(c("GameQBBlack", "OppGameQBBlack")) |>
    mutate(GameQBPriorBlack = if_else(GameQBBlackMiss == 1, 0, coalesce(GameQBPriorBlack, 0)),
           OppGameQBPriorBlack = if_else(OppGameQBBlackMiss == 1, 0,
                                         coalesce(OppGameQBPriorBlack, 0)))
  list(all = season_df,
       snap = filter(season_df, season >= FirstSnapEstSeason13, season <= LastSeason13),
       head = filter(season_df, season >= 2002, season <= LastSeason13),
       game = game_df)
}

Samples13 <- build_samples13(measure)
TeamSeason <- Samples13$all
SnapData <- Samples13$snap
HeadData <- Samples13$head
GameData <- Samples13$game
message(glue("13: {nrow(SnapData)} snap-weighted team-seasons ({SnapSeasons13}; ",
             "{sum(TeamSeason$season >= FirstSnapSeason13)} with snap counts from ",
             "{FirstSnapSeason13}, {sum(is.na(SnapData$L1TeamCapShare))} without the prior-season ",
             "cap share), {nrow(HeadData)} headcount team-seasons, {nrow(GameData)} REG team-games"))

# Label of the share in exhibits under the chosen measure (a predicted share
# is an expected share; the notes say so)
share_word <- function(m) if (m == "provisional") "flagged Black" else "Black"
ShareWord <- share_word(measure)
IsPredMeasure <- is_pred(measure)
# Effect rows refer to a one-SD change in the expected share under the
# predicted measures (its SD is smaller than that of the true share)
EffectWord <- if (IsPredMeasure) " in expected share" else ""


# Within-franchise deviation of x (x minus its franchise mean)
within_dev <- function(x, g) x - ave(x, g, FUN = \(v) mean(v, na.rm = TRUE))

# Within-franchise SD of x: SD of the deviation from the franchise mean
within_sd <- function(x, g) sd(within_dev(x, g), na.rm = TRUE)

# Number formatting for hand-built tables ("" for NA)
fmt_num <- function(x, digits = 3) {
  x <- if_else(abs(x) < 0.5 * 10^-digits, 0, x)
  if_else(is.na(x), "", trimws(formatC(x, format = "f", digits = digits, big.mark = ",")))
}

# Team-level calibration check of the predicted measure among documented
# players (validation only; documentation is selected on fame): the
# documented Black share of a team-season's documented players regressed on
# the same players' mean model-only P(Black), with season fixed effects and
# with franchise and season fixed effects (the identifying variation of the
# main tables). A slope of one is consistent with team-level calibration;
# below (above) one, expected shares overstate (understate) between-team
# differences among these players. Clustered by franchise.
CalibCheck13 <- map_dfr(c(Roster = "Roster", SnapW = "SnapW"), \(g) {
  d <- TeamSeason |>
    filter(season >= 2002, .data[[glue("DocShare{g}")]] > 0,
           !is.na(.data[[glue("ShareBlackDoc{g}")]]), !is.na(.data[[glue("DocMeanPBlack{g}")]]))
  map_dfr(c("season", "franchise_id + season"), \(fe) {
    mod <- feols(as.formula(glue("ShareBlackDoc{g} ~ DocMeanPBlack{g} | {fe}")), data = d,
                 vcov = ~franchise_id, notes = FALSE)
    ct <- coeftable(mod)
    tibble(group = g, fe = fe, estimate = ct[1, 1], std_error = ct[1, 2], nobs = nobs(mod),
           mean_doc_share = mean(d[[glue("DocShare{g}")]]),
           mean_doc_black = mean(d[[glue("ShareBlackDoc{g}")]]))
  })
})
print(CalibCheck13)
CalibTwoWay <- filter(CalibCheck13, fe == "franchise_id + season")
NoteCalibCheck <- paste(
  "A team-level calibration check among documented players (validation only): regressing a",
  "franchise-season's documented Black share of its documented players on the same players'",
  "mean predicted probability, with franchise and season fixed effects, gives a slope of",
  glue("{fmt_num(CalibTwoWay$estimate[CalibTwoWay$group == 'SnapW'], 2)} ",
       "(SE {fmt_num(CalibTwoWay$std_error[CalibTwoWay$group == 'SnapW'], 2)}) for snap weights and "),
  glue("{fmt_num(CalibTwoWay$estimate[CalibTwoWay$group == 'Roster'], 2)} ",
       "(SE {fmt_num(CalibTwoWay$std_error[CalibTwoWay$group == 'Roster'], 2)}) for headcount weights,"),
  "against one under calibration. Documented players are a selected, mostly Black and famous",
  glue("subset (about {fmt_num(100 * mean(CalibTwoWay$mean_doc_share), 0)} percent of the weight),"),
  "so the check does not establish the team-level calibration of the full roster in either",
  "direction.")

# Design-specific caveat on the race measure for the team tables, added to
# the shared race_measure_note(measure, "team")
note_measure_team <- function(m) switch(m,
  hand = character(),
  predicted = paste(
    "Under regression calibration the coefficient on an expected roster share (the mean over",
    "players of the predicted probability of being non-Hispanic Black) equals the effect of the",
    "true share only if the probabilities are calibrated at the team level given the regression's",
    "controls; this requires that players' names and hometown counties be unrelated to team",
    "performance given race and the prior's covariates, and the calibration controls (the team",
    "mean of the prior and the weighted shares of its draft-round, college-type, rookie-era and",
    "hometown-county levels, plus the priors of the coaches, head coach and quarterback whose",
    "predicted race enters as a control) condition on those covariates. Shrinkage toward the",
    "prior by itself causes no attenuation (the error of an expected share is of the Berkson",
    "type), but individual-level calibration does not imply team-level calibration: if the true",
    "share rises more (less) than one-for-one with the expected share across teams, the",
    "coefficient overstates (understates) the effect, so the sign of any bias is not known. A",
    "league-wide level error, such as the model's shortfall against the TIDES league share, is",
    "absorbed by the season fixed effects.", NoteCalibCheck,
    "Effect rows refer to a one-SD change in the expected share, whose SD is smaller than that of",
    "the true share."),
  preddoc = paste(
    "For these team-level estimates documentation is more complete for famous players, and",
    "successful franchise-seasons may be documented more completely, so the measurement error",
    "can be correlated with the outcome; the estimates are a sensitivity check only."),
  provisional = paste(
    "For these team-level estimates the provisional measure's error is not classical: the",
    "flagged share also varies with how completely Wikipedia editors categorize a",
    "franchise-season's players, which may rise with team success. The measurement error can",
    "therefore be correlated with the outcome, and the bias is of unknown sign, not",
    "attenuation. The estimates test the pipeline; they are not estimates of the effect of",
    "roster composition."))

# What "Black" means under the chosen measure, for the exhibits whose notes
# do not otherwise say so: the primary model-only predicted measure is
# non-Hispanic Black alone; the documented, provisional and hand-coded
# measures are Black alone or in combination (black_any)
NoteBlackDef <- switch(measure,
  predicted = paste(
    "Under the model-only predicted measure a share Black is the expected share of players who",
    "are non-Hispanic Black alone (multiracial and Hispanic Black players count as non-Black);",
    "the documented, provisional and hand-coded sensitivity measures count Black alone or in",
    "combination."),
  preddoc = paste(
    "Under the documented variant, documented players count as Black alone or in combination",
    "and undocumented players carry the model-only probability of being non-Hispanic Black",
    "alone, so the share mixes the two definitions; the primary model-only measure is",
    "non-Hispanic Black alone."),
  provisional = paste(
    "The provisional flag (hand code where available, else the Wikipedia category) counts Black",
    "alone or in combination; the primary model-only predicted measure is non-Hispanic Black",
    "alone."),
  hand = paste(
    "Hand-coded Black is Black alone or in combination (black_any, notes/race-coding-protocol.md);",
    "the model-only predicted measure is non-Hispanic Black alone."))
NoteMeasureTeam <- paste(c(NoteBlackDef, note_measure_team(measure)), collapse = " ")

# ---------------------------------------------------------------------------
# Table 14: summary statistics of roster diversity, by period
# ---------------------------------------------------------------------------

SumVars <- tribble(
  ~var,                        ~label,
  "ShareBlackRoster",          "Share Black, headcount",
  "ExpectedShareBlackRoster",  "Position-mix expected share Black, headcount",
  "ResidualShareBlackRoster",  "Residual share Black, headcount",
  "MeanPriorBlackRoster",      "Mean prior P(Black), headcount",
  "ShareOtherRoster",          "Share other race, headcount",
  "Blau3Roster",               "Three-group Blau index, headcount",
  "ShareBlackWeek1",           "Share Black, opening-day active roster",
  "ShareBlackSnapW",           "Share Black, snap-weighted",
  "ExpectedShareBlackSnapW",   "Position-mix expected share Black, snap-weighted",
  "ResidualShareBlackSnapW",   "Residual share Black, snap-weighted",
  "MeanPriorBlackSnapW",       "Mean prior P(Black), snap-weighted",
  "ShareOtherSnapW",           "Share other race, snap-weighted",
  "Blau3SnapW",                "Three-group Blau index, snap-weighted",
  "ShareBlackWeek1PriorSnapW", "Share Black, opening day, prior-snap-weighted",
  "QBWeek1Black",              if (IsPredMeasure) "Opening-day starting QB P(Black)" else
                                 "Opening-day starting QB Black"
)
Periods <- list("2002-2012" = c(2002, 2012), "2013-2025" = c(2013, 2025))

# Mean, SD, within-franchise SD and N by period (QB indicator: coded rows only)
SumStats <- imap_dfr(Periods, \(yrs, per) {
  d <- filter(TeamSeason, season >= yrs[1], season <= yrs[2])
  pmap_dfr(SumVars, \(var, label) {
    x <- d[[var]]
    if (var == "QBWeek1Black") x[d$QBWeek1BlackMiss == 1] <- NA
    ok <- !is.na(x)
    tibble(period = per, var = var, label = label,
           mean = if (any(ok)) mean(x[ok]) else NA_real_,
           sd = if (sum(ok) > 1) sd(x[ok]) else NA_real_,
           within_sd = if (sum(ok) > 1) within_sd(x[ok], d$franchise_id[ok]) else NA_real_,
           n = sum(ok))
  })
})

# Wide layout: one row per variable, four columns per period
SumWide <- SumStats |>
  mutate(across(c(mean, sd, within_sd), \(x) fmt_num(x, 3)),
         n = if_else(n == 0, "", formatC(n, format = "d", big.mark = ","))) |>
  pivot_wider(id_cols = c(var, label), names_from = period,
              values_from = c(mean, sd, within_sd, n), names_glue = "{period}_{.value}")
SumTable14 <- SumWide |>
  transmute(Variable = if_else(str_detect(label, "P\\(Black\\)"), label,
                               str_replace(label, "Black", ShareWord)),
            `2002-2012_mean`, `2002-2012_sd`, `2002-2012_within_sd`, `2002-2012_n`,
            `2013-2025_mean`, `2013-2025_sd`, `2013-2025_within_sd`, `2013-2025_n`)

Tab14 <- kbl(SumTable14, format = "latex", booktabs = TRUE, escape = FALSE,
             linesep = "", align = "lrrrrrrrr",
             col.names = c("", rep(c("Mean", "SD", "Within SD", "N"), 2)),
             caption = paste0("Roster racial composition: summary statistics by period",
                              if (!is_primary_measure(measure)) glue(" ({measure} race measure)"),
                              " \\label{tab:roster-sumstats}")) |>
  add_header_above(c(" " = 1, "2002-2012" = 4, "2013-2025" = 4)) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_notes(c(
    "This table reports summary statistics of roster racial composition across franchise-seasons.",
    paste("The headcount share is the Black share of the game-day roster, weighting each player by",
          "the regular-season weeks he was on the game-day roster (2002-2025). The snap-weighted share",
          "weights each player by his offensive plus defensive snaps (2013-2025, when snap counts start);",
          glue("the snap-weighted estimation panel (Tables \\ref{{tab:roster-diversity-main}}, "),
          glue("\\ref{{tab:roster-diversity-outcomes}} and \\ref{{tab:roster-diversity-robustness}}) "),
          glue("is {SnapSeasons13}, from the first season with the prior season's cap share.")),
    paste("The position-mix expected share is the sum over position groups of the team's position",
          "weight times the league-season Black share at that position, computed on all other",
          "franchises; the residual share is the actual minus the position-mix expected share,",
          "composition net of position mix."),
    paste("The mean prior P(Black) is the weighted mean over players of the predicted-race model's",
          "prior probability of being Black, which depends only on predetermined characteristics",
          "(position at entry, rookie era, draft round, college type, hometown-county availability)",
          "and not on names; it summarizes the roster's position, draft and college mix."),
    paste("Other race is neither Black nor white (e.g., Hispanic, Asian, Pacific Islander). The",
          "three-group Blau index is $1 - b^2 - w^2 - o^2$, the probability that two players drawn at",
          "random belong to different groups (Black, white, other)."),
    paste("The opening-day shares are measured on the active roster of the franchise's first",
          "regular-season game, before the season's results arrive; the prior-snap-weighted version",
          "weights those players by their offensive plus defensive snaps in the previous season",
          "(2014-2025). The opening-day starting QB started the first regular-season game."),
    paste("The within SD is the standard deviation of the deviation from the franchise mean within",
          "the period."),
    NoteBlackDef,
    race_measure_note(measure, "team"),
    if (!IsPredMeasure || measure == "preddoc") note_measure_team(measure)))
save_exhibit_tex(Tab14, "table-14-roster-diversity-sumstats", measure)
save_estimates(SumStats, "13-roster-diversity-sumstats", measure)

# ---------------------------------------------------------------------------
# Figure: league mean share Black by season, 10th-90th percentile band
# ---------------------------------------------------------------------------

# Descriptive: the snap-weighted series runs from 2013 (snap counts), one
# season before the estimation panel
FigData <- bind_rows(
  HeadData |> transmute(season, franchise_id, share = ShareBlackRoster,
                        Measure = "Headcount (game-day roster weeks)"),
  TeamSeason |> filter(season >= FirstSnapSeason13) |>
    transmute(season, franchise_id, share = ShareBlackSnapW,
              Measure = "Snap-weighted (offense + defense)")) |>
  filter(!is.na(share)) |>
  group_by(Measure, season) |>
  summarise(Mean = mean(share), P10 = quantile(share, 0.1),
            P90 = quantile(share, 0.9), n = n(), .groups = "drop")

FigShare <- ggplot(FigData, aes(season, Mean, colour = Measure, fill = Measure)) +
  geom_ribbon(aes(ymin = P10, ymax = P90), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  scale_colour_manual(values = c("#1b4f72", "#b9770e")) +
  scale_fill_manual(values = c("#1b4f72", "#b9770e")) +
  scale_x_continuous(breaks = seq(2002, 2025, 2)) +
  labs(title = paste0("Roster share ", ShareWord, " by season"),
       subtitle = "League mean across franchises; band = 10th to 90th percentile across teams",
       x = "Season", y = paste0("Share ", ShareWord), colour = NULL, fill = NULL,
       caption = switch(measure,
         hand = "Hand-coded race: Black alone or in combination.",
         predicted = "Predicted race: mean over players of the predicted probability of being non-Hispanic Black alone (expected share).",
         preddoc = "Predicted race with documented race (Black alone or in combination) where a public source states it, else the model probability of non-Hispanic Black alone (expected share).",
         provisional = "Provisional race measure: hand code where available, else the positive-only Wikipedia category flag (Black alone or in combination).")) +
  theme_customs()
save_exhibit_figure(FigShare, "figure-roster-share-black-by-season", measure)

# ---------------------------------------------------------------------------
# Estimation helpers
# ---------------------------------------------------------------------------

# Control sets. Predetermined (fixed by opening day): lagged outcomes,
# opening-day roster quality, the prior season's cap share (L1TeamCapShare,
# 09; snap-weighted tables only, 2014+), the Black share of the coaches
# listed in the preseason snapshot (full staff observed), opening-day head
# coach and starting QB. Contemporaneous (set during the season; possible bad
# controls): the season's annual cap share (TeamCapShare: the sum of the
# season's cap numbers, which in-season signings, releases, restructures and
# injury settlements change, so it is not fixed by opening day), the snap- or
# roster-week-weighted quality, the coaches' share over all snapshots
# (in-season firings and interim promotions), the season QB (most starts) and
# the season HC (most games). Author decision Oct 2026: no opening-day cap
# budget is constructed from the annual contract numbers (it would still be
# an annual measure); the lag is the predetermined cap control. Under the
# predicted measures the calibration controls add the roster group's mean
# prior and prior-covariate shares and the priors of the persons whose race
# enters as a control.
Lags <- c("LagWinPct", "LagExpectedWins")
QualityPre <- c("MeanLogPickWeek1", "ShareFirstRoundWeek1", "MeanAgeWeek1", "MeanExperienceWeek1")
CapPre <- "L1TeamCapShare"
CapCont <- "TeamCapShare"
QualitySnapPre <- c(CapPre, QualityPre)
QualityHeadPre <- QualityPre
QualitySnapCont <- c(CapCont, "MeanLogPickSnapW", "MeanAgeSnapW", "MeanExperienceSnapW")
QualityHeadCont <- c("MeanLogPickRoster", "ShareFirstRoundRoster", "MeanAgeRoster",
                     "MeanExperienceRoster")
StaffPre <- c("ShareBlackCoachesPre", "ShareBlackCoachesPreMiss", "HCWeek1Black",
              "HCWeek1BlackMiss", "QBWeek1Black", "QBWeek1BlackMiss")
StaffCont <- c("ShareBlackCoaches", "ShareBlackCoachesMiss", "HCBlack", "HCBlackMiss",
               "QBBlack", "QBBlackMiss")
StaffPrePrior <- c("MeanPriorBlackCoachesPre", "HCWeek1PriorBlack", "QBWeek1PriorBlack")
StaffContPrior <- c("MeanPriorBlackCoaches", "HCPriorBlack", "QBPriorBlack")
FE2 <- "franchise_id + season"

# Calibration controls of a roster share (v: list with prior = mean-prior
# name and cov = prior-covariate share names) under measure m: the mean prior,
# the covariate shares and the staff/QB priors (`staff`); none outside the
# predicted measures
calib_terms <- function(v, m, staff = StaffPrePrior) {
  if (is_pred(m)) c(v$prior, v$cov, staff) else character()
}
has_calib <- function(s) any(str_starts(s$rhs, "ShareCollege"))

# feols formula y ~ rhs | fe (fe = "" for none)
make_fml13 <- function(y, rhs, fe = "") {
  f <- paste(y, "~", paste(rhs, collapse = " + "))
  if (fe != "") f <- paste(f, "|", fe)
  as.formula(f)
}

# Drop missing-indicator controls (*Miss) that are constant within every
# season: season fixed effects absorb them (e.g. the coaches'-share indicator
# for 2002-2006, when no full staff is observed), and the lm refit of the
# wild cluster bootstrap cannot carry aliased columns
prune_rhs <- function(rhs, data) {
  keep <- map_lgl(rhs, \(v) {
    if (!str_ends(v, "Miss")) return(TRUE)
    any(tapply(data[[v]], data$season, \(x) n_distinct(x) > 1))
  })
  rhs[keep]
}

# Hold the sample fixed within a table: keep rows with every variable of
# every column observed. Stops when controls drop more than 10% of the rows
# that have the outcome and the diversity terms (a symptom of incomplete
# control coverage), and reports the loss.
common_sample <- function(df, specs, label) {
  vars <- unique(unlist(map(specs, \(s) c(s$y, s$rhs))))
  key <- unique(unlist(map(specs, \(s) c(s$y, s$key, s$quad))))
  n_key <- sum(complete.cases(df[key]))
  out <- df |> filter(if_all(all_of(vars), \(x) !is.na(x)))
  message(glue("13: {label}: {nrow(out)} of {n_key} rows with outcome and treatment ",
               "observed are kept ({round(100 * (1 - nrow(out) / n_key), 1)}% lost to controls)"))
  if (nrow(out) < 0.9 * n_key) {
    stop("13: ", label, ": controls drop more than 10% of the rows; check control coverage")
  }
  out
}

# Fit a named list of specs (each: y, rhs, fe, key = diversity term, wcb =
# coefficients to test) on `data` with franchise-clustered standard errors
# (the model is fitted on the columns it uses only: modelsummary recovers
# the estimation data from the model, which is slow with the full samples'
# hundreds of columns; obs() row indices are unchanged)
fit_one <- function(s, data) {
  rhs <- prune_rhs(s$rhs, data)
  fe_vars <- if (s$fe == "") character() else all.vars(as.formula(paste("~", gsub("^", "+", s$fe, fixed = TRUE))))
  keep <- unique(c(s$y, rhs, fe_vars, "franchise_id",
                   intersect(c("game_id", "opponent_franchise_id"), names(data))))
  feols(make_fml13(s$y, rhs, s$fe), data = as.data.frame(data)[keep],
        vcov = ~franchise_id, notes = FALSE)
}
fit_specs <- function(specs, data) map(specs, \(s) fit_one(s, data))

# Wild cluster bootstrap p-values of each spec's `wcb` coefficients; one row
# per model x term (NA with a message if the bootstrap fails)
wcb_specs <- function(models, specs, data) {
  imap_dfr(models, \(m, nm) {
    map_dfr(specs[[nm]]$wcb, \(p) {
      out <- tryCatch(wild_cluster_test(m, data, p, "franchise_id", B = WcbReps),
                      error = \(e) {
                        message(glue("13: WCB failed for {nm} / {p}: {conditionMessage(e)}"))
                        tibble(term = p, estimate = unname(coef(m)[p]), p_wcb = NA_real_,
                               ci_low = NA_real_, ci_high = NA_real_,
                               n_clusters = NA_integer_, B = WcbReps)
                      })
      mutate(out, model = nm, .before = 1)
    })
  })
}

# WCB p-value of (model, term) as text
wcb_text <- function(wcb, nm, term) {
  p <- wcb$p_wcb[wcb$model == nm & wcb$term == term]
  if (length(p) == 0) "" else fmt_p(p)
}

# SD of x net of the fixed effects `fe` (a fixest FE string such as
# "franchise_id + season" or "franchise_id^season"; "" = total SD): the
# identifying variation of a coefficient in a specification with those FE
fe_resid_sd <- function(x, data, fe) {
  if (fe == "") return(sd(x))
  terms <- str_split(fe, fixed("+"))[[1]] |> str_trim()
  fe_df <- as.data.frame(set_names(map(terms, \(t) {
    do.call(paste, c(as.list(data[str_split(t, fixed("^"))[[1]]]), sep = "_"))
  }), paste0("fe", seq_along(terms))))
  sd(fixest::demean(x, fe_df)[, 1])
}

# Yes/No
yes_no13 <- function(cond) if_else(cond, "Yes", "No")

# Tidy estimates of `terms` with WCB p-values, tagged with the table
tidy_with_wcb <- function(models, terms, wcb, table) {
  tidy_terms(models, terms) |>
    left_join(select(wcb, model, term, p_wcb, wcb_ci_low = ci_low, wcb_ci_high = ci_high),
              by = c("model", "term")) |>
    mutate(table = table, .before = 1)
}
Estimates13 <- list()

# Rows shared by the team-season tables: WCB p-value of the first wcb term,
# outcome mean, SD of the diversity term net of the column's fixed effects
# and the effect of a one-SD change (times `scale`, e.g. 100 for win-pct
# points). For a share + share^2 column (spec$quad), the effect is the
# marginal effect at the sample mean share times the SD, and a turning-point
# row is filled.
diversity_rows <- function(models, specs, data_list, wcb, scale = 100,
                           effect_label = "Effect of 1-SD change", effect_digits = 2) {
  rows <- tibble(term = c("WCB p-value, diversity", "Mean of outcome",
                          "SD of diversity net of FE", effect_label, "Turning point (share)"))
  for (nm in names(models)) {
    m <- models[[nm]]
    s <- specs[[nm]]
    d <- (if (is.data.frame(data_list)) data_list else data_list[[nm]])[obs(m), ]
    x <- d[[s$key]]
    sdx <- fe_resid_sd(x, d, s$fe)
    slope <- coef(m)[[s$key]]
    turn <- ""
    if (!is.null(s$quad)) {
      b2 <- coef(m)[[s$quad]]
      slope <- slope + 2 * b2 * mean(x)
      turn <- fmt_num(-coef(m)[[s$key]] / (2 * b2), 3)
    }
    rows[[nm]] <- c(wcb_text(wcb, nm, s$wcb[1]), fmt_num(mean(d[[s$y]]), 3), fmt_num(sdx, 3),
                    fmt_num(scale[[1]] * slope * sdx, effect_digits), turn)
  }
  if (all(map_lgl(specs, \(s) is.null(s$quad)))) rows <- filter(rows, term != "Turning point (share)")
  rows
}

# Yes/No indicator rows from spec contents
indicator_rows <- function(specs, items) {
  rows <- tibble(term = names(items))
  for (nm in names(specs)) rows[[nm]] <- map_chr(items, \(f) yes_no13(f(specs[[nm]])))
  rows
}

# Shared note text
NoteCluster13 <- paste("Standard errors, in parentheses, are clustered at the franchise level",
                       "(32 clusters); wild cluster bootstrap-t p-values (restricted, WCR) use",
                       glue("Webb weights and {formatC(WcbReps, big.mark = ',', format = 'd')} replications."))
NoteIdent <- paste("The identifying assumption is that, conditional on franchise and season fixed",
                   "effects and the controls, within-franchise changes in roster composition are",
                   "uncorrelated with other time-varying determinants of performance (talent,",
                   "injuries, coaching).")
NoteSnap <- paste("The snap-weighted share Black weights each player by his offensive plus",
                  "defensive snaps for the franchise in the regular season; it is measured during",
                  "season $t$ and can respond to injuries and benching. Snap weights also respond",
                  "to game script: teams that trail run more offensive plays, teams with weak",
                  "offenses play more defensive snaps, and defensive units have higher Black",
                  "shares, so losing can raise the snap-weighted share mechanically (Table",
                  "\\ref{tab:roster-diversity-robustness} reports the unit-balanced share and a",
                  "control for the defensive snap share). The coefficients on this share are",
                  "therefore descriptive conditional associations between in-season playing time",
                  "by race and results, not effects of a roster fixed before the season; the",
                  "opening-day roster treatments of Table \\ref{tab:roster-diversity-robustness},",
                  "columns (3)-(4), are the ones fixed before the season's results arrive.")
NoteHead <- paste("The headcount share Black weights each player by the regular-season weeks",
                  "he was on the franchise's game-day roster.")
NoteResid <- paste("The residual share is the actual share minus the expected share given the",
                   "team's position mix (the sum over position groups of the team's position",
                   "weight times the league-season Black share at that position on all other",
                   "franchises).")
NoteControls <- paste("Controls are fixed by opening day: the lagged win percentage and lagged",
                      "market expected wins; roster quality of the active roster of the first",
                      "regular-season game (mean log draft pick, undrafted = log 300; share of",
                      "first-round picks; mean age; mean experience); and the Black share of the",
                      "on-field coaches listed in the preseason staff snapshot, used only where the",
                      "full staff is observed (2007 on; a missing indicator absorbs earlier",
                      "seasons), the opening-day head coach's race and the opening-day starting",
                      "quarterback's race.",
                      if (IsPredMeasure) paste(
                        "Under the predicted measures these race controls are predicted",
                        "probabilities like the treatment, and the calibration controls include",
                        "their priors (the coaches' mean prior and the head coach's and",
                        "quarterback's priors). The head coach's predicted probability is",
                        "calibrated to the staff population, not to the selected population of",
                        "head coaches, so it is a noisy control.")
                      else paste("Race controls are zero-filled with missing indicators when a",
                                 "person is uncoded."))
NoteCap <- paste("Columns with roster quality also include the prior season's cap share (the sum of",
                 "players' cap percent on the franchise's season $t-1$ cap table; first observed in",
                 glue("{FirstSnapEstSeason13}, which is why the panel starts there). The season's own"),
                 "cap share is an annual total that in-season signings, releases and restructures",
                 "change, so it is not fixed by opening day and enters only the contemporaneous",
                 "column of Table \\ref{tab:roster-diversity-robustness}.")
NoteNickell <- paste("With franchise fixed effects over a short panel the lagged win percentage is",
                     "subject to Nickell bias; it is a rough control for persistent talent, which",
                     "is why lagged market expected wins enter as well.")

# ---------------------------------------------------------------------------
# Tables 15 and 15b: equation (2), WinPct, columns (1)-(9), for the
# snap-weighted share (2014-2025) and the headcount share (2002-2025)
# ---------------------------------------------------------------------------

# The nine columns for one share measure under race measure m. v: share,
# residual, expected, squared share, three-group Blau, mean-prior and
# prior-covariate share names; quality: roster-quality controls. Under the
# predicted measures columns (4) and (6)-(9) include the calibration controls
# and column (5) drops them; under the other measures column (5) adds the
# mean prior and prior-covariate shares as composition controls.
ladder_specs <- function(v, quality, m = measure) {
  base <- c(Lags, quality, StaffPre, calib_terms(v, m))
  col5 <- if (is_pred(m)) c(v$share, Lags, quality, StaffPre) else c(v$share, base, v$prior, v$cov)
  list(
    "(1)" = list(y = "WinPct", rhs = v$share, fe = "season", key = v$share, wcb = v$share),
    "(2)" = list(y = "WinPct", rhs = v$share, fe = FE2, key = v$share, wcb = v$share),
    "(3)" = list(y = "WinPct", rhs = c(v$share, Lags, quality), fe = FE2, key = v$share,
                 wcb = v$share),
    "(4)" = list(y = "WinPct", rhs = c(v$share, base), fe = FE2, key = v$share, wcb = v$share),
    "(5)" = list(y = "WinPct", rhs = col5, fe = FE2, key = v$share, wcb = v$share),
    "(6)" = list(y = "WinPct", rhs = c(v$resid, base), fe = FE2, key = v$resid, wcb = v$resid),
    "(7)" = list(y = "WinPct", rhs = c(v$share, v$expected, base), fe = FE2, key = v$share,
                 wcb = v$share),
    "(8)" = list(y = "WinPct", rhs = c(v$share, v$sq, base), fe = FE2, key = v$share,
                 quad = v$sq, wcb = v$sq),
    "(9)" = list(y = "WinPct", rhs = c(v$blau3, base), fe = FE2, key = v$blau3, wcb = v$blau3))
}

VarsSnap <- list(share = "ShareBlackSnapW", resid = "ResidualShareBlackSnapW",
                 expected = "ExpectedShareBlackSnapW", sq = "ShareBlackSnapWSq",
                 blau3 = "Blau3SnapW", prior = "MeanPriorBlackSnapW", cov = cov_terms("SnapW"),
                 offense = "ShareBlackOffenseSnapW", defense = "ShareBlackDefenseSnapW",
                 unit_prior = c("MeanPriorBlackOffenseSnapW", "MeanPriorBlackDefenseSnapW"))
VarsHead <- list(share = "ShareBlackRoster", resid = "ResidualShareBlackRoster",
                 expected = "ExpectedShareBlackRoster", sq = "ShareBlackRosterSq",
                 blau3 = "Blau3Roster", prior = "MeanPriorBlackRoster", cov = cov_terms("Roster"),
                 offense = "ShareBlackOffense", defense = "ShareBlackDefense",
                 unit_prior = c("MeanPriorBlackOffense", "MeanPriorBlackDefense"))
VarsWeek1 <- list(share = "ShareBlackWeek1", prior = "MeanPriorBlackWeek1",
                  cov = cov_terms("Week1"))
VarsWeek1Prior <- list(share = "ShareBlackWeek1PriorSnapW", prior = "MeanPriorBlackWeek1PriorSnapW",
                       cov = cov_terms("Week1PriorSnapW"))

# Coefficient labels shared by the team-season tables (one generic label per
# role so that the snap-weighted and headcount tables read the same)
coef_map_ladder <- function(v, what) c(
  set_names(glue("Roster share {ShareWord} ({what})"), v$share),
  set_names("Mean prior P(Black)", v$prior),
  set_names(glue("Residual roster share {ShareWord} (net of position mix)"), v$resid),
  set_names(glue("Position-mix expected share {ShareWord}"), v$expected),
  set_names(glue("Roster share {ShareWord}, squared"), v$sq),
  set_names("Three-group Blau index (diversity)", v$blau3),
  LagWinPct = "Lagged win pct.",
  LagExpectedWins = "Lagged market expected wins",
  ShareBlackCoachesPre = glue("Opening-day coaches' share {ShareWord}"),
  HCWeek1Black = glue("Opening-day head coach {ShareWord}"),
  QBWeek1Black = glue("Opening-day starting QB {ShareWord}"))

CalibLabel <- if (IsPredMeasure) "Calibration controls (priors, prior-covariate shares)" else
  "Mean prior and prior-covariate shares"
Items15 <- set_names(list(
  \(s) TRUE,
  \(s) str_detect(s$fe, "franchise_id"),
  \(s) "LagWinPct" %in% s$rhs,
  has_calib), c("Season FE", "Franchise FE", "Lagged outcomes and roster quality", CalibLabel))

# Fit, bootstrap and write one ladder table; returns the tidy estimates
run_ladder <- function(v, quality, data, label_suffix, what, seasons, table_id, name, label,
                       extra_note) {
  specs <- ladder_specs(v, quality)
  d <- common_sample(data, specs, table_id)
  models <- fit_specs(specs, d)
  wcb <- wcb_specs(models, specs, d)
  rows <- bind_rows(diversity_rows(models, specs, d, wcb,
                                   effect_label = glue("Effect of 1-SD change{EffectWord} (win pct. pts.)")),
                    indicator_rows(specs, Items15))
  cm <- coef_map_ladder(v, what)
  # Correlation of the Blau index with the share net of franchise and season FE
  cor_blau <- cor(fixest::demean(as.matrix(d[c(v$share, v$blau3)]),
                                 d[c("franchise_id", "season")]))[1, 2]
  message(glue("13: {table_id}: correlation of {v$blau3} with {v$share} net of FE = {round(cor_blau, 3)}"))
  # Correlation of the mean prior with the share net of franchise and season
  # FE: its square is the share of the share's identifying variation that the
  # prior's covariates account for
  cor_prior <- cor(fixest::demean(as.matrix(d[c(v$share, v$prior)]),
                                  d[c("franchise_id", "season")]))[1, 2]
  message(glue("13: {table_id}: correlation of {v$prior} with {v$share} net of FE = {round(cor_prior, 3)}"))
  write_model_table(
    models, cm,
    title = glue("Roster racial composition and win percentage, {label_suffix}, {seasons}"),
    label = label, name = name, measure = measure, add_rows = rows, design = "team",
    notes = c(
      glue("This table includes the estimation results of equation (2). The unit of observation ",
           "is a franchise-season, {seasons} ({nrow(d)} franchise-seasons); the dependent variable ",
           "is the regular-season win percentage (0-1)."),
      extra_note,
      paste("Column (1) includes season fixed effects; column (2) adds franchise fixed effects, so",
            "identification comes from changes in a franchise's roster composition over time.",
            NoteIdent),
      paste("Column (3) adds lagged outcomes and opening-day roster quality, to address talent:",
            "rosters with different composition may simply be more talented.", NoteNickell),
      paste("Column (4) adds the staff and quarterback controls, so the roster share is not",
            "proxying for a Black head coach, staff composition or the quarterback's race.",
            NoteControls,
            if (is_pred(measure)) paste(
              "Under the predicted measure column (4), the headline specification, also includes",
              "the calibration controls: the mean prior P(Black), the weighted mean over players",
              "of the predicted-race model's prior probability of being Black, and the weighted",
              "shares of the levels of the prior's non-position covariates (draft round, college",
              "type, rookie era, hometown-county availability). The probabilities are posteriors",
              "given these covariates, so the coefficient on the expected share identifies the",
              "effect of the true share only conditional on them; draft round and college type",
              "also measure talent directly. Columns (6)-(9) include them as well. Column (5)",
              "drops them, as a sensitivity check.",
              glue("Net of franchise and season fixed effects the mean prior's correlation with the "),
              glue("share is {fmt_num(cor_prior, 2)}."))
            else paste(
              "Column (5) adds the mean prior P(Black), the weighted mean over players of the",
              "predicted-race model's prior probability of being Black, and the weighted shares of",
              "the levels of the prior's non-position covariates (draft round, college type, rookie",
              "era, hometown-county availability). Under this race measure they are predetermined",
              "composition controls, not calibration controls.",
              glue("Net of franchise and season fixed effects the mean prior's correlation with the "),
              glue("share is {fmt_num(cor_prior, 2)}.")),
            NotePlacebo),
      paste("Column (6) replaces the share with the residual share, to address positional",
            "composition: Black shares differ sharply by position (position at entry is also a",
            "covariate of the race prior).", NoteResid,
            "Column (7) instead controls for the position-mix expected share; the",
            "leave-one-franchise-out league share makes it mechanically, but only slightly (of",
            "order 1/31 of the own-share variation), lower when the own share is high."),
      paste("The Black share measures composition, not diversity. Column (8) adds the squared",
            "share; it nests the two-group (Black/non-Black) Blau index $2s(1-s)$, which is a",
            "quadratic in the share. Its WCB p-value refers to the squared term; the effect row",
            "reports the marginal effect at the sample mean share, and the turning point is",
            "$-\\beta_1/(2\\beta_2)$. Column (9) uses the three-group (Black, white, other) Blau",
            "index $1 - b^2 - w^2 - o^2$, the probability that two players drawn at random belong",
            "to different groups, as the diversity measure. Net of franchise and season fixed",
            glue("effects its correlation with the Black share is {fmt_num(cor_blau, 2)}; the mean"),
            glue("share is {fmt_num(mean(d[[v$share]]), 2)},"),
            if (mean(d[[v$share]]) > 0.5) "above one half, so a higher Black share means a less diverse roster." else
              "below one half, so the index rises with the share.",
            switch(measure,
              provisional = "Under the provisional measure few players are flagged other.",
              predicted = , preddoc = paste(
                "Under the predicted measures the index is computed from the expected shares",
                "(mean member probabilities), not as the expected index."),
              NULL)),
      paste("The SD row is the SD of the diversity measure net of the column's fixed effects in the",
            "estimation sample; the effect row multiplies it by the coefficient and by 100",
            "(win-percentage points). Roster-quality and calibration-control coefficients other",
            "than the mean prior are not shown."),
      NoteCluster13, NoteMeasureTeam))
  message(glue("13: {table_id} done at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))
  tidy_with_wcb(models, names(cm), wcb, table_id)
}

# ---------------------------------------------------------------------------
# Table 18 models (estimated here so that the main tables' notes can report
# the placebo results; the table is written below): one-season leads and the
# reverse-causality test (composition on past performance)
# ---------------------------------------------------------------------------

# Base controls of the column-(4) specification (with the calibration
# controls under the predicted measures)
BaseSnap <- c(Lags, QualitySnapPre, StaffPre, calib_terms(VarsSnap, measure))
BaseHead <- c(Lags, QualityHeadPre, StaffPre, calib_terms(VarsHead, measure))
# Leads (F1) or lags (L1) of a roster group's mean prior and prior-covariate
# shares (predicted measures only)
calib_shift <- function(v, p, m = measure) {
  if (is_pred(m)) paste0(p, c(v$prior, v$cov)) else character()
}
Specs18 <- list(
  "(1)" = list(y = "WinPct",
               rhs = c("ShareBlackSnapW", "F1ShareBlackSnapW", BaseSnap, calib_shift(VarsSnap, "F1")),
               fe = FE2, key = "F1ShareBlackSnapW", wcb = c("F1ShareBlackSnapW", "ShareBlackSnapW"),
               data = "snap"),
  "(2)" = list(y = "WinsOverExpected",
               rhs = c("ShareBlackSnapW", "F1ShareBlackSnapW", BaseSnap, calib_shift(VarsSnap, "F1")),
               fe = FE2, key = "F1ShareBlackSnapW",
               wcb = c("F1ShareBlackSnapW", "ShareBlackSnapW"), data = "snap"),
  "(3)" = list(y = "WinPct",
               rhs = c("ShareBlackRoster", "F1ShareBlackRoster", BaseHead, calib_shift(VarsHead, "F1")),
               fe = FE2, key = "F1ShareBlackRoster", wcb = c("F1ShareBlackRoster", "ShareBlackRoster"),
               data = "head"),
  "(4)" = list(y = "ShareBlackSnapW", rhs = c("LagWinPct", "L1ShareBlackSnapW"), fe = FE2,
               key = "LagWinPct", wcb = "LagWinPct", data = "snap"),
  "(5)" = list(y = "ShareBlackRoster", rhs = c("LagWinPct", "L1ShareBlackRoster"), fe = FE2,
               key = "LagWinPct", wcb = "LagWinPct", data = "head"),
  # Decomposition: the change in composition net of the change in the mean
  # prior (draft, college, position and era mix), i.e. the part of the
  # reverse-causality coefficient not explained by the prior's covariates
  "(6)" = list(y = "ShareBlackSnapW",
               rhs = c("LagWinPct", "L1ShareBlackSnapW", "MeanPriorBlackSnapW", "L1MeanPriorBlackSnapW"),
               fe = FE2, key = "LagWinPct", wcb = "LagWinPct", data = "snap"),
  "(7)" = list(y = "ShareBlackRoster",
               rhs = c("LagWinPct", "L1ShareBlackRoster", "MeanPriorBlackRoster", "L1MeanPriorBlackRoster"),
               fe = FE2, key = "LagWinPct", wcb = "LagWinPct", data = "head"))
Data18 <- imap(Specs18, \(s, nm) common_sample(if (s$data == "snap") SnapData else HeadData,
                                               list(s), paste("table-18", nm)))
Models18 <- imap(Specs18, \(s, nm) fit_one(s, Data18[[nm]]))
Wcb18 <- imap_dfr(Models18, \(m, nm) wcb_specs(set_names(list(m), nm), Specs18[nm], Data18[[nm]]))

# Placebo results for the notes of the main tables: lead and current share in
# column (1) (snap-weighted) and column (3) (headcount), past win percentage
# in columns (4)-(5) and net of the mean prior in columns (6)-(7)
coef_txt18 <- function(nm, term, digits = 3) {
  ct <- coeftable(Models18[[nm]])
  glue("{fmt_num(ct[term, 1], digits)} (SE {fmt_num(ct[term, 2], digits)}; WCB p = ",
       "{wcb_text(Wcb18, nm, term)})")
}
LeadRatio18 <- abs(coef(Models18[["(1)"]])[["F1ShareBlackSnapW"]] /
                     coef(Models18[["(1)"]])[["ShareBlackSnapW"]])
LeadP18 <- min(Wcb18$p_wcb[Wcb18$model %in% c("(1)", "(3)") & str_starts(Wcb18$term, "F1")],
               na.rm = TRUE)
NotePlacebo <- paste(
  "Placebo tests (Table \\ref{tab:roster-diversity-placebo}): with the column (4) controls, next",
  glue("season's snap-weighted share enters the win percentage with {coef_txt18('(1)', 'F1ShareBlackSnapW')}"),
  glue("against {coef_txt18('(1)', 'ShareBlackSnapW')} for the current share; for the headcount share"),
  glue("the lead is {coef_txt18('(3)', 'F1ShareBlackRoster')} and the current share"),
  glue("{coef_txt18('(3)', 'ShareBlackRoster')}. Last season's win percentage predicts the current"),
  glue("share with {coef_txt18('(4)', 'LagWinPct', 4)} (snap-weighted) and"),
  glue("{coef_txt18('(5)', 'LagWinPct', 4)} (headcount); net of the current and lagged mean prior"),
  glue("these become {coef_txt18('(6)', 'LagWinPct', 4)} and {coef_txt18('(7)', 'LagWinPct', 4)}."),
  if (LeadP18 < 0.1) paste(
    "Next season's composition is", if (LeadRatio18 >= 1) "more strongly" else "about as strongly",
    "related to this season's results", if (LeadRatio18 >= 1) "than" else "as",
    "the current composition, so the identifying assumption is rejected and these estimates",
    "should not be read as causal effects: performance shapes composition, or a persistent",
    "franchise trait drives both.")
  else if (LeadRatio18 >= 0.5) paste(
    "The leads are as large as the current share but imprecise, so the placebo test cannot",
    "rule out that performance shapes composition; these estimates should not be read as",
    "causal effects without further evidence.")
  else paste(
    "The leads are small relative to the current share and imprecise, so the placebo test does",
    "not reject the identifying assumption."))
message("13: ", NotePlacebo)

# Common sample of Table 15 (every column on the same franchise-seasons);
# Table 18b, column (1), reuses it so that the contemporaneous-controls
# comparison holds the sample fixed
Sample15 <- common_sample(SnapData, ladder_specs(VarsSnap, QualitySnapPre), "table-15 sample")
Estimates13[["table-15"]] <- run_ladder(
  VarsSnap, QualitySnapPre, Sample15, "snap-weighted share", "snap-weighted", SnapSeasons13,
  "table-15", "table-15-roster-diversity-team-season", "roster-diversity-main",
  paste(NoteSnap, NoteCap))
Estimates13[["table-15b"]] <- run_ladder(
  VarsHead, QualityHeadPre, HeadData, "headcount share", "headcount", "2002-2025",
  "table-15b", "table-15b-roster-diversity-team-season-headcount", "roster-diversity-main-headcount",
  paste(NoteHead, "The headcount panel is longer than the snap-weighted panel (Table",
        "\\ref{tab:roster-diversity-main}) and offers more within-franchise variation at the cost",
        "of a noisier exposure measure. No cap share enters: team cap sums are incomplete",
        "before 2013, so the prior season's cap share would cut the panel to",
        glue("{SnapSeasons13}.")))

# ---------------------------------------------------------------------------
# Tables 16 and 16b: column-(4) specification for the other outcomes
# ---------------------------------------------------------------------------

# Column-(4) spec for outcome y. The offensive (defensive) EPA columns enter
# the offense and defense unit shares jointly, so that each unit's own
# composition is separated from the other unit's; the key term is the unit's
# own share.
spec4 <- function(y, v, quality, unit = NULL, m = measure) {
  base <- c(Lags, quality, StaffPre, calib_terms(v, m))
  if (is.null(unit)) {
    list(y = y, rhs = c(v$share, base), fe = FE2, key = v$share, wcb = v$share)
  } else {
    # Unit columns also condition on each unit's mean prior
    own <- v[[unit]]
    list(y = y, rhs = c(v$offense, v$defense, base, if (is_pred(m)) v$unit_prior), fe = FE2,
         key = own, wcb = own)
  }
}

outcome_specs <- function(v, quality) list(
  "(1) Point diff./game" = spec4("PointDiffPerGame", v, quality),
  "(2) Wins over exp." = spec4("WinsOverExpected", v, quality),
  "(3) Off. EPA/play" = spec4("OffEPAPerPlay", v, quality, "offense"),
  "(4) $-$Def. EPA/play" = spec4("NegDefEPAPerPlay", v, quality, "defense"),
  "(5) Playoffs" = spec4("Playoffs", v, quality))

run_outcomes <- function(v, quality, data, what, seasons, table_id, name, label, extra_note) {
  specs <- outcome_specs(v, quality)
  # Each column on its own complete-case sample (outcome coverage differs)
  dl <- map(specs, \(s) common_sample(data, list(s), paste(table_id, s$y)))
  models <- imap(specs, \(s, nm) fit_one(s, dl[[nm]]))
  wcb <- imap_dfr(models, \(m, nm) wcb_specs(set_names(list(m), nm), specs[nm], dl[[nm]]))
  rows <- bind_rows(
    diversity_rows(models, specs, dl, wcb, scale = 1,
                   effect_label = glue("Effect of 1-SD change{EffectWord}"), effect_digits = 3),
    indicator_rows(specs, set_names(list(\(s) TRUE, \(s) TRUE, has_calib),
                                    c("Season and franchise FE",
                                      "Lagged outcomes, roster quality, staff and QB", CalibLabel))))
  cm <- c(set_names(glue("Roster share {ShareWord} ({what})"), v$share),
          set_names(glue("Offense share {ShareWord} ({what})"), v$offense),
          set_names(glue("Defense share {ShareWord} ({what})"), v$defense),
          ShareBlackCoachesPre = glue("Opening-day coaches' share {ShareWord}"),
          HCWeek1Black = glue("Opening-day head coach {ShareWord}"),
          QBWeek1Black = glue("Opening-day starting QB {ShareWord}"))
  write_model_table(
    models, cm,
    title = glue("Roster racial composition and team performance: other outcomes, {what} share, {seasons}"),
    label = label, name = name, measure = measure, add_rows = rows, design = "team",
    notes = c(
      glue("This table includes the estimation results of equation (2) with the column (4) ",
           "specification of Table \\ref{{tab:roster-diversity-main}}: season and franchise fixed ",
           "effects, lagged outcomes, opening-day roster quality and the staff and quarterback ",
           "controls{if (IsPredMeasure) ', and the calibration controls (mean prior and prior-covariate shares of the roster, priors of the race controls; the offense and defense columns add the mean prior of each unit)' else ''}. ",
           "The unit of observation is a franchise-season, {seasons}."),
      extra_note, NoteControls,
      paste("The outcomes are regular-season point differential per game (column (1)), wins over",
            "expected (column (2): actual wins minus the sum of the market's pre-game win",
            "probabilities), offensive EPA per play (column (3)), minus defensive EPA per play",
            "(column (4), higher = better defense) and a playoff indicator (column (5), linear",
            "probability model)."),
      paste("Wins over expected nets out what the betting market prices before each game,",
            "including roster composition, so its coefficient estimates only the part of any",
            "composition effect the market fails to price; it is zero under market efficiency even",
            "if composition has a causal effect on winning."),
      paste("Columns (3) and (4) enter the offense and defense shares jointly, so each unit's",
            "composition is separated from the other unit's; the WCB p-value, SD and effect rows",
            "refer to the unit's own share."),
      paste("The SD row is the SD of the diversity term net of franchise and season fixed effects;",
            "the effect row multiplies it by the coefficient, in units of the outcome. Lagged-outcome",
            "and roster-quality coefficients are not shown."),
      NoteCluster13, NoteMeasureTeam))
  message(glue("13: {table_id} done at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))
  tidy_with_wcb(models, names(cm), wcb, table_id)
}

Estimates13[["table-16"]] <- run_outcomes(
  VarsSnap, QualitySnapPre, SnapData, "snap-weighted", SnapSeasons13, "table-16",
  "table-16-roster-diversity-outcomes", "roster-diversity-outcomes",
  paste(NoteSnap, "The unit shares weight players by their offensive or defensive snaps.", NoteCap))
Estimates13[["table-16b"]] <- run_outcomes(
  VarsHead, QualityHeadPre, HeadData, "headcount", "2002-2025", "table-16b",
  "table-16b-roster-diversity-outcomes-headcount", "roster-diversity-outcomes-headcount",
  paste(NoteHead, "The unit shares are the headcount shares of offensive (QB, RB, WR, TE, OL)",
        "and defensive (DL, LB, DB) game-day players."))

# ---------------------------------------------------------------------------
# Table 17: within-season game-level design (equation (3)), REG 2002-2025
# ---------------------------------------------------------------------------

# Game starting-QB race is zero-filled with missing indicators in
# build_samples13()

# Wild cluster bootstrap for a model with one high-dimensional fixed effect
# (franchise x season). The shared wild_cluster_test() refits with every
# fixed effect as dummies, and boottest.lm() in fwildclusterboot 0.14 has no
# `fe` argument, so here the outcome and regressors are demeaned within the
# fixed effect (Frisch-Waugh-Lovell). The refit keeps an intercept (zero on
# demeaned data) because boottest() fails with a single regressor. Team
# seasons are nested in franchise clusters, so the bootstrap weights are
# constant within each fixed-effect group, the bootstrap outcome stays
# demeaned, and the bootstrap-t p-value equals the one with the dummies.
# `fe_var` names a column of `data` identifying the FE.
wcb_one_fe <- function(model, data, param, fe_var, cluster = "franchise_id") {
  d <- as.data.frame(data)[obs(model), , drop = FALSE]
  vars <- c(as.character(model$fml[[2]]), names(coef(model)))
  dm <- as.data.frame(fixest::demean(as.matrix(d[vars]), d[[fe_var]]))
  names(dm) <- vars
  dm[[cluster]] <- d[[cluster]]
  fml <- as.formula(paste(vars[1], "~", paste(vars[-1], collapse = " + ")))
  refit <- do.call("lm", list(formula = fml, data = dm))
  if (!isTRUE(all.equal(unname(coef(refit)[param]), unname(coef(model)[param]),
                        tolerance = 1e-6))) {
    stop("wcb_one_fe: demeaned refit does not reproduce the fixest estimate of ", param)
  }
  set.seed(20261002)
  dqrng::dqset.seed(20261002)
  bt <- suppressMessages(suppressWarnings(
    fwildclusterboot::boottest(refit, param = param, clustid = cluster, B = WcbReps,
                               type = "webb")))
  tibble(term = param, estimate = unname(coef(model)[param]), p_wcb = bt$p_val,
         ci_low = bt$conf_int[1], ci_high = bt$conf_int[2], n_clusters = unname(bt$N_G[1]),
         B = WcbReps)
}

FETS <- "franchise_id^season"
FETSOpp <- "franchise_id^season + opponent_franchise_id^season"
Share17 <- "ShareBlackActiveRoster"
# Game-level calibration controls of the active-roster share (predicted
# measures only): the active roster's mean prior and prior-covariate shares,
# and the opponent's in the one-row-per-game symmetry columns
calib17 <- function(m, opp = FALSE) {
  if (!is_pred(m)) return(character())
  own <- c("MeanPriorBlackActiveRoster", cov_terms("ActiveRoster"))
  c(own, if (opp) paste0("Opp", own))
}
specs17 <- function(m = measure) {
  C <- calib17(m)
  CO <- calib17(m, opp = TRUE)
  list(
    "(1) ATS margin" = list(y = "AtsMargin", rhs = c(Share17, C), fe = FETS, key = Share17,
                            wcb = "ts"),
    "(2) ATS, residual share" = list(y = "AtsMargin", rhs = c("ResidualShareBlackActiveRoster", C),
                                     fe = FETS, key = "ResidualShareBlackActiveRoster", wcb = "ts"),
    "(3) Margin, spread and home" = list(y = "margin",
                                         rhs = c(Share17, "team_spread_line", "Home", C),
                                         fe = FETS, key = Share17, wcb = "ts"),
    "(4) Win minus market prob." = list(y = "WinOverMarket", rhs = c(Share17, C), fe = FETS,
                                        key = Share17, wcb = "ts"),
    "(5) ATS, franchise and season FE" = list(y = "AtsMargin", rhs = c(Share17, C), fe = FE2,
                                              key = Share17, wcb = "dummies"),
    "(6) ATS, opponent-season FE" = list(y = "AtsMargin", rhs = c(Share17, C), fe = FETSOpp,
                                         key = Share17),
    "(7) Margin, opponent-season FE" = list(y = "margin",
                                            rhs = c(Share17, "team_spread_line", "Home", C),
                                            fe = FETSOpp, key = Share17),
    "(8) Symmetry check" = list(y = "AtsMargin", rhs = c(Share17, "OppShareBlackActiveRoster", CO),
                                fe = FETSOpp, key = Share17, home = TRUE),
    "(9) Symmetry, game QB" = list(y = "AtsMargin",
                                   rhs = c(Share17, "OppShareBlackActiveRoster", "GameQBBlack",
                                           "GameQBBlackMiss", "OppGameQBBlack", "OppGameQBBlackMiss",
                                           CO, if (is_pred(m)) c("GameQBPriorBlack",
                                                                 "OppGameQBPriorBlack")),
                                   fe = FETSOpp, key = Share17, home = TRUE))
}
Specs17 <- specs17(measure)
Stacked17 <- names(Specs17)[!map_lgl(Specs17, \(s) isTRUE(s$home))]
Home17 <- setdiff(names(Specs17), Stacked17)

# Stacked columns share one sample; the symmetry columns use one row per
# game (the home team; exactly one per game), because in the stacked,
# symmetric sample the opponent-share coefficient is mechanically minus the
# own-share coefficient
Data17 <- common_sample(GameData, Specs17[Stacked17], "table-17 stacked")
Data17Home <- common_sample(filter(GameData, Home == 1), Specs17[Home17], "table-17 home rows")
data17_for <- function(s) if (isTRUE(s$home)) Data17Home else Data17
Models17 <- map(Specs17, \(s) fit_one(s, data17_for(s)))

# Wild cluster bootstrap (franchise clusters) of the diversity term
Wcb17 <- imap_dfr(Specs17, \(s, nm) {
  if (is.null(s$wcb)) return(tibble())
  out <- tryCatch(
    if (s$wcb == "ts") wcb_one_fe(Models17[[nm]], Data17, s$key, "TeamSeasonId") else
      wild_cluster_test(Models17[[nm]], Data17, s$key, "franchise_id", B = WcbReps),
    error = \(e) {
      message(glue("13: WCB failed for {nm}: {conditionMessage(e)}"))
      tibble(term = s$key, p_wcb = NA_real_, ci_low = NA_real_, ci_high = NA_real_)
    })
  mutate(out, model = nm, .before = 1)
})

# Two-way clustered SEs of the roster-share term: franchise and game for the
# stacked columns (the two rows of a game are mirror images and share a game
# cluster, but neither a franchise nor an opponent cluster); home and away
# franchise for the one-row-per-game columns
TwoWaySE <- imap_dbl(Models17, \(m, nm) {
  v <- if (isTRUE(Specs17[[nm]]$home)) ~franchise_id + opponent_franchise_id else ~franchise_id + game_id
  se(summary(m, vcov = v))[[Specs17[[nm]]$key]]
})

# Symmetry test, H0: beta_own + beta_opp = 0 (franchise-clustered vcov, t
# with G - 1 degrees of freedom)
sym_p <- function(m) {
  b <- coef(m)[c(Share17, "OppShareBlackActiveRoster")]
  V <- vcov(m)[names(b), names(b)]
  t <- sum(b) / sqrt(sum(V))
  2 * pt(-abs(t), df = fitstat(m, "g")[[1]] - 1)
}

Rows17 <- tibble(term = c("Two-way clustered SE, roster share", "Two-way clusters",
                          "WCB p-value, roster share", "p-value, own + opponent share = 0",
                          "Mean of outcome", "SD of roster share net of FE",
                          glue("Effect of 1-SD change{EffectWord}")))
for (nm in names(Models17)) {
  M17 <- Models17[[nm]]
  S17 <- Specs17[[nm]]
  D17 <- data17_for(S17)[obs(M17), ]
  Sd17 <- fe_resid_sd(D17[[S17$key]], D17, S17$fe)
  Rows17[[nm]] <- c(paste0("(", fmt_num(TwoWaySE[[nm]], 3), ")"),
                    if (isTRUE(S17$home)) "Home, away team" else "Franchise, game",
                    wcb_text(Wcb17, nm, S17$key),
                    if (isTRUE(S17$home)) fmt_p(sym_p(M17)) else "",
                    fmt_num(mean(D17[[S17$y]]), 3), fmt_num(Sd17, 3),
                    fmt_num(coef(M17)[[S17$key]] * Sd17, 3))
}
Rows17 <- bind_rows(
  Rows17,
  tibble(term = "Sample", !!!set_names(map(Specs17, \(s)
    if (isTRUE(s$home)) "One row per game" else "Both teams"), names(Specs17))),
  indicator_rows(Specs17, list(
    "Franchise $\\times$ season FE" = \(s) str_detect(s$fe, fixed("franchise_id^season")),
    "Franchise and season FE" = \(s) s$fe == FE2,
    "Opponent $\\times$ season FE" = \(s) str_detect(s$fe, "opponent"),
    "Mean prior and prior-covariate shares" = has_calib)))

CoefMap17 <- c(ShareBlackActiveRoster = glue("Active-roster share {ShareWord}"),
               ResidualShareBlackActiveRoster = glue("Residual active-roster share {ShareWord}"),
               OppShareBlackActiveRoster = glue("Opponent active-roster share {ShareWord}"),
               team_spread_line = "Point spread (team favored $>$ 0)",
               Home = "Home team",
               GameQBBlack = glue("Starting QB {ShareWord} (game)"),
               OppGameQBBlack = glue("Opponent starting QB {ShareWord} (game)"))

write_model_table(
  Models17, CoefMap17,
  title = "Roster racial composition and game outcomes: within-season design, regular season 2002-2025",
  label = "roster-diversity-game",
  name = "table-17-roster-diversity-team-game", measure = measure,
  add_rows = Rows17, design = "team",
  notes = c(
    glue("This table includes the estimation results of equation (3), the within-season ",
         "game-level version of equation (2). The unit of observation is a franchise-game in the ",
         "regular season, 2002-2025 ({formatC(nrow(Data17), big.mark = ',', format = 'd')} ",
         "team-games; each game enters once for each team, except in columns (8)-(9))."),
    paste("The treatment is the Black share of the game-day active roster (players not listed",
          "inactive), weighting players equally. Inactive designations are not recorded in",
          "2016-2018, when the active roster equals the full game-day roster. With franchise",
          "$\\times$ season fixed effects, identification comes from week-to-week changes in a",
          "team's active roster (injuries, inactives, signings) within a team-season."),
    if (IsPredMeasure) paste(
      "Under the predicted measure every column includes the calibration controls of the active",
      "roster (its mean prior P(Black) and the weighted shares of the prior's draft-round,",
      "college-type, rookie-era and hometown-county levels); columns (8)-(9) also include the",
      "opponent's, and column (9) the quarterbacks' priors."),
    paste("Column (1), the main specification, uses the margin against the closing spread",
          "(margin minus the spread, positive when the team is favored) as the outcome, which",
          "imposes a unit slope on the spread; the raw slope of the margin on the spread is about",
          "1.04. The spread prices opponent strength and home field, so no opponent fixed effects",
          "are needed. Because the betting market prices roster changes, including composition,",
          "the coefficient estimates only the part of any composition effect the market fails to",
          "price; it is zero under market efficiency even if composition has a causal effect."),
    paste("Column (2) uses the residual share, net of the team's game-day position mix.", NoteResid,
          "Column (3) uses the raw margin and controls for the spread and a home indicator (home",
          "varies within a team-season and is not absorbed). The spread's slope there is",
          "estimated within team-season, where the spread also responds to earlier results of the",
          "same team-season; it is therefore not strictly exogenous, and the slope is far below",
          "one. Column (4) is a linear probability model of a win (ties count one half) net of the",
          "market's pre-game win probability."),
    paste("Columns (5)-(7) show the sensitivity to the fixed effects: franchise and season fixed",
          "effects (column (5)), and franchise $\\times$ season plus opponent $\\times$ season fixed",
          "effects (columns (6)-(7), the plan's original design). In column (7) the spread's slope",
          "is distorted further by the two sets of incidental team-season parameters, so the",
          "column is a robustness check only."),
    paste("Columns (8)-(9) are a symmetry check, not a placebo: the opponent's composition can",
          "legitimately affect the margin, and in a symmetric model it should enter with the",
          "opposite sign and similar size. In the stacked sample the opponent coefficient is",
          "mechanically minus the own coefficient, so these columns use one row per game (the",
          glue("home team; {formatC(nrow(Data17Home), big.mark = ',', format = 'd')} games), with "),
          "home-team $\\times$ season and away-team $\\times$ season fixed effects. The p-value row",
          "tests that the own and opponent coefficients sum to zero. Column (9) adds the race of",
          "each team's starting quarterback in that game (zero-filled with missing indicators)."),
    paste("Standard errors, in parentheses, are clustered at the franchise level (32 clusters).",
          "The rows below report standard errors clustered two ways: by franchise and game in the",
          "stacked columns, where the two rows of a game are mirror images and share a game",
          "cluster but neither a franchise nor an opponent cluster; and by home and away",
          "franchise in columns (8)-(9). Wild cluster bootstrap-t p-values (franchise clusters,",
          glue("Webb weights, {formatC(WcbReps, big.mark = ',', format = 'd')} replications) are "),
          "reported for columns (1)-(5); in columns (1)-(4) the team-season fixed effects are",
          "projected out before the bootstrap (team-seasons are nested in franchise clusters, so",
          "this is equivalent to including them)."),
    paste("The SD row is the SD of the treatment net of the column's fixed effects; the effect row",
          "multiplies it by the coefficient, in points (columns (1)-(3) and (5)-(9)) or win",
          "probability (column (4))."),
    NoteMeasureTeam))

Estimates13[["table-17"]] <- tidy_terms(Models17, names(CoefMap17)) |>
  mutate(table = "table-17", .before = 1) |>
  left_join(select(Wcb17, model, term, p_wcb, wcb_ci_low = ci_low, wcb_ci_high = ci_high),
            by = c("model", "term")) |>
  left_join(tibble(model = names(TwoWaySE), term = map_chr(Specs17, "key"),
                   std_error_twoway = unname(TwoWaySE)), by = c("model", "term")) |>
  left_join(tibble(model = Home17, term = Share17,
                   p_sym = map_dbl(Models17[Home17], sym_p)), by = c("model", "term"))
message(glue("13: table 17 done at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))

# ---------------------------------------------------------------------------
# Table 18: placebo tests (one-season leads; past performance)
# ---------------------------------------------------------------------------


# Rows: WCB p-values, outcome mean, measure and seasons
Rows18 <- tibble(term = c("WCB p-value, share (t+1)", "WCB p-value, share (t)",
                          "WCB p-value, lagged win pct.", "Mean of outcome",
                          "Diversity measure", "Seasons"))
for (nm in names(Specs18)) {
  Sp18 <- Specs18[[nm]]
  D18 <- Data18[[nm]]
  Cur18 <- if (Sp18$data == "snap") "ShareBlackSnapW" else "ShareBlackRoster"
  Rows18[[nm]] <- c(wcb_text(Wcb18, nm, paste0("F1", Cur18)),
                    if (str_starts(Sp18$y, "ShareBlack")) "" else wcb_text(Wcb18, nm, Cur18),
                    wcb_text(Wcb18, nm, "LagWinPct"),
                    fmt_num(mean(D18[[Sp18$y]]), 3),
                    if (Sp18$data == "snap") "Snap-weighted" else "Headcount",
                    paste0(min(D18$season), "-", max(D18$season)))
}
Rows18 <- bind_rows(Rows18, indicator_rows(Specs18, set_names(list(
  \(s) TRUE,
  \(s) "LagExpectedWins" %in% s$rhs,
  \(s) has_calib(s) && any(str_starts(s$rhs, "F1ShareCollege"))),
  c("Season and franchise FE", "Lagged outcomes, roster quality, staff and QB",
    "Calibration controls and their leads"))))

CoefMap18 <- c(F1ShareBlackSnapW = glue("Roster share {ShareWord}, season $t+1$"),
               F1ShareBlackRoster = glue("Roster share {ShareWord}, season $t+1$"),
               ShareBlackSnapW = glue("Roster share {ShareWord}, season $t$"),
               ShareBlackRoster = glue("Roster share {ShareWord}, season $t$"),
               L1ShareBlackSnapW = glue("Roster share {ShareWord}, season $t-1$"),
               L1ShareBlackRoster = glue("Roster share {ShareWord}, season $t-1$"),
               LagWinPct = "Win pct., season $t-1$",
               MeanPriorBlackSnapW = "Mean prior P(Black), season $t$",
               MeanPriorBlackRoster = "Mean prior P(Black), season $t$",
               L1MeanPriorBlackSnapW = "Mean prior P(Black), season $t-1$",
               L1MeanPriorBlackRoster = "Mean prior P(Black), season $t-1$")

write_model_table(
  Models18, CoefMap18,
  title = "Roster racial composition and team performance: placebo tests",
  label = "roster-diversity-placebo",
  name = "table-18-roster-diversity-placebo", measure = measure,
  add_rows = Rows18, design = "team",
  notes = c(
    paste("This table includes the estimation results of placebo versions of equation (2). The",
          "unit of observation is a franchise-season; all columns include season and franchise",
          "fixed effects."),
    paste("Columns (1)-(3) add next season's roster share (season $t+1$) to the current share,",
          "with the column (4) controls of Table \\ref{tab:roster-diversity-main}. Next season's",
          "roster cannot affect this season's results, so a lead coefficient similar to the",
          "current-share coefficient indicates that performance shapes composition (winning",
          "teams retain or acquire different players) or that a persistent franchise trait drives",
          "both. The dependent variable is the win percentage (columns (1) and (3)) or wins over",
          "the market expectation (column (2)).",
          if (IsPredMeasure) paste(
            "Under the predicted measure these columns include the calibration controls of the",
            "column (4) specification and their leads (next season's mean prior and",
            "prior-covariate shares)."),
          if (measure == "hand") "The lead is coverage-gated like the current share.",
          "The lead is missing in 2025, the last season observed."),
    paste("Columns (4)-(5) test reverse causality directly: the dependent variable is the current",
          "roster share, regressed on last season's win percentage conditional on last season's",
          "share, so the coefficient on the lagged win percentage is the change in composition",
          "predicted by past performance. No season-$t$ controls enter, because roster quality in",
          "season $t$ is itself an outcome of season $t-1$ results (e.g., draft position). With",
          "franchise fixed effects the coefficient on the lagged share is subject to Nickell",
          "bias.", if (IsPredMeasure) paste(
            "Under the predicted measure an expected share is unbiased for the true share as a",
            "dependent variable without the prior's covariates, and the draft and college mix",
            "of season $t$ is itself an outcome of season $t-1$ results, so columns (4)-(5) do",
            "not condition on them."),
          "Columns (6)-(7) decompose the reverse-causality coefficient: they add the current and",
          "lagged mean prior P(Black), so the coefficient on the lagged win percentage is the",
          "change in composition not accounted for by changes in the roster's position, draft,",
          "college and era mix (on which the prior depends)."),
    paste(NoteSnap, NoteHead, NoteControls),
    NoteCluster13, NoteMeasureTeam))

Estimates13[["table-18"]] <- tidy_with_wcb(Models18, names(CoefMap18), Wcb18, "table-18")
message(glue("13: table 18 done at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))

# ---------------------------------------------------------------------------
# Table 18b: robustness (predetermined treatments, contemporaneous controls,
# no lagged outcomes, trimming)
# ---------------------------------------------------------------------------

# Drop each season's franchise-seasons above the league-season 99th
# percentile of a share (in practice the season's largest share), a guard
# against franchise-season spikes in the measured share (e.g. Wikipedia
# coverage artifacts under the provisional measure)
trim_top <- function(df, var) {
  df |>
    group_by(season) |>
    filter(.data[[var]] <= quantile(.data[[var]], 0.99, na.rm = TRUE)) |>
    ungroup()
}

# Calibration controls (predicted measures) for each treatment: the roster
# group's mean prior and prior-covariate shares and the staff/QB priors
# (season HC/QB and pooled coaches in the contemporaneous column)
CalSnap <- calib_terms(VarsSnap, measure)
CalHead <- calib_terms(VarsHead, measure)
Specs18b <- list(
  "(1) Contemp. controls" = list(y = "WinPct",
                                 rhs = c("ShareBlackSnapW", Lags, QualitySnapCont, StaffCont,
                                         calib_terms(VarsSnap, measure, StaffContPrior)),
                                 fe = FE2, key = "ShareBlackSnapW", wcb = "ShareBlackSnapW",
                                 data = Sample15),
  "(2) No lagged outcomes" = list(y = "WinPct", rhs = c("ShareBlackSnapW", QualitySnapPre, StaffPre,
                                                        CalSnap),
                                  fe = FE2, key = "ShareBlackSnapW", wcb = "ShareBlackSnapW",
                                  data = SnapData),
  "(3) Opening-day share" = list(y = "WinPct",
                                 rhs = c("ShareBlackWeek1", Lags, QualityHeadPre, StaffPre,
                                         calib_terms(VarsWeek1, measure)),
                                 fe = FE2, key = "ShareBlackWeek1", wcb = "ShareBlackWeek1",
                                 data = HeadData),
  "(4) Opening day, prior snaps" = list(y = "WinPct",
                                        rhs = c("ShareBlackWeek1PriorSnapW", Lags, QualitySnapPre,
                                                StaffPre, calib_terms(VarsWeek1Prior, measure)),
                                        fe = FE2, key = "ShareBlackWeek1PriorSnapW",
                                        wcb = "ShareBlackWeek1PriorSnapW", data = SnapData),
  "(5) Trimmed, snap-weighted" = list(y = "WinPct", rhs = c("ShareBlackSnapW", BaseSnap), fe = FE2,
                                      key = "ShareBlackSnapW", wcb = "ShareBlackSnapW",
                                      data = trim_top(SnapData, "ShareBlackSnapW")),
  "(6) Trimmed, headcount" = list(y = "WinPct", rhs = c("ShareBlackRoster", BaseHead), fe = FE2,
                                  key = "ShareBlackRoster", wcb = "ShareBlackRoster",
                                  data = trim_top(HeadData, "ShareBlackRoster")),
  # Game-script channel of the snap weights: the unit-balanced share (mean of
  # the offense and defense shares), a control for the defensive snap share,
  # and the offense and defense shares entered jointly
  "(7) Unit-balanced snap share" = list(y = "WinPct",
                                        rhs = c("ShareBlackBalancedSnapW", BaseSnap,
                                                if (IsPredMeasure) VarsSnap$unit_prior),
                                        fe = FE2, key = "ShareBlackBalancedSnapW",
                                        wcb = "ShareBlackBalancedSnapW", data = SnapData),
  "(8) Defensive snap share control" = list(y = "WinPct",
                                            rhs = c("ShareBlackSnapW", "DefenseSnapWeightShare",
                                                    BaseSnap),
                                            fe = FE2, key = "ShareBlackSnapW",
                                            wcb = "ShareBlackSnapW", data = SnapData),
  "(9) Offense and defense shares" = list(y = "WinPct",
                                          rhs = c("ShareBlackOffenseSnapW", "ShareBlackDefenseSnapW",
                                                  BaseSnap, if (IsPredMeasure) VarsSnap$unit_prior),
                                          fe = FE2, key = "ShareBlackOffenseSnapW",
                                          wcb = c("ShareBlackOffenseSnapW", "ShareBlackDefenseSnapW"),
                                          data = SnapData))
Data18b <- imap(Specs18b, \(s, nm) common_sample(s$data, list(s), paste("table-18b", nm)))
# The contemporaneous-controls column must be estimated on exactly the
# Table 15 franchise-seasons (its note says so)
if (nrow(Data18b[["(1) Contemp. controls"]]) != nrow(Sample15)) {
  stop("13: table-18b column (1) loses ", nrow(Sample15) - nrow(Data18b[["(1) Contemp. controls"]]),
       " of the Table 15 franchise-seasons to the contemporaneous controls")
}
Models18b <- imap(Specs18b, \(s, nm) fit_one(s, Data18b[[nm]]))
Wcb18b <- imap_dfr(Models18b, \(m, nm) wcb_specs(set_names(list(m), nm), Specs18b[nm], Data18b[[nm]]))

Rows18b <- bind_rows(
  diversity_rows(Models18b, Specs18b, Data18b, Wcb18b,
                 effect_label = glue("Effect of 1-SD change{EffectWord} (win pct. pts.)")),
  tibble(term = "WCB p-value, defense share",
         !!!map(set_names(names(Specs18b)), \(nm) wcb_text(Wcb18b, nm, "ShareBlackDefenseSnapW"))),
  tibble(term = "Seasons", !!!map(Data18b, \(d) paste0(min(d$season), "-", max(d$season)))),
  indicator_rows(Specs18b, list(
    "Season and franchise FE" = \(s) TRUE,
    "Lagged outcomes" = \(s) "LagWinPct" %in% s$rhs,
    "Opening-day quality, HC and QB" = \(s) "MeanLogPickWeek1" %in% s$rhs,
    "Prior-season cap share" = \(s) CapPre %in% s$rhs,
    "In-season quality, season HC and QB" = \(s) "HCBlack" %in% s$rhs,
    "Current-season cap share" = \(s) CapCont %in% s$rhs,
    "Calibration controls" = has_calib)))

CoefMap18b <- c(ShareBlackSnapW = glue("Roster share {ShareWord} (snap-weighted)"),
                ShareBlackRoster = glue("Roster share {ShareWord} (headcount)"),
                ShareBlackWeek1 = glue("Opening-day active-roster share {ShareWord}"),
                ShareBlackWeek1PriorSnapW = glue("Opening-day share {ShareWord}, prior-snap-weighted"),
                ShareBlackCoaches = glue("Coaches' share {ShareWord} (all snapshots)"),
                ShareBlackCoachesPre = glue("Opening-day coaches' share {ShareWord}"),
                HCBlack = glue("Season head coach {ShareWord} (most games)"),
                QBBlack = glue("Season starting QB {ShareWord} (most starts)"),
                HCWeek1Black = glue("Opening-day head coach {ShareWord}"),
                QBWeek1Black = glue("Opening-day starting QB {ShareWord}"),
                ShareBlackBalancedSnapW = glue("Unit-balanced snap-weighted share {ShareWord}"),
                ShareBlackOffenseSnapW = glue("Offense share {ShareWord} (snap-weighted)"),
                ShareBlackDefenseSnapW = glue("Defense share {ShareWord} (snap-weighted)"),
                DefenseSnapWeightShare = "Defensive share of snap weight")
# The 'Calibration controls' row is dropped outside the predicted measures
if (!IsPredMeasure) Rows18b <- filter(Rows18b, term != "Calibration controls")

write_model_table(
  Models18b, CoefMap18b,
  title = "Roster racial composition and win percentage: robustness",
  label = "roster-diversity-robustness",
  name = "table-18b-roster-diversity-robustness", measure = measure,
  add_rows = Rows18b, design = "team",
  notes = c(
    paste("This table includes the estimation results of equation (2) under alternative treatments,",
          "controls and samples. The unit of observation is a franchise-season; the dependent",
          "variable is the regular-season win percentage; all columns include season and franchise",
          "fixed effects."),
    paste("Column (1) replaces the opening-day controls of Table \\ref{tab:roster-diversity-main},",
          "column (4), with contemporaneous ones, on the same franchise-seasons: the season's own",
          "cap share (the sum of the season's cap numbers, which in-season signings, releases and",
          "restructures change) in place of the prior season's, snap-weighted roster quality, the",
          "season starting QB (most starts) and the season head coach (most games). These are set",
          "during the season and respond to injuries, benching, tanking and firings that also drive",
          "results, so they are possible bad controls. Column (2) drops the lagged outcomes",
          "(Nickell bias)."),
    paste("Columns (3)-(4) use predetermined treatments, fixed before the season's results arrive:",
          "the Black share of the active roster of the first regular-season game (column (3),",
          "2002-2025, without a cap share), and the same players weighted by their offensive plus",
          glue("defensive snaps in the previous season (column (4), {SnapSeasons13}, with the prior"),
          "season's cap share; players without prior-season snaps get zero weight). The",
          "snap-weighted share of Table \\ref{tab:roster-diversity-main} is measured during the",
          "season and is partly an outcome of it, so its coefficients are descriptive conditional",
          "associations; columns (3)-(4) are the estimates for a roster fixed before the season."),
    paste("Columns (5)-(6) drop, in each season, franchise-seasons above the league-season 99th",
          "percentile of the share (in practice the season's largest share), a guard against",
          "franchise-season spikes in the measured share.",
          if (measure == "provisional") paste(
            "Under the provisional measure such spikes arise when Wikipedia editors categorize one",
            "franchise-season's players more completely than others.")),
    paste("Columns (7)-(9) address the game-script channel of the snap weights (teams that trail",
          "run more offensive plays, teams with weak offenses play more defensive snaps, and",
          "defensive units have higher Black shares): column (7) uses the unit-balanced share, the",
          "mean of the offense and defense snap-weighted shares, which does not depend on the",
          "offense-defense snap balance; column (8) controls for the defensive share of the snap",
          "weight (itself an outcome of the season, so a possible bad control); column (9) enters",
          "the offense and defense shares jointly (the SD and effect rows refer to the offense",
          "share).", if (IsPredMeasure) "Columns (7) and (9) add the mean prior of each unit."),
    NoteControls,
    if (IsPredMeasure) paste(
      "Under the predicted measure every column includes the calibration controls of its",
      "treatment's roster group (mean prior and prior-covariate shares of the snap-weighted, headcount,",
      "opening-day or prior-snap-weighted opening-day roster) and the priors of the race controls",
      "(in column (1), of the season head coach and quarterback and of all listed coaches)."),
    paste("The SD row is the SD of the share net of franchise and season fixed effects; the effect",
          "row multiplies it by the coefficient and by 100 (win-percentage points)."),
    NoteCluster13, NoteMeasureTeam))

Estimates13[["table-18b"]] <- tidy_with_wcb(Models18b, names(CoefMap18b), Wcb18b, "table-18b")
message(glue("13: table 18b done at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))

# ---------------------------------------------------------------------------
# Table 28: the headline specifications under every race measure
# ---------------------------------------------------------------------------

# Panels: the primary measure first, then the sensitivity measures (hand
# codes only when they cover enough of the roster treatments), then the
# treatment variants of the predicted measure
Measures28 <- unique(c(getOption("nfl.race_primary", "predicted"),
                       "predicted", "preddoc", "provisional",
                       if (isTRUE(HandCoverage13 >= MinHandCoverage)) "hand"))
Panels28 <- bind_rows(
  tibble(measure = Measures28, variant = "none"),
  tibble(measure = "predicted", variant = names(Variants13))) |>
  mutate(panel = if_else(variant == "none", measure, paste0(measure, "-", variant)))

# The six headline columns under measure m: column (4) of Tables 15 and 15b
# and column (1) of Table 17, each without (columns (1), (3), (5)) and with
# (columns (2), (4), (6)) the mean prior and prior-covariate shares (plus,
# under the predicted measures, the staff and QB priors). Under the
# predicted measures the "with" columns are the headline specifications.
# The nodraft variant conditions on its own prior, without the draft shares.
cols28 <- function(m, variant) {
  dd <- variant == "nodraft"
  vs <- modifyList(VarsSnap, list(cov = cov_terms("SnapW", dd)))
  vh <- modifyList(VarsHead, list(cov = cov_terms("Roster", dd)))
  staff <- if (is_pred(m)) StaffPrePrior
  ts <- \(v, q, with) list(y = "WinPct",
                           rhs = c(v$share, Lags, q, StaffPre, if (with) c(v$prior, v$cov, staff)),
                           fe = FE2, key = v$share)
  gm <- \(with) list(y = "AtsMargin",
                     rhs = c(Share17, if (with) c("MeanPriorBlackActiveRoster",
                                                  cov_terms("ActiveRoster", dd))),
                     fe = FETS, key = Share17)
  list("(1)" = list(design = "snap", group = "SnapW", spec = ts(vs, QualitySnapPre, FALSE)),
       "(2)" = list(design = "snap", group = "SnapW", spec = ts(vs, QualitySnapPre, TRUE)),
       "(3)" = list(design = "head", group = "Roster", spec = ts(vh, QualityHeadPre, FALSE)),
       "(4)" = list(design = "head", group = "Roster", spec = ts(vh, QualityHeadPre, TRUE)),
       "(5)" = list(design = "game", group = "ActiveRoster", spec = gm(FALSE)),
       "(6)" = list(design = "game", group = "ActiveRoster", spec = gm(TRUE)))
}

# Berkson variance column of the expected share (predicted measures only;
# variants and the other measures have none). Not used for the game columns:
# the same players' errors are nearly constant within a team-season and are
# absorbed by the franchise x season fixed effects.
berkson_col <- function(m, variant, g) {
  if (variant != "none" || !is_pred(m) || g == "ActiveRoster") return(NA_character_)
  paste0("BerksonVar", RaceMeasureTags[[m]], g)
}

# Fit the six columns for one panel on the main tables' samples (the common
# samples of Tables 15, 15b and 17 under the panel's measure); returns one
# row per column with the coefficient, clustered SE and p-value, WCB
# p-value, SD of the share net of the column's FE, the implied SD of the
# true share (adding the mean Berkson variance) and N
run_panel28 <- function(m, variant, panel) {
  smp <- if (m == measure && variant == "none") Samples13 else build_samples13(m, variant)
  d28 <- list(
    snap = common_sample(smp$snap, ladder_specs(VarsSnap, QualitySnapPre, m),
                         glue("table-28 {panel} snap")),
    head = common_sample(smp$head, ladder_specs(VarsHead, QualityHeadPre, m),
                         glue("table-28 {panel} head")),
    game = common_sample(smp$game, c(specs17(m)[Stacked17], list(cols28(m, variant)[["(6)"]]$spec)),
                         glue("table-28 {panel} game")))
  imap_dfr(cols28(m, variant), \(cl, nm) {
    s <- cl$spec
    d <- d28[[cl$design]]
    mod <- fit_one(s, d)
    wcb <- tryCatch(
      if (cl$design == "game") wcb_one_fe(mod, d, s$key, "TeamSeasonId") else
        wild_cluster_test(mod, d, s$key, "franchise_id", B = WcbReps),
      error = \(e) {
        message(glue("13: table-28 WCB failed for {panel} {nm}: {conditionMessage(e)}"))
        tibble(p_wcb = NA_real_, ci_low = NA_real_, ci_high = NA_real_)
      })
    de <- d[obs(mod), ]
    ct <- coeftable(mod)
    sd_fe <- fe_resid_sd(de[[s$key]], de, s$fe)
    bcol <- berkson_col(m, variant, cl$group)
    berk <- if (is.na(bcol)) NA_real_ else mean(de[[bcol]])
    tibble(panel = panel, panel_measure = m, variant = variant, model = nm, design = cl$design,
           term = s$key, estimate = ct[s$key, 1], std_error = ct[s$key, 2],
           p_value = ct[s$key, 4], p_wcb = wcb$p_wcb, wcb_ci_low = wcb$ci_low,
           wcb_ci_high = wcb$ci_high, sd_net_fe = sd_fe, berkson_var = berk,
           sd_true_implied = sqrt(sd_fe^2 + berk), mean_share = mean(de[[s$key]]),
           calibration_controls = any(str_starts(s$rhs, "ShareCollege")),
           nobs = nobs(mod), dep_var = s$y)
  })
}
Res28 <- pmap_dfr(Panels28, \(measure, variant, panel) run_panel28(measure, variant, panel))
message(glue("13: table 28 estimated at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))

# Stars from the franchise-clustered p-value, as in the other tables
stars28 <- function(p) {
  case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
}

# Panel label of each panel
MeasureLabels28 <- c(
  predicted = "Predicted race, model only",
  preddoc = "Predicted race, documented where stated",
  provisional = "Provisional (Wikipedia category flag)",
  hand = "Hand-coded race",
  "predicted-nodraft" = "Predicted race, prior without draft round",
  "predicted-multi" = "Predicted race, Black or multiracial",
  "predicted-raked" = "Predicted race, raked to TIDES margins")
panel_label28 <- function(panel, m, variant) {
  paste0(MeasureLabels28[[panel]],
         if (variant == "none" && is_primary_measure(m)) " (primary)" else "")
}

# One panel per measure or variant: coefficient (stars), SE, WCB p-value, SD
# of the expected share net of FE and the effect of a one-SD change in it,
# the implied SD of the true share (predicted measures) and the effect of a
# one-SD change in it (win-pct. points for the team-season columns, points
# for the game columns), mean share and N
Body28 <- map_dfr(Panels28$panel, \(pn) {
  r <- filter(Res28, panel == pn) |> arrange(model)
  scale <- if_else(r$design == "game", 1, 100)
  pred_panel <- is_pred(r$panel_measure[1])
  cells <- list(
    "Roster share Black" = paste0(fmt_num(r$estimate, 3), stars28(r$p_value)),
    " " = paste0("(", fmt_num(r$std_error, 3), ")"),
    "WCB p-value" = paste0("[", fmt_p(r$p_wcb), "]"),
    "Mean share" = fmt_num(r$mean_share, 3),
    "SD of share net of FE" = fmt_num(r$sd_net_fe, 3),
    "Effect of 1-SD change in share" = fmt_num(r$estimate * r$sd_net_fe * scale, 2),
    "Implied SD of true share (upper bound)" = fmt_num(r$sd_true_implied, 3),
    "Effect of 1-SD change in true share" = fmt_num(r$estimate * r$sd_true_implied * scale, 2),
    "Observations" = formatC(r$nobs, format = "d", big.mark = ","))
  imap_dfr(cells, \(x, lab) tibble(panel = pn, term = lab, !!!set_names(as.list(x), r$model)))
})
RowsPerPanel28 <- n_distinct(Body28$term)

# Specification rows shared by all panels
Foot28 <- tibble(
  term = c("Unit of observation", "Seasons", "Share", "Dependent variable",
           "Franchise and season FE", "Franchise $\\times$ season FE",
           "Lags, opening-day quality, staff and QB",
           "Mean prior and prior-covariate shares"),
  "(1)" = c("Team-season", SnapSeasons13, "Snap-weighted", "Win pct.", "Yes", "No", "Yes", "No"),
  "(2)" = c("Team-season", SnapSeasons13, "Snap-weighted", "Win pct.", "Yes", "No", "Yes", "Yes"),
  "(3)" = c("Team-season", "2002-2025", "Headcount", "Win pct.", "Yes", "No", "Yes", "No"),
  "(4)" = c("Team-season", "2002-2025", "Headcount", "Win pct.", "Yes", "No", "Yes", "Yes"),
  "(5)" = c("Team-game", "2002-2025", "Active roster", "ATS margin", "No", "Yes", "No", "No"),
  "(6)" = c("Team-game", "2002-2025", "Active roster", "ATS margin", "No", "Yes", "No", "Yes"))

Table28 <- bind_rows(select(Body28, -panel), Foot28)
PanelSizes28 <- set_names(rep(RowsPerPanel28, nrow(Panels28)),
                          paste0("Panel ", LETTERS[seq_len(nrow(Panels28))], ": ",
                                 pmap_chr(Panels28, \(measure, variant, panel)
                                   panel_label28(panel, measure, variant))))

Tab28 <- kbl(Table28, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
             align = paste0("l", strrep("c", 6)),
             col.names = c("", paste0("(", 1:6, ")")),
             caption = paste0("Roster racial composition and team performance: headline estimates by race measure",
                              if (!is_primary_measure(measure)) glue(" ({measure} race measure run)"),
                              " \\label{tab:roster-diversity-race-measures}")) |>
  add_header_above(c(" " = 1, "Win pct., snap-weighted" = 2, "Win pct., headcount" = 2,
                     "ATS margin, game level" = 2)) |>
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8)
Start28 <- 1L
for (i in seq_along(PanelSizes28)) {
  Tab28 <- pack_rows(Tab28, names(PanelSizes28)[i], Start28, Start28 + PanelSizes28[[i]] - 1L,
                     escape = FALSE, latex_gap_space = "0pt")
  Start28 <- Start28 + PanelSizes28[[i]]
}
Tab28 <- row_spec(Tab28, Start28 - 1L, hline_after = TRUE) |>
  add_notes(c(
    paste("This table reports the headline estimates of equations (2) and (3) under each race",
          "measure and under variants of the predicted measure. Columns (1)-(4) use the",
          "specification of column (4) of Tables \\ref{tab:roster-diversity-main} and",
          "\\ref{tab:roster-diversity-main-headcount} (season and franchise fixed effects, lagged",
          "outcomes, opening-day roster quality, the prior season's cap share in the snap-weighted",
          "columns, and the staff and quarterback controls), without",
          "(columns (1) and (3)) and with (columns (2) and (4)) the mean prior P(Black) and the",
          "weighted shares of the prior's draft-round, college-type, rookie-era and hometown-county",
          "levels; in the predicted panels the latter columns also include the priors of the",
          "coaches, head coach and quarterback and are the headline specifications. Columns (5)-(6)",
          "are column (1) of Table \\ref{tab:roster-diversity-game} (the margin against the closing",
          "spread, with franchise $\\times$ season fixed effects), without and with the active",
          "roster's mean prior and prior-covariate shares. Each panel re-estimates the columns on",
          "the main tables' samples with the treatment and the staff and quarterback race controls",
          "measured under that panel's race measure."),
    paste("Predicted race: each player's probability of being non-Hispanic Black combines first",
          "name, surname and hometown county (BIFSG) with an NFL-specific prior estimated on",
          "predetermined characteristics; team shares are expected shares (mean member",
          "probability), and in the predicted panels the mean prior and prior-covariate shares are",
          "the regression-calibration controls (notes/race-prediction-design.md); in the other",
          "panels they are composition controls. The model-only measure counts multiracial and",
          "Hispanic Black players as non-Black, while the documented, provisional and hand-coded",
          "measures count them as Black, so the panels differ in the estimand as well as in",
          "measurement. Predicted race documented where stated replaces the prediction by",
          "documented race where a public source states it; documentation depends on fame, so its",
          "error may be correlated with success (its calibration controls use the documented",
          "variant's prior for the roster and the model-only prior for staff and quarterbacks).",
          "The provisional measure is the positive-only Wikipedia category flag (hand code where",
          "available), whose team shares are lower bounds that vary with Wikipedia editing.",
          if ("hand" %in% Measures28) "Hand-coded race follows notes/race-coding-protocol.md.",
          "The variant panels replace the model-only treatment by the expected share under the",
          "prior without draft round (which does not proxy draft capital; its calibration controls",
          "omit the draft shares), by the expected share Black or multiracial (an upper bound for",
          "Black alone or in combination), and by the expected share raked to the TIDES league",
          "margins (a calibration sensitivity); the staff and quarterback controls stay model-only."),
    paste("Under regression calibration the coefficient on an expected share is in units of the",
          "true share if the probabilities are calibrated at the team level given the controls; the",
          "error of an expected share is then of the Berkson type, and the SD of the true share",
          "exceeds the SD of the expected share. The implied SD of the true share adds to the SD",
          "of the expected share net of the fixed effects the mean Berkson variance",
          "$\\sum_i w_i^2 p_i(1-p_i)/(\\sum_i w_i)^2$ (players' races independent given the",
          "predictions); it is reported for the team-season columns of the model-only and",
          "documented panels. It is an upper bound: the errors of players who stay with a",
          "franchise persist across seasons and are partly absorbed by the franchise fixed effects",
          "(in the game columns they are nearly constant within a team-season, so no implied SD is",
          "reported there). The provisional",
          "measure's error is not classical (the flag is positive-only and its coverage varies",
          "with Wikipedia editing), so neither its coefficient nor its one-SD effect estimates the",
          "same object.", NoteCalibCheck),
    paste("Standard errors, in parentheses, are clustered at the franchise level (32 clusters);",
          "stars refer to them (*** p$<$0.01, ** p$<$0.05, * p$<$0.1). Wild cluster bootstrap-t",
          glue("p-values in brackets use Webb weights and {formatC(WcbReps, big.mark = ',', format = 'd')}"),
          "replications; in columns (5)-(6) the team-season fixed effects are projected out first."),
    NoteIdent, NotePlacebo))
save_exhibit_tex(Tab28, "table-28-roster-diversity-race-measures", measure)
Estimates13[["table-28"]] <- Res28 |> mutate(table = "table-28", .before = 1)
Estimates13[["calibration-check"]] <- CalibCheck13 |>
  transmute(table = "calibration-check", model = fe, term = glue("DocMeanPBlack{group}"),
            estimate, std_error, nobs, dep_var = glue("ShareBlackDoc{group}"),
            mean_doc_share, mean_doc_black)
message(glue("13: table 28 done at {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))

# ---------------------------------------------------------------------------
# Tidy estimates
# ---------------------------------------------------------------------------

save_estimates(bind_rows(Estimates13), "13-roster-diversity", measure)
message(glue("13: done in {round(difftime(Sys.time(), T0Script13, units = 'secs'))}s"))

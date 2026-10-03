# ============================================================================
# 11-team-analysis-sample.R
# Builds the team-level estimation samples for the roster- and staff-diversity
# designs (notes/analysis-plan.md, sections 2-3):
#   - analysis/analysis_team_season: team_season (all 861 franchise-seasons,
#     1999-2025) + roster_composition_team_season (roster measures NA before
#     2002) + one-season leads (F1*) of roster and staff composition (union
#     and opening-snapshot Pre measures) for the placebo tests, one-season
#     lags (L1*) of the roster shares (and of the roster calibration
#     controls: preddoc mean prior, prior-covariate shares) and of the
#     opening-snapshot coaches' shares and turnover, the opening-day head
#     coach's race + outcome transforms (higher = better, changes).
#   - analysis/analysis_team_unit_season: two rows per franchise-season
#     (Unit = offense, defense) for the stacked unit design (A3): unit
#     outcomes oriented so that higher is better, the unit coordinator's race
#     (season holder, and the opening-snapshot holder with suffix Pre), unit
#     coaches' (union and opening Pre) and unit roster Black shares, unit
#     roster quality, and the opening-day head coach and incumbent spell.
#   - analysis/analysis_team_game: team_game (REG + POST) +
#     roster_composition_team_game for the team and the opponent (Opp*),
#     the ATS margin and the season starting QB's race.
# Hand-coded derived shares that apply_race_measure() does not coverage-gate
# (names not starting with ShareBlackHand: F1*, Opp*, Expected*, Residual*)
# are set to NA here when their coverage is below MinHandCoverage, so that
# every *BlackHand* share in these samples is gated the same way. The
# predicted measures (*BlackPred*, *BlackPredDoc*, MeanPriorBlackPred*) need
# no gating: every person has a prediction.
# Inputs: analysis/team_season (02), analysis/team_game (03),
#   analysis/roster_composition_team_season and _team_game (09).
# Outputs: analysis/analysis_team_season, analysis/analysis_team_unit_season,
#   analysis/analysis_team_game (.parquet, .csv, codebook).
# Date: 2026-10-02 (predicted race added the same day); opening-snapshot
# leads/lags and unit fields 2026-10-03
# ============================================================================

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

TeamSeasonBase <- read_parquet(file.path(analysis, "team_season.parquet"))
RosterSeason <- read_parquet(file.path(analysis, "roster_composition_team_season.parquet"))
TeamGameBase <- read_parquet(file.path(analysis, "team_game.parquet"))
RosterGame <- read_parquet(file.path(analysis, "roster_composition_team_game.parquet"))
TeamSeasonCodebook <- read_csv(file.path(analysis, "codebook_team_season.csv"),
                               show_col_types = FALSE)
TeamGameCodebook <- read_csv(file.path(analysis, "codebook_team_game.csv"),
                             show_col_types = FALSE)
RosterSeasonCodebook <- read_csv(file.path(analysis, "codebook_roster_composition_team_season.csv"),
                                 show_col_types = FALSE)
RosterGameCodebook <- read_csv(file.path(analysis, "codebook_roster_composition_team_game.csv"),
                               show_col_types = FALSE)
SeasonKeys <- c("franchise_id", "season")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Lead of x within a group ordered by `time`, requiring consecutive time (NA
# when time + 1 is not observed). Use inside group_by().
lead_within <- function(x, time) {
  ord <- order(time)
  out <- rep(x[NA_integer_], length(x))
  led <- dplyr::lead(x[ord])
  lead_time <- dplyr::lead(time[ord])
  led[is.na(lead_time) | lead_time - time[ord] != 1] <- NA
  out[ord] <- led
  out
}

# Set a hand-coded share to NA when its coverage column is below
# MinHandCoverage (the rule apply_race_measure() applies to ShareBlackHand*)
gate_hand <- function(df, share_col, coverage_col) {
  df[[share_col]] <- if_else(coalesce(df[[coverage_col]], 0) >= MinHandCoverage,
                             df[[share_col]], NA_real_)
  df
}

# Stop when the generic names apply_race_measure() creates collide with each
# other or with existing columns
# ("BlackPred" excludes "BlackPredDoc", as in apply_race_measure())
check_race_names <- function(df, name) {
  for (tag in c("Hand", "Prov", "Pred", "PredDoc")) {
    pattern <- paste0("Black", tag, if (tag == "Pred") "(?!Doc)" else "")
    src <- grep(pattern, names(df), value = TRUE, perl = TRUE)
    generic <- sub(pattern, "Black", src, perl = TRUE)
    clash <- generic[duplicated(generic) | generic %in% names(df)]
    if (length(clash) > 0) {
      stop(name, ": apply_race_measure() name collision for ", tag, ": ",
           paste(clash, collapse = ", "))
    }
  }
  invisible(TRUE)
}

# Labels of an input codebook as a named vector
codebook_labels <- function(cb) set_names(cb$label, cb$variable)

# ---------------------------------------------------------------------------
# (c) analysis_team_season
# ---------------------------------------------------------------------------

# Union groups, then the opening-snapshot (Pre) groups of 02; every lead
# variable must exist in team_season
LeadGroups <- c("Roster", "SnapW", "Coaches", "Coordinators", "PositionCoaches",
                "FrontOffice", "CoachesPre", "CoordinatorsPre", "PositionCoachesPre",
                "FrontOfficePre")
ShareMeasures <- c("ShareBlackHand", "ShareBlackProv", "CodedShare", "ShareBlackPred",
                   "ShareBlackPredDoc", "MeanPriorBlackPred")
RoleMeasures <- c("BlackHand", "BlackProv", "BlackPred", "BlackPredDoc", "PriorBlackPred")
LeadRoles <- c("HC", "OC", "DC", "GM")
LeadVars <- c(paste0(rep(ShareMeasures, each = length(LeadGroups)), LeadGroups),
              paste0(rep(LeadRoles, each = length(RoleMeasures)), RoleMeasures),
              paste0(rep(LeadRoles, each = length(RoleMeasures)), RoleMeasures, "Pre"))

# One-season lags (L1*) of the roster shares, for the reverse-causality test
# (composition on past performance, conditional on past composition), and of
# the opening-snapshot coaches' shares and turnover (lagged opening staff is
# predetermined with respect to season t - 1 results as well)
LagGroups <- c("Roster", "SnapW")
LagStaffGroups <- "CoachesPre"
LagVars <- c(paste0(rep(ShareMeasures, each = length(LagGroups)), LagGroups),
             paste0(ShareMeasures, LagStaffGroups),
             "ShareCoachesNewToFranchisePre", "ShareCoachesPromotedPre")

# Leads and lags of the roster calibration controls (preddoc mean prior and
# the shares of the prior's covariate levels, 09) for the placebo and
# reverse-causality tests under the predicted measures
CovShareVars <- grep("^Share(Draft|College|Era|County)[A-Za-z0-9]*Roster$", names(RosterSeason),
                     value = TRUE) |> str_remove("Roster$")
RosterCalibVars <- paste0(rep(c("MeanPriorBlackPredDoc", CovShareVars), each = length(LagGroups)),
                          LagGroups)
stopifnot(length(CovShareVars) == 12, all(RosterCalibVars %in% names(RosterSeason)))
LeadVars <- c(LeadVars, RosterCalibVars)
LagVars <- c(LagVars, RosterCalibVars)
StaffLeadLag <- setdiff(c(LeadVars, LagVars), c(names(RosterSeason)))
if (!all(StaffLeadLag %in% names(TeamSeasonBase))) {
  stop("Lead/lag variables missing from team_season: ",
       paste(setdiff(StaffLeadLag, names(TeamSeasonBase)), collapse = ", "))
}

# Opening-day head coach: the head coach of the franchise's first REG game
# (team_game; predetermined with respect to in-season firings, unlike the
# season HC, who coached the most REG games). HCPersonIdPre (02) is the
# Wikipedia opening-snapshot listing, HCIncumbentSpellId his franchise run.
HCWeek1 <- TeamGameBase |>
  filter(game_type == "REG") |>
  group_by(franchise_id, season) |>
  slice_min(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(franchise_id, season, HCWeek1BlackHand = HCBlackHand, HCWeek1BlackProv = HCBlackProv,
         HCWeek1BlackPred = HCBlackPred, HCWeek1BlackPredDoc = HCBlackPredDoc,
         HCWeek1PriorBlackPred = HCPriorBlackPred)

AnalysisTeamSeason <- TeamSeasonBase |>
  left_join(RosterSeason, by = SeasonKeys) |>
  left_join(HCWeek1, by = SeasonKeys) |>
  group_by(franchise_id) |>
  mutate(across(all_of(LeadVars), \(x) lead_within(x, season), .names = "F1{.col}"),
         across(all_of(LagVars), \(x) lag_within(x, season), .names = "L1{.col}")) |>
  ungroup() |>
  mutate(NegDefEPAPerPlay = -DefEPAPerPlay,
         NegDefSuccessRate = -DefSuccessRate,
         DeltaWinPct = WinPct - LagWinPct,
         DeltaWinsOverExpected = WinsOverExpected - LagWinsOverExpected) |>
  arrange(franchise_id, season)

# Coverage-gate the hand-coded leads and expected/residual roster shares
for (g in LeadGroups) {
  AnalysisTeamSeason <- gate_hand(AnalysisTeamSeason, glue("F1ShareBlackHand{g}"),
                                  glue("F1CodedShare{g}"))
}
for (g in c(LagGroups, LagStaffGroups)) {
  AnalysisTeamSeason <- gate_hand(AnalysisTeamSeason, glue("L1ShareBlackHand{g}"),
                                  glue("L1CodedShare{g}"))
}
ExpectedGroups <- c("Roster", "SnapW", "Offense", "Defense", "OffenseSnapW", "DefenseSnapW")
for (g in ExpectedGroups) {
  for (v in c("ExpectedShareBlackHand", "ResidualShareBlackHand")) {
    AnalysisTeamSeason <- gate_hand(AnalysisTeamSeason, glue("{v}{g}"), glue("CodedShare{g}"))
  }
}

if (nrow(AnalysisTeamSeason) != nrow(TeamSeasonBase)) {
  stop("analysis_team_season has ", nrow(AnalysisTeamSeason), " rows; team_season has ",
       nrow(TeamSeasonBase))
}
check_race_names(AnalysisTeamSeason, "analysis_team_season")

# ---------------------------------------------------------------------------
# (d) analysis_team_unit_season: offense and defense rows
# ---------------------------------------------------------------------------

# One unit's rows: `p` maps each generic unit variable to its source column
# in AnalysisTeamSeason for that unit; `sign` orients outcomes so that higher
# is better (-1 for defensive outcomes, which are measured as allowed)
unit_rows <- function(unit, p, sign) {
  AnalysisTeamSeason |>
    transmute(franchise_id, season, Unit = unit, team_code,
              UnitEPAPerPlay = sign * .data[[p$EPA]],
              UnitSuccessRate = sign * .data[[p$Success]],
              UnitPassEPAPerPlay = sign * .data[[p$PassEPA]],
              UnitRushEPAPerPlay = sign * .data[[p$RushEPA]],
              LagUnitEPAPerPlay = sign * .data[[paste0("Lag", p$EPA)]],
              LagUnitSuccessRate = sign * .data[[paste0("Lag", p$Success)]],
              UnitCoordPersonId = .data[[paste0(p$Coord, "PersonId")]],
              UnitCoordBlackHand = .data[[paste0(p$Coord, "BlackHand")]],
              UnitCoordBlackProv = .data[[paste0(p$Coord, "BlackProv")]],
              UnitCoordPBlackBifsg = .data[[paste0(p$Coord, "PBlackBifsg")]],
              UnitCoordBlackPred = .data[[paste0(p$Coord, "BlackPred")]],
              UnitCoordBlackPredDoc = .data[[paste0(p$Coord, "BlackPredDoc")]],
              UnitCoordPriorBlackPred = .data[[paste0(p$Coord, "PriorBlackPred")]],
              UnitCoordChange = .data[[paste0(p$Coord, "Change")]],
              # Opening-snapshot (Pre) coordinator and unit coaches (02)
              UnitCoordPersonIdPre = .data[[paste0(p$Coord, "PersonIdPre")]],
              UnitCoordBlackHandPre = .data[[paste0(p$Coord, "BlackHandPre")]],
              UnitCoordBlackProvPre = .data[[paste0(p$Coord, "BlackProvPre")]],
              UnitCoordPBlackBifsgPre = .data[[paste0(p$Coord, "PBlackBifsgPre")]],
              UnitCoordBlackPredPre = .data[[paste0(p$Coord, "BlackPredPre")]],
              UnitCoordBlackPredDocPre = .data[[paste0(p$Coord, "BlackPredDocPre")]],
              UnitCoordPriorBlackPredPre = .data[[paste0(p$Coord, "PriorBlackPredPre")]],
              UnitCoordChangePre = .data[[paste0(p$Coord, "ChangePre")]],
              NUnitCoordPre = .data[[paste0("N", p$Coord, "Pre")]],
              NUnitCoachesPre = .data[[paste0("N", p$Coaches, "Pre")]],
              CodedShareUnitCoachesPre = .data[[paste0("CodedShare", p$Coaches, "Pre")]],
              ShareBlackHandUnitCoachesPre = .data[[paste0("ShareBlackHand", p$Coaches, "Pre")]],
              ShareBlackProvUnitCoachesPre = .data[[paste0("ShareBlackProv", p$Coaches, "Pre")]],
              MeanPBlackBifsgUnitCoachesPre = .data[[paste0("MeanPBlackBifsg", p$Coaches, "Pre")]],
              ShareBlackPredUnitCoachesPre = .data[[paste0("ShareBlackPred", p$Coaches, "Pre")]],
              ShareBlackPredDocUnitCoachesPre = .data[[paste0("ShareBlackPredDoc", p$Coaches, "Pre")]],
              MeanPriorBlackPredUnitCoachesPre = .data[[paste0("MeanPriorBlackPred", p$Coaches, "Pre")]],
              NUnitCoaches = .data[[paste0("N", p$Coaches)]],
              CodedShareUnitCoaches = .data[[paste0("CodedShare", p$Coaches)]],
              ShareBlackHandUnitCoaches = .data[[paste0("ShareBlackHand", p$Coaches)]],
              ShareBlackProvUnitCoaches = .data[[paste0("ShareBlackProv", p$Coaches)]],
              MeanPBlackBifsgUnitCoaches = .data[[paste0("MeanPBlackBifsg", p$Coaches)]],
              ShareBlackPredUnitCoaches = .data[[paste0("ShareBlackPred", p$Coaches)]],
              ShareBlackPredDocUnitCoaches = .data[[paste0("ShareBlackPredDoc", p$Coaches)]],
              MeanPriorBlackPredUnitCoaches = .data[[paste0("MeanPriorBlackPred", p$Coaches)]],
              NUnitRoster = .data[[paste0("N", p$Roster)]],
              CodedShareUnitRoster = .data[[paste0("CodedShare", p$Roster)]],
              ShareBlackHandUnitRoster = .data[[paste0("ShareBlackHand", p$Roster)]],
              ShareBlackProvUnitRoster = .data[[paste0("ShareBlackProv", p$Roster)]],
              ExpectedShareBlackHandUnitRoster = .data[[paste0("ExpectedShareBlackHand", p$Roster)]],
              ExpectedShareBlackProvUnitRoster = .data[[paste0("ExpectedShareBlackProv", p$Roster)]],
              ResidualShareBlackHandUnitRoster = .data[[paste0("ResidualShareBlackHand", p$Roster)]],
              ResidualShareBlackProvUnitRoster = .data[[paste0("ResidualShareBlackProv", p$Roster)]],
              ShareBlackPredUnitRoster = .data[[paste0("ShareBlackPred", p$Roster)]],
              ShareBlackPredDocUnitRoster = .data[[paste0("ShareBlackPredDoc", p$Roster)]],
              MeanPriorBlackPredUnitRoster = .data[[paste0("MeanPriorBlackPred", p$Roster)]],
              ExpectedShareBlackPredUnitRoster = .data[[paste0("ExpectedShareBlackPred", p$Roster)]],
              ExpectedShareBlackPredDocUnitRoster = .data[[paste0("ExpectedShareBlackPredDoc", p$Roster)]],
              ResidualShareBlackPredUnitRoster = .data[[paste0("ResidualShareBlackPred", p$Roster)]],
              ResidualShareBlackPredDocUnitRoster = .data[[paste0("ResidualShareBlackPredDoc", p$Roster)]],
              CodedShareUnitRosterSnapW = .data[[paste0("CodedShare", p$Roster, "SnapW")]],
              ShareBlackHandUnitRosterSnapW = .data[[paste0("ShareBlackHand", p$Roster, "SnapW")]],
              ShareBlackProvUnitRosterSnapW = .data[[paste0("ShareBlackProv", p$Roster, "SnapW")]],
              ExpectedShareBlackHandUnitRosterSnapW = .data[[paste0("ExpectedShareBlackHand", p$Roster, "SnapW")]],
              ExpectedShareBlackProvUnitRosterSnapW = .data[[paste0("ExpectedShareBlackProv", p$Roster, "SnapW")]],
              ResidualShareBlackHandUnitRosterSnapW = .data[[paste0("ResidualShareBlackHand", p$Roster, "SnapW")]],
              ResidualShareBlackProvUnitRosterSnapW = .data[[paste0("ResidualShareBlackProv", p$Roster, "SnapW")]],
              ShareBlackPredUnitRosterSnapW = .data[[paste0("ShareBlackPred", p$Roster, "SnapW")]],
              ShareBlackPredDocUnitRosterSnapW = .data[[paste0("ShareBlackPredDoc", p$Roster, "SnapW")]],
              MeanPriorBlackPredUnitRosterSnapW = .data[[paste0("MeanPriorBlackPred", p$Roster, "SnapW")]],
              ExpectedShareBlackPredUnitRosterSnapW = .data[[paste0("ExpectedShareBlackPred", p$Roster, "SnapW")]],
              ExpectedShareBlackPredDocUnitRosterSnapW = .data[[paste0("ExpectedShareBlackPredDoc", p$Roster, "SnapW")]],
              ResidualShareBlackPredUnitRosterSnapW = .data[[paste0("ResidualShareBlackPred", p$Roster, "SnapW")]],
              ResidualShareBlackPredDocUnitRosterSnapW = .data[[paste0("ResidualShareBlackPredDoc", p$Roster, "SnapW")]],
              UnitCapShare = .data[[paste0(p$Roster, "CapShare")]],
              UnitMeanLogPickRoster = .data[[paste0(p$Roster, "MeanLogPickRoster")]],
              UnitMeanLogPickSnapW = .data[[paste0(p$Roster, "MeanLogPickSnapW")]],
              UnitMeanAgeRoster = .data[[paste0(p$Roster, "MeanAgeRoster")]],
              UnitMeanAgeSnapW = .data[[paste0(p$Roster, "MeanAgeSnapW")]],
              HCPersonId, HCBlackHand, HCBlackProv, HCPBlackBifsg, HCBlackPred, HCBlackPredDoc,
              HCPriorBlackPred,
              HCPersonIdPre, HCBlackHandPre, HCBlackProvPre, HCPBlackBifsgPre, HCBlackPredPre,
              HCBlackPredDocPre, HCPriorBlackPredPre, HCIncumbentKey, HCFirstGameKey,
              HCIncumbentSpellId,
              HCWeek1BlackHand, HCWeek1BlackProv, HCWeek1BlackPred, HCWeek1BlackPredDoc,
              HCWeek1PriorBlackPred,
              FullStaffObserved, OpeningStaffObserved, StaffSource, WinPct, LagWinPct,
              LagExpectedWins)
}

AnalysisTeamUnitSeason <- bind_rows(
  unit_rows("offense", list(EPA = "OffEPAPerPlay", Success = "OffSuccessRate",
                            PassEPA = "OffPassEPAPerPlay", RushEPA = "OffRushEPAPerPlay",
                            Coord = "OC", Coaches = "OffenseCoaches", Roster = "Offense"), 1),
  unit_rows("defense", list(EPA = "DefEPAPerPlay", Success = "DefSuccessRate",
                            PassEPA = "DefPassEPAPerPlay", RushEPA = "DefRushEPAPerPlay",
                            Coord = "DC", Coaches = "DefenseCoaches", Roster = "Defense"), -1)
) |>
  mutate(UnitDefense = as.integer(Unit == "defense")) |>
  arrange(franchise_id, season, Unit)

if (nrow(AnalysisTeamUnitSeason) != 2 * nrow(AnalysisTeamSeason)) {
  stop("analysis_team_unit_season should have two rows per franchise-season")
}
check_race_names(AnalysisTeamUnitSeason, "analysis_team_unit_season")

# ---------------------------------------------------------------------------
# (e) analysis_team_game: own and opponent game-day roster composition
# ---------------------------------------------------------------------------

RosterGameOwn <- RosterGame |> select(-season, -week)
RosterGameOpp <- RosterGameOwn |>
  rename_with(\(x) paste0("Opp", x), -c(franchise_id, game_id)) |>
  rename(opponent_franchise_id = franchise_id)

SeasonQBRace <- AnalysisTeamSeason |>
  select(franchise_id, season, StartingQBId, QBStartShare, QBBlackHand, QBBlackProv,
         QBPBlackBifsg, QBBlackPred, QBBlackPredDoc, QBPriorBlackPred)

AnalysisTeamGame <- TeamGameBase |>
  left_join(RosterGameOwn, by = c("franchise_id", "game_id")) |>
  left_join(RosterGameOpp, by = c("opponent_franchise_id", "game_id")) |>
  left_join(SeasonQBRace, by = SeasonKeys) |>
  mutate(AtsMargin = margin - team_spread_line)

# Coverage-gate the hand-coded expected/residual and opponent shares
for (p in c("", "Opp")) {
  for (v in c("ExpectedShareBlackHand", "ResidualShareBlackHand")) {
    AnalysisTeamGame <- gate_hand(AnalysisTeamGame, glue("{p}{v}ActiveRoster"),
                                  glue("{p}CodedShareActiveRoster"))
  }
}
for (g in c("ActiveRoster", "ActiveOffense", "ActiveDefense", "GameDayRoster")) {
  AnalysisTeamGame <- gate_hand(AnalysisTeamGame, glue("OppShareBlackHand{g}"),
                                glue("OppCodedShare{g}"))
}

if (nrow(AnalysisTeamGame) != nrow(TeamGameBase)) {
  stop("analysis_team_game has ", nrow(AnalysisTeamGame), " rows; team_game has ",
       nrow(TeamGameBase))
}
check_race_names(AnalysisTeamGame, "analysis_team_game")

# Opponent columns mirror the opponent's own row of the same game
OppCheck <- AnalysisTeamGame |>
  filter(game_type == "REG") |>
  select(game_id, franchise_id, opponent_franchise_id, OppShareBlackProvActiveRoster) |>
  inner_join(AnalysisTeamGame |>
               select(game_id, opponent_franchise_id = franchise_id,
                      OwnShare = ShareBlackProvActiveRoster),
             by = c("game_id", "opponent_franchise_id"))
if (!isTRUE(all.equal(OppCheck$OppShareBlackProvActiveRoster, OppCheck$OwnShare))) {
  stop("Opponent roster shares do not match the opponent's own rows")
}

# Market sign: the slope of margin on team_spread_line (positive = favored)
SpreadFit <- feols(margin ~ team_spread_line, data = AnalysisTeamGame)
message("Slope of margin on team_spread_line: ",
        round(coef(SpreadFit)[["team_spread_line"]], 3), " (SE ",
        round(se(SpreadFit)[["team_spread_line"]], 3), "; N = ", nobs(SpreadFit), ")")
if (abs(coef(SpreadFit)[["team_spread_line"]] - 1) > 0.15) {
  stop("team_spread_line sign or scale is off: slope of margin on it is ",
       round(coef(SpreadFit)[["team_spread_line"]], 3))
}

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

SeasonInputLabels <- c(codebook_labels(TeamSeasonCodebook), codebook_labels(RosterSeasonCodebook))
gate_note <- glue(" Set to NA here when the matching coverage is below {MinHandCoverage} (MinHandCoverage at build time).")
F1Labels <- set_names(paste0("One-season lead (season + 1; NA when season + 1 is not observed for the franchise) of ",
                             LeadVars, ": ", SeasonInputLabels[LeadVars]),
                      paste0("F1", LeadVars))
F1Hand <- paste0("F1ShareBlackHand", LeadGroups)
F1Labels[F1Hand] <- paste0(F1Labels[F1Hand], gate_note, " (coverage F1CodedShare)")
HandDerived <- c(outer(c("ExpectedShareBlackHand", "ResidualShareBlackHand"), ExpectedGroups, paste0))
SeasonInputLabels[HandDerived] <- paste0(SeasonInputLabels[HandDerived], ".", gate_note,
                                         " (coverage CodedShare of the same group)")

L1Labels <- set_names(paste0("One-season lag (season - 1; NA when season - 1 is not observed for the franchise) of ",
                             LagVars, ": ", SeasonInputLabels[LagVars]),
                      paste0("L1", LagVars))
L1Hand <- paste0("L1ShareBlackHand", LagGroups)
L1Labels[L1Hand] <- paste0(L1Labels[L1Hand], gate_note, " (coverage L1CodedShare)")

AnalysisTeamSeasonLabels <- c(
  SeasonInputLabels,
  F1Labels,
  L1Labels,
  HCWeek1BlackHand = "Opening-day head coach (in force at the franchise's first REG game, team_game staff snapshot) hand-coded Black (NA until coded)",
  HCWeek1BlackProv = "Opening-day head coach flagged Black by black_provisional (1) or not flagged (0); lower bound",
  HCWeek1BlackPred = "Opening-day head coach model-only predicted P(non-Hispanic Black alone) (p_black_any_pred; team_game staff snapshot); calibrated to the staff population at first appearance, not to the selected population of head coaches",
  HCWeek1BlackPredDoc = "Opening-day head coach P(Black), documented-race variant (p_black_any_preddoc; sensitivity)",
  HCWeek1PriorBlackPred = "Opening-day head coach EM prior P(Black) (prior_black_pred; person-level calibration control for HCWeek1BlackPred)",
  NegDefEPAPerPlay = "Minus defensive EPA per play allowed (higher = better defense)",
  NegDefSuccessRate = "Minus defensive success rate allowed (higher = better defense)",
  DeltaWinPct = "WinPct minus LagWinPct (NA if the previous season is not observed)",
  DeltaWinsOverExpected = "WinsOverExpected minus LagWinsOverExpected"
)

unit_desc <- "the unit (offense: QB-OL; defense: DL-DB)"
AnalysisTeamUnitSeasonLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  season = "NFL season (1999-2025)",
  Unit = "offense or defense (key)",
  UnitDefense = "1 for the defense row",
  team_code = "nflverse team code in that season",
  UnitEPAPerPlay = "Unit EPA per play, higher = better: OffEPAPerPlay for offense, -DefEPAPerPlay for defense",
  UnitSuccessRate = "Unit success rate, higher = better: OffSuccessRate, or -DefSuccessRate",
  UnitPassEPAPerPlay = "Unit EPA per pass play, higher = better: OffPassEPAPerPlay, or -DefPassEPAPerPlay",
  UnitRushEPAPerPlay = "Unit EPA per rush play, higher = better: OffRushEPAPerPlay, or -DefRushEPAPerPlay",
  LagUnitEPAPerPlay = "UnitEPAPerPlay in the previous season (NA if not consecutive)",
  LagUnitSuccessRate = "UnitSuccessRate in the previous season (NA if not consecutive)",
  UnitCoordPersonId = "Staff person_id of the unit coordinator (OC for offense, DC for defense; holder listed in the most snapshots)",
  UnitCoordBlackHand = "Unit coordinator hand-coded Black (NA until coded)",
  UnitCoordBlackProv = "Unit coordinator flagged Black by black_provisional (1) or not flagged (0); NA when no coordinator is listed",
  UnitCoordPBlackBifsg = "Unit coordinator BIFSG P(Black)",
  UnitCoordBlackPred = "Unit coordinator model-only predicted P(non-Hispanic Black alone) (OCBlackPred / DCBlackPred; miscalibrated for promoted holders; NA when no coordinator is listed)",
  UnitCoordBlackPredDoc = "Unit coordinator P(Black), documented-race variant (OCBlackPredDoc / DCBlackPredDoc; sensitivity)",
  UnitCoordPriorBlackPred = "Unit coordinator EM prior P(Black) (OCPriorBlackPred / DCPriorBlackPred; person-level calibration control for UnitCoordBlackPred)",
  UnitCoordChange = "Unit coordinator differs from the previous season's (OCChange / DCChange)",
  NUnitCoaches = "Number of on-field coaches of the unit (NOffenseCoaches / NDefenseCoaches)",
  CodedShareUnitCoaches = "Share of the unit's coaches with a hand-coded black_any",
  ShareBlackHandUnitCoaches = "Share Black among the unit's hand-coded coaches (NA until coded)",
  ShareBlackProvUnitCoaches = "Share of the unit's coaches flagged Black by black_provisional; positive-only lower bound",
  MeanPBlackBifsgUnitCoaches = "Mean BIFSG P(Black) of the unit's coaches",
  ShareBlackPredUnitCoaches = "Expected Black share of the unit's coaches: mean model-only predicted P(non-Hispanic Black alone) (ShareBlackPredOffenseCoaches / ShareBlackPredDefenseCoaches)",
  ShareBlackPredDocUnitCoaches = "Mean P(Black) of the unit's coaches, documented-race variant (sensitivity)",
  MeanPriorBlackPredUnitCoaches = "Mean EM prior P(Black) of the unit's coaches (regression-calibration control for ShareBlackPredUnitCoaches)",
  NUnitRoster = glue("Number of distinct game-day players in {unit_desc} (2002+)"),
  CodedShareUnitRoster = glue("Game-day-week-weighted share of {unit_desc} with a hand code (2002+)"),
  ShareBlackHandUnitRoster = glue("Game-day-week-weighted Black share among hand-coded players of {unit_desc} (2002+)"),
  ShareBlackProvUnitRoster = glue("Game-day-week-weighted share of {unit_desc} flagged Black by black_provisional among all players (2002+)"),
  ExpectedShareBlackHandUnitRoster = paste0("Expected hand-coded Black share of the unit's game-day players given position mix (leave-one-franchise-out league shares).", gate_note),
  ExpectedShareBlackProvUnitRoster = "Expected provisional Black share of the unit's game-day players given position mix (leave-one-franchise-out league shares)",
  ResidualShareBlackHandUnitRoster = paste0("ShareBlackHandUnitRoster minus ExpectedShareBlackHandUnitRoster.", gate_note),
  ResidualShareBlackProvUnitRoster = "ShareBlackProvUnitRoster minus ExpectedShareBlackProvUnitRoster",
  ShareBlackPredUnitRoster = glue("Expected Black share of {unit_desc}: game-day-week-weighted mean predicted P(non-Hispanic Black alone) (2002+)"),
  ShareBlackPredDocUnitRoster = glue("Game-day-week-weighted mean P(Black) of {unit_desc}, documented-race variant (2002+; sensitivity)"),
  MeanPriorBlackPredUnitRoster = glue("Game-day-week-weighted mean EM prior P(Black) of {unit_desc} (2002+; regression-calibration control)"),
  ExpectedShareBlackPredUnitRoster = "Expected predicted Black share of the unit's game-day players given position mix (leave-one-franchise-out league mean P(non-Hispanic Black alone) by position group)",
  ExpectedShareBlackPredDocUnitRoster = "Expected documented-variant Black share of the unit's game-day players given position mix (leave-one-franchise-out)",
  ResidualShareBlackPredUnitRoster = "ShareBlackPredUnitRoster minus ExpectedShareBlackPredUnitRoster",
  ResidualShareBlackPredDocUnitRoster = "ShareBlackPredDocUnitRoster minus ExpectedShareBlackPredDocUnitRoster",
  CodedShareUnitRosterSnapW = "Unit-snap-weighted (offense snaps for offense, defense snaps for defense) share of players with a hand code (2013+)",
  ShareBlackHandUnitRosterSnapW = "Unit-snap-weighted Black share among hand-coded players (2013+)",
  ShareBlackProvUnitRosterSnapW = "Unit-snap-weighted share flagged Black by black_provisional among all players (2013+)",
  ExpectedShareBlackHandUnitRosterSnapW = paste0("Expected hand-coded unit-snap-weighted Black share given position mix (2013+).", gate_note),
  ExpectedShareBlackProvUnitRosterSnapW = "Expected provisional unit-snap-weighted Black share given position mix (2013+)",
  ResidualShareBlackHandUnitRosterSnapW = paste0("ShareBlackHandUnitRosterSnapW minus its expected value.", gate_note),
  ResidualShareBlackProvUnitRosterSnapW = "ShareBlackProvUnitRosterSnapW minus its expected value",
  ShareBlackPredUnitRosterSnapW = "Unit-snap-weighted mean predicted P(non-Hispanic Black alone) (expected Black share; 2013+)",
  ShareBlackPredDocUnitRosterSnapW = "Unit-snap-weighted mean P(Black), documented-race variant (2013+; sensitivity)",
  MeanPriorBlackPredUnitRosterSnapW = "Unit-snap-weighted mean EM prior P(Black) (2013+; regression-calibration control)",
  ExpectedShareBlackPredUnitRosterSnapW = "Expected predicted unit-snap-weighted Black share given position mix (2013+)",
  ExpectedShareBlackPredDocUnitRosterSnapW = "Expected documented-variant unit-snap-weighted Black share given position mix (2013+)",
  ResidualShareBlackPredUnitRosterSnapW = "ShareBlackPredUnitRosterSnapW minus its expected value",
  ResidualShareBlackPredDocUnitRosterSnapW = "ShareBlackPredDocUnitRosterSnapW minus its expected value",
  UnitCapShare = "Cap share (sum of CapPercent) of players in the unit's position groups (OffenseCapShare / DefenseCapShare; 2013+)",
  UnitMeanLogPickRoster = "Game-day-week-weighted mean log draft pick of the unit's players (undrafted = log 300)",
  UnitMeanLogPickSnapW = "Unit-snap-weighted mean log draft pick (2013+; undrafted = log 300)",
  UnitMeanAgeRoster = "Game-day-week-weighted mean age of the unit's players",
  UnitMeanAgeSnapW = "Unit-snap-weighted mean age (2013+)",
  HCPersonId = SeasonInputLabels[["HCPersonId"]],
  HCBlackHand = SeasonInputLabels[["HCBlackHand"]],
  HCBlackProv = SeasonInputLabels[["HCBlackProv"]],
  HCPBlackBifsg = SeasonInputLabels[["HCPBlackBifsg"]],
  HCBlackPred = SeasonInputLabels[["HCBlackPred"]],
  HCBlackPredDoc = SeasonInputLabels[["HCBlackPredDoc"]],
  HCPriorBlackPred = SeasonInputLabels[["HCPriorBlackPred"]],
  HCPersonIdPre = SeasonInputLabels[["HCPersonIdPre"]],
  HCBlackHandPre = SeasonInputLabels[["HCBlackHandPre"]],
  HCBlackProvPre = SeasonInputLabels[["HCBlackProvPre"]],
  HCPBlackBifsgPre = SeasonInputLabels[["HCPBlackBifsgPre"]],
  HCBlackPredPre = SeasonInputLabels[["HCBlackPredPre"]],
  HCBlackPredDocPre = SeasonInputLabels[["HCBlackPredDocPre"]],
  HCPriorBlackPredPre = SeasonInputLabels[["HCPriorBlackPredPre"]],
  HCIncumbentKey = SeasonInputLabels[["HCIncumbentKey"]],
  HCFirstGameKey = SeasonInputLabels[["HCFirstGameKey"]],
  HCIncumbentSpellId = SeasonInputLabels[["HCIncumbentSpellId"]],
  FullStaffObserved = SeasonInputLabels[["FullStaffObserved"]],
  OpeningStaffObserved = SeasonInputLabels[["OpeningStaffObserved"]],
  StaffSource = SeasonInputLabels[["StaffSource"]],
  WinPct = SeasonInputLabels[["WinPct"]],
  LagWinPct = SeasonInputLabels[["LagWinPct"]],
  LagExpectedWins = SeasonInputLabels[["LagExpectedWins"]],
  # Opening-snapshot unit fields (02, suffix Pre): the OC/DC and unit coaches
  # listed in the opening revision; NA before 2007 and when none is listed
  UnitCoordPersonIdPre = "Staff person_id of the unit coordinator listed in the opening snapshot (OCPersonIdPre / DCPersonIdPre; non-interim holder first when several are listed)",
  UnitCoordBlackHandPre = "Opening-snapshot unit coordinator hand-coded Black (NA until coded)",
  UnitCoordBlackProvPre = "Opening-snapshot unit coordinator flagged Black by black_provisional (1) or not flagged (0); NA when none is listed",
  UnitCoordPBlackBifsgPre = "Opening-snapshot unit coordinator BIFSG P(Black)",
  UnitCoordBlackPredPre = "Opening-snapshot unit coordinator model-only predicted P(non-Hispanic Black alone) (OCBlackPredPre / DCBlackPredPre; miscalibrated for promoted holders; NA when none is listed)",
  UnitCoordBlackPredDocPre = "Opening-snapshot unit coordinator P(Black), documented-race variant (sensitivity)",
  UnitCoordPriorBlackPredPre = "Opening-snapshot unit coordinator EM prior P(Black) (person-level calibration control for UnitCoordBlackPredPre)",
  UnitCoordChangePre = "Opening-snapshot unit coordinator differs from the previous season's opening-snapshot coordinator (OCChangePre / DCChangePre; NA if either is unobserved, all of 2007)",
  NUnitCoordPre = "Number of unit coordinators listed in the opening snapshot (NOCPre / NDCPre)",
  NUnitCoachesPre = "Number of on-field coaches of the unit listed in the opening snapshot (NOffenseCoachesPre / NDefenseCoachesPre; 0 when parsed and none, NA when unobserved or before 2007)",
  CodedShareUnitCoachesPre = "Share of the opening snapshot's unit coaches with a hand-coded black_any",
  ShareBlackHandUnitCoachesPre = "Share Black among the opening snapshot's hand-coded unit coaches (NA until coded)",
  ShareBlackProvUnitCoachesPre = "Share of the opening snapshot's unit coaches flagged Black by black_provisional; positive-only lower bound",
  MeanPBlackBifsgUnitCoachesPre = "Mean BIFSG P(Black) of the opening snapshot's unit coaches",
  ShareBlackPredUnitCoachesPre = "Expected Black share of the opening snapshot's unit coaches: mean model-only predicted P(non-Hispanic Black alone) (ShareBlackPredOffenseCoachesPre / ShareBlackPredDefenseCoachesPre)",
  ShareBlackPredDocUnitCoachesPre = "Mean P(Black) of the opening snapshot's unit coaches, documented-race variant (sensitivity)",
  MeanPriorBlackPredUnitCoachesPre = "Mean EM prior P(Black) of the opening snapshot's unit coaches (regression-calibration control for ShareBlackPredUnitCoachesPre)",
  HCWeek1BlackHand = "Opening-day head coach (head coach of the franchise's first REG game, team_game) hand-coded Black (NA until coded)",
  HCWeek1BlackProv = "Opening-day head coach flagged Black by black_provisional (1) or not flagged (0); lower bound",
  HCWeek1BlackPred = "Opening-day head coach model-only predicted P(non-Hispanic Black alone) (p_black_any_pred)",
  HCWeek1BlackPredDoc = "Opening-day head coach P(Black), documented-race variant (sensitivity)",
  HCWeek1PriorBlackPred = "Opening-day head coach EM prior P(Black) (person-level calibration control for HCWeek1BlackPred)"
)

GameRosterLabels <- codebook_labels(RosterGameCodebook)
GameRosterLabels <- GameRosterLabels[setdiff(names(GameRosterLabels),
                                             c("franchise_id", "game_id", "season", "week"))]
GameHandDerived <- c("ExpectedShareBlackHandActiveRoster", "ResidualShareBlackHandActiveRoster")
GameRosterLabels[GameHandDerived] <- paste0(GameRosterLabels[GameHandDerived], ".", gate_note)
OppLabels <- set_names(paste0("Opponent's ", GameRosterLabels), paste0("Opp", names(GameRosterLabels)))
OppHand <- c(paste0("OppShareBlackHand", c("ActiveRoster", "ActiveOffense", "ActiveDefense",
                                            "GameDayRoster")))
OppLabels[OppHand] <- paste0(OppLabels[OppHand], ".", gate_note, " (coverage OppCodedShare)")

AnalysisTeamGameLabels <- c(
  codebook_labels(TeamGameCodebook),
  GameRosterLabels,
  OppLabels,
  StartingQBId = SeasonInputLabels[["StartingQBId"]],
  QBStartShare = SeasonInputLabels[["QBStartShare"]],
  QBBlackHand = SeasonInputLabels[["QBBlackHand"]],
  QBBlackProv = SeasonInputLabels[["QBBlackProv"]],
  QBPBlackBifsg = SeasonInputLabels[["QBPBlackBifsg"]],
  QBBlackPred = SeasonInputLabels[["QBBlackPred"]],
  QBBlackPredDoc = SeasonInputLabels[["QBBlackPredDoc"]],
  QBPriorBlackPred = SeasonInputLabels[["QBPriorBlackPred"]],
  AtsMargin = "Against-the-spread margin: margin minus team_spread_line (team_spread_line > 0 when the team is favored)"
)

# Roster measures exist for REG games only
reg_note <- " (REG games 2002-2025; NA for POST games)"
for (v in c(names(GameRosterLabels), names(OppLabels))) {
  AnalysisTeamGameLabels[[v]] <- paste0(AnalysisTeamGameLabels[[v]], reg_note)
}

# ---------------------------------------------------------------------------
# Write the samples
# ---------------------------------------------------------------------------

write_sample(AnalysisTeamSeason, "analysis_team_season", key = SeasonKeys,
             labels = AnalysisTeamSeasonLabels)
write_sample(AnalysisTeamUnitSeason, "analysis_team_unit_season",
             key = c(SeasonKeys, "Unit"), labels = AnalysisTeamUnitSeasonLabels)
write_sample(AnalysisTeamGame, "analysis_team_game", key = c("franchise_id", "game_id"),
             labels = AnalysisTeamGameLabels)

# ---------------------------------------------------------------------------
# Coverage report
# ---------------------------------------------------------------------------

AnalysisTeamSeason |>
  mutate(Period = case_when(season < 2002 ~ "1999-2001", season <= 2012 ~ "2002-2012",
                            TRUE ~ "2013-2025")) |>
  group_by(Period) |>
  summarise(TeamSeasons = n(),
            ShareProvRoster = mean(!is.na(ShareBlackProvRoster)),
            ShareProvSnapW = mean(!is.na(ShareBlackProvSnapW)),
            ShareProvCoaches = mean(!is.na(ShareBlackProvCoaches)),
            CodedShareRoster = mean(CodedShareRoster, na.rm = TRUE),
            CodedShareSnapW = mean(CodedShareSnapW, na.rm = TRUE),
            CodedShareCoaches = mean(CodedShareCoaches, na.rm = TRUE),
            HandRoster = mean(!is.na(ShareBlackHandRoster)),
            QBRace = mean(!is.na(QBBlackProv)),
            CapShare = mean(!is.na(TeamCapShare)),
            .groups = "drop") |>
  print(width = Inf)

message("League mean of the provisional roster shares by season:")
AnalysisTeamSeason |>
  filter(season %in% c(2002, 2007, 2013, 2016, 2019, 2022, 2025)) |>
  group_by(season) |>
  summarise(ShareBlackProvRoster = mean(ShareBlackProvRoster),
            ShareBlackProvSnapW = mean(ShareBlackProvSnapW),
            QBBlackProv = mean(QBBlackProv), .groups = "drop") |>
  print()

message("TeamCapShare distribution (2013-2025):")
print(round(quantile(AnalysisTeamSeason$TeamCapShare, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                     na.rm = TRUE), 3))

# Within-franchise variation of the snap-weighted provisional share
SnapVar <- AnalysisTeamSeason |>
  filter(!is.na(ShareBlackProvSnapW)) |>
  group_by(franchise_id) |>
  mutate(Demeaned = ShareBlackProvSnapW - mean(ShareBlackProvSnapW)) |>
  group_by(season) |>
  mutate(TwoWay = Demeaned - mean(Demeaned)) |>
  ungroup()
message("ShareBlackProvSnapW: total SD ", round(sd(SnapVar$ShareBlackProvSnapW), 4),
        "; within-franchise SD ", round(sd(SnapVar$Demeaned), 4),
        "; SD net of franchise and season means ", round(sd(SnapVar$TwoWay), 4))

# Predicted roster shares: league means by period, within-franchise SD and
# agreement with the provisional measure
PredReport <- AnalysisTeamSeason |>
  filter(season >= 2002) |>
  mutate(Period = if_else(season <= 2012, "2002-2012", "2013-2025"))
PredReport |>
  group_by(Period) |>
  summarise(across(c(ShareBlackPredRoster, ShareBlackPredSnapW, ShareBlackPredDocRoster,
                     ShareBlackPredDocSnapW, ShareBlackProvRoster, ShareBlackProvSnapW),
                   \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  print(width = Inf)
for (v in c("ShareBlackPredRoster", "ShareBlackPredSnapW")) {
  Within <- PredReport |>
    filter(!is.na(.data[[v]])) |>
    group_by(franchise_id) |>
    mutate(Demeaned = .data[[v]] - mean(.data[[v]])) |>
    group_by(season) |>
    mutate(TwoWay = Demeaned - mean(Demeaned)) |>
    ungroup()
  message(v, ": total SD ", round(sd(Within[[v]]), 4), "; within-franchise SD ",
          round(sd(Within$Demeaned), 4), "; SD net of franchise and season means ",
          round(sd(Within$TwoWay), 4))
}
message("Correlation across team-seasons: ShareBlackPredSnapW vs ShareBlackProvSnapW ",
        round(cor(PredReport$ShareBlackPredSnapW, PredReport$ShareBlackProvSnapW,
                  use = "complete.obs"), 3),
        "; ShareBlackPredRoster vs ShareBlackProvRoster ",
        round(cor(PredReport$ShareBlackPredRoster, PredReport$ShareBlackProvRoster,
                  use = "complete.obs"), 3),
        "; ShareBlackPredSnapW vs ShareBlackPredDocSnapW ",
        round(cor(PredReport$ShareBlackPredSnapW, PredReport$ShareBlackPredDocSnapW,
                  use = "complete.obs"), 3))

message("analysis_team_game: REG team-games with own active-roster shares: ",
        sum(!is.na(AnalysisTeamGame$ShareBlackProvActiveRoster)), "; with opponent shares: ",
        sum(!is.na(AnalysisTeamGame$OppShareBlackProvActiveRoster)))

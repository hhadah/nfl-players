# ============================================================================
# 09-roster-composition-sample.R
# Builds the roster-composition samples for the roster-diversity design
# (notes/analysis-plan.md, section 2):
#   - analysis/roster_composition_team_season: one row per franchise_id x
#     season, 2002-2025 (every franchise-season of team_season from 2002).
#     For each player group G (game-day roster, offense/defense headcount,
#     snap-weighted roster and units, position groups): N<G>, W<G>,
#     CodedShare<G>, ShareBlackHand<G>, ShareBlackProv<G>, MeanPBlackBifsg<G>,
#     ShareBlackPred<G>, ShareBlackPredDoc<G>, MeanPriorBlackPred<G>;
#     expected Black shares given the position mix (leave-one-franchise-out
#     league shares) and residual shares; the season starting QB's race; and
#     roster-quality controls (cap share, draft capital, age, experience);
#     predetermined opening-day versions (Week1: first-REG-game active roster;
#     Week1PriorSnapW: the same players weighted by prior-season snaps; the
#     opening-day starting QB and Week1 roster quality); and Other-race and
#     Wikipedia-article shares (three-group Blau index, provisional coverage).
#   - analysis/roster_composition_team_game: one row per franchise_id x
#     game_id, REG games 2002-2025: the same shares for the week's game-day
#     active roster (no snap weights: a game's own snaps reflect game flow),
#     expected/residual active-roster shares and the game's starting QB race.
# Inputs: DuckDB (nfl_rosters_weekly, nfl_snap_counts, nfl_team_games,
#   nfl_ids, franchise_seasons, race_predicted), analysis/player_season (04), person race from
#   load_person_race() (00-race-measures.R).
# Outputs: analysis/roster_composition_team_season.{parquet,csv},
#   analysis/roster_composition_team_game.{parquet,csv} and their codebooks.
# Race measures are kept separate (Hand = hand-coded black_any, among coded
# members; Prov = share flagged by black_provisional among ALL members, the
# positive-only lower bound used in 02; Bifsg = mean BIFSG P(Black); Pred =
# mean model-only predicted P(non-Hispanic Black alone) (p_black_any_pred
# equals p_black_pred despite its name), i.e. the expected Black share, the
# primary measure while hand codes are absent;
# PredDoc = the documented-race sensitivity variant; MeanPriorBlackPred = mean
# EM prior P(Black), the group summary of the prior's predetermined
# covariates along the prior index). For the main groups (Roster, SnapW,
# Week1, Week1PriorSnapW, ActiveRoster) also: variant expected shares
# (ShareBlackNoDraft, ShareBlackOrMulti, ShareBlackRaked), the nodraft and
# preddoc mean priors, Berkson variances, a documented-race calibration check
# and the weighted shares of the prior's non-position covariate levels
# (regression-calibration controls; notes/race-prediction-design.md); and the
# defensive share of the snap weight (DefenseSnapWeightShare). The variants
# and county availability are read from race_predicted directly.
# Date: 2026-10-02 (predicted race added the same day)
# ============================================================================

con <- db_connect()

FirstRosterSeason <- 2002L
LastSeason <- 2025L
FirstSnapSeason <- 2013L
OffenseGroups <- c("QB", "RB", "WR", "TE", "OL")
DefenseGroups <- c("DL", "LB", "DB")

# ---------------------------------------------------------------------------
# Player race (hand code, provisional flag, BIFSG)
# ---------------------------------------------------------------------------

# Besides the Black indicators, each player gets an "Other" (neither Black
# nor white) indicator under each measure, for the three-group (Black /
# white / other) Blau index, and an indicator for having a Wikipedia article
# (non-missing category signal), used to gauge the provisional measure's
# coverage artifacts. Hand: person_race_group() among hand-coded players.
# Prov: not flagged Black and flagged other (hand Other where coded, else a
# Wikipedia Hispanic/Asian/Pacific Islander/Native American category), among
# ALL players (unflagged count as 0, as for black_provisional).
PlayerRace <- load_person_race(con, hand_coded) |>
  filter(entity == "player")
PlayerRace <- PlayerRace |>
  mutate(GroupHand = person_race_group(PlayerRace, "hand"),
         GroupProv = person_race_group(PlayerRace, "provisional"),
         other_hand = case_when(is.na(black_any) ~ NA_integer_,
                                coalesce(GroupHand == "Other", FALSE) ~ 1L,
                                TRUE ~ 0L),
         other_provisional = as.integer(coalesce(GroupProv == "Other", FALSE) &
                                          coalesce(black_provisional, 0L) != 1L),
         has_article = as.integer(!is.na(wiki_cat_black)),
         # Predicted race: P(other) = 1 - P(Black any) - P(white), floored at 0
         other_pred = pmax(1 - p_black_any_pred - p_white_pred, 0),
         other_preddoc = pmax(1 - p_black_any_preddoc - p_white_preddoc, 0)) |>
  select(gsis_id = person_id, black_any, black_provisional, p_black_bifsg,
         other_hand, other_provisional, has_article, p_black_any_pred,
         p_black_any_preddoc, prior_black_pred, other_pred, other_preddoc,
         documented_black_any, pred_draft_bucket, pred_college_type, pred_rookie_era)

# Player-level predicted-race columns that load_person_race() does not carry,
# read from race_predicted (scripts/04e_predict_race.py): the variants used as
# sensitivity panels (prior without draft round, which does not proxy draft
# capital; P(Black or multiracial); shares raked to TIDES), the preddoc prior
# (the calibration control under the preddoc measure) and hometown-county
# availability (a covariate of the primary player prior)
PredExtra <- tbl(con, "race_predicted") |>
  filter(entity == "player") |>
  select(person_uid, p_black_pred_nodraft, prior_black_pred_nodraft, p_black_or_multi_pred,
         p_black_pred_raked, prior_black_preddoc, county_available) |>
  collect() |>
  mutate(gsis_id = str_remove(person_uid, "^player:"), .keep = "unused")
stopifnot(!anyDuplicated(PredExtra$gsis_id))

# Indicators of the levels of the primary prior's covariates other than
# position (omitted levels: undrafted, power-conference college, rookie era
# <= 2005, no county likelihood). Their weighted team shares are the
# team-level analogue of conditioning on the prior's covariates (regression
# calibration; notes/race-prediction-design.md, section 4).
CovLevels <- tribble(
  ~CovVar,           ~covariate,          ~level,
  "DraftR12",        "pred_draft_bucket", "R1-2",
  "DraftR34",        "pred_draft_bucket", "R3-4",
  "DraftR5",         "pred_draft_bucket", "R5+",
  "CollegeHBCU",     "pred_college_type", "hbcu",
  "CollegeFCS",      "pred_college_type", "fcs_or_lower",
  "CollegeOtherFBS", "pred_college_type", "other_fbs",
  "CollegeUnknown",  "pred_college_type", "unknown",
  "Era0610",         "pred_rookie_era",   "2006-10",
  "Era1115",         "pred_rookie_era",   "2011-15",
  "Era1620",         "pred_rookie_era",   "2016-20",
  "Era21",           "pred_rookie_era",   "2021+",
  "CountyAvail",     "county_available",  "yes")
PlayerRace <- PlayerRace |>
  left_join(PredExtra, by = "gsis_id")
for (i in seq_len(nrow(CovLevels))) {
  x <- PlayerRace[[CovLevels$covariate[i]]]
  PlayerRace[[CovLevels$CovVar[i]]] <- as.integer(x == CovLevels$level[i])
}
# Every level must occur (a renamed level in 04e would silently give zeros)
stopifnot(all(map_lgl(CovLevels$CovVar, \(v) any(PlayerRace[[v]] == 1L, na.rm = TRUE))))
PlayerRace <- select(PlayerRace, -pred_draft_bucket, -pred_college_type, -pred_rookie_era,
                     -county_available)
message("Players with a predicted P(Black): ", sum(!is.na(PlayerRace$p_black_any_pred)),
        "; with the nodraft / Black-or-multi / raked variants: ",
        sum(!is.na(PlayerRace$p_black_pred_nodraft)), " / ",
        sum(!is.na(PlayerRace$p_black_or_multi_pred)), " / ",
        sum(!is.na(PlayerRace$p_black_pred_raked)), "; documented race: ",
        sum(!is.na(PlayerRace$documented_black_any)),
        " of ", nrow(PlayerRace), " (PredDoc: ", sum(!is.na(PlayerRace$p_black_any_preddoc)),
        "; prior: ", sum(!is.na(PlayerRace$prior_black_pred)), ")")

# ---------------------------------------------------------------------------
# REG team-games (week -> game_id) and weekly game-day rosters
# ---------------------------------------------------------------------------

GameWeeks <- tbl(con, "nfl_team_games") |>
  filter(game_type == "REG") |>
  select(franchise_id, season, week, game_id, gameday, starting_qb_id) |>
  collect() |>
  mutate(season = as.integer(season), week = as.integer(week),
         gameday = as.Date(gameday)) |>
  filter(season >= FirstRosterSeason, season <= LastSeason)
stopifnot(!anyDuplicated(GameWeeks[c("franchise_id", "season", "week")]))

# Primary rows of the weekly roster in weeks in which the franchise played a
# REG game (bye weeks dropped). Status groups follow roster_status_group() in
# 00-player-functions.R: Active = ACT except game-day inactives (abbr I*),
# Inactive = INA (2019+) or ACT with abbr I*. In 2016-2018 no row is flagged
# inactive, so Active includes game-day inactives in those seasons.
RosterWeeks <- tbl(con, "nfl_rosters_weekly") |>
  filter(season_type == "REG", is_key_primary, !is.na(gsis_id), gsis_id != "",
         status %in% c("ACT", "INA")) |>
  select(gsis_id, franchise_id, season, week, status, status_description_abbr,
         position_group) |>
  collect() |>
  mutate(season = as.integer(season), week = as.integer(week),
         StatusGroup = case_when(
           status == "ACT" & coalesce(str_starts(status_description_abbr, "I"), FALSE) ~ "Inactive",
           status == "ACT" ~ "Active",
           TRUE ~ "Inactive"),
         PosGroup = coalesce(position_group, "Unknown")) |>
  inner_join(GameWeeks |> select(franchise_id, season, week, game_id),
             by = c("franchise_id", "season", "week")) |>
  left_join(PlayerRace, by = "gsis_id")
stopifnot(!anyDuplicated(RosterWeeks[c("gsis_id", "franchise_id", "season", "week")]))

# ---------------------------------------------------------------------------
# Composition helpers
# ---------------------------------------------------------------------------

# Weighted composition of a group. `df` has one row per member x key x
# position group with Weight > 0 and the race columns. Returns, by key:
#   N (distinct members), W (total weight), CodedShare (weight share with a
#   hand code), ShareBlackHand (weighted Black share among hand-coded
#   members; NA if none coded), ShareBlackProv (weighted share flagged by
#   black_provisional among ALL members; NA flags count as not flagged),
#   MeanPBlackBifsg (weighted mean BIFSG P(Black) over members with one),
#   ShareBlackPred / ShareBlackPredDoc (weighted mean predicted P(Black any)
#   over members with a prediction: the expected Black share; every player
#   has one), MeanPriorBlackPred (weighted mean EM prior P(Black)), and the
#   Other-race analogues (ShareOtherPred/PredDoc: weighted mean of
#   1 - P(Black any) - P(white)).
roster_compose <- function(df, keys, suffix) {
  df |>
    filter(Weight > 0) |>
    group_by(across(all_of(keys))) |>
    summarise(N = n_distinct(gsis_id),
              W = sum(Weight),
              CodedW = sum(Weight * !is.na(black_any)),
              BlackHandW = sum(Weight * black_any, na.rm = TRUE),
              ProvW = sum(Weight * (coalesce(black_provisional, 0L) == 1L)),
              BifsgW = sum(Weight * !is.na(p_black_bifsg)),
              BifsgSum = sum(Weight * p_black_bifsg, na.rm = TRUE),
              OtherHandW = sum(Weight * other_hand, na.rm = TRUE),
              OtherProvW = sum(Weight * coalesce(other_provisional, 0L)),
              ArticleW = sum(Weight * coalesce(has_article, 0L)),
              PredW = sum(Weight * !is.na(p_black_any_pred)),
              PredSum = sum(Weight * p_black_any_pred, na.rm = TRUE),
              OtherPredSum = sum(Weight * other_pred, na.rm = TRUE),
              PredDocW = sum(Weight * !is.na(p_black_any_preddoc)),
              PredDocSum = sum(Weight * p_black_any_preddoc, na.rm = TRUE),
              OtherPredDocSum = sum(Weight * other_preddoc, na.rm = TRUE),
              PriorW = sum(Weight * !is.na(prior_black_pred)),
              PriorSum = sum(Weight * prior_black_pred, na.rm = TRUE),
              .groups = "drop") |>
    transmute(across(all_of(keys)), N, W,
              CodedShare = CodedW / W,
              ShareBlackHand = safe_div(BlackHandW, CodedW),
              ShareBlackProv = ProvW / W,
              MeanPBlackBifsg = safe_div(BifsgSum, BifsgW),
              ShareOtherHand = safe_div(OtherHandW, CodedW),
              ShareOtherProv = OtherProvW / W,
              ArticleShare = ArticleW / W,
              ShareBlackPred = safe_div(PredSum, PredW),
              ShareBlackPredDoc = safe_div(PredDocSum, PredDocW),
              MeanPriorBlackPred = safe_div(PriorSum, PriorW),
              ShareOtherPred = safe_div(OtherPredSum, PredW),
              ShareOtherPredDoc = safe_div(OtherPredDocSum, PredDocW)) |>
    rename_with(\(x) paste0(x, suffix), -all_of(keys))
}

# Expected Black share given the position mix:
#   Expected_k = sum_p (team weight share in position group p) x (league
#                Black share in p in the season, on all OTHER franchises)
# using the same measure and weights. Hand: the team's mix among its
# hand-coded weight and the league share among coded weight (so that the
# residual compares coded members with coded members); Prov: the team's full
# weight mix and the league share flagged among all weight; Pred / PredDoc:
# the team's mix among weight with a prediction and the league mean predicted
# P(Black any) in the position group. `keys` must contain franchise_id and
# season. Returns ExpectedShareBlack{Hand,Prov,Pred,PredDoc}<G> by key.
roster_expected <- function(df, keys, suffix) {
  ByPos <- df |>
    filter(Weight > 0) |>
    group_by(across(all_of(c(keys, "PosGroup")))) |>
    summarise(W = sum(Weight),
              CodedW = sum(Weight * !is.na(black_any)),
              BlackHandW = sum(Weight * black_any, na.rm = TRUE),
              ProvW = sum(Weight * (coalesce(black_provisional, 0L) == 1L)),
              PredW = sum(Weight * !is.na(p_black_any_pred)),
              PredSum = sum(Weight * p_black_any_pred, na.rm = TRUE),
              PredDocW = sum(Weight * !is.na(p_black_any_preddoc)),
              PredDocSum = sum(Weight * p_black_any_preddoc, na.rm = TRUE),
              .groups = "drop")
  SumCols <- c("W", "CodedW", "BlackHandW", "ProvW", "PredW", "PredSum", "PredDocW",
               "PredDocSum")
  # League totals by season x position group, and each franchise's own
  # season total (removed for the leave-one-out share)
  League <- ByPos |>
    group_by(season, PosGroup) |>
    summarise(across(all_of(SumCols), sum, .names = "League{.col}"),
              .groups = "drop")
  Own <- ByPos |>
    group_by(franchise_id, season, PosGroup) |>
    summarise(across(all_of(SumCols), sum, .names = "Own{.col}"),
              .groups = "drop")
  ByPos |>
    left_join(League, by = c("season", "PosGroup")) |>
    left_join(Own, by = c("franchise_id", "season", "PosGroup")) |>
    mutate(LooHand = safe_div(LeagueBlackHandW - OwnBlackHandW, LeagueCodedW - OwnCodedW),
           LooProv = safe_div(LeagueProvW - OwnProvW, LeagueW - OwnW),
           LooPred = safe_div(LeaguePredSum - OwnPredSum, LeaguePredW - OwnPredW),
           LooPredDoc = safe_div(LeaguePredDocSum - OwnPredDocSum, LeaguePredDocW - OwnPredDocW)) |>
    group_by(across(all_of(keys))) |>
    # Position groups that no other franchise fields that season (e.g. a
    # stray 'Unknown' group) have no leave-one-out share; the expected share
    # is then taken over the remaining groups (weights renormalized)
    summarise(ExpectedShareBlackHand = safe_div(sum((CodedW * LooHand)[!is.na(LooHand)]),
                                                sum(CodedW[!is.na(LooHand)])),
              ExpectedShareBlackProv = safe_div(sum((W * LooProv)[!is.na(LooProv)]),
                                                sum(W[!is.na(LooProv)])),
              ExpectedShareBlackPred = safe_div(sum((PredW * LooPred)[!is.na(LooPred)]),
                                                sum(PredW[!is.na(LooPred)])),
              ExpectedShareBlackPredDoc = safe_div(sum((PredDocW * LooPredDoc)[!is.na(LooPredDoc)]),
                                                   sum(PredDocW[!is.na(LooPredDoc)])),
              .groups = "drop") |>
    rename_with(\(x) paste0(x, suffix), -all_of(keys))
}

# Weighted mean over members with a non-missing value
wmean <- function(x, w) safe_div(sum(w * x, na.rm = TRUE), sum(w[!is.na(x)]))

# Berkson variance of an expected share: Var(true share - expected share) =
# sum_i w_i^2 p_i (1 - p_i) / (sum_i w_i)^2 if members' races are independent
# given the information behind the probabilities
berkson_var <- function(p, w) {
  ok <- !is.na(p)
  safe_div(sum(w[ok]^2 * p[ok] * (1 - p[ok])), sum(w[ok])^2)
}

# Additional predicted-race composition of a group (main groups only):
# the variant expected shares (ShareBlackNoDraft, ShareBlackOrMulti,
# ShareBlackRaked) and the nodraft and preddoc mean priors; the Berkson
# variances of the Pred and PredDoc expected shares; the team-level
# calibration check among documented members (DocShare = documented weight
# share, ShareBlackDoc = documented Black share, DocMeanPBlack = mean model
# P(Black) of the same members); and the weighted shares of the prior's
# non-position covariate levels (Share<CovVar>). Names avoid the BlackHand /
# BlackProv / BlackPred tags, so apply_race_measure() leaves them alone,
# except MeanPriorBlackPredDoc, which it maps to MeanPriorBlack under preddoc.
roster_compose_extra <- function(df, keys, suffix) {
  df |>
    filter(Weight > 0) |>
    group_by(across(all_of(keys))) |>
    summarise(ShareBlackNoDraft = wmean(p_black_pred_nodraft, Weight),
              MeanPriorBlackNoDraft = wmean(prior_black_pred_nodraft, Weight),
              ShareBlackOrMulti = wmean(p_black_or_multi_pred, Weight),
              ShareBlackRaked = wmean(p_black_pred_raked, Weight),
              MeanPriorBlackPredDoc = wmean(prior_black_preddoc, Weight),
              BerksonVarPred = berkson_var(p_black_any_pred, Weight),
              BerksonVarPredDoc = berkson_var(p_black_any_preddoc, Weight),
              DocShare = sum(Weight * !is.na(documented_black_any)) / sum(Weight),
              ShareBlackDoc = wmean(documented_black_any, Weight),
              DocMeanPBlack = wmean(if_else(is.na(documented_black_any), NA_real_,
                                            p_black_any_pred), Weight),
              across(all_of(CovLevels$CovVar), \(x) wmean(x, Weight), .names = "Share{.col}"),
              .groups = "drop") |>
    rename_with(\(x) paste0(x, suffix), -all_of(keys))
}

# ---------------------------------------------------------------------------
# Team-season members: game-day roster weeks and REG snaps
# ---------------------------------------------------------------------------

# Game-day weeks (Active + Inactive) of each player with each franchise, by
# roster position group. A player on several teams in a season counts for
# each team with his own weeks (PrimaryFranchise is never used).
RosterSeasonMembers <- RosterWeeks |>
  count(franchise_id, season, gsis_id, PosGroup, name = "Weight") |>
  left_join(PlayerRace, by = "gsis_id")

# Modal roster position group of a player with a franchise in a season (most
# game-day weeks; ties to the latest week), used to classify his snaps
RosterPosition <- RosterWeeks |>
  group_by(gsis_id, franchise_id, season, PosGroup) |>
  summarise(NWeeks = n(), LastWeek = max(week), .groups = "drop") |>
  arrange(gsis_id, franchise_id, season, desc(NWeeks), desc(LastWeek)) |>
  distinct(gsis_id, franchise_id, season, .keep_all = TRUE) |>
  select(gsis_id, franchise_id, season, RosterPosGroup = PosGroup)

# REG snap counts (2013+; the 2012 file is empty). Rows without gsis_id are
# recovered through pfr_player_id -> nfl_ids.pfr_id where the link is unique.
PfrToGsis <- tbl(con, "nfl_ids") |>
  filter(!is.na(pfr_id), !is.na(gsis_id)) |>
  distinct(pfr_id, gsis_id) |>
  collect() |>
  add_count(pfr_id) |>
  filter(n == 1) |>
  select(pfr_player_id = pfr_id, GsisFromPfr = gsis_id)

SnapRows <- tbl(con, "nfl_snap_counts") |>
  filter(game_type == "REG") |>
  select(gsis_id, pfr_player_id, franchise_id, season, week, game_id,
         position_group, offense_snaps, defense_snaps) |>
  collect() |>
  mutate(season = as.integer(season), week = as.integer(week),
         gsis_id = if_else(gsis_id == "", NA_character_, gsis_id)) |>
  filter(season >= FirstSnapSeason, season <= LastSeason) |>
  left_join(PfrToGsis, by = "pfr_player_id") |>
  mutate(MissingGsis = is.na(gsis_id), gsis_id = coalesce(gsis_id, GsisFromPfr))

SnapGsisReport <- SnapRows |>
  summarise(Rows = n(),
            Recovered = sum(MissingGsis & !is.na(gsis_id)),
            MissingGsis = sum(MissingGsis),
            StillMissing = sum(is.na(gsis_id)),
            SnapsStillMissing = sum((offense_snaps + defense_snaps)[is.na(gsis_id)], na.rm = TRUE),
            SnapsTotal = sum(offense_snaps + defense_snaps, na.rm = TRUE))
message("Snap rows without gsis_id: ", SnapGsisReport$MissingGsis, " of ",
        SnapGsisReport$Rows, "; recovered via pfr_player_id: ", SnapGsisReport$Recovered,
        "; still missing: ", SnapGsisReport$StillMissing, " (",
        round(100 * SnapGsisReport$SnapsStillMissing / SnapGsisReport$SnapsTotal, 3),
        "% of offense + defense snaps, dropped)")

# Season snaps of each player with each franchise; the position group is the
# player's modal roster group with that franchise, else the snap file's group
SnapSeasonMembers <- SnapRows |>
  filter(!is.na(gsis_id)) |>
  group_by(franchise_id, season, gsis_id) |>
  summarise(OffSnaps = sum(offense_snaps, na.rm = TRUE),
            DefSnaps = sum(defense_snaps, na.rm = TRUE),
            SnapPosGroup = names(which.max(table(position_group))) %||% NA_character_,
            .groups = "drop") |>
  left_join(RosterPosition, by = c("gsis_id", "franchise_id", "season")) |>
  mutate(PosGroup = coalesce(RosterPosGroup, SnapPosGroup, "Unknown")) |>
  left_join(PlayerRace, by = "gsis_id")
message("Snap player-franchise-seasons whose snap-file position group differs from the roster group: ",
        round(mean(SnapSeasonMembers$SnapPosGroup != SnapSeasonMembers$RosterPosGroup,
                   na.rm = TRUE), 4),
        "; without a game-day roster row (snap-file group used): ",
        round(mean(is.na(SnapSeasonMembers$RosterPosGroup)), 4))

# ---------------------------------------------------------------------------
# Team-season group definitions (member rows with a Weight)
# ---------------------------------------------------------------------------

SeasonKeys <- c("franchise_id", "season")

SeasonGroups <- list(
  Roster = RosterSeasonMembers,
  Offense = filter(RosterSeasonMembers, PosGroup %in% OffenseGroups),
  Defense = filter(RosterSeasonMembers, PosGroup %in% DefenseGroups),
  SnapW = mutate(SnapSeasonMembers, Weight = OffSnaps + DefSnaps),
  OffenseSnapW = mutate(SnapSeasonMembers, Weight = OffSnaps),
  DefenseSnapW = mutate(SnapSeasonMembers, Weight = DefSnaps)
)
# Position-group snap weights: offense snaps for offensive groups, defense
# snaps for defensive groups
for (g in c(OffenseGroups, DefenseGroups)) {
  SeasonGroups[[paste0(g, "SnapW")]] <- SnapSeasonMembers |>
    filter(PosGroup == g) |>
    mutate(Weight = if (g %in% DefenseGroups) DefSnaps else OffSnaps)
}

# Predetermined (opening-day) groups, fixed before the season's results
# arrive: the active roster of the franchise's first REG game (Week1, one
# weight per player; 2016-2018 rows carry no inactive flag, so game-day
# inactives are included in those seasons), and the same players weighted by
# their offense + defense snaps in the PREVIOUS season with any franchise
# (Week1PriorSnapW, 2014+; players without prior-season snaps, e.g. rookies,
# get zero weight)
FirstWeeks <- GameWeeks |>
  group_by(franchise_id, season) |>
  summarise(week = min(week), .groups = "drop")
Week1Members <- RosterWeeks |>
  semi_join(FirstWeeks, by = c("franchise_id", "season", "week")) |>
  filter(StatusGroup == "Active") |>
  mutate(Weight = 1)
PriorSnaps <- SnapSeasonMembers |>
  group_by(gsis_id, season) |>
  summarise(PriorSnaps = sum(OffSnaps + DefSnaps), .groups = "drop") |>
  mutate(season = season + 1L)
SeasonGroups$Week1 <- Week1Members
SeasonGroups$Week1PriorSnapW <- Week1Members |>
  filter(season >= FirstSnapSeason + 1L) |>
  left_join(PriorSnaps, by = c("gsis_id", "season")) |>
  mutate(Weight = coalesce(PriorSnaps, 0))

SeasonComposition <- SeasonGroups |>
  imap(\(df, g) roster_compose(df, SeasonKeys, g)) |>
  reduce(full_join, by = SeasonKeys)

# Additional predicted-race composition for the main groups
ExtraGroups <- c("Roster", "SnapW", "Week1", "Week1PriorSnapW")
SeasonExtra <- SeasonGroups[ExtraGroups] |>
  imap(\(df, g) roster_compose_extra(df, SeasonKeys, g)) |>
  reduce(full_join, by = SeasonKeys)
SeasonComposition <- left_join(SeasonComposition, SeasonExtra, by = SeasonKeys)

# Defensive share of the snap weight (offense + defense snaps): it responds to
# game script (teams that trail run more offensive plays, teams with weak
# offenses play more defensive snaps), and defensive units have higher Black
# shares, so it is a channel from results to the snap-weighted share
SnapBalance <- SnapSeasonMembers |>
  group_by(franchise_id, season) |>
  summarise(DefenseSnapWeightShare = safe_div(sum(DefSnaps), sum(OffSnaps + DefSnaps)),
            .groups = "drop")
SeasonComposition <- left_join(SeasonComposition, SnapBalance, by = SeasonKeys)

# Expected and residual shares for the main groups
ExpectedGroups <- c("Roster", "SnapW", "Offense", "Defense", "OffenseSnapW", "DefenseSnapW")
SeasonExpected <- SeasonGroups[ExpectedGroups] |>
  imap(\(df, g) roster_expected(df, SeasonKeys, g)) |>
  reduce(full_join, by = SeasonKeys)

SeasonComposition <- SeasonComposition |>
  left_join(SeasonExpected, by = SeasonKeys)
for (g in ExpectedGroups) {
  for (m in c("Hand", "Prov", "Pred", "PredDoc")) {
    SeasonComposition[[glue("ResidualShareBlack{m}{g}")]] <-
      SeasonComposition[[glue("ShareBlack{m}{g}")]] -
      SeasonComposition[[glue("ExpectedShareBlack{m}{g}")]]
  }
}

# ---------------------------------------------------------------------------
# Season starting QB: most REG starts (ties: more offense snaps with the
# franchise, then the later start)
# ---------------------------------------------------------------------------

QBSnaps <- SnapSeasonMembers |>
  select(franchise_id, season, starting_qb_id = gsis_id, QBOffSnaps = OffSnaps)

SeasonQB <- GameWeeks |>
  filter(!is.na(starting_qb_id)) |>
  group_by(franchise_id, season, starting_qb_id) |>
  summarise(Starts = n(), LastStart = max(gameday), .groups = "drop") |>
  left_join(QBSnaps, by = c("franchise_id", "season", "starting_qb_id")) |>
  group_by(franchise_id, season) |>
  arrange(desc(Starts), desc(coalesce(QBOffSnaps, 0)), desc(LastStart), .by_group = TRUE) |>
  summarise(StartingQBId = first(starting_qb_id),
            QBStartShare = first(Starts) / sum(Starts),
            .groups = "drop") |>
  left_join(PlayerRace, by = c(StartingQBId = "gsis_id")) |>
  transmute(franchise_id, season, StartingQBId, QBStartShare,
            QBBlackHand = black_any,
            QBBlackProv = coalesce(black_provisional, 0L),
            QBPBlackBifsg = p_black_bifsg,
            QBBlackPred = p_black_any_pred,
            QBBlackPredDoc = p_black_any_preddoc,
            QBPriorBlackPred = prior_black_pred)

# Opening-day starting QB: the starter of the franchise's first REG game
# (predetermined with respect to the season's results)
Week1QB <- GameWeeks |>
  semi_join(FirstWeeks, by = c("franchise_id", "season", "week")) |>
  left_join(PlayerRace, by = c(starting_qb_id = "gsis_id")) |>
  transmute(franchise_id, season,
            QBWeek1BlackHand = black_any,
            QBWeek1BlackProv = if_else(!is.na(starting_qb_id), coalesce(black_provisional, 0L),
                                       NA_integer_),
            QBWeek1PBlackBifsg = p_black_bifsg,
            QBWeek1BlackPred = p_black_any_pred,
            QBWeek1BlackPredDoc = p_black_any_preddoc,
            QBWeek1PriorBlackPred = prior_black_pred)

# ---------------------------------------------------------------------------
# Roster-quality controls (talent inputs) from player_season
# ---------------------------------------------------------------------------

# Person-season characteristics: log draft pick (undrafted = log 300),
# first-round pick, age on Sep 1, experience (season - rookie season,
# floored at 0)
PlayerSeason <- read_parquet(file.path(analysis, "player_season.parquet"),
                             col_select = c(gsis_id, season, PositionGroup, DraftPick,
                                            DraftRound, Undrafted, Age, Experience,
                                            CapPercent, PayFranchise))
PlayerChars <- PlayerSeason |>
  transmute(gsis_id, season,
            LogPick = if_else(Undrafted == 1L | is.na(DraftPick), log(300), log(DraftPick)),
            FirstRound = as.integer(coalesce(DraftRound, 0L) == 1L),
            Age,
            ExperienceYears = pmax(Experience, 0L))

# Quality block of a weighted group: mean log pick, share first-round, mean
# age and experience, with a name suffix (e.g. Roster, SnapW)
roster_quality <- function(df, suffix, vars = c("MeanLogPick", "ShareFirstRound",
                                                "MeanAge", "MeanExperience")) {
  df |>
    filter(Weight > 0) |>
    left_join(PlayerChars, by = c("gsis_id", "season")) |>
    group_by(franchise_id, season) |>
    summarise(MeanLogPick = wmean(LogPick, Weight),
              ShareFirstRound = wmean(FirstRound, Weight),
              MeanAge = wmean(Age, Weight),
              MeanExperience = wmean(ExperienceYears, Weight),
              .groups = "drop") |>
    select(franchise_id, season, all_of(vars)) |>
    rename_with(\(x) paste0(x, suffix), all_of(vars))
}

unit_vars <- c("MeanLogPick", "MeanAge")
SeasonQuality <- list(
  roster_quality(SeasonGroups$Roster, "Roster"),
  roster_quality(SeasonGroups$SnapW, "SnapW"),
  roster_quality(SeasonGroups$Week1, "Week1"),
  roster_quality(SeasonGroups$Offense, "Roster", unit_vars) |>
    rename_with(\(x) paste0("Offense", x), -all_of(SeasonKeys)),
  roster_quality(SeasonGroups$Defense, "Roster", unit_vars) |>
    rename_with(\(x) paste0("Defense", x), -all_of(SeasonKeys)),
  roster_quality(SeasonGroups$OffenseSnapW, "SnapW", unit_vars) |>
    rename_with(\(x) paste0("Offense", x), -all_of(SeasonKeys)),
  roster_quality(SeasonGroups$DefenseSnapW, "SnapW", unit_vars) |>
    rename_with(\(x) paste0("Defense", x), -all_of(SeasonKeys))
) |>
  reduce(full_join, by = SeasonKeys)

# Cap share: sum of CapPercent (a share of the team cap) over players whose
# PayFranchise is the franchise; unit versions by the player's position
# group. OTC cap tables are sparse before 2011 and 2010 had no cap
# (CapPercent = 0), so the team sums are kept from 2013 on only (see the
# distribution reported below).
FirstCapSeason <- 2013L
CapByPlayer <- PlayerSeason |>
  filter(!is.na(PayFranchise), !is.na(CapPercent)) |>
  transmute(franchise_id = PayFranchise, season, CapPercent,
            Unit = case_when(PositionGroup %in% OffenseGroups ~ "Offense",
                             PositionGroup %in% DefenseGroups ~ "Defense",
                             TRUE ~ "Other"))
CapAll <- CapByPlayer |>
  group_by(franchise_id, season) |>
  summarise(TeamCapShare = sum(CapPercent),
            OffenseCapShare = sum(CapPercent[Unit == "Offense"]),
            DefenseCapShare = sum(CapPercent[Unit == "Defense"]),
            .groups = "drop")
message("TeamCapShare (sum of CapPercent by PayFranchise) by season, all seasons (kept from 2013 on):")
CapAll |>
  group_by(season) |>
  summarise(Teams = n(), Mean = mean(TeamCapShare), Min = min(TeamCapShare),
            Max = max(TeamCapShare), .groups = "drop") |>
  print(n = Inf)
CapShares <- CapAll |> filter(season >= FirstCapSeason)

# ---------------------------------------------------------------------------
# Team-game composition (REG games): the week's game-day roster
# ---------------------------------------------------------------------------

GameKeys <- c("franchise_id", "season", "game_id")
GameMembers <- RosterWeeks |> mutate(Weight = 1)
GameGroups <- list(
  ActiveRoster = filter(GameMembers, StatusGroup == "Active"),
  ActiveOffense = filter(GameMembers, StatusGroup == "Active", PosGroup %in% OffenseGroups),
  ActiveDefense = filter(GameMembers, StatusGroup == "Active", PosGroup %in% DefenseGroups),
  GameDayRoster = GameMembers
)
GameComposition <- GameGroups |>
  imap(\(df, g) roster_compose(df, GameKeys, g)) |>
  reduce(full_join, by = GameKeys) |>
  left_join(roster_compose_extra(GameGroups$ActiveRoster, GameKeys, "ActiveRoster"),
            by = GameKeys) |>
  left_join(roster_expected(GameGroups$ActiveRoster, GameKeys, "ActiveRoster"),
            by = GameKeys) |>
  mutate(ResidualShareBlackHandActiveRoster = ShareBlackHandActiveRoster - ExpectedShareBlackHandActiveRoster,
         ResidualShareBlackProvActiveRoster = ShareBlackProvActiveRoster - ExpectedShareBlackProvActiveRoster,
         ResidualShareBlackPredActiveRoster = ShareBlackPredActiveRoster - ExpectedShareBlackPredActiveRoster,
         ResidualShareBlackPredDocActiveRoster = ShareBlackPredDocActiveRoster - ExpectedShareBlackPredDocActiveRoster)

GameQB <- GameWeeks |>
  left_join(PlayerRace, by = c(starting_qb_id = "gsis_id")) |>
  transmute(franchise_id, game_id,
            GameQBBlackHand = black_any,
            GameQBBlackProv = if_else(!is.na(starting_qb_id), coalesce(black_provisional, 0L),
                                      NA_integer_),
            GameQBPBlackBifsg = p_black_bifsg,
            GameQBBlackPred = p_black_any_pred,
            GameQBBlackPredDoc = p_black_any_preddoc,
            GameQBPriorBlackPred = prior_black_pred)

RosterTeamGame <- GameWeeks |>
  select(franchise_id, game_id, season, week) |>
  left_join(GameComposition, by = GameKeys) |>
  left_join(GameQB, by = c("franchise_id", "game_id")) |>
  mutate(across(c(NActiveRoster, NActiveOffense, NActiveDefense, NGameDayRoster),
                \(x) coalesce(x, 0L)),
         across(c(WActiveRoster, WActiveOffense, WActiveDefense, WGameDayRoster),
                \(x) coalesce(x, 0))) |>
  arrange(season, week, franchise_id)

# ---------------------------------------------------------------------------
# Assemble the team-season sample
# ---------------------------------------------------------------------------

TeamSeasonKeys <- read_parquet(file.path(analysis, "team_season.parquet"),
                               col_select = c(franchise_id, season)) |>
  filter(season >= FirstRosterSeason, season <= LastSeason)

RosterTeamSeason <- TeamSeasonKeys |>
  left_join(SeasonComposition, by = SeasonKeys) |>
  left_join(SeasonQB, by = SeasonKeys) |>
  left_join(Week1QB, by = SeasonKeys) |>
  left_join(SeasonQuality, by = SeasonKeys) |>
  left_join(CapShares, by = SeasonKeys) |>
  # Snap groups are unobserved (NA, not 0) before 2013; headcount groups are
  # observed from 2002, so an empty group has N = 0
  mutate(across(c(starts_with("N") & where(is.integer)),
                \(x) if (str_detect(cur_column(), "SnapW$")) x else coalesce(x, 0L))) |>
  arrange(franchise_id, season)

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

SeasonGroupDescriptions <- c(
  Roster = "the game-day roster (Active + Inactive in REG game weeks), weighted by each player's game-day weeks with the franchise",
  Offense = "offensive players (roster position group QB, RB, WR, TE, OL) on the game-day roster, weighted by game-day weeks",
  Defense = "defensive players (roster position group DL, LB, DB) on the game-day roster, weighted by game-day weeks",
  SnapW = "players weighted by REG offense + defense snaps (2013+; NA before)",
  OffenseSnapW = "players weighted by REG offense snaps (2013+; NA before)",
  DefenseSnapW = "players weighted by REG defense snaps (2013+; NA before)",
  Week1 = "the active roster of the franchise's first REG game of the season (opening day; predetermined), one weight per player; 2016-2018 rows carry no inactive flag, so inactives are included in those seasons",
  Week1PriorSnapW = "the opening-day active roster weighted by each player's REG offense + defense snaps in the previous season with any franchise (2014+; NA before; players without prior-season snaps get zero weight)"
)
for (g in OffenseGroups) {
  SeasonGroupDescriptions[[paste0(g, "SnapW")]] <-
    glue("{g}s (modal roster position group with the franchise) weighted by REG offense snaps (2013+)")
}
for (g in DefenseGroups) {
  SeasonGroupDescriptions[[paste0(g, "SnapW")]] <-
    glue("{g}s (modal roster position group with the franchise) weighted by REG defense snaps (2013+)")
}
GameGroupDescriptions <- c(
  ActiveRoster = "the game-day active roster of the game week (status ACT, not a game-day inactive; 2016-2018 rows carry no inactive flag, so inactives are included in those seasons), one weight per player",
  ActiveOffense = "offensive players (QB, RB, WR, TE, OL) on the game-day active roster",
  ActiveDefense = "defensive players (DL, LB, DB) on the game-day active roster",
  GameDayRoster = "the game-day roster of the game week (Active + Inactive)"
)

roster_group_labels <- function(group, desc) {
  c(N = glue("Number of distinct players in {desc}"),
    W = glue("Total weight of {desc}"),
    CodedShare = glue("Weighted share of {desc} with a hand-coded black_any"),
    ShareBlackHand = glue("Weighted Black share among hand-coded members of {desc} (NA when none coded; use with CodedShare)"),
    ShareBlackProv = glue("Weighted share of {desc} flagged Black by black_provisional among ALL members (unflagged count as 0); positive-only lower bound"),
    MeanPBlackBifsg = glue("Weighted mean BIFSG P(Black) of {desc} (secondary; understates Black share)"),
    ShareOtherHand = glue("Weighted share of hand-coded members of {desc} who are neither Black nor white (person_race_group() 'Other', e.g. Hispanic, Asian, Pacific Islander); NA when none coded; not coverage-gated"),
    ShareOtherProv = glue("Weighted share of {desc} not flagged Black and flagged other (hand 'Other' where coded, else a Wikipedia Hispanic/Asian/Pacific Islander/Native American category) among ALL members; positive-only"),
    ArticleShare = glue("Weighted share of {desc} with a Wikipedia article (non-missing Wikipedia race-category signal); gauges the provisional measure's coverage"),
    ShareBlackPred = glue("Expected Black share of {desc}: weighted mean of members' model-only predicted P(non-Hispanic Black alone) (p_black_any_pred, which equals p_black_pred: multiracial and Hispanic Black players count as non-Black; BIFSG name/county likelihood x NFL prior; primary measure while hand codes are absent)"),
    ShareBlackPredDoc = glue("Weighted mean of members' P(Black) under the documented-race variant (p_black_any_preddoc: 1 for documented Black alone or in combination, 0 for other documented race, else the model P(non-Hispanic Black alone)) for {desc}; fame-dependent, sensitivity only"),
    MeanPriorBlackPred = glue("Weighted mean of members' EM prior P(Black) (prior_black_pred; function of position at entry, rookie era, draft bucket, college type and hometown-county availability) for {desc}; summarizes the prior's covariates along the prior index only (the Share<covariate level> columns give their full composition for the main groups)"),
    ShareOtherPred = glue("Weighted mean of members' predicted P(neither Black nor white) = 1 - p_black_any_pred - p_white_pred (floored at 0) for {desc}"),
    ShareOtherPredDoc = glue("Weighted mean of members' P(neither Black nor white) under the documented-race variant (1 - p_black_any_preddoc - p_white_preddoc) for {desc}")) |>
    set_names(\(x) paste0(x, group))
}
extra_labels <- function(group, desc) {
  c(ShareBlackNoDraft = glue("Expected Black share of {desc} under the prior without draft round (weighted mean p_black_pred_nodraft, P(non-Hispanic Black alone)); does not proxy draft capital"),
    MeanPriorBlackNoDraft = glue("Weighted mean EM prior P(Black) without draft round (prior_black_pred_nodraft) of {desc}"),
    ShareBlackOrMulti = glue("Weighted mean predicted P(non-Hispanic Black alone or non-Hispanic multiracial) (p_black_or_multi_pred) of {desc}; counts multiracial players of any combination, an upper bound for Black alone or in combination"),
    ShareBlackRaked = glue("Weighted mean predicted P(Black), raked to the TIDES league margins (p_black_pred_raked), of {desc}; calibration sensitivity"),
    MeanPriorBlackPredDoc = glue("Weighted mean preddoc prior P(Black) (prior_black_preddoc: prior of the documented variant, fitted on undocumented players with fame proxies) of {desc}; calibration control under the preddoc measure"),
    BerksonVarPred = glue("Berkson variance of ShareBlackPred for {desc}: sum_i w_i^2 p_i (1 - p_i) / (sum_i w_i)^2 with p = p_black_any_pred (variance of the true minus the expected share if races are independent across members given the predictions)"),
    BerksonVarPredDoc = glue("Berkson variance of ShareBlackPredDoc for {desc} (as BerksonVarPred with p_black_any_preddoc)"),
    DocShare = glue("Weight share of {desc} with documented race (documented_black_any; validation only)"),
    ShareBlackDoc = glue("Weighted documented Black share (alone or in combination) among documented members of {desc} (validation only; documentation is selected on fame)"),
    DocMeanPBlack = glue("Weighted mean model-only p_black_any_pred among the documented members of {desc} (pairs with ShareBlackDoc for the team-level calibration check)"),
    set_names(glue("Weighted share of {desc} with prior covariate {CovLevels$covariate} = {CovLevels$level}"),
              CovLevels$CovVar) |>
      set_names(\(x) paste0("Share", x))) |>
    set_names(\(x) paste0(x, group))
}
expected_labels <- function(group, desc) {
  c(ExpectedShareBlackHand = glue("Expected hand-coded Black share of {desc} given its position mix: sum over position groups of the team's share of hand-coded weight in the group x the league-season Black share in the group among hand-coded weight of all OTHER franchises"),
    ExpectedShareBlackProv = glue("Expected provisional Black share of {desc} given its position mix: sum over position groups of the team's weight share in the group x the league-season flagged share in the group on all OTHER franchises (same weights)"),
    ResidualShareBlackHand = glue("ShareBlackHand minus ExpectedShareBlackHand for {desc} (diversity net of position mix)"),
    ResidualShareBlackProv = glue("ShareBlackProv minus ExpectedShareBlackProv for {desc} (diversity net of position mix)"),
    ExpectedShareBlackPred = glue("Expected predicted Black share of {desc} given its position mix: sum over position groups of the team's weight share in the group x the league-season mean p_black_any_pred in the group on all OTHER franchises (same weights)"),
    ExpectedShareBlackPredDoc = glue("Expected documented-variant Black share of {desc} given its position mix: as ExpectedShareBlackPred with p_black_any_preddoc"),
    ResidualShareBlackPred = glue("ShareBlackPred minus ExpectedShareBlackPred for {desc} (diversity net of position mix)"),
    ResidualShareBlackPredDoc = glue("ShareBlackPredDoc minus ExpectedShareBlackPredDoc for {desc} (diversity net of position mix)")) |>
    set_names(\(x) paste0(x, group))
}

RosterTeamSeasonLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  season = "NFL season (2002-2025; weekly rosters start in 2002)",
  unlist(unname(imap(SeasonGroupDescriptions, \(d, g) roster_group_labels(g, d)))),
  unlist(unname(imap(SeasonGroupDescriptions[ExpectedGroups], \(d, g) expected_labels(g, d)))),
  unlist(unname(imap(SeasonGroupDescriptions[ExtraGroups], \(d, g) extra_labels(g, d)))),
  DefenseSnapWeightShare = "Defensive share of the snap weight: REG defense snaps / (offense + defense snaps) of the franchise's players (2013+; responds to game script)",
  StartingQBId = "Season starting QB (gsis_id): most REG starts in nfl_team_games (ties: more offense snaps with the franchise, then the later start)",
  QBStartShare = "Share of the franchise's REG games started by StartingQBId",
  QBBlackHand = "Season starting QB hand-coded Black (NA until coded)",
  QBBlackProv = "Season starting QB flagged Black by black_provisional (1) or not flagged (0); lower bound",
  QBPBlackBifsg = "Season starting QB BIFSG P(Black)",
  QBBlackPred = "Season starting QB model-only predicted P(non-Hispanic Black alone) (p_black_any_pred)",
  QBBlackPredDoc = "Season starting QB P(Black any), documented-race variant (p_black_any_preddoc; sensitivity)",
  QBPriorBlackPred = "Season starting QB EM prior P(Black) (prior_black_pred; regression-calibration control for QBBlackPred)",
  QBWeek1BlackHand = "Opening-day starting QB (starter of the franchise's first REG game; predetermined) hand-coded Black (NA until coded)",
  QBWeek1BlackProv = "Opening-day starting QB flagged Black by black_provisional (1) or not flagged (0); lower bound",
  QBWeek1PBlackBifsg = "Opening-day starting QB BIFSG P(Black)",
  QBWeek1BlackPred = "Opening-day starting QB model-only predicted P(non-Hispanic Black alone) (p_black_any_pred)",
  QBWeek1BlackPredDoc = "Opening-day starting QB P(Black any), documented-race variant (sensitivity)",
  QBWeek1PriorBlackPred = "Opening-day starting QB EM prior P(Black) (regression-calibration control)",
  MeanLogPickWeek1 = "Mean log overall draft pick of the opening-day active roster (undrafted = log 300; predetermined)",
  ShareFirstRoundWeek1 = "Share of first-round picks on the opening-day active roster (predetermined)",
  MeanAgeWeek1 = "Mean age (Sep 1) of the opening-day active roster (predetermined)",
  MeanExperienceWeek1 = "Mean experience (season - rookie season, floored at 0) of the opening-day active roster (predetermined)",
  MeanLogPickRoster = "Mean log overall draft pick of the game-day roster, weighted by game-day weeks (undrafted = log 300)",
  ShareFirstRoundRoster = "Share of first-round picks on the game-day roster, weighted by game-day weeks",
  MeanAgeRoster = "Mean age (Sep 1) of the game-day roster, weighted by game-day weeks",
  MeanExperienceRoster = "Mean experience (season - rookie season, floored at 0) of the game-day roster, weighted by game-day weeks",
  MeanLogPickSnapW = "Mean log overall draft pick, weighted by offense + defense snaps (undrafted = log 300; 2013+)",
  ShareFirstRoundSnapW = "Snap-weighted (offense + defense) share of first-round picks (2013+)",
  MeanAgeSnapW = "Snap-weighted (offense + defense) mean age on Sep 1 (2013+)",
  MeanExperienceSnapW = "Snap-weighted (offense + defense) mean experience, floored at 0 (2013+)",
  OffenseMeanLogPickRoster = "Mean log draft pick of offensive game-day players (QB-OL), weighted by game-day weeks (undrafted = log 300)",
  DefenseMeanLogPickRoster = "Mean log draft pick of defensive game-day players (DL-DB), weighted by game-day weeks (undrafted = log 300)",
  OffenseMeanAgeRoster = "Mean age of offensive game-day players, weighted by game-day weeks",
  DefenseMeanAgeRoster = "Mean age of defensive game-day players, weighted by game-day weeks",
  OffenseMeanLogPickSnapW = "Mean log draft pick weighted by offense snaps (2013+; undrafted = log 300)",
  DefenseMeanLogPickSnapW = "Mean log draft pick weighted by defense snaps (2013+; undrafted = log 300)",
  OffenseMeanAgeSnapW = "Mean age weighted by offense snaps (2013+)",
  DefenseMeanAgeSnapW = "Mean age weighted by defense snaps (2013+)",
  TeamCapShare = "Sum of CapPercent (share of the team cap, OTC) over player-seasons whose PayFranchise is the franchise (2013+; NA before because OTC cap tables are sparse before 2011 and 2010 was uncapped). Below 1 because dead money and unmatched players are missing",
  OffenseCapShare = "TeamCapShare over players in offensive position groups (QB-OL; player_season PositionGroup; 2013+)",
  DefenseCapShare = "TeamCapShare over players in defensive position groups (DL-DB; 2013+)"
)

RosterTeamGameLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  game_id = "nflverse game identifier (REG games 2002-2025)",
  season = "NFL season", week = "REG week",
  unlist(unname(imap(GameGroupDescriptions, \(d, g) roster_group_labels(g, d)))),
  expected_labels("ActiveRoster", "the game-day active roster (league position-group shares computed per season over all other franchises' active team-game rosters)"),
  extra_labels("ActiveRoster", GameGroupDescriptions[["ActiveRoster"]]),
  GameQBBlackHand = "Game starting QB hand-coded Black (NA until coded)",
  GameQBBlackProv = "Game starting QB flagged Black by black_provisional (1) or not flagged (0); lower bound",
  GameQBPBlackBifsg = "Game starting QB BIFSG P(Black)",
  GameQBBlackPred = "Game starting QB model-only predicted P(non-Hispanic Black alone) (p_black_any_pred)",
  GameQBBlackPredDoc = "Game starting QB P(Black any), documented-race variant (sensitivity)",
  GameQBPriorBlackPred = "Game starting QB EM prior P(Black) (regression-calibration control)"
)

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

n_expected <- nrow(TeamSeasonKeys)
if (nrow(RosterTeamSeason) != n_expected) {
  stop("roster_composition_team_season has ", nrow(RosterTeamSeason), " rows, expected ", n_expected)
}
n_games <- nrow(GameWeeks)
if (nrow(RosterTeamGame) != n_games) {
  stop("roster_composition_team_game has ", nrow(RosterTeamGame), " rows; REG team-games: ", n_games)
}

# Shares in [0, 1] (residuals in [-1, 1]); counts and weights >= 0
check_ranges <- function(df, name) {
  share_cols <- names(df)[str_detect(names(df),
    "^(CodedShare|ShareBlack|ShareOther|ArticleShare|ShareFirstRound|MeanPBlack|MeanPrior|ExpectedShare|QBStartShare|QBBlack|QBPBlack|QBPrior|QBWeek1|GameQB|ShareDraft|ShareCollege|ShareEra|ShareCounty|DocShare|DocMeanPBlack|BerksonVar|DefenseSnapWeightShare)")]
  resid_cols <- names(df)[str_detect(names(df), "^ResidualShare")]
  count_cols <- names(df)[str_detect(names(df), "^(N|W)[A-Z]")]
  bad <- c(share_cols[map_lgl(df[share_cols], \(x) any(x < -1e-12 | x > 1 + 1e-12, na.rm = TRUE))],
           resid_cols[map_lgl(df[resid_cols], \(x) any(abs(x) > 1 + 1e-12, na.rm = TRUE))],
           count_cols[map_lgl(df[count_cols], \(x) any(x < 0, na.rm = TRUE))])
  if (length(bad) > 0) stop(name, ": out-of-range values in ", paste(bad, collapse = ", "))
  invisible(TRUE)
}
check_ranges(RosterTeamSeason, "roster_composition_team_season")
check_ranges(RosterTeamGame, "roster_composition_team_game")

# Roster weights add up: game-day weeks of the season = sum over the
# franchise's REG games of the game-day roster size
WeightCheck <- RosterTeamGame |>
  group_by(franchise_id, season) |>
  summarise(GameW = sum(WGameDayRoster), .groups = "drop") |>
  inner_join(RosterTeamSeason |> select(franchise_id, season, WRoster), by = SeasonKeys)
if (any(abs(WeightCheck$GameW - WeightCheck$WRoster) > 1e-9, na.rm = TRUE)) {
  stop("Game-day roster weights disagree between the team-season and team-game samples")
}

# Name check: the generic names apply_race_measure() creates must not collide
# with each other or with existing columns
# ("BlackPred" excludes "BlackPredDoc", as in apply_race_measure())
check_race_names <- function(df, name) {
  for (tag in c("Hand", "Prov", "Pred", "PredDoc")) {
    pattern <- paste0("Black", tag, if (tag == "Pred") "(?!Doc)" else "")
    src <- grep(pattern, names(df), value = TRUE, perl = TRUE)
    generic <- sub(pattern, "Black", src, perl = TRUE)
    if (anyDuplicated(generic) || any(generic %in% names(df))) {
      stop(name, ": apply_race_measure() name collision for ", tag, ": ",
           paste(generic[duplicated(generic) | generic %in% names(df)], collapse = ", "))
    }
  }
  invisible(TRUE)
}
check_race_names(RosterTeamSeason, "roster_composition_team_season")
check_race_names(RosterTeamGame, "roster_composition_team_game")

write_sample(RosterTeamSeason, "roster_composition_team_season", key = SeasonKeys,
             labels = RosterTeamSeasonLabels)
write_sample(RosterTeamGame, "roster_composition_team_game",
             key = c("franchise_id", "game_id"), labels = RosterTeamGameLabels)

# ---------------------------------------------------------------------------
# Coverage report
# ---------------------------------------------------------------------------

message("REG team-games with a non-empty active roster: ",
        round(mean(RosterTeamGame$NActiveRoster > 0), 4), " (",
        sum(RosterTeamGame$NActiveRoster > 0), " of ", nrow(RosterTeamGame), ")")
RosterTeamGame |>
  group_by(Era = case_when(season <= 2015 ~ "2002-2015", season <= 2018 ~ "2016-2018",
                           TRUE ~ "2019-2025")) |>
  summarise(MeanActive = mean(NActiveRoster), MeanGameDay = mean(NGameDayRoster),
            .groups = "drop") |>
  print()
RosterTeamSeason |>
  group_by(Period = if_else(season <= 2012, "2002-2012", "2013-2025")) |>
  summarise(TeamSeasons = n(),
            CodedShareRoster = mean(CodedShareRoster, na.rm = TRUE),
            HasShareProvRoster = mean(!is.na(ShareBlackProvRoster)),
            HasShareProvSnapW = mean(!is.na(ShareBlackProvSnapW)),
            HasQBRace = mean(!is.na(QBBlackProv)),
            HasCapShare = mean(!is.na(TeamCapShare)),
            .groups = "drop") |>
  print()
RosterTeamSeason |>
  filter(season %in% c(2002, 2008, 2013, 2019, 2025)) |>
  group_by(season) |>
  summarise(ShareBlackProvRoster = mean(ShareBlackProvRoster),
            ShareBlackProvSnapW = mean(ShareBlackProvSnapW),
            ExpectedShareBlackProvRoster = mean(ExpectedShareBlackProvRoster),
            ShareBlackPredRoster = mean(ShareBlackPredRoster),
            ShareBlackPredSnapW = mean(ShareBlackPredSnapW),
            ShareBlackPredDocRoster = mean(ShareBlackPredDocRoster),
            MeanPriorBlackPredRoster = mean(MeanPriorBlackPredRoster),
            .groups = "drop") |>
  print(width = Inf)

# Predicted measures by period (expected Black shares; compare with the TIDES
# league shares of roughly 0.55-0.70)
RosterTeamSeason |>
  group_by(Period = if_else(season <= 2012, "2002-2012", "2013-2025")) |>
  summarise(across(c(ShareBlackPredRoster, ShareBlackPredDocRoster, ShareBlackPredSnapW,
                     ShareBlackPredDocSnapW, MeanPriorBlackPredRoster, QBBlackPred,
                     ShareOtherPredRoster),
                   \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  print(width = Inf)

# Variant expected shares, Berkson SD of the expected share, documented
# coverage and the defensive snap-weight share by period
RosterTeamSeason |>
  group_by(Period = if_else(season <= 2012, "2002-2012", "2013-2025")) |>
  summarise(across(c(ShareBlackNoDraftRoster, ShareBlackOrMultiRoster, ShareBlackRakedRoster,
                     ShareBlackNoDraftSnapW, ShareBlackOrMultiSnapW, ShareBlackRakedSnapW,
                     DocShareRoster, ShareBlackDocRoster, DocMeanPBlackRoster,
                     DefenseSnapWeightShare), \(x) mean(x, na.rm = TRUE)),
            BerksonSDPredRoster = sqrt(mean(BerksonVarPredRoster, na.rm = TRUE)),
            BerksonSDPredSnapW = sqrt(mean(BerksonVarPredSnapW, na.rm = TRUE)),
            .groups = "drop") |>
  print(width = Inf)

db_disconnect(con)

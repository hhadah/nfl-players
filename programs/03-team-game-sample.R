# ============================================================================
# 03-team-game-sample.R
# Builds analysis/team_game: one row per franchise_id x game_id (REG + POST,
# 1999-2025; the rows of nfl_team_games). Adds
#   - game outcomes and market benchmarks (nfl_team_games),
#   - the game's head coach (nflverse schedule, corrected with verified
#     in-season change dates and, failing those, with Wikipedia interim
#     overrides where staff_hc_reconciliation shows nflverse missed a firing;
#     rule in load_game_head_coaches(), 00-setup-functions.R) and HC race,
#   - TIME-VARYING staff composition: each game gets the staff snapshot in
#     force on game day. Template era (2007+): the preseason (Sep 10)
#     snapshot for games before Nov 1, the midseason (Nov 1) snapshot from
#     Nov 1 to the day before the late-snapshot date, the late snapshot from
#     then on (incl. playoffs). Article era (1999-2006): the season article.
#     Black-share measures (hand-coded, provisional lower bound, BIFSG mean)
#     for coaches, coordinators, offensive coaches, defensive coaches and front
#     office, plus OC and DC race for the offense-vs-defense unit designs.
# Uses the shared helpers in 00-setup-functions.R and 00-race-measures.R.
# Date: 2026-09-26
# ============================================================================

con <- db_connect()

# ---------------------------------------------------------------------------
# Game outcomes and market benchmarks
# ---------------------------------------------------------------------------

Games <- tbl(con, "nfl_team_games") |>
  select(franchise_id, game_id, season, game_type, season_type, week, gameday,
         team_code, opponent_franchise_id, home, neutral_site, div_game,
         rest_days, opponent_rest_days, points_for, points_against, margin,
         win, loss, tie, off_epa_per_play, def_epa_per_play, off_success_rate,
         def_success_rate, off_plays, def_plays, team_spread_line, total_line,
         implied_win_prob, implied_win_prob_source, starting_qb_id,
         pbp_available) |>
  collect() |>
  mutate(season = as.integer(season), gameday = as.Date(gameday),
         Home = as.integer(home), NeutralSite = as.integer(neutral_site),
         WinOrHalfTie = win + 0.5 * tie,
         WinOverExpected = WinOrHalfTie - implied_win_prob) |>
  select(-home, -neutral_site)

# ---------------------------------------------------------------------------
# Head coach of the game and HC race
# ---------------------------------------------------------------------------

StaffRace <- load_person_race(con, hand_coded) |>
  filter(entity == "staff") |>
  select(person_id, black_any, nonwhite, black_provisional, p_black_bifsg)

GameHC <- load_game_head_coaches(con) |>
  select(franchise_id, game_id, HeadCoachName, HeadCoachPersonId, HCMatchMethod,
         HCSource, HCChangeWindow) |>
  left_join(StaffRace, by = c(HeadCoachPersonId = "person_id")) |>
  mutate(HCBlackHand = black_any, HCNonwhiteHand = nonwhite,
         HCBlackProv = if_else(!is.na(HeadCoachPersonId),
                               coalesce(black_provisional, 0L), NA_integer_),
         HCPBlackBifsg = p_black_bifsg) |>
  select(-black_any, -nonwhite, -black_provisional, -p_black_bifsg)

# ---------------------------------------------------------------------------
# Staff snapshot in force on game day
# ---------------------------------------------------------------------------

# Late-snapshot date per team-season (Dec 31, or the day after the team's
# last REG game when earlier)
LateDates <- tbl(con, "staff_snapshots") |>
  filter(snapshot == "late") |>
  select(franchise_id, season, LateDate = target_date) |>
  collect() |>
  mutate(season = as.integer(season))

Games <- Games |>
  left_join(LateDates, by = c("franchise_id", "season")) |>
  mutate(Nov1 = as.Date(paste0(season, "-11-01")),
         StaffSnapshot = case_when(season < 2007 ~ "season_article",
                                   gameday < Nov1 ~ "preseason",
                                   gameday < LateDate ~ "midseason",
                                   TRUE ~ "late")) |>
  select(-Nov1, -LateDate)

# ---------------------------------------------------------------------------
# Staff composition by snapshot
# ---------------------------------------------------------------------------

coach_groups <- c("head_coach", "coordinator", "position_coach", "assistant_coach")
fo_groups <- c("owner_executive", "general_manager", "personnel_scouting",
               "other_front_office")

# Person x role rows expanded to the snapshots they are listed in, then
# collapsed to one row per snapshot x person with group flags
SnapshotStaff <- tbl(con, "staff_team_season") |>
  select(franchise_id, season, person_id, role_std, role_group, unit,
         in_preseason, in_midseason, in_late, in_season_article) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  pivot_longer(c(in_preseason, in_midseason, in_late, in_season_article),
               names_to = "StaffSnapshot", names_prefix = "in_", values_to = "Listed") |>
  filter(Listed) |>
  group_by(franchise_id, season, StaffSnapshot, person_id) |>
  summarise(IsCoach = any(role_group %in% coach_groups),
            IsCoordinator = any(role_std %in% c("OC", "DC", "STC")),
            IsOC = any(role_std == "OC"),
            IsDC = any(role_std == "DC"),
            IsOffenseCoach = any(role_group %in% coach_groups & unit == "offense"),
            IsDefenseCoach = any(role_group %in% coach_groups & unit == "defense"),
            IsFrontOffice = any(role_group %in% fo_groups),
            .groups = "drop") |>
  left_join(StaffRace, by = "person_id")

# Composition of one group in each snapshot (the three Black-share measures
# are kept separate; CodedShare is the coverage of hand codes)
compose_snapshot <- function(flag, suffix) {
  SnapshotStaff |>
    filter(.data[[flag]]) |>
    group_by(franchise_id, season, StaffSnapshot) |>
    summarise(N = n(),
              CodedShare = mean(!is.na(black_any)),
              ShareBlackHand = mean_or_na(black_any),
              ShareBlackProv = sum(black_provisional == 1, na.rm = TRUE) / n(),
              MeanPBlackBifsg = mean_or_na(p_black_bifsg),
              .groups = "drop") |>
    rename_with(\(x) paste0(x, suffix), -c(franchise_id, season, StaffSnapshot))
}

SnapshotComposition <- list(IsCoach = "Coaches", IsCoordinator = "Coordinators",
                            IsOffenseCoach = "OffenseCoaches",
                            IsDefenseCoach = "DefenseCoaches",
                            IsFrontOffice = "FrontOffice") |>
  imap(\(suffix, flag) compose_snapshot(flag, suffix)) |>
  reduce(full_join, by = c("franchise_id", "season", "StaffSnapshot"))

# OC and DC race in each snapshot (unit designs). Several listed holders
# (co-coordinators) are averaged, so each variable is the share of holders.
coordinator_race <- function(flag, prefix) {
  SnapshotStaff |>
    filter(.data[[flag]]) |>
    group_by(franchise_id, season, StaffSnapshot) |>
    summarise(N = n(),
              BlackHand = mean_or_na(black_any),
              BlackProv = sum(black_provisional == 1, na.rm = TRUE) / n(),
              PBlackBifsg = mean_or_na(p_black_bifsg),
              .groups = "drop") |>
    rename_with(\(x) paste0(prefix, x), -c(franchise_id, season, StaffSnapshot))
}

CoordinatorRace <- full_join(coordinator_race("IsOC", "OC"),
                             coordinator_race("IsDC", "DC"),
                             by = c("franchise_id", "season", "StaffSnapshot"))

# ---------------------------------------------------------------------------
# Assemble the team-game sample
# ---------------------------------------------------------------------------

StaffObservedSnapshots <- SnapshotComposition |>
  distinct(franchise_id, season, StaffSnapshot) |>
  mutate(SnapshotObserved = TRUE)

TeamGame <- Games |>
  left_join(GameHC, by = c("franchise_id", "game_id")) |>
  left_join(StaffObservedSnapshots, by = c("franchise_id", "season", "StaffSnapshot")) |>
  left_join(SnapshotComposition, by = c("franchise_id", "season", "StaffSnapshot")) |>
  left_join(CoordinatorRace, by = c("franchise_id", "season", "StaffSnapshot")) |>
  # Counts are 0 (not NA) when the snapshot is observed but lists nobody in
  # the group
  mutate(SnapshotObserved = coalesce(SnapshotObserved, FALSE),
         across(c(NCoaches, NCoordinators, NOffenseCoaches, NDefenseCoaches,
                  NFrontOffice, OCN, DCN),
                \(x) if_else(SnapshotObserved, coalesce(x, 0L), x))) |>
  rename(NOC = OCN, NDC = DCN) |>
  relocate(franchise_id, game_id, season, game_type, season_type, week, gameday) |>
  arrange(season, gameday, game_id, franchise_id)

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

snapshot_group_labels <- function(group, desc) {
  c(N = glue("Number of {desc} in the snapshot in force (0 when observed and none listed; NA when unobserved)"),
    CodedShare = glue("Share of {desc} in the snapshot with a hand-coded black_any"),
    ShareBlackHand = glue("Share Black among hand-coded {desc} in the snapshot (NA until coded)"),
    ShareBlackProv = glue("Share of {desc} in the snapshot flagged Black by black_provisional; positive-only lower bound"),
    MeanPBlackBifsg = glue("Mean BIFSG P(Black) of {desc} in the snapshot (secondary measure)")) |>
    set_names(\(x) paste0(x, group))
}
unit_race_labels <- function(prefix, role) {
  c(BlackHand = glue("Share of {role}s in the snapshot hand-coded Black (NA until coded)"),
    BlackProv = glue("Share of {role}s in the snapshot flagged Black by black_provisional (lower bound)"),
    PBlackBifsg = glue("Mean BIFSG P(Black) of the snapshot's {role}s")) |>
    set_names(\(x) paste0(prefix, x))
}

TeamGameLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  game_id = "nflverse game identifier",
  season = "NFL season", game_type = "Game type (REG, WC, DIV, CON, SB)",
  season_type = "REG or POST", week = "Week", gameday = "Game date",
  team_code = "nflverse team code in that season",
  opponent_franchise_id = "Opponent franchise identifier",
  div_game = "Division game (1/0)",
  rest_days = "Days of rest before the game", opponent_rest_days = "Opponent days of rest",
  points_for = "Points scored", points_against = "Points allowed",
  margin = "Points for minus points against",
  win = "Win (1/0)", loss = "Loss (1/0)", tie = "Tie (1/0)",
  off_epa_per_play = "Offensive EPA per play (non-penalty scrimmage plays)",
  def_epa_per_play = "Defensive EPA per play allowed (lower is better)",
  off_success_rate = "Offensive success rate", def_success_rate = "Defensive success rate allowed",
  off_plays = "Offensive plays", def_plays = "Defensive plays",
  team_spread_line = "Closing spread from the team's perspective (positive = favored)",
  total_line = "Closing over/under total",
  implied_win_prob = "Pre-game implied win probability (vig-free moneyline, else spread mapping)",
  implied_win_prob_source = "Source of implied_win_prob: moneyline or spread",
  starting_qb_id = "Starting QB gsis_id",
  pbp_available = "Play-by-play available for the game",
  Home = "Home team (1/0)", NeutralSite = "Neutral site (1/0)",
  WinOrHalfTie = "Win = 1, tie = 0.5, loss = 0",
  WinOverExpected = "WinOrHalfTie minus implied_win_prob",
  StaffSnapshot = "Staff snapshot in force on game day: preseason (before Nov 1), midseason (Nov 1 to the day before the late-snapshot date), late (from the late-snapshot date, incl. playoffs), season_article (1999-2006)",
  HeadCoachName = "Head coach of the game: nflverse schedule coach, corrected by HCSpellCorrections (verified in-season change dates) and otherwise by the Wikipedia interim HC on or after the first snapshot revision listing him when nflverse missed a firing",
  HeadCoachPersonId = "Staff person_id of the game's head coach (NA when unmatched)",
  HCMatchMethod = "How HeadCoachName was matched to a staff person (see match_hc_person)",
  HCSource = "nflverse, verified_date_correction (HCSpellCorrections in 00-setup-functions.R) or wiki_interim_override",
  HCChangeWindow = "Game falls between the last snapshot listing the fired HC and the first revision listing the interim; kept with the fired HC (true change date unknown)",
  HCBlackHand = "Game HC hand-coded Black (NA until coded)",
  HCNonwhiteHand = "Game HC hand-coded non-white or Hispanic (NA until coded)",
  HCBlackProv = "Game HC flagged Black by black_provisional (1) or not flagged (0); lower bound",
  HCPBlackBifsg = "Game HC BIFSG P(Black)",
  SnapshotObserved = "The staff snapshot in force is observed (22 article-era team-seasons lack a staff box)",
  NOC = "Offensive coordinators listed in the snapshot in force",
  NDC = "Defensive coordinators listed in the snapshot in force"
)
TeamGameLabels <- c(
  TeamGameLabels,
  snapshot_group_labels("Coaches", "on-field coaches"),
  snapshot_group_labels("Coordinators", "OC/DC/STC"),
  snapshot_group_labels("OffenseCoaches", "offensive-unit coaches"),
  snapshot_group_labels("DefenseCoaches", "defensive-unit coaches"),
  snapshot_group_labels("FrontOffice", "front-office staff"),
  unit_race_labels("OC", "offensive coordinator"),
  unit_race_labels("DC", "defensive coordinator")
)

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

n_source <- tbl(con, "nfl_team_games") |> count() |> pull(n)
if (nrow(TeamGame) != n_source) {
  stop("team_game has ", nrow(TeamGame), " rows; nfl_team_games has ", n_source)
}

# Outcomes equal the DB source for a random sample of 500 team-games
set.seed(20260926)
SampleCheck <- TeamGame |>
  slice_sample(n = 500) |>
  select(franchise_id, game_id, points_for, margin, off_epa_per_play,
         implied_win_prob) |>
  inner_join(tbl(con, "nfl_team_games") |>
               select(franchise_id, game_id, SrcPoints = points_for,
                      SrcMargin = margin, SrcEPA = off_epa_per_play,
                      SrcProb = implied_win_prob) |>
               collect(),
             by = c("franchise_id", "game_id"))
if (!isTRUE(all.equal(SampleCheck$points_for, SampleCheck$SrcPoints)) ||
    !isTRUE(all.equal(SampleCheck$margin, SampleCheck$SrcMargin)) ||
    !isTRUE(all.equal(SampleCheck$off_epa_per_play, SampleCheck$SrcEPA)) ||
    !isTRUE(all.equal(SampleCheck$implied_win_prob, SampleCheck$SrcProb))) {
  stop("team_game outcomes differ from nfl_team_games")
}

# Market sign: favorites (positive team spread) have win probability > 0.5
message("corr(team_spread_line, implied_win_prob) = ",
        round(cor(TeamGame$team_spread_line, TeamGame$implied_win_prob,
                  use = "complete.obs"), 3))

# No impossible values: shares in [0, 1], counts >= 0
share_cols <- names(TeamGame)[str_detect(names(TeamGame),
  "^(Share|CodedShare|MeanPBlack|OCBlack|DCBlack|OCPBlack|DCPBlack|implied_win_prob$)")]
count_cols <- names(TeamGame)[str_detect(names(TeamGame), "^N[A-Z]")]
bad_share <- map_lgl(TeamGame[share_cols], \(x) any(x < 0 | x > 1, na.rm = TRUE))
bad_count <- map_lgl(TeamGame[count_cols], \(x) any(x < 0, na.rm = TRUE))
if (any(bad_share) || any(bad_count)) {
  stop("Out-of-range values in: ",
       paste(c(share_cols[bad_share], count_cols[bad_count]), collapse = ", "))
}

write_sample(TeamGame, "team_game", key = c("franchise_id", "game_id"),
             labels = TeamGameLabels)

# ---------------------------------------------------------------------------
# Coverage report
# ---------------------------------------------------------------------------

TeamGame |>
  count(StaffSnapshot, SnapshotObserved, name = "TeamGames") |>
  print()
TeamGame |>
  count(HCSource, HCChangeWindow, name = "TeamGames") |>
  print()
message("Team-games with a matched HC person: ",
        round(mean(!is.na(TeamGame$HeadCoachPersonId)), 4),
        "; with hand-coded HC race: ", round(mean(!is.na(TeamGame$HCBlackHand)), 4))

db_disconnect(con)

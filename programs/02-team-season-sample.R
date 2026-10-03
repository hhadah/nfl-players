# ============================================================================
# 02-team-season-sample.R
# Builds analysis/team_season: one row per franchise_id x season, 1999-2025
# (31 teams in 1999-2001, 32 from 2002; 861 rows). Combines
#   - REG-season outcomes (nfl_team_seasons) and their one-season lags,
#   - staff composition by group (counts; Black share under separate
#     measures: hand-coded, provisional lower bound, BIFSG mean, and the
#     predicted expected share (model-only Pred, the primary measure while
#     hand codes are absent, and the documented PredDoc variant) with the
#     members' mean prior P(Black); expected and categorical Blau indices)
#     in two timings:
#       * union of the season's snapshots (no suffix; staff_person_season,
#         01; includes in-season hires and interim promotions, which respond
#         to results; descriptive and robustness use), NA where no staff box
#         is parsed (StaffObserved FALSE: 22 article-era team-seasons with
#         infobox rows only);
#       * opening snapshot (suffix Pre; staff_person_opening_season, 01: the
#         template revision in force at 00:00 UTC on the franchise's first
#         REG game date, 2007+), predetermined with respect to the season's
#         results; NA before 2007 and where the opening snapshot is not
#         parsed (OpeningStaffObserved FALSE),
#   - head coach, coordinator and GM race (incl. predicted P(Black) and the
#     holder's prior) and experience: the season holder (most snapshots /
#     most REG games) and the opening-snapshot holder (suffix Pre),
#   - staff turnover (union and opening-snapshot Pre versions), the
#     opening-day head coach's franchise-run spell (HCIncumbentSpellId),
#     starting-QB instability and the source-backed policy indicators of
#     00-policy-functions.R (add_rooney_policies, opening-staff timing).
# Requires 01-staff-person-season.R to have run.
# Date: 2026-09-26; predicted race added 2026-10-02; opening-snapshot
# measures and policy registry 2026-10-03
# ============================================================================

con <- db_connect()

# ---------------------------------------------------------------------------
# Outcomes (REG season) and one-season lags
# ---------------------------------------------------------------------------

Outcomes <- tbl(con, "nfl_team_seasons") |>
  collect() |>
  transmute(franchise_id, season = as.integer(season), team_code,
            conf, division,
            Games = as.integer(games), wins, losses, ties,
            WinPct = win_pct,
            PointDiffPerGame = point_diff / games,
            Pythagorean = pythag_win_pct,
            OffEPAPerPlay = off_epa_per_play,
            DefEPAPerPlay = def_epa_per_play,
            OffSuccessRate = off_success_rate,
            DefSuccessRate = def_success_rate,
            ExpectedWins = expected_wins,
            WinsOverExpected = wins_minus_expected,
            Playoffs = as.integer(made_playoffs),
            PlayoffWins = as.integer(playoff_wins))

Outcomes <- Outcomes |>
  group_by(franchise_id) |>
  mutate(across(c(WinPct, PointDiffPerGame, Pythagorean, OffEPAPerPlay,
                  DefEPAPerPlay, OffSuccessRate, DefSuccessRate, ExpectedWins,
                  WinsOverExpected, Playoffs, PlayoffWins),
                \(x) lag_within(x, season), .names = "Lag{.col}")) |>
  ungroup()

# ---------------------------------------------------------------------------
# Staff composition by group
# ---------------------------------------------------------------------------

StaffPersonSeason <- read_parquet(file.path(analysis, "staff_person_season.parquet"))
StaffPersonOpeningSeason <- read_parquet(file.path(analysis, "staff_person_opening_season.parquet"))
OpeningCoverage <- read_parquet(file.path(analysis, "staff_opening_coverage.parquet")) |>
  select(franchise_id, season, OpeningStaffObserved, OpeningTargetDate,
         OpeningRevisionTimestamp, OpeningDaysStale)
bifsg_cols <- c("p_white_bifsg", "p_black_bifsg", "p_hispanic_bifsg",
                "p_api_bifsg", "p_aian_bifsg", "p_multi_bifsg")
pred_cols <- c("p_white_pred", "p_black_pred", "p_hispanic_pred", "p_api_pred",
               "p_aian_pred", "p_multi_pred")
preddoc_cols <- str_replace(pred_cols, "_pred$", "_preddoc")

# Hand-coded category for the categorical Blau: Hispanic of any race is its
# own category; otherwise the hand-coded race ('unknown' treated as uncoded)
hand_category <- function(df) {
  mutate(df, RaceCategoryHand = case_when(hispanic == "yes" ~ "hispanic",
                                          race == "unknown" ~ NA_character_,
                                          TRUE ~ race))
}
StaffPersonSeason <- hand_category(StaffPersonSeason)
StaffPersonOpeningSeason <- hand_category(StaffPersonOpeningSeason)

# Composition of one group of staff: N, the Black-share measures kept
# separate, coverage of hand codes, and the expected/categorical Blau indices.
# Every staff person has a predicted probability, so the predicted shares are
# means over all members (the expected Black share; no coverage gating).
compose_group <- function(df, suffix) {
  df |>
    group_by(franchise_id, season) |>
    summarise(N = n(),
              CodedShare = mean(!is.na(black_any)),
              ShareBlackHand = mean_or_na(black_any),
              ShareBlackProv = sum(black_provisional == 1, na.rm = TRUE) / n(),
              MeanPBlackBifsg = mean_or_na(p_black_bifsg),
              BlauBifsg = blau_expected(na.omit(pick(all_of(bifsg_cols)))),
              BlauHand = blau_categorical(RaceCategoryHand),
              ShareBlackPred = mean_or_na(p_black_any_pred),
              ShareBlackPredDoc = mean_or_na(p_black_any_preddoc),
              MeanPriorBlackPred = mean_or_na(prior_black_pred),
              BlauPred = blau_expected(na.omit(pick(all_of(pred_cols)))),
              BlauPredDoc = blau_expected(na.omit(pick(all_of(preddoc_cols)))),
              .groups = "drop") |>
    rename_with(\(x) paste0(x, suffix), -c(franchise_id, season))
}

# Group definitions shared by the union and the opening-snapshot panels. In
# the union panel a flag means "held such a role in any snapshot of the
# season"; in the opening panel it means "listed in such a role in the
# opening snapshot" (roles first held later in the season are excluded).
staff_groups <- function(df) {
  list(
    AllStaff = df,
    Coaches = filter(df, IsCoach),
    Coordinators = filter(df, IsCoordinator),
    PositionCoaches = filter(df, IsPositionCoach),
    Assistants = filter(df, IsAssistantCoach),
    OffenseCoaches = filter(df, IsOffenseCoach),
    DefenseCoaches = filter(df, IsDefenseCoach),
    FrontOffice = filter(df, IsFrontOffice),
    Personnel = filter(df, IsPersonnelScouting)
  )
}

StaffComposition <- staff_groups(StaffPersonSeason) |>
  imap(compose_group) |>
  reduce(full_join, by = c("franchise_id", "season"))

# Opening-snapshot composition: the same groups with suffix Pre (CoachesPre
# replaces the former union-flag definition, which also mixed in the
# retrospective 1999-2006 season-article boxes)
OpeningComposition <- staff_groups(StaffPersonOpeningSeason) |>
  imap(\(df, g) compose_group(df, paste0(g, "Pre"))) |>
  reduce(full_join, by = c("franchise_id", "season"))

# ---------------------------------------------------------------------------
# Head coach of the season (REG games)
# ---------------------------------------------------------------------------

# Person-level race for staff (hand-coded, provisional, BIFSG, predicted)
StaffRace <- load_person_race(con, hand_coded) |>
  filter(entity == "staff") |>
  select(person_id, black_any, nonwhite, hispanic, black_provisional, p_black_bifsg,
         p_black_any_pred, p_black_any_preddoc, prior_black_pred)

# Race variables for a role holder, prefixed (e.g. HCBlackHand) and
# optionally suffixed (HCBlackHandPre for the opening-snapshot holder); the
# provisional indicator is 1 when flagged Black and 0 otherwise (lower bound);
# the predicted measures are the holder's predicted P(Black) and the holder's
# EM prior (the person-level calibration control), NA without a holder
role_race <- function(df, prefix, suffix = "") {
  df |>
    left_join(StaffRace, by = c(PersonId = "person_id")) |>
    transmute(franchise_id, season, PersonId,
              BlackHand = black_any, NonwhiteHand = nonwhite,
              HispanicHand = case_when(hispanic == "yes" ~ 1L,
                                       hispanic == "no" ~ 0L,
                                       TRUE ~ NA_integer_),
              BlackProv = if_else(!is.na(PersonId), coalesce(black_provisional, 0L),
                                  NA_integer_),
              PBlackBifsg = p_black_bifsg,
              BlackPred = p_black_any_pred,
              BlackPredDoc = p_black_any_preddoc,
              PriorBlackPred = prior_black_pred) |>
    rename_with(\(x) paste0(prefix, x, suffix), -c(franchise_id, season))
}

GameHC <- load_game_head_coaches(con)

# Season head coach = the coach of the most REG games (ties: the later spell)
SeasonHC <- GameHC |>
  filter(game_type == "REG") |>
  group_by(franchise_id, season, HeadCoachName, HeadCoachPersonId) |>
  summarise(HCGames = n(), LastGame = max(gameday), .groups = "drop") |>
  group_by(franchise_id, season) |>
  arrange(desc(HCGames), desc(LastGame), .by_group = TRUE) |>
  summarise(NHeadCoaches = n(),
            HeadCoachName = first(HeadCoachName),
            PersonId = first(HeadCoachPersonId),
            HCGamesShare = first(HCGames) / sum(HCGames),
            .groups = "drop") |>
  mutate(TwoHCSeason = NHeadCoaches >= 2)

HCFlags <- GameHC |>
  filter(game_type == "REG") |>
  group_by(franchise_id, season) |>
  summarise(HCGamesReassigned = sum(HCSource != "nflverse"),
            HCChangeWindowGames = sum(HCChangeWindow),
            .groups = "drop")

HeadCoach <- SeasonHC |>
  select(franchise_id, season, HeadCoachName, NHeadCoaches, TwoHCSeason,
         HCGamesShare) |>
  left_join(role_race(SeasonHC, "HC"), by = c("franchise_id", "season")) |>
  left_join(HCFlags, by = c("franchise_id", "season"))

# ---------------------------------------------------------------------------
# Coordinators and general manager of the season
# ---------------------------------------------------------------------------

# Role rows (person x role) with the snapshots in which the person held that
# role; person-level snapshot flags would credit, e.g., a DC promoted to
# interim HC with DC snapshots he did not hold
RoleSnapshots <- tbl(con, "staff_team_season") |>
  filter(role_std %in% c("OC", "DC", "STC", "GM")) |>
  select(franchise_id, season, person_id, role_std, in_preseason, in_midseason,
         in_late, in_season_article) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  group_by(franchise_id, season, person_id, role_std) |>
  summarise(across(starts_with("in_"), any), .groups = "drop")

# The holder of a role for the season: listed in that role in the most
# snapshots (ties: listed in it in the preseason snapshot, then person_id);
# NRole counts holders
season_role_holder <- function(role, prefix) {
  holders <- RoleSnapshots |>
    filter(role_std == role) |>
    mutate(NSnapshots = in_preseason + in_midseason + in_late + in_season_article) |>
    arrange(franchise_id, season, desc(NSnapshots), desc(in_preseason), person_id) |>
    group_by(franchise_id, season) |>
    summarise(N = n(), PersonId = first(person_id), .groups = "drop")
  holders |>
    select(franchise_id, season, PersonId) |>
    role_race(prefix) |>
    left_join(holders |> select(franchise_id, season, N) |>
                rename_with(\(x) paste0("N", prefix), N),
              by = c("franchise_id", "season"))
}

RoleHolders <- c("OC", "DC", "STC", "GM") |>
  map(\(r) season_role_holder(r, r)) |>
  reduce(full_join, by = c("franchise_id", "season"))

# The holder of a role in the opening snapshot (template era): listed in
# that role in the opening revision's raw entries (staff_entries, snapshot
# 'preseason'; the entry's own interim tag, not the season-pooled
# interim_any); when several are listed, the non-interim holder first, then
# person_id (NO 2012 lists the suspended Sean Payton and two interim head
# coaches). N<Role>Pre counts the listed holders. Variables carry the suffix
# Pre (HCPersonIdPre, OCBlackPredPre).
OpeningRoleEntries <- tbl(con, "staff_entries") |>
  filter(snapshot == "preseason", source == "staff_template", !is.na(person_id), !vacant,
         role_std %in% c("HC", "OC", "DC", "STC", "GM")) |>
  select(franchise_id, season, person_id, role_std, interim) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  group_by(franchise_id, season, person_id, role_std) |>
  summarise(Interim = any(interim), .groups = "drop")
opening_role_holder <- function(role, prefix) {
  holders <- OpeningRoleEntries |>
    filter(role_std == role) |>
    arrange(franchise_id, season, Interim, person_id) |>
    group_by(franchise_id, season) |>
    summarise(N = n(), PersonId = first(person_id), .groups = "drop")
  holders |>
    select(franchise_id, season, PersonId) |>
    role_race(prefix, "Pre") |>
    left_join(holders |> select(franchise_id, season, N) |>
                rename_with(\(x) paste0("N", prefix, "Pre"), N),
              by = c("franchise_id", "season"))
}

OpeningRoleHolders <- c("HC", "OC", "DC", "STC", "GM") |>
  map(\(r) opening_role_holder(r, r)) |>
  reduce(full_join, by = c("franchise_id", "season"))

# ---------------------------------------------------------------------------
# Staff turnover and head-coach tenure
# ---------------------------------------------------------------------------

# HC identity key: staff person_id, else the nflverse name
GameHC <- GameHC |> mutate(HCKey = coalesce(HeadCoachPersonId, HeadCoachName))

HCTurnover <- GameHC |>
  filter(game_type == "REG") |>
  arrange(franchise_id, season, gameday) |>
  group_by(franchise_id, season) |>
  summarise(FirstHCKey = first(HCKey), LastHCKey = last(HCKey),
            InSeasonHCChange = as.integer(n_distinct(HCKey) > 1),
            .groups = "drop") |>
  group_by(franchise_id) |>
  mutate(HCChange = as.integer(FirstHCKey != lag_within(LastHCKey, season))) |>
  ungroup()

# Franchise runs of a head coach: consecutive seasons with an observation
# (franchise_id, HCKey, season) for the same coach and franchise. Spell is
# the run number, HCSpellKey the run id (franchise / coach key / run number);
# left-censored in 1999.
franchise_runs <- function(obs) {
  obs |>
    distinct(franchise_id, HCKey, season) |>
    arrange(franchise_id, HCKey, season) |>
    group_by(franchise_id, HCKey) |>
    mutate(Spell = cumsum(season - lag(season, default = first(season) - 2L) != 1L)) |>
    group_by(franchise_id, HCKey, Spell) |>
    mutate(HCTenure = row_number(), HCTenureLeftCensored = first(season) == 1999L) |>
    ungroup() |>
    mutate(HCSpellKey = paste(franchise_id, HCKey, Spell, sep = "/")) |>
    select(franchise_id, HCKey, season, HCTenure, HCTenureLeftCensored, HCSpellKey)
}

# Tenure of the season head coach: consecutive seasons in which he coached at
# least one REG game for this franchise, including the current one
# (left-censored in 1999). Counting any REG game keeps a coach's tenure intact
# across a season in which a temporary acting HC coached most games (IND 2012).
GameHCSeasons <- GameHC |>
  filter(game_type == "REG") |>
  distinct(franchise_id, HCKey, season)
HCFranchiseRuns <- franchise_runs(GameHCSeasons)
HCTenure <- SeasonHC |>
  transmute(franchise_id, season, HCKey = coalesce(PersonId, HeadCoachName)) |>
  left_join(HCFranchiseRuns, by = c("franchise_id", "HCKey", "season")) |>
  select(-HCSpellKey)

# Incumbent head coach of the season: the non-interim head coach listed in
# the opening snapshot (2007+, parsed opening), else the head coach of the
# franchise's first REG game (before 2007, or when the opening lists only
# interim head coaches). The head coach of the first game is kept as a
# separate field (HCFirstGameKey): a suspended incumbent (NO 2012: Payton
# listed, Kromer coached the opener) is the incumbent, not the acting coach.
# HCIncumbentSpellId is the incumbent's franchise run from the any-REG-game
# observations augmented with the opening-incumbent observations, so a
# season he opened as incumbent but coached no game (suspension) and a
# season a temporary acting HC coached most games (IND 2012) both continue
# his run, while a different coach always starts a new run. Full 1999-2025
# panel.
# The opening listing is used when it names the first-game coach, or when
# it disagrees but itself lists an interim head coach acting for the
# non-interim one (a suspension). A disagreement without a listed interim
# is a stale box (a fired coach not yet removed), so the first-game coach is
# the incumbent (HCIncumbentSource 'first_reg_game_opening_stale').
OpeningIncumbentHC <- OpeningRoleEntries |>
  filter(role_std == "HC") |>
  arrange(franchise_id, season, Interim, person_id) |>
  group_by(franchise_id, season) |>
  summarise(OpeningHCKey = if (any(!Interim)) first(person_id) else NA_character_,
            OpeningInterimListed = any(Interim), .groups = "drop")
HCIncumbentKeys <- HCTurnover |>
  select(franchise_id, season, HCFirstGameKey = FirstHCKey) |>
  left_join(OpeningIncumbentHC, by = c("franchise_id", "season")) |>
  mutate(UseOpening = !is.na(OpeningHCKey) &
           (OpeningHCKey == HCFirstGameKey | coalesce(OpeningInterimListed, FALSE)),
         HCIncumbentKey = if_else(UseOpening, OpeningHCKey, HCFirstGameKey),
         HCIncumbentSource = case_when(
           UseOpening ~ "opening_snapshot_non_interim",
           !is.na(OpeningHCKey) ~ "first_reg_game_opening_stale",
           TRUE ~ "first_reg_game")) |>
  select(-UseOpening, -OpeningInterimListed)
HCIncumbentRuns <- franchise_runs(bind_rows(
  GameHCSeasons,
  HCIncumbentKeys |> transmute(franchise_id, HCKey = HCIncumbentKey, season)))
HCIncumbent <- HCIncumbentKeys |>
  left_join(HCIncumbentRuns, by = c("franchise_id", HCIncumbentKey = "HCKey", "season")) |>
  transmute(franchise_id, season, HCFirstGameKey, HCIncumbentKey, HCIncumbentSource,
            HCIncumbentSpellId = HCSpellKey, HCIncumbentTenure = HCTenure,
            HCIncumbentTenureLeftCensored = HCTenureLeftCensored)
if (anyNA(HCIncumbent$HCIncumbentSpellId) || anyNA(HCIncumbent$HCIncumbentKey)) {
  stop("HCIncumbentSpellId is missing for ", sum(is.na(HCIncumbent$HCIncumbentSpellId)),
       " franchise-seasons")
}
message("Incumbent HC differs from the first-game HC in ",
        sum(HCIncumbent$HCIncumbentKey != HCIncumbent$HCFirstGameKey), " franchise-seasons; ",
        "incumbent from the opening snapshot in ",
        sum(HCIncumbent$HCIncumbentSource == "opening_snapshot_non_interim"))

# Change of OC / DC / GM holder relative to the previous season (NA when
# either season has no holder observed); the same for the opening holders
RoleChanges <- RoleHolders |>
  select(franchise_id, season, OCPersonId, DCPersonId, GMPersonId) |>
  group_by(franchise_id) |>
  mutate(OCChange = as.integer(OCPersonId != lag_within(OCPersonId, season)),
         DCChange = as.integer(DCPersonId != lag_within(DCPersonId, season)),
         GMChange = as.integer(GMPersonId != lag_within(GMPersonId, season))) |>
  ungroup() |>
  select(franchise_id, season, OCChange, DCChange, GMChange)
OpeningRoleChanges <- OpeningRoleHolders |>
  select(franchise_id, season, HCPersonIdPre, OCPersonIdPre, DCPersonIdPre, GMPersonIdPre) |>
  group_by(franchise_id) |>
  mutate(HCChangePre = as.integer(HCPersonIdPre != lag_within(HCPersonIdPre, season)),
         OCChangePre = as.integer(OCPersonIdPre != lag_within(OCPersonIdPre, season)),
         DCChangePre = as.integer(DCPersonIdPre != lag_within(DCPersonIdPre, season)),
         GMChangePre = as.integer(GMPersonIdPre != lag_within(GMPersonIdPre, season))) |>
  ungroup() |>
  select(franchise_id, season, HCChangePre, OCChangePre, DCChangePre, GMChangePre)

# Coach turnover: union panel (new to the franchise relative to any snapshot
# of season - 1) and opening panel (Pre: opening snapshot vs the previous
# opening snapshot; NA in 2007, whose 2006 opening staff is unobserved, and
# wherever the previous opening snapshot is unparsed, never 0)
coach_inflow <- function(df, suffix = "") {
  df |>
    filter(IsCoach) |>
    group_by(franchise_id, season) |>
    summarise(ShareCoachesNewToFranchise = mean_or_na(as.numeric(NewToFranchise)),
              ShareCoachesPromoted = mean_or_na(as.numeric(PromotedWithinFranchise)),
              .groups = "drop") |>
    rename_with(\(x) paste0(x, suffix), -c(franchise_id, season))
}
CoachInflow <- coach_inflow(StaffPersonSeason)
OpeningCoachInflow <- coach_inflow(StaffPersonOpeningSeason, "Pre")
if (any(!is.na(OpeningCoachInflow$ShareCoachesNewToFranchisePre[OpeningCoachInflow$season ==
                                                                 min(OpeningCoachInflow$season)]))) {
  stop("Opening turnover is defined in the first template season, whose prior opening is unobserved")
}

# ---------------------------------------------------------------------------
# Head-coach experience
# ---------------------------------------------------------------------------

# Prior NFL HC seasons (any franchise, at least one REG game as HC) since 1999
HCSeasonsByKey <- GameHC |>
  filter(game_type == "REG") |>
  distinct(HCKey, season)

PriorNFLHC <- HCTenure |>
  select(franchise_id, season, HCKey) |>
  left_join(HCSeasonsByKey |> rename(HCSeason = season), by = "HCKey",
            relationship = "many-to-many") |>
  group_by(franchise_id, season) |>
  summarise(PriorNFLHCSeasons = sum(HCSeason < season, na.rm = TRUE),
            PriorNFLHCSeasonsLeftCensored = any(HCSeason == 1999L, na.rm = TRUE),
            .groups = "drop")

# Prior college (FBS) HC seasons from college_coaches (2004+), matched on the
# normalized full name. A name held by more than one CFBD coach_id is
# ambiguous (count NA); a match is low-confidence when a matched college
# season coincides with a season the person is on an NFL staff.
normalize_name <- function(x) {
  x |> str_to_lower() |> str_remove_all("[.']") |>
    str_remove(",? (jr|sr|ii|iii)$") |> str_squish()
}
CollegeHC <- tbl(con, "college_coaches") |>
  select(coach_id, first_name, last_name, season) |>
  collect() |>
  mutate(NameKey = normalize_name(paste(first_name, last_name)),
         season = as.integer(season))
CollegeNames <- CollegeHC |>
  group_by(NameKey) |>
  summarise(NCoachIds = n_distinct(coach_id), .groups = "drop")
NFLStaffSeasons <- StaffPersonSeason |> distinct(person_id, season)

HCCollege <- SeasonHC |>
  transmute(franchise_id, season, PersonId,
            NameKey = normalize_name(HeadCoachName)) |>
  left_join(CollegeNames, by = "NameKey") |>
  left_join(CollegeHC |> select(NameKey, CollegeSeason = season), by = "NameKey",
            relationship = "many-to-many") |>
  left_join(NFLStaffSeasons |> mutate(OnNFLStaff = TRUE),
            by = c(PersonId = "person_id", CollegeSeason = "season")) |>
  group_by(franchise_id, season) |>
  summarise(NCoachIds = first(NCoachIds),
            PriorCollegeHC = sum(CollegeSeason < season, na.rm = TRUE),
            Overlap = any(OnNFLStaff, na.rm = TRUE),
            CollegeLeftCensored = any(CollegeSeason == min(CollegeHC$season) &
                                        CollegeSeason < season, na.rm = TRUE),
            .groups = "drop") |>
  mutate(CollegeHCMatch = case_when(is.na(NCoachIds) ~ "no_match",
                                    NCoachIds > 1 ~ "ambiguous",
                                    Overlap ~ "low_confidence_overlap",
                                    TRUE ~ "unique_name"),
         PriorCollegeHCSeasons = if_else(CollegeHCMatch == "ambiguous",
                                         NA_integer_, PriorCollegeHC)) |>
  select(franchise_id, season, PriorCollegeHCSeasons,
         PriorCollegeHCLeftCensored = CollegeLeftCensored, CollegeHCMatch)

# ---------------------------------------------------------------------------
# Starting-QB instability (REG games, nfl_team_games)
# ---------------------------------------------------------------------------

QBInstability <- tbl(con, "nfl_team_games") |>
  filter(game_type == "REG") |>
  select(franchise_id, season, starting_qb_id) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  count(franchise_id, season, starting_qb_id, name = "Starts") |>
  group_by(franchise_id, season) |>
  summarise(NStartingQBs = sum(!is.na(starting_qb_id)),
            TopQBStartShare = max(Starts) / sum(Starts),
            .groups = "drop")

# ---------------------------------------------------------------------------
# Unit outcomes (play-weighted pass/rush EPA, sacks) and the market's
# season-opening expectation (implied win probability of the first REG game)
# ---------------------------------------------------------------------------

sum_if_any <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

UnitOutcomes <- tbl(con, "nfl_team_games") |>
  filter(game_type == "REG") |>
  select(franchise_id, season, week, implied_win_prob,
         off_pass_plays, off_rush_plays, def_pass_plays, def_rush_plays,
         off_pass_epa_per_play, off_rush_epa_per_play,
         def_pass_epa_per_play, def_rush_epa_per_play, off_sacks, def_sacks) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  group_by(franchise_id, season) |>
  summarise(OffPassEPAPerPlay = weighted.mean(off_pass_epa_per_play, off_pass_plays, na.rm = TRUE),
            OffRushEPAPerPlay = weighted.mean(off_rush_epa_per_play, off_rush_plays, na.rm = TRUE),
            DefPassEPAPerPlay = weighted.mean(def_pass_epa_per_play, def_pass_plays, na.rm = TRUE),
            DefRushEPAPerPlay = weighted.mean(def_rush_epa_per_play, def_rush_plays, na.rm = TRUE),
            SacksAllowed = sum_if_any(off_sacks),
            SacksMade = sum_if_any(def_sacks),
            SeasonOpenerImpliedWinProb = implied_win_prob[which.min(week)],
            .groups = "drop") |>
  mutate(across(where(is.double), \(x) if_else(is.nan(x), NA_real_, x)))

# ---------------------------------------------------------------------------
# GM tenure: consecutive seasons the same person is the season GM
# ---------------------------------------------------------------------------

GMTenure <- RoleHolders |>
  select(franchise_id, season, GMPersonId) |>
  arrange(franchise_id, season) |>
  group_by(franchise_id) |>
  mutate(PrevGM = lag(GMPersonId),
         NewSpell = is.na(GMPersonId) | is.na(PrevGM) | GMPersonId != PrevGM |
           season != lag(season) + 1L,
         Spell = cumsum(NewSpell)) |>
  group_by(franchise_id, Spell) |>
  mutate(GMTenure = if_else(is.na(GMPersonId), NA_integer_, row_number()),
         GMTenureLeftCensored = !is.na(GMPersonId) & min(season) == 1999L) |>
  ungroup() |>
  select(franchise_id, season, GMTenure, GMTenureLeftCensored)

# ---------------------------------------------------------------------------
# Staff-source flags and Rooney Rule eras
# ---------------------------------------------------------------------------

# Team-seasons with a parsed staff box: someone listed in the season article
# or a template snapshot. In 22 article-era team-seasons only the infobox (HC,
# GM, owner) is observed; their group compositions would describe the head
# coach alone, so they are set to NA below (as in team_game, 03).
ParsedStaffBox <- tbl(con, "staff_team_season") |>
  group_by(franchise_id, season) |>
  summarise(StaffObserved = any(in_season_article | in_preseason | in_midseason | in_late,
                                na.rm = TRUE),
            .groups = "drop") |>
  collect() |>
  mutate(season = as.integer(season))

StaffSnapshotFlags <- tbl(con, "staff_snapshots") |>
  select(franchise_id, season, source, parse_ok) |>
  collect() |>
  group_by(franchise_id, season) |>
  summarise(StaffSource = min(source),
            AllSnapshotsParsed = all(parse_ok),
            .groups = "drop") |>
  mutate(season = as.integer(season))

# Policy indicators come from add_rooney_policies() (00-policy-functions.R,
# registry data/reference/nfl_staff_policies.csv) under the opening-staff
# timing convention: a provision counts in the first season whose opening
# staff it was in force for. They describe the policy environment of the
# season, not an assigned treatment.
if (!exists("add_rooney_policies") || !exists("rooney_era")) {
  stop("00-policy-functions.R must be sourced before 02-team-season-sample.R")
}

# ---------------------------------------------------------------------------
# Assemble the team-season sample
# ---------------------------------------------------------------------------

OpeningVars <- c(setdiff(names(OpeningComposition), c("franchise_id", "season")),
                 "ShareCoachesNewToFranchisePre", "ShareCoachesPromotedPre")
OpeningCounts <- grep("^N[A-Z].*Pre$", OpeningVars, value = TRUE)

TeamSeason <- tbl(con, "franchise_seasons") |>
  select(franchise_id, season) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  filter(season >= 1999, season <= 2025) |>
  left_join(Outcomes, by = c("franchise_id", "season")) |>
  left_join(QBInstability, by = c("franchise_id", "season")) |>
  left_join(UnitOutcomes, by = c("franchise_id", "season")) |>
  left_join(StaffSnapshotFlags, by = c("franchise_id", "season")) |>
  left_join(ParsedStaffBox, by = c("franchise_id", "season")) |>
  left_join(OpeningCoverage, by = c("franchise_id", "season")) |>
  left_join(StaffComposition, by = c("franchise_id", "season")) |>
  left_join(OpeningComposition, by = c("franchise_id", "season")) |>
  left_join(HeadCoach, by = c("franchise_id", "season")) |>
  left_join(HCTurnover |> select(franchise_id, season, HCChange, InSeasonHCChange),
            by = c("franchise_id", "season")) |>
  left_join(HCTenure |> select(-HCKey), by = c("franchise_id", "season")) |>
  left_join(HCIncumbent, by = c("franchise_id", "season")) |>
  left_join(PriorNFLHC, by = c("franchise_id", "season")) |>
  left_join(HCCollege, by = c("franchise_id", "season")) |>
  left_join(RoleHolders, by = c("franchise_id", "season")) |>
  left_join(RoleChanges, by = c("franchise_id", "season")) |>
  left_join(OpeningRoleHolders, by = c("franchise_id", "season")) |>
  left_join(OpeningRoleChanges, by = c("franchise_id", "season")) |>
  left_join(GMTenure, by = c("franchise_id", "season")) |>
  left_join(CoachInflow, by = c("franchise_id", "season")) |>
  left_join(OpeningCoachInflow, by = c("franchise_id", "season")) |>
  # Staff observation flags; group counts are 0 (not NA) when the staff box
  # is observed but lists nobody in the group. Group compositions and coach
  # inflow are NA when no staff box is parsed (infobox-only team-seasons);
  # role holders (HC, coordinators, GM) keep the infobox information. The
  # opening-snapshot (Pre) measures are gated by OpeningStaffObserved instead
  # (NA before 2007; counts 0 when the opening snapshot is parsed and lists
  # nobody in the group).
  mutate(StaffObserved = coalesce(StaffObserved, FALSE),
         FullStaffObserved = StaffSource == "staff_template" & AllSnapshotsParsed,
         OpeningStaffObserved = coalesce(OpeningStaffObserved, FALSE),
         across(all_of(c(setdiff(names(StaffComposition), c("franchise_id", "season")),
                         "ShareCoachesNewToFranchise", "ShareCoachesPromoted")),
                \(x) if_else(StaffObserved, x, NA)),
         across(c(starts_with("N") & where(is.integer) & !ends_with("Pre") &
                    !c(NHeadCoaches, NStartingQBs)),
                \(x) if_else(StaffObserved, coalesce(x, 0L), x)),
         across(all_of(OpeningVars), \(x) if_else(OpeningStaffObserved, x, NA)),
         across(all_of(c(OpeningCounts, "NHCPre", "NOCPre", "NDCPre", "NSTCPre", "NGMPre")),
                \(x) if_else(OpeningStaffObserved, coalesce(x, 0L), x))) |>
  select(-AllSnapshotsParsed) |>
  add_rooney_policies(season_col = "season", timing = "opening_staff") |>
  arrange(franchise_id, season)

# Opening-snapshot measures exist exactly where the opening snapshot is
# observed, and the opening head coach is listed wherever it is parsed
if (any(!is.na(TeamSeason$ShareBlackPredCoachesPre) & !TeamSeason$OpeningStaffObserved) ||
    any(is.na(TeamSeason$NCoachesPre) & TeamSeason$OpeningStaffObserved) ||
    any(TeamSeason$OpeningStaffObserved & TeamSeason$season < 2007) ||
    any(is.na(TeamSeason$HCPersonIdPre) & TeamSeason$OpeningStaffObserved)) {
  stop("Opening-snapshot (Pre) measures are inconsistent with OpeningStaffObserved")
}

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

GroupDescriptions <- c(
  AllStaff = "all listed staff (coaches, S&C, support and front office)",
  Coaches = "on-field coaches (HC, coordinators, position coaches, assistants/QC)",
  Coordinators = "OC, DC and STC",
  PositionCoaches = "position coaches",
  Assistants = "assistant and quality-control coaches",
  OffenseCoaches = "on-field coaches of the offensive unit",
  DefenseCoaches = "on-field coaches of the defensive unit",
  FrontOffice = "front office (owner/executives, GM, personnel/scouting, other)",
  Personnel = "player-personnel and scouting staff"
)
opening_desc <- "listed in the opening snapshot (template revision in force at 00:00 UTC on the franchise's first REG game date; 2007+)"
OpeningGroupDescriptions <- set_names(paste(GroupDescriptions, opening_desc),
                                      paste0(names(GroupDescriptions), "Pre"))
group_labels <- function(group, desc, opening = FALSE) {
  listed <- if (opening) "(0 when the opening snapshot is parsed and lists none; NA when it is unobserved or before 2007)"
            else "listed in any snapshot of the season (0 when the staff box is observed and lists none; NA when unobserved)"
  c(N = glue("Number of {desc} {listed}"),
    CodedShare = glue("Share of {desc} with a hand-coded black_any"),
    ShareBlackHand = glue("Share Black among hand-coded {desc} (NA until coded; use with CodedShare)"),
    ShareBlackProv = glue("Share of {desc} flagged Black by black_provisional (hand code, else Wikipedia category); positive-only lower bound"),
    MeanPBlackBifsg = glue("Mean BIFSG P(Black) of {desc} (secondary; understates Black share)"),
    BlauBifsg = glue("Expected Blau index of {desc} from BIFSG probability vectors: P(two distinct randomly drawn members differ in race) = 1 - sum_k[(sum_i p_ik)^2 - sum_i p_ik^2]/(n(n-1)); NA if n < 2"),
    BlauHand = glue("Categorical Blau index of hand-coded {desc} (Hispanic of any race its own category), same without-replacement formula over coded members; NA if fewer than 2 coded"),
    ShareBlackPred = glue("Expected Black share of {desc}: mean model-only predicted P(non-Hispanic Black alone) (p_black_any_pred; primary measure while hand codes are absent). Equals the expected true share only if the probabilities are calibrated at the team level given the regression's controls; the sign of any bias is not known"),
    ShareBlackPredDoc = glue("Expected Black share of {desc} under the documented variant: mean p_black_any_preddoc (documented Black alone or in combination = 1, else model P(non-Hispanic Black alone); fame-dependent; sensitivity only)"),
    MeanPriorBlackPred = glue("Mean EM prior P(Black) of {desc} (team-level summary of the prior's predetermined covariates; regression-calibration control for ShareBlackPred)"),
    BlauPred = glue("Expected Blau index of {desc} from model-only predicted probability vectors (p_*_pred), same formula as BlauBifsg; NA if n < 2"),
    BlauPredDoc = glue("Expected Blau index of {desc} from documented-variant probability vectors (p_*_preddoc), same formula; NA if n < 2; sensitivity only")) |>
    set_names(\(x) paste0(x, group))
}
role_labels <- function(prefix, role, suffix = "") {
  c(PersonId = glue("Staff person_id of the {role}"),
    BlackHand = glue("{role}: hand-coded Black (NA until coded)"),
    NonwhiteHand = glue("{role}: hand-coded non-white or Hispanic (NA until coded)"),
    HispanicHand = glue("{role}: hand-coded Hispanic (NA until coded)"),
    BlackProv = glue("{role}: flagged Black by black_provisional (1) or not flagged (0); lower bound"),
    PBlackBifsg = glue("{role}: BIFSG P(Black)"),
    BlackPred = glue("{role}: model-only predicted P(non-Hispanic Black alone) (primary measure while hand codes are absent); calibrated to the staff population at first appearance, not to the selected population of role holders, so miscalibrated for promoted holders (Black head coaches are under-predicted); NA when no holder"),
    BlackPredDoc = glue("{role}: P(Black) under the documented variant (sensitivity only); NA when no holder"),
    PriorBlackPred = glue("{role}: EM prior P(Black) (prior_black_pred: first role group, unit and era; person-level calibration control for {prefix}BlackPred{suffix}); NA when no holder")) |>
    set_names(\(x) paste0(prefix, x, suffix))
}
opening_holder_rule <- "listed in that role in the opening snapshot (raw opening-revision entry; non-interim holder first when several are listed); NA before 2007 or when none is listed"

# Policy indicator labels from the registry (00-policy-functions.R)
PolicyLabels <- with(StaffPolicyRegistry, set_names(
  paste0("Policy environment indicator (1/0), opening-staff timing: ", requirement,
         " (scope ", scope, "; in force for opening staffs from ", first_season,
         if_else(is.na(last_season), "", paste0(" through ", last_season)),
         "; source ", source_url, "). Describes the season's policy environment, not an assigned treatment"),
  indicator))

role_holder_rule <- "holder listed in the most snapshots, ties to the preseason holder"
TeamSeasonLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  season = "NFL season",
  team_code = "nflverse team code in that season",
  conf = "Conference (standings; 2002+)",
  division = "Division (standings; 2002+)",
  Games = "REG games played",
  wins = "REG wins", losses = "REG losses", ties = "REG ties",
  WinPct = "REG win percentage (ties count one half)",
  PointDiffPerGame = "REG point differential per game",
  Pythagorean = "Pythagorean win percentage (exponent 2.37)",
  OffEPAPerPlay = "Offensive EPA per play (nflverse pbp, non-penalty scrimmage plays)",
  DefEPAPerPlay = "Defensive EPA per play allowed (lower is better)",
  OffSuccessRate = "Offensive success rate",
  DefSuccessRate = "Defensive success rate allowed",
  ExpectedWins = "Market expected wins: sum of pre-game implied win probabilities (moneyline, else spread mapping)",
  WinsOverExpected = "REG wins minus ExpectedWins",
  Playoffs = "Made the playoffs (1/0)",
  PlayoffWins = "Playoff wins",
  NStartingQBs = "Number of distinct starting QBs in REG games (nfl_team_games)",
  TopQBStartShare = "Share of REG games started by the most frequent starting QB",
  OffPassEPAPerPlay = "Offensive EPA per pass play (REG games, weighted by pass plays)",
  OffRushEPAPerPlay = "Offensive EPA per rush play (REG games, weighted by rush plays)",
  DefPassEPAPerPlay = "Defensive EPA per pass play allowed (lower is better)",
  DefRushEPAPerPlay = "Defensive EPA per rush play allowed (lower is better)",
  SacksAllowed = "Sacks taken by the offense (REG, pbp)",
  SacksMade = "Sacks made by the defense (REG, pbp)",
  SeasonOpenerImpliedWinProb = "Market implied win probability of the first REG game (lines set after the offseason hiring cycle; a pre-season expectation benchmark, not pre-hire)",
  GMTenure = "Consecutive seasons the season GM has been this franchise's season GM, including the current one (a gap in GM observation starts a new spell)",
  GMTenureLeftCensored = "GM spell starts in 1999 (first observed season)",
  StaffSource = "Staff source: staff_template (2007-2025, three snapshots) or season_article (1999-2006, one partial box)",
  HeadCoachName = "Season head coach: coached the most REG games (ties: the later spell); nflverse schedule coach corrected for in-season changes (load_game_head_coaches)",
  NHeadCoaches = "Distinct head coaches in REG games",
  TwoHCSeason = "More than one head coach during the REG season",
  HCGamesShare = "Share of REG games coached by the season head coach",
  HCGamesReassigned = "REG games whose head coach differs from the nflverse schedule coach (verified in-season change dates or Wikipedia interim overrides; see load_game_head_coaches)",
  HCChangeWindowGames = "REG games inside an unknown firing-date window (kept with the fired HC)",
  HCChange = "First-game HC differs from the previous season's last-game HC (between-season change; NA in a franchise's first season)",
  InSeasonHCChange = "More than one head coach during the REG season (1/0)",
  HCTenure = "Consecutive seasons in which the season HC coached at least one REG game for this franchise, including the current one",
  HCTenureLeftCensored = "HCTenure run starts in 1999 (first observed season)",
  PriorNFLHCSeasons = "Seasons before this one in which the season HC was an NFL head coach (any franchise, >= 1 REG game), counted from 1999",
  PriorNFLHCSeasonsLeftCensored = "Season HC was an NFL HC in 1999 (or the season is 1999), so earlier HC seasons are unobserved; pre-1999 careers of other coaches are also unobserved",
  PriorCollegeHCSeasons = "FBS head-coach seasons before this season in college_coaches (2004+) under the same normalized full name; NA when the name is ambiguous",
  PriorCollegeHCLeftCensored = "The matched college HC career includes the first season in college_coaches (2004), so earlier college HC seasons are unobserved (e.g. Pete Carroll at USC 2001-2003); FALSE also when no college record matches",
  CollegeHCMatch = "College match: no_match, unique_name, ambiguous (several CFBD coach_ids), low_confidence_overlap (a matched college season overlaps an NFL staff season of the person)",
  NOC = "Number of offensive coordinators listed", NDC = "Number of defensive coordinators listed",
  NSTC = "Number of special-teams coordinators listed", NGM = "Number of general managers listed (teams without a GM title have 0)",
  NHCPre = "Number of head coaches listed in the opening snapshot (2+ when an interim and a suspended head coach are both listed; NA before 2007)",
  NOCPre = "Number of offensive coordinators listed in the opening snapshot (NA before 2007)",
  NDCPre = "Number of defensive coordinators listed in the opening snapshot (NA before 2007)",
  NSTCPre = "Number of special-teams coordinators listed in the opening snapshot (NA before 2007)",
  NGMPre = "Number of general managers listed in the opening snapshot (NA before 2007)",
  OCChange = "Season OC differs from the previous season's (NA if either unobserved)",
  DCChange = "Season DC differs from the previous season's (NA if either unobserved)",
  GMChange = "Season GM differs from the previous season's (NA if either unobserved)",
  HCChangePre = "Opening-snapshot head coach differs from the previous season's opening-snapshot head coach (NA if either is unobserved; all of 2007)",
  OCChangePre = "Opening-snapshot OC differs from the previous season's (NA if either unobserved; all of 2007)",
  DCChangePre = "Opening-snapshot DC differs from the previous season's (NA if either unobserved; all of 2007)",
  GMChangePre = "Opening-snapshot GM differs from the previous season's (NA if either unobserved; all of 2007)",
  ShareCoachesNewToFranchise = "Share of on-field coaches (any snapshot) not on the franchise's staff in season - 1 (NA when season - 1 or season is unobserved); union measure, includes in-season hires",
  ShareCoachesPromoted = "Share of on-field coaches (any snapshot) promoted within the franchise from season - 1; union measure",
  ShareCoachesNewToFranchisePre = "Share of the opening snapshot's on-field coaches not on the franchise's opening-snapshot staff in season - 1 (adjacent opening snapshots only; NA in 2007, whose 2006 opening staff is unobserved, and before 2007)",
  ShareCoachesPromotedPre = "Share of the opening snapshot's on-field coaches promoted within the franchise from the previous opening snapshot (same domain, lower tier in season - 1; NA in 2007 and before)",
  StaffObserved = "Staff box observed: someone listed in a parsed season-article box or template snapshot (FALSE for the 22 article-era team-seasons with only infobox rows; group compositions are NA there, role holders are kept)",
  FullStaffObserved = "Full staff observed: template era (2007+) with all three snapshots parsed; article-era boxes are partial",
  OpeningStaffObserved = "Opening snapshot observed: the staff-template revision in force at 00:00 UTC on the franchise's first REG game date parsed (>= 15 persons and a head coach); FALSE before 2007 (season-article boxes are retrospective, not an opening measure). All *Pre measures are NA when FALSE",
  OpeningTargetDate = "Opening snapshot target: the franchise's first REG game date (revision in force at 00:00 UTC that day; revision_timestamp <= target by construction)",
  OpeningRevisionTimestamp = "UTC timestamp of the template revision used for the opening snapshot",
  OpeningDaysStale = "Days between the opening revision and its target (how long before the opener the box was last edited)",
  HCFirstGameKey = "Identity key of the head coach of the franchise's first REG game (staff person_id, else the nflverse name); the acting coach of the opener, which differs from the incumbent under a suspension (NO 2012)",
  HCIncumbentKey = "Identity key of the season's incumbent head coach: the non-interim head coach listed in the opening snapshot (2007+; used when he is the first-game coach, or when the opening listing also names an interim acting for him), else the first-game head coach (before 2007, no non-interim opening listing, or a stale opening box); see HCIncumbentSource",
  HCIncumbentSource = "Source of HCIncumbentKey: opening_snapshot_non_interim, first_reg_game (no usable opening listing: pre-2007 or interim-only), first_reg_game_opening_stale (opening box names a different non-interim coach and no interim: a box not yet updated)",
  HCIncumbentSpellId = "Franchise-run id of the incumbent head coach: franchise / coach key / run number. A run is the consecutive seasons in which the coach either coached at least one REG game for the franchise or opened the season as its incumbent, so a suspension season (NO 2012: Payton) and a season in which a temporary acting HC coached most games (IND 2012: Pagano) continue the run; a different coach always starts a new run. Full 1999-2025 panel; left-censored in 1999",
  HCIncumbentTenure = "Consecutive seasons (incl. this one) of the incumbent head coach's run with this franchise (any REG game coached or opened as incumbent)",
  HCIncumbentTenureLeftCensored = "HCIncumbentTenure run starts in 1999 (first observed season)",
  RooneyEra = "Calendar policy cohort (rooney_era, 00-policy-functions.R): pre_rule (<2003), rule_2003 (2003-2020), amend_2020 (2021), amend_2022 (2022-2024), post_mandate_2025 (2025+). Bundles simultaneous provisions; descriptive, not a date-specific treatment",
  PolicyTimingConvention = "Timing convention of the policy indicators: opening_staff (a provision counts from the first season whose opening staff it was in force for)"
)
lagged <- c("WinPct", "PointDiffPerGame", "Pythagorean", "OffEPAPerPlay", "DefEPAPerPlay",
            "OffSuccessRate", "DefSuccessRate", "ExpectedWins", "WinsOverExpected",
            "Playoffs", "PlayoffWins")
TeamSeasonLabels <- c(
  TeamSeasonLabels,
  PolicyLabels,
  set_names(paste0(TeamSeasonLabels[lagged], ", previous season (NA if not consecutive)"),
            paste0("Lag", lagged)),
  unlist(unname(imap(GroupDescriptions, \(desc, g) group_labels(g, desc)))),
  unlist(unname(imap(OpeningGroupDescriptions, \(desc, g) group_labels(g, desc, opening = TRUE)))),
  role_labels("HC", "season head coach (coached the most REG games)"),
  role_labels("OC", paste("season offensive coordinator,", role_holder_rule)),
  role_labels("DC", paste("season defensive coordinator,", role_holder_rule)),
  role_labels("STC", paste("season special-teams coordinator,", role_holder_rule)),
  role_labels("GM", paste("season general manager,", role_holder_rule)),
  role_labels("HC", paste("opening-snapshot head coach,", opening_holder_rule), "Pre"),
  role_labels("OC", paste("opening-snapshot offensive coordinator,", opening_holder_rule), "Pre"),
  role_labels("DC", paste("opening-snapshot defensive coordinator,", opening_holder_rule), "Pre"),
  role_labels("STC", paste("opening-snapshot special-teams coordinator,", opening_holder_rule), "Pre"),
  role_labels("GM", paste("opening-snapshot general manager,", opening_holder_rule), "Pre")
)
if (!all(StaffPolicyRegistry$indicator %in% names(TeamSeason))) {
  stop("add_rooney_policies() did not add every registry indicator")
}

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

if (nrow(TeamSeason) != 861) stop("team_season has ", nrow(TeamSeason), " rows, expected 861")

# Outcomes against an independent recomputation from nfl_team_games (REG)
CheckGames <- tbl(con, "nfl_team_games") |>
  filter(game_type == "REG") |>
  group_by(franchise_id, season) |>
  summarise(CheckWinPct = mean(win + 0.5 * tie, na.rm = TRUE),
            CheckPD = mean(margin, na.rm = TRUE),
            .groups = "drop") |>
  collect() |>
  mutate(season = as.integer(season)) |>
  inner_join(TeamSeason, by = c("franchise_id", "season"))
if (max(abs(CheckGames$CheckWinPct - CheckGames$WinPct)) > 1e-9 ||
    max(abs(CheckGames$CheckPD - CheckGames$PointDiffPerGame)) > 1e-9) {
  stop("team_season outcomes disagree with nfl_team_games")
}

# Starting-QB count against nfl_team_seasons.n_starting_qbs
CheckQB <- tbl(con, "nfl_team_seasons") |>
  select(franchise_id, season, n_starting_qbs) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  inner_join(TeamSeason, by = c("franchise_id", "season"))
message("NStartingQBs equals nfl_team_seasons.n_starting_qbs in ",
        sum(CheckQB$n_starting_qbs == CheckQB$NStartingQBs), " of ", nrow(CheckQB))

# Staff counts against staff_team_season (distinct persons)
CheckStaff <- tbl(con, "staff_team_season") |>
  group_by(franchise_id, season) |>
  summarise(CheckN = n_distinct(person_id), .groups = "drop") |>
  collect() |>
  mutate(season = as.integer(season)) |>
  full_join(TeamSeason |> select(franchise_id, season, NAllStaff, StaffObserved),
            by = c("franchise_id", "season"))
if (any(with(filter(CheckStaff, StaffObserved), coalesce(CheckN, -1L) != coalesce(NAllStaff, -1L))) ||
    any(!is.na(CheckStaff$NAllStaff[!CheckStaff$StaffObserved]))) {
  stop("NAllStaff disagrees with staff_team_season (or is set where no staff box is parsed)")
}

# No impossible values: shares and indices in [0, 1], counts >= 0
share_cols <- names(TeamSeason)[str_detect(names(TeamSeason),
  "^(Share|CodedShare|MeanPBlack|MeanPrior|Blau|HCGamesShare|TopQBStartShare|WinPct|Pythagorean)|BlackPred")]
count_cols <- names(TeamSeason)[str_detect(names(TeamSeason), "^N[A-Z]")]
bad_share <- TeamSeason |> select(all_of(share_cols)) |>
  map_lgl(\(x) any(x < 0 | x > 1, na.rm = TRUE))
bad_count <- TeamSeason |> select(all_of(count_cols)) |>
  map_lgl(\(x) any(x < 0, na.rm = TRUE))
if (any(bad_share) || any(bad_count)) {
  stop("Out-of-range values in: ",
       paste(c(names(bad_share)[bad_share], names(bad_count)[bad_count]), collapse = ", "))
}

write_sample(TeamSeason, "team_season", key = c("franchise_id", "season"),
             labels = TeamSeasonLabels)

# ---------------------------------------------------------------------------
# Coverage report
# ---------------------------------------------------------------------------

message("Share of team-seasons with the full staff observed: ",
        round(mean(TeamSeason$FullStaffObserved), 3),
        " (staff box observed: ", round(mean(TeamSeason$StaffObserved), 3), ")")
TeamSeason |>
  mutate(Era = if_else(season < 2007, "1999-2006 (season articles)",
                       "2007-2025 (staff templates)")) |>
  group_by(Era) |>
  summarise(TeamSeasons = n(),
            MeanAllStaff = mean(NAllStaff, na.rm = TRUE),
            MeanCoaches = mean(NCoaches, na.rm = TRUE),
            MeanFrontOffice = mean(NFrontOffice, na.rm = TRUE),
            ShareWithOC = mean(NOC > 0, na.rm = TRUE),
            .groups = "drop") |>
  print()
# Predicted measures by era (expected shares; every person has a prediction)
TeamSeason |>
  mutate(Era = if_else(season < 2007, "1999-2006", "2007-2025")) |>
  group_by(Era) |>
  summarise(across(c(ShareBlackPredCoaches, ShareBlackPredDocCoaches,
                     ShareBlackProvCoaches, ShareBlackPredFrontOffice,
                     HCBlackPred, HCBlackPredDoc), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  print(width = Inf)
# Opening snapshot vs union of snapshots (template era): headcounts, Black
# share and turnover differ when in-season hires and interim promotions are
# excluded; opening turnover is unknown in 2007 (NA), not zero
Template <- filter(TeamSeason, OpeningStaffObserved)
message("Opening vs union (", nrow(Template), " template-era team-seasons): coaches differ in ",
        sum(Template$NCoachesPre != Template$NCoaches), "; |ShareBlackPredCoachesPre - ",
        "ShareBlackPredCoaches| mean ",
        round(mean(abs(Template$ShareBlackPredCoachesPre - Template$ShareBlackPredCoaches)), 4),
        "; opening HC differs from season HC in ",
        sum(Template$HCPersonIdPre != Template$HCPersonId, na.rm = TRUE),
        "; ShareCoachesNewToFranchisePre NA in ",
        sum(is.na(Template$ShareCoachesNewToFranchisePre)), " (2007: ",
        sum(Template$season == 2007), "); distinct HCIncumbentSpellId ",
        n_distinct(TeamSeason$HCIncumbentSpellId))
message("Share of season HCs with any hand-coded race: ",
        round(mean(!is.na(TeamSeason$HCBlackHand)), 3))
TeamSeason |>
  group_by(season) |>
  summarise(Teams = n(), HCBlackProvisional = sum(HCBlackProv, na.rm = TRUE),
            .groups = "drop") |>
  print(n = Inf)

db_disconnect(con)

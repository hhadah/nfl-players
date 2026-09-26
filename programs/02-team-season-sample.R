# ============================================================================
# 02-team-season-sample.R
# Builds analysis/team_season: one row per franchise_id x season, 1999-2025
# (31 teams in 1999-2001, 32 from 2002; 861 rows). Combines
#   - REG-season outcomes (nfl_team_seasons) and their one-season lags,
#   - staff composition by group (counts; Black share under three separate
#     measures: hand-coded, provisional lower bound, BIFSG mean; expected and
#     categorical Blau indices) from analysis/staff_person_season (01),
#   - head coach, coordinator and GM race and experience,
#   - staff turnover, starting-QB instability and Rooney Rule era indicators.
# Requires 01-staff-person-season.R to have run.
# Date: 2026-09-26
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
bifsg_cols <- c("p_white_bifsg", "p_black_bifsg", "p_hispanic_bifsg",
                "p_api_bifsg", "p_aian_bifsg", "p_multi_bifsg")

# Hand-coded category for the categorical Blau: Hispanic of any race is its
# own category; otherwise the hand-coded race ('unknown' treated as uncoded)
StaffPersonSeason <- StaffPersonSeason |>
  mutate(RaceCategoryHand = case_when(hispanic == "yes" ~ "hispanic",
                                      race == "unknown" ~ NA_character_,
                                      TRUE ~ race))

# Composition of one group of staff: N, the three Black-share measures kept
# separate, coverage of hand codes, and the expected/categorical Blau indices
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
              .groups = "drop") |>
    rename_with(\(x) paste0(x, suffix), -c(franchise_id, season))
}

StaffGroups <- list(
  AllStaff = StaffPersonSeason,
  Coaches = filter(StaffPersonSeason, IsCoach),
  Coordinators = filter(StaffPersonSeason, IsCoordinator),
  PositionCoaches = filter(StaffPersonSeason, IsPositionCoach),
  Assistants = filter(StaffPersonSeason, IsAssistantCoach),
  OffenseCoaches = filter(StaffPersonSeason, IsOffenseCoach),
  DefenseCoaches = filter(StaffPersonSeason, IsDefenseCoach),
  FrontOffice = filter(StaffPersonSeason, IsFrontOffice),
  Personnel = filter(StaffPersonSeason, IsPersonnelScouting)
)

StaffComposition <- StaffGroups |>
  imap(compose_group) |>
  reduce(full_join, by = c("franchise_id", "season"))

# ---------------------------------------------------------------------------
# Head coach of the season (REG games)
# ---------------------------------------------------------------------------

# Person-level race for staff (hand-coded, provisional, BIFSG)
StaffRace <- load_person_race(con, hand_coded) |>
  filter(entity == "staff") |>
  select(person_id, black_any, nonwhite, hispanic, black_provisional, p_black_bifsg)

# Race variables for a role holder, prefixed (e.g. HCBlackHand); the
# provisional indicator is 1 when flagged Black and 0 otherwise (lower bound)
role_race <- function(df, prefix) {
  df |>
    left_join(StaffRace, by = c(PersonId = "person_id")) |>
    transmute(franchise_id, season, PersonId,
              BlackHand = black_any, NonwhiteHand = nonwhite,
              HispanicHand = case_when(hispanic == "yes" ~ 1L,
                                       hispanic == "no" ~ 0L,
                                       TRUE ~ NA_integer_),
              BlackProv = if_else(!is.na(PersonId), coalesce(black_provisional, 0L),
                                  NA_integer_),
              PBlackBifsg = p_black_bifsg) |>
    rename_with(\(x) paste0(prefix, x), -c(franchise_id, season))
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

# Tenure of the season head coach: consecutive seasons in which he coached at
# least one REG game for this franchise, including the current one
# (left-censored in 1999). Counting any REG game keeps a coach's tenure intact
# across a season in which a temporary acting HC coached most games (IND 2012).
HCFranchiseRuns <- GameHC |>
  filter(game_type == "REG") |>
  distinct(franchise_id, HCKey, season) |>
  arrange(franchise_id, HCKey, season) |>
  group_by(franchise_id, HCKey) |>
  mutate(Spell = cumsum(season - lag(season, default = first(season) - 2L) != 1L)) |>
  group_by(franchise_id, HCKey, Spell) |>
  mutate(HCTenure = row_number(), HCTenureLeftCensored = first(season) == 1999L) |>
  ungroup() |>
  select(franchise_id, HCKey, season, HCTenure, HCTenureLeftCensored)
HCTenure <- SeasonHC |>
  transmute(franchise_id, season, HCKey = coalesce(PersonId, HeadCoachName)) |>
  left_join(HCFranchiseRuns, by = c("franchise_id", "HCKey", "season"))

# Change of OC / DC / GM holder relative to the previous season (NA when
# either season has no holder observed)
RoleChanges <- RoleHolders |>
  select(franchise_id, season, OCPersonId, DCPersonId, GMPersonId) |>
  group_by(franchise_id) |>
  mutate(OCChange = as.integer(OCPersonId != lag_within(OCPersonId, season)),
         DCChange = as.integer(DCPersonId != lag_within(DCPersonId, season)),
         GMChange = as.integer(GMPersonId != lag_within(GMPersonId, season))) |>
  ungroup() |>
  select(franchise_id, season, OCChange, DCChange, GMChange)

CoachInflow <- StaffPersonSeason |>
  filter(IsCoach) |>
  group_by(franchise_id, season) |>
  summarise(ShareCoachesNewToFranchise = mean_or_na(as.numeric(NewToFranchise)),
            ShareCoachesPromoted = mean_or_na(as.numeric(PromotedWithinFranchise)),
            .groups = "drop")

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
# Staff-source flags and Rooney Rule eras
# ---------------------------------------------------------------------------

StaffSnapshotFlags <- tbl(con, "staff_snapshots") |>
  select(franchise_id, season, source, parse_ok) |>
  collect() |>
  group_by(franchise_id, season) |>
  summarise(StaffSource = min(source),
            AllSnapshotsParsed = all(parse_ok),
            .groups = "drop") |>
  mutate(season = as.integer(season))

# Rooney Rule eras, by the hiring cycle before the season: the 2003 rule
# (adopted Dec 2002) covers HC hires for 2003+; the extension to GM and senior
# football-operations searches took effect June 15, 2009 (first full hiring
# cycle: 2010); the May 2020 amendments (two external minority candidates for
# HC, one for coordinator and front-office posts) first cover the hiring cycle
# before 2021; the March 28, 2022 amendment (a minority or female offensive
# assistant on every staff; women count toward all interview requirements)
# applies from 2022. Sources: NFL, "The Rooney Rule"
# (nfl.com/causes/inclusion/the-rooney-rule); Wikipedia, "Rooney Rule"
# (accessed 2026-09-26).
rooney_era <- function(season) {
  case_when(season < 2003 ~ "pre_rule",
            season < 2021 ~ "rule_2003",
            season < 2022 ~ "amend_2020",
            TRUE ~ "amend_2022")
}

# ---------------------------------------------------------------------------
# Assemble the team-season sample
# ---------------------------------------------------------------------------

TeamSeason <- tbl(con, "franchise_seasons") |>
  select(franchise_id, season) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  filter(season >= 1999, season <= 2025) |>
  left_join(Outcomes, by = c("franchise_id", "season")) |>
  left_join(QBInstability, by = c("franchise_id", "season")) |>
  left_join(StaffSnapshotFlags, by = c("franchise_id", "season")) |>
  left_join(StaffComposition, by = c("franchise_id", "season")) |>
  left_join(HeadCoach, by = c("franchise_id", "season")) |>
  left_join(HCTurnover |> select(franchise_id, season, HCChange, InSeasonHCChange),
            by = c("franchise_id", "season")) |>
  left_join(HCTenure |> select(-HCKey), by = c("franchise_id", "season")) |>
  left_join(PriorNFLHC, by = c("franchise_id", "season")) |>
  left_join(HCCollege, by = c("franchise_id", "season")) |>
  left_join(RoleHolders, by = c("franchise_id", "season")) |>
  left_join(RoleChanges, by = c("franchise_id", "season")) |>
  left_join(CoachInflow, by = c("franchise_id", "season")) |>
  # Staff observation flags; group counts are 0 (not NA) when the staff box
  # is observed but lists nobody in the group
  mutate(StaffObserved = !is.na(NAllStaff),
         FullStaffObserved = StaffSource == "staff_template" & AllSnapshotsParsed,
         across(c(starts_with("N") & where(is.integer) & !c(NHeadCoaches, NStartingQBs)),
                \(x) if_else(StaffObserved, coalesce(x, 0L), x)),
         RooneyEra = rooney_era(season),
         RooneyRule = as.integer(season >= 2003),
         RooneyFrontOffice2009 = as.integer(season >= 2010),
         RooneyAmend2020 = as.integer(season >= 2021),
         RooneyAmend2022 = as.integer(season >= 2022)) |>
  select(-AllSnapshotsParsed) |>
  arrange(franchise_id, season)

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
group_labels <- function(group, desc) {
  c(N = glue("Number of {desc} listed in any snapshot (0 when the staff box is observed and lists none; NA when unobserved)"),
    CodedShare = glue("Share of {desc} with a hand-coded black_any"),
    ShareBlackHand = glue("Share Black among hand-coded {desc} (NA until coded; use with CodedShare)"),
    ShareBlackProv = glue("Share of {desc} flagged Black by black_provisional (hand code, else Wikipedia category); positive-only lower bound"),
    MeanPBlackBifsg = glue("Mean BIFSG P(Black) of {desc} (secondary; understates Black share)"),
    BlauBifsg = glue("Expected Blau index of {desc} from BIFSG probability vectors: P(two distinct randomly drawn members differ in race) = 1 - sum_k[(sum_i p_ik)^2 - sum_i p_ik^2]/(n(n-1)); NA if n < 2"),
    BlauHand = glue("Categorical Blau index of hand-coded {desc} (Hispanic of any race its own category), same without-replacement formula over coded members; NA if fewer than 2 coded")) |>
    set_names(\(x) paste0(x, group))
}
role_labels <- function(prefix, role) {
  c(PersonId = glue("Staff person_id of the season {role}"),
    BlackHand = glue("{role} hand-coded Black (NA until coded)"),
    NonwhiteHand = glue("{role} hand-coded non-white or Hispanic (NA until coded)"),
    HispanicHand = glue("{role} hand-coded Hispanic (NA until coded)"),
    BlackProv = glue("{role} flagged Black by black_provisional (1) or not flagged (0); lower bound"),
    PBlackBifsg = glue("{role} BIFSG P(Black)")) |>
    set_names(\(x) paste0(prefix, x))
}

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
  OCChange = "Season OC differs from the previous season's (NA if either unobserved)",
  DCChange = "Season DC differs from the previous season's (NA if either unobserved)",
  GMChange = "Season GM differs from the previous season's (NA if either unobserved)",
  ShareCoachesNewToFranchise = "Share of on-field coaches not on the franchise's staff in season - 1 (NA when season - 1 unobserved)",
  ShareCoachesPromoted = "Share of on-field coaches promoted within the franchise from season - 1",
  StaffObserved = "Staff box observed (any staff listed; 22 article-era team-seasons lack one)",
  FullStaffObserved = "Full staff observed: template era (2007+) with all three snapshots parsed; article-era boxes are partial",
  RooneyEra = "Rooney Rule era by hiring cycle: pre_rule (<2003), rule_2003 (2003-2020), amend_2020 (2021), amend_2022 (2022+)",
  RooneyRule = "Season >= 2003 (Rooney Rule in force for the preceding hiring cycle)",
  RooneyFrontOffice2009 = "Season >= 2010 (GM/senior football-operations searches covered from June 15, 2009)",
  RooneyAmend2020 = "Season >= 2021 (May 2020 amendments in force for the preceding hiring cycle)",
  RooneyAmend2022 = "Season >= 2022 (2022 amendment: minority or female offensive assistant)"
)
lagged <- c("WinPct", "PointDiffPerGame", "Pythagorean", "OffEPAPerPlay", "DefEPAPerPlay",
            "OffSuccessRate", "DefSuccessRate", "ExpectedWins", "WinsOverExpected",
            "Playoffs", "PlayoffWins")
TeamSeasonLabels <- c(
  TeamSeasonLabels,
  set_names(paste0(TeamSeasonLabels[lagged], ", previous season (NA if not consecutive)"),
            paste0("Lag", lagged)),
  unlist(unname(imap(GroupDescriptions, \(desc, g) group_labels(g, desc)))),
  role_labels("HC", "season head coach"),
  role_labels("OC", paste("offensive coordinator,", role_holder_rule)),
  role_labels("DC", paste("defensive coordinator,", role_holder_rule)),
  role_labels("STC", paste("special-teams coordinator,", role_holder_rule)),
  role_labels("GM", paste("general manager,", role_holder_rule))
)

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
  full_join(TeamSeason |> select(franchise_id, season, NAllStaff),
            by = c("franchise_id", "season"))
if (any(coalesce(CheckStaff$CheckN, -1L) != coalesce(CheckStaff$NAllStaff, -1L))) {
  stop("NAllStaff disagrees with staff_team_season")
}

# No impossible values: shares and indices in [0, 1], counts >= 0
share_cols <- names(TeamSeason)[str_detect(names(TeamSeason),
  "^(Share|CodedShare|MeanPBlack|Blau|HCGamesShare|TopQBStartShare|WinPct|Pythagorean)")]
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
message("Share of season HCs with any hand-coded race: ",
        round(mean(!is.na(TeamSeason$HCBlackHand)), 3))
TeamSeason |>
  group_by(season) |>
  summarise(Teams = n(), HCBlackProvisional = sum(HCBlackProv, na.rm = TRUE),
            .groups = "drop") |>
  print(n = Inf)

db_disconnect(con)

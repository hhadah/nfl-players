# ============================================================================
# 06-draft-sample.R
# Builds analysis/draft_prospects: one row per draft prospect, draft classes
# 2000-2026. The sampling frame is
#   - every drafted player (nfl_draft_picks; one row per season x pick), and
#   - every NFL combine invitee who was not drafted (nfl_combine rows that
#     match no pick). Undrafted players who were NOT invited to the combine
#     are outside the frame, so Drafted = 0 means "combine invitee, not
#     drafted" and the extensive-margin comparison is among invitees only.
# Combines
#   - draft outcomes (Drafted, Round, Pick, LogPick),
#   - realized NFL outcomes from PFR (nfl_draft_picks: weighted AV, games,
#     seasons started, Pro Bowls, All-Pro) and from the DB (seasons rostered
#     and played, games, starts, contracts after the rookie deal),
#   - strictly pre-draft signals: combine measurables, college production
#     and final college team context, recruit profile, CFBD pre-draft grade
#     and rankings, age at the draft, position group, height and weight,
#   - race measures from load_person_race (prospects with a gsis_id only;
#     kept separate, never imputed).
# Helpers: programs/00-player-functions.R.
# Date: 2026-09-26
# ============================================================================

source(file.path(programs, "00-player-functions.R"))

con <- db_connect()

# First and last draft classes in the sample; last NFL season in the DB
first_class <- 2000L
last_class <- 2026L
last_nfl_season <- 2025L

# Normalized name for exact name matching: lower case, no punctuation, no
# generational suffix
normalize_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z ]", "") |>
    str_remove("\\s+(jr|sr|ii|iii|iv|v)$") |>
    str_squish()
}

# School strings agree when they are equal after normalization or share a
# distinctive token (3+ letters, not a generic word): 'Ball State' ~
# 'Ball St.', 'Tenn-Chattanooga' ~ 'Chattanooga', 'USC' ~ 'USC'
school_agree <- function(a, b) {
  generic <- c("state", "university", "college", "tech", "north", "south",
               "east", "west", "northern", "southern", "eastern", "western",
               "central")
  tokens <- function(x) {
    t <- str_split(str_to_lower(str_replace_all(coalesce(x, ""), "[^A-Za-z]", " ")), "\\s+")
    map(t, \(v) setdiff(v[nchar(v) >= 3], c(generic, "the", "and", "univ")))
  }
  normalize_school <- function(x) str_squish(str_to_lower(str_replace_all(coalesce(x, ""), "[^A-Za-z]", " ")))
  (normalize_school(a) == normalize_school(b) & normalize_school(a) != "") |
    map2_lgl(tokens(a), tokens(b), \(x, y) length(intersect(x, y)) > 0)
}

# ---------------------------------------------------------------------------
# Drafted players (nfl_draft_picks)
# ---------------------------------------------------------------------------

Picks <- tbl(con, "nfl_draft_picks") |>
  filter(season >= first_class, season <= last_class) |>
  collect() |>
  mutate(season = as.integer(season), pick = as.integer(pick),
         NameKey = normalize_name(pfr_player_name))

# ---------------------------------------------------------------------------
# Combine rows and their match to a pick
# ---------------------------------------------------------------------------

Combine <- tbl(con, "nfl_combine") |>
  filter(season >= first_class, season <= last_class) |>
  collect() |>
  arrange(season, player_name, pos) |>
  mutate(season = as.integer(season), CombineRowId = row_number(),
         NameKey = normalize_name(player_name))

# Match each combine row to at most one pick, in three steps:
#   1. gsis_id: the combine row's gsis_id is the pick's gsis_id (the combine
#      year may precede the draft year, e.g. an invitee drafted a year later);
#   2. slot: the source draft year equals the combine year and (draft year,
#      overall pick) is a pick not already matched in step 1 (the source
#      copies a drafted player's slot onto same-name invitees, e.g. the two
#      2000 Mike Greens; those twins stay unmatched);
#   3. name: remaining combine rows whose normalized name matches a
#      remaining pick of the same class (without a conflicting gsis_id) and
#      whose school agrees with the pick's college (school_agree); the pair
#      must be unique on both sides. Most such rows are 2021-2026 invitees
#      whose combine rows carry no draft fields.
MatchGsis <- Combine |>
  filter(!is.na(gsis_id)) |>
  inner_join(select(Picks, pick_season = season, pick, gsis_id), by = "gsis_id") |>
  filter(season <= pick_season) |>
  transmute(CombineRowId, season = pick_season, pick, CombineMatch = "gsis_id")
MatchSlot <- Combine |>
  filter(!CombineRowId %in% MatchGsis$CombineRowId, !is.na(draft_ovr),
         draft_year == season) |>
  inner_join(Picks |>
               anti_join(MatchGsis, by = c("season", "pick")) |>
               select(season, pick, pick_gsis = gsis_id),
             by = c(season = "season", draft_ovr = "pick")) |>
  filter(is.na(gsis_id) | gsis_id == coalesce(pick_gsis, gsis_id)) |>
  transmute(CombineRowId, season, pick = as.integer(draft_ovr), CombineMatch = "slot")
Matched <- bind_rows(MatchGsis, MatchSlot)
MatchName <- Combine |>
  filter(!CombineRowId %in% Matched$CombineRowId) |>
  inner_join(Picks |>
               anti_join(Matched, by = c("season", "pick")) |>
               select(season, pick, NameKey, college, pick_gsis = gsis_id),
             by = c("season", "NameKey")) |>
  filter(is.na(pick_gsis) | is.na(gsis_id) | pick_gsis == gsis_id,
         school_agree(school, college)) |>
  add_count(CombineRowId, name = "NPick") |>
  add_count(season, pick, name = "NCombine") |>
  filter(NPick == 1, NCombine == 1) |>
  transmute(CombineRowId, season, pick, CombineMatch = "name")
Matched <- bind_rows(Matched, MatchName)
stopifnot(!anyDuplicated(Matched$CombineRowId),
          !anyDuplicated(Matched[c("season", "pick")]))

# ---------------------------------------------------------------------------
# Prospect frame: drafted players plus unmatched (undrafted) invitees
# ---------------------------------------------------------------------------

Drafted <- Picks |>
  left_join(Matched, by = c("season", "pick")) |>
  left_join(select(Combine, CombineRowId, combine_gsis = gsis_id), by = "CombineRowId") |>
  transmute(DraftClass = season, Drafted = 1L, Round = as.integer(round),
            Pick = pick, gsis_id = coalesce(gsis_id, combine_gsis),
            ProspectName = pfr_player_name, Position = position,
            PositionGroup = position_group, College = college,
            DraftFranchise = franchise_id, CombineRowId, CombineMatch,
            NameKey)

# Undrafted invitees: combine rows that match no pick. DraftStatusUncertain
# flags rows whose undrafted status is doubtful: the source carries draft
# fields that match no pick (same-name twins, wrong draft year), an
# unmatched pick of the same class has the same normalized name, or
# nfl_players records a draft year for the linked gsis_id
UnmatchedPicks <- Picks |> anti_join(Matched, by = c("season", "pick"))
PlayersDraftYear <- tbl(con, "nfl_players") |>
  select(gsis_id, players_draft_year = draft_year) |>
  collect()
Undrafted <- Combine |>
  anti_join(Matched, by = "CombineRowId") |>
  left_join(PlayersDraftYear, by = "gsis_id") |>
  mutate(DraftStatusUncertain = as.integer(
    !is.na(draft_ovr) | !is.na(players_draft_year) |
      paste(season, NameKey) %in% paste(UnmatchedPicks$season, UnmatchedPicks$NameKey))) |>
  transmute(DraftClass = season, Drafted = 0L, Round = NA_integer_,
            Pick = NA_integer_, gsis_id, ProspectName = player_name,
            Position = pos, PositionGroup = position_group, College = school,
            DraftFranchise = NA_character_, CombineRowId,
            CombineMatch = NA_character_, NameKey, DraftStatusUncertain,
            combine_pfr_id = pfr_id)
Prospects <- bind_rows(Drafted, Undrafted)

# ---------------------------------------------------------------------------
# Plausibility of the gsis_id links
# ---------------------------------------------------------------------------

# Some upstream links attach a prospect to a player of another era (e.g. a
# 2000 pick or a 2000 combine invitee linked to a player born in 1986-88).
# A link is suspect when the linked player's birth date implies an age on
# April 30 of the draft year below 19 or above 31, or the linked player is
# on an NFL season roster before the draft class. Suspect links are dropped
# (gsis_id set to NA, kept in SuspectGsisId) so that no gsis-keyed data
# (college, recruit, race, NFL outcomes) is attached from the wrong person.
# Birth date: nfl_players, else the latest non-missing season-roster value.
BirthDates <- tbl(con, "nfl_players") |>
  select(gsis_id, birth_date) |>
  collect() |>
  mutate(birth_date = as.Date(birth_date))
RosterBirth <- tbl(con, "nfl_rosters_season") |>
  filter(!is.na(gsis_id), !is.na(birth_date)) |>
  select(gsis_id, season, birth_date) |>
  collect() |>
  arrange(gsis_id, desc(season)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(gsis_id, roster_birth_date = birth_date)
FirstRoster <- tbl(con, "nfl_rosters_season") |>
  filter(!is.na(gsis_id)) |>
  group_by(gsis_id) |>
  summarise(FirstNFLRosterSeason = min(season, na.rm = TRUE), .groups = "drop") |>
  collect() |>
  mutate(FirstNFLRosterSeason = as.integer(FirstNFLRosterSeason))
Prospects <- Prospects |>
  left_join(BirthDates, by = "gsis_id") |>
  left_join(RosterBirth, by = "gsis_id") |>
  left_join(FirstRoster, by = "gsis_id") |>
  mutate(BirthDate = coalesce(birth_date, as.Date(roster_birth_date)),
         LinkAge = as.numeric(as.Date(paste0(DraftClass, "-04-30")) - BirthDate) / 365.25,
         GsisLinkSuspect = as.integer(!is.na(gsis_id) &
                                        (coalesce(LinkAge < 19 | LinkAge > 31, FALSE) |
                                           coalesce(FirstNFLRosterSeason < DraftClass, FALSE))),
         SuspectGsisId = if_else(GsisLinkSuspect == 1L, gsis_id, NA_character_),
         gsis_id = if_else(GsisLinkSuspect == 1L, NA_character_, gsis_id),
         BirthDate = if_else(GsisLinkSuspect == 1L, as.Date(NA), BirthDate),
         FirstNFLRosterSeason = if_else(GsisLinkSuspect == 1L, NA_integer_, FirstNFLRosterSeason)) |>
  select(-birth_date, -roster_birth_date, -LinkAge)

# ProspectId: gsis_id when available (and not suspect); otherwise
# 'D<class>-<pick>' for a drafted player and 'C<class>-<pfr_id>' (else
# 'C<class>-<name>-<pos>') for an undrafted invitee. Deterministic given
# the source tables.
Prospects <- Prospects |>
  mutate(DraftStatusUncertain = coalesce(DraftStatusUncertain, 0L),
         ProspectId = case_when(
           !is.na(gsis_id) ~ gsis_id,
           Drafted == 1L ~ sprintf("D%d-%03d", DraftClass, Pick),
           !is.na(combine_pfr_id) ~ paste0("C", DraftClass, "-", combine_pfr_id),
           TRUE ~ paste0("C", DraftClass, "-", str_replace_all(NameKey, " ", "_"),
                         "-", Position)),
         ProspectIdSource = case_when(!is.na(gsis_id) ~ "gsis_id",
                                      Drafted == 1L ~ "draft_slot",
                                      TRUE ~ "combine"),
         CombineInvite = as.integer(!is.na(CombineRowId)),
         LogPick = log(Pick)) |>
  select(-combine_pfr_id)
check_key(Prospects, "ProspectId", "Prospects")

# ---------------------------------------------------------------------------
# Pre-draft signals: combine measurables and CFBD pre-draft grade
# ---------------------------------------------------------------------------

# Measurables come from the prospect's own combine row (CombineRowId), so
# they are available for invitees without a gsis_id as well
# The RAS-style athletic score is computed on all nfl_combine rows (the
# reference population) and joined by the row's natural key
AthleticScores <- combine_athletic_scores(con) |>
  select(season, player_name, pos, school, AthleticScore, AthleticScoreN,
         AthleticSizeScore, AthleticSpeedScore, AthleticExplosionScore,
         AthleticAgilityScore)
stopifnot(!anyDuplicated(AthleticScores[c("season", "player_name", "pos", "school")]))
CombineMeasures <- Combine |>
  left_join(AthleticScores, by = c("season", "player_name", "pos", "school")) |>
  transmute(CombineRowId, CombineYear = season,
            CombineHeight = height_to_inches(ht), CombineWeight = wt,
            Forty = forty, Vertical = vertical, Bench = bench,
            BroadJump = broad_jump, Cone = cone, Shuttle = shuttle,
            AthleticScore, AthleticScoreN, AthleticSizeScore,
            AthleticSpeedScore, AthleticExplosionScore, AthleticAgilityScore,
            CombineLinkMethod = link_method)

# CFBD draft records (drafted players only; 2000-2026) joined by slot
# (season, overall pick); a record is kept when its gsis_id equals the
# pick's or, for picks without a gsis_id, the last names agree
CfbdDraft <- tbl(con, "cfbd_draft_picks") |>
  select(season, overall, name, cfbd_gsis = gsis_id, pre_draft_ranking,
         pre_draft_position_ranking, pre_draft_grade, college_conference,
         height, weight) |>
  collect() |>
  transmute(DraftClass = as.integer(season), Pick = as.integer(overall),
            CfbdLastName = word(normalize_name(name), -1), cfbd_gsis,
            PreDraftRank = pre_draft_ranking,
            PreDraftPosRank = pre_draft_position_ranking,
            PreDraftGrade = pre_draft_grade,
            CfbdDraftConference = college_conference,
            # listed heights outside 60-84 inches are data-entry errors
            CfbdHeight = if_else(between(height, 60, 84), height, NA_real_),
            CfbdWeight = weight)

Prospects <- Prospects |>
  left_join(CombineMeasures, by = "CombineRowId") |>
  left_join(CfbdDraft, by = c("DraftClass", "Pick")) |>
  mutate(CfbdAgree = coalesce(cfbd_gsis == gsis_id,
                              CfbdLastName == word(NameKey, -1), FALSE),
         across(c(PreDraftRank, PreDraftPosRank, PreDraftGrade,
                  CfbdDraftConference, CfbdHeight, CfbdWeight),
                \(x) if_else(CfbdAgree, x, NA)),
         CfbdDraftRecord = as.integer(CfbdAgree)) |>
  select(-CfbdLastName, -cfbd_gsis, -CfbdAgree)

# ---------------------------------------------------------------------------
# Pre-draft signals: college production and final college team context
# ---------------------------------------------------------------------------

# college_production() keeps only CFBD seasons before the player's NFL entry
# year. As a guard, every college variable is set to NA when the final CFBD
# season is not strictly before the draft class (CollegePreDraftViolation)
College <- college_production(con)
college_vars <- setdiff(names(College), "gsis_id")
Prospects <- Prospects |>
  left_join(College, by = "gsis_id") |>
  mutate(CollegePreDraftViolation = as.integer(coalesce(FinalCollegeSeason >= DraftClass, FALSE)),
         HasCollegeLink = as.integer(!is.na(FinalCollegeSeason) & CollegePreDraftViolation == 0L),
         across(all_of(college_vars), \(x) if_else(CollegePreDraftViolation == 1L, NA, x)))

# ---------------------------------------------------------------------------
# Pre-draft signals: recruit profile
# ---------------------------------------------------------------------------

# 247 composite profile; the recruiting class must precede the draft class
Recruit <- recruit_signals(con)
recruit_vars <- setdiff(names(Recruit), "gsis_id")
Prospects <- Prospects |>
  left_join(Recruit, by = "gsis_id") |>
  mutate(HasRecruit = as.integer(!is.na(recruit_id) & coalesce(RecruitClass < DraftClass, FALSE)),
         across(all_of(recruit_vars), \(x) if_else(HasRecruit == 1L, x, NA)))

# ---------------------------------------------------------------------------
# Age at the draft, height and weight
# ---------------------------------------------------------------------------

# The DB has no draft dates, so the draft date is set to April 30 of the
# draft year (DraftDateAssumed); actual dates vary by year, so AgeAtDraft
# carries an error of a few days to about two weeks.
Prospects <- Prospects |>
  mutate(DraftDateAssumed = as.Date(paste0(DraftClass, "-04-30")),
         AgeAtDraft = as.numeric(DraftDateAssumed - BirthDate) / 365.25,
         # Height and weight: combine measurement, else the CFBD draft listing
         Height = coalesce(CombineHeight, CfbdHeight),
         Weight = coalesce(CombineWeight, CfbdWeight),
         HeightWeightSource = case_when(!is.na(CombineHeight) | !is.na(CombineWeight) ~ "combine",
                                        !is.na(CfbdHeight) | !is.na(CfbdWeight) ~ "cfbd_draft",
                                        TRUE ~ NA_character_))

# ---------------------------------------------------------------------------
# Realized NFL outcomes: PFR career values (drafted players)
# ---------------------------------------------------------------------------

# nfl_draft_picks carries PFR career values for every pick. games, w_av and
# 'to' are NA exactly for picks with no NFL game (none of them has a row in
# nfl_player_stats_season), so they are set to 0 for drafted players; the
# other counts are never NA. car_av (career AV) is NA for every row in the
# DB (stored as BOOLEAN upstream), so career AV is not available; weighted
# AV (w_av) and draft-team AV (dr_av) are. Values for recent classes are
# censored (the 2026 class has at most a few 2026 games).
PfrOutcomes <- Picks |>
  transmute(DraftClass = season, Pick = pick,
            PfrGames = coalesce(games, 0L), PfrWeightedAV = coalesce(w_av, 0L),
            PfrDraftTeamAV = coalesce(dr_av, 0L),
            PfrSeasonsStarted = seasons_started, PfrProBowls = probowls,
            PfrAllPro = allpro, PfrHallOfFame = as.integer(hof),
            PfrLastSeason = `to`, PfrNoGames = as.integer(is.na(games)))
Prospects <- Prospects |>
  left_join(PfrOutcomes, by = c("DraftClass", "Pick"))

# ---------------------------------------------------------------------------
# Realized NFL outcomes: seasons, games and starts from the DB
# ---------------------------------------------------------------------------

# Seasons on any NFL season roster (nfl_rosters_season, 1999-2025, any
# status) from the draft class on
RosterSeasons <- tbl(con, "nfl_rosters_season") |>
  filter(!is.na(gsis_id)) |>
  distinct(gsis_id, season) |>
  collect() |>
  mutate(season = as.integer(season))

# Weekly usage (REG, 2002-2025): games played, depth-chart starts, game-day
# roster weeks (nfl_season_usage)
Usage <- nfl_season_usage(con) |>
  select(gsis_id, season, WeeksGameDay, GamesPlayed, GamesStartedDepth)

ProspectSeasons <- Prospects |>
  filter(!is.na(gsis_id)) |>
  select(ProspectId, gsis_id, DraftClass)
Rostered <- ProspectSeasons |>
  inner_join(RosterSeasons, by = "gsis_id") |>
  filter(season >= DraftClass) |>
  group_by(ProspectId) |>
  summarise(NFLSeasonsRostered = n_distinct(season),
            NFLSeasonsRosteredFirst3 = n_distinct(season[season <= DraftClass + 2]),
            LastNFLRosterSeason = max(season), .groups = "drop")
Played <- ProspectSeasons |>
  inner_join(Usage, by = "gsis_id") |>
  filter(season >= DraftClass) |>
  group_by(ProspectId) |>
  summarise(NFLSeasonsGameDay = sum(WeeksGameDay > 0),
            NFLSeasonsPlayed = sum(GamesPlayed > 0),
            NFLGamesPlayed = sum(GamesPlayed),
            NFLGamesStarted = sum(GamesStartedDepth),
            NFLGamesPlayedFirst3 = sum(GamesPlayed[season <= DraftClass + 2]),
            NFLGamesStartedFirst3 = sum(GamesStartedDepth[season <= DraftClass + 2]),
            .groups = "drop")

# Observability: DB outcomes need a gsis_id; weekly measures need the whole
# career window inside 2002-2025 at the start (class >= 2002) and at least
# one observed season (class <= 2025); the first-3 versions also need the
# three seasons class..class+2 to be observed (class <= 2023). A linked
# prospect with no roster/usage row has 0.
Prospects <- Prospects |>
  left_join(Rostered, by = "ProspectId") |>
  left_join(Played, by = "ProspectId") |>
  mutate(NFLSeasonsObservable = pmax(last_nfl_season - DraftClass + 1L, 0L),
         Linked = !is.na(gsis_id),
         across(c(NFLSeasonsRostered, NFLSeasonsRosteredFirst3),
                \(x) if_else(Linked & DraftClass <= last_nfl_season, coalesce(x, 0L), NA_integer_)),
         across(c(NFLSeasonsGameDay, NFLSeasonsPlayed, NFLGamesPlayed, NFLGamesStarted),
                \(x) if_else(Linked & between(DraftClass, 2002L, last_nfl_season),
                             coalesce(as.integer(x), 0L), NA_integer_)),
         across(c(NFLGamesPlayedFirst3, NFLGamesStartedFirst3),
                \(x) if_else(Linked & between(DraftClass, 2002L, last_nfl_season - 2L),
                             coalesce(as.integer(x), 0L), NA_integer_)),
         NFLSeasonsRosteredFirst3 = if_else(DraftClass <= last_nfl_season - 2L,
                                            NFLSeasonsRosteredFirst3, NA_integer_),
         MadeNFLRoster = as.integer(NFLSeasonsRostered > 0),
         # Lower-bound version: unlinked prospects (no gsis_id) coded 0,
         # on the assumption that a player who made a roster has a gsis_id
         MadeNFLRosterUnlinked0 = if_else(DraftClass <= last_nfl_season,
                                          coalesce(MadeNFLRoster, 0L), NA_integer_)) |>
  select(-Linked)

# ---------------------------------------------------------------------------
# Realized NFL outcomes: contracts after the rookie deal (OverTheCap)
# ---------------------------------------------------------------------------

# Contract types (nfl_contracts.contract_type, from the OTC history):
#   rookie:  Drafted, UDFA;
#   veteran market contracts: Extension, UFA, RFA, Franchise, Transition
#            (the market-pricing margin);
#   other non-rookie: Practice, SFA (futures/street free agent), ERFA,
#            Other. Rows without a type are ignored.
# Only contracts signed in or after the draft year count; rows with a
# missing signing year (year_signed_missing) count for the ever-indicators
# but not for timing.
veteran_types <- c("Extension", "UFA", "RFA", "Franchise", "Transition")
Contracts <- tbl(con, "nfl_contracts") |>
  filter(!is.na(gsis_id), !is.na(contract_type), contract_type != "") |>
  select(contract_id, gsis_id, contract_type, year_signed, year_signed_missing,
         years, apy, apy_cap_pct, inflated_apy, guaranteed, value) |>
  collect() |>
  inner_join(select(filter(Prospects, !is.na(gsis_id)), ProspectId, gsis_id, DraftClass),
             by = "gsis_id") |>
  filter(year_signed_missing | year_signed >= DraftClass) |>
  mutate(Rookie = contract_type %in% c("Drafted", "UDFA"),
         Veteran = contract_type %in% veteran_types)
ContractSummary <- Contracts |>
  group_by(ProspectId) |>
  summarise(RookieContract = as.integer(any(Rookie)),
            SecondContract = as.integer(any(!Rookie)),
            VeteranContract = as.integer(any(Veteran)),
            .groups = "drop")
# First veteran market contract with a signing year (earliest year, then
# the highest APY within that year)
FirstVeteran <- Contracts |>
  filter(Veteran, !year_signed_missing) |>
  arrange(ProspectId, year_signed, desc(apy), contract_id) |>
  distinct(ProspectId, .keep_all = TRUE) |>
  transmute(ProspectId, VetContractYear = as.integer(year_signed),
            VetContractType = contract_type, VetContractYears = as.integer(years),
            VetContractAPY = apy, VetContractAPYCapPct = apy_cap_pct,
            VetContractInflatedAPY = inflated_apy,
            VetContractGuaranteed = guaranteed, VetContractValue = value)
FirstSecond <- Contracts |>
  filter(!Rookie, !year_signed_missing) |>
  group_by(ProspectId) |>
  summarise(SecondContractYear = as.integer(min(year_signed)), .groups = "drop")

# Contract coverage by class and draft status. Drafted: share of linked
# picks with a Drafted contract row; OTC contracts are near-complete only
# from the 2011 class (2000-2010: 3-42%; 2011+: 86-100%, with a dip to
# 86-88% in 2015-2016). Contract outcomes are set to NA for class x status
# cells below 80% coverage (ContractClassCovered = 0) and for prospects
# without a gsis_id.
CoverageDrafted <- Prospects |>
  filter(Drafted == 1L, !is.na(gsis_id)) |>
  left_join(ContractSummary, by = "ProspectId") |>
  group_by(DraftClass) |>
  summarise(ContractShare = mean(coalesce(RookieContract, 0L)), .groups = "drop") |>
  mutate(Drafted = 1L)
# Undrafted invitees need their own coverage rule: OTC UDFA coverage lags
# the drafted coverage by several classes (e.g. about 10-18% of linked
# 2011-2015 invitees have a UDFA row, against 99% of picks). Not every
# invitee signs, so the denominator is linked invitees who appear on an NFL
# season roster from the class on (they must have signed a contract); the
# share is those with any typed OTC contract signed in/after the class.
# The rule needs roster data, so the 2026 invitees are not covered.
CoverageUndrafted <- Prospects |>
  filter(Drafted == 0L, !is.na(gsis_id), coalesce(MadeNFLRoster, 0L) == 1L) |>
  left_join(ContractSummary, by = "ProspectId") |>
  group_by(DraftClass) |>
  summarise(ContractShare = mean(!is.na(RookieContract)), .groups = "drop") |>
  mutate(Drafted = 0L)
Coverage <- bind_rows(CoverageDrafted, CoverageUndrafted) |>
  mutate(ContractClassCovered = as.integer(ContractShare >= 0.8))

# All contract outcomes (indicators, timing and terms) are NA unless the
# prospect is linked and the class x draft status is covered
contract_terms <- c("SecondContractYear", "VetContractYear", "VetContractType",
                    "VetContractYears", "VetContractAPY", "VetContractAPYCapPct",
                    "VetContractInflatedAPY", "VetContractGuaranteed", "VetContractValue")
n_before <- nrow(Prospects)
Prospects <- Prospects |>
  left_join(ContractSummary, by = "ProspectId") |>
  left_join(FirstSecond, by = "ProspectId") |>
  left_join(FirstVeteran, by = "ProspectId") |>
  left_join(select(Coverage, DraftClass, Drafted, ContractClassCovered),
            by = c("DraftClass", "Drafted")) |>
  mutate(ContractClassCovered = coalesce(ContractClassCovered, 0L),
         ContractObservable = !is.na(gsis_id) & ContractClassCovered == 1L,
         across(c(RookieContract, SecondContract, VeteranContract),
                \(x) if_else(ContractObservable, coalesce(x, 0L), NA_integer_)),
         across(all_of(contract_terms), \(x) if_else(ContractObservable, x, NA)),
         YearsToSecondContract = SecondContractYear - DraftClass,
         YearsToVetContract = VetContractYear - DraftClass) |>
  select(-ContractObservable)
stopifnot(nrow(Prospects) == n_before)

# ---------------------------------------------------------------------------
# Race measures (prospects with a gsis_id; kept separate, never imputed)
# ---------------------------------------------------------------------------

PersonRace <- load_person_race(con, hand_coded) |>
  filter(entity == "player") |>
  select(gsis_id = person_id, race, hispanic, black_any, nonwhite, race_source,
         black_provisional, black_provisional_source, wiki_cat_black,
         p_black_bifsg, p_white_bifsg, p_hispanic_bifsg, race_bifsg)
Prospects <- Prospects |>
  left_join(PersonRace, by = "gsis_id")

db_disconnect(con)

# ---------------------------------------------------------------------------
# Column order
# ---------------------------------------------------------------------------

identity_vars <- c("ProspectId", "ProspectIdSource", "gsis_id", "GsisLinkSuspect",
                   "SuspectGsisId", "DraftClass", "ProspectName", "Position",
                   "PositionGroup", "College", "BirthDate", "DraftDateAssumed",
                   "AgeAtDraft", "Height", "Weight", "HeightWeightSource")
draft_vars <- c("Drafted", "DraftStatusUncertain", "Round", "Pick", "LogPick",
                "DraftFranchise")
combine_vars <- c("CombineInvite", "CombineRowId", "CombineMatch", "CombineYear",
                  "CombineHeight", "CombineWeight", "Forty", "Vertical", "Bench",
                  "BroadJump", "Cone", "Shuttle", "AthleticScore", "AthleticScoreN",
                  "AthleticSizeScore", "AthleticSpeedScore", "AthleticExplosionScore",
                  "AthleticAgilityScore", "CombineLinkMethod")
cfbd_vars <- c("CfbdDraftRecord", "PreDraftRank", "PreDraftPosRank", "PreDraftGrade",
               "CfbdDraftConference", "CfbdHeight", "CfbdWeight")
pfr_vars <- c("PfrNoGames", "PfrGames", "PfrWeightedAV", "PfrDraftTeamAV",
              "PfrSeasonsStarted", "PfrProBowls", "PfrAllPro", "PfrHallOfFame",
              "PfrLastSeason")
nfl_vars <- c("NFLSeasonsObservable", "FirstNFLRosterSeason", "LastNFLRosterSeason",
              "MadeNFLRoster", "MadeNFLRosterUnlinked0", "NFLSeasonsRostered",
              "NFLSeasonsRosteredFirst3", "NFLSeasonsGameDay", "NFLSeasonsPlayed",
              "NFLGamesPlayed", "NFLGamesStarted", "NFLGamesPlayedFirst3",
              "NFLGamesStartedFirst3")
contract_vars <- c("ContractClassCovered", "RookieContract", "SecondContract",
                   "SecondContractYear", "YearsToSecondContract", "VeteranContract",
                   "VetContractYear", "YearsToVetContract", "VetContractType",
                   "VetContractYears", "VetContractAPY", "VetContractAPYCapPct",
                   "VetContractInflatedAPY", "VetContractGuaranteed", "VetContractValue")
race_vars <- c("race", "hispanic", "black_any", "nonwhite", "race_source",
               "black_provisional", "black_provisional_source", "wiki_cat_black",
               "p_black_bifsg", "p_white_bifsg", "p_hispanic_bifsg", "race_bifsg")
DraftProspects <- Prospects |>
  select(all_of(c(identity_vars, draft_vars, combine_vars, cfbd_vars,
                  "HasCollegeLink", "CollegePreDraftViolation", college_vars,
                  "HasRecruit", recruit_vars, pfr_vars, nfl_vars, contract_vars,
                  race_vars))) |>
  arrange(DraftClass, desc(Drafted), Pick, ProspectName)
stopifnot(setequal(names(DraftProspects),
                   setdiff(names(Prospects), c("NameKey"))))

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

IdentityLabels <- c(
  ProspectId = "Prospect id: gsis_id when linked (and not suspect), else 'D<class>-<pick>' (drafted) or 'C<class>-<pfr_id>' / 'C<class>-<name>-<pos>' (undrafted combine invitee)",
  ProspectIdSource = "Source of ProspectId (gsis_id, draft_slot, combine)",
  gsis_id = "NFL GSIS id (draft pick's id, else the combine row's id; NA when unlinked or suspect)",
  GsisLinkSuspect = "1 if the upstream gsis_id link was dropped: linked birth date implies age < 19 or > 31 on April 30 of the draft year, or the linked player is on an NFL season roster before the draft class",
  SuspectGsisId = "The dropped suspect gsis_id (audit only; nothing is joined on it)",
  DraftClass = "Draft class: draft year (drafted) or combine year (undrafted invitees)",
  ProspectName = "Name (PFR draft record for drafted; combine record for undrafted)",
  Position = "Position (draft record for drafted; combine for undrafted)",
  PositionGroup = "Position group (QB, RB, WR, TE, OL, DL, LB, DB, K, P, LS)",
  College = "College (draft record for drafted; combine school for undrafted)",
  BirthDate = "Birth date (nfl_players, else latest season roster; linked prospects only)",
  DraftDateAssumed = "Assumed draft date: April 30 of the draft year (no draft dates in the DB)",
  AgeAtDraft = "Age in years on DraftDateAssumed",
  Height = "Height, inches: combine measurement, else CFBD draft listing",
  Weight = "Weight, lb: combine measurement, else CFBD draft listing",
  HeightWeightSource = "Source of Height/Weight (combine, cfbd_draft)"
)

DraftLabels <- c(
  Drafted = "1 if selected in the NFL draft (nfl_draft_picks); 0 = combine invitee not drafted",
  DraftStatusUncertain = "1 for an undrafted invitee whose status is doubtful: source draft fields match no pick, an unmatched pick of the class has the same name, or nfl_players records a draft year",
  Round = "Draft round (NA if undrafted)",
  Pick = "Overall pick number (NA if undrafted)",
  LogPick = "log(Pick) (NA if undrafted)",
  DraftFranchise = "Drafting franchise_id"
)

CombineLabels <- c(
  CombineInvite = "1 if the prospect has an NFL combine row (all undrafted prospects by construction)",
  CombineRowId = "Row id of the matched nfl_combine row within this build (ordered by season, name, position)",
  CombineMatch = "How a drafted player's combine row was matched: gsis_id, slot (draft year + overall pick), name (exact normalized name + school agreement)",
  CombineYear = "Combine year", CombineHeight = "Combine height, inches",
  CombineWeight = "Combine weight, lb", Forty = "40-yard dash, seconds",
  Vertical = "Vertical jump, inches", Bench = "Bench press reps (225 lb)",
  BroadJump = "Broad jump, inches", Cone = "3-cone drill, seconds",
  Shuttle = "20-yard shuttle, seconds",
  AthleticScore = "RAS-style athletic score, 0-10: percentile of the mean component score within position through the combine year (combine_athletic_scores(); >= 6 of 8 measurables)",
  AthleticScoreN = "Number of the 8 measurables scored (height, weight, forty, bench, vertical, broad jump, cone, shuttle)",
  AthleticSizeScore = "Mean 0-10 percentile score of height and weight within position",
  AthleticSpeedScore = "0-10 percentile score of the forty within position (faster = higher)",
  AthleticExplosionScore = "Mean 0-10 percentile score of bench, vertical and broad jump within position",
  AthleticAgilityScore = "Mean 0-10 percentile score of the cone and shuttle within position (faster = higher)",
  CombineLinkMethod = "Upstream combine-to-gsis link method (pfr_id, draft_slot, name_pos_year)"
)

CfbdLabels <- c(
  CfbdDraftRecord = "1 if a CFBD draft record matches the pick (slot + gsis_id or last name); drafted players only",
  PreDraftRank = "CFBD pre-draft overall ranking (drafted players only)",
  PreDraftPosRank = "CFBD pre-draft position ranking (drafted players only)",
  PreDraftGrade = "CFBD pre-draft grade (drafted players only)",
  CfbdDraftConference = "College conference in the CFBD draft record",
  CfbdHeight = "Height in the CFBD draft record, inches",
  CfbdWeight = "Weight in the CFBD draft record, lb"
)

# College stat families (career totals and final pre-NFL season)
CollegeStatDesc <- c(
  PassAtt = "pass attempts", PassComp = "pass completions", PassYds = "passing yards",
  PassTD = "passing TDs", PassInt = "interceptions thrown", RushAtt = "rushing attempts",
  RushYds = "rushing yards", RushTD = "rushing TDs", Rec = "receptions",
  RecYds = "receiving yards", RecTD = "receiving TDs",
  Tackles = "total tackles (2016+ seasons only)", SoloTackles = "solo tackles (2016+ only)",
  TFL = "tackles for loss (2016+ only)", Sacks = "sacks (2016+ only)",
  QBHurries = "QB hurries (2016+ only)", PassesDefended = "passes defended (2016+ only)",
  DefInt = "interceptions", FumblesRecovered = "fumbles recovered (2016+ only)",
  FGMade = "field goals made", FGAtt = "field goals attempted", XPMade = "extra points made",
  XPAtt = "extra points attempted", Punts = "punts", PuntYds = "punt yards")
CollegeLabels <- c(
  setNames(glue("College career {CollegeStatDesc}, pre-NFL seasons (NA unless CollegeCareerObservable)"),
           paste0("Coll", names(CollegeStatDesc))),
  setNames(glue("Final pre-NFL college season {CollegeStatDesc} (NA unless CollegeFinalObservable)"),
           paste0("CollFinal", names(CollegeStatDesc))),
  CollCompletionPct = "College career completions / attempts",
  CollYardsPerAttempt = "College career passing yards / attempts",
  CollYardsPerCarry = "College career rushing yards / carries",
  CollYardsPerReception = "College career receiving yards / receptions",
  CollFGPct = "College career FG made / attempted",
  CollYardsPerPunt = "College career punt yards / punts",
  CollFinalCompletionPct = "Final college season completions / attempts",
  CollFinalYardsPerAttempt = "Final college season passing yards / attempts",
  CollFinalYardsPerCarry = "Final college season rushing yards / carries",
  CollFinalYardsPerReception = "Final college season receiving yards / receptions",
  CollFinalPPAPerPlay = "Final college season average PPA per play (CFBD, 2013+)",
  CollFinalPPAPerPass = "Final college season average PPA per pass play (2013+)",
  CollFinalPPAPerRush = "Final college season average PPA per rush play (2013+)",
  CollFinalPPATotal = "Final college season total PPA (2013+)",
  CollFinalUsage = "Final college season share of team plays (CFBD usage, 2013+)",
  CollFinalUsagePass = "Final college season share of team pass plays (2013+)",
  CollFinalUsageRush = "Final college season share of team rush plays (2013+)",
  HasCollegeLink = "1 if the prospect links to at least one CFBD college id with a pre-NFL season (player_college_xwalk) that precedes the draft class",
  CollegePreDraftViolation = "1 if the final CFBD season is not before the draft class; all college variables are then NA",
  CollegeCareerObservable = "1 if every pre-NFL CFBD team-season is box-score complete (FBS 2009+, FCS 2022+) and seen through a non-placeholder id, the player has at least one pre-NFL stat row, and the position has box-score stats (not OL/LS)",
  CollegeFinalObservable = "1 if the final pre-NFL team-season is box-score complete and seen through a non-placeholder id, the player has at least one pre-NFL stat row, and the position is not OL/LS",
  CollegeDefCareerObservable = "CollegeCareerObservable and every pre-NFL season >= 2016 (defensive stats start 2016)",
  CollegeDefFinalObservable = "CollegeFinalObservable and final college season >= 2016",
  FirstCollegeSeason = "First pre-NFL CFBD season (roster or stats; can be early for backfilled rosters)",
  FinalCollegeSeason = "Final pre-NFL CFBD season",
  NCollegeSeasons = "Number of pre-NFL CFBD seasons (roster or stats)",
  NCollegeSeasonsWithStats = "Number of pre-NFL CFBD seasons with player stat rows",
  FinalCollegeTeam = "College team in the final pre-NFL season (most stat rows)",
  FinalCollegeConference = "Conference of the final college team in that season (college_teams)",
  FinalCollegeClassification = "Classification of the final college team in that season (fbs, fcs, ii, iii)",
  FinalCollegePower = "1 if final college team in a Power conference that season (BCS AQ six 2004-13; Power Five 2014-23; Power Four 2024+; Notre Dame always)",
  FinalCollegeHBCU = "1 if final college team in the SWAC or MEAC that season",
  FinalCollegeSPRating = "SP+ rating of the final college team (FBS only)",
  FinalCollegeSRS = "SRS rating of the final college team (FBS + FCS; FBS only in 2020)"
)

RecruitLabels <- c(
  HasRecruit = "1 if the prospect links to a 247 recruit profile (player_xwalk.recruit_id) with recruiting class before the draft class",
  recruit_id = "Primary CFBD recruit id (HS before JUCO)",
  RecruitType = "Recruit type (HighSchool, JUCO)",
  RecruitClass = "Recruiting class year",
  RecruitPosition = "247 recruit position",
  RecruitStars = "247 composite stars",
  RecruitRating = "247 composite rating",
  RecruitNationalRank = "247 national ranking",
  RecruitPosRank = "Rank by rating within recruit class x type x 247 position (computed; ties share best rank)",
  RecruitPosN = "Number of rated recruits in the class x type x position cell",
  RecruitState = "High-school state/province",
  RecruitCountry = "High-school country",
  RecruitCountyFips = "Hometown county FIPS code",
  RecruitHeight = "Recruit height, inches (cleaned)",
  RecruitWeight = "Recruit weight, lb (cleaned)",
  RecruitLinkMethod = "Method of the NFL-to-recruit link",
  RecruitLinkConfidence = "Confidence of the NFL-to-recruit link (high/medium)"
)

OutcomeLabels <- c(
  PfrNoGames = "1 if PFR records no NFL game for the pick (games/w_av/to NA in nfl_draft_picks); drafted only",
  PfrGames = "PFR career NFL games (0 when PfrNoGames; drafted only; censored for recent classes)",
  PfrWeightedAV = "PFR weighted career Approximate Value (w_av; 0 when PfrNoGames; drafted only)",
  PfrDraftTeamAV = "PFR Approximate Value accumulated for the drafting team (dr_av; 0 when PfrNoGames)",
  PfrSeasonsStarted = "PFR seasons as primary starter (drafted only)",
  PfrProBowls = "PFR Pro Bowl selections (drafted only)",
  PfrAllPro = "PFR first-team All-Pro selections (drafted only)",
  PfrHallOfFame = "1 if in the Pro Football Hall of Fame (drafted only)",
  PfrLastSeason = "Last NFL season per PFR (NA if no game)",
  NFLSeasonsObservable = "NFL seasons observable in the DB from the draft class to 2025 (0 for 2026)",
  FirstNFLRosterSeason = "First season on any NFL season roster (nfl_rosters_season 1999-2025, any status)",
  LastNFLRosterSeason = "Last season (>= draft class) on any NFL season roster",
  MadeNFLRoster = "1 if on any NFL season roster from the draft class on (linked prospects, classes <= 2025)",
  MadeNFLRosterUnlinked0 = "MadeNFLRoster with unlinked prospects coded 0 (assumes a rostered player has a gsis_id; classes <= 2025)",
  NFLSeasonsRostered = "Seasons on any NFL season roster from the draft class on (linked, classes <= 2025)",
  NFLSeasonsRosteredFirst3 = "Seasons rostered among the first three (class..class+2; classes <= 2023)",
  NFLSeasonsGameDay = "REG seasons with at least one game-day (active/inactive) roster week (classes 2002-2025)",
  NFLSeasonsPlayed = "REG seasons with at least one game played (box-score row or snap; classes 2002-2025)",
  NFLGamesPlayed = "REG games played (box-score row, or any snap 2013+; pre-2013 OL undercounted; classes 2002-2025)",
  NFLGamesStarted = "REG weeks listed as a depth-chart starter (classes 2002-2025)",
  NFLGamesPlayedFirst3 = "REG games played in the first three seasons (classes 2002-2023)",
  NFLGamesStartedFirst3 = "REG depth-chart starts in the first three seasons (classes 2002-2023)",
  ContractClassCovered = "1 if OTC contract coverage of the class x draft status is >= 80%: drafted = share of linked picks with a Drafted contract (2011+); undrafted = share of linked, NFL-rostered invitees with any typed contract (2016-2020, 2022-2025). All contract variables are NA otherwise",
  RookieContract = "1 if an OTC Drafted or UDFA contract signed in/after the draft year exists",
  SecondContract = "1 if any non-rookie OTC contract (Extension, UFA, RFA, Franchise, Transition, ERFA, Practice, SFA, Other) signed in/after the draft year exists",
  SecondContractYear = "Earliest signing year of a non-rookie contract",
  YearsToSecondContract = "SecondContractYear - DraftClass",
  VeteranContract = "1 if a veteran market contract (Extension, UFA, RFA, Franchise, Transition) exists",
  VetContractYear = "Signing year of the first veteran market contract",
  YearsToVetContract = "VetContractYear - DraftClass",
  VetContractType = "Type of the first veteran market contract",
  VetContractYears = "Length of the first veteran market contract, years",
  VetContractAPY = "APY of the first veteran market contract, $ millions",
  VetContractAPYCapPct = "APY of the first veteran market contract as a share of the salary cap",
  VetContractInflatedAPY = "Inflation-adjusted APY of the first veteran market contract (OTC inflated_apy)",
  VetContractGuaranteed = "Guarantees of the first veteran market contract, $ millions (0 may mean unknown)",
  VetContractValue = "Total value of the first veteran market contract, $ millions"
)

RaceLabels <- c(
  race = "Hand-coded race (load_person_race; NA until coded)",
  hispanic = "Hand-coded Hispanic (yes/no; NA until coded)",
  black_any = "Hand-coded Black alone or in combination (0/1; NA until coded)",
  nonwhite = "Hand-coded nonwhite (0/1; NA until coded)",
  race_source = "Source of the hand code (coder_agree, single_coder, disputed, adjudicated)",
  black_provisional = "Hand-coded black_any when coded, else 1 if a Wikipedia category flags Black, else NA (positive-only lower bound)",
  black_provisional_source = "Source of black_provisional",
  wiki_cat_black = "Wikipedia category flags the player as Black (NA without an article)",
  p_black_bifsg = "BIFSG posterior P(Black) (name-based; secondary)",
  p_white_bifsg = "BIFSG posterior P(White)",
  p_hispanic_bifsg = "BIFSG posterior P(Hispanic)",
  race_bifsg = "BIFSG modal race category"
)

CollegeLabels <- c(CollegeLabels,
  BoxScorePosition = "1 unless the NFL position group (player_xwalk) is OL or LS, which have no college box-score production")

AllLabels <- c(IdentityLabels, DraftLabels, CombineLabels, CfbdLabels, CollegeLabels,
               RecruitLabels, OutcomeLabels, RaceLabels)
AllLabels <- setNames(as.character(AllLabels), names(AllLabels))

# ---------------------------------------------------------------------------
# Write
# ---------------------------------------------------------------------------

write_sample(DraftProspects, "draft_prospects", key = "ProspectId", labels = AllLabels)

# ---------------------------------------------------------------------------
# Validate
# ---------------------------------------------------------------------------

# Drafted rows per class equal the picks in nfl_draft_picks (independent
# count); every combine row is either matched to a pick or an undrafted row
con <- db_connect()
NPicks <- DBI::dbGetQuery(con, glue("
  SELECT season AS DraftClass, COUNT(*) AS NPicksDB FROM nfl_draft_picks
  WHERE season BETWEEN {first_class} AND {last_class} GROUP BY season"))
NCombineDB <- DBI::dbGetQuery(con, glue("
  SELECT COUNT(*) AS n FROM nfl_combine
  WHERE season BETWEEN {first_class} AND {last_class}"))$n
db_disconnect(con)
ClassCounts <- DraftProspects |>
  group_by(DraftClass) |>
  summarise(NDrafted = sum(Drafted), NUndrafted = sum(1L - Drafted),
            NUndraftedUncertain = sum(DraftStatusUncertain),
            NDraftedNoGsis = sum(Drafted == 1L & is.na(gsis_id)),
            .groups = "drop") |>
  left_join(mutate(NPicks, DraftClass = as.integer(DraftClass),
                   NPicksDB = as.integer(NPicksDB)), by = "DraftClass")
stopifnot(all(ClassCounts$NDrafted == ClassCounts$NPicksDB),
          sum(DraftProspects$CombineInvite) == NCombineDB)
message(glue("draft_prospects: key unique; {sum(DraftProspects$Drafted)} drafted rows = ",
             "nfl_draft_picks {first_class}-{last_class}; {sum(DraftProspects$CombineInvite)} ",
             "combine rows all placed ({sum(!is.na(DraftProspects$CombineMatch))} matched to a pick)"))
message("Combine-to-pick match method:")
print(count(DraftProspects, CombineMatch))
message(glue("Suspect gsis_id links dropped: {sum(DraftProspects$GsisLinkSuspect)}"))
print(as.data.frame(filter(DraftProspects, GsisLinkSuspect == 1L) |>
                      select(DraftClass, Pick, ProspectName, SuspectGsisId)), row.names = FALSE)

# Coverage of pre-draft signals by class and by round (undrafted = 'UDFA'):
# share with a combine row, any college link, observable college stats
# (career or final season), a recruit profile, a CFBD pre-draft grade, a
# gsis_id, and race measures
draft_coverage <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(N = n(),
              Gsis = mean(!is.na(gsis_id)),
              Combine = mean(CombineInvite == 1),
              Forty = mean(!is.na(Forty)),
              CollegeLink = mean(HasCollegeLink == 1),
              CollegeStats = mean(coalesce(CollegeCareerObservable == 1 |
                                             CollegeFinalObservable == 1, FALSE)),
              Recruit = mean(HasRecruit == 1),
              PreDraftGrade = mean(!is.na(PreDraftGrade)),
              PreDraftRank = mean(!is.na(PreDraftRank)),
              AgeAtDraft = mean(!is.na(AgeAtDraft)),
              BlackProvisional = mean(coalesce(black_provisional, 0L) == 1),
              BifsgObserved = mean(!is.na(p_black_bifsg)),
              .groups = "drop") |>
    mutate(across(Gsis:BifsgObserved, \(x) round(x, 3)))
}
DraftProspects <- DraftProspects |>
  mutate(RoundLabel = if_else(Drafted == 1L, paste0("R", Round), "UDFA"))
CoverageClass <- draft_coverage(DraftProspects, DraftClass, Drafted)
CoverageRound <- draft_coverage(DraftProspects, RoundLabel)
write_csv(draft_coverage(DraftProspects, DraftClass, RoundLabel),
          file.path(analysis, "coverage_draft_prospects.csv"))
DraftProspects <- select(DraftProspects, -RoundLabel)
message("Contract coverage by class x draft status (ContractClassCovered = share >= 0.8):")
print(as.data.frame(Coverage |> mutate(ContractShare = round(ContractShare, 3)) |>
                      pivot_wider(id_cols = DraftClass, names_from = Drafted,
                                  values_from = c(ContractShare, ContractClassCovered)) |>
                      arrange(DraftClass)), row.names = FALSE)
withr::with_options(list(width = 220), {
  print(as.data.frame(ClassCounts), row.names = FALSE)
  print(as.data.frame(CoverageClass), row.names = FALSE)
  print(as.data.frame(CoverageRound), row.names = FALSE)
})

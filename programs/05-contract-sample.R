# ============================================================================
# 05-contract-sample.R
# Builds analysis/contracts: one row per OverTheCap contract (contract_id;
# every nfl_contracts row). Combines
#   - contract terms at signing (value, APY, guarantees, cap share, years,
#     inflation-adjusted terms), logs and the guarantee share,
#   - the OTC contract type and the market margin it prices,
#   - signing franchise, new-team flag, age and NFL experience at signing,
#   - prior-season (year_signed - 1) and career-to-date NFL usage and
#     production from analysis/player_season (seasons < year_signed only),
#   - a data-driven near-minimum flag,
#   - draft slot and time-invariant pre-NFL signals (college production,
#     recruit profile, combine, CFBD pre-draft grade),
#   - race measures from load_person_race (kept separate, never imputed).
# SampleMain: signed 2011-2026, gsis_id present, year_signed not missing,
# first row of each group of identical-terms rows.
# Timing: OTC gives the signing YEAR only (no signing date). Prior-season
# measures use season year_signed - 1, and career measures use seasons
# strictly before year_signed, so no measure uses the season of signing.
# Helpers: programs/00-player-functions.R. Needs analysis/player_season
# (04-player-season-sample.R).
# Date: 2026-09-26
# ============================================================================

source(file.path(programs, "00-player-functions.R"))

# First non-missing value (NA when none)
first_obs <- function(x) x[!is.na(x)][1]

con <- db_connect()

# ---------------------------------------------------------------------------
# Contract spine: every nfl_contracts row
# ---------------------------------------------------------------------------

# Nested player-level lists (season_history, contract_history) are dropped;
# their contents are in nfl_contract_years / nfl_contract_history
Contracts <- tbl(con, "nfl_contracts") |>
  select(contract_id, otc_id, gsis_id, gsis_link_method, contract_seq,
         identical_rows, player, position, position_group, team, franchise_id,
         franchise_ids, n_teams, history_franchise_id, year_signed,
         year_signed_missing, years, value, apy, guaranteed, apy_cap_pct,
         inflated_value, inflated_apy, inflated_guaranteed, contract_type,
         contract_status, history_match, draft_year, draft_round,
         draft_overall, date_of_birth) |>
  collect() |>
  mutate(across(c(otc_id, contract_seq, identical_rows, n_teams, year_signed,
                  years, draft_year, draft_round, draft_overall), as.integer),
         gsis_id = na_if(gsis_id, ""),
         contract_type = na_if(str_trim(contract_type), ""))

# Identical-terms rows: identical_rows counts rows of the same OTC player with
# the same team, year_signed, years, value, APY and guarantees (repeat
# practice-squad or street free-agent deals within a year, or duplicated
# source rows; the two cannot be told apart). Keep the first (lowest
# contract_seq) in SampleMain.
Contracts <- Contracts |>
  group_by(otc_id, team, year_signed, years, value, apy, guaranteed) |>
  mutate(IdenticalRank = rank(contract_seq, ties.method = "first")) |>
  ungroup() |>
  mutate(IdenticalTerms = as.integer(identical_rows > 1),
         IdenticalDuplicate = as.integer(IdenticalRank > 1))
stopifnot(all(Contracts$IdenticalRank <= Contracts$identical_rows))

# ---------------------------------------------------------------------------
# Contract terms, contract type and market margin
# ---------------------------------------------------------------------------

# OTC records an unknown guarantee as 0 (zero and unknown cannot be told
# apart), so GuaranteedZero flags every zero and GuaranteeShare is kept as
# delivered. apy_cap_pct is the APY as a share (0-1) of the league cap in
# the signing year (OTC). Logs are NA for non-positive values.
# MarketMargin groups OTC contract types by the margin that sets the pay:
#   Drafted                          -> Rookie scale (drafted): slotted by pick
#   UDFA                             -> UDFA entry
#   UFA                              -> Veteran free agent (UFA): open market,
#                                       incl. re-signing with the own team
#                                       before free agency (UFAResign = 1)
#   Extension                        -> Re-sign/Extension: extension of a
#                                       running contract
#   Franchise, Transition, RFA, ERFA -> Tag/Tender: price set by the CBA
#                                       tag/tender rules
#   SFA, Practice, Other             -> Other/SFA/Practice: street free agents
#                                       (mostly in-season minimum deals),
#                                       practice-squad deals, and OTC 'Other'
#                                       (renegotiations and reworked deals)
#   missing type (no OTC history)    -> NA
margin_levels <- c("Rookie scale (drafted)", "UDFA entry", "Veteran free agent (UFA)",
                   "Re-sign/Extension", "Tag/Tender", "Other/SFA/Practice")
Contracts <- Contracts |>
  mutate(ContractType = contract_type,
         MarketMargin = case_when(
           contract_type == "Drafted" ~ margin_levels[1],
           contract_type == "UDFA" ~ margin_levels[2],
           contract_type == "UFA" ~ margin_levels[3],
           contract_type == "Extension" ~ margin_levels[4],
           contract_type %in% c("Franchise", "Transition", "RFA", "ERFA") ~ margin_levels[5],
           contract_type %in% c("SFA", "Practice", "Other") ~ margin_levels[6],
           TRUE ~ NA_character_),
         MarketMargin = factor(MarketMargin, levels = margin_levels),
         VeteranMarket = if_else(is.na(MarketMargin), NA_integer_,
                                 as.integer(MarketMargin %in% margin_levels[3:5])),
         LogAPY = if_else(apy > 0, log(apy), NA_real_),
         LogAPYCapPct = if_else(apy_cap_pct > 0, log(apy_cap_pct), NA_real_),
         LogInflatedAPY = if_else(inflated_apy > 0, log(inflated_apy), NA_real_),
         GuaranteeShare = safe_div(guaranteed, value),
         GuaranteedZero = as.integer(guaranteed == 0),
         GuaranteeShareAbove1 = as.integer(coalesce(GuaranteeShare > 1, FALSE)),
         MultiYear = as.integer(years > 1))

# ---------------------------------------------------------------------------
# Identity: birth date, rookie season, draft slot
# ---------------------------------------------------------------------------

# Season-level NFL panel built by 04 (REG seasons 2002-2025)
PlayerSeason <- arrow::read_parquet(file.path(analysis, "player_season.parquet"))

# Birth date: nfl_players, else the weekly rosters (player_season), else OTC.
# Draft slot: nfl_players, else nfl_draft_picks, else OTC's own draft fields
# (which cover many pre-2010 draftees missing in nflverse).
Players <- tbl(con, "nfl_players") |>
  select(gsis_id, birth_date, rookie_season, draft_year, draft_round,
         draft_pick, position_group) |>
  collect() |>
  mutate(birth_date = as.Date(birth_date))
DraftPicks <- tbl(con, "nfl_draft_picks") |>
  filter(!is.na(gsis_id)) |>
  select(gsis_id, PickSeason = season, PickRound = round, PickOverall = pick) |>
  collect() |>
  arrange(gsis_id, PickSeason) |>
  distinct(gsis_id, .keep_all = TRUE)
PanelIdentity <- PlayerSeason |>
  group_by(gsis_id) |>
  summarise(PanelBirthDate = first_obs(birth_date),
            PanelRookieSeason = first_obs(RookieSeason), .groups = "drop")
OtcIdentity <- Contracts |>
  filter(!is.na(gsis_id)) |>
  # OTC draft_year has a few malformed values (e.g. 2, 206) and is also
  # filled for undrafted players (entry year): kept only with a draft round.
  # OTC uses December 31, 1899 as a missing birth date.
  mutate(OtcBirthDate = suppressWarnings(mdy(date_of_birth)),
         OtcBirthDate = if_else(year(OtcBirthDate) < 1930, as.Date(NA), OtcBirthDate),
         draft_year = if_else(draft_year >= 1936 & !is.na(draft_round), draft_year,
                              NA_integer_)) |>
  arrange(gsis_id, desc(year_signed)) |>
  group_by(gsis_id) |>
  summarise(OtcBirthDate = first_obs(OtcBirthDate),
            OtcDraftYear = first_obs(draft_year),
            OtcDraftRound = first_obs(draft_round),
            OtcDraftOverall = first_obs(draft_overall), .groups = "drop")

Identity <- distinct(filter(Contracts, !is.na(gsis_id)), gsis_id) |>
  left_join(Players, by = "gsis_id") |>
  left_join(DraftPicks, by = "gsis_id") |>
  left_join(PanelIdentity, by = "gsis_id") |>
  left_join(OtcIdentity, by = "gsis_id") |>
  transmute(gsis_id,
            InNflPlayers = as.integer(gsis_id %in% Players$gsis_id),
            BirthDate = coalesce(birth_date, as.Date(PanelBirthDate), OtcBirthDate),
            RookieSeason = as.integer(coalesce(rookie_season, PanelRookieSeason)),
            DraftYear = as.integer(coalesce(draft_year, PickSeason, OtcDraftYear)),
            DraftRound = as.integer(coalesce(draft_round, PickRound, OtcDraftRound)),
            DraftPick = as.integer(coalesce(draft_pick, PickOverall, OtcDraftOverall)),
            DraftSource = case_when(!is.na(draft_round) ~ "nfl_players",
                                    !is.na(PickRound) ~ "nfl_draft_picks",
                                    !is.na(OtcDraftRound) ~ "otc",
                                    TRUE ~ NA_character_),
            Undrafted = as.integer(is.na(DraftRound)),
            NflversePositionGroup = position_group)

# ---------------------------------------------------------------------------
# Signing franchise
# ---------------------------------------------------------------------------

# Single-team contracts: franchise_id. Multi-team strings ('NYJ/GB'; the
# contract moved with a trade) list the teams in no consistent order, so the
# signing franchise is the listed franchise the player was with in
# year_signed - 1 (primary franchise), else in year_signed, else OTC's
# matched history team.
PanelFranchise <- PlayerSeason |>
  select(gsis_id, season, PrimaryFranchise)
MultiTeam <- Contracts |>
  filter(coalesce(n_teams, 1L) > 1) |>
  select(contract_id, gsis_id, year_signed, franchise_ids, history_franchise_id) |>
  mutate(ListedFranchises = str_split(franchise_ids, "/")) |>
  left_join(rename(PanelFranchise, SignSeasonFr = PrimaryFranchise),
            by = join_by(gsis_id, year_signed == season), relationship = "many-to-one") |>
  mutate(year_prev = year_signed - 1L) |>
  left_join(rename(PanelFranchise, PriorFr = PrimaryFranchise),
            by = join_by(gsis_id, year_prev == season), relationship = "many-to-one") |>
  mutate(InPrior = map2_lgl(PriorFr, ListedFranchises, \(f, l) !is.na(f) && f %in% l),
         InSigning = map2_lgl(SignSeasonFr, ListedFranchises, \(f, l) !is.na(f) && f %in% l),
         MultiFranchise = case_when(InPrior ~ PriorFr,
                                    InSigning ~ SignSeasonFr,
                                    TRUE ~ history_franchise_id),
         MultiMethod = case_when(InPrior ~ "multi_team_prior_season",
                                 InSigning ~ "multi_team_signing_season",
                                 !is.na(history_franchise_id) ~ "multi_team_otc_history",
                                 TRUE ~ NA_character_)) |>
  select(contract_id, MultiFranchise, MultiMethod)

Contracts <- Contracts |>
  left_join(MultiTeam, by = "contract_id") |>
  mutate(MultiTeamContract = as.integer(coalesce(n_teams, 1L) > 1),
         SigningFranchise = coalesce(franchise_id, MultiFranchise),
         SigningFranchiseMethod = case_when(!is.na(franchise_id) ~ "single_team",
                                            TRUE ~ MultiMethod)) |>
  select(-MultiFranchise, -MultiMethod)

# ---------------------------------------------------------------------------
# Prior-season (year_signed - 1) NFL usage, production and pay
# ---------------------------------------------------------------------------

# Contracts that can be placed on the NFL panel
ContractKeys <- Contracts |>
  filter(!is.na(gsis_id), !coalesce(year_signed_missing, FALSE), year_signed > 0) |>
  select(contract_id, gsis_id, year_signed)

# Measures taken from player_season for season year_signed - 1 (prefix
# Prior). Box-score counts are 0 for a rostered season without a stats row;
# snaps exist from 2013, PFR advanced defense from 2018, NGS from 2016.
prior_vars <- c("PositionGroup", "PrimaryFranchise", "NFranchises", "WeeksOnRoster",
                "WeeksGameDay", "WeeksActive", "WeeksPracticeSquad", "WeeksReserve",
                "GamesPlayed", "GamesStartedDepth", "GamesStartedSnaps", "OffSnaps",
                "DefSnaps", "STSnaps", "OffSnapPctMean", "DefSnapPctMean",
                "STSnapPctMean", "InjuryWeeks", "HasStatRow", "FantasyPointsPPR",
                "PassAtt", "PassYds", "PassTD", "PassInt", "PassEPA", "RushAtt",
                "RushYds", "RushTD", "RushEPA", "Targets", "Rec", "RecYds", "RecTD",
                "RecEPA", "Tackles", "SoloTackles", "TFL", "Sacks", "QBHits",
                "PassesDefended", "DefInt", "ForcedFumbles", "FGMade", "FGAtt",
                "Punts", "PuntNetYds", "PfrPressures", "PfrMissedTacklePct",
                "PfrTargetsAllowed", "PfrYardsPerTargetAllowed", "NgsCPOE",
                "CapNumber", "CashPaid", "CapPercent")
Prior <- ContractKeys |>
  mutate(season = year_signed - 1L) |>
  inner_join(select(PlayerSeason, gsis_id, season, all_of(prior_vars)),
             by = c("gsis_id", "season")) |>
  select(contract_id, all_of(prior_vars)) |>
  rename_with(\(x) paste0("Prior", x), all_of(prior_vars)) |>
  mutate(InPanelPriorSeason = 1L)

# End-of-season franchise in year_signed - 1: franchise of the player's
# latest REG weekly-roster row (any status). A player traded or claimed
# during t-1 re-signs with his end-of-season team, which the primary
# franchise (most game-day weeks) can miss. Ties within the latest week
# (rare) go to a non-CUT row, then to the franchise id.
EndFranchise <- tbl(con, "nfl_rosters_weekly") |>
  filter(season_type == "REG", is_key_primary, !is.na(gsis_id), gsis_id != "") |>
  select(gsis_id, season, week, franchise_id, status) |>
  collect() |>
  mutate(season = as.integer(season), week = as.integer(week)) |>
  arrange(gsis_id, season, desc(week), status == "CUT", franchise_id) |>
  distinct(gsis_id, season, .keep_all = TRUE) |>
  select(gsis_id, season, PriorEndFranchise = franchise_id)
NPrior <- nrow(Prior)
Prior <- Prior |>
  left_join(select(ContractKeys, contract_id, gsis_id, year_signed), by = "contract_id") |>
  mutate(season = year_signed - 1L) |>
  left_join(EndFranchise, by = c("gsis_id", "season"), relationship = "many-to-one") |>
  select(-gsis_id, -year_signed, -season)
stopifnot(nrow(Prior) == NPrior, !anyDuplicated(Prior$contract_id))

# ---------------------------------------------------------------------------
# Career to date (all panel seasons < year_signed) and experience
# ---------------------------------------------------------------------------

# A season counts toward ExperienceAtSigning when the player played at least
# one REG game (GamesPlayed >= 1). Before 2013 GamesPlayed misses players
# without box-score stats (e.g. OL), so a pre-2013 season also counts when
# the player has at least one ACT (dressed) roster week. The panel starts in
# 2002: experience is left-censored for players whose rookie season is
# earlier (ExperienceLeftCensored).
Career <- ContractKeys |>
  inner_join(select(PlayerSeason, gsis_id, season, PrimaryFranchise, GamesPlayed,
                    WeeksActive, WeeksGameDay, GamesStartedDepth, OffSnaps, DefSnaps,
                    STSnaps, InjuryWeeks, FantasyPointsPPR, PassYds, PassTD, PassInt,
                    RushYds, RushTD, RecYds, RecTD, Tackles, Sacks, PassesDefended,
                    DefInt, FGMade, FGAtt, CashPaid),
             by = "gsis_id", relationship = "many-to-many") |>
  filter(season < year_signed) |>
  arrange(contract_id, season) |>
  group_by(contract_id) |>
  summarise(CareerPanelSeasons = n(),
            ExperienceAtSigning = sum(coalesce(GamesPlayed, 0) >= 1 |
                                        (season < 2013 & coalesce(WeeksActive, 0) >= 1)),
            ExperienceGameDay = sum(coalesce(WeeksGameDay, 0) >= 1),
            LastPanelSeason = last(season),
            LastPanelFranchise = last(PrimaryFranchise),
            CareerGames = sum_or_na(GamesPlayed),
            CareerStartsDepth = sum_or_na(GamesStartedDepth),
            CareerSnapSeasons = sum(season >= 2013),
            CareerOffDefSnaps = sum_or_na(if_else(season >= 2013, OffSnaps + DefSnaps, NA_real_)),
            CareerSTSnaps = sum_or_na(if_else(season >= 2013, STSnaps, NA_real_)),
            CareerInjuryWeeks = sum_or_na(InjuryWeeks),
            across(c(FantasyPointsPPR, PassYds, PassTD, PassInt, RushYds, RushTD,
                     RecYds, RecTD, Tackles, Sacks, PassesDefended, DefInt, FGMade,
                     FGAtt, CashPaid),
                   sum_or_na, .names = "Career{.col}"),
            .groups = "drop")

# ---------------------------------------------------------------------------
# Assemble: identity, timing, experience, new team, sample flag
# ---------------------------------------------------------------------------

# Age at signing: on September 1 of year_signed (OTC gives no signing date).
# NewTeam: signing franchise differs from the player's END-OF-SEASON
# franchise in year_signed - 1 (NA when he is not on a 2002-2025 REG roster
# that season or the signing franchise is unknown). NewTeamPrimary: the same
# against his primary franchise (most game-day weeks) in year_signed - 1.
# NewTeamLastSeason: against the primary franchise of his most recent panel
# season before year_signed.
# Experience bins (0, 1, 2, 3, 4-6, 7+ seasons) follow the steps of the CBA
# minimum-salary schedule by credited seasons; ExperienceAtSigning is a
# data-driven proxy for credited seasons, not the CBA definition.
ContractSample <- Contracts |>
  left_join(Identity, by = "gsis_id") |>
  left_join(Prior, by = "contract_id") |>
  left_join(Career, by = "contract_id") |>
  mutate(InPanelPriorSeason = coalesce(InPanelPriorSeason, 0L),
         CareerPanelSeasons = coalesce(CareerPanelSeasons, 0L),
         ExperienceAtSigning = if_else(is.na(gsis_id) | coalesce(year_signed_missing, FALSE),
                                       NA_integer_, coalesce(ExperienceAtSigning, 0L)),
         ExperienceGameDay = if_else(is.na(ExperienceAtSigning), NA_integer_,
                                     coalesce(ExperienceGameDay, 0L)),
         ExperienceLeftCensored = as.integer(coalesce(RookieSeason < 2002, FALSE)),
         YearsSinceRookie = year_signed - RookieSeason,
         ExperienceBin = cut(ExperienceAtSigning, breaks = c(-Inf, 0, 1, 2, 3, 6, Inf),
                             labels = c("0", "1", "2", "3", "4-6", "7+")),
         AgeAtSigning = as.numeric(as.Date(paste0(year_signed, "-09-01")) - BirthDate) / 365.25,
         AgeAtSigning = if_else(coalesce(year_signed_missing, TRUE), NA_real_, AgeAtSigning),
         NewTeam = case_when(is.na(SigningFranchise) | is.na(PriorEndFranchise) ~ NA_integer_,
                             SigningFranchise != PriorEndFranchise ~ 1L,
                             TRUE ~ 0L),
         NewTeamPrimary = case_when(is.na(SigningFranchise) | is.na(PriorPrimaryFranchise) ~ NA_integer_,
                                    SigningFranchise != PriorPrimaryFranchise ~ 1L,
                                    TRUE ~ 0L),
         NewTeamLastSeason = case_when(is.na(SigningFranchise) | is.na(LastPanelFranchise) ~ NA_integer_,
                                       SigningFranchise != LastPanelFranchise ~ 1L,
                                       TRUE ~ 0L),
         UFAResign = if_else(coalesce(ContractType == "UFA", FALSE), 1L - NewTeam,
                             NA_integer_),
         YearsSincePanelSeason = year_signed - LastPanelSeason,
         CareerSnapSeasons = if_else(is.na(ExperienceAtSigning), NA_integer_,
                                     coalesce(CareerSnapSeasons, 0L)),
         # OTC links some contracts to a namesake's gsis_id (e.g. a 1994 'Rod
         # Smith' deal linked to a player born in 1992). Flag deals signed
         # before the player's NFL draft year or before age 20.
         # (NA when year_signed is missing: the rule cannot be evaluated).
         GsisLinkSuspect = if_else(is.na(gsis_id) | coalesce(year_signed_missing, FALSE),
                                   NA_integer_,
                                   as.integer(coalesce(year_signed < DraftYear, FALSE) |
                                                coalesce(AgeAtSigning < 20, FALSE))),
         # Entry-type deals signed away from the entry year (OTC type likely
         # mislabelled or linked to a namesake): Drafted outside the draft
         # year, UDFA by a drafted player or outside the rookie season
         EntryTypeMismatch = case_when(
           !coalesce(ContractType %in% c("Drafted", "UDFA"), FALSE) ~ NA_integer_,
           ContractType == "Drafted" ~ as.integer(coalesce(year_signed != DraftYear, TRUE)),
           TRUE ~ as.integer(Undrafted == 0L | coalesce(year_signed != RookieSeason, FALSE))),
         SampleMain = as.integer(!is.na(gsis_id) & !coalesce(year_signed_missing, FALSE) &
                                   between(year_signed, 2011L, 2026L) &
                                   IdenticalDuplicate == 0L))

# ---------------------------------------------------------------------------
# Data-driven near-minimum flag (no CBA salary numbers are hard-coded)
# ---------------------------------------------------------------------------

# Reference cells: SampleMain one-year contracts with APY > 0 and a known
# OTC type other than Practice, RFA, Franchise and Transition (practice pay
# is below the minimum; RFA tenders and tags are one-year deals at CBA
# tender amounts that form their own mass points, e.g. $1.84M in 2011), by
# year_signed x ExperienceBin. Minimum-salary
# deals pile up at the CBA minimum of the cell, so RefMinAPY is the MODAL
# APY value (rounded to $1,000) of the cell, required to hold at least 10%
# (and at least 3) of the cell's contracts. The mode, not the lowest mass
# point, is used because ExperienceAtSigning only proxies credited seasons:
# every cell also has a secondary mass point at the next-lower bin's
# minimum (e.g. 2021, bin 1: 55% at $0.78M, 32% at $0.66M), and the lowest
# mass point would leave most true minimum deals of the cell unflagged.
# Within a year the schedule is forced to be non-decreasing in experience
# (running maximum), and a cell without a qualifying mode takes the value
# of the next-lower experience bin (RefMinFilled = 1).
# NearMinimum = APY within 10% of RefMinAPY (APY <= 1.10 x RefMinAPY), which
# includes deals below it (practice-squad deals and deals at a lower bin's
# minimum are flagged 1).
near_min_band <- 1.10
RefMin <- ContractSample |>
  filter(SampleMain == 1, years == 1, apy > 0, !is.na(ContractType),
         !ContractType %in% c("Practice", "RFA", "Franchise", "Transition"),
         !is.na(ExperienceBin)) |>
  mutate(ApyRounded = round(apy, 3)) |>
  count(year_signed, ExperienceBin, ApyRounded) |>
  group_by(year_signed, ExperienceBin) |>
  mutate(CellN = sum(n)) |>
  filter(n / CellN >= 0.10, n >= 3) |>
  arrange(desc(n), ApyRounded, .by_group = TRUE) |>
  summarise(RefMinAPY = first(ApyRounded), RefMinCellN = first(CellN), .groups = "drop") |>
  complete(year_signed, ExperienceBin) |>
  arrange(year_signed, ExperienceBin) |>
  group_by(year_signed) |>
  mutate(RefMinFilled = as.integer(is.na(RefMinAPY))) |>
  fill(RefMinAPY, .direction = "down") |>
  mutate(RefMinAPY = if_else(is.na(RefMinAPY), NA_real_,
                             cummax(coalesce(RefMinAPY, -Inf)))) |>
  ungroup() |>
  select(year_signed, ExperienceBin, RefMinAPY, RefMinFilled)
print(RefMin |> select(-RefMinFilled) |>
        pivot_wider(names_from = ExperienceBin, values_from = RefMinAPY), n = 30)

ContractSample <- ContractSample |>
  left_join(RefMin, by = c("year_signed", "ExperienceBin")) |>
  mutate(ApyToRefMin = safe_div(apy, RefMinAPY),
         NearMinimum = case_when(is.na(ApyToRefMin) | is.na(apy) ~ NA_integer_,
                                 apy <= near_min_band * RefMinAPY ~ 1L,
                                 TRUE ~ 0L))

# ---------------------------------------------------------------------------
# Pre-NFL signals (time-invariant) and race measures
# ---------------------------------------------------------------------------

College <- college_production(con)
Recruit <- recruit_signals(con)
Combine <- combine_signals(con)
PreDraft <- predraft_signals(con)

# Three race measures kept separate and never imputed: hand-coded (NA until
# coding), black_provisional (positive-only Wikipedia flag) and BIFSG.
# Only gsis_ids in nfl_players have race rows.
PersonRace <- load_person_race(con, hand_coded) |>
  filter(entity == "player") |>
  select(gsis_id = person_id, race, hispanic, black_any, nonwhite, race_source,
         black_provisional, black_provisional_source, wiki_cat_black,
         p_black_bifsg, p_white_bifsg, p_hispanic_bifsg, race_bifsg)

db_disconnect(con)

ContractSample <- ContractSample |>
  left_join(College |> mutate(HasCollegeLink = 1L), by = "gsis_id") |>
  left_join(Recruit, by = "gsis_id") |>
  left_join(Combine, by = "gsis_id") |>
  left_join(PreDraft, by = "gsis_id") |>
  left_join(PersonRace, by = "gsis_id") |>
  mutate(HasCollegeLink = coalesce(HasCollegeLink, 0L),
         CollegeCareerObservable = coalesce(CollegeCareerObservable, 0L),
         CollegeFinalObservable = coalesce(CollegeFinalObservable, 0L),
         CollegeDefCareerObservable = coalesce(CollegeDefCareerObservable, 0L),
         CollegeDefFinalObservable = coalesce(CollegeDefFinalObservable, 0L),
         HasRecruit = as.integer(!is.na(recruit_id)),
         CombineInvite = coalesce(CombineInvite, 0L)) |>
  select(-any_of("BoxScorePosition"))

# ---------------------------------------------------------------------------
# Column order
# ---------------------------------------------------------------------------

ContractSample <- ContractSample |>
  select(contract_id, SampleMain, otc_id, gsis_id, gsis_link_method, InNflPlayers,
         GsisLinkSuspect, EntryTypeMismatch, player, contract_seq, identical_rows, IdenticalTerms, IdenticalRank,
         IdenticalDuplicate,
         # contract terms
         year_signed, year_signed_missing, years, value, apy, guaranteed,
         apy_cap_pct, inflated_value, inflated_apy, inflated_guaranteed, LogAPY,
         LogAPYCapPct, LogInflatedAPY, GuaranteeShare, GuaranteedZero,
         GuaranteeShareAbove1, MultiYear, RefMinAPY, RefMinFilled, ApyToRefMin,
         NearMinimum,
         # type, margin, team
         contract_type, ContractType, MarketMargin, VeteranMarket, UFAResign,
         contract_status, history_match, team, franchise_id, franchise_ids, n_teams,
         history_franchise_id, MultiTeamContract, SigningFranchise,
         SigningFranchiseMethod, NewTeam, NewTeamPrimary, NewTeamLastSeason,
         # position and identity
         position, position_group, NflversePositionGroup, PriorPositionGroup,
         BirthDate, AgeAtSigning, RookieSeason, YearsSinceRookie,
         ExperienceAtSigning, ExperienceGameDay, ExperienceBin,
         ExperienceLeftCensored, DraftYear, DraftRound, DraftPick, DraftSource,
         Undrafted, draft_year, draft_round, draft_overall,
         # NFL usage and production before signing
         InPanelPriorSeason, starts_with("Prior") & !PriorPositionGroup,
         CareerPanelSeasons, LastPanelSeason, LastPanelFranchise,
         YearsSincePanelSeason, starts_with("Career"),
         # pre-NFL signals
         HasCollegeLink, CollegeCareerObservable, CollegeFinalObservable,
         CollegeDefCareerObservable, CollegeDefFinalObservable,
         FirstCollegeSeason, FinalCollegeSeason, NCollegeSeasons,
         NCollegeSeasonsWithStats, FinalCollegeTeam, FinalCollegeConference,
         FinalCollegeClassification, FinalCollegePower, FinalCollegeHBCU,
         FinalCollegeSPRating, FinalCollegeSRS, starts_with("Coll"),
         HasRecruit, recruit_id, starts_with("Recruit"),
         CombineInvite, CombineYear, CombineHeight, CombineWeight, Forty,
         Vertical, Bench, BroadJump, Cone, Shuttle, CombineLinkMethod,
         PreDraftRank, PreDraftPosRank, PreDraftGrade, CfbdDraftConference,
         # race measures
         race, hispanic, black_any, nonwhite, race_source, black_provisional,
         black_provisional_source, wiki_cat_black, p_black_bifsg, p_white_bifsg,
         p_hispanic_bifsg, race_bifsg) |>
  mutate(MarketMargin = as.character(MarketMargin),
         ExperienceBin = as.character(ExperienceBin)) |>
  arrange(year_signed, otc_id, contract_seq)

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

ContractLabels <- c(
  contract_id = "OTC contract id (key) = otc_id-year_signed-contract_seq; one row per nfl_contracts row",
  SampleMain = "1 if signed 2011-2026, gsis_id present, year_signed not missing, and not a repeat identical-terms row (IdenticalDuplicate == 0)",
  otc_id = "OverTheCap player id",
  gsis_id = "NFL GSIS player id (OTC's, else nfl_players.otc_id; NA if unlinked)",
  gsis_link_method = "How gsis_id was attached (nfl_contracts)",
  GsisLinkSuspect = "1 if the contract predates the linked player's NFL draft year or his age at signing is below 20 (likely OTC link to a namesake; kept in SampleMain); NA if no gsis_id or year_signed missing",
  EntryTypeMismatch = "For Drafted/UDFA contracts: 1 if a Drafted deal is not signed in DraftYear, or a UDFA deal is for a drafted player or not signed in RookieSeason (mislabelled type or namesake link); NA for other types",
  InNflPlayers = "1 if gsis_id is in nfl_players (only those have race measures and pre-NFL links)",
  player = "Player name (OTC)",
  contract_seq = "Sequence of the contract among the player's contracts signed in year_signed (ordered by terms)",
  identical_rows = "Number of rows of the same OTC player with the same team, year_signed, years, value, APY and guarantees",
  IdenticalTerms = "1 if identical_rows > 1 (repeat practice-squad/street FA deals or duplicated source rows; indistinguishable)",
  IdenticalRank = "Rank of the row (by contract_seq) within its identical-terms group",
  IdenticalDuplicate = "1 if IdenticalRank > 1 (excluded from SampleMain; the first row is kept)",
  year_signed = "Year signed (OTC; 0 = missing; OTC has no signing date)",
  year_signed_missing = "TRUE if year_signed is 0 in the source",
  years = "Contract length in years (OTC)",
  value = "Total contract value, $ millions (OTC)",
  apy = "Average per year (value / years), $ millions (OTC)",
  guaranteed = "Guaranteed money, $ millions (OTC; unknown guarantees are recorded as 0)",
  apy_cap_pct = "APY as a share (0-1) of the league salary cap in the signing year (OTC)",
  inflated_value = "Total value adjusted for salary-cap growth (source-provided), $ millions",
  inflated_apy = "APY adjusted for salary-cap growth (source-provided), $ millions",
  inflated_guaranteed = "Guarantees adjusted for salary-cap growth (source-provided), $ millions",
  LogAPY = "log(apy) (NA if apy <= 0)",
  LogAPYCapPct = "log(apy_cap_pct) (NA if <= 0)",
  LogInflatedAPY = "log(inflated_apy) (NA if <= 0)",
  GuaranteeShare = "guaranteed / value (NA when value is 0); 0 may mean unknown guarantees (see GuaranteedZero)",
  GuaranteedZero = "1 if guaranteed == 0 (OTC does not separate zero from unknown guarantees)",
  GuaranteeShareAbove1 = "1 if GuaranteeShare > 1 (source inconsistency; left as delivered)",
  MultiYear = "1 if years > 1",
  RefMinAPY = "Data-driven minimum APY of the year_signed x ExperienceBin cell, $ millions: modal APY value (rounded to $1,000; must hold >= 10% and >= 3) of the cell's SampleMain one-year, APY > 0 contracts of known type other than Practice/RFA/Franchise/Transition; running maximum across bins within a year; from the next-lower bin when none (2011-2026 only)",
  RefMinFilled = "1 if RefMinAPY was taken from the next-lower experience bin",
  ApyToRefMin = "apy / RefMinAPY",
  NearMinimum = "1 if apy <= 1.10 x RefMinAPY (includes below-minimum practice-squad deals); NA without a reference cell",
  contract_type = "OTC contract type as delivered (from the matched contract_history entry)",
  ContractType = "OTC contract type: Drafted, UDFA, UFA, RFA, ERFA, Extension, Franchise, Transition, Practice, SFA, Other (NA if unmatched)",
  MarketMargin = "Margin that prices the deal: Rookie scale (drafted) = Drafted; UDFA entry = UDFA; Veteran free agent (UFA) = UFA; Re-sign/Extension = Extension; Tag/Tender = Franchise, Transition, RFA, ERFA; Other/SFA/Practice = SFA, Practice, Other; NA if no type",
  VeteranMarket = "1 if MarketMargin is Veteran free agent (UFA), Re-sign/Extension or Tag/Tender (NA if MarketMargin is NA)",
  UFAResign = "For UFA contracts: 1 - NewTeam (1 = re-signed with the end-of-season team of year_signed - 1; NA otherwise, incl. missing type)",
  contract_status = "OTC contract status (Expired, Terminated, Active, Extended, ...); EX POST (as of the OTC scrape), not a pre-signing variable",
  history_match = "How the contract was matched to OTC contract_history (terms_team, terms_only; NA unmatched)",
  team = "OTC team string (nickname, or '/'-joined codes for contracts that moved with a trade)",
  franchise_id = "Franchise of a single-team contract (NA for multi-team strings)",
  franchise_ids = "'/'-joined franchises of a multi-team string",
  n_teams = "Number of teams in the OTC team string",
  history_franchise_id = "Franchise of the matched OTC contract_history entry",
  MultiTeamContract = "1 if the OTC team string lists more than one team",
  SigningFranchise = "Signing franchise: franchise_id; for multi-team strings the listed franchise the player was with in year_signed - 1, else in year_signed, else history_franchise_id",
  SigningFranchiseMethod = "Rule that set SigningFranchise",
  NewTeam = "1 if SigningFranchise differs from the player's end-of-season franchise (latest REG roster week) in year_signed - 1 (NA if not on a REG roster that season)",
  NewTeamPrimary = "1 if SigningFranchise differs from the player's primary franchise (most game-day weeks) in year_signed - 1 (NA if not on a REG roster that season)",
  PriorEndFranchise = "Franchise of the player's latest REG weekly-roster row in year_signed - 1 (end-of-season team)",
  NewTeamLastSeason = "1 if SigningFranchise differs from the primary franchise in the player's latest panel season before year_signed",
  position = "OTC market position (QB, WR, CB, IDL, ED, LT, ...)",
  position_group = "Position group of the OTC position (nfl_contracts)",
  NflversePositionGroup = "Position group in nfl_players",
  PriorPositionGroup = "Modal roster position group in year_signed - 1 (player_season)",
  BirthDate = "Birth date (nfl_players, else weekly rosters, else OTC; OTC's 1899-12-31 placeholder set to NA)",
  AgeAtSigning = "Age in years on September 1 of year_signed (OTC has no signing date)",
  RookieSeason = "First NFL season (nfl_players, else weekly rosters)",
  YearsSinceRookie = "year_signed - RookieSeason",
  ExperienceAtSigning = "NFL seasons before year_signed with >= 1 REG game played (before 2013 also seasons with >= 1 ACT roster week); 2002-2025 panel seasons only",
  ExperienceGameDay = "NFL seasons before year_signed with >= 1 game-day (Active/Inactive) roster week",
  ExperienceBin = "ExperienceAtSigning bin: 0, 1, 2, 3, 4-6, 7+ (steps of the CBA minimum schedule; proxy for credited seasons)",
  ExperienceLeftCensored = "1 if RookieSeason < 2002 (seasons before the 2002 panel start are not counted)",
  DraftYear = "NFL draft year (nfl_players, else nfl_draft_picks, else OTC); NA if undrafted",
  DraftRound = "NFL draft round (same sources); NA if undrafted",
  DraftPick = "Overall NFL draft pick (same sources); NA if undrafted",
  DraftSource = "Source of the draft slot (nfl_players, nfl_draft_picks, otc)",
  Undrafted = "1 if no draft round in any source",
  draft_year = "OTC draft year as delivered (a few malformed values)",
  draft_round = "OTC draft round as delivered",
  draft_overall = "OTC overall pick as delivered",
  InPanelPriorSeason = "1 if the player is on a REG weekly roster in year_signed - 1 (Prior* measures observed)",
  CareerPanelSeasons = "Number of 2002-2025 REG roster seasons before year_signed",
  LastPanelSeason = "Latest REG roster season before year_signed",
  LastPanelFranchise = "Primary franchise in LastPanelSeason",
  YearsSincePanelSeason = "year_signed - LastPanelSeason",
  CareerGames = "Sum of REG games played over seasons < year_signed (pre-2013 OL undercounted)",
  CareerStartsDepth = "Sum of depth-chart starts over seasons < year_signed",
  CareerSnapSeasons = "Number of panel seasons < year_signed from 2013 on (seasons with snap data)",
  CareerOffDefSnaps = "Offensive + defensive snaps over 2013+ seasons < year_signed (NA if none)",
  CareerSTSnaps = "Special-teams snaps over 2013+ seasons < year_signed (NA if none)",
  CareerInjuryWeeks = "Injury weeks (Out/Doubtful or RES/PUP) over seasons < year_signed"
)

# Prior-season, career and pre-NFL/race labels reuse the player_season
# codebook (04) so the definitions stay identical
PanelLabels <- read_csv(file.path(analysis, "codebook_player_season.csv"),
                        show_col_types = FALSE) |>
  filter(!is.na(label)) |>
  select(variable, label) |>
  deframe()
PriorLabels <- setNames(paste0("Season year_signed - 1 (player_season): ",
                               PanelLabels[prior_vars[prior_vars != "PositionGroup"]]),
                        paste0("Prior", prior_vars[prior_vars != "PositionGroup"]))
career_sum_vars <- c("FantasyPointsPPR", "PassYds", "PassTD", "PassInt", "RushYds",
                     "RushTD", "RecYds", "RecTD", "Tackles", "Sacks",
                     "PassesDefended", "DefInt", "FGMade", "FGAtt", "CashPaid")
CareerLabels <- setNames(paste0("Sum over panel seasons < year_signed (NA if none) of: ",
                                PanelLabels[career_sum_vars]),
                         paste0("Career", career_sum_vars))
SharedVars <- setdiff(names(ContractSample),
                      c(names(ContractLabels), names(PriorLabels), names(CareerLabels)))
AllLabels <- c(ContractLabels, PriorLabels, CareerLabels,
               PanelLabels[intersect(SharedVars, names(PanelLabels))])
AllLabels <- setNames(as.character(AllLabels), names(AllLabels))

# ---------------------------------------------------------------------------
# Write and validate
# ---------------------------------------------------------------------------

write_sample(ContractSample, "contracts", key = "contract_id", labels = AllLabels)

# One row per nfl_contracts row, key unique
con <- db_connect()
NContracts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n, COUNT(DISTINCT contract_id) AS k FROM nfl_contracts")
db_disconnect(con)
stopifnot(nrow(ContractSample) == NContracts$n, NContracts$n == NContracts$k,
          !anyDuplicated(ContractSample$contract_id))
message(glue("contracts: key unique; {nrow(ContractSample)} rows = nfl_contracts rows; ",
             "SampleMain = {sum(ContractSample$SampleMain)}"))

Main <- filter(ContractSample, SampleMain == 1)

# Exclusions from SampleMain
print(ContractSample |>
        summarise(Total = n(), NoGsis = sum(is.na(gsis_id)),
                  YearMissing = sum(coalesce(year_signed_missing, FALSE)),
                  Outside2011_2026 = sum(!coalesce(year_signed_missing, FALSE) &
                                           !between(year_signed, 2011L, 2026L)),
                  IdenticalDuplicate = sum(IdenticalDuplicate),
                  SampleMain = sum(SampleMain)))

# SampleMain counts by year_signed and MarketMargin
withr::with_options(list(width = 200), {
  print(as.data.frame(Main |>
                        count(year_signed, MarketMargin) |>
                        pivot_wider(names_from = MarketMargin, values_from = n,
                                    values_fill = 0)), row.names = FALSE)
})

# Coverage of veteran-market contracts (UFA, Re-sign/Extension, Tag/Tender)
# by position group: prior-season snaps (2014+ signings, i.e. 2013+ seasons),
# prior-season production row, college stats, recruit profile, combine and
# the provisional race flag
VeteranCoverage <- Main |>
  filter(VeteranMarket == 1) |>
  group_by(position_group) |>
  summarise(N = n(),
            PriorSeason = mean(InPanelPriorSeason == 1),
            PriorSnaps2014 = mean(coalesce(PriorOffSnaps + PriorDefSnaps + PriorSTSnaps, 0)[year_signed >= 2014] > 0),
            PriorStatRow = mean(coalesce(PriorHasStatRow, 0L) == 1),
            PriorPfrDef2019 = mean(!is.na(PriorPfrMissedTacklePct[year_signed >= 2019])),
            CollegeStats = mean(CollegeCareerObservable == 1 | CollegeFinalObservable == 1),
            Recruit = mean(HasRecruit == 1),
            Combine = mean(CombineInvite == 1),
            Age = mean(!is.na(AgeAtSigning)),
            BlackProvisional = mean(coalesce(black_provisional, 0L) == 1),
            Bifsg = mean(!is.na(p_black_bifsg)),
            .groups = "drop") |>
  mutate(across(PriorSeason:Bifsg, \(x) round(x, 3)))
write_csv(VeteranCoverage, file.path(analysis, "coverage_contracts_veteran.csv"))
withr::with_options(list(width = 200), print(as.data.frame(VeteranCoverage), row.names = FALSE))

# Coverage of all SampleMain contracts by MarketMargin
MarginCoverage <- Main |>
  group_by(MarketMargin) |>
  summarise(N = n(), PriorSeason = mean(InPanelPriorSeason == 1),
            DraftSlot = mean(!is.na(DraftPick)), NearMinimum = mean(NearMinimum == 1, na.rm = TRUE),
            GuaranteedZero = mean(GuaranteedZero == 1, na.rm = TRUE),
            CollegeStats = mean(CollegeCareerObservable == 1 | CollegeFinalObservable == 1),
            Recruit = mean(HasRecruit == 1), Combine = mean(CombineInvite == 1),
            NewTeam = mean(NewTeam == 1, na.rm = TRUE),
            NewTeamPrimary = mean(NewTeamPrimary == 1, na.rm = TRUE),
            EntryTypeMismatch = mean(EntryTypeMismatch == 1, na.rm = TRUE),
            GsisLinkSuspect = mean(GsisLinkSuspect == 1),
            .groups = "drop") |>
  mutate(across(PriorSeason:GsisLinkSuspect, \(x) round(x, 3)))
withr::with_options(list(width = 200), print(as.data.frame(MarginCoverage), row.names = FALSE))

# NewTeam (end-of-season team) against NewTeamPrimary (primary team)
print(count(Main, NewTeam, NewTeamPrimary))

# Descriptive log APY by MarketMargin (descriptive only; no controls)
withr::with_options(list(width = 200), {
  print(as.data.frame(Main |>
                        group_by(MarketMargin) |>
                        summarise(N = n(), MeanLogAPY = mean(LogAPY, na.rm = TRUE),
                                  SDLogAPY = sd(LogAPY, na.rm = TRUE),
                                  MedianAPY = median(apy, na.rm = TRUE),
                                  MeanLogAPYCapPct = mean(LogAPYCapPct, na.rm = TRUE),
                                  MeanGuaranteeShare = mean(GuaranteeShare, na.rm = TRUE),
                                  .groups = "drop") |>
                        mutate(across(where(is.double), \(x) round(x, 3)))), row.names = FALSE)
})

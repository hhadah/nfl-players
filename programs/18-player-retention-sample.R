# ============================================================================
# 18-player-retention-sample.R
# Builds analysis/analysis_player_retention: one row per gsis_id x season for
# EVERY player on a 2002-2025 REG weekly roster (the player_season universe of
# 04), with explicit employment risk sets, next-season retention outcomes and
# first freely bargained contract (UFA or extension) access outcomes. The
# sample is not conditioned on observed pay or on surviving to a veteran
# contract: players who never reach a cap-table year are in it.
# Person-weeks. Weekly roster rows (REG, is_key_primary) are classified into
#   UnderContract  = Active, Inactive, Reserve (RES/PUP/NWT/RSN/RSR),
#                    SuspendedExempt (SUS/EXE/E01/E14): on the 53-man roster
#                    or a reserve list of a franchise
#   PracticeSquad  = DEV (observable essentially from 2016 on)
#   NotEmployed    = Cut, Retired, FreeAgent (UFA/RFA/UDF): roster rows that
#                    list a player no franchise employs that week
#   Ambiguous      = TradePending (TRC/TRD/TRT), Unknown (missing status),
#                    UnverifiedListing2016 (2016 weeks 1-2 listings of players
#                    who neither played that week nor stayed under contract
#                    from week 3: the 2016 source leaks preseason rosters)
# and reduced to ONE row per person-week (the most employed status; a player
# listed by two franchises in the same week, 13 weeks in 2019, counts once).
# Team weeks vs calendar weeks: rosters list bye weeks before 2016 and omit
# them from 2016 on, so counts over all roster weeks are not comparable
# across eras. The comparable count is over game weeks, weeks in which the
# listing franchise played a REG game (nfl_team_games); the season has 16
# (2002-2020) or 17 (2021+) game weeks and 17 or 18 calendar weeks.
# Roster weeks are NOT paid weeks: pay (cap number, cash) is an annual
# accounting total from the OverTheCap cap table, attached for description
# only (HasPay marks a cap-table year; never divide pay by roster weeks).
# Outcomes
#   Retention: under contract to any franchise in at least one game week of
#   season t+1 (and game-day, same-franchise, final-week and practice-squad-
#   inclusive variants); NA (right-censored) for t = 2025.
#   Duration: weeks under contract in t (game weeks and all roster weeks),
#   spells, first/last week, final-week status; career seasons under
#   contract to date; the next-season retention indicator is the complement
#   of the discrete-time exit hazard. No t+1 listing at all is an exit; a
#   t+1 with only ambiguous listings leaves the outcome unknown (NA).
#   Employer: RetentionEmployer, the franchise of the last verified under-
#   contract week of t (NA when two franchises list the player under
#   contract in that week, EndFranchiseTied); the season-t employer for the
#   employer x season FE comparison of 19 (Table 34c), never read from t+1.
#   Access: first OBSERVED UFA/Extension contract (OTC contract type) signed
#   in year t+1 among seasons with none observed in a year <= t (discrete-
#   time first-passage risk set), with veteran-market (adds tags/tenders) and
#   any-non-rookie variants; the risk-set flags restrict to rookie cohorts
#   2011+ and seasons 2013+. OTC lists entry contracts for essentially all
#   drafted 2011+ entrants but for a minority of undrafted entrants before
#   2017 (coverage_player_contract_history.csv): dense coverage for drafted
#   entrants, not proof of complete histories (a page can omit a past deal);
#   year 2026 signings are observed through the OTC scrape only, so t = 2025
#   is flagged AccessWindowPartial and kept out of the access risk sets.
# Controls follow the quality blocks of 10 (pay_control_blocks.csv) measured
# in season t and before: A career stage, B season-t production, C career
# production before t, D pre-NFL signals, E season-t usage, F scout grade,
# G season-t employment (weeks under contract, final-week status). Written
# to analysis/player_retention_control_blocks.csv.
# Race: the person race vector as attached by 10 (hand codes, Wikipedia
# flags, BIFSG, predicted race and its prior covariates, race_predicted
# extras); race regressors are built in 19 by person_race_regressors().
# Inputs: DuckDB nfl_rosters_weekly, nfl_team_games, race_predicted (read-
# only), analysis/player_season.parquet (04), analysis/contracts.parquet
# (05), analysis/pay_control_blocks.csv (10), load_person_race().
# Outputs: analysis/analysis_player_retention.{parquet,csv} + codebook,
# analysis/player_retention_control_blocks.csv,
# analysis/coverage_player_retention.csv (by season),
# analysis/coverage_player_contract_history.csv (by rookie cohort x drafted),
# analysis/player_retention_status_weeks.csv (person-weeks by status class).
# Run after 10 in 95-make-all.R (19 reads the outputs).
# Date: 2026-10-03
# ============================================================================

T0Script18 <- Sys.time()
msg18 <- function(...) message(sprintf("18: %s", sprintf(...)))

# ---------------------------------------------------------------------------
# Status classes and season constants
# ---------------------------------------------------------------------------

# Weekly roster status -> status class (see header). ACT rows with a game-day
# inactive abbreviation (I*) are Inactive, as in roster_status_group() of
# 00-player-functions.R; in 2016-2020 many ACT rows carry no abbreviation, so
# inactives are partly counted as Active then (both are UnderContract).
retention_status_class <- function(status, abbr) {
  out <- rep("Unknown", length(status))
  inactive_abbr <- !is.na(abbr) & startsWith(abbr, "I")
  act <- !is.na(status) & status == "ACT"
  out[act & !inactive_abbr] <- "Active"
  out[act & inactive_abbr] <- "Inactive"
  out[status %in% "INA"] <- "Inactive"
  out[status %in% c("RES", "PUP", "NWT", "RSN", "RSR")] <- "Reserve"
  out[status %in% c("SUS", "EXE", "E01", "E14")] <- "SuspendedExempt"
  out[status %in% "DEV"] <- "PracticeSquad"
  out[status %in% "CUT"] <- "Cut"
  out[status %in% "RET"] <- "Retired"
  out[status %in% c("UFA", "RFA", "UDF")] <- "FreeAgent"
  out[status %in% c("TRC", "TRD", "TRT")] <- "TradePending"
  out
}
StatusClassOrder <- c("Active", "Inactive", "Reserve", "SuspendedExempt",
                      "PracticeSquad", "TradePending", "UnverifiedListing2016", "Cut",
                      "FreeAgent", "Retired", "Unknown")
UnderContractClasses <- c("Active", "Inactive", "Reserve", "SuspendedExempt")

con <- db_connect()

Weekly <- as.data.table(DBI::dbGetQuery(con, "
  SELECT gsis_id, season, week, franchise_id, status, status_description_abbr AS abbr
  FROM nfl_rosters_weekly
  WHERE season_type = 'REG' AND is_key_primary AND gsis_id IS NOT NULL AND gsis_id <> ''
    AND season BETWEEN 2002 AND 2025"))
TeamGameWeeks <- as.data.table(DBI::dbGetQuery(con, "
  SELECT DISTINCT franchise_id, season, week FROM nfl_team_games
  WHERE game_type = 'REG' AND season BETWEEN 2002 AND 2025"))
Weekly[, `:=`(season = as.integer(season), week = as.integer(week))]
TeamGameWeeks[, `:=`(season = as.integer(season), week = as.integer(week), GameWeek = 1L)]

# Calendar weeks (max REG week) and game weeks (modal franchise REG game
# count; BUF and CIN played 16 in 2022 after the cancelled game) by season
SeasonWeeks <- TeamGameWeeks[, .(NGames = .N), by = .(season, franchise_id)]
SeasonWeeks <- SeasonWeeks[, .(SeasonGameWeeks = as.integer(names(which.max(table(NGames))))),
                           by = season]
SeasonWeeks <- merge(SeasonWeeks, TeamGameWeeks[, .(SeasonCalendarWeeks = max(week)), by = season],
                     by = "season")
stopifnot(nrow(SeasonWeeks) == 24L,
          all(SeasonWeeks$SeasonCalendarWeeks - SeasonWeeks$SeasonGameWeeks == 1L))

# ---------------------------------------------------------------------------
# Person-weeks: one row per gsis_id x season x week
# ---------------------------------------------------------------------------

Weekly[, StatusClass := retention_status_class(status, abbr)]
Weekly[, Priority := match(StatusClass, StatusClassOrder)]
Weekly <- merge(Weekly, TeamGameWeeks, by = c("franchise_id", "season", "week"), all.x = TRUE)
Weekly[is.na(GameWeek), GameWeek := 0L]
NRowsWeekly <- nrow(Weekly)
# Person-weeks by status class before deduplication (coverage output)
StatusWeeks <- Weekly[, .(NRows = .N), by = .(season, StatusClass, GameWeek)]

# Trap (2026-10-03): do not count a person twice in a week. Keep the most
# employed row (status priority), then a game-week row, then the franchise
# with the most rows for the player that season, then franchise id.
Weekly[, FranchiseRows := .N, by = .(gsis_id, season, franchise_id)]
setorder(Weekly, gsis_id, season, week, Priority, -GameWeek, -FranchiseRows, franchise_id)
Weekly[, `:=`(NRowsWeek = .N, NFranchisesWeek = uniqueN(franchise_id),
              NFranchisesUnderContractWeek = uniqueN(franchise_id[StatusClass %in% UnderContractClasses])),
       by = .(gsis_id, season, week)]
PersonWeek <- unique(Weekly, by = c("gsis_id", "season", "week"))
PersonWeek <- PersonWeek[, .(gsis_id, season, week, franchise_id, StatusClass, GameWeek,
                             NRowsWeek, NFranchisesWeek, NFranchisesUnderContractWeek)]

# Trap (2026-10-03): the 2016 REG rosters list about 77 ACT players per team
# in week 1 and 56 in week 2 (53 from week 3 on; 2015 and 2017 show 44-53
# every week): preseason rosters leak into the first two weeks. About 500
# players appear only in 2016 week 1 and never play. An under-contract row
# in 2016 weeks 1-2 is kept only if the player has a snap-count row in that
# week (played) or is under contract in a week >= 3 of 2016 (survived the
# cutdown); otherwise it is reclassified as UnverifiedListing2016 (neither
# employed nor not employed). The few players on a week-1 53-man roster who
# were released before week 3 without playing are lost with the leak.
Snaps2016 <- as.data.table(DBI::dbGetQuery(con, "
  SELECT DISTINCT gsis_id, week FROM nfl_snap_counts
  WHERE season = 2016 AND game_type = 'REG' AND gsis_id IS NOT NULL"))
Snaps2016[, `:=`(season = 2016L, week = as.integer(week), Played = 1L)]
PersonWeek <- merge(PersonWeek, Snaps2016, by = c("gsis_id", "season", "week"), all.x = TRUE)
Later2016 <- PersonWeek[season == 2016L & week >= 3L & StatusClass %in% UnderContractClasses,
                        .(gsis_id, Later = 1L)]
Later2016 <- unique(Later2016)
PersonWeek <- merge(PersonWeek, Later2016, by = "gsis_id", all.x = TRUE)
PersonWeek[, Unverified2016 := as.integer(season == 2016L & week <= 2L &
                                            StatusClass %in% UnderContractClasses &
                                            is.na(Played) & is.na(Later))]
NUnverified2016 <- sum(PersonWeek$Unverified2016)
PersonWeek[Unverified2016 == 1L, StatusClass := "UnverifiedListing2016"]
PersonWeek[, c("Played", "Later") := NULL]
msg18("2016 weeks 1-2: %d under-contract listings reclassified as UnverifiedListing2016 (%d players)",
      NUnverified2016, PersonWeek[Unverified2016 == 1L, uniqueN(gsis_id)])
stopifnot(NUnverified2016 > 0L)
PersonWeek[, UnderContract := as.integer(StatusClass %in% UnderContractClasses)]
PersonWeek[, Employed := as.integer(UnderContract == 1L | StatusClass == "PracticeSquad")]
PersonWeek <- merge(PersonWeek, SeasonWeeks, by = "season")
# The listing franchise had a bye in the preceding calendar week (for spells)
PrevGame <- TeamGameWeeks[, .(franchise_id, season, week = week + 1L, PrevWeekGame = 1L)]
PersonWeek <- merge(PersonWeek, PrevGame, by = c("franchise_id", "season", "week"), all.x = TRUE)
PersonWeek[, PrevWeekBye := as.integer(week > 1L & is.na(PrevWeekGame))]
PersonWeek[, PrevWeekGame := NULL]
NMultiFranchiseWeeks <- sum(PersonWeek$NFranchisesWeek > 1L)
msg18("%s roster rows -> %s person-weeks (%s weeks listed by two franchises)",
      format(NRowsWeekly, big.mark = ","), format(nrow(PersonWeek), big.mark = ","),
      NMultiFranchiseWeeks)
rm(Weekly)

# ---------------------------------------------------------------------------
# Season aggregates per gsis_id x season
# ---------------------------------------------------------------------------

# Under-contract spells within a season: a new spell starts when the previous
# under-contract week is not the preceding calendar week, unless the one
# skipped week is a bye of the franchise listing the later week (rosters
# omit bye weeks from 2016 on) or the franchise changes
count_spells <- function(week, franchise, prev_week_bye, under_contract) {
  keep <- under_contract == 1L
  w <- week[keep]
  if (length(w) <= 1L) return(length(w))
  f <- franchise[keep]
  bye <- prev_week_bye[keep]
  gap <- diff(w)
  same_fr <- f[-1] == f[-length(f)]
  continuous <- gap == 1L | (gap == 2L & bye[-1] == 1L)
  1L + sum(!same_fr | !continuous)
}

setorder(PersonWeek, gsis_id, season, week)
Season <- PersonWeek[, .(
  WeeksObserved = .N,
  WeeksObservedGame = sum(GameWeek),
  WeeksUnderContract = sum(UnderContract),
  WeeksUnderContractGame = sum(UnderContract * GameWeek),
  WeeksActive = sum(StatusClass == "Active"),
  WeeksInactive = sum(StatusClass == "Inactive"),
  WeeksGameDayGame = sum(StatusClass %in% c("Active", "Inactive") & GameWeek == 1L),
  WeeksReserve = sum(StatusClass == "Reserve"),
  WeeksSuspendedExempt = sum(StatusClass == "SuspendedExempt"),
  WeeksPracticeSquad = sum(StatusClass == "PracticeSquad"),
  WeeksPracticeSquadGame = sum(StatusClass == "PracticeSquad" & GameWeek == 1L),
  WeeksEmployedGame = sum(Employed * GameWeek),
  WeeksCut = sum(StatusClass == "Cut"),
  WeeksRetired = sum(StatusClass == "Retired"),
  WeeksFreeAgent = sum(StatusClass == "FreeAgent"),
  WeeksTradePending = sum(StatusClass == "TradePending"),
  WeeksUnverified2016 = sum(StatusClass == "UnverifiedListing2016"),
  WeeksUnknownStatus = sum(StatusClass == "Unknown"),
  WeeksMultiFranchise = sum(NFranchisesWeek > 1L),
  FirstWeekUnderContract = if (any(UnderContract == 1L)) min(week[UnderContract == 1L]) else NA_integer_,
  LastWeekUnderContract = if (any(UnderContract == 1L)) max(week[UnderContract == 1L]) else NA_integer_,
  UnderContractWeek1 = as.integer(any(UnderContract == 1L & week == 1L)),
  UnderContractFinalWeek = as.integer(any(UnderContract == 1L & week == SeasonCalendarWeeks)),
  EmployedFinalWeek = as.integer(any(Employed == 1L & week == SeasonCalendarWeeks)),
  NSpellsUnderContract = count_spells(week, franchise_id, PrevWeekBye, UnderContract),
  NFranchisesUnderContract = uniqueN(franchise_id[UnderContract == 1L]),
  NFranchisesAnyRow = uniqueN(franchise_id),
  EndFranchiseUnderContract = if (any(UnderContract == 1L)) franchise_id[UnderContract == 1L][sum(UnderContract)] else NA_character_,
  EndFranchiseTied = if (any(UnderContract == 1L)) as.integer(NFranchisesUnderContractWeek[UnderContract == 1L][sum(UnderContract)] > 1L) else NA_integer_,
  UnderContractFranchises = paste(sort(unique(franchise_id[UnderContract == 1L])), collapse = ";"),
  SeasonGameWeeks = SeasonGameWeeks[1L],
  SeasonCalendarWeeks = SeasonCalendarWeeks[1L]),
  by = .(gsis_id, season)]
Season[UnderContractFranchises == "", UnderContractFranchises := NA_character_]
# A player who moves between franchises with different bye weeks can be under
# contract in every calendar week (SeasonGameWeeks + 1 game weeks); the share
# is capped at 1
Season[, ShareSeasonUnderContract := pmin(1, WeeksUnderContractGame / SeasonGameWeeks)]
# Ambiguous-only seasons (trade-pending, unknown or unverified 2016 listings
# and nothing else) are neither employment nor evidence of exit; they stay
# distinct from NoEmploymentWeek (cut, retired or free-agent listings only)
Season[, WeeksAmbiguous := WeeksTradePending + WeeksUnverified2016 + WeeksUnknownStatus]
Season[, WeeksNotEmployed := WeeksCut + WeeksRetired + WeeksFreeAgent]
Season[, EmploymentClass := fifelse(WeeksUnderContractGame > 0L, "UnderContract",
                             fifelse(WeeksPracticeSquad > 0L, "PracticeSquadOnly",
                             fifelse(WeeksUnderContract > 0L, "UnderContractByeOnly",
                             fifelse(WeeksNotEmployed == 0L & WeeksAmbiguous > 0L, "AmbiguousOnly",
                                     "NoEmploymentWeek"))))]
stopifnot(all(Season$WeeksUnderContractGame <= Season$SeasonCalendarWeeks),
          all(Season$ShareSeasonUnderContract <= 1))
# Author decision 2026-10-03: the season-t employer for the employer-FE
# retention comparison (19, Table 34c) is the franchise of the last verified
# under-contract week of t. EndFranchiseUnderContract is well-defined when a
# single franchise lists the player under contract in that week; when two
# do (the 2019 duplicate listings and trade weeks), the person-week
# deduplication picked one by rows-in-season and then franchise id, which
# is a tie-break, not a verified employer. Those seasons keep
# EndFranchiseUnderContract but get no RetentionEmployer; 19 drops them
# from the employer-FE sample rather than assigning them to either team.
# Unverified 2016 listings are not under contract, so they never set the
# end franchise; a 2016 week-1/2 listing kept because the player played
# that week or stayed under contract from week 3 is verified by that fact.
Season[, RetentionEmployer := fifelse(!is.na(EndFranchiseTied) & EndFranchiseTied == 0L,
                                      EndFranchiseUnderContract, NA_character_)]
msg18("season-t employer: %s player-seasons with an end franchise, %s tied in the final under-contract week (no RetentionEmployer)",
      format(sum(!is.na(Season$EndFranchiseUnderContract)), big.mark = ","),
      format(sum(Season$EndFranchiseTied %in% 1L), big.mark = ","))
stopifnot(all(is.na(Season$RetentionEmployer) == (is.na(Season$EndFranchiseTied) | Season$EndFranchiseTied == 1L)),
          all(Season$NFranchisesUnderContract[!is.na(Season$RetentionEmployer)] >= 1L))
print(Season[, .N, by = .(season, EmploymentClass)][order(season, EmploymentClass)],
      nrows = 200)

# ---------------------------------------------------------------------------
# Player-season panel of 04: identity, usage, production, pay, signals
# ---------------------------------------------------------------------------

PlayerSeason <- as.data.table(arrow::read_parquet(file.path(analysis, "player_season.parquet")))
PanelLabels <- {
  cb <- fread(file.path(analysis, "codebook_player_season.csv"))
  cb <- cb[!is.na(label) & nzchar(label)]
  setNames(cb$label, cb$variable)
}
# Same universe as 04 (distinct REG roster (gsis_id, season) pairs)
stopifnot(nrow(Season) == nrow(PlayerSeason),
          nrow(merge(Season, PlayerSeason[, .(gsis_id, season)], by = c("gsis_id", "season"))) ==
            nrow(Season))
# 04's status counts are over all roster weeks (bye weeks included before
# 2016); the game-day count must agree with 04 up to bye weeks
Chk <- merge(Season[, .(gsis_id, season, WeeksActive, WeeksInactive, WeeksReserve, WeeksPracticeSquad)],
             PlayerSeason[, .(gsis_id, season, A = WeeksActive, I = WeeksInactive, R = WeeksReserve,
                              P = WeeksPracticeSquad)], by = c("gsis_id", "season"))
msg18("status weeks vs 04 (rows that differ; 04 counts each status once per week even when two franchises list the player): active %d, inactive %d, reserve %d, practice squad %d",
      sum(Chk$WeeksActive != Chk$A), sum(Chk$WeeksInactive != Chk$I),
      sum(Chk$WeeksReserve != Chk$R), sum(Chk$WeeksPracticeSquad != Chk$P))
stopifnot(sum(Chk$WeeksActive != Chk$A & Chk$season != 2016L) <= NMultiFranchiseWeeks)
rm(Chk)

keep_panel <- c("gsis_id", "season", "display_name", "PositionGroup", "PrimaryFranchise",
                "Age", "RookieSeason", "Experience", "IsRookie", "DraftYear", "DraftRound",
                "DraftPick", "Undrafted", "FirstRosterSeason", "InNflPlayers",
                "InSampleTminus1", "NFranchises", "GamesPlayed", "GamesPlayedSnapBased",
                "GamesStartedDepth", "GamesStartedSnaps", "OffSnaps", "DefSnaps", "STSnaps",
                "InjuryWeeks", "HasStatRow", "HasPay", "CapNumber", "CashPaid", "CapPercent",
                "GoverningContractId", "GoverningContractType", "OnRookieContract",
                "HasCollegeLink", "HasRecruit", "CombineInvite", "AthleticScoreN",
                "race", "hispanic", "black_any", "nonwhite", "race_source",
                "black_provisional", "black_provisional_source", "wiki_cat_black",
                "p_black_bifsg")
Retention <- merge(Season, PlayerSeason[, ..keep_panel], by = c("gsis_id", "season"))
stopifnot(nrow(Retention) == nrow(Season))

# ---------------------------------------------------------------------------
# Next-season retention outcomes (right-censored for 2025)
# ---------------------------------------------------------------------------

MaxRosterSeason <- max(Retention$season)
Next <- Season[, .(gsis_id, season = season - 1L,
                   NextSeasonWeeksUnderContractGame = WeeksUnderContractGame,
                   NextSeasonWeeksGameDayGame = WeeksGameDayGame,
                   NextSeasonWeeksEmployedGame = WeeksEmployedGame,
                   NextSeasonWeeksPracticeSquad = WeeksPracticeSquad,
                   NextSeasonUnderContractFinalWeek = UnderContractFinalWeek,
                   NextSeasonUnderContractFranchises = UnderContractFranchises,
                   NextSeasonAmbiguousOnly = as.integer(EmploymentClass == "AmbiguousOnly"),
                   NextSeasonAnyRosterRow = 1L)]
Retention <- merge(Retention, Next, by = c("gsis_id", "season"), all.x = TRUE)
# Next season observed: t+1 rosters exist (t <= 2024) and the player's t+1
# evidence is not ambiguous only. No listing at all in t+1 is an exit (the
# rosters list every employed player); an ambiguous-only listing leaves the
# outcome unknown rather than asserting exit
Retention[, RetentionCensored := as.integer(season >= MaxRosterSeason)]
Retention[is.na(NextSeasonAmbiguousOnly), NextSeasonAmbiguousOnly := 0L]
Retention[RetentionCensored == 1L, NextSeasonAmbiguousOnly := NA_integer_]
Retention[, RetentionObserved := as.integer(RetentionCensored == 0L & NextSeasonAmbiguousOnly == 0L)]
nxt_cols <- c("NextSeasonWeeksUnderContractGame", "NextSeasonWeeksGameDayGame",
              "NextSeasonWeeksEmployedGame", "NextSeasonWeeksPracticeSquad",
              "NextSeasonUnderContractFinalWeek", "NextSeasonAnyRosterRow")
for (v in nxt_cols) {
  Retention[RetentionObserved == 1L & is.na(get(v)), (v) := 0L]
  Retention[RetentionObserved == 0L, (v) := NA_integer_]
}
Retention[RetentionObserved == 0L, NextSeasonUnderContractFranchises := NA_character_]
msg18("next-season outcomes unknown because the only t+1 evidence is ambiguous: %d player-seasons",
      sum(Retention$NextSeasonAmbiguousOnly %in% 1L))
same_franchise <- function(a, b) {
  out <- rep(NA_integer_, length(a))
  ok <- !is.na(a) & !is.na(b)
  out[ok] <- vapply(which(ok), function(i) {
    as.integer(length(intersect(strsplit(a[i], ";", fixed = TRUE)[[1]],
                                strsplit(b[i], ";", fixed = TRUE)[[1]])) > 0L)
  }, integer(1))
  out
}
Retention[, `:=`(
  RetainedNextSeason = fifelse(RetentionObserved == 1L,
                               as.integer(NextSeasonWeeksUnderContractGame > 0L), NA_integer_),
  RetainedNextSeasonGameDay = fifelse(RetentionObserved == 1L,
                                      as.integer(NextSeasonWeeksGameDayGame > 0L), NA_integer_),
  RetainedNextSeasonEmployed = fifelse(RetentionObserved == 1L,
                                       as.integer(NextSeasonWeeksEmployedGame > 0L), NA_integer_),
  RetainedNextSeasonFinalWeek = fifelse(RetentionObserved == 1L,
                                        NextSeasonUnderContractFinalWeek, NA_integer_))]
Retention[, RetainedNextSeasonSameFranchise := same_franchise(UnderContractFranchises,
                                                               NextSeasonUnderContractFranchises)]
Retention[RetentionObserved == 1L & !is.na(UnderContractFranchises) &
            is.na(RetainedNextSeasonSameFranchise), RetainedNextSeasonSameFranchise := 0L]
# Author decision 2026-10-03: the 2015 -> 2016 transition is a source break.
# 2016's leaked preseason listings mean that most 2015 players who left the
# league carry an unverified 2016 listing; treating those as unknown removes
# the exits (2015 retention 0.93 against 0.77 in adjacent seasons), treating
# them as exits trusts a listing the source does not verify. t = 2015 is
# therefore outside the main retention risk set; 19 reports both treatments
# as bounds (InRiskSetRetentionIncl2015 with RetainedNextSeason and with
# RetainedNextSeasonAmbiguousAsExit).
Retention[, NextSeasonSourceBreak := as.integer(season == 2015L)]
Retention[, RetainedNextSeasonAmbiguousAsExit := fifelse(
  RetentionCensored == 0L & NextSeasonAmbiguousOnly == 1L, 0L, RetainedNextSeason)]
Retention[, RetainedNextSeasonNewFranchiseOnly := fifelse(
  RetainedNextSeason == 1L & !is.na(RetainedNextSeasonSameFranchise),
  as.integer(RetainedNextSeasonSameFranchise == 0L), NA_integer_)]

# Career employment to date (seasons strictly before t) and the career spell
setorder(Retention, gsis_id, season)
Retention[, `:=`(
  CareerSeasonsUnderContract = shift(cumsum(as.integer(WeeksUnderContractGame > 0L)), fill = 0L),
  CareerWeeksUnderContractGame = shift(cumsum(WeeksUnderContractGame), fill = 0L),
  CareerPanelSeasons = seq_len(.N) - 1L,
  LastRosterSeason = max(season)), by = gsis_id]
Retention[, `:=`(
  CareerLeftCensored = as.integer(!is.na(RookieSeason) & RookieSeason < 2002L),
  CareerRightCensored = as.integer(LastRosterSeason == MaxRosterSeason),
  ObservedRosterSeasons = LastRosterSeason - FirstRosterSeason + 1L)]

# ---------------------------------------------------------------------------
# Contract access: first UFA/extension, veteran-market and non-rookie deals
# ---------------------------------------------------------------------------

Contracts <- as.data.table(arrow::read_parquet(
  file.path(analysis, "contracts.parquet"),
  col_select = c("contract_id", "gsis_id", "year_signed", "year_signed_missing",
                 "ContractType", "IdenticalDuplicate", "GsisLinkSuspect", "SigningFranchise",
                 "NewTeam", "apy", "years", "contract_seq")))
NContractsAll <- nrow(Contracts)
Contracts <- Contracts[!is.na(gsis_id) & nzchar(gsis_id) & !year_signed_missing & year_signed > 0 &
                         IdenticalDuplicate == 0L & !(GsisLinkSuspect %in% 1L) & !is.na(ContractType)]
msg18("contracts with a player, a signing year, a type and a clean link: %s of %s",
      format(nrow(Contracts), big.mark = ","), format(NContractsAll, big.mark = ","))
MaxContractYear <- max(Contracts$year_signed)
BargainedTypes <- c("UFA", "Extension")
VeteranMarketTypes <- c(BargainedTypes, "Franchise", "Transition", "RFA", "ERFA")
NonRookieTypes <- c(VeteranMarketTypes, "SFA", "Other")
first_of <- function(types, prefix) {
  d <- Contracts[ContractType %in% types]
  setorder(d, gsis_id, year_signed, contract_seq, contract_id)
  d <- d[, .SD[1L], by = gsis_id]
  out <- d[, .(gsis_id, year_signed, ContractType, SigningFranchise, NewTeam, apy, years)]
  setnames(out, c("year_signed", "ContractType", "SigningFranchise", "NewTeam", "apy", "years"),
           paste0(prefix, c("Year", "Type", "Franchise", "NewTeam", "APY", "Years")))
  out
}
FirstBarg <- first_of(BargainedTypes, "FirstBargained")
FirstVet <- first_of(VeteranMarketTypes, "FirstVeteranMarket")[, .(gsis_id, FirstVeteranMarketYear, FirstVeteranMarketType)]
FirstNonRookie <- first_of(NonRookieTypes, "FirstNonRookie")[, .(gsis_id, FirstNonRookieYear, FirstNonRookieType)]
Retention <- merge(Retention, FirstBarg, by = "gsis_id", all.x = TRUE)
Retention <- merge(Retention, FirstVet, by = "gsis_id", all.x = TRUE)
Retention <- merge(Retention, FirstNonRookie, by = "gsis_id", all.x = TRUE)
Retention[, HasAnyContractRow := as.integer(gsis_id %in% Contracts$gsis_id)]
Retention[, EntryContractObserved := as.integer(gsis_id %in% Contracts[ContractType %in% c("Drafted", "UDFA"), gsis_id])]

# Author decision 2026-10-03: the access risk set starts with rookie cohorts
# 2011+ and seasons 2013+. Coverage evidence (coverage_player_contract_
# history.csv): OTC lists an entry contract for essentially every drafted
# entrant from 2011 (98-100%) but for 7-44% of drafted entrants before 2011
# (players whose careers ended before OTC's coverage are missing, a survivor
# selection); undrafted entrants' entry deals are observed for 17-30% of
# 2011-2016 cohorts and 56-96% from 2017. A missing entry deal does not hide
# a later UFA/extension when the player has an OTC page at all, but dense
# entry coverage is not proof of complete histories (a page can omit a past
# deal) and undrafted coverage is thin, so every First* outcome is the first
# OBSERVED qualifying contract and 19 reports the drafted-only and
# OTC-page-only sensitivities.
AccessCohortStart <- 2011L
AccessSeasonStart <- 2013L
access_flags <- function(dt, first_year, prefix) {
  before <- !is.na(dt[[first_year]]) & dt[[first_year]] <= dt$season
  observable <- !is.na(dt$RookieSeason) & dt$RookieSeason >= AccessCohortStart &
    dt$season >= AccessSeasonStart & !is.na(dt$Experience) & dt$Experience >= 0L
  set(dt, j = paste0(prefix, "Before"), value = as.integer(before))
  set(dt, j = paste0(prefix, "AtRisk"),
      value = as.integer(!before & observable & dt$season < MaxContractYear))
  set(dt, j = paste0(prefix, "EventNext"),
      value = fifelse(before | dt$season >= MaxContractYear, NA_integer_,
                      as.integer(!is.na(dt[[first_year]]) & dt[[first_year]] == dt$season + 1L)))
  dt
}
Retention <- access_flags(Retention, "FirstBargainedYear", "Bargained")
Retention <- access_flags(Retention, "FirstVeteranMarketYear", "VeteranMarket")
Retention <- access_flags(Retention, "FirstNonRookieYear", "NonRookie")
Retention[, AccessWindowPartial := as.integer(season + 1L == MaxContractYear)]
Retention[, AccessCohortObservable := as.integer(!is.na(RookieSeason) & RookieSeason >= AccessCohortStart &
                                                   season >= AccessSeasonStart)]
Retention[, YearsToFirstBargained := fifelse(!is.na(FirstBargainedYear) & FirstBargainedYear > season,
                                             FirstBargainedYear - season, NA_integer_)]
# First bargained contracts not preceded by a panel season (the event is not
# reachable from any risk-set row; reported, not modelled)
FirstBargRows <- Retention[!is.na(FirstBargainedYear) & FirstBargainedYear == season + 1L &
                             BargainedAtRisk == 1L, uniqueN(gsis_id)]
FirstBargPlayers <- Retention[AccessCohortObservable == 1L & !is.na(FirstBargainedYear) &
                                FirstBargainedYear > AccessSeasonStart, uniqueN(gsis_id)]
msg18("first UFA/extension contracts of 2011+ cohorts signed after %d: %d players; %d reached from an at-risk season (the rest were not on a roster the season before signing)",
      AccessSeasonStart, FirstBargPlayers, FirstBargRows)

# ---------------------------------------------------------------------------
# Controls: quality blocks of 10 measured in season t and before
# ---------------------------------------------------------------------------

PayBlocks <- fread(file.path(analysis, "pay_control_blocks.csv"))
PayBlocks <- PayBlocks[sample == "player_season"]
is_miss <- function(v) endsWith(v, "Miss")
# Block B: season-t analogues of 10's one-season lags (Lag<stem> -> <stem>)
BRows <- PayBlocks[block == "B" & !is_miss(variable) & startsWith(variable, "Lag")]
BRows[, variable := sub("^Lag", "", variable)]
BRows <- BRows[!variable %in% c("GamesPlayed", "InjuryWeeks")]
# Block C: career sums before t (recomputed here on the full panel)
# (NoCareerSeasons and CareerLeftCensored are rebuilt here, not read)
CRows <- PayBlocks[block == "C" & !is_miss(variable) &
                     !variable %in% c("NoCareerSeasons", "CareerLeftCensored")]
CareerStems <- sub("^Career", "", CRows[startsWith(variable, "Career") & slope_groups != "all",
                                        variable])
# Block D: pre-NFL signals (time-invariant; copied from the panel)
DRows <- PayBlocks[block == "D" & !is_miss(variable)]
# Block E: season-t usage (Lag<x> -> <x>) and career usage before t
ERows <- PayBlocks[block == "E" & !is_miss(variable)]
ERows[, variable := sub("^Lag", "", variable)]
FRows <- PayBlocks[block == "F" & !is_miss(variable)]
stopifnot(nrow(BRows) > 0, nrow(CRows) > 0, nrow(DRows) > 0, nrow(ERows) > 0, nrow(FRows) > 0)

need_panel <- unique(c(BRows$variable, CareerStems, DRows$variable, ERows$variable[!startsWith(ERows$variable, "Career")],
                       FRows$variable, "GamesPlayed", "InjuryWeeks", "GamesStartedDepth",
                       "OffSnaps", "DefSnaps", "STSnaps"))
need_panel <- setdiff(need_panel, c("LogDraftPick", "Undrafted"))
missing_panel <- setdiff(need_panel, names(PlayerSeason))
if (length(missing_panel) > 0) {
  stop("18: pay_control_blocks.csv names variables absent from player_season: ",
       paste(missing_panel, collapse = ", "), call. = FALSE)
}
Extra <- PlayerSeason[, c("gsis_id", "season", setdiff(need_panel, names(Retention))), with = FALSE]
Retention <- merge(Retention, Extra, by = c("gsis_id", "season"))
stopifnot(nrow(Retention) == nrow(Season))

# Career sums over panel seasons strictly before t (NA when no earlier season
# recorded the measure; seasons where the measure is NA add 0), as in 10
setorder(Retention, gsis_id, season)
career_sum <- function(x) {
  obs <- shift(cumsum(!is.na(x)), fill = 0L)
  s <- shift(cumsum(fifelse(is.na(x), 0, as.numeric(x))), fill = 0)
  fifelse(obs > 0L, s, NA_real_)
}
Retention[, OffDefSnaps2013 := fifelse(season >= 2013L, OffSnaps + DefSnaps, NA_real_)]
Retention[, STSnaps2013 := fifelse(season >= 2013L, STSnaps, NA_real_)]
career_src <- c(CareerGames = "GamesPlayed", CareerStartsDepth = "GamesStartedDepth",
                CareerInjuryWeeks = "InjuryWeeks", CareerOffDefSnaps = "OffDefSnaps2013",
                CareerSTSnaps = "STSnaps2013",
                setNames(CareerStems, paste0("Career", CareerStems)))
Retention[, (names(career_src)) := lapply(.SD, career_sum), by = gsis_id,
          .SDcols = unname(career_src)]
Retention[, c("OffDefSnaps2013", "STSnaps2013") := NULL]
Retention[, NoCareerSeasons := as.integer(CareerPanelSeasons == 0L)]

# Block A and D constructed variables
experience_bin <- function(x) {
  as.character(cut(x, breaks = c(-Inf, 0, 1, 2, 3, 6, Inf),
                   labels = c("0", "1", "2", "3", "4-6", "7+")))
}
Retention[, `:=`(ExperienceBin = experience_bin(Experience),
                 AgeSq = Age^2,
                 LogDraftPick = fifelse(Undrafted == 1L, 0, log(DraftPick)),
                 DraftRound = fifelse(Undrafted == 1L, 0L, DraftRound),
                 PracticeSquadOnly = as.integer(EmploymentClass == "PracticeSquadOnly"),
                 MultipleSpells = as.integer(NSpellsUnderContract > 1L))]

# Zero-fill with Miss indicators (fill_missing() of 00-analysis-functions.R);
# indicators that are constant or identical to a base indicator are dropped
fill_with_base <- function(dt, vars, base = NULL) {
  df <- fill_missing(as.data.frame(dt), vars)
  kept <- character()
  for (v in vars) {
    m <- paste0(v, "Miss")
    redundant <- length(unique(df[[m]])) == 1L ||
      (!is.null(base) && identical(df[[m]], as.integer(df[[base]])))
    if (redundant) df[[m]] <- NULL else kept <- c(kept, m)
  }
  dt <- as.data.table(df)
  attr(dt, "miss_kept") <- kept
  dt
}
blockA <- c("Age", "AgeSq")
blockB <- c("GamesPlayed", "InjuryWeeks", BRows$variable)
blockC <- c(CRows$variable)
blockD <- c("LogDraftPick", setdiff(DRows$variable, c("LogDraftPick", "Undrafted")))
blockE <- ERows$variable
blockF <- FRows$variable
blockG <- c("WeeksUnderContractGame", "UnderContractFinalWeek", "MultipleSpells")
stopifnot(all(c(blockA, blockB, blockC, blockD, blockE, blockF, blockG) %in% names(Retention)))
Retention <- fill_with_base(Retention, blockA); MissA <- attr(Retention, "miss_kept")
Retention <- fill_with_base(Retention, blockB); MissB <- attr(Retention, "miss_kept")
Retention <- fill_with_base(Retention, blockC, base = "NoCareerSeasons"); MissC <- attr(Retention, "miss_kept")
Retention <- fill_with_base(Retention, blockD); MissD <- attr(Retention, "miss_kept")
Retention <- fill_with_base(Retention, blockE, base = "NoCareerSeasons"); MissE <- attr(Retention, "miss_kept")
Retention <- fill_with_base(Retention, blockF); MissF <- attr(Retention, "miss_kept")
Retention <- fill_with_base(Retention, blockG); MissG <- attr(Retention, "miss_kept")
msg18("Miss indicators kept: %s", paste(c(MissA, MissB, MissC, MissD, MissE, MissF, MissG), collapse = ", "))

# ---------------------------------------------------------------------------
# Race vector (as attached by 10): hand codes, flags, BIFSG, predicted race
# ---------------------------------------------------------------------------

race_cols_hand <- c("race", "hispanic", "black_any", "nonwhite", "race_source",
                    "black_provisional", "black_provisional_source", "wiki_cat_black",
                    "wiki_cat_hispanic_latino", "wiki_cat_asian",
                    "wiki_cat_pacific_islander", "wiki_cat_native_american",
                    "p_black_bifsg")
race_cols_pred <- c("p_white_pred", "p_black_pred", "p_hispanic_pred", "p_api_pred",
                    "p_aian_pred", "p_multi_pred", "p_black_any_pred", "prior_black_pred",
                    "pred_method", "p_white_preddoc", "p_black_any_preddoc",
                    "p_hispanic_preddoc", "p_api_preddoc", "p_aian_preddoc",
                    "p_multi_preddoc", "pred_method_preddoc", "documented_race",
                    "documented_black_any")
race_prior_cols <- c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket",
                     "pred_college_type")
race_extra_map <- c(pred_county_available = "county_available",
                    pred_has_wiki = "has_wiki", pred_career_bucket = "career_bucket",
                    p_white_pred_raked = "p_white_pred_raked",
                    p_black_pred_raked = "p_black_pred_raked",
                    p_white_pred_nodraft = "p_white_pred_nodraft",
                    p_black_pred_nodraft = "p_black_pred_nodraft",
                    p_black_or_multi_pred = "p_black_or_multi_pred")
race_extra_prior <- c("pred_county_available", "pred_has_wiki", "pred_career_bucket")
race_cols_load <- c(race_cols_hand, race_cols_pred, race_prior_cols)
PlayerRace <- as.data.table(load_person_race(con, hand_coded))
PlayerRace <- PlayerRace[entity == "player", c("person_id", race_cols_load), with = FALSE]
setnames(PlayerRace, "person_id", "gsis_id")
PredExtra <- data.table(gsis_id = character())
if (DBI::dbExistsTable(con, "race_predicted")) {
  avail <- race_extra_map[race_extra_map %in% DBI::dbListFields(con, "race_predicted")]
  PredExtra <- as.data.table(DBI::dbGetQuery(con, sprintf(
    "SELECT person_uid, %s FROM race_predicted WHERE entity = 'player'",
    paste(sprintf("%s AS %s", avail, names(avail)), collapse = ", "))))
  PredExtra[, gsis_id := sub("^player:", "", person_uid)]
  PredExtra[, person_uid := NULL]
}
db_disconnect(con)
stopifnot(!anyDuplicated(PredExtra$gsis_id), !anyDuplicated(PlayerRace$gsis_id))
race_cols <- c(race_cols_load, intersect(names(race_extra_map), names(PredExtra)))
PlayerRace <- merge(PlayerRace, PredExtra, by = "gsis_id", all.x = TRUE)
PlayerRace[, HasWikiArticle := as.integer(!is.na(wiki_cat_black))]
for (v in intersect(c(race_prior_cols, race_extra_prior), names(PlayerRace))) {
  PlayerRace[, (v) := fifelse(is.na(as.character(get(v))), "unknown", as.character(get(v)))]
}
Retention[, (intersect(race_cols_hand, names(Retention))) := NULL]
Retention <- merge(Retention, PlayerRace, by = "gsis_id", all.x = TRUE)
stopifnot(nrow(Retention) == nrow(Season))
msg18("player-seasons without a predicted P(Black): %d of %d (roster-only ids without an nfl_players row: %d)",
      sum(is.na(Retention$p_black_any_pred)), nrow(Retention), sum(Retention$InNflPlayers == 0L))

# ---------------------------------------------------------------------------
# Risk-set flags
# ---------------------------------------------------------------------------

# Retention risk set: under contract in at least one game week of t, a
# position group, experience 0-15 (as in 10), and t+1 observed
Retention[, InRiskSetRetentionIncl2015 := as.integer(
  WeeksUnderContractGame > 0L & !is.na(PositionGroup) & !is.na(Experience) &
    Experience >= 0L & Experience <= 15L & RetentionCensored == 0L)]
Retention[, InRiskSetRetention := as.integer(
  InRiskSetRetentionIncl2015 == 1L & RetentionObserved == 1L & NextSeasonSourceBreak == 0L)]
# Employed risk set (adds practice-squad-only seasons; 2016+ when the practice
# squad is observed in the rosters)
Retention[, InRiskSetEmployed := as.integer(
  WeeksEmployedGame > 0L & season >= 2016L & !is.na(PositionGroup) & !is.na(Experience) &
    Experience >= 0L & Experience <= 15L & RetentionObserved == 1L)]
# Access risk sets: no earlier contract of the type, cohort/season window,
# under contract in t, position group, experience 0-15
for (p in c("Bargained", "VeteranMarket", "NonRookie")) {
  Retention[, (paste0("InRiskSet", p)) := as.integer(
    get(paste0(p, "AtRisk")) == 1L & AccessWindowPartial == 0L & WeeksUnderContractGame > 0L &
      !is.na(PositionGroup) & Experience <= 15L)]
}

# ---------------------------------------------------------------------------
# Control dictionary
# ---------------------------------------------------------------------------

slope_of_B <- setNames(BRows$slope_groups, BRows$variable)
slope_of_C <- setNames(CRows$slope_groups, CRows$variable)
slope_of_D <- setNames(DRows$slope_groups, DRows$variable)
slope_of_E <- setNames(ERows$slope_groups, ERows$variable)
dict_rows <- function(block, vars, slope_groups) {
  data.table(sample = "player_retention", block = block, variable = vars,
             slope_groups = rep_len(slope_groups, length(vars)))
}
miss_of <- function(vars, miss) intersect(paste0(vars, "Miss"), miss)
Dictionary <- rbindlist(list(
  dict_rows("A", c("Experience", blockA, miss_of(blockA, MissA)), "none"),
  dict_rows("B", c("GamesPlayed", "InjuryWeeks"), "all"),
  dict_rows("B", BRows$variable, slope_of_B[BRows$variable]),
  dict_rows("B", miss_of(BRows$variable, MissB), slope_of_B[sub("Miss$", "", miss_of(BRows$variable, MissB))]),
  dict_rows("B", miss_of(c("GamesPlayed", "InjuryWeeks"), MissB), "all"),
  dict_rows("C", c("NoCareerSeasons", "CareerLeftCensored"), "none"),
  dict_rows("C", CRows$variable, slope_of_C[CRows$variable]),
  dict_rows("C", miss_of(CRows$variable, MissC), slope_of_C[sub("Miss$", "", miss_of(CRows$variable, MissC))]),
  dict_rows("D", c("LogDraftPick", "Undrafted", miss_of("LogDraftPick", MissD)), "none"),
  dict_rows("D", setdiff(DRows$variable, c("LogDraftPick", "Undrafted")),
            slope_of_D[setdiff(DRows$variable, c("LogDraftPick", "Undrafted"))]),
  dict_rows("D", miss_of(setdiff(DRows$variable, c("LogDraftPick", "Undrafted")), MissD),
            slope_of_D[sub("Miss$", "", miss_of(setdiff(DRows$variable, c("LogDraftPick", "Undrafted")), MissD))]),
  dict_rows("E", c(blockE, miss_of(blockE, MissE)), "all"),
  dict_rows("F", c(blockF, miss_of(blockF, MissF)), "none"),
  dict_rows("G", c(blockG, miss_of(blockG, MissG)), "none")))
Dictionary <- Dictionary[!is.na(variable)]
InAnyRiskSet <- Retention$InRiskSetRetentionIncl2015 == 1L | Retention$InRiskSetEmployed == 1L |
  Retention$InRiskSetBargained == 1L | Retention$InRiskSetVeteranMarket == 1L |
  Retention$InRiskSetNonRookie == 1L
stopifnot(!anyDuplicated(Dictionary$variable), all(Dictionary$variable %in% names(Retention)),
          all(vapply(Dictionary$variable, function(v) is.numeric(Retention[[v]]), logical(1))),
          all(vapply(Dictionary$variable, function(v) !anyNA(Retention[[v]][InAnyRiskSet]), logical(1))))

# ---------------------------------------------------------------------------
# Labels
# ---------------------------------------------------------------------------

week_note <- " (one row per person-week; a player listed by two franchises in a week counts once, by the most employed status)"
game_note <- " in game weeks (weeks in which the listing franchise played a REG game; comparable across eras, bye weeks excluded)"
Labels18 <- c(
  gsis_id = "NFL GSIS player id (key)",
  season = "NFL season (key); REG season",
  display_name = PanelLabels[["display_name"]],
  PositionGroup = PanelLabels[["PositionGroup"]],
  PrimaryFranchise = PanelLabels[["PrimaryFranchise"]],
  Age = PanelLabels[["Age"]], RookieSeason = PanelLabels[["RookieSeason"]],
  Experience = PanelLabels[["Experience"]], IsRookie = PanelLabels[["IsRookie"]],
  DraftYear = PanelLabels[["DraftYear"]],
  DraftRound = "NFL draft round; 0 = undrafted (for draft-round FE)",
  DraftPick = PanelLabels[["DraftPick"]], Undrafted = PanelLabels[["Undrafted"]],
  FirstRosterSeason = PanelLabels[["FirstRosterSeason"]],
  InNflPlayers = PanelLabels[["InNflPlayers"]], InSampleTminus1 = PanelLabels[["InSampleTminus1"]],
  NFranchises = PanelLabels[["NFranchises"]],
  WeeksObserved = paste0("Distinct REG weeks with any weekly-roster row, any status", week_note, "; includes bye weeks before 2016 only"),
  WeeksObservedGame = paste0("Distinct REG weeks with any weekly-roster row", game_note),
  WeeksUnderContract = "Weeks under contract to a franchise (status class Active, Inactive, Reserve or SuspendedExempt), all roster weeks (bye weeks included before 2016 only; use WeeksUnderContractGame across eras)",
  WeeksUnderContractGame = paste0("Weeks under contract to a franchise (Active, Inactive, Reserve, SuspendedExempt)", game_note, "; the within-season employment duration"),
  WeeksActive = "Weeks with status class Active (ACT without a game-day inactive abbreviation; 2016-2020 rows without an abbreviation count as Active)",
  WeeksInactive = "Weeks with status class Inactive (INA, or ACT with abbreviation I*)",
  WeeksGameDayGame = paste0("Weeks Active or Inactive (game-day roster)", game_note),
  WeeksReserve = "Weeks on a reserve list (RES incl. injured reserve, PUP, NWT, RSN, RSR)",
  WeeksSuspendedExempt = "Weeks suspended or on the exempt list (SUS, EXE, E01, E14); under contract",
  WeeksPracticeSquad = "Weeks on the practice squad (DEV), all roster weeks; observable essentially from 2016",
  WeeksPracticeSquadGame = paste0("Weeks on the practice squad (DEV)", game_note),
  WeeksEmployedGame = paste0("Weeks under contract or on the practice squad", game_note),
  WeeksCut = "Weeks listed with status CUT (not employed that week; 2016-2020 rosters list released players for many weeks)",
  WeeksRetired = "Weeks listed with status RET (not employed)",
  WeeksFreeAgent = "Weeks listed with status UFA, RFA or UDF (free agent; not employed)",
  WeeksTradePending = "Weeks listed with status TRC, TRD or TRT (trade pending; neither counted as employed nor as not employed)",
  WeeksUnverified2016 = "2016 weeks 1-2 under-contract listings without a snap-count row that week and without an under-contract week >= 3 in 2016 (preseason rosters leak into the 2016 source; neither employed nor not employed)",
  WeeksUnknownStatus = "Weeks with a missing roster status",
  WeeksMultiFranchise = "Weeks in which two franchises list the player (counted once)",
  FirstWeekUnderContract = "First calendar week under contract (NA if never)",
  LastWeekUnderContract = "Last calendar week under contract (NA if never)",
  UnderContractWeek1 = "1 if under contract in week 1",
  UnderContractFinalWeek = "1 if under contract in the season's final calendar week (17 through 2020, 18 from 2021); end-of-season employment status",
  EmployedFinalWeek = "1 if under contract or on the practice squad in the final calendar week",
  NSpellsUnderContract = "Number of under-contract spells in the season (a spell breaks at a gap of one or more non-bye weeks or a change of franchise); inflated in 2016, whose week-1 listing is a preseason snapshot and whose reserve listings skip weeks",
  NFranchisesUnderContract = "Number of franchises the player was under contract to in the season",
  NFranchisesAnyRow = "Number of franchises with any weekly-roster row for the player in the season",
  EndFranchiseUnderContract = "Franchise of the last week under contract in t (NA if never under contract); when two franchises list the player under contract in that week this is a tie-break (rows in season, then franchise id), see EndFranchiseTied",
  EndFranchiseTied = "1 if two franchises list the player under contract in his last under-contract week of t (EndFranchiseUnderContract is then a tie-break, not a verified employer); NA if never under contract",
  RetentionEmployer = "Season-t employer for the employer-FE retention comparison (19, Table 34c): EndFranchiseUnderContract when EndFranchiseTied == 0, else NA (known pre-t+1 employer; measured from season-t rosters only, never from t+1)",
  UnderContractFranchises = "';'-joined franchises the player was under contract to in the season (NA if none)",
  SeasonGameWeeks = "REG game weeks per franchise in the season (modal: 16 through 2020, 17 from 2021; BUF/CIN 2022 played 16)",
  SeasonCalendarWeeks = "REG calendar weeks in the season (17 through 2020, 18 from 2021)",
  ShareSeasonUnderContract = "min(1, WeeksUnderContractGame / SeasonGameWeeks); a player who changes franchise across different bye weeks can reach SeasonGameWeeks + 1 game weeks",
  EmploymentClass = "UnderContract (>= 1 under-contract game week), PracticeSquadOnly (practice squad but never under contract), UnderContractByeOnly (under contract only in a bye week; pre-2016 artefact), AmbiguousOnly (only trade-pending, unknown-status or unverified-2016 listings: no evidence either way), NoEmploymentWeek (only cut, retired or free-agent listings)",
  WeeksAmbiguous = "WeeksTradePending + WeeksUnverified2016 + WeeksUnknownStatus",
  WeeksNotEmployed = "WeeksCut + WeeksRetired + WeeksFreeAgent",
  PracticeSquadOnly = "1 if EmploymentClass is PracticeSquadOnly",
  MultipleSpells = "1 if NSpellsUnderContract > 1 (released and re-signed, or changed franchise, during the season)",
  GamesPlayed = PanelLabels[["GamesPlayed"]], GamesPlayedSnapBased = PanelLabels[["GamesPlayedSnapBased"]],
  GamesStartedDepth = PanelLabels[["GamesStartedDepth"]], GamesStartedSnaps = PanelLabels[["GamesStartedSnaps"]],
  OffSnaps = PanelLabels[["OffSnaps"]], DefSnaps = PanelLabels[["DefSnaps"]], STSnaps = PanelLabels[["STSnaps"]],
  InjuryWeeks = PanelLabels[["InjuryWeeks"]], HasStatRow = PanelLabels[["HasStatRow"]],
  HasPay = "1 if the player has a realized OverTheCap cap-table year in the season (descriptive; roster weeks are not paid weeks and cap-table coverage is incomplete before 2013)",
  CapNumber = "Cap number, $ millions (OTC, realized year): an annual accounting charge, not a wage rate; descriptive only, never divide by roster weeks",
  CashPaid = "Cash paid, $ millions (OTC, realized year): an annual total conditional on a cap-table row, not a wage rate; descriptive only, never divide by roster weeks",
  CapPercent = PanelLabels[["CapPercent"]],
  GoverningContractId = PanelLabels[["GoverningContractId"]],
  GoverningContractType = PanelLabels[["GoverningContractType"]],
  OnRookieContract = PanelLabels[["OnRookieContract"]],
  HasCollegeLink = PanelLabels[["HasCollegeLink"]], HasRecruit = PanelLabels[["HasRecruit"]],
  CombineInvite = PanelLabels[["CombineInvite"]], AthleticScoreN = PanelLabels[["AthleticScoreN"]],
  NextSeasonWeeksUnderContractGame = "Weeks under contract in game weeks of season t+1 (0 if not on a t+1 roster; NA for t = 2025, right-censored)",
  NextSeasonWeeksGameDayGame = "Weeks Active or Inactive in game weeks of season t+1 (0 if none; NA for t = 2025)",
  NextSeasonWeeksEmployedGame = "Weeks under contract or on the practice squad in game weeks of t+1 (0 if none; NA for t = 2025)",
  NextSeasonWeeksPracticeSquad = "Practice-squad weeks in t+1 (0 if none; NA for t = 2025)",
  NextSeasonUnderContractFinalWeek = "1 if under contract in the final calendar week of t+1 (NA for t = 2025)",
  NextSeasonUnderContractFranchises = "';'-joined franchises under contract to in t+1 (NA if none or t = 2025)",
  NextSeasonAnyRosterRow = "1 if any weekly-roster row (any status) in t+1 (NA for t = 2025)",
  RetentionCensored = "1 if t = 2025 (t+1 rosters not in the data; right-censored)",
  NextSeasonAmbiguousOnly = "1 if the player's only t+1 evidence is ambiguous (EmploymentClass AmbiguousOnly in t+1); the retention outcomes are then NA, not 0; NA if censored",
  RetentionObserved = "1 if the t+1 outcome is determined: t <= 2024 and the t+1 evidence is not ambiguous only. No t+1 listing at all counts as exit (the rosters list every employed player)",
  RetainedNextSeason = "Main retention outcome: 1 if under contract to any franchise in at least one game week of t+1; 0 if not listed or listed only as cut, retired, free agent or practice squad; NA if censored (t = 2025) or if the only t+1 evidence is ambiguous. 1 - RetainedNextSeason is the discrete-time exit hazard after season t",
  RetainedNextSeasonGameDay = "1 if Active or Inactive in at least one game week of t+1 (NA if censored)",
  RetainedNextSeasonEmployed = "1 if under contract or on the practice squad in at least one game week of t+1 (practice squad observable from 2016; NA if censored)",
  RetainedNextSeasonFinalWeek = "1 if under contract in the final calendar week of t+1 (NA if censored)",
  RetainedNextSeasonSameFranchise = "1 if under contract in t+1 to a franchise the player was under contract to in t; 0 if not retained or only by other franchises; NA if censored or never under contract in t",
  RetainedNextSeasonNewFranchiseOnly = "Among seasons retained in t+1: 1 if retained only by franchises other than those of t; NA otherwise",
  NextSeasonSourceBreak = "1 if t = 2015: the 2016 rosters leak preseason listings, so the t+1 evidence is unreliable in both directions; excluded from InRiskSetRetention (see InRiskSetRetentionIncl2015)",
  RetainedNextSeasonAmbiguousAsExit = "RetainedNextSeason with ambiguous-only t+1 evidence counted as exit (0) instead of unknown; the other bound for the 2015 -> 2016 transition",
  CareerSeasonsUnderContract = "Panel seasons before t with at least one under-contract game week",
  CareerWeeksUnderContractGame = "Sum of WeeksUnderContractGame over panel seasons before t",
  CareerPanelSeasons = "Number of 2002-2025 REG roster seasons before t",
  LastRosterSeason = "Last 2002-2025 REG roster season of the player (right-censored at 2025)",
  CareerLeftCensored = "1 if RookieSeason < 2002 (career seasons before the panel start are not observed)",
  CareerRightCensored = "1 if LastRosterSeason == 2025 (the career may continue beyond the data)",
  ObservedRosterSeasons = "LastRosterSeason - FirstRosterSeason + 1 (calendar span of the observed roster career; censored at both ends per the flags)",
  FirstBargainedYear = "Year signed of the player's first OBSERVED UFA or Extension contract (OTC type, clean link; NA if none observed through the 2026 scrape; OTC entry-contract coverage is dense for drafted 2011+ entrants and thin for undrafted entrants before 2017, see EntryContractObserved; an OTC page can still omit a past deal)",
  FirstBargainedType = "OTC type of the first bargained contract (UFA or Extension)",
  FirstBargainedFranchise = "Signing franchise of the first bargained contract (05 SigningFranchise)",
  FirstBargainedNewTeam = "1 if the first bargained contract is with a new team (05 NewTeam; NA if not on a roster the season before)",
  FirstBargainedAPY = "APY of the first bargained contract, $ millions (descriptive)",
  FirstBargainedYears = "Length of the first bargained contract, years",
  FirstVeteranMarketYear = "Year signed of the first observed UFA, Extension, Franchise, Transition, RFA or ERFA contract (NA if none)",
  FirstVeteranMarketType = "OTC type of the first veteran-market contract",
  FirstNonRookieYear = "Year signed of the first observed contract of any type other than Drafted/UDFA/Practice (adds SFA and Other; NA if none)",
  FirstNonRookieType = "OTC type of the first non-rookie contract",
  HasAnyContractRow = "1 if the player has any linked OTC contract row (clean link, signing year known, type known); 0 = no OTC contract history observed, so no bargained contract can be observed",
  EntryContractObserved = "1 if an OTC Drafted or UDFA contract row is observed for the player (contract-history coverage indicator)",
  BargainedBefore = "1 if a UFA/Extension contract was signed in a year <= t (no longer at risk of the first one)",
  BargainedAtRisk = "1 if at risk of a first observed UFA/Extension in year t+1: none observed in a year <= t, RookieSeason >= 2011, season >= 2013, Experience >= 0, t+1 <= 2026",
  BargainedEventNext = "1 if the first UFA/Extension contract is signed in year t+1; 0 if not; NA if already bargained or t+1 beyond the contract data",
  VeteranMarketBefore = "As BargainedBefore for the veteran market (adds tags and tenders)",
  VeteranMarketAtRisk = "As BargainedAtRisk for the veteran market",
  VeteranMarketEventNext = "As BargainedEventNext for the veteran market",
  NonRookieBefore = "As BargainedBefore for any non-rookie contract (adds SFA and Other)",
  NonRookieAtRisk = "As BargainedAtRisk for any non-rookie contract",
  NonRookieEventNext = "As BargainedEventNext for any non-rookie contract",
  AccessWindowPartial = "1 if t+1 = 2026: signings observed through the OTC scrape only (in-season 2026 extensions incomplete); excluded from the InRiskSet* access flags, kept in the raw AtRisk/EventNext columns",
  AccessCohortObservable = "1 if RookieSeason >= 2011 and season >= 2013 (OTC lists entry contracts for 97-100% of drafted entrants from 2011; dense coverage, not proof of complete histories)",
  YearsToFirstBargained = "FirstBargainedYear - season when the first bargained contract comes after t (descriptive; NA otherwise)",
  ExperienceBin = "Experience bin: 0, 1, 2, 3, 4-6, 7+ (CBA minimum-schedule steps, as in 10)",
  AgeSq = "Age squared",
  LogDraftPick = "log(DraftPick); 0 for undrafted players (Undrafted absorbs their level)",
  NoCareerSeasons = "1 if no 2002-2025 panel season before t (Career* sums set to 0)",
  CareerGames = "Sum of REG games played over panel seasons < t (pre-2013 OL undercounted)",
  CareerStartsDepth = "Sum of depth-chart starts over panel seasons < t",
  CareerInjuryWeeks = "Injury weeks (Out/Doubtful or RES/PUP) over panel seasons < t",
  CareerOffDefSnaps = "Offensive + defensive snaps over 2013+ panel seasons < t (NA if none)",
  CareerSTSnaps = "Special-teams snaps over 2013+ panel seasons < t (NA if none)",
  InRiskSetRetention = "1 if in the main retention risk set: >= 1 under-contract game week in t, position group known, Experience 0-15, t+1 outcome determined (not censored, not ambiguous only) and t != 2015 (source break)",
  InRiskSetRetentionIncl2015 = "Retention risk set before the t+1 determinacy and source-break exclusions: >= 1 under-contract game week in t, position group known, Experience 0-15, t <= 2024 (for the bounding sensitivities)",
  InRiskSetEmployed = "1 if in the employed risk set (adds practice-squad-only seasons): >= 1 employed game week in t, season >= 2016, position group known, Experience 0-15, t+1 observed",
  InRiskSetBargained = "1 if in the first-UFA/Extension risk set: BargainedAtRisk, AccessWindowPartial == 0 (t <= 2024), >= 1 under-contract game week in t, position group known, Experience <= 15",
  InRiskSetVeteranMarket = "As InRiskSetBargained for the veteran market",
  InRiskSetNonRookie = "As InRiskSetBargained for any non-rookie contract",
  HasWikiArticle = "1 if the player has a Wikipedia article with category signals (wiki_cat_black not NA)"
)
# Season-t production/usage/signal controls carry the panel label; career
# sums of production stems and Miss indicators get built labels
PanelCopy <- setdiff(need_panel, names(Labels18))
Labels18 <- c(Labels18, setNames(paste0("Season t (player_season): ", PanelLabels[PanelCopy]), PanelCopy))
Labels18 <- c(Labels18, setNames(paste0("Sum over panel seasons < t (NA if none; seasons where the measure is NA add 0) of: ",
                                        PanelLabels[CareerStems]), paste0("Career", CareerStems)))
fill_note <- "; NA set to 0 by fill_missing() (see the Miss indicator or NoCareerSeasons)"
filled <- c(blockA, blockB, blockC, blockD, blockE, blockF, blockG)
Labels18[filled] <- paste0(Labels18[filled], fill_note)
AllMiss <- c(MissA, MissB, MissC, MissD, MissE, MissF, MissG)
Labels18 <- c(Labels18, setNames(paste0("1 if ", sub("Miss$", "", AllMiss),
                                        " was missing (the variable is set to 0 then)"), AllMiss))
RaceLabels18 <- c(
  race = "Hand-coded race (load_person_race; NA until coded)",
  hispanic = "Hand-coded Hispanic (yes/no/unknown; NA until coded)",
  black_any = "Hand-coded Black alone or in combination (0/1; NA until coded)",
  nonwhite = "Hand-coded nonwhite (0/1; NA until coded)",
  race_source = "Source of the hand code (coder_agree, single_coder, disputed, adjudicated)",
  black_provisional = "Hand-coded black_any when coded, else 1 if a Wikipedia category flags Black, else NA (positive-only lower bound)",
  black_provisional_source = "Source of black_provisional",
  wiki_cat_black = "Wikipedia category flags the player as Black (NA without an article; 0 is not evidence of race)",
  wiki_cat_hispanic_latino = "Wikipedia category flags the player as Hispanic/Latino (NA without an article)",
  wiki_cat_asian = "Wikipedia category flags the player as Asian (NA without an article)",
  wiki_cat_pacific_islander = "Wikipedia category flags the player as Pacific Islander (NA without an article)",
  wiki_cat_native_american = "Wikipedia category flags the player as Native American (NA without an article)",
  p_black_bifsg = "BIFSG posterior P(Black) (name-based; descriptive only)",
  p_white_pred = "Predicted P(white, non-Hispanic) (race_predicted, model-only)",
  p_black_pred = "Predicted P(Black alone, non-Hispanic) (race_predicted, model-only)",
  p_hispanic_pred = "Predicted P(Hispanic) (race_predicted, model-only)",
  p_api_pred = "Predicted P(Asian or Pacific Islander) (race_predicted, model-only)",
  p_aian_pred = "Predicted P(American Indian or Alaska Native) (race_predicted, model-only)",
  p_multi_pred = "Predicted P(multiracial) (race_predicted, model-only)",
  p_black_any_pred = "Predicted P(non-Hispanic Black alone) (equals p_black_pred despite the name; model-only); primary race regressor under regression calibration with the pred_* covariates",
  prior_black_pred = "NFL prior P(Black) from the EM model on the predetermined covariates",
  pred_method = "Method of the model-only prediction",
  p_white_preddoc = "Sensitivity variant (preddoc): P(white); one-hot where documented, else the model prediction (fame-dependent)",
  p_black_any_preddoc = "Sensitivity variant (preddoc): 1/0 where a public source documents race, else the model P(non-Hispanic Black alone)",
  p_hispanic_preddoc = "Sensitivity variant (preddoc): P(Hispanic)",
  p_api_preddoc = "Sensitivity variant (preddoc): P(Asian or Pacific Islander)",
  p_aian_preddoc = "Sensitivity variant (preddoc): P(American Indian or Alaska Native)",
  p_multi_preddoc = "Sensitivity variant (preddoc): P(multiracial)",
  pred_method_preddoc = "Method of the preddoc value",
  documented_race = "Race stated by a public source (validation only; NA if undocumented)",
  documented_black_any = "1 if documented Black alone or in combination, 0 if documented otherwise, NA if undocumented (validation only)",
  pred_pos_group = "Prior covariate of the predicted race: position at NFL entry (character FE; NA -> \"unknown\")",
  pred_rookie_era = "Prior covariate: rookie-season era (character FE; NA -> \"unknown\")",
  pred_draft_bucket = "Prior covariate: draft-round bucket (character FE; NA -> \"unknown\")",
  pred_college_type = "Prior covariate: college type (character FE; NA -> \"unknown\")",
  pred_county_available = "Prior covariate: home-county likelihood available (character FE; NA -> \"unknown\")",
  pred_has_wiki = "Prior covariate of the preddoc variant only: Wikipedia article found (post-treatment: fame)",
  pred_career_bucket = "Prior covariate of the preddoc variant only: career-length bucket (post-treatment: career length is an outcome here)",
  p_white_pred_raked = "Predicted P(white), TIDES-raked variant",
  p_black_pred_raked = "Predicted P(non-Hispanic Black alone), TIDES-raked variant",
  p_white_pred_nodraft = "Predicted P(white), draft-free variant (prior without the draft-round bucket)",
  p_black_pred_nodraft = "Predicted P(non-Hispanic Black alone), draft-free variant",
  p_black_or_multi_pred = "Predicted P(non-Hispanic Black alone) + P(multiracial)"
)
Labels18 <- c(Labels18, RaceLabels18)
Labels18 <- Labels18[!duplicated(names(Labels18), fromLast = TRUE)]
Labels18 <- setNames(as.character(Labels18), names(Labels18))

# ---------------------------------------------------------------------------
# Write
# ---------------------------------------------------------------------------

setcolorder(Retention, intersect(names(Labels18), names(Retention)))
setorder(Retention, gsis_id, season)
Dictionary[, label := unname(Labels18[variable])]
stopifnot(!anyNA(Dictionary$label))
write.csv(Dictionary, file.path(analysis, "player_retention_control_blocks.csv"), row.names = FALSE)
msg18("player_retention_control_blocks.csv: %d controls", nrow(Dictionary))

RetentionOut <- as.data.frame(Retention)
write_sample(RetentionOut, "analysis_player_retention", key = c("gsis_id", "season"),
             labels = Labels18[names(Labels18) %in% names(RetentionOut)])

# ---------------------------------------------------------------------------
# Coverage: by season and by status class
# ---------------------------------------------------------------------------

StatusWeeks <- dcast(StatusWeeks, season + StatusClass ~ GameWeek, value.var = "NRows", fill = 0L)
for (g in c("0", "1")) if (!g %in% names(StatusWeeks)) StatusWeeks[, (g) := 0L]
setnames(StatusWeeks, c("0", "1"), c("RowsByeOrNoGame", "RowsGameWeek"))
PW <- PersonWeek[, .(PersonWeeks = .N, PersonWeeksGame = sum(GameWeek)), by = .(season, StatusClass)]
StatusWeeks <- merge(StatusWeeks, PW, by = c("season", "StatusClass"), all = TRUE)
StatusWeeks[, UnderContract := as.integer(StatusClass %in% UnderContractClasses)]
setorder(StatusWeeks, season, StatusClass)
write.csv(StatusWeeks, file.path(analysis, "player_retention_status_weeks.csv"), row.names = FALSE)

share <- function(x) round(mean(x, na.rm = TRUE), 3)
Coverage <- Retention[, .(
  N = .N,
  NUnderContract = sum(EmploymentClass == "UnderContract"),
  NPracticeSquadOnly = sum(EmploymentClass == "PracticeSquadOnly"),
  NAmbiguousOnly = sum(EmploymentClass == "AmbiguousOnly"),
  NNoEmploymentWeek = sum(EmploymentClass == "NoEmploymentWeek"),
  NNextSeasonAmbiguousOnly = sum(NextSeasonAmbiguousOnly %in% 1L),
  MeanWeeksUnderContractGame = round(mean(WeeksUnderContractGame[EmploymentClass == "UnderContract"]), 2),
  ShareFinalWeek = share(UnderContractFinalWeek[EmploymentClass == "UnderContract"]),
  ShareMultipleSpells = share(MultipleSpells[EmploymentClass == "UnderContract"]),
  NRiskRetention = sum(InRiskSetRetention),
  RetainedNextSeason = share(RetainedNextSeason[InRiskSetRetention == 1L]),
  RetainedGameDay = share(RetainedNextSeasonGameDay[InRiskSetRetention == 1L]),
  RetainedSameFranchise = share(RetainedNextSeasonSameFranchise[InRiskSetRetention == 1L]),
  RetainedEmployedPSOnly = share(RetainedNextSeasonEmployed[InRiskSetEmployed == 1L & PracticeSquadOnly == 1L]),
  NRiskBargained = sum(InRiskSetBargained),
  BargainedEventNext = share(BargainedEventNext[InRiskSetBargained == 1L]),
  NRiskVeteranMarket = sum(InRiskSetVeteranMarket),
  VeteranMarketEventNext = share(VeteranMarketEventNext[InRiskSetVeteranMarket == 1L]),
  NRiskNonRookie = sum(InRiskSetNonRookie),
  NonRookieEventNext = share(NonRookieEventNext[InRiskSetNonRookie == 1L]),
  HasPayUnderContract = share(HasPay[EmploymentClass == "UnderContract"]),
  HasPayPracticeSquadOnly = share(HasPay[EmploymentClass == "PracticeSquadOnly"]),
  HasPayNoEmploymentWeek = share(HasPay[EmploymentClass == "NoEmploymentWeek"]),
  MeanCashPaidUnderContract = round(mean(CashPaid[EmploymentClass == "UnderContract" & HasPay == 1L], na.rm = TRUE), 3),
  PredictedRace = share(!is.na(p_black_any_pred)),
  HandCoded = share(!is.na(black_any)),
  WikiArticle = share(HasWikiArticle)), by = season][order(season)]
write.csv(Coverage, file.path(analysis, "coverage_player_retention.csv"), row.names = FALSE)
options(width = 220)
print(Coverage, nrows = 40)

# Contract-history coverage by rookie cohort and draft status: the evidence
# behind the 2011 cohort start of the access risk set (players in the panel
# with an nfl_players row; one row per player)
ContractCoverage <- Retention[InNflPlayers == 1L & !is.na(RookieSeason) & RookieSeason >= 2002L,
                              .SD[1L], by = gsis_id,
                              .SDcols = c("RookieSeason", "Undrafted", "HasAnyContractRow",
                                          "EntryContractObserved", "FirstBargainedYear")]
ContractCoverage <- ContractCoverage[, .(
  NPlayers = .N,
  ShareAnyContractRow = share(HasAnyContractRow),
  ShareEntryContractObserved = share(EntryContractObserved),
  ShareFirstBargainedObserved = share(!is.na(FirstBargainedYear))),
  by = .(RookieSeason, Undrafted)][order(RookieSeason, Undrafted)]
write.csv(ContractCoverage, file.path(analysis, "coverage_player_contract_history.csv"),
          row.names = FALSE)
print(dcast(ContractCoverage, RookieSeason ~ Undrafted,
            value.var = c("NPlayers", "ShareAnyContractRow", "ShareEntryContractObserved")),
      nrows = 40)

# Retention and access by experience in the risk sets (console diagnostics)
print(Retention[InRiskSetRetention == 1L, .(N = .N, Retained = share(RetainedNextSeason),
                                            GameDay = share(RetainedNextSeasonGameDay),
                                            SameFranchise = share(RetainedNextSeasonSameFranchise)),
                by = Experience][order(Experience)], nrows = 20)
print(Retention[InRiskSetBargained == 1L, .(N = .N, EventNext = share(BargainedEventNext)),
                by = Experience][order(Experience)], nrows = 20)

# Invariants
stopifnot(
  all(Retention$RetainedNextSeason[Retention$RetentionObserved == 1L] %in% 0:1),
  all(is.na(Retention$RetainedNextSeason[Retention$RetentionObserved == 0L])),
  all(is.na(Retention$RetainedNextSeason[Retention$NextSeasonAmbiguousOnly %in% 1L])),
  all(Retention$RetentionObserved[Retention$season == MaxRosterSeason] == 0L),
  all(Retention$InRiskSetRetention[Retention$RetentionObserved == 0L] == 0L),
  !any(Retention$EmploymentClass == "NoEmploymentWeek" & Retention$WeeksNotEmployed == 0L),
  all(Retention$BargainedEventNext[Retention$InRiskSetBargained == 1L] %in% 0:1),
  all(Retention$BargainedBefore[Retention$InRiskSetBargained == 1L] == 0L),
  all(Retention$season[Retention$InRiskSetBargained == 1L] <= MaxContractYear - 2L),
  all(Retention$AccessWindowPartial[Retention$InRiskSetNonRookie == 1L] == 0L),
  all(Retention$WeeksUnderContractGame[Retention$InRiskSetRetention == 1L] > 0L),
  all(Retention$WeeksGameDayGame <= Retention$WeeksUnderContractGame),
  all(Retention$WeeksUnderContractGame <= Retention$WeeksUnderContract),
  # Season-t employer: only from season-t under-contract listings (never
  # t+1) and never a tie-broken franchise. Coverage is reported, not imposed.
  all(is.na(Retention$RetentionEmployer[is.na(Retention$EndFranchiseUnderContract)])),
  all(is.na(Retention$RetentionEmployer[Retention$EndFranchiseTied %in% 1L])),
  all(mapply(function(e, f) e %in% strsplit(f, ";", fixed = TRUE)[[1]],
             Retention$RetentionEmployer[!is.na(Retention$RetentionEmployer)],
             Retention$UnderContractFranchises[!is.na(Retention$RetentionEmployer)])))
msg18("done at %ds", round(as.numeric(difftime(Sys.time(), T0Script18, units = "secs"))))
rm(PersonWeek, Season, PlayerSeason, Extra, Next, Contracts, PlayerRace, PredExtra, RetentionOut)

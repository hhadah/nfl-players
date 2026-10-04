#!/usr/bin/env Rscript
# Verify temporal boundaries after rebuilding samples, before estimation in
# 95 (or run standalone). Fails on look-ahead, broken incumbent tenure,
# cap lags, policy timing, or employment censoring.
# Results -> a PASS line; no data are modified.

if (!exists("root", inherits = TRUE)) {
  root <- Sys.getenv("NFL_PLAYERS_ROOT", unset = getwd())
}
if (!exists("analysis", inherits = TRUE)) {
  analysis <- file.path(root, "data", "datasets", "analysis")
}
source(file.path(root, "programs", "00-policy-functions.R"), local = TRUE)
source(file.path(root, "programs", "00-analysis-functions.R"), local = TRUE)

assert_analysis <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
read_analysis <- function(name, columns = NULL) {
  file <- file.path(analysis, paste0(name, ".parquet"))
  if (is.null(columns)) return(as.data.frame(arrow::read_parquet(file)))
  as.data.frame(arrow::read_parquet(file, col_select = tidyselect::all_of(columns)))
}
unique_analysis <- function(x, key, name) {
  assert_analysis(all(key %in% names(x)) &&
                    all(complete.cases(x[key])) && !anyDuplicated(x[key]),
                  sprintf("%s has missing or duplicate keys.", name))
}

# The 2025 sunset applies to the mandate and subsidy, not interview eligibility.
policy_years <- c(2002L, 2003L, 2020L, 2021L, 2022L, 2024L, 2025L)
policy <- add_rooney_policies(data.frame(season = policy_years))
assert_analysis(identical(policy$RooneyRule, c(0L, 1L, 1L, 1L, 1L, 1L, 1L)),
                "The original Rooney Rule starts in the 2003 cycle.")
assert_analysis(identical(policy$OffensiveAssistantMandate,
                          c(0L, 0L, 0L, 0L, 1L, 1L, 0L)) &&
                  identical(policy$OffensiveAssistantSubsidy,
                            policy$OffensiveAssistantMandate),
                "The assistant mandate and subsidy must cover 2022-2024 only.")
assert_analysis(policy$RooneyAmend2022[7] == 1L &&
                  policy$OffensiveAssistantVoluntary2025[7] == 1L &&
                  policy$RooneyCompensatory2021[3] == 0L &&
                  policy$RooneyCompensatory2021[4] == 1L,
                "Interview, voluntary and compensatory provisions are distinct.")
hiring_policy <- add_rooney_policies(data.frame(season = 2022:2023),
                                     timing = "offseason_hire")
assert_analysis(identical(hiring_policy$RooneyAmend2022, c(0L, 1L)),
                "March 2022 changes follow the main 2022 HC hiring cycle.")

# Rank deficiency must not fabricate unsupported race-specific contrasts or
# discard estimable contrasts merely because individual coefficients alias.
angle <- seq(0, 4 * pi, length.out = 40)
X <- cbind(1, seq(-1, 1, length.out = 40))
p <- 0.3 + 0.15 * sin(angle)
po <- 0.1 + 0.03 * cos(angle)
true_gap <- drop(X %*% c(-0.3, 0.7))
y <- drop(X %*% c(2, 0.5)) + p * true_gap + po * drop(X %*% c(0.4, -0.2))
aliased <- race_contrast_fit(cbind(X, X[, 2], 0), p, po, y)
assert_analysis(all(aliased$estimable) && max(abs(aliased$gap - true_gap)) < 1e-7,
                "Aliased or absent controls changed an identified race contrast.")
scaled <- race_contrast_fit(sweep(X, 2, c(1e-5, 1e5), "*"), p, po, y)
assert_analysis(all(scaled$estimable) && max(abs(scaled$gap - true_gap)) < 1e-7,
                "Race contrasts depend on control units.")
sparse <- race_contrast_fit(cbind(X, as.integer(seq_len(40) == 1L)), p, po, y)
assert_analysis(is.na(sparse$gap[1]) && !sparse$estimable[1] &&
                  all(sparse$estimable[-1]) &&
                  max(abs(sparse$gap[-1] - true_gap[-1])) < 1e-7,
                "An unsupported race contrast was imputed or supported rows were lost.")

opening <- read_analysis("staff_person_opening_season")
team <- read_analysis("team_season")
roster <- read_analysis("roster_composition_team_season")
game <- read_analysis("team_game")
unique_analysis(opening, c("franchise_id", "season", "person_id"),
                "Opening staff")
unique_analysis(team, c("franchise_id", "season"), "Team season")
unique_analysis(roster, c("franchise_id", "season"), "Roster composition")
unique_analysis(game, c("franchise_id", "game_id"), "Team game")
timed_games <- game[game$season >= 2007L,
  c("gameday", "StaffSnapshotTargetDate", "StaffSnapshotRevisionTimestamp")]
assert_analysis(all(complete.cases(timed_games)),
                "Every template-era game needs dated snapshot provenance.")
target_midnight <- as.POSIXct(
  as.character(as.Date(timed_games$StaffSnapshotTargetDate)), tz = "UTC")
assert_analysis(all(timed_games$StaffSnapshotRevisionTimestamp <=
                      target_midnight) &&
                  all(as.Date(timed_games$StaffSnapshotTargetDate) <=
                        as.Date(timed_games$gameday)),
                "A game received staff information observed after its cutoff.")

opening_counts <- aggregate(
  list(ExpectedCoaches = as.integer(opening$IsCoach)),
  opening[c("franchise_id", "season")], sum)
opening_counts <- merge(opening_counts,
  team[c("franchise_id", "season", "NCoachesPre")],
  by = c("franchise_id", "season"), all.x = TRUE)
assert_analysis(nrow(opening_counts) == 608L &&
                  all(opening_counts$ExpectedCoaches == opening_counts$NCoachesPre),
                "Opening composition includes people outside the opening staff.")
assert_analysis(all(opening$season >= 2007L),
                "Retrospective staff boxes cannot become opening-day measures.")
first_opening <- team[team$season == 2007L,
  c("ShareCoachesNewToFranchisePre", "ShareCoachesPromotedPre")]
assert_analysis(nrow(first_opening) == 32L &&
                  all(is.na(as.matrix(first_opening))),
                "Opening turnover in 2007 has an unobserved prior opening.")

# Pagano's medical absence is not a new incumbent tenure. Caldwell's departure is.
ind <- team[team$franchise_id == "IND" & team$season %in% 2011:2014,
            c("season", "HCIncumbentSpellId")]
assert_analysis(nrow(ind) == 4L && all(!is.na(ind$HCIncumbentSpellId)),
                "IND 2011-2014 needs observed incumbent spell keys.")
pagano_spells <- unique(ind$HCIncumbentSpellId[ind$season >= 2012L])
assert_analysis(length(pagano_spells) == 1L &&
                  ind$HCIncumbentSpellId[ind$season == 2011L] != pagano_spells,
                "Medical absence must not split tenure; actual hires must.")
no <- team[team$franchise_id == "NO" & team$season %in% 2011:2013,
           c("season", "HCIncumbentSpellId")]
assert_analysis(nrow(no) == 3L && !anyNA(no$HCIncumbentSpellId) &&
                  length(unique(no$HCIncumbentSpellId)) == 1L,
                "Payton's suspension must not create a new incumbent tenure.")

# The cap control must link to the same franchise in exactly the prior year.
lag_cap <- roster[c("franchise_id", "season", "TeamCapShare")]
lag_cap$season <- lag_cap$season + 1L
names(lag_cap)[3] <- "ExpectedLagCap"
cap_check <- merge(roster[c("franchise_id", "season", "L1TeamCapShare")],
                   lag_cap, by = c("franchise_id", "season"), all.x = TRUE)
known_cap <- !is.na(cap_check$L1TeamCapShare)
assert_analysis(all(!is.na(cap_check$ExpectedLagCap[known_cap])) &&
                  all(abs(cap_check$L1TeamCapShare[known_cap] -
                            cap_check$ExpectedLagCap[known_cap]) < 1e-10),
                "Predetermined cap control is not the consecutive-season lag.")

coach <- read_analysis("coach_policy_person_season")
units <- read_analysis("coach_policy_team_unit_season")
spells <- read_analysis("coach_job_spells")
hires <- read_analysis("analysis_rooney_hires")
unique_analysis(coach, c("franchise_id", "season", "person_id"), "Coach transitions")
unique_analysis(units, c("franchise_id", "season", "Unit"), "Coach unit cells")
unique_analysis(spells, "SpellId", "Coach job spells")
unique_analysis(hires, c("franchise_id", "season"), "Head-coach history")
for (horizon in 1:2) {
  observed <- coach[[sprintf("Next%sObserved", horizon)]]
  retained <- coach[[sprintf("RetainedNext%s", horizon)]]
  promoted <- coach[[sprintf("PromotedNext%s", horizon)]]
  assert_analysis(length(observed) == nrow(coach) &&
                    all(is.na(retained[observed == 0L])) &&
                    all(is.na(promoted[observed == 0L])),
                  sprintf("Coach t+%s outcomes conflate censoring with exit.", horizon))
  known <- observed == 1L
  assert_analysis(all(promoted[known] <= retained[known]),
                  sprintf("Coach t+%s promotion occurs without retention.", horizon))
}
assert_analysis(all(units$EligibleShareLower <= units$EligibleShareUpper) &&
                  all(units$EligibleShareLower >= 0) &&
                  all(units$EligibleShareUpper <= 1),
                "Documented eligibility bounds do not contain a valid share.")
dated <- !is.na(spells$ActualHireDate)
assert_analysis(any(dated) && all(spells$ActualHireDate[dated] <= spells$EntryUpperDate[dated]) &&
                  all(!is.na(spells$HireDateSourceUrl[dated])) &&
                  all(nzchar(spells$HireDateSourceUrl[dated])),
                "Sourced appointment dates must precede the first listing.")
assert_analysis(all(1990:1998 %in% hires$season),
                "The head-coach comparison lost its historical pre-rule years.")
permanent <- which(hires$RetainedPermanent == 1L & hires$HCNameUncertain == 0L)
assert_analysis(all(hires$IsHire[permanent] == 0L),
                "A retained permanent in-season appointee was counted as an offseason hire.")
unknown_status <- which(hires$RetainedUnknownStatus == 1L)
assert_analysis(all(is.na(hires$IsHire[unknown_status])),
                "A retained coach with undocumented appointment status was classified as a hire.")
returning_cle <- hires[hires$franchise_id == "CLE" & hires$season == 1999L, ]
assert_analysis(nrow(returning_cle) == 1L && is.na(returning_cle$IsHire),
                "Cleveland's re-entry is left-censored, not an observed coach change.")

player <- read_analysis("analysis_player_retention",
  c("gsis_id", "season", "InRiskSetRetention", "InRiskSetBargained",
    "RetentionObserved", "NextSeasonAmbiguousOnly", "NextSeasonSourceBreak",
    "AccessWindowPartial", "RetentionEmployer", "EndFranchiseUnderContract",
    "EndFranchiseTied", "UnderContractFranchises",
    "RetainedNextSeason", "RetainedNextSeasonGameDay",
    "RetainedNextSeasonSameFranchise", "RetainedNextSeasonFinalWeek"))
player_keys <- read_analysis("player_season", c("gsis_id", "season"))
unique_analysis(player, c("gsis_id", "season"), "Player employment panel")
key <- function(x) sort(paste(x$gsis_id, x$season, sep = "|"))
assert_analysis(identical(key(player), key(player_keys)),
                "Employment construction selected out part of the roster universe.")
unknown_employer <- is.na(player$EndFranchiseUnderContract) |
  player$EndFranchiseTied %in% 1L
assert_analysis(all(is.na(player$RetentionEmployer[unknown_employer])),
                "Unknown or tied season-t employers were assigned a franchise.")
known_employer <- which(!is.na(player$RetentionEmployer))
assert_analysis(all(vapply(known_employer, function(i) {
  player$RetentionEmployer[i] %in%
    strsplit(player$UnderContractFranchises[i], ";", fixed = TRUE)[[1L]]
}, logical(1))),
  "A retention employer is not in the player's season-t contract history.")
last_season <- player$season == max(player$season)
uncertain <- player$RetentionObserved == 0L
assert_analysis(all(is.na(player$RetainedNextSeason[uncertain])) &&
                  all(player$InRiskSetRetention[uncertain] == 0L) &&
                  all(player$InRiskSetRetention[player$NextSeasonSourceBreak == 1L] == 0L),
                "Unobserved or source-break retention entered the primary risk set.")
ambiguous_only <- which(player$NextSeasonAmbiguousOnly == 1L)
assert_analysis(all(player$RetentionObserved[ambiguous_only] == 0L),
                "Ambiguous-only future listings were treated as verified employment or exit.")
assert_analysis(all(player$InRiskSetBargained[player$AccessWindowPartial == 1L] == 0L),
                "Partial contract-signing windows entered the completed access risk set.")
assert_analysis(all(is.na(player$RetainedNextSeason[last_season])) &&
                  all(player$InRiskSetRetention[last_season] == 0L) &&
                  all(player$InRiskSetBargained[last_season] == 0L),
                "Incomplete next-year outcomes entered an employment risk set.")
for (outcome in c("RetainedNextSeasonGameDay", "RetainedNextSeasonSameFranchise",
                  "RetainedNextSeasonFinalWeek")) {
  assert_analysis(all(is.na(player[[outcome]][uncertain])),
                  sprintf("%s turns an unobserved outcome into zero.", outcome))
  both <- !is.na(player[[outcome]]) & !is.na(player$RetainedNextSeason)
  assert_analysis(all(player[[outcome]][both] <= player$RetainedNextSeason[both]),
                  sprintf("%s exceeds any-franchise under-contract retention.", outcome))
}

cat(sprintf(paste0("PASS: policy timing, %s dated team-games, %s opening staff ",
                   "observations, incumbent tenure, %s consecutive cap lags, ",
                   "coach transitions and %s player-season risk-set rows.\n"),
            nrow(timed_games), nrow(opening), sum(known_cap), nrow(player)))

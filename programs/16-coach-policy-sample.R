# ============================================================================
# 16-coach-policy-sample.R
# Coach job spells, coach person-season transitions, team-unit composition
# and head-coach hires for the Rooney-policy exhibits (17). Inputs: the
# opening-snapshot staff panel of 01 (staff_person_opening_season), the
# opening coverage list, the raw staff snapshots/entries (interval bounds),
# documented race (race_predicted, read-only DuckDB), Wikidata gender and
# program participation acquired by scripts/02e_coach_policy_reference.py
# (data/derived/coach_policy, ignored), the game head coaches of
# 00-setup-functions.R and the 1989-1998 head-coach history of 02d.
# Design rules:
#   - Dates: only observation bounds (Wikipedia snapshot revision/target
#     dates, REG game days, verified dismissal dates) are written; no hire or
#     departure date is imputed. Spells are interval censored.
#   - Eligibility follows the NFL's definitions (woman OR racial/ethnic
#     minority); it is 1 only with a documented source (Wikidata, Wikipedia
#     category/text, league program designation), 0 only when race AND gender
#     are both documented as white and male, NA otherwise. Predicted race
#     never creates eligibility.
#   - Head-coach "documented-positive vs all other" indicators are explicit
#     ascertainment comparisons (undocumented coaches are NOT labelled
#     white); they are benchmarked to TIDES in the coverage file.
#   - Promotion/retention outcomes at t+1/t+2 are right-censored when the
#     later opening snapshot is unobserved (2025, or after the last season).
# Base R + data.table (no tidyverse verbs). Sourced by 95-make-all.R after
# 00-policy-functions.R and 01/02.
# Results -> analysis/coach_job_spells.{parquet,csv}
#         -> analysis/coach_policy_person_season.{parquet,csv}
#         -> analysis/coach_policy_team_unit_season.{parquet,csv}
#         -> analysis/analysis_rooney_hires.{parquet,csv}
#         -> analysis/coach_policy_coverage.csv
# Date: October 3rd, 2026
# ============================================================================

library(data.table)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
as_int01 <- function(x) as.integer(as.logical(x))
norm_name16 <- function(x) {
  x <- gsub(",? (Jr\\.?|Sr\\.?|II|III|IV)$", "", x)
  x <- gsub("[^A-Za-z ]", "", gsub("\\.", "", x))
  tolower(trimws(gsub(" +", " ", x)))
}
need_cols <- function(df, cols, what) {
  miss <- setdiff(cols, names(df))
  assert_true(length(miss) == 0,
              sprintf("%s: missing columns %s", what, paste(miss, collapse = ", ")))
}

con <- db_connect()
derived_cp <- file.path(root, "data", "derived", "coach_policy")

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

opening_path <- file.path(analysis, "staff_person_opening_season.parquet")
coverage_path <- file.path(analysis, "staff_opening_coverage.csv")
assert_true(file.exists(opening_path),
            "16: analysis/staff_person_opening_season.parquet not found (run 01 first)")
assert_true(file.exists(coverage_path),
            "16: analysis/staff_opening_coverage.csv not found (run 01 first)")
Opening <- as.data.table(arrow::read_parquet(opening_path))
need_cols(Opening, c("franchise_id", "season", "person_id", "person_name", "role_std",
                     "RoleTier", "Domain", "Unit", "IsCoach", "IsHeadCoach", "IsInterimHC",
                     "IsCoordinator", "IsOC", "IsDC", "IsPositionCoach", "IsAssistantCoach",
                     "IsOffenseCoach", "IsDefenseCoach", "IsSpecialTeamsCoach",
                     "OpeningTargetDate", "OpeningRevisionTimestamp", "NewToFranchise",
                     "PromotedWithinFranchise", "PrevTeamSeasonObserved",
                     "SeasonsWithFranchise", "TenureLeftCensored", "p_black_any_pred",
                     "p_white_pred", "prior_black_pred", "pred_role_group_first",
                     "pred_unit_first", "pred_first_era"),
          "staff_person_opening_season")
Opening[, season := as.integer(season)]
OpeningCov <- as.data.table(read.csv(coverage_path, stringsAsFactors = FALSE))
need_cols(OpeningCov, c("franchise_id", "season", "OpeningStaffObserved"),
          "staff_opening_coverage")
OpeningCov[, `:=`(season = as.integer(season),
                  OpeningStaffObserved = as_int01(OpeningStaffObserved))]
assert_true(!anyDuplicated(OpeningCov[, .(franchise_id, season)]),
            "16: staff_opening_coverage is not unique by franchise-season")
MaxOpeningSeason <- max(OpeningCov$season[OpeningCov$OpeningStaffObserved == 1L])
message(sprintf("16: opening staff rows %d; observed team-seasons %d of %d (through %d)",
                nrow(Opening), sum(OpeningCov$OpeningStaffObserved), nrow(OpeningCov),
                MaxOpeningSeason))

# Documented race (read-only; Wikidata P172, Wikipedia categories and
# classified article text as assembled by 04d/04e). documented_race is the
# protocol category; documented_hispanic flags Hispanic ethnicity separately.
RaceDoc <- as.data.table(DBI::dbGetQuery(con, "
  SELECT entity_id AS person_id, documented_race, documented_black_any,
         documented_hispanic, documented_sources, documented_components
  FROM race_predicted WHERE entity = 'staff'"))
RaceDoc[, DocumentedRace := ifelse(is.na(documented_race), NA_character_, documented_race)]
RaceDoc[, DocumentedBlack := ifelse(is.na(documented_race), NA_integer_,
                                    as.integer(documented_black_any == 1L))]
# NFL eligibility: "racial or ethnic minority" = any documented non-white
# race component or documented Hispanic ethnicity. A documented multiracial
# person counts as a minority (every protocol multi label has a non-white
# component). White and not Hispanic -> 0.
RaceDoc[, DocumentedMinority := fifelse(is.na(documented_race), NA_integer_,
                                 fifelse(documented_race != "white" |
                                           (!is.na(documented_hispanic) & documented_hispanic == 1L),
                                         1L, 0L))]
RaceDoc[, DocumentedRaceSource := ifelse(is.na(documented_race), NA_character_,
                                         documented_sources)]
RaceDoc <- RaceDoc[, .(person_id, DocumentedRace, DocumentedBlack, DocumentedMinority,
                       DocumentedRaceSource)]
assert_true(!anyDuplicated(RaceDoc$person_id), "16: race_predicted staff rows not unique")

# Documented gender. (1) Wikidata P21 for persons with an item (02e);
# (2) Wikipedia categories that state the person is a woman (positive-only;
# an article without such a category says nothing). No gender is inferred
# from a first name.
gender_path <- file.path(derived_cp, "staff_gender_wikidata.csv")
participation_path <- file.path(derived_cp, "program_participation.csv")
assert_true(file.exists(gender_path) && file.exists(participation_path),
            "16: data/derived/coach_policy inputs missing (run scripts/02e_coach_policy_reference.py)")
Gender <- as.data.table(read.csv(gender_path, stringsAsFactors = FALSE, na.strings = ""))
Gender[, WomanWikidata := fifelse(is.na(gender_label), NA_integer_,
                           fifelse(gender_label == "female", 1L,
                             fifelse(gender_label == "male", 0L, NA_integer_)))]
WikiCats <- as.data.table(DBI::dbGetQuery(con, "
  SELECT person_id,
         len(list_filter(categories, x -> regexp_matches(x,
           '^(Female coaches of American football|American female sports coaches|Women American football executives|Women sports owners|American women sports executives|Female players of American football|American sportswomen|[0-9]+(st|th)-century American (sports)?women|[0-9]+(st|th)-century American businesswomen|American businesswomen|American female billionaires)'))) > 0
           AS WomanCategory
  FROM staff_person_wiki_signals"))
WikiCats[, WomanCategory := as.integer(WomanCategory)]
GenderAll <- merge(Gender[, .(person_id, WomanWikidata, wikidata_qid)],
                   WikiCats[WomanCategory == 1L], by = "person_id", all = TRUE)
GenderAll[, DocumentedWoman := fifelse(!is.na(WomanWikidata), WomanWikidata,
                                fifelse(!is.na(WomanCategory) & WomanCategory == 1L, 1L,
                                        NA_integer_))]
GenderAll[, WomanSource := fifelse(!is.na(WomanWikidata), paste0("wikidata_P21:", wikidata_qid),
                            fifelse(!is.na(DocumentedWoman), "wikipedia_category", NA_character_))]
assert_true(!any(!is.na(GenderAll$WomanWikidata) & GenderAll$WomanWikidata == 0L &
                   !is.na(GenderAll$WomanCategory) & GenderAll$WomanCategory == 1L),
            "16: Wikidata says male but a Wikipedia category says woman; inspect")
GenderAll <- GenderAll[, .(person_id, DocumentedWoman, WomanSource)]

# Program participation (02e): NFL Coach Accelerator lists (league-designated
# diverse candidates) and named offensive-assistant mandate participants.
Participation <- as.data.table(read.csv(participation_path, stringsAsFactors = FALSE,
                                        na.strings = ""))
Participation <- Participation[!is.na(person_id)]
Participation[, season := as.integer(season)]
# Person-level designation (any program, any year): documents eligibility
Designated <- Participation[, .(ProgramDesignated = 1L,
                                ProgramNames = paste(sort(unique(program)), collapse = "|"),
                                ProgramSourceUrls = paste(sort(unique(source_url)), collapse = "|"),
                                FirstProgramSeason = min(season)),
                            by = person_id]
# Person-season participation (listed in that season's program event)
ParticipantSeason <- Participation[, .(ProgramParticipant = 1L,
                                       ProgramName = paste(sort(unique(program)), collapse = "|"),
                                       ProgramSourceUrl = paste(sort(unique(source_url)), collapse = "|")),
                                   by = .(person_id, season)]

# Person-level documented demographics and eligibility
Persons <- unique(Opening[, .(person_id)])
Persons <- merge(Persons, RaceDoc, by = "person_id", all.x = TRUE)
Persons <- merge(Persons, GenderAll, by = "person_id", all.x = TRUE)
Persons <- merge(Persons, Designated, by = "person_id", all.x = TRUE)
Persons[is.na(ProgramDesignated), ProgramDesignated := 0L]
# Author decision Oct 2026: eligibility is positive-only. PolicyEligible = 1
# for a documented woman, a documented NFL-definition minority (any
# non-white ancestry component or Hispanic ethnicity) or a person the league
# itself designated (Accelerator participant, mandate hire, named minority
# head coach); NA otherwise. It is never 0: the NFL's "minority" is wider
# than the Census "non-white" (the league counts a Lebanese head coach as a
# minority, whom ancestry mapping files as white), so a documented white man
# is not documented as ineligible. CensusWhiteMan marks the ancestry-mapped
# white + Wikidata-male persons for an explicitly assumption-based upper
# bound; it is a separate column, not a value of PolicyEligible.
Persons[, PolicyEligible := fifelse(
  (!is.na(DocumentedWoman) & DocumentedWoman == 1L) |
    (!is.na(DocumentedMinority) & DocumentedMinority == 1L) | ProgramDesignated == 1L,
  1L, NA_integer_)]
Persons[, CensusWhiteMan := as.integer(is.na(PolicyEligible) &
                                         !is.na(DocumentedMinority) & DocumentedMinority == 0L &
                                         !is.na(DocumentedWoman) & DocumentedWoman == 0L)]
MenaOverride <- Persons[ProgramDesignated == 1L & !is.na(DocumentedMinority) &
                          DocumentedMinority == 0L]
message(sprintf("16: %d league-designated persons whose ancestry mapping is white (NFL definition prevails)",
                nrow(MenaOverride)))
Persons[, EligibilityKnown := as.integer(!is.na(PolicyEligible))]
Persons[, EligibilitySource := fifelse(is.na(PolicyEligible), NA_character_,
  fifelse(!is.na(DocumentedWoman) & DocumentedWoman == 1L, "documented_woman",
    fifelse(!is.na(DocumentedMinority) & DocumentedMinority == 1L, "documented_minority",
            "nfl_designation")))]
message(sprintf("16: %d opening-staff persons: race documented %d, gender documented %d, league-designated %d, documented eligible %d, Census white men (assumption-only) %d",
                nrow(Persons), sum(!is.na(Persons$DocumentedRace)),
                sum(!is.na(Persons$DocumentedWoman)), sum(Persons$ProgramDesignated),
                sum(Persons$EligibilityKnown), sum(Persons$CensusWhiteMan)))

# ---------------------------------------------------------------------------
# Person-season panel (opening-snapshot coaches, 2007+)
# ---------------------------------------------------------------------------

AllOpen <- Opening[, .(franchise_id, season, person_id, RoleTier, Domain)]
ObservedTS <- OpeningCov[OpeningStaffObserved == 1L, .(franchise_id, season)]

PS <- Opening[IsCoach == TRUE]
PS <- merge(PS, Persons, by = "person_id", all.x = TRUE)
PS <- merge(PS, ParticipantSeason, by = c("person_id", "season"), all.x = TRUE)
PS[is.na(ProgramParticipant), ProgramParticipant := 0L]
PS[, OpeningSnapshotDate := as.Date(OpeningRevisionTimestamp)]
for (v in c("IsCoach", "IsHeadCoach", "IsInterimHC", "IsCoordinator", "IsOC", "IsDC",
            "IsPositionCoach", "IsAssistantCoach", "IsOffenseCoach", "IsDefenseCoach",
            "IsSpecialTeamsCoach", "NewToFranchise", "PromotedWithinFranchise",
            "PrevTeamSeasonObserved", "TenureLeftCensored")) {
  set(PS, j = v, value = as_int01(PS[[v]]))
}
setnames(PS, c("p_black_any_pred", "p_white_pred", "prior_black_pred"),
         c("PBlackPred", "PWhitePred", "PriorBlackPred"))

# Outcomes at t+k: same-franchise retention and promotion need the
# franchise's opening snapshot at t+k; any-franchise outcomes need the league
# wide snapshot season (all franchises are observed together in the template
# era, so the league-wide condition is season + k <= MaxOpeningSeason).
next_outcomes <- function(ps, k) {
  key <- ps[, .(franchise_id, person_id, season, RoleTier)]
  key[, SeasonNext := season + k]
  obs <- merge(key, ObservedTS[, .(franchise_id, SeasonNext = season, Observed = 1L)],
               by = c("franchise_id", "SeasonNext"), all.x = TRUE)
  same <- AllOpen[, .(franchise_id, person_id, SeasonNext = season,
                      RoleTierNext = RoleTier, DomainNext = Domain)]
  obs <- merge(obs, same, by = c("franchise_id", "person_id", "SeasonNext"), all.x = TRUE)
  anyf <- AllOpen[Domain == "coach", .(AnyBest = min(RoleTier), AnyStaff = 1L),
                  by = .(person_id, SeasonNext = season)]
  anystaff <- AllOpen[, .(AnyStaffAll = 1L), by = .(person_id, SeasonNext = season)]
  obs <- merge(obs, anyf, by = c("person_id", "SeasonNext"), all.x = TRUE)
  obs <- merge(obs, anystaff, by = c("person_id", "SeasonNext"), all.x = TRUE)
  obs[, Observed := fifelse(is.na(Observed), 0L, 1L)]
  obs[, Retained := fifelse(Observed == 1L, as.integer(!is.na(RoleTierNext)), NA_integer_)]
  obs[, Promoted := fifelse(Observed == 1L,
                            as.integer(!is.na(RoleTierNext) & DomainNext == "coach" &
                                         RoleTierNext < RoleTier), NA_integer_)]
  obs[, AnyObserved := as.integer(SeasonNext <= MaxOpeningSeason)]
  obs[, OnAny := fifelse(AnyObserved == 1L, as.integer(!is.na(AnyStaffAll)), NA_integer_)]
  obs[, Advanced := fifelse(AnyObserved == 1L,
                            as.integer(!is.na(AnyBest) & AnyBest < RoleTier), NA_integer_)]
  out <- obs[, .(franchise_id, person_id, season, Observed, Retained, Promoted,
                 AnyObserved, OnAny, Advanced)]
  setnames(out, c("Observed", "Retained", "Promoted", "AnyObserved", "OnAny", "Advanced"),
           paste0(c("Next", "RetainedNext", "PromotedNext", "AnyNext", "OnAnyStaffNext",
                    "AdvancedAnyNext"), k, c("Observed", "", "", "Observed", "", "")))
  out
}
PS <- merge(PS, next_outcomes(PS, 1L), by = c("franchise_id", "person_id", "season"))
PS <- merge(PS, next_outcomes(PS, 2L), by = c("franchise_id", "person_id", "season"))
assert_true(all(is.na(PS$RetainedNext1[PS$Next1Observed == 0L])),
            "16: retention outcome set where the next opening is unobserved")
assert_true(all(PS$PromotedNext1[PS$RetainedNext1 == 0L & !is.na(PS$RetainedNext1)] == 0L),
            "16: promotion without retention")

PS <- add_rooney_policies(PS, "season")
PS <- PS[, .(franchise_id, season, person_id, person_name, role_std, RoleTier, Unit,
             IsCoach, IsHeadCoach, IsInterimHC, IsCoordinator, IsOC, IsDC, IsPositionCoach,
             IsAssistantCoach, IsOffenseCoach, IsDefenseCoach, IsSpecialTeamsCoach,
             OpeningSnapshotDate, OpeningTargetDate, NewToFranchise, PromotedWithinFranchise,
             PrevTeamSeasonObserved, SeasonsWithFranchise, TenureLeftCensored,
             Next1Observed, RetainedNext1, PromotedNext1, AnyNext1Observed, OnAnyStaffNext1,
             AdvancedAnyNext1, Next2Observed, RetainedNext2, PromotedNext2, AnyNext2Observed,
             OnAnyStaffNext2, AdvancedAnyNext2,
             DocumentedRace, DocumentedBlack, DocumentedMinority, DocumentedRaceSource,
             DocumentedWoman, WomanSource, PolicyEligible, EligibilityKnown, EligibilitySource,
             CensusWhiteMan,
             ProgramDesignated, ProgramNames, ProgramParticipant, ProgramName, ProgramSourceUrl,
             PBlackPred, PWhitePred, PriorBlackPred, pred_role_group_first, pred_unit_first,
             pred_first_era, RooneyRule, RooneyFrontOffice2009, RooneyAmend2020, RooneyAmend2022,
             RooneyCompensatory2021, OffensiveAssistantMandate, OffensiveAssistantSubsidy,
             OffensiveAssistantVoluntary2025, RooneyEra, PolicyTimingConvention)]
setorder(PS, franchise_id, season, RoleTier, person_id)

PSLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  season = "NFL season (template era, 2007+)",
  person_id = "Staff person identifier (01)",
  person_name = "Person name as listed",
  role_std = "Primary standardized role in the opening snapshot (01 priority order)",
  RoleTier = "Hierarchy tier of the primary role (1 = HC ... 8 = other coaching support)",
  Unit = "Unit of the primary role (offense, defense, special_teams, general)",
  IsCoach = "On-field coaching role in the opening snapshot (always 1 in this panel)",
  IsHeadCoach = "Listed as head coach (incl. interim) in the opening snapshot",
  IsInterimHC = "Listed as interim head coach in the opening snapshot",
  IsCoordinator = "OC/DC/STC in the opening snapshot",
  IsOC = "Offensive coordinator", IsDC = "Defensive coordinator",
  IsPositionCoach = "Position coach", IsAssistantCoach = "Assistant / quality-control coach",
  IsOffenseCoach = "Any on-field coaching role in the offensive unit",
  IsDefenseCoach = "Any on-field coaching role in the defensive unit",
  IsSpecialTeamsCoach = "Any on-field coaching role in the special-teams unit",
  OpeningSnapshotDate = "Date of the Wikipedia staff-template revision used as the opening snapshot (the listing is observed on this day; not a hire date)",
  OpeningTargetDate = "Opening-snapshot target date (00:00 UTC on the franchise's first REG game date)",
  NewToFranchise = "Not on this franchise's opening staff in season - 1 (NA when that snapshot is unobserved)",
  PromotedWithinFranchise = "On the franchise's opening staff in season - 1 in the coach domain with a lower tier (NA when unobserved)",
  PrevTeamSeasonObserved = "Franchise opening staff for season - 1 observed",
  SeasonsWithFranchise = "Consecutive opening snapshots with the franchise, including this one",
  TenureLeftCensored = "The run with the franchise starts in the first template season (2007)",
  Next1Observed = "Franchise opening staff for season + 1 observed (0 = right-censored)",
  RetainedNext1 = "On the same franchise's opening staff (any role) in season + 1; NA when Next1Observed = 0",
  PromotedNext1 = "Retained at the franchise in season + 1 in a coaching role of a higher tier (lower RoleTier number); 0 when not retained; NA when unobserved",
  AnyNext1Observed = "League-wide opening snapshots for season + 1 exist (season + 1 <= last template season)",
  OnAnyStaffNext1 = "On any franchise's opening staff in season + 1; NA when AnyNext1Observed = 0",
  AdvancedAnyNext1 = "Holds a higher-tier coaching role on any franchise's opening staff in season + 1; NA when AnyNext1Observed = 0",
  Next2Observed = "Franchise opening staff for season + 2 observed",
  RetainedNext2 = "On the same franchise's opening staff in season + 2; NA when unobserved",
  PromotedNext2 = "Higher-tier coaching role at the same franchise in season + 2; NA when unobserved",
  AnyNext2Observed = "League-wide opening snapshots for season + 2 exist",
  OnAnyStaffNext2 = "On any franchise's opening staff in season + 2",
  AdvancedAnyNext2 = "Higher-tier coaching role on any opening staff in season + 2",
  DocumentedRace = "Documented race/ethnicity category (Wikidata P172, Wikipedia category or classified article text; 04d/04e); NA = undocumented, NOT white",
  DocumentedBlack = "Documented Black (alone or in combination); NA when race undocumented",
  DocumentedMinority = "Documented NFL-definition racial/ethnic minority by ancestry mapping (any non-white component or Hispanic) = 1; 0 = ancestry mapped white non-Hispanic (NOT proof of NFL non-minority status: the league counts e.g. Lebanese coaches as minorities); NA = undocumented",
  DocumentedRaceSource = "Source types of the documented race (wikidata/category/text)",
  DocumentedWoman = "1 = documented woman (Wikidata P21 female or a Wikipedia women's category); 0 = Wikidata P21 male; NA = no gender statement (never inferred from the name)",
  WomanSource = "Source of DocumentedWoman",
  PolicyEligible = "NFL diversity-provision eligibility, positive-only: 1 = documented woman, documented minority or league-designated (Accelerator participant, mandate hire, named minority HC); NA = not documented (never 0)",
  EligibilityKnown = "PolicyEligible is documented (= PolicyEligible == 1)",
  EligibilitySource = "Basis of PolicyEligible (documented_woman, documented_minority, nfl_designation)",
  CensusWhiteMan = "Assumption-only marker: ancestry-mapped white AND Wikidata male AND not league-designated; used for EligibleShareUpperCensus, not for PolicyEligible",
  ProgramDesignated = "Ever listed by the NFL as a Coach Accelerator participant (2022-2024) or named as an offensive-assistant mandate hire (02e)",
  ProgramNames = "Programs in which the person is documented (pipe-separated)",
  ProgramParticipant = "Listed in a program event for this season (Accelerator cohort of the season, or mandate-created job in this season)",
  ProgramName = "Program(s) of this season's participation",
  ProgramSourceUrl = "Source URL(s) of this season's participation",
  PBlackPred = "Predicted P(non-Hispanic Black alone), model-only (sensitivity; never used for eligibility)",
  PWhitePred = "Predicted P(white), model-only",
  PriorBlackPred = "EM prior P(Black) of the predicted-race model",
  pred_role_group_first = "Prior covariate: role group at first staff appearance",
  pred_unit_first = "Prior covariate: unit at first staff appearance",
  pred_first_era = "Prior covariate: era of first staff season",
  RooneyRule = "Policy environment flag (00-policy-functions.R; opening-staff timing)",
  RooneyFrontOffice2009 = "Policy environment flag", RooneyAmend2020 = "Policy environment flag",
  RooneyAmend2022 = "Policy environment flag", RooneyCompensatory2021 = "Policy environment flag",
  OffensiveAssistantMandate = "Mandate in force for the season's opening staff (2022-2024)",
  OffensiveAssistantSubsidy = "League reimbursement in force (2022-2024)",
  OffensiveAssistantVoluntary2025 = "Voluntary continuation (2025+)",
  RooneyEra = "Calendar policy cohort (pre_rule, rule_2003, amend_2020, amend_2022, post_mandate_2025)",
  PolicyTimingConvention = "Timing convention of the policy flags"
)
write_sample(as.data.frame(PS), "coach_policy_person_season",
             key = c("franchise_id", "season", "person_id"), labels = PSLabels)

# ---------------------------------------------------------------------------
# Team-unit-season panel (offense / defense)
# ---------------------------------------------------------------------------

unit_rows <- function(unit, flag) {
  d <- PS[get(flag) == 1L]
  d[, Unit := unit]
  d
}
UnitCoaches <- rbind(unit_rows("offense", "IsOffenseCoach"),
                     unit_rows("defense", "IsDefenseCoach"))
PrevObs <- OpeningCov[OpeningStaffObserved == 1L, .(franchise_id, season = season + 1L,
                                                    PrevOpeningObserved = 1L)]
NextObs <- OpeningCov[OpeningStaffObserved == 1L, .(franchise_id, season = season - 1L,
                                                    NextOpeningObserved = 1L)]
Cells <- CJ(franchise_id = unique(ObservedTS$franchise_id), season = unique(ObservedTS$season),
            Unit = c("offense", "defense"))
Cells <- merge(Cells, ObservedTS[, .(franchise_id, season, OpeningObserved = 1L)],
               by = c("franchise_id", "season"))
Cells <- merge(Cells, PrevObs, by = c("franchise_id", "season"), all.x = TRUE)
Cells <- merge(Cells, NextObs, by = c("franchise_id", "season"), all.x = TRUE)
Cells[is.na(PrevOpeningObserved), PrevOpeningObserved := 0L]
Cells[is.na(NextOpeningObserved), NextOpeningObserved := 0L]

sum_na <- function(x) sum(x, na.rm = TRUE)
UnitAgg <- UnitCoaches[, .(
  NCoaches = .N,
  NDocumentedEligible = sum_na(PolicyEligible == 1L),
  NCensusWhiteMen = sum_na(CensusWhiteMan == 1L),
  NEligibilityKnown = sum(!is.na(PolicyEligible)),
  NDocumentedWomen = sum_na(DocumentedWoman == 1L),
  NDocumentedMinority = sum_na(DocumentedMinority == 1L),
  NDocumentedBlack = sum_na(DocumentedBlack == 1L),
  NRaceDocumented = sum(!is.na(DocumentedRace)),
  NNewCoachesRaw = sum_na(NewToFranchise == 1L),
  NNewKnownRaw = sum(!is.na(NewToFranchise)),
  NNewEligibleRaw = sum_na(NewToFranchise == 1L & PolicyEligible == 1L),
  NNewCensusWhiteMenRaw = sum_na(NewToFranchise == 1L & CensusWhiteMan == 1L),
  NNewEligibilityKnownRaw = sum_na(NewToFranchise == 1L & !is.na(PolicyEligible)),
  NRetainedRaw = sum_na(RetainedNext1 == 1L),
  NRetainedKnownRaw = sum(!is.na(RetainedNext1)),
  NPromotedRaw = sum_na(PromotedNext1 == 1L),
  NPromotedKnownRaw = sum(!is.na(PromotedNext1)),
  NProgramParticipants = sum_na(ProgramParticipant == 1L),
  NProgramDesignated = sum_na(ProgramDesignated == 1L),
  MeanPBlackPred = if (any(!is.na(PBlackPred))) mean(PBlackPred, na.rm = TRUE) else NA_real_,
  NPBlackPredKnown = sum(!is.na(PBlackPred)),
  MeanPriorBlackPred = if (any(!is.na(PriorBlackPred))) mean(PriorBlackPred, na.rm = TRUE) else NA_real_
), by = .(franchise_id, season, Unit)]

# Unit coordinator: the OC (offense) / DC (defense) listed at the opening;
# when several are listed the lowest person_id is kept and NUnitCoords > 1
Coord <- UnitCoaches[(Unit == "offense" & IsOC == 1L) | (Unit == "defense" & IsDC == 1L)]
setorder(Coord, franchise_id, season, Unit, person_id)
Coord <- Coord[, .(UnitCoordPersonId = person_id[1], NUnitCoords = .N,
                   UnitCoordDocumentedBlack = DocumentedBlack[1],
                   UnitCoordDocumentedMinority = DocumentedMinority[1],
                   UnitCoordPolicyEligible = PolicyEligible[1],
                   UnitCoordPBlackPred = PBlackPred[1]),
               by = .(franchise_id, season, Unit)]

TU <- merge(Cells, UnitAgg, by = c("franchise_id", "season", "Unit"), all.x = TRUE)
TU <- merge(TU, Coord, by = c("franchise_id", "season", "Unit"), all.x = TRUE)
zero_cols <- c("NCoaches", "NDocumentedEligible", "NCensusWhiteMen", "NEligibilityKnown",
               "NDocumentedWomen", "NDocumentedMinority", "NDocumentedBlack", "NRaceDocumented",
               "NNewCoachesRaw", "NNewKnownRaw", "NNewEligibleRaw", "NNewCensusWhiteMenRaw",
               "NNewEligibilityKnownRaw", "NRetainedRaw", "NRetainedKnownRaw", "NPromotedRaw",
               "NPromotedKnownRaw", "NProgramParticipants", "NProgramDesignated",
               "NPBlackPredKnown", "NUnitCoords")
for (v in zero_cols) set(TU, i = which(is.na(TU[[v]])), j = v, value = 0L)
# Observed cell with no listed coach: counts are genuine zeros (the snapshot
# was parsed); shares are undefined
TU[, ShareEligibleKnown := fifelse(NEligibilityKnown > 0, NDocumentedEligible / NEligibilityKnown, NA_real_)]
TU[, EligibleShareLower := fifelse(NCoaches > 0, NDocumentedEligible / NCoaches, NA_real_)]
# No coach is documented as ineligible (positive-only eligibility), so the
# identified upper bound is 1; EligibleShareUpperCensus assumes ancestry-
# mapped white men are ineligible (fails for MENA coaches the NFL counts)
TU[, EligibleShareUpper := fifelse(NCoaches > 0, 1, NA_real_)]
TU[, EligibleShareUpperCensus := fifelse(NCoaches > 0, (NCoaches - NCensusWhiteMen) / NCoaches, NA_real_)]
TU[, ShareEligibleCensus := fifelse(NDocumentedEligible + NCensusWhiteMen > 0,
                                    NDocumentedEligible / (NDocumentedEligible + NCensusWhiteMen), NA_real_)]
# New-coach counts need the previous opening snapshot; retention/promotion
# counts need the next one. Unobserved -> NA, never zero.
TU[, NNewCoaches := fifelse(PrevOpeningObserved == 1L, NNewCoachesRaw, NA_integer_)]
TU[, NNewKnown := fifelse(PrevOpeningObserved == 1L, NNewKnownRaw, NA_integer_)]
TU[, NNewEligible := fifelse(PrevOpeningObserved == 1L, NNewEligibleRaw, NA_integer_)]
TU[, NNewEligibilityKnown := fifelse(PrevOpeningObserved == 1L, NNewEligibilityKnownRaw, NA_integer_)]
TU[, ShareNewEligibleKnown := fifelse(!is.na(NNewEligibilityKnown) & NNewEligibilityKnown > 0,
                                      NNewEligible / NNewEligibilityKnown, NA_real_)]
TU[, NewEligibleShareLower := fifelse(!is.na(NNewCoaches) & NNewCoaches > 0,
                                      NNewEligible / NNewCoaches, NA_real_)]
TU[, NewEligibleShareUpper := fifelse(!is.na(NNewCoaches) & NNewCoaches > 0, 1, NA_real_)]
TU[, NewEligibleShareUpperCensus := fifelse(!is.na(NNewCoaches) & NNewCoaches > 0,
                                            (NNewCoaches - NNewCensusWhiteMenRaw) / NNewCoaches, NA_real_)]
TU[, NRetainedNextSeason := fifelse(NextOpeningObserved == 1L, NRetainedRaw, NA_integer_)]
TU[, NRetainedKnown := fifelse(NextOpeningObserved == 1L, NRetainedKnownRaw, NA_integer_)]
TU[, NPromotedNextSeason := fifelse(NextOpeningObserved == 1L, NPromotedRaw, NA_integer_)]
TU[, NPromotedKnown := fifelse(NextOpeningObserved == 1L, NPromotedKnownRaw, NA_integer_)]
assert_true(all(TU$NRetainedKnown[!is.na(TU$NRetainedKnown)] ==
                  TU$NCoaches[!is.na(TU$NRetainedKnown)]),
            "16: retention known-count differs from NCoaches where the next opening is observed")
TU[, c("NNewCoachesRaw", "NNewKnownRaw", "NNewEligibleRaw", "NNewCensusWhiteMenRaw",
       "NNewEligibilityKnownRaw", "NRetainedRaw", "NRetainedKnownRaw", "NPromotedRaw",
       "NPromotedKnownRaw") := NULL]
TU <- add_rooney_policies(TU, "season")
setcolorder(TU, c("franchise_id", "season", "Unit", "OpeningObserved", "PrevOpeningObserved",
                  "NextOpeningObserved", "NCoaches", "NDocumentedEligible", "NCensusWhiteMen",
                  "NEligibilityKnown", "ShareEligibleKnown", "ShareEligibleCensus", "EligibleShareLower",
                  "EligibleShareUpper", "EligibleShareUpperCensus", "NDocumentedWomen", "NDocumentedMinority",
                  "NDocumentedBlack", "NRaceDocumented", "NNewCoaches", "NNewKnown",
                  "NNewEligible", "NNewEligibilityKnown", "ShareNewEligibleKnown",
                  "NewEligibleShareLower", "NewEligibleShareUpper", "NewEligibleShareUpperCensus", "NRetainedNextSeason",
                  "NRetainedKnown", "NPromotedNextSeason", "NPromotedKnown",
                  "NProgramParticipants", "NProgramDesignated", "MeanPBlackPred",
                  "NPBlackPredKnown", "MeanPriorBlackPred", "UnitCoordPersonId", "NUnitCoords",
                  "UnitCoordDocumentedBlack", "UnitCoordDocumentedMinority",
                  "UnitCoordPolicyEligible", "UnitCoordPBlackPred"))
setorder(TU, franchise_id, season, Unit)

TULabels <- c(
  franchise_id = "Franchise identifier", season = "NFL season (2007+)",
  Unit = "offense or defense",
  OpeningObserved = "Opening snapshot parsed (always 1; unobserved team-seasons are absent and listed in the coverage file)",
  PrevOpeningObserved = "Opening snapshot of season - 1 observed (new-coach counts defined)",
  NextOpeningObserved = "Opening snapshot of season + 1 observed (retention/promotion counts defined)",
  NCoaches = "On-field coaches of the unit listed at the opening (coordinator, position and assistant coaches); 0 = none listed in a parsed snapshot",
  NDocumentedEligible = "Unit coaches with PolicyEligible = 1 (documented woman, documented minority or league-designated)",
  NCensusWhiteMen = "Unit coaches ancestry-mapped white AND Wikidata male AND not league-designated (assumption-only marker)",
  NEligibilityKnown = "Unit coaches with documented eligibility (= NDocumentedEligible; eligibility is positive-only)",
  ShareEligibleKnown = "NDocumentedEligible / NEligibilityKnown: identically 1 where any eligibility is documented (positive-only ascertainment); kept for the contract, use the bounds",
  ShareEligibleCensus = "Assumption-based ascertained share: NDocumentedEligible / (NDocumentedEligible + NCensusWhiteMen); assumes ancestry-mapped white men are NFL-ineligible",
  EligibleShareLower = "Identified lower bound of the eligible share: NDocumentedEligible / NCoaches",
  EligibleShareUpper = "Identified upper bound: 1 (no coach is documented as NFL-ineligible)",
  EligibleShareUpperCensus = "Assumption-based upper bound: (NCoaches - NCensusWhiteMen) / NCoaches",
  NDocumentedWomen = "Unit coaches documented as women",
  NDocumentedMinority = "Unit coaches documented as racial/ethnic minority",
  NDocumentedBlack = "Unit coaches documented as Black",
  NRaceDocumented = "Unit coaches with documented race",
  NNewCoaches = "Unit coaches not on the franchise's opening staff in season - 1; NA when that snapshot is unobserved",
  NNewKnown = "Unit coaches whose NewToFranchise is known (= NCoaches when the previous opening is observed)",
  NNewEligible = "New unit coaches with PolicyEligible = 1",
  NNewEligibilityKnown = "New unit coaches with known eligibility",
  ShareNewEligibleKnown = "NNewEligible / NNewEligibilityKnown",
  NewEligibleShareLower = "Lower bound: NNewEligible / NNewCoaches",
  NewEligibleShareUpper = "Identified upper bound: 1",
  NewEligibleShareUpperCensus = "Assumption-based upper bound: (NNewCoaches - new Census white men) / NNewCoaches",
  NRetainedNextSeason = "Unit coaches on the franchise's opening staff in season + 1; NA when unobserved",
  NRetainedKnown = "Unit coaches with a defined retention outcome",
  NPromotedNextSeason = "Unit coaches in a higher-tier coaching role at the franchise in season + 1; NA when unobserved",
  NPromotedKnown = "Unit coaches with a defined promotion outcome",
  NProgramParticipants = "Unit coaches listed in a league program event for this season",
  NProgramDesignated = "Unit coaches ever league-designated (Accelerator or mandate hire)",
  MeanPBlackPred = "Mean predicted P(Black) of the unit coaches (sensitivity; not eligibility)",
  NPBlackPredKnown = "Unit coaches with a predicted-race probability",
  MeanPriorBlackPred = "Mean prior P(Black) of the unit coaches",
  UnitCoordPersonId = "OC (offense) / DC (defense) listed at the opening",
  NUnitCoords = "Number of coordinators listed (0 = none)",
  UnitCoordDocumentedBlack = "Coordinator documented Black (NA = undocumented)",
  UnitCoordDocumentedMinority = "Coordinator documented minority (NA = undocumented)",
  UnitCoordPolicyEligible = "Coordinator PolicyEligible (tri-state)",
  UnitCoordPBlackPred = "Coordinator predicted P(Black)",
  RooneyRule = "Policy environment flag (opening-staff timing)",
  RooneyFrontOffice2009 = "Policy environment flag", RooneyAmend2020 = "Policy environment flag",
  RooneyAmend2022 = "Policy environment flag", RooneyCompensatory2021 = "Policy environment flag",
  OffensiveAssistantMandate = "Mandate in force (2022-2024)",
  OffensiveAssistantSubsidy = "Subsidy in force (2022-2024)",
  OffensiveAssistantVoluntary2025 = "Voluntary continuation (2025+)",
  RooneyEra = "Calendar policy cohort", PolicyTimingConvention = "Timing convention of the flags",
  CoordinatorMobility2021 = "Policy environment flag", CoachingFellowship2021 = "Policy environment flag",
  RooneyInPerson2022 = "Policy environment flag"
)
write_sample(as.data.frame(TU), "coach_policy_team_unit_season",
             key = c("franchise_id", "season", "Unit"), labels = TULabels)

# ---------------------------------------------------------------------------
# Job spells with interval-censored dates
# ---------------------------------------------------------------------------

# Template snapshots of each franchise in time order (k), with the revision
# timestamp (listing observed at this time) and the target date (revision
# still in force on this day)
Snaps <- as.data.table(DBI::dbGetQuery(con, "
  SELECT franchise_id, season, snapshot, target_date, revision_timestamp, revid, template_title
  FROM staff_snapshots WHERE source = 'staff_template' AND parse_ok"))
Snaps[, `:=`(season = as.integer(season), target_date = as.Date(target_date),
             RevisionDate = as.Date(revision_timestamp))]
setorder(Snaps, franchise_id, target_date, season, snapshot)
Snaps[, k := seq_len(.N), by = franchise_id]
Snaps[, SourceUrl := sprintf("https://en.wikipedia.org/w/index.php?title=%s&oldid=%d",
                             gsub(" ", "_", template_title), revid)]
Listed <- as.data.table(DBI::dbGetQuery(con, "
  SELECT DISTINCT e.franchise_id, e.season, e.snapshot, e.person_id, e.role_std
  FROM staff_entries e JOIN staff_snapshots s USING (franchise_id, season, snapshot)
  WHERE s.source = 'staff_template' AND s.parse_ok AND e.person_id IS NOT NULL
    AND e.role_std IS NOT NULL"))
Listed[, season := as.integer(season)]
Listed <- merge(Listed, Snaps[, .(franchise_id, season, snapshot, k)],
                by = c("franchise_id", "season", "snapshot"))
setorder(Listed, franchise_id, person_id, role_std, k)
Listed[, Run := cumsum(c(1L, diff(k) != 1L)), by = .(franchise_id, person_id, role_std)]
Runs <- Listed[, .(kmin = min(k), kmax = max(k)), by = .(franchise_id, person_id, role_std, Run)]
Listed <- merge(Listed, Runs, by = c("franchise_id", "person_id", "role_std", "Run"))
# Person-level listing runs (any role), the fallback for role lookups
ListedP <- unique(Listed[, .(franchise_id, person_id, k)])
setorder(ListedP, franchise_id, person_id, k)
ListedP[, RunP := cumsum(c(1L, diff(k) != 1L)), by = .(franchise_id, person_id)]
ListedP[, `:=`(kminP = min(k), kmaxP = max(k)), by = .(franchise_id, person_id, RunP)]

# Role spells: consecutive opening snapshots of a person in the same primary
# role at a franchise. All roles of the person-season are kept in AllRoles
# through the person-season panel; a role change ends the spell.
Sp <- Opening[IsCoach == TRUE, .(franchise_id, season, person_id, person_name, role_std,
                                 RoleTier, Unit, SeasonsWithFranchise)]
setorder(Sp, person_id, franchise_id, role_std, season)
Sp[, RoleRun := cumsum(c(1L, diff(season) != 1L)), by = .(person_id, franchise_id, role_std)]
Spells <- Sp[, .(FirstSeason = min(season), LastSeason = max(season), NSeasons = .N,
                 person_name = person_name[1], RoleTier = RoleTier[1], Unit = Unit[1],
                 FranchiseSpellStart = min(season - SeasonsWithFranchise + 1L)),
             by = .(person_id, franchise_id, role_std, RoleRun)]
Spells[, SpellId := sprintf("%s|%s|%s|%d", person_id, franchise_id, role_std, FirstSeason)]
Spells[, FranchiseSpellId := sprintf("%s|%s|%d", person_id, franchise_id, FranchiseSpellStart)]
assert_true(!anyDuplicated(Spells$SpellId), "16: SpellId not unique")

# Observation bounds. Entry: the listing run containing the first opening
# snapshot of the spell may start in a midseason/late snapshot of the
# previous season (an in-season hire); EntryUpperDate = revision date of the
# first listing, EntryLowerDate = target date of the preceding snapshot
# (which did not list the person in the role). Exit: ExitLowerDate = target
# date of the last listing in the run containing the last opening snapshot;
# ExitUpperDate = revision date of the next snapshot (not listing).
PreK <- Snaps[snapshot == "preseason", .(franchise_id, season, k)]
Spells <- merge(Spells, PreK[, .(franchise_id, FirstSeason = season, kFirst = k)],
                by = c("franchise_id", "FirstSeason"), all.x = TRUE)
Spells <- merge(Spells, PreK[, .(franchise_id, LastSeason = season, kLast = k)],
                by = c("franchise_id", "LastSeason"), all.x = TRUE)
assert_true(!anyNA(Spells$kFirst) && !anyNA(Spells$kLast),
            "16: opening snapshot of a spell season not found in staff_snapshots")
Spells <- merge(Spells, Listed[, .(franchise_id, person_id, role_std, kFirst = k, kEntry = kmin)],
                by = c("franchise_id", "person_id", "role_std", "kFirst"), all.x = TRUE)
Spells <- merge(Spells, Listed[, .(franchise_id, person_id, role_std, kLast = k, kExit = kmax)],
                by = c("franchise_id", "person_id", "role_std", "kLast"), all.x = TRUE)
# The opening table keys the primary role; a person listed under several
# roles may have a primary role that staff_entries records under another
# role_std of the same snapshot (e.g. ASST_HC + OL). Then the run lookup
# fails; the bounds then come from the person's listing run in ANY role at
# the franchise (BoundsFromRoleRun = 0).
Spells[, BoundsFromRoleRun := as.integer(!is.na(kEntry) & !is.na(kExit))]
Spells <- merge(Spells, ListedP[, .(franchise_id, person_id, kFirst = k, kEntryP = kminP)],
                by = c("franchise_id", "person_id", "kFirst"), all.x = TRUE)
Spells <- merge(Spells, ListedP[, .(franchise_id, person_id, kLast = k, kExitP = kmaxP)],
                by = c("franchise_id", "person_id", "kLast"), all.x = TRUE)
Spells[is.na(kEntry), kEntry := kEntryP]
Spells[is.na(kExit), kExit := kExitP]
assert_true(!anyNA(Spells$kEntry) && !anyNA(Spells$kExit),
            "16: a spell's opening snapshot has no staff_entries listing for the person")
SnapDates <- Snaps[, .(franchise_id, k, RevisionDate, target_date, SourceUrl)]
Spells <- merge(Spells, SnapDates[, .(franchise_id, kEntry = k, EntryUpperDate = RevisionDate,
                                      FirstSourceUrl = SourceUrl)],
                by = c("franchise_id", "kEntry"), all.x = TRUE)
Spells <- merge(Spells, SnapDates[, .(franchise_id, kEntry = k + 1L, EntryLowerDate = target_date)],
                by = c("franchise_id", "kEntry"), all.x = TRUE)
Spells <- merge(Spells, SnapDates[, .(franchise_id, kExit = k, ExitLowerDate = target_date,
                                      LastSourceUrl = SourceUrl)],
                by = c("franchise_id", "kExit"), all.x = TRUE)
Spells <- merge(Spells, SnapDates[, .(franchise_id, kExit = k - 1L, ExitUpperDate = RevisionDate)],
                by = c("franchise_id", "kExit"), all.x = TRUE)
Spells[, EntryLeftCensored := as.integer(is.na(EntryLowerDate))]
Spells[, ExitRightCensored := as.integer(is.na(ExitUpperDate))]
Spells <- merge(Spells, SnapDates[, .(franchise_id, kFirst = k, FirstObservedDate = RevisionDate)],
                by = c("franchise_id", "kFirst"), all.x = TRUE)
Spells <- merge(Spells, SnapDates[, .(franchise_id, kLast = k, LastObservedDate = RevisionDate)],
                by = c("franchise_id", "kLast"), all.x = TRUE)
assert_true(all(Spells$EntryLowerDate <= Spells$EntryUpperDate, na.rm = TRUE) &&
              all(Spells$ExitLowerDate <= Spells$ExitUpperDate, na.rm = TRUE),
            "16: interval bounds out of order")

# Head-coach spells cross-checked against the game data: a HC role spell is
# verified when the person coached at least one REG game for the franchise
# in every season of the spell; the REG game days give evidence-based tenure
# bounds (FirstGameDate / LastGameDate). Verified in-season dismissal dates
# (hc_spell_corrections.csv, article-stated date) are the only actual
# departure dates written.
GameHC <- as.data.table(load_game_head_coaches(con))
GameHC[, HCKey := fifelse(is.na(HeadCoachPersonId), HeadCoachName, HeadCoachPersonId)]
GameReg <- GameHC[game_type == "REG"]
HCGameSeasons <- GameReg[, .(NGames = .N, FirstGameDate = min(gameday),
                             LastGameDate = max(gameday)),
                         by = .(franchise_id, HCKey, season)]
# Consecutive-season any-REG-game runs of a coach at a franchise (the
# HCFranchiseRuns logic of 02: a run survives a season in which an acting HC
# coached most games); keys the HC spells of the hires table
Runs2 <- unique(GameReg[, .(franchise_id, HCKey, season)])
setorder(Runs2, franchise_id, HCKey, season)
Runs2[, Break := cumsum(c(1L, diff(season) != 1L)), by = .(franchise_id, HCKey)]
Runs2[, RunStart := min(season), by = .(franchise_id, HCKey, Break)]
Runs2[, HCSpellId := sprintf("%s|%s|%d", franchise_id, HCKey, RunStart)]
Runs2[, HCSpellSeason := season - RunStart + 1L]
HCSpellGames <- Spells[role_std == "HC", .(SpellId, franchise_id, person_id, FirstSeason, LastSeason)]
HCSpellGames <- HCSpellGames[, .(season = seq(FirstSeason, LastSeason)),
                             by = .(SpellId, franchise_id, person_id)]
HCSpellGames <- merge(HCSpellGames,
                      HCGameSeasons[, .(franchise_id, person_id = HCKey, season, NGames,
                                        FirstGameDate, LastGameDate)],
                      by = c("franchise_id", "person_id", "season"), all.x = TRUE)
HCSpellGames <- HCSpellGames[, .(HCVerifiedSpell = as.integer(all(!is.na(NGames))),
                                 FirstGameDate = if (all(is.na(NGames))) as.Date(NA) else min(FirstGameDate, na.rm = TRUE),
                                 LastGameDate = if (all(is.na(NGames))) as.Date(NA) else max(LastGameDate, na.rm = TRUE)),
                             by = SpellId]
Spells <- merge(Spells, HCSpellGames, by = "SpellId", all.x = TRUE)
Spells[role_std != "HC", HCVerifiedSpell := NA_integer_]
# Verified dismissal dates: Source text states the date (not "after game N")
Dismissals <- as.data.table(HCSpellCorrections)
Dismissals <- Dismissals[grepl("(fired|resigned) [A-Z][a-z]+ [0-9]+", Source)]
Dismissals <- merge(Dismissals,
                    unique(GameReg[, .(franchise_id, season, ReplacedHC = HeadCoachName,
                                       person_id = HeadCoachPersonId)]),
                    by = c("franchise_id", "season", "ReplacedHC"))
Dismissals <- Dismissals[!is.na(person_id), .(franchise_id, person_id, LastSeason = season,
                                               ActualDepartureDate = as.Date(AfterDate),
                                               DepartureSource = Source)]
Spells <- merge(Spells, Dismissals, by = c("franchise_id", "person_id", "LastSeason"), all.x = TRUE)
Spells[role_std != "HC", `:=`(ActualDepartureDate = as.Date(NA), DepartureSource = NA_character_)]
# Source-stated head-coach appointment dates (02e: strict sentence
# extraction from the coach's Wikipedia biography, with the quoted sentence
# and revision permalink). A candidate date is accepted for a HC spell only
# when it falls in the hiring window of that spell: after the franchise's
# last REG game of the season before the spell's game-data run started and
# no later than the person's first REG game of the run (opening snapshot
# target date for spells the game data does not verify). "announced" /
# "introduced" dates are kept apart from "hired/named/signed/agreed" dates;
# neither is asserted to be the contract's effective date.
hire_dates_path <- file.path(derived_cp, "hc_hire_dates.csv")
assert_true(file.exists(hire_dates_path),
            "16: data/derived/coach_policy/hc_hire_dates.csv missing (run scripts/02e_coach_policy_reference.py)")
HireDates <- as.data.table(read.csv(hire_dates_path, stringsAsFactors = FALSE, na.strings = ""))
HireDates[, date := as.Date(date)]
HireDates[, interim := as.integer(interim)]
FranchiseGames <- GameReg[, .(FirstGame = min(gameday), LastGame = max(gameday)),
                          by = .(franchise_id, season)]
# Hiring window of each HC spell
HCWin <- Spells[role_std == "HC", .(SpellId, franchise_id, person_id, FirstSeason, HCVerifiedSpell)]
HCWin <- merge(HCWin, Runs2[, .(franchise_id, person_id = HCKey, FirstSeason = season, RunStart)],
               by = c("franchise_id", "person_id", "FirstSeason"), all.x = TRUE)
HCWin[, StartSeason := fifelse(!is.na(RunStart) & HCVerifiedSpell == 1L, RunStart, FirstSeason)]
HCWin <- merge(HCWin, FranchiseGames[, .(franchise_id, StartSeason = season + 1L, WinLower = LastGame)],
               by = c("franchise_id", "StartSeason"), all.x = TRUE)
HCWin <- merge(HCWin, HCGameSeasons[, .(franchise_id, person_id = HCKey, StartSeason = season,
                                        WinUpperGame = FirstGameDate)],
               by = c("franchise_id", "person_id", "StartSeason"), all.x = TRUE)
HCWin <- merge(HCWin, Snaps[snapshot == "preseason", .(franchise_id, FirstSeason = season,
                                                        WinUpperOpen = target_date)],
               by = c("franchise_id", "FirstSeason"), all.x = TRUE)
HCWin[, WinUpper := fifelse(is.na(WinUpperGame), WinUpperOpen, WinUpperGame)]
# A run starting in the first game-data season (1999) has no observed lower
# bound: any earlier date is admissible and the latest one is taken
HCWin[, WindowLeftOpen := as.integer(is.na(WinLower))]
HCWin[is.na(WinLower), WinLower := as.Date("1900-01-01")]
Cand <- merge(HireDates[interim == 0L], HCWin, by = c("franchise_id", "person_id"),
              allow.cartesian = TRUE)
Cand <- Cand[date > WinLower & date <= WinUpper]
pick_date <- function(d, left_open) if (left_open == 1L) max(d) else min(d)
Chosen <- Cand[, .(ActualHireDate = if (any(date_kind == "hired_named"))
                     pick_date(date[date_kind == "hired_named"], WindowLeftOpen[1]) else as.Date(NA),
                   AnnouncementDate = if (any(date_kind == "announced"))
                     pick_date(date[date_kind == "announced"], WindowLeftOpen[1]) else as.Date(NA),
                   NHireDateCandidates = uniqueN(date),
                   HireDateSourceUrl = source_url[1],
                   HireDateQuote = sentence[order(date_kind != "hired_named", date)][1]),
               by = SpellId]
Spells <- merge(Spells, Chosen, by = "SpellId", all.x = TRUE)
Spells[is.na(NHireDateCandidates), NHireDateCandidates := 0L]
Spells[, HireDateKind := fifelse(!is.na(ActualHireDate), "hired_named",
                          fifelse(!is.na(AnnouncementDate), "announced_only", NA_character_))]
assert_true(all(is.na(Spells$ActualHireDate) | Spells$ActualHireDate <= Spells$EntryUpperDate),
            "16: a sourced hire date lies after the first snapshot listing the person")
Spells[, DateSource := paste0(
  fifelse(!is.na(ActualHireDate), "entry: article-stated appointment date (02e)",
   fifelse(!is.na(AnnouncementDate), "entry: article-stated announcement date only (02e)",
           "entry: snapshot bounds")),
  "; ",
  fifelse(!is.na(ActualDepartureDate), "departure: article-stated dismissal date (hc_spell_corrections)",
          "departure: snapshot bounds"))]
message(sprintf("16: HC spells %d; sourced appointment dates %d (announcement-only %d); dismissal dates %d",
                sum(Spells$role_std == "HC"), sum(!is.na(Spells$ActualHireDate)),
                sum(is.na(Spells$ActualHireDate) & !is.na(Spells$AnnouncementDate)),
                sum(!is.na(Spells$ActualDepartureDate))))

# Documented demographics, program participation and pre-policy membership
Spells <- merge(Spells, Persons, by = "person_id", all.x = TRUE)
Spells[, PreRooney2003Member := NA_integer_]   # template era starts 2007
Spells[, PreAmend2020Member := as.integer(FirstSeason <= 2020L)]
Spells[, PreMandate2022Member := as.integer(FirstSeason <= 2021L)]
Spells[, ProgramParticipant := ProgramDesignated]
Spells[, ProgramName := ProgramNames]
Spells[, ProgramSourceUrl := ProgramSourceUrls]
Spells[, SourceUrl := FirstSourceUrl]
Spells[, season := FirstSeason]
Spells <- add_rooney_policies(Spells, "season")
Spells[, season := NULL]
Spells <- Spells[, .(SpellId, FranchiseSpellId, franchise_id, person_id, person_name, role_std,
                     RoleTier, Unit, FirstSeason, LastSeason, NSeasons, FirstObservedDate,
                     LastObservedDate, EntryLowerDate, EntryUpperDate, ExitLowerDate, ExitUpperDate,
                     EntryLeftCensored, ExitRightCensored, BoundsFromRoleRun, ActualHireDate,
                     AnnouncementDate, HireDateKind, NHireDateCandidates, HireDateSourceUrl,
                     HireDateQuote, ActualDepartureDate, DateSource, DepartureSource, HCVerifiedSpell,
                     FirstGameDate, LastGameDate, DocumentedRace, DocumentedBlack,
                     DocumentedMinority, DocumentedRaceSource, DocumentedWoman, WomanSource,
                     PolicyEligible, EligibilityKnown, EligibilitySource, CensusWhiteMan, ProgramParticipant,
                     ProgramName, ProgramSourceUrl, PreRooney2003Member, PreAmend2020Member,
                     PreMandate2022Member, SourceUrl, LastSourceUrl, RooneyRule,
                     RooneyFrontOffice2009, RooneyAmend2020, RooneyAmend2022,
                     RooneyCompensatory2021, OffensiveAssistantMandate, OffensiveAssistantSubsidy,
                     OffensiveAssistantVoluntary2025, RooneyEra, PolicyTimingConvention)]
setorder(Spells, franchise_id, FirstSeason, RoleTier, person_id)

SpellLabels <- c(
  SpellId = "person_id|franchise_id|role_std|FirstSeason",
  FranchiseSpellId = "person_id|franchise_id|first season of the consecutive run with the franchise (any role)",
  franchise_id = "Franchise identifier", person_id = "Staff person identifier",
  person_name = "Person name", role_std = "Primary standardized role held throughout the spell",
  RoleTier = "Hierarchy tier of the role", Unit = "Unit of the role",
  FirstSeason = "First season on the opening staff in this role",
  LastSeason = "Last season on the opening staff in this role",
  NSeasons = "Consecutive opening snapshots in the role",
  FirstObservedDate = "Revision date of the first opening snapshot of the spell",
  LastObservedDate = "Revision date of the last opening snapshot of the spell",
  EntryLowerDate = "Target date of the last template snapshot before the first listing (person not yet listed in the role); NA = left-censored at the start of the template era",
  EntryUpperDate = "Revision date of the first template snapshot listing the person in the role (entry happened on or before this day)",
  ExitLowerDate = "Target date of the last template snapshot listing the person in the role (still listed on this day)",
  ExitUpperDate = "Revision date of the first later snapshot not listing the person; NA = right-censored (still listed at the last snapshot)",
  EntryLeftCensored = "EntryLowerDate missing", ExitRightCensored = "ExitUpperDate missing",
  BoundsFromRoleRun = "1 = bounds from the staff_entries listing run of this role (may extend into the previous season's in-season snapshots); 0 = the role run was not found under this role_std and bounds are the opening snapshots themselves",
  ActualHireDate = "HC spells: article-stated date on which the person was hired/named/signed/agreed as the franchise's head coach (02e, quoted in HireDateQuote), accepted only inside the spell's hiring window; NA = not sourced (never imputed); always NA for other roles",
  AnnouncementDate = "HC spells: article-stated introduction/announcement date (kept apart from the hiring-action date)",
  HireDateKind = "hired_named, announced_only or NA",
  NHireDateCandidates = "Distinct candidate dates found in the hiring window",
  HireDateSourceUrl = "Wikipedia revision permalink of the biography sentence",
  HireDateQuote = "The sentence that states the date",
  ActualDepartureDate = "Article-stated in-season dismissal/resignation date of a head coach (data/hand_coded/hc_spell_corrections.csv); NA otherwise",
  DateSource = "Which dates are actual and which are observation bounds",
  DepartureSource = "Wikipedia season article that states the dismissal date",
  HCVerifiedSpell = "HC spells: 1 when the person coached >= 1 REG game for the franchise in every season of the spell (game data); NA for other roles",
  FirstGameDate = "HC spells: first REG game day coached in the spell seasons",
  LastGameDate = "HC spells: last REG game day coached in the spell seasons",
  DocumentedRace = "Documented race category (NA = undocumented, not white)",
  DocumentedBlack = "Documented Black", DocumentedMinority = "Documented NFL-definition minority",
  DocumentedRaceSource = "Source types of documented race",
  DocumentedWoman = "Documented woman (Wikidata P21 / Wikipedia category); 0 = Wikidata male; NA = unknown",
  WomanSource = "Source of DocumentedWoman",
  PolicyEligible = "Positive-only NFL eligibility (documented woman, documented minority or league-designated); NA = not documented",
  EligibilityKnown = "PolicyEligible documented", EligibilitySource = "Basis of PolicyEligible",
  CensusWhiteMan = "Assumption-only marker (ancestry-mapped white, Wikidata male, not league-designated)",
  ProgramParticipant = "Person documented in a league program (Accelerator 2022-2024 or offensive-assistant mandate hire)",
  ProgramName = "Program(s)", ProgramSourceUrl = "Source URL(s) of the program listing",
  PreRooney2003Member = "NA: the template era starts in 2007, so pre-2003 membership is unobserved",
  PreAmend2020Member = "Spell starts on or before the 2020 opening staff (before the 2020 amendments' first full cycle)",
  PreMandate2022Member = "Spell starts on or before the 2021 opening staff (before the 2022 mandate)",
  SourceUrl = "Permalink of the first opening-snapshot template revision",
  LastSourceUrl = "Permalink of the last listing revision used for the exit bound",
  RooneyRule = "Policy flags at FirstSeason (opening-staff timing)",
  RooneyFrontOffice2009 = "Policy flag at FirstSeason", RooneyAmend2020 = "Policy flag at FirstSeason",
  RooneyAmend2022 = "Policy flag at FirstSeason", RooneyCompensatory2021 = "Policy flag at FirstSeason",
  OffensiveAssistantMandate = "Mandate in force at FirstSeason",
  OffensiveAssistantSubsidy = "Subsidy in force at FirstSeason",
  OffensiveAssistantVoluntary2025 = "Voluntary continuation at FirstSeason",
  RooneyEra = "Calendar policy cohort of FirstSeason", PolicyTimingConvention = "Timing convention"
)
write_sample(as.data.frame(Spells), "coach_job_spells", key = "SpellId", labels = SpellLabels)

# ---------------------------------------------------------------------------
# Head-coach hires: 1999-2025 from game data, 1990-1998 from nfl_hc_history
# ---------------------------------------------------------------------------

setorder(GameReg, franchise_id, season, gameday, week)
SeasonHC <- GameReg[, .(HeadCoachName = HeadCoachName[1], HeadCoachPersonId = HeadCoachPersonId[1],
                        HCMatchMethod = HCMatchMethod[1], FirstHC = HCKey[1], LastHC = HCKey[.N],
                        LastHCName = HeadCoachName[.N],
                        InSeasonHCChange = as.integer(uniqueN(HCKey) > 1L),
                        NHeadCoaches = uniqueN(HCKey)),
                    by = .(franchise_id, season)]
# Status of the season's last head coach when he was not the first-game HC:
# interim (listed as interim HC on the franchise's staff that season) or a
# permanent in-season appointment (listed as HC without the interim flag).
# Unknown when the staff tables have no HC row for him; unknown is NOT
# treated as interim.
StaffHC <- as.data.table(DBI::dbGetQuery(con, "
  SELECT franchise_id, season, person_id, bool_or(interim_any) AS AnyInterim,
         bool_or(NOT interim_any) AS AnyPermanent
  FROM staff_team_season WHERE role_std = 'HC' GROUP BY 1, 2, 3"))
StaffHC[, season := as.integer(season)]
StaffHC[, LastHCInterim := fifelse(AnyInterim, 1L, fifelse(AnyPermanent, 0L, NA_integer_))]
SeasonHC <- merge(SeasonHC, StaffHC[, .(franchise_id, season, LastHC = person_id, LastHCInterim)],
                  by = c("franchise_id", "season", "LastHC"), all.x = TRUE)
# Any-REG-game spell key of the first-game HC (Runs2 built above with the
# HCFranchiseRuns logic of 02)
SeasonHC <- merge(SeasonHC, Runs2[, .(franchise_id, FirstHC = HCKey, season, HCSpellId, HCSpellSeason)],
                  by = c("franchise_id", "FirstHC", "season"))
TeamOut <- as.data.table(DBI::dbGetQuery(con, "
  SELECT franchise_id, season, wins, losses, ties, games, win_pct AS WinPct,
         expected_wins AS ExpectedWins, wins_minus_expected AS WinsOverExpected
  FROM nfl_team_seasons"))
TeamOut[, season := as.integer(season)]
Current <- merge(SeasonHC, TeamOut, by = c("franchise_id", "season"), all.x = TRUE)
Current[, HistoricalCoverage := "current"]
Current[, HCSourceUrl := "nflverse schedules (nfl_team_week_head_coach) with verified corrections (00-setup-functions.R)"]
Current[, HCNameUncertain := 0L]
# Source-stated appointment date of the first-game HC (02e), accepted inside
# (first REG game of season - 1, first REG game of season]; 1999 has no
# game-data lower bound and uses 1998-01-01
HWin <- merge(Current[, .(franchise_id, season, person_id = HeadCoachPersonId)],
              FranchiseGames[, .(franchise_id, season = season + 1L, WinLower = FirstGame)],
              by = c("franchise_id", "season"), all.x = TRUE)
HWin <- merge(HWin, FranchiseGames[, .(franchise_id, season, WinUpper = FirstGame)],
              by = c("franchise_id", "season"))
HWin[is.na(WinLower), WinLower := as.Date("1998-01-01")]
HCand <- merge(HireDates[interim == 0L], HWin[!is.na(person_id)], by = c("franchise_id", "person_id"),
               allow.cartesian = TRUE)
HCand <- HCand[date > WinLower & date <= WinUpper]
HChosen <- HCand[, .(HCHireDate = if (any(date_kind == "hired_named"))
                       min(date[date_kind == "hired_named"]) else as.Date(NA),
                     HCAnnouncedDate = if (any(date_kind == "announced"))
                       min(date[date_kind == "announced"]) else as.Date(NA),
                     NHCHireDateCandidates = uniqueN(date),
                     HCHireDateSourceUrl = source_url[1],
                     HCHireDateQuote = sentence[order(date_kind != "hired_named", date)][1]),
                 by = .(franchise_id, season)]
Current <- merge(Current, HChosen, by = c("franchise_id", "season"), all.x = TRUE)
Current[is.na(NHCHireDateCandidates), NHCHireDateCandidates := 0L]
Current[, HCHireDateKind := fifelse(!is.na(HCHireDate), "hired_named",
                             fifelse(!is.na(HCAnnouncedDate), "announced_only", NA_character_))]

# Historical block (02d): 1989-1998 opening coaches with documented race
HistAvailable <- DBI::dbExistsTable(con, "nfl_hc_history")
if (HistAvailable) {
  Hist <- as.data.table(DBI::dbGetQuery(con, "SELECT * FROM nfl_hc_history"))
  need_cols(Hist, c("franchise_id", "season", "opening_coach_name", "wins", "losses", "ties",
                    "games", "win_pct", "n_head_coaches", "source_url", "coach_source_url",
                    "opening_coach_documented_black", "opening_coach_documented_minority",
                    "demographic_source_url", "demographic_basis"), "nfl_hc_history")
  Hist[, season := as.integer(season)]
  assert_true(!anyDuplicated(Hist[, .(franchise_id, season)]), "16: nfl_hc_history key not unique")
  # Last head coach of the season and his status, from the chronological
  # coach rows of 02d (nfl_hc_history_coaches: wiki_order, wiki_note) when
  # present; otherwise from head_coaches_all ('Name (W-L-T); ...'), with
  # the status unknown. A note containing "interim" documents an interim.
  if (DBI::dbExistsTable(con, "nfl_hc_history_coaches")) {
    HistCoaches <- as.data.table(DBI::dbGetQuery(con, "
      SELECT franchise_id, season, coach_name, wiki_order, wiki_note
      FROM nfl_hc_history_coaches"))
    HistCoaches[, season := as.integer(season)]
    setorder(HistCoaches, franchise_id, season, wiki_order)
    HistLast <- HistCoaches[, .(LastHCName = coach_name[.N],
                                LastHCInterim = fifelse(.N == 1L, NA_integer_,
                                  fifelse(grepl("interim", wiki_note[.N], ignore.case = TRUE), 1L,
                                          NA_integer_))),
                            by = .(franchise_id, season)]
    Hist <- merge(Hist, HistLast, by = c("franchise_id", "season"), all.x = TRUE)
    Hist[, LastHCStatusSource := "nfl_hc_history_coaches wiki_note"]
  } else if ("head_coaches_all" %in% names(Hist)) {
    Hist[, LastHCName := vapply(strsplit(head_coaches_all, ";"), function(parts) {
      p <- trimws(parts[length(parts)])
      sub(" \\([^)]*\\)$", "", p)
    }, character(1))]
    Hist[is.na(head_coaches_all) | !nzchar(head_coaches_all), LastHCName := NA_character_]
    Hist[, LastHCInterim := NA_integer_]
    Hist[, LastHCStatusSource := "head_coaches_all (status unknown)"]
  } else {
    Hist[, LastHCName := fifelse(n_head_coaches == 1L, opening_coach_name, NA_character_)]
    Hist[, LastHCInterim := NA_integer_]
    Hist[, LastHCStatusSource := "single-coach seasons only"]
  }
  assert_true(!anyNA(Hist$LastHCName), "16: nfl_hc_history season without a last head coach")
  Hist[, HCNameUncertain := if ("opening_coach_uncertain" %in% names(Hist))
    as.integer(opening_coach_uncertain) else 0L]
  Hist[, InSeasonHCChange := as.integer(n_head_coaches > 1L)]
  Historical <- Hist[, .(franchise_id, season, HeadCoachName = opening_coach_name,
                         HeadCoachPersonId = NA_character_, HCMatchMethod = NA_character_,
                         FirstHC = norm_name16(opening_coach_name),
                         LastHC = norm_name16(LastHCName), LastHCName, LastHCInterim,
                         InSeasonHCChange, NHeadCoaches = as.integer(n_head_coaches),
                         wins, losses, ties, games, WinPct = win_pct,
                         ExpectedWins = NA_real_, WinsOverExpected = NA_real_,
                         HistoricalCoverage = "historical", HCSourceUrl = coach_source_url,
                         HCNameUncertain, DocumentedBlack = as.integer(opening_coach_documented_black),
                         DocumentedMinority = as.integer(opening_coach_documented_minority),
                         DemographicSourceUrl = demographic_source_url,
                         DemographicBasis = demographic_basis)]
  message(sprintf("16: nfl_hc_history %d franchise-seasons %d-%d", nrow(Historical),
                  min(Historical$season), max(Historical$season)))
} else {
  message("16: nfl_hc_history not found: hires table has no 1990-1998 block (coverage notes it)")
  Historical <- data.table()
}

# Current-era documented race of the first-game HC
Current <- merge(Current, RaceDoc[, .(HeadCoachPersonId = person_id, DocumentedRace,
                                      DocumentedBlack, DocumentedMinority, DocumentedRaceSource)],
                 by = "HeadCoachPersonId", all.x = TRUE)
PredHC <- as.data.table(DBI::dbGetQuery(con, "
  SELECT entity_id AS HeadCoachPersonId, p_black_any_pred AS PBlackPred,
         p_white_pred AS PWhitePred, prior_black_pred AS PriorBlackPred
  FROM race_predicted WHERE entity = 'staff'"))
Current <- merge(Current, PredHC, by = "HeadCoachPersonId", all.x = TRUE)
Current[, `:=`(DemographicSourceUrl = fifelse(is.na(DocumentedRace), NA_character_,
                                              "race_predicted documented_* (04d/04e)"),
               DemographicBasis = fifelse(is.na(DocumentedRace), NA_character_,
                                          paste0("documented ", DocumentedRace, " (", DocumentedRaceSource, ")")))]
Current[, FirstHC := norm_name16(HeadCoachName)]
Current[, LastHC := norm_name16(LastHCName)]
Current[, FirstHCKey := HeadCoachPersonId]

Hires <- rbind(Current, Historical, fill = TRUE)
Hires[is.na(FirstHCKey), FirstHCKey := FirstHC]
setorder(Hires, franchise_id, season)
# Lags/leads over consecutive seasons within franchise. Across the
# 1998/1999 boundary the identity is a normalized-name comparison between
# the history source and nflverse (HireBoundaryMatch = 'cross_source_name').
Hires[, `:=`(LagSeason = shift(season), LagLastHC = shift(LastHC), LagFirstHC = shift(FirstHC),
             LagLastHCName = shift(LastHCName), LagLastHCInterim = shift(LastHCInterim),
             LagWinPct = shift(WinPct),
             LagExpectedWins = shift(ExpectedWins), LagWinsOverExpected = shift(WinsOverExpected),
             LeadSeason = shift(season, type = "lead"), LeadFirstHC = shift(FirstHC, type = "lead")),
      by = franchise_id]
Hires[LagSeason != season - 1L | is.na(LagSeason),
      `:=`(LagLastHC = NA_character_, LagFirstHC = NA_character_, LagLastHCName = NA_character_,
           LagLastHCInterim = NA_integer_,
           LagWinPct = NA_real_, LagExpectedWins = NA_real_, LagWinsOverExpected = NA_real_)]
Hires[LeadSeason != season + 1L | is.na(LeadSeason), LeadFirstHC := NA_character_]
Hires[, PrevSeasonObserved := as.integer(!is.na(LagLastHC))]
Hires[, BetweenHire := fifelse(is.na(LagLastHC), NA_integer_, as.integer(FirstHC != LagLastHC))]
# A first-game HC who took over during season - 1 and was kept represents an
# offseason hiring decision only when the previous appointment was documented
# as interim. A retained permanent in-season appointee was hired in season - 1.
# Unknown status gives IsHire = NA; it is never assumed to mean interim.
Hires[, RetainedLastHC := fifelse(is.na(LagLastHC) | is.na(LagFirstHC), NA_integer_,
                                  as.integer(FirstHC == LagLastHC & FirstHC != LagFirstHC))]
Hires[, RetainedInterim := fifelse(is.na(RetainedLastHC), NA_integer_,
                            fifelse(RetainedLastHC == 1L & !is.na(LagLastHCInterim) & LagLastHCInterim == 1L,
                                    1L, 0L))]
Hires[, RetainedPermanent := fifelse(is.na(RetainedLastHC), NA_integer_,
                              fifelse(RetainedLastHC == 1L & !is.na(LagLastHCInterim) & LagLastHCInterim == 0L,
                                      1L, 0L))]
Hires[, RetainedUnknownStatus := fifelse(is.na(RetainedLastHC), NA_integer_,
                                  as.integer(RetainedLastHC == 1L & is.na(LagLastHCInterim)))]
# Coach-franchise seasons (any REG game / any listed HC) for the return rule
CoachSeasons <- rbind(unique(GameReg[, .(franchise_id, season, HC = norm_name16(HeadCoachName))]),
                      if (nrow(Historical)) {
                        rbind(Historical[, .(franchise_id, season, HC = FirstHC)],
                              Historical[!is.na(LastHC), .(franchise_id, season, HC = LastHC)])
                      } else data.table())
CoachSeasons <- unique(CoachSeasons)
Earlier <- merge(Hires[, .(franchise_id, season, FirstHC, RetainedLastHC)],
                 CoachSeasons[, .(franchise_id, FirstHC = HC, seasonCoached = season)],
                 by = c("franchise_id", "FirstHC"), allow.cartesian = TRUE)
Earlier <- Earlier[seasonCoached >= season - 2L &
                     seasonCoached < season - fifelse(is.na(RetainedLastHC), 0L, RetainedLastHC)]
Earlier <- unique(Earlier[, .(franchise_id, season, EarlierSpell = 1L)])
Hires <- merge(Hires, Earlier, by = c("franchise_id", "season"), all.x = TRUE)
Hires[is.na(EarlierSpell), EarlierSpell := 0L]
Hires[, ReturnHire := fifelse(is.na(BetweenHire), NA_integer_,
                              as.integer((BetweenHire == 1L | RetainedInterim == 1L) & EarlierSpell == 1L))]
Hires[, StandIn := fifelse(is.na(BetweenHire), NA_integer_,
                           as.integer(BetweenHire == 1L & !is.na(LeadFirstHC) & LagLastHC == LeadFirstHC))]
Hires[, IsHire := fifelse(is.na(BetweenHire), NA_integer_,
                   fifelse(RetainedUnknownStatus == 1L, NA_integer_,
                           as.integer((BetweenHire == 1L | RetainedInterim == 1L) &
                                        ReturnHire == 0L & StandIn == 0L)))]
Hires[, HireType := fifelse(is.na(BetweenHire), "unknown_prev_season",
                     fifelse(StandIn == 1L, "stand_in",
                      fifelse(ReturnHire == 1L, "return",
                       fifelse(RetainedInterim == 1L, "retained_interim",
                        fifelse(RetainedPermanent == 1L, "midseason_hire_retained",
                         fifelse(RetainedUnknownStatus == 1L, "retained_unknown_status",
                          fifelse(BetweenHire == 1L, "between_season", "none")))))))]
Hires[, HireBoundaryMatch := fifelse(season == 1999L & PrevSeasonObserved == 1L, "cross_source_name",
                              fifelse(HistoricalCoverage == "historical", "history_name", "game_data_name"))]
# Historical rows: the 02d assignment uncertainty flag propagates to IsHire
Hires[HCNameUncertain == 1L, `:=`(IsHire = NA_integer_, HireType = "uncertain_opening_coach")]
# Documented-positive comparison indicators (explicit ascertainment
# assumption: undocumented = not documented positive; NOT white)
Hires[, HCRaceDocumented := as.integer(!is.na(DocumentedBlack) | !is.na(DocumentedMinority))]
Hires[, HCDocPositiveBlack := as.integer(!is.na(DocumentedBlack) & DocumentedBlack == 1L)]
Hires[, HCDocPositiveMinority := as.integer(!is.na(DocumentedMinority) & DocumentedMinority == 1L)]
Hires[, HCSpellId := fifelse(is.na(HCSpellId), sprintf("H|%s|%s", franchise_id, FirstHC), HCSpellId)]
Hires <- Hires[season >= 1990L]
Hires <- add_rooney_policies(Hires, "season", timing = "offseason_hire")
Hires <- Hires[, .(franchise_id, season, HistoricalCoverage, HeadCoachName, HeadCoachPersonId,
                   HCMatchMethod, HCNameUncertain, HCSpellId, HCSpellSeason, LastHCName,
                   PrevSeasonObserved, HireBoundaryMatch, IsHire, HireType, BetweenHire,
                   RetainedLastHC, LagLastHCInterim, RetainedInterim, RetainedPermanent,
                   RetainedUnknownStatus, ReturnHire, StandIn, InSeasonHCChange, NHeadCoaches,
                   wins, losses, ties, games, WinPct, ExpectedWins, WinsOverExpected,
                   LagWinPct, LagExpectedWins, LagWinsOverExpected,
                   DocumentedRace, DocumentedBlack, DocumentedMinority, HCRaceDocumented,
                   HCDocPositiveBlack, HCDocPositiveMinority, DocumentedRaceSource,
                   DemographicSourceUrl, DemographicBasis, PBlackPred, PWhitePred, PriorBlackPred,
                   HCHireDate, HCAnnouncedDate, HCHireDateKind, NHCHireDateCandidates,
                   HCHireDateSourceUrl, HCHireDateQuote,
                   HCSourceUrl, RooneyRule, RooneyFrontOffice2009, RooneyAmend2020,
                   RooneyAmend2022, RooneyCompensatory2021, OffensiveAssistantMandate,
                   OffensiveAssistantSubsidy, OffensiveAssistantVoluntary2025, RooneyEra,
                   PolicyTimingConvention)]
Hires[HistoricalCoverage == "historical", HCSpellSeason := NA_integer_]
setorder(Hires, franchise_id, season)
assert_true(all(Hires$games[Hires$HistoricalCoverage == "current"] > 0, na.rm = TRUE),
            "16: current-era hire row without games")
message(sprintf("16: hires panel %d franchise-seasons (%d current, %d historical); IsHire = 1 in %d, unknown in %d",
                nrow(Hires), sum(Hires$HistoricalCoverage == "current"),
                sum(Hires$HistoricalCoverage == "historical"), sum(Hires$IsHire == 1L, na.rm = TRUE),
                sum(is.na(Hires$IsHire))))
print(table(Hires$HireType, Hires$HistoricalCoverage))

HiresLabels <- c(
  franchise_id = "Franchise identifier (current conventions; relocated franchises included)",
  season = "NFL season (1990-2025; 1989 enters only as a lag)",
  HistoricalCoverage = "current (1999+, nflverse game head coaches) or historical (1990-1998, nfl_hc_history)",
  HeadCoachName = "Head coach of the franchise's first REG game (current) or opening head coach (historical)",
  HeadCoachPersonId = "Staff person_id of the first-game HC (NA historical / unmatched)",
  HCMatchMethod = "How the name was matched to a staff person (match_hc_person)",
  HCNameUncertain = "Historical: 02d flags the opening-coach assignment as uncertain (IsHire set to NA)",
  HCSpellId = "Any-REG-game franchise run key of the first-game HC: franchise|person|first season (current; 'H|' name-keyed historical)",
  HCSpellSeason = "Season number within the HC spell (1 = first season; current era only)",
  LastHCName = "Head coach of the franchise's last REG game of the season",
  PrevSeasonObserved = "Previous season's last head coach known (hire indicators defined)",
  HireBoundaryMatch = "Identity comparison with season - 1: game_data_name (normalized nflverse names within the game data), history_name (within 02d names), cross_source_name (1999 vs 1998 normalized names)",
  IsHire = "New initial head coach for the season: between-season change or retained documented interim, excluding returns after <= 2 seasons and stand-ins; NA when season - 1 is unknown, the opening coach is uncertain, or a retained in-season successor's status is undocumented",
  HireType = "between_season, retained_interim, midseason_hire_retained (permanent in-season appointee kept; hired in season - 1), retained_unknown_status, return, stand_in, none, unknown_prev_season, uncertain_opening_coach",
  BetweenHire = "First-game HC differs from the previous season's last-game HC",
  RetainedLastHC = "First-game HC was the previous season's last-game HC but not its first-game HC (took over in season - 1)",
  LagLastHCInterim = "Status of the previous season's last HC: 1 = listed as interim (staff tables / 02d note), 0 = listed as HC without interim flag, NA = undocumented",
  RetainedInterim = "RetainedLastHC and the previous season's last HC was a documented interim (an offseason hiring decision)",
  RetainedPermanent = "RetainedLastHC and the previous season's last HC was a documented permanent in-season appointment (not an offseason hire)",
  RetainedUnknownStatus = "RetainedLastHC with undocumented status (IsHire = NA)",
  ReturnHire = "The new HC coached the franchise in season - 2 or season - 1 (short absence)",
  StandIn = "Previous season's last HC returns at the first game of season + 1",
  InSeasonHCChange = "More than one head coach during the REG season",
  NHeadCoaches = "Distinct head coaches in the REG season",
  wins = "REG wins", losses = "REG losses", ties = "REG ties", games = "REG games",
  WinPct = "REG win percentage (ties = half win); team-season outcome under the initial hire (intention-to-initial-hire)",
  ExpectedWins = "Betting-market expected wins (NA before 1999)",
  WinsOverExpected = "Wins minus expected wins (NA before 1999)",
  LagWinPct = "Previous season's win percentage (consecutive seasons only)",
  LagExpectedWins = "Previous season's expected wins", LagWinsOverExpected = "Previous season's wins over expected",
  DocumentedRace = "Documented race category of the first-game HC (NA = undocumented, not white)",
  DocumentedBlack = "Documented Black (0/1/NA)", DocumentedMinority = "Documented NFL-definition minority (0/1/NA)",
  HCRaceDocumented = "Race of the head coach is documented",
  HCDocPositiveBlack = "Documented Black vs ALL OTHER head coaches (undocumented treated as not documented positive, NOT as white); benchmark against TIDES in coach_policy_coverage.csv",
  HCDocPositiveMinority = "Documented minority vs ALL OTHER head coaches (same ascertainment convention)",
  DocumentedRaceSource = "Source types (current era)",
  DemographicSourceUrl = "Source of the documented race (historical rows: 02d URL)",
  DemographicBasis = "Basis of the documented race",
  PBlackPred = "Predicted P(Black) of the HC (current era; sensitivity)",
  PWhitePred = "Predicted P(white)", PriorBlackPred = "Prior P(Black)",
  HCHireDate = "Current era: article-stated hired/named/signed date of the first-game HC's appointment by this franchise, inside (first REG game of season - 1, first REG game of season]; NA = not sourced",
  HCAnnouncedDate = "Article-stated introduction/announcement date (same window)",
  HCHireDateKind = "hired_named, announced_only or NA",
  NHCHireDateCandidates = "Distinct candidate dates in the window",
  HCHireDateSourceUrl = "Wikipedia revision permalink", HCHireDateQuote = "Sentence stating the date",
  HCSourceUrl = "Source of the head-coach assignment",
  RooneyRule = "Policy flag, offseason-hire timing (00-policy-functions.R)",
  RooneyFrontOffice2009 = "Policy flag", RooneyAmend2020 = "Policy flag", RooneyAmend2022 = "Policy flag",
  RooneyCompensatory2021 = "Policy flag", OffensiveAssistantMandate = "Policy flag",
  OffensiveAssistantSubsidy = "Policy flag", OffensiveAssistantVoluntary2025 = "Policy flag",
  RooneyEra = "Calendar policy cohort", PolicyTimingConvention = "offseason_hire"
)
write_sample(as.data.frame(Hires), "analysis_rooney_hires", key = c("franchise_id", "season"),
             labels = HiresLabels)

# ---------------------------------------------------------------------------
# Coverage: missing eligibility / dates / participation / outcomes, historical
# coverage and the TIDES head-coach benchmark
# ---------------------------------------------------------------------------

# One coverage row per block x season x unit x item; `season` always comes
# from the grouping, so cov_row() never returns it itself
cov_row <- function(block, unit, item, n_total, n_known, n_positive, note = "") {
  data.table(block = block, unit = unit, item = item,
             n_total = as.integer(n_total), n_known = as.integer(n_known),
             n_positive = as.integer(n_positive),
             share_known = ifelse(n_total > 0, n_known / n_total, NA_real_), note = note)
}
Coverage <- list()
# Person-season eligibility, gender, race, participation by season x unit
for (u in c("offense", "defense", "all_coaches")) {
  d <- if (u == "all_coaches") PS else UnitCoaches[Unit == u]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "eligibility", u, "PolicyEligible", .N, sum(!is.na(PolicyEligible)),
    sum_na(PolicyEligible == 1L), "1 = documented woman/minority/league-designated; NA = unknown"),
    by = season]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "eligibility", u, "DocumentedRace", .N, sum(!is.na(DocumentedRace)),
    sum_na(DocumentedMinority == 1L), "documented race coverage (Wikidata/category/text); n_positive = documented minority"),
    by = season]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "eligibility", u, "DocumentedWoman", .N, sum(!is.na(DocumentedWoman)),
    sum_na(DocumentedWoman == 1L), "Wikidata P21 or Wikipedia women's category"),
    by = season]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "participation", u, "ProgramParticipant", .N, .N, sum_na(ProgramParticipant == 1L),
    "league-published lists only: Accelerator 2022-2024 cohorts; 2 named mandate hires (ESPN); the league published no 32-club mandate roster"),
    by = season]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "outcomes", u, "RetainedNext1", .N, sum(!is.na(RetainedNext1)), sum_na(RetainedNext1 == 1L),
    "NA = next opening snapshot unobserved (right-censored)"), by = season]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "outcomes", u, "PromotedNext2", .N, sum(!is.na(PromotedNext2)), sum_na(PromotedNext2 == 1L),
    "NA = opening snapshot at t+2 unobserved"), by = season]
  Coverage[[length(Coverage) + 1]] <- d[, cov_row(
    "outcomes", u, "NewToFranchise", .N, sum(!is.na(NewToFranchise)), sum_na(NewToFranchise == 1L),
    "NA = previous opening snapshot unobserved"), by = season]
}
# Dates
Coverage[[length(Coverage) + 1]] <- Spells[, cov_row(
  "dates", "spells", "EntryBoundsKnown", .N, sum(EntryLeftCensored == 0L), NA_integer_,
  "spells with a lower entry bound (not left-censored)"),
  by = .(season = FirstSeason)]
Coverage[[length(Coverage) + 1]] <- Spells[, cov_row(
  "dates", "spells", "ExitBoundsKnown", .N, sum(ExitRightCensored == 0L),
  sum(!is.na(ActualDepartureDate)),
  "spells with an upper exit bound; n_positive = article-stated dismissal dates (HC only)"),
  by = .(season = LastSeason)]
Coverage[[length(Coverage) + 1]] <- Spells[role_std == "HC", cov_row(
  "dates", "hc_spells", "HCVerifiedSpell", .N, .N, sum_na(HCVerifiedSpell == 1L),
  "HC role spells confirmed by REG game data in every spell season"),
  by = .(season = FirstSeason)]
Coverage[[length(Coverage) + 1]] <- Spells[role_std == "HC", cov_row(
  "dates", "hc_spells", "ActualHireDate", .N, sum(!is.na(ActualHireDate)),
  sum(is.na(ActualHireDate) & !is.na(AnnouncementDate)),
  "HC spells with an article-stated appointment date inside the hiring window (02e); n_positive = announcement date only"),
  by = .(season = FirstSeason)]
Coverage[[length(Coverage) + 1]] <- Hires[HistoricalCoverage == "current" & !is.na(IsHire) & IsHire == 1L, cov_row(
  "dates", "hires", "HCHireDate", .N, sum(!is.na(HCHireDate)),
  sum(is.na(HCHireDate) & !is.na(HCAnnouncedDate)),
  "hires (IsHire = 1) with an article-stated appointment date; n_positive = announcement date only; historical rows have no sourced dates"),
  by = season]
# Team-seasons without an opening snapshot
Unobs <- OpeningCov[OpeningStaffObserved == 0L]
if (nrow(Unobs)) {
  Coverage[[length(Coverage) + 1]] <- Unobs[, cov_row(
    "opening_snapshots", "team_seasons", "UnobservedOpening", .N, 0L, NA_integer_,
    paste(franchise_id, collapse = "|")), by = season]
}
# Hires
Coverage[[length(Coverage) + 1]] <- Hires[, cov_row(
  "hires", HistoricalCoverage, "IsHire", .N, sum(!is.na(IsHire)), sum_na(IsHire == 1L),
  "NA = previous season's head coach unknown or opening coach uncertain"),
  by = .(season, HistoricalCoverage)][, HistoricalCoverage := NULL]
Coverage[[length(Coverage) + 1]] <- Hires[, cov_row(
  "hires", HistoricalCoverage, "HCRaceDocumented", .N, sum(HCRaceDocumented),
  sum(HCDocPositiveBlack), "n_positive = documented Black opening HCs; undocumented are NOT white"),
  by = .(season, HistoricalCoverage)][, HistoricalCoverage := NULL]
Coverage[[length(Coverage) + 1]] <- Hires[, cov_row(
  "hires", HistoricalCoverage, "LagWinPct", .N, sum(!is.na(LagWinPct)), NA_integer_,
  "previous-season record available (1990 needs 1989 history rows)"),
  by = .(season, HistoricalCoverage)][, HistoricalCoverage := NULL]
if (!HistAvailable) {
  Coverage[[length(Coverage) + 1]] <- cbind(
    season = NA_integer_,
    cov_row("hires", "historical", "nfl_hc_history", 0L, 0L, NA_integer_,
            "DuckDB table nfl_hc_history absent: no 1990-1998 rows"))
}
# TIDES benchmark: documented-positive opening HC counts vs TIDES counts
# (one TIDES observation per season x category: the report of the same
# year when available, else the earliest later report)
Tides <- as.data.table(read.csv(file.path(root, "data", "reference", "tides_nfl_race_shares.csv"),
                                stringsAsFactors = FALSE, na.strings = ""))
TidesHC <- Tides[group == "head_coaches" & unit == "count" & category %in% c("black", "people_of_color")]
TidesHC[, SameYear := as.integer(report_year == season)]
setorder(TidesHC, season, category, -SameYear, report_year)
TidesHC <- TidesHC[, .SD[1L], by = .(season, category)]
OursHC <- Hires[, .(n_total = .N, n_doc_black = sum(HCDocPositiveBlack),
                    n_doc_minority = sum(HCDocPositiveMinority)), by = season]
Bench <- merge(TidesHC[, .(season, category, TidesCount = as.integer(value), TidesAsOf = as_of,
                           TidesReport = report_year)],
               OursHC, by = "season")
Bench[, Ours := fifelse(category == "black", n_doc_black, n_doc_minority)]
Coverage[[length(Coverage) + 1]] <- Bench[, cov_row(
  "hc_tides_benchmark", category, "DocumentedPositiveVsTIDES", n_total, Ours, TidesCount,
  sprintf("n_known = our documented-positive opening HCs; n_positive = TIDES count (report %s, as of %s); shortfall = undocumented, not white",
          TidesReport, ifelse(is.na(TidesAsOf), "season", TidesAsOf))),
  by = .(season, category)][, category := NULL]
Coverage <- rbindlist(Coverage, use.names = TRUE, fill = TRUE)
setcolorder(Coverage, c("block", "season", "unit", "item", "n_total", "n_known", "n_positive",
                        "share_known", "note"))
setorder(Coverage, block, item, unit, season)
write.csv(Coverage, file.path(analysis, "coach_policy_coverage.csv"), row.names = FALSE, na = "")
message(sprintf("16: coverage file %d rows", nrow(Coverage)))
print(Bench[, .(season, category, ours = fifelse(category == "black", n_doc_black, n_doc_minority),
                TidesCount)])

db_disconnect(con)

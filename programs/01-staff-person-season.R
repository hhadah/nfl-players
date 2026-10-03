# ============================================================================
# 01-staff-person-season.R
# Builds two person-season panels from the Wikipedia staff boxes (table
# staff_team_season, one row per franchise x season x person x role):
#   - analysis/staff_person_season: one row per franchise_id x season x
#     person_id (1999-2025) for every coach and front-office member listed in
#     ANY snapshot of the season (staff template revisions 2007-2025, season
#     articles 1999-2006). The union of snapshots: in-season hires, interim
#     promotions and firings all leave a row, so flags such as IsHeadCoach
#     mean "held the role at some point in the season". Descriptive and
#     robustness use only.
#   - analysis/staff_person_opening_season: the same shape, 2007-2025, built
#     ONLY from the role rows listed in the opening snapshot (the template
#     revision in force at 00:00 UTC on the franchise's first REG game date,
#     staff_snapshots.snapshot == 'preseason'). Primary role, role flags,
#     unit, tenure and turnover (NewToFranchise, PromotedWithinFranchise)
#     compare adjacent opening snapshots, so they are predetermined with
#     respect to the season's results. 2007 is the first opening snapshot:
#     turnover relative to 2006 is unknown (NA), not zero. The pre-2007
#     season-article boxes are written after the fact and are not an opening
#     measure, so they are excluded. This is the input for the opening-day
#     composition (02), the policy job transitions (16) and the models (14).
#   - analysis/staff_opening_coverage: franchise x season (2007-2025) opening
#     snapshot provenance (target date, revision, staleness, parse status) and
#     headcounts, so readers can tell zero listed coaches from an unobserved
#     opening snapshot without the DuckDB.
# Both person panels add the person's primary role (priority order below),
# all roles, role flags, unit, snapshot flags, tenure/mobility measures and
# race measures from load_person_race(): hand-coded, provisional (hand code >
# Wikipedia category), BIFSG, and predicted race (primary measure while hand
# codes are absent: model-only *_pred, documented sensitivity variant
# *_preddoc, and the predetermined covariates of the prior;
# notes/race-prediction-design.md).
# Date: 2026-09-26; predicted race added 2026-10-02; opening panel 2026-10-03
# ============================================================================

# Primary-role priority (lower = higher priority). Coaching roles rank above
# front-office roles, so a head coach who is also GM gets role HC (IsGM stays
# TRUE). RoleTier is the level in the hierarchy used to define promotions.
RolePriority <- tribble(
  ~role_std,              ~Priority, ~RoleTier, ~Domain,
  "HC",                   1,         1,         "coach",
  "ASST_HC",              2,         2,         "coach",
  "OC",                   3,         3,         "coach",
  "DC",                   4,         3,         "coach",
  "STC",                  5,         3,         "coach",
  "PASS_GAME_COORD",      6,         4,         "coach",
  "RUN_GAME_COORD",       7,         4,         "coach",
  "QB",                   10,        5,         "coach",
  "OL",                   11,        5,         "coach",
  "WR",                   12,        5,         "coach",
  "RB",                   13,        5,         "coach",
  "TE",                   14,        5,         "coach",
  "DL",                   15,        5,         "coach",
  "EDGE_OLB",             16,        5,         "coach",
  "LB",                   17,        5,         "coach",
  "ILB",                  18,        5,         "coach",
  "DB",                   19,        5,         "coach",
  "CB",                   20,        5,         "coach",
  "S",                    21,        5,         "coach",
  "NICKEL",               22,        5,         "coach",
  "OFF_ASST",             30,        6,         "coach",
  "DEF_ASST",             31,        6,         "coach",
  "ST_ASST",              32,        6,         "coach",
  "QC_OFF",               33,        6,         "coach",
  "QC_DEF",               34,        6,         "coach",
  "COACH_ASST",           35,        6,         "coach",
  "S_AND_C",              40,        7,         "coach",
  "OTHER_COACH",          45,        8,         "coach",
  "OWNER",                50,        1,         "front_office",
  "CHAIR",                51,        1,         "front_office",
  "CEO",                  52,        1,         "front_office",
  "PRESIDENT",            53,        1,         "front_office",
  "VICE_CHAIR",           54,        1,         "front_office",
  "GM",                   60,        2,         "front_office",
  "FOOTBALL_OPS_EXEC",    61,        3,         "front_office",
  "ASST_GM",              62,        3,         "front_office",
  "VP_PLAYER_PERSONNEL",  70,        4,         "front_office",
  "DIR_PLAYER_PERSONNEL", 71,        4,         "front_office",
  "DIR_PRO_PERSONNEL",    72,        4,         "front_office",
  "DIR_COLLEGE_SCOUTING", 73,        5,         "front_office",
  "SCOUT",                74,        5,         "front_office",
  "CAP_ADMIN",            80,        6,         "front_office",
  "OTHER_FO",             81,        6,         "front_office"
)

con <- db_connect()

# Person x role rows; every role_std must have a priority
StaffRoles <- tbl(con, "staff_team_season") |>
  filter(season >= 1999, season <= 2025) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  left_join(RolePriority, by = "role_std")
if (anyNA(StaffRoles$Priority)) {
  stop("role_std without a priority: ",
       paste(unique(StaffRoles$role_std[is.na(StaffRoles$Priority)]), collapse = ", "))
}

# Opening (preseason) template snapshots: provenance per franchise-season.
# target_date is 00:00 UTC on the franchise's first REG game date and the
# revision used was saved at or before it (asserted in scripts/02b and here).
OpeningSnapshots <- tbl(con, "staff_snapshots") |>
  filter(source == "staff_template", snapshot == "preseason") |>
  select(franchise_id, season, target_date, revision_timestamp, revid, days_stale,
         parse_ok) |>
  collect() |>
  transmute(franchise_id, season = as.integer(season),
            OpeningStaffObserved = parse_ok,
            OpeningTargetDate = as.Date(target_date),
            OpeningRevisionTimestamp = lubridate::with_tz(revision_timestamp, "UTC"),
            OpeningRevisionId = as.integer(revid),
            OpeningDaysStale = as.integer(days_stale))
if (any(as.Date(OpeningSnapshots$OpeningRevisionTimestamp, tz = "UTC") >
        OpeningSnapshots$OpeningTargetDate)) {
  stop("An opening snapshot revision was saved after its target date (scripts/02b)")
}
OpeningFirstSeason <- min(OpeningSnapshots$season)

coach_groups <- c("head_coach", "coordinator", "position_coach", "assistant_coach")
fo_groups <- c("owner_executive", "general_manager", "personnel_scouting",
               "other_front_office")

# Collapse person x role rows to one row per franchise x season x person:
# primary role = the highest-priority role among `roles`; flags are "any role
# of this type" among `roles`. Which role rows are passed in defines the
# measure (every snapshot of the season, or the opening snapshot only).
collapse_person_season <- function(roles) {
  roles |>
    arrange(franchise_id, season, person_id, Priority) |>
    group_by(franchise_id, season, person_id) |>
    summarise(person_name = first(person_name),
              PrimaryRole = first(role_std),
              PrimaryRoleGroup = first(role_group),
              PrimaryUnit = first(unit),
              RoleTier = first(RoleTier),
              Domain = first(Domain),
              AllRoles = paste(unique(role_std), collapse = "|"),
              NRoles = n_distinct(role_std),
              IsHeadCoach = any(role_std == "HC"),
              IsInterimHC = any(role_std == "HC" & interim_any),
              IsInterimAnyRole = any(interim_any),
              IsCoach = any(role_group %in% coach_groups),
              IsCoordinator = any(role_std %in% c("OC", "DC", "STC")),
              IsOC = any(role_std == "OC"),
              IsDC = any(role_std == "DC"),
              IsSTC = any(role_std == "STC"),
              IsOtherCoordinator = any(role_std %in% c("ASST_HC", "PASS_GAME_COORD",
                                                       "RUN_GAME_COORD")),
              IsPositionCoach = any(role_group == "position_coach"),
              IsAssistantCoach = any(role_group == "assistant_coach"),
              IsStrengthCond = any(role_group == "strength_conditioning"),
              IsSupportStaff = any(role_group == "support_staff"),
              IsFrontOffice = any(role_group %in% fo_groups),
              IsGM = any(role_std == "GM"),
              IsOwnerExec = any(role_group == "owner_executive"),
              IsPersonnelScouting = any(role_group == "personnel_scouting"),
              IsOffenseCoach = any(role_group %in% coach_groups & unit == "offense"),
              IsDefenseCoach = any(role_group %in% coach_groups & unit == "defense"),
              IsSpecialTeamsCoach = any(role_group %in% coach_groups &
                                          unit == "special_teams"),
              in_preseason = any(in_preseason),
              in_midseason = any(in_midseason),
              in_late = any(in_late),
              in_season_article = any(in_season_article),
              StaffSource = first(source),
              .groups = "drop") |>
    # (the primary role is renamed only now: inside summarise() a new role_std
    # would mask the person's other roles in the flags above)
    rename(role_std = PrimaryRole) |>
    mutate(Unit = case_when(PrimaryUnit %in% c("offense", "defense", "special_teams",
                                               "front_office") ~ PrimaryUnit,
                            TRUE ~ "general")) |>
    select(-PrimaryUnit)
}

# Tenure, career and turnover measures of a person-season panel `df`.
# `observed` (franchise_id, season) lists the team-seasons whose staff box is
# observed in the panel's measure: NewToFranchise and PromotedWithinFranchise
# compare season with season - 1 and are NA when season - 1 is unobserved
# (the first season of the panel, `first_season`, and any gap); the run with
# the franchise is left-censored when it starts in first_season.
add_mobility <- function(df, observed, first_season) {
  PrevObserved <- observed |>
    transmute(franchise_id, season = season + 1L, PrevTeamSeasonObserved = TRUE)
  # Previous season's primary role of the same person at the same franchise
  PrevRole <- df |>
    transmute(franchise_id, person_id, season = season + 1L,
              PrevRoleTier = RoleTier, PrevDomain = Domain, OnStaffPrevSeason = TRUE)
  # Person career: franchises in each season, cumulative seasons on any staff,
  # and the franchise(s) of the person's most recent earlier staff season
  PersonCareer <- df |>
    group_by(person_id, season) |>
    summarise(SeasonFranchises = paste(sort(unique(franchise_id)), collapse = "|"),
              .groups = "drop") |>
    arrange(person_id, season) |>
    group_by(person_id) |>
    mutate(SeasonsOnStaffAnyTeam = row_number(),
           PrevFranchise = lag(SeasonFranchises),
           PrevStaffSeason = lag(season)) |>
    ungroup() |>
    select(person_id, season, SeasonsOnStaffAnyTeam, PrevFranchise, PrevStaffSeason)
  df |>
    # Consecutive seasons with the franchise, including the current one
    arrange(person_id, franchise_id, season) |>
    group_by(person_id, franchise_id) |>
    mutate(SpellId = cumsum(season - lag(season, default = first(season) - 2L) != 1L)) |>
    group_by(person_id, franchise_id, SpellId) |>
    mutate(SeasonsWithFranchise = row_number(),
           TenureLeftCensored = first(season) == first_season) |>
    ungroup() |>
    select(-SpellId) |>
    left_join(PrevObserved, by = c("franchise_id", "season")) |>
    left_join(PrevRole, by = c("franchise_id", "person_id", "season")) |>
    left_join(PersonCareer, by = c("person_id", "season")) |>
    mutate(PrevTeamSeasonObserved = coalesce(PrevTeamSeasonObserved, FALSE),
           OnStaffPrevSeason = coalesce(OnStaffPrevSeason, FALSE),
           NewToFranchise = case_when(!PrevTeamSeasonObserved ~ NA,
                                      TRUE ~ !OnStaffPrevSeason),
           PromotedWithinFranchise = case_when(!PrevTeamSeasonObserved ~ NA,
                                               !OnStaffPrevSeason ~ FALSE,
                                               PrevDomain != Domain ~ FALSE,
                                               TRUE ~ RoleTier < PrevRoleTier)) |>
    select(-PrevRoleTier, -PrevDomain, -OnStaffPrevSeason)
}

# ---------------------------------------------------------------------------
# Union panel: every snapshot of the season
# ---------------------------------------------------------------------------

StaffPersonSeason <- collapse_person_season(StaffRoles)

# Team-seasons whose staff box was observed: someone is listed in a parsed
# snapshot (season article or template revision). In 22 article-era
# team-seasons only the infobox (HC, GM, owner; 2-4 rows) is observed, so they
# count as unobserved.
ObservedTeamSeasons <- StaffPersonSeason |>
  filter(in_season_article | in_preseason | in_midseason | in_late) |>
  distinct(franchise_id, season)
message("Team-seasons with a parsed staff box: ", nrow(ObservedTeamSeasons), " of ",
        nrow(distinct(StaffPersonSeason, franchise_id, season)), " with any staff row")

StaffPersonSeason <- add_mobility(StaffPersonSeason, ObservedTeamSeasons, 1999L)

# ---------------------------------------------------------------------------
# Opening panel: role rows listed in the opening snapshot only (2007+)
# ---------------------------------------------------------------------------

# Role rows from the raw opening-revision entries (staff_entries, snapshot
# 'preseason'), not from staff_team_season: its in_preseason flag is
# role-specific, but its interim_any, unit and role_group pool every snapshot
# of the season (an interim tag or a unit change acquired later in the season
# would leak into the opening measure). Person names, snapshot flags and the
# role priority are joined back by the role key; every opening entry must
# have a staff_team_season row.
OpeningEntries <- tbl(con, "staff_entries") |>
  filter(snapshot == "preseason", source == "staff_template", !is.na(person_id),
         !vacant) |>
  select(franchise_id, season, person_id, role_std, role_group, unit, interim,
         is_primary_role) |>
  collect() |>
  mutate(season = as.integer(season)) |>
  arrange(franchise_id, season, person_id, role_std, desc(is_primary_role)) |>
  group_by(franchise_id, season, person_id, role_std) |>
  summarise(role_group = first(role_group), unit = first(unit),
            interim_any = any(interim), .groups = "drop")
OpeningRoles <- OpeningEntries |>
  inner_join(StaffRoles |>
               select(franchise_id, season, person_id, role_std, person_name, source,
                      in_preseason, in_midseason, in_late, in_season_article,
                      Priority, RoleTier, Domain),
             by = c("franchise_id", "season", "person_id", "role_std")) |>
  filter(season >= OpeningFirstSeason)
if (nrow(OpeningRoles) != nrow(OpeningEntries) || !all(OpeningRoles$in_preseason)) {
  stop("Opening entries (staff_entries, snapshot preseason) do not match the ",
       "in_preseason role rows of staff_team_season")
}
StaffPersonOpeningSeason <- collapse_person_season(OpeningRoles)

# Observed opening snapshots: parsed template revisions. Turnover in the
# first template season compares with an unobserved 2006 opening, so it is NA.
ObservedOpenings <- OpeningSnapshots |>
  filter(OpeningStaffObserved) |>
  select(franchise_id, season)
StaffPersonOpeningSeason <- StaffPersonOpeningSeason |>
  add_mobility(ObservedOpenings, OpeningFirstSeason) |>
  left_join(OpeningSnapshots |> select(-OpeningStaffObserved),
            by = c("franchise_id", "season"))

# Roles held only after the opener must not appear: every row comes from
# opening role rows, and the panel is no larger than the union panel
if (!all(StaffPersonOpeningSeason$in_preseason) ||
    !all(StaffPersonOpeningSeason$StaffSource == "staff_template") ||
    anyNA(StaffPersonOpeningSeason$OpeningTargetDate)) {
  stop("staff_person_opening_season contains rows outside the opening snapshots")
}
OpeningVsUnion <- StaffPersonOpeningSeason |>
  select(franchise_id, season, person_id, OpeningRole = role_std) |>
  left_join(StaffPersonSeason |> select(franchise_id, season, person_id, AllRoles),
            by = c("franchise_id", "season", "person_id"))
if (anyNA(OpeningVsUnion$AllRoles) ||
    !all(map2_lgl(OpeningVsUnion$OpeningRole, OpeningVsUnion$AllRoles,
                  \(r, all) r %in% str_split_1(all, fixed("|"))))) {
  stop("An opening-panel primary role is not among the person's season roles")
}
message("Opening panel: ", nrow(StaffPersonOpeningSeason), " person-seasons (",
        n_distinct(paste(StaffPersonOpeningSeason$franchise_id,
                         StaffPersonOpeningSeason$season)), " franchise-seasons); ",
        "union panel in the same seasons: ",
        sum(StaffPersonSeason$season >= OpeningFirstSeason), "; coaches whose union ",
        "primary role differs from the opening primary role: ",
        StaffPersonOpeningSeason |>
          select(franchise_id, season, person_id, OpeningRole = role_std, IsCoach) |>
          inner_join(StaffPersonSeason |> select(franchise_id, season, person_id, role_std),
                     by = c("franchise_id", "season", "person_id")) |>
          filter(IsCoach, OpeningRole != role_std) |> nrow())

# ---------------------------------------------------------------------------
# Race measures (person_uid = 'staff:<person_id>')
# ---------------------------------------------------------------------------

PersonRace <- load_person_race(con, hand_coded) |>
  filter(entity == "staff") |>
  select(person_id, race, hispanic, black_any, nonwhite, race_source,
         black_provisional, black_provisional_source, wiki_cat_black,
         wiki_cat_hispanic_latino, p_white_bifsg, p_black_bifsg, p_hispanic_bifsg,
         p_api_bifsg, p_aian_bifsg, p_multi_bifsg, race_bifsg,
         p_black_any_pred, p_white_pred, p_black_pred, p_hispanic_pred, p_api_pred,
         p_aian_pred, p_multi_pred, prior_black_pred, p_black_any_preddoc,
         p_white_preddoc, p_black_preddoc, p_hispanic_preddoc, p_api_preddoc,
         p_aian_preddoc, p_multi_preddoc, documented_black_any,
         pred_role_group_first, pred_unit_first, pred_first_era, pred_former_player)

finish_panel <- function(df, extra = NULL) {
  df |>
    left_join(PersonRace, by = "person_id") |>
    select(franchise_id, season, person_id, person_name, role_std, PrimaryRoleGroup,
           RoleTier, Domain, Unit, AllRoles, NRoles, starts_with("Is"),
           in_preseason, in_midseason, in_late, in_season_article, StaffSource,
           all_of(extra),
           SeasonsWithFranchise, TenureLeftCensored, SeasonsOnStaffAnyTeam,
           PrevStaffSeason, PrevFranchise, PrevTeamSeasonObserved, NewToFranchise,
           PromotedWithinFranchise, race:pred_former_player) |>
    arrange(franchise_id, season, RoleTier, person_id)
}
OpeningExtra <- c("OpeningTargetDate", "OpeningRevisionTimestamp", "OpeningRevisionId",
                  "OpeningDaysStale")
StaffPersonSeason <- finish_panel(StaffPersonSeason)
StaffPersonOpeningSeason <- finish_panel(StaffPersonOpeningSeason, OpeningExtra)

# ---------------------------------------------------------------------------
# Opening-snapshot coverage: franchise x season, 2007-2025
# ---------------------------------------------------------------------------

StaffOpeningCoverage <- OpeningSnapshots |>
  left_join(StaffPersonOpeningSeason |>
              group_by(franchise_id, season) |>
              summarise(NPersonsOpening = n(), NCoachesOpening = sum(IsCoach),
                        NFrontOfficeOpening = sum(IsFrontOffice),
                        HasHeadCoachOpening = any(IsHeadCoach), .groups = "drop"),
            by = c("franchise_id", "season")) |>
  mutate(across(c(NPersonsOpening, NCoachesOpening, NFrontOfficeOpening),
                \(x) if_else(OpeningStaffObserved, coalesce(x, 0L), NA_integer_)),
         HasHeadCoachOpening = if_else(OpeningStaffObserved, coalesce(HasHeadCoachOpening, FALSE),
                                       NA),
         PrevOpeningObserved = paste(franchise_id, season - 1L) %in%
           paste(franchise_id, season)[OpeningStaffObserved]) |>
  arrange(franchise_id, season)
if (nrow(StaffOpeningCoverage) != n_distinct(paste(OpeningRoles$franchise_id,
                                                   OpeningRoles$season)) &&
    all(StaffOpeningCoverage$OpeningStaffObserved)) {
  stop("Opening coverage rows do not match the franchise-seasons with opening role rows")
}

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

StaffPersonSeasonLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  season = "NFL season (1999-2025)",
  person_id = "Staff person identifier (Wikipedia title, or 'name:<key>' when unlinked)",
  person_name = "Person name as listed on the staff box",
  role_std = "Primary standardized role: highest priority among the person's roles in any snapshot of the season (HC > ASST_HC > OC/DC/STC > pass/run-game coordinators > position coaches > assistants/quality control > S&C > other coaching support > owner/chair/CEO/president/vice-chair > GM > football-ops executive/ASST_GM > VP/director personnel > college scouting/scouts > cap/admin > other front office; coaching roles rank above front-office roles)",
  PrimaryRoleGroup = "Role group of the primary role (staff_team_season.role_group)",
  RoleTier = "Hierarchy tier of the primary role within its domain (1 = top: HC or owner/executive)",
  Domain = "Domain of the primary role: coach or front_office",
  Unit = "Unit of the primary role: offense, defense, special_teams, front_office, or general (HC, assistant HC without unit, S&C, support)",
  AllRoles = "All standardized roles held in the franchise-season (any snapshot), pipe-separated in priority order",
  NRoles = "Number of distinct standardized roles held",
  IsHeadCoach = "Listed as head coach (including interim) in any snapshot of the season (union measure: includes in-season interim promotions)",
  IsInterimHC = "Listed as interim head coach in any snapshot",
  IsInterimAnyRole = "Listed as interim in any role in any snapshot",
  IsCoach = "Holds an on-field coaching role (head coach, coordinator, position coach, assistant/quality control) in any snapshot; excludes S&C and support staff",
  IsCoordinator = "Offensive, defensive or special-teams coordinator (OC/DC/STC) in any snapshot",
  IsOC = "Offensive coordinator in any snapshot",
  IsDC = "Defensive coordinator in any snapshot",
  IsSTC = "Special-teams coordinator in any snapshot",
  IsOtherCoordinator = "Assistant head coach or pass/run-game coordinator in any snapshot",
  IsPositionCoach = "Position coach (QB, OL, WR, RB, TE, DL, EDGE/OLB, LB, ILB, DB, CB, S, nickel) in any snapshot",
  IsAssistantCoach = "Assistant or quality-control coach in any snapshot",
  IsStrengthCond = "Strength and conditioning staff in any snapshot",
  IsSupportStaff = "Other coaching support staff (role_std OTHER_COACH) in any snapshot",
  IsFrontOffice = "Holds any front-office role in any snapshot",
  IsGM = "General manager in any snapshot",
  IsOwnerExec = "Owner, chair, vice-chair, CEO or president in any snapshot",
  IsPersonnelScouting = "Player-personnel or scouting role (VP/director personnel, director of college scouting, scout) in any snapshot",
  IsOffenseCoach = "Holds an on-field coaching role in the offensive unit in any snapshot",
  IsDefenseCoach = "Holds an on-field coaching role in the defensive unit in any snapshot",
  IsSpecialTeamsCoach = "Holds an on-field coaching role in the special-teams unit in any snapshot",
  in_preseason = "Listed in the opening staff-template revision (in force at 00:00 UTC on the franchise's first REG game date; template era 2007+)",
  in_midseason = "Listed in the Nov 1 staff-template revision (template era)",
  in_late = "Listed in the Dec 31 (or day after last REG game) revision (template era)",
  in_season_article = "Listed in the season article staff box (1999-2006; written after the season, not an opening measure)",
  StaffSource = "Source: staff_template (Wikipedia template revisions, 2007-2025) or season_article (1999-2006)",
  SeasonsWithFranchise = "Consecutive seasons on this franchise's staff, including the current one (a season whose staff box is unobserved breaks the run)",
  TenureLeftCensored = "The current run with the franchise starts in 1999, the first observed season",
  SeasonsOnStaffAnyTeam = "Cumulative number of seasons on any NFL staff since 1999, including the current one",
  PrevStaffSeason = "Most recent earlier season in which the person is on any staff",
  PrevFranchise = "Franchise(s) of the person in PrevStaffSeason (pipe-separated when several)",
  PrevTeamSeasonObserved = "The franchise's staff box for season - 1 is observed",
  NewToFranchise = "Not on this franchise's staff (any snapshot) in season - 1 (NA when that staff box is unobserved)",
  PromotedWithinFranchise = "On this franchise's staff in season - 1 in the same domain with a lower tier (e.g. position coach to coordinator); NA when season - 1 is unobserved",
  race = "Hand-coded race (NA until coded; notes/race-coding-protocol.md)",
  hispanic = "Hand-coded Hispanic ethnicity (yes/no/unknown)",
  black_any = "Hand-coded Black (alone or in combination); NA until coded",
  nonwhite = "Hand-coded non-white or Hispanic; NA until coded",
  race_source = "Source of the hand code: adjudicated, coder_agree, single_coder, disputed",
  black_provisional = "Hand-coded black_any when coded, else 1 if a Wikipedia category flags the person as Black, else NA (positive-only; a lower bound)",
  black_provisional_source = "Source of black_provisional",
  wiki_cat_black = "Wikipedia category flags the person as Black/African American (positive-only screening aid)",
  wiki_cat_hispanic_latino = "Wikipedia category flags the person as Hispanic/Latino (positive-only screening aid)",
  p_white_bifsg = "Name-based BIFSG posterior P(white) (secondary measure; misclassifies most Black coaches)",
  p_black_bifsg = "Name-based BIFSG posterior P(Black)",
  p_hispanic_bifsg = "Name-based BIFSG posterior P(Hispanic)",
  p_api_bifsg = "Name-based BIFSG posterior P(Asian/Pacific Islander)",
  p_aian_bifsg = "Name-based BIFSG posterior P(American Indian/Alaska Native)",
  p_multi_bifsg = "Name-based BIFSG posterior P(multiracial)",
  race_bifsg = "Argmax BIFSG category",
  p_black_any_pred = "Predicted P(non-Hispanic Black alone), model-only (primary measure; equals p_black_pred despite the name, so multiracial and Hispanic Black persons count as non-Black): BIFSG name/county likelihood x NFL staff prior estimated by EM on predetermined covariates; documented race not used",
  p_white_pred = "Predicted P(white), model-only",
  p_black_pred = "Predicted P(non-Hispanic Black alone), model-only",
  p_hispanic_pred = "Predicted P(Hispanic), model-only",
  p_api_pred = "Predicted P(Asian/Pacific Islander), model-only",
  p_aian_pred = "Predicted P(American Indian/Alaska Native), model-only",
  p_multi_pred = "Predicted P(multiracial), model-only",
  prior_black_pred = "EM prior P(non-Hispanic Black alone) given the predetermined covariates of the primary staff prior (pred_role_group_first, pred_unit_first, pred_first_era), before the name/county likelihood",
  p_black_any_preddoc = "P(Black), documented variant: 1 for documented Black alone or in combination (incl. multiracial and Hispanic Black), 0 for other documented race, else the posterior P(non-Hispanic Black alone) under a prior fitted on the undocumented (fame-dependent; sensitivity only)",
  p_white_preddoc = "P(white), documented variant (sensitivity only)",
  p_black_preddoc = "P(non-Hispanic Black alone), documented variant (sensitivity only)",
  p_hispanic_preddoc = "P(Hispanic), documented variant (sensitivity only)",
  p_api_preddoc = "P(Asian/Pacific Islander), documented variant (sensitivity only)",
  p_aian_preddoc = "P(American Indian/Alaska Native), documented variant (sensitivity only)",
  p_multi_preddoc = "P(multiracial), documented variant (sensitivity only)",
  documented_black_any = "Black per a public source (Wikidata, Wikipedia category or article text); NA when undocumented; validation only",
  pred_role_group_first = "Primary prior covariate: role group at the person's first staff appearance",
  pred_unit_first = "Primary prior covariate: unit at the person's first staff appearance",
  pred_first_era = "Primary prior covariate: era of the person's first staff season",
  pred_former_player = "Former NFL player: covariate of the preddoc prior only (fame proxy, coded through a Wikidata link); not in the primary staff prior"
)

# Opening panel: the same variables, defined on the opening snapshot's role
# rows only (the union labels' "in any snapshot" becomes "at the opening")
opening_note <- " (opening snapshot: the staff-template revision in force at 00:00 UTC on the franchise's first REG game date, 2007+; roles first listed later in the season are excluded)"
StaffPersonOpeningSeasonLabels <- StaffPersonSeasonLabels |>
  str_replace(" in any snapshot of the season", " at the opening") |>
  str_replace(" in any snapshot", " at the opening") |>
  str_replace(" \\(any snapshot\\)", " at the opening") |>
  set_names(names(StaffPersonSeasonLabels))
StaffPersonOpeningSeasonLabels[c("season", "role_std", "AllRoles", "IsHeadCoach", "in_preseason",
                                 "in_midseason", "in_late", "in_season_article",
                                 "SeasonsWithFranchise", "TenureLeftCensored",
                                 "SeasonsOnStaffAnyTeam", "PrevStaffSeason",
                                 "PrevTeamSeasonObserved", "NewToFranchise",
                                 "PromotedWithinFranchise")] <- c(
  "NFL season (2007-2025; the first season with staff-template revisions)",
  paste0("Primary standardized role at the opening: highest priority among the roles listed in the opening snapshot (same priority order as staff_person_season)", opening_note),
  "All standardized roles listed in the opening snapshot, pipe-separated in priority order",
  "Listed as head coach (including interim) in the opening snapshot; an in-season interim promotion does not count",
  "TRUE on every row: the panel is built from the opening snapshot's role rows",
  "One of the person's opening roles is also listed in the Nov 1 revision",
  "One of the person's opening roles is also listed in the Dec 31 (or day after last REG game) revision",
  "FALSE on every row (season-article boxes are not an opening measure)",
  "Consecutive opening snapshots on this franchise's staff, including the current one (an unobserved opening snapshot breaks the run)",
  "The current run of opening snapshots with the franchise starts in 2007, the first opening snapshot (earlier tenure unobserved)",
  "Cumulative number of opening snapshots on any NFL staff since 2007, including the current one",
  "Most recent earlier season in which the person is on any opening-snapshot staff",
  "The franchise's opening snapshot for season - 1 is observed and parsed (FALSE in 2007: the 2006 opening staff is unobserved)",
  "Not on this franchise's opening-snapshot staff in season - 1; NA when that opening snapshot is unobserved (all of 2007)",
  "On this franchise's opening staff in season - 1 in the same domain with a lower tier (e.g. position coach to coordinator); NA when season - 1 is unobserved (all of 2007)"
)
StaffPersonOpeningSeasonLabels <- c(
  StaffPersonOpeningSeasonLabels,
  OpeningTargetDate = "Opening snapshot target: the franchise's first REG game date (the revision in force at 00:00 UTC that day)",
  OpeningRevisionTimestamp = "UTC timestamp of the template revision used for the opening snapshot (at or before the target by construction)",
  OpeningRevisionId = "Wikipedia revision id of the opening snapshot",
  OpeningDaysStale = "Days between the opening revision and the target (staleness of the listing; a large value means the box was last edited long before the opener)"
)

StaffOpeningCoverageLabels <- c(
  franchise_id = "Franchise identifier (stable across relocations)",
  season = "NFL season (2007-2025)",
  OpeningStaffObserved = "The opening (preseason) template snapshot parsed (>= 15 persons and a head coach; staff_snapshots.parse_ok)",
  OpeningTargetDate = StaffPersonOpeningSeasonLabels[["OpeningTargetDate"]],
  OpeningRevisionTimestamp = StaffPersonOpeningSeasonLabels[["OpeningRevisionTimestamp"]],
  OpeningRevisionId = StaffPersonOpeningSeasonLabels[["OpeningRevisionId"]],
  OpeningDaysStale = StaffPersonOpeningSeasonLabels[["OpeningDaysStale"]],
  NPersonsOpening = "Persons listed in the opening snapshot (0 when parsed and none; NA when unobserved)",
  NCoachesOpening = "On-field coaches listed in the opening snapshot (IsCoach)",
  NFrontOfficeOpening = "Front-office staff listed in the opening snapshot (IsFrontOffice)",
  HasHeadCoachOpening = "A head coach is listed in the opening snapshot",
  PrevOpeningObserved = "The franchise's opening snapshot for season - 1 is observed (FALSE in 2007), i.e. opening turnover measures are defined"
)

write_sample(StaffPersonSeason, "staff_person_season",
             key = c("franchise_id", "season", "person_id"),
             labels = StaffPersonSeasonLabels)
write_sample(StaffPersonOpeningSeason, "staff_person_opening_season",
             key = c("franchise_id", "season", "person_id"),
             labels = StaffPersonOpeningSeasonLabels)
write_sample(StaffOpeningCoverage, "staff_opening_coverage",
             key = c("franchise_id", "season"),
             labels = StaffOpeningCoverageLabels)

db_disconnect(con)

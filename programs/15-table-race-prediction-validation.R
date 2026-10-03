# ============================================================================
# 15-table-race-prediction-validation.R
# Validation exhibits for the predicted race measure (race_predicted, built by
# scripts/04e_predict_race.py; design in notes/race-prediction-design.md).
# Hand codes are not used. The primary measure is the model-only posterior
# P(non-Hispanic Black alone) (p_black_any_pred: BIFSG name/county likelihood
# x NFL prior estimated by EM on predetermined covariates); P(Black alone or
# two or more races) (p_black_or_multi_pred), preddoc (documented race where
# a public source states it) and the names-only BIFSG posterior
# (p_black_bifsg) are shown for comparison.
# Exhibits (saved with measure = "predicted", the primary measure):
#   - table-24-race-prediction-tides (tab:race-prediction-tides): league
#     shares by season, predicted vs TIDES, for game-day players (player-week
#     weighted), head coaches (non-interim, one per franchise) and assistant
#     coaches, with period means, mean absolute differences and correlations
#     of model vs TIDES
#   - table-25-race-prediction-documented (tab:race-prediction-documented):
#     AUC (bootstrap intervals), calibration slope, reliability, mean
#     P(Black) by documented group and calibration by decile among persons
#     with documented race (game-day players and staff of 2002-2025, head
#     coaches 2010-2025)
#   - figure-race-prediction-tides: season series, predicted vs TIDES
#   - figure-race-prediction-distribution: P(Black) by entry position group
#     (players) and first role group (staff), posterior vs prior
# Definitions:
#   players: game-day roster = Active + Inactive rows of the REG weekly roster
#     in weeks in which the franchise played a REG game (programs/09, group
#     Roster); league share = mean over player-team-weeks (players weighted by
#     game-day weeks); a headcount version weights each player-season once.
#   head coaches: IsHeadCoach and not IsInterimHC in
#     analysis/staff_person_season, each franchise-season weighted once (1/n
#     when a franchise lists two non-interim head coaches).
#   assistant coaches: every coach below the head coach, as in the TIDES
#     definition (coordinators, position, assistant/quality-control and
#     strength-and-conditioning coaches): IsCoach or IsStrengthCond, not
#     IsHeadCoach; support-only staff excluded (a person-season with both a
#     coaching and a support role stays); one row per person x franchise x
#     season.
#   TIDES: data/reference/tides_nfl_race_shares.csv (README alongside).
#     Contemporaneous primary percentages; a table transcription is preferred
#     to the rounded text figure of the same report, values flagged as typos
#     are dropped, remaining ties take the median; the 2023 retrospective
#     appendix is used only for a season without a contemporaneous value
#     (flagged). Players: 'African-American' through 2016 (media-guide coding,
#     multiracial players inside one category); 'Black' from 2019
#     (self-identified, excluding two or more races and not disclosed).
# Inputs: DuckDB (read-only) race_predicted, race_bifsg, nfl_rosters_weekly,
#   nfl_team_games via load_person_race() and tbl(); analysis/
#   staff_person_season.parquet (programs/01); data/reference/
#   tides_nfl_race_shares.csv; data/derived/race_text_labels.csv.
# Outputs: output/tables/table-24-race-prediction-tides.tex,
#   output/tables/table-25-race-prediction-documented.tex (and my_paper/
#   tables), output/figures/figure-race-prediction-tides.{pdf,png},
#   output/figures/figure-race-prediction-distribution.{pdf,png},
#   output/estimates/15-race-prediction-validation.csv.
# Date: 2026-10-02 (revised 2026-10-03)
# ============================================================================

T0Script15 <- Sys.time()
con <- db_connect()

# Exhibits are about the predicted measure itself: route them as primary for
# this script only (the previous primary measure is restored at the end)
OldPrimary15 <- getOption("nfl.race_primary")
options(nfl.race_primary = "predicted")
ValMeasure <- "predicted"

FirstSeason15 <- 2002L
LastSeason15 <- 2025L

# ---------------------------------------------------------------------------
# Person-level race: predicted, preddoc, BIFSG, documented
# ---------------------------------------------------------------------------

# Columns of race_predicted that load_person_race() does not expose yet:
# P(Black alone or two or more races), the linked staff/player record, the
# races a source states, and the player prior's county-availability covariate
ExtraRace15 <- tbl(con, "race_predicted") |>
  select(person_uid, p_black_or_multi_pred, linked_uid, documented_races_stated,
         pred_county_available = county_available) |>
  collect()

# Documented groups (validation only; never used by the primary posterior):
# Black = documented Black alone or in combination, including Black Hispanic
# and multiracial Black persons; White = documented white, not Black; Other =
# any other documented race or Hispanic (a Hispanic person whose race no
# source states is Other). BlackAlone = documented Black, not Hispanic, not
# multiracial: the target of p_black_any_pred.
PersonRace <- load_person_race(con, hand_coded) |>
  select(person_uid, entity, person_id, p_black_any_pred, p_white_pred,
         p_multi_pred, prior_black_pred, p_black_any_preddoc, p_black_bifsg,
         documented_race, documented_black_any, documented_hispanic,
         documented_sources, pred_pos_group, pred_rookie_era, pred_draft_bucket,
         pred_college_type, pred_role_group_first, pred_unit_first,
         pred_first_era) |>
  left_join(ExtraRace15, by = "person_uid") |>
  mutate(DocGroup = case_when(documented_black_any == 1L ~ "Black",
                              documented_race == "white" ~ "White",
                              !is.na(documented_race) ~ "Other",
                              TRUE ~ NA_character_),
         DocBlackAlone = coalesce(DocGroup == "Black" & documented_race == "black" &
                                    coalesce(as.integer(documented_hispanic), 0L) != 1L,
                                  FALSE),
         # Hispanic with no race stated: Black or not is unknown
         DocHispNoRace = coalesce(documented_race == "hispanic" &
                                    coalesce(documented_races_stated, "") == "", FALSE),
         # One id per human: linked staff and player records share it
         HumanId = coalesce(pmin(person_uid, linked_uid), person_uid))
stopifnot(!anyDuplicated(PersonRace$person_uid))
message("15: persons with P(Black): ", sum(!is.na(PersonRace$p_black_any_pred)),
        " of ", nrow(PersonRace), "; documented: ", sum(!is.na(PersonRace$DocGroup)))

# Players and staff race columns, keyed by gsis_id / staff person_id
RaceCols <- c("p_black_any_pred", "p_black_or_multi_pred", "p_black_any_preddoc",
              "p_black_bifsg", "prior_black_pred", "p_multi_pred")
PlayerRace15 <- PersonRace |>
  filter(entity == "player") |>
  select(gsis_id = person_id, all_of(RaceCols), pred_pos_group)
StaffRace15 <- PersonRace |>
  filter(entity == "staff") |>
  select(person_id, all_of(RaceCols), pred_role_group_first, DocGroup)

# ---------------------------------------------------------------------------
# Season series: game-day players (programs/09 roster definition)
# ---------------------------------------------------------------------------

# REG game weeks of each franchise (bye weeks dropped)
GameWeeks15 <- tbl(con, "nfl_team_games") |>
  filter(game_type == "REG") |>
  distinct(franchise_id, season, week) |>
  collect() |>
  mutate(season = as.integer(season), week = as.integer(week)) |>
  filter(season >= FirstSeason15, season <= LastSeason15)

# Primary weekly-roster rows (ACT or INA) in REG game weeks: one row per
# player x franchise x week, i.e. each player weighted by game-day weeks
RosterWeeks15 <- tbl(con, "nfl_rosters_weekly") |>
  filter(season_type == "REG", is_key_primary, !is.na(gsis_id), gsis_id != "",
         status %in% c("ACT", "INA")) |>
  distinct(gsis_id, franchise_id, season, week) |>
  collect() |>
  mutate(season = as.integer(season), week = as.integer(week)) |>
  inner_join(GameWeeks15, by = c("franchise_id", "season", "week")) |>
  left_join(PlayerRace15, by = "gsis_id")
stopifnot(!anyDuplicated(RosterWeeks15[c("gsis_id", "franchise_id", "season", "week")]))
message("15: game-day player-weeks without P(Black): ",
        sum(is.na(RosterWeeks15$p_black_any_pred)), " of ", nrow(RosterWeeks15))

# League means by season of each race column over the rows of `df`, with
# weights `w` (NA probabilities dropped column by column)
season_shares <- function(df, w = NULL) {
  if (is.null(w)) df$w <- 1 else df$w <- df[[w]]
  df |>
    group_by(season) |>
    summarise(N = n(),
              across(all_of(RaceCols), \(x) sum(x * w, na.rm = TRUE) /
                       sum(w[!is.na(x)])),
              .groups = "drop")
}

# Week-weighted (player-weeks) and headcount (player-seasons) league shares
PlayerSeries <- season_shares(RosterWeeks15) |> mutate(Group = "players")
PlayerSeriesHead <- RosterWeeks15 |>
  distinct(gsis_id, season, .keep_all = TRUE) |>
  season_shares() |>
  mutate(Group = "players_headcount")

# ---------------------------------------------------------------------------
# Season series: head coaches and assistant coaches
# ---------------------------------------------------------------------------

StaffSeason15 <- read_parquet(file.path(analysis, "staff_person_season.parquet")) |>
  filter(season >= FirstSeason15, season <= LastSeason15) |>
  select(franchise_id, season, person_id, IsHeadCoach, IsInterimHC, IsCoach,
         IsStrengthCond, IsSupportStaff) |>
  left_join(StaffRace15, by = "person_id")

# Head coaches: non-interim, each franchise-season weighted once
HeadCoaches15 <- StaffSeason15 |>
  filter(IsHeadCoach, !IsInterimHC) |>
  group_by(franchise_id, season) |>
  mutate(w = 1 / n()) |>
  ungroup()
message("15: non-interim head-coach rows ", nrow(HeadCoaches15), " in ",
        n_distinct(HeadCoaches15[c("franchise_id", "season")]), " franchise-seasons")

# Assistant coaches: every on-field or strength coach below the head coach;
# support-only person-seasons drop out (a person-season holding a coaching
# and a support role stays, as a coach)
Assistants15 <- StaffSeason15 |>
  filter(IsCoach | IsStrengthCond, !IsHeadCoach)
message("15: assistant rows also flagged support staff (multi-role): ",
        sum(Assistants15$IsSupportStaff))

StaffSeries <- bind_rows(
  season_shares(HeadCoaches15, "w") |> mutate(Group = "head_coaches",
                                              N = as.integer(N)),
  season_shares(Assistants15) |> mutate(Group = "assistant_coaches"))

ModelSeries <- bind_rows(PlayerSeries, PlayerSeriesHead, StaffSeries)

# ---------------------------------------------------------------------------
# TIDES published shares
# ---------------------------------------------------------------------------

# One value per group x season x category (shares 0-1): contemporaneous
# primary percentages first (the report's own figure; later restatements and
# the 2023 Appendix II are marked "Retrospective" in notes); within the
# chosen rows, table transcriptions beat the rounded text figure of the same
# report and values the README flags as typos are dropped; ties -> median.
# Source records the report year(s) used and whether the value is
# retrospective.
TidesRaw <- read_csv(file.path(root, "data", "reference", "tides_nfl_race_shares.csv"),
                     show_col_types = FALSE)
Tides <- TidesRaw |>
  filter(unit == "percent", source_type == "primary",
         group %in% c("players", "head_coaches", "assistant_coaches"),
         category %in% c("black", "two_or_more_races", "not_disclosed"),
         !str_detect(coalesce(notes, ""), "looks like a typo")) |>
  mutate(Retro = str_detect(coalesce(notes, ""), "Retrospective"),
         TableRow = str_detect(coalesce(notes, ""), "^Table transcription")) |>
  group_by(group, season, category) |>
  filter(Retro == min(Retro)) |>
  group_by(group, season, category, report_year) |>
  filter(TableRow == max(TableRow)) |>
  group_by(group, season, category) |>
  summarise(Value = median(value) / 100,
            Retro = first(Retro),
            Source = paste0("TIDES ", paste(sort(unique(report_year)), collapse = "/"),
                            if_else(first(Retro), " (retrospective)", "")),
            .groups = "drop")

TidesWide <- Tides |>
  select(Group = group, season, category, Value) |>
  pivot_wider(names_from = category, values_from = Value,
              names_prefix = "Tides_") |>
  mutate(Tides_black_2plus = Tides_black + Tides_two_or_more_races,
         # 2019+ upper bound: (Black + two or more) among those who disclosed;
         # NA where TIDES prints no not-disclosed share
         Tides_upper_disclosed = Tides_black_2plus / (1 - Tides_not_disclosed))
TidesSource <- Tides |>
  filter(category == "black") |>
  select(Group = group, season, TidesSource = Source, TidesRetro = Retro)
# Retrospective flags of the two-or-more and not-disclosed shares
TidesRetroOther <- Tides |>
  filter(category != "black") |>
  group_by(group, category) |>
  summarise(NRetro = sum(Retro), N = n(), First = min(season), Last = max(season),
            .groups = "drop")
print(TidesRetroOther)

# Model series next to TIDES (the headcount player series uses the player
# TIDES figures). TIDES 'black' is 'African-American' through 2018 and
# Black alone from 2019 (players have no 2017-2018 figure; the 2018
# assistant-coach table already separates two or more races)
Validation <- ModelSeries |>
  mutate(TidesGroup = if_else(Group == "players_headcount", "players", Group)) |>
  left_join(TidesWide, by = c("TidesGroup" = "Group", "season")) |>
  left_join(TidesSource, by = c("TidesGroup" = "Group", "season")) |>
  mutate(DiffPred = p_black_any_pred - Tides_black,
         # Black or multiracial vs the matching TIDES definition: Black + two
         # or more where TIDES reports two or more races, else the single-race
         # 'African-American' share (multiracial persons inside one race)
         TidesBM = coalesce(Tides_black_2plus, Tides_black),
         DiffPredBM = p_black_or_multi_pred - TidesBM)
print(Validation |>
        select(Group, season, N, p_black_any_pred, p_black_or_multi_pred,
               p_black_any_preddoc, p_black_bifsg, Tides_black, Tides_black_2plus,
               TidesSource) |>
        mutate(across(where(is.double), \(x) round(x, 3))), n = 100)

# ---------------------------------------------------------------------------
# Table 24: predicted vs TIDES league shares by season
# ---------------------------------------------------------------------------

# Percent with one decimal; "--" where missing
pct <- function(x, digits = 1) {
  if_else(is.na(x), "--", formatC(100 * x, format = "f", digits = digits))
}
# Signed percentage-point difference
pp <- function(x, digits = 1) {
  if_else(is.na(x), "--", formatC(100 * x, format = "f", digits = digits, flag = "+"))
}

# Season columns of one group: Pred (Black alone), Pred. B+m (Black alone or
# two or more races), PredDoc, BIFSG, TIDES (retrospective values marked r),
# [players: TIDES Black + two or more], Pred - TIDES, [players: Pred. B+m
# minus the matching TIDES share]
GroupCols <- function(g, players = FALSE) {
  d <- Validation |>
    filter(Group == g) |>
    arrange(season)
  tides <- paste0(pct(d$Tides_black),
                  if_else(coalesce(d$TidesRetro, FALSE), "$^{r}$", ""))
  out <- tibble(season = d$season, Pred = pct(d$p_black_any_pred),
                PredBM = pct(d$p_black_or_multi_pred),
                PredDoc = pct(d$p_black_any_preddoc), Bifsg = pct(d$p_black_bifsg),
                Tides = tides)
  if (players) out$Tides2 <- pct(d$Tides_black_2plus)
  out$Diff <- pp(d$DiffPred)
  if (players) out$DiffBM <- pp(d$DiffPredBM)
  rename_with(out, \(x) paste0(g, "_", x), -season)
}

Table24Body <- GroupCols("players", players = TRUE) |>
  left_join(GroupCols("head_coaches"), by = "season") |>
  left_join(GroupCols("assistant_coaches"), by = "season") |>
  mutate(season = as.character(season))

# Model vs TIDES over the seasons of a period that have the TIDES value
# `bench`: mean difference, mean absolute difference and correlation across
# seasons, for each model series; also returns the seasons used
ModelScores <- c(Pred = "p_black_any_pred", PredBM = "p_black_or_multi_pred",
                 PredDoc = "p_black_any_preddoc", Bifsg = "p_black_bifsg")
PeriodGap <- function(g, seasons, bench) {
  d <- Validation |>
    filter(Group == g, season %in% seasons, !is.na(.data[[bench]]))
  stats <- map_dfr(ModelScores, \(m) {
    if (nrow(d) == 0) return(tibble(mean = NA_real_, mae = NA_real_, cor = NA_real_))
    e <- d[[m]] - d[[bench]]
    tibble(mean = mean(e), mae = mean(abs(e)),
           # undefined with fewer than 3 seasons or a constant TIDES share
           cor = if (nrow(d) >= 3 && sd(d[[bench]]) > 0) cor(d[[m]], d[[bench]])
                 else NA_real_)
  }, .id = "score")
  mutate(stats, used = list(sort(d$season)))
}

# Compact season list, e.g. 2003, 2005--2016
season_span <- function(x) {
  if (length(x) == 0) return("none")
  runs <- split(x, cumsum(c(1, diff(x) != 1)))
  paste(map_chr(runs, \(r) if (length(r) == 1) as.character(r)
                else paste0(min(r), "--", max(r))), collapse = ", ")
}

# Panel B rows: statistic x period x benchmark. Scores lists the model series
# compared with the benchmark (Pred. B+m is not compared with Black alone)
AllScores <- names(ModelScores)
NotBM <- setdiff(AllScores, "PredBM")
GapRows <- tribble(
  ~Label,                                                          ~Stat,  ~Seasons,   ~Bench,                  ~Scores,
  "Mean diff., through 2016, vs.\\ African-American",              "mean", 2002:2016,  "Tides_black",           AllScores,
  "Mean diff., 2010--2015, vs.\\ African-American",                "mean", 2010:2015,  "Tides_black",           AllScores,
  "Mean diff., 2017--2018, vs.\\ TIDES (staff only)",              "mean", 2017:2018,  "Tides_black",           NotBM,
  "Mean diff., 2019--2023, vs.\\ Black alone",                     "mean", 2019:2023,  "Tides_black",           NotBM,
  "Mean diff., 2019--2023, vs.\\ Black + two or more",             "mean", 2019:2023,  "Tides_black_2plus",     AllScores,
  "Mean diff., 2019--2023, vs.\\ (Black + 2+)/(1 $-$ not discl.)", "mean", 2019:2023,  "Tides_upper_disclosed", AllScores,
  "Mean abs.\\ diff., through 2016, vs.\\ African-American",       "mae",  2002:2016,  "Tides_black",           AllScores,
  "Mean abs.\\ diff., 2019--2023, vs.\\ Black alone",              "mae",  2019:2023,  "Tides_black",           NotBM,
  "Mean abs.\\ diff., 2019--2023, vs.\\ Black + two or more",      "mae",  2019:2023,  "Tides_black_2plus",     AllScores,
  "Correlation, through 2016, vs.\\ African-American",             "cor",  2002:2016,  "Tides_black",           AllScores,
  "Correlation, 2019--2023, vs.\\ Black alone",                    "cor",  2019:2023,  "Tides_black",           NotBM)
GapGroups <- c("players", "head_coaches", "assistant_coaches")
GapLong <- pmap_dfr(GapRows, \(Label, Stat, Seasons, Bench, Scores) {
  map_dfr(GapGroups, \(g) {
    if (g == "players" && all(Seasons %in% 2017:2018)) return(tibble())
    PeriodGap(g, Seasons, Bench) |>
      filter(score %in% Scores) |>
      transmute(Label, Stat, Group = g, score, Bench, Nominal = list(Seasons),
                value = case_when(Stat == "mean" ~ mean, Stat == "mae" ~ mae,
                                  TRUE ~ cor),
                used)
  })
})
GapTable <- GapLong |>
  mutate(cell = case_when(is.na(value) ~ "--",
                          Stat == "cor" ~ formatC(value, format = "f", digits = 2),
                          Stat == "mae" ~ formatC(100 * value, format = "f", digits = 1),
                          TRUE ~ pp(value)),
         col = paste0(Group, "_", score)) |>
  select(season = Label, col, cell) |>
  pivot_wider(names_from = col, values_from = cell)
GapTable <- GapTable[match(GapRows$Label, GapTable$season), ]
Table24 <- bind_rows(Table24Body, GapTable) |>
  mutate(across(everything(), \(x) coalesce(x, "")))
Table24 <- Table24[names(Table24Body)]
print(GapTable, width = 250)

# Period cells averaging fewer seasons than the row label (quoted in notes)
GapCoverage <- GapLong |>
  filter(score == "Pred", Stat == "mean") |>
  mutate(Lab = map2_chr(Nominal, used, \(n, u) {
    want <- n[n <= 2023]
    if (setequal(u, want)) NA_character_ else season_span(u)
  })) |>
  filter(!is.na(Lab)) |>
  distinct(Group, Bench, Lab, Label)
print(GapCoverage)

# Facts quoted in the notes, computed from the data
gap_value <- function(g, label, score = "Pred") {
  GapLong |> filter(Group == g, Label == label, score == !!score) |> pull(value)
}
LabAA <- GapRows$Label[1]; Lab1015 <- GapRows$Label[2]
Lab2plus <- GapRows$Label[5]
LabMaeAA <- GapRows$Label[7]; LabCorAA <- GapRows$Label[10]
AssistRange <- range(Validation$N[Validation$Group == "assistant_coaches"])
TidesAssistN <- TidesRaw |>
  filter(group == "assistant_coaches", !is.na(base_n)) |>
  distinct(season, base_n) |>
  arrange(season)
TidesAssistText <- paste(glue("{scales::comma(TidesAssistN$base_n)} in {TidesAssistN$season}"),
                         collapse = " and ")
NoTidesPlayers <- Validation |>
  filter(Group == "players", is.na(Tides_black)) |>
  pull(season) |>
  season_span()
Players19 <- TidesWide |> filter(Group == "players", season >= 2019)
pct_range <- function(x) {
  r <- round(100 * range(x, na.rm = TRUE))
  if (r[1] == r[2]) paste0(r[1], "\\%") else paste0(r[1], "--", r[2], "\\%")
}
Range2plus <- pct_range(Players19$Tides_two_or_more_races)
RangeNotDisc <- pct_range(Players19$Tides_not_disclosed)
HeadN <- PlayerSeriesHead |> select(season, N)
n_range <- function(x) {
  r <- range(x)
  if (r[1] == r[2]) scales::comma(r[1])
  else paste0(scales::comma(r[1]), "--", scales::comma(r[2]))
}
HeadNText <- glue("{n_range(HeadN$N[HeadN$season <= 2015])} distinct players a season through 2015, ",
                  "{n_range(HeadN$N[HeadN$season == 2016])} in 2016 and ",
                  "{n_range(HeadN$N[HeadN$season >= 2017])} from 2017")
HeadcountGap1015 <- PeriodGap("players_headcount", 2010:2015, "Tides_black")$mean[1]
HeadcountGap19 <- PeriodGap("players_headcount", 2019:2023, "Tides_upper_disclosed")$mean[1]
# Seasons of the staff two-or-more / not-disclosed shares and whether they
# come from the retrospective appendix
RetroStaff <- TidesRetroOther |>
  filter(group != "players") |>
  group_by(group) |>
  summarise(AllRetro = all(NRetro == N), First = min(First), Last = max(Last),
            .groups = "drop") |>
  mutate(txt = glue("{str_replace(group, '_', ' ')} {First}--{Last}"))
RetroStaffText <- paste0(
  "the staff two-or-more and not-disclosed shares (",
  paste(RetroStaff$txt, collapse = ", "), ") ",
  if (all(RetroStaff$AllRetro)) "all come from that appendix"
  else paste0("come from that appendix for ",
              paste(RetroStaff$txt[RetroStaff$AllRetro], collapse = ", ")))
# Cells averaging fewer seasons than the label states
GapCoverageText <- GapCoverage |>
  mutate(GroupLab = str_replace(Group, "_", " "),
         Bench = recode(Bench, Tides_black = "the TIDES share",
                        Tides_black_2plus = "Black + two or more",
                        Tides_upper_disclosed = "the disclosure-adjusted share")) |>
  distinct(GroupLab, Bench, Lab) |>
  mutate(txt = glue("{GroupLab} against {Bench}: {Lab}"))
GapCoverageText <- paste(unique(GapCoverageText$txt), collapse = "; ")
# Multiracial mass on game-day rosters and head-coach documentation
MeanMultiRoster <- mean(RosterWeeks15$p_multi_pred, na.rm = TRUE)
HCDoc <- HeadCoaches15 |>
  filter(season >= 2010) |>
  distinct(person_id, DocGroup) |>
  summarise(NDoc = sum(!is.na(DocGroup)), NDocBlack = sum(DocGroup == "Black", na.rm = TRUE),
            N = n())

Table24Notes <- c(
  "This table compares league-wide predicted Black shares with the shares published by",
  "The Institute for Diversity and Ethics in Sport (TIDES), \\textit{Racial and Gender Report",
  "Card: National Football League}. Panel A reports shares in percent by season. Pred.\\ is the",
  "mean primary predicted probability of being non-Hispanic Black alone (model-only; documented race",
  "is not used); Pred.\\ B+m adds the predicted probability of two or more races, which includes",
  "non-Black multiracial persons and so bounds Black alone or in combination from above;",
  "PredDoc is the documented-race sensitivity variant; BIFSG is the names-and-county posterior",
  "with Census population priors and no NFL prior. Players are game-day roster members (Active",
  "and Inactive rows of the regular-season weekly roster in weeks in which the team played),",
  "weighted by game-day weeks. Head coaches are non-interim head coaches, one per franchise.",
  "Assistant coaches are all coaches below the head coach (coordinators, position,",
  "assistant and quality-control, and strength-and-conditioning coaches; support-only staff",
  glue("excluded), one row per person, franchise and season ({AssistRange[1]}--{AssistRange[2]} a season;"),
  glue("TIDES prints totals of {TidesAssistText})."),
  "TIDES definitions: through 2016 the player share is `African-American', coded by TIDES from",
  "team media guides, with each player in one category (multiracial players inside a single race),",
  "so it lies between Black alone and Black alone or in combination;",
  "from 2019 it is self-identified `Black or African American', which excludes players who report",
  glue("two or more races ({Range2plus}) or do not disclose ({RangeNotDisc}); the drop between 2016 and 2019 is"),
  "largely this definitional break. TIDES +2+ adds two or more races.",
  "Diff.\\ is Pred.\\ minus TIDES; from 2019 the TIDES Black share has non-disclosers in its",
  "denominator, so Diff.\\ rises with non-disclosure. Diff.\\ B+m is Pred.\\ B+m minus TIDES +2+",
  "where TIDES reports two or more races, else minus the `African-American' share.",
  "Each season uses the report's own contemporaneous figure (the season the report states);",
  "$^{r}$ marks values taken from the retrospective appendix of the 2023 report because no",
  glue("contemporaneous percentage exists; {RetroStaffText}."),
  glue("-- marks seasons without a TIDES figure (players: {NoTidesPlayers};"),
  "no report covers 2024 or 2025). Panel B compares each model series with TIDES over the seasons",
  "of the period that have the TIDES figure: mean difference and mean absolute difference in",
  "percentage points, and the correlation across seasons. Pred.\\ B+m is not compared with",
  glue("Black alone. Cells that average fewer seasons than the row states: {GapCoverageText}."),
  "TIDES conflicts kept as printed: the 2002 assistant-coach figure may refer to 2001; the 2004",
  "head-coach share (16\\%, before the season) differs from the three Black head coaches reported for",
  "2004 in the next report.",
  glue("The weekly rosters list {HeadNText}; with each player-season weighted once (headcount), the"),
  glue("player gap of Pred.\\ is {pp(HeadcountGap1015)} pp in 2010--2015 against `African-American'"),
  glue("and {pp(HeadcountGap19)} pp in 2019--2023 against the disclosure-adjusted share."),
  glue("For players, most of the shortfall of Pred.\\ against `African-American'"),
  glue("({pp(gap_value('players', LabAA))} pp through 2016) is the multiracial mass: the mean predicted"),
  glue("probability of two or more races on game-day rosters is {formatC(100 * MeanMultiRoster, format = 'f', digits = 1)}\\%,"),
  glue("Pred.\\ B+m differs from `African-American' by {pp(gap_value('players', Lab1015, 'PredBM'))} pp"),
  glue("in 2010--2015 and from Black + two or more by {pp(gap_value('players', Lab2plus, 'PredBM'))} pp in"),
  "2019--2023, and the model separates multiracial from Black alone only weakly. For coaches the",
  glue("level shortfall survives: Pred.\\ B+m is {pp(gap_value('assistant_coaches', LabAA, 'PredBM'))} pp"),
  "from the `African-American' assistant-coach share through 2016, consistent with name likelihoods",
  "that understate Black names among coaches.",
  glue("Head-coach season errors are large and offsetting: through 2016 the mean absolute difference"),
  glue("of Pred.\\ is {formatC(100 * gap_value('head_coaches', LabMaeAA), format = 'f', digits = 1)} pp"),
  glue("against a mean difference of {pp(gap_value('head_coaches', LabAA))} pp, and its correlation with"),
  glue("TIDES across seasons is {formatC(gap_value('head_coaches', LabCorAA), format = 'f', digits = 2)}"),
  "(-- marks a correlation that is undefined because the TIDES share is constant). For head",
  glue("coaches PredDoc is close to documented race: documentation is positive-only and concentrated"),
  glue("among Black head coaches ({HCDoc$NDocBlack} of the {HCDoc$NDoc} documented among {HCDoc$N} head"),
  "coaches of 2010--2025 are Black), and PredDoc sets documented persons to their documented race,",
  "so its fit to TIDES checks documentation, not the model.",
  "Race is predicted, not observed: each person's probability combines first name, surname and",
  "hometown county (BIFSG) with an NFL prior estimated by EM on predetermined characteristics",
  "(players: entry position, rookie era, draft round, college type, county availability; staff:",
  "first role group, unit and era); documented race is not used (notes/race-prediction-design.md).",
  "A league share is the mean member probability; it is unbiased for the true share when the",
  "probabilities are calibrated, so level gaps against TIDES measure miscalibration.")

Table24Tex <- kbl(Table24, format = "latex", booktabs = TRUE, escape = FALSE,
                  linesep = "", align = c("l", rep("r", ncol(Table24) - 1)),
                  col.names = c("Season", "Pred.", "Pred.\\ B+m", "PredDoc", "BIFSG",
                                "TIDES", "TIDES +2+", "Diff.", "Diff.\\ B+m",
                                rep(c("Pred.", "Pred.\\ B+m", "PredDoc", "BIFSG",
                                      "TIDES", "Diff."), 2)),
                  caption = paste0("Predicted Black Shares and TIDES Published Shares, by Season",
                                   " \\label{tab:race-prediction-tides}")) |>
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 8) |>
  add_header_above(c(" " = 1, "Players (game-day, week-weighted)" = 8,
                     "Head coaches" = 6, "Assistant coaches" = 6)) |>
  pack_rows("Panel A. Black share by season (percent)", 1, nrow(Table24Body),
            bold = FALSE, italic = TRUE) |>
  pack_rows("Panel B. Model vs.\\ TIDES across seasons",
            nrow(Table24Body) + 1, nrow(Table24), bold = FALSE, italic = TRUE,
            escape = FALSE) |>
  add_notes(Table24Notes)
save_exhibit_tex(Table24Tex, "table-24-race-prediction-tides", ValMeasure)

# ---------------------------------------------------------------------------
# Table 25: discrimination and calibration on persons with documented race
# ---------------------------------------------------------------------------

# AUC (Mann-Whitney, average ranks for ties): probability that a random
# positive case has a higher score than a random negative case
auc <- function(score, y) {
  ok <- !is.na(score) & !is.na(y)
  score <- score[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(score)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
# Percentile bootstrap 95% interval of the AUC, resampling persons within
# the positive and the negative group (B draws, fixed seed)
auc_ci <- function(score, y, B = 999, seed = 20261003) {
  ok <- !is.na(score) & !is.na(y)
  score <- score[ok]; y <- y[ok]
  i1 <- which(y == 1); i0 <- which(y == 0)
  if (length(i1) < 2 || length(i0) < 2) return(c(NA_real_, NA_real_))
  set.seed(seed)
  draws <- replicate(B, {
    idx <- c(sample(i1, replace = TRUE), sample(i0, replace = TRUE))
    auc(score[idx], y[idx])
  })
  unname(quantile(draws, c(0.025, 0.975)))
}
# Calibration slope: logit of documented Black on logit(score) among
# documented persons, optionally with prior-covariate fixed effects
# (fixest::feglm, heteroskedasticity-robust SE). Returns slope, SE, N.
cal_slope <- function(doc, col, fe = character()) {
  d <- doc |>
    filter(!is.na(.data[[col]])) |>
    mutate(BlackY = as.integer(DocGroup == "Black"),
           LogitP = qlogis(pmin(pmax(.data[[col]], 1e-4), 1 - 1e-4)))
  if (n_distinct(d$BlackY) < 2) return(c(NA_real_, NA_real_, NA_real_))
  fml <- if (length(fe) == 0) BlackY ~ LogitP else
    as.formula(paste("BlackY ~ LogitP |", paste(fe, collapse = " + ")))
  m <- tryCatch(feglm(fml, data = d, family = "logit", vcov = "hetero",
                      notes = FALSE, warn = FALSE),
                error = \(e) NULL)
  if (is.null(m) || !"LogitP" %in% names(coef(m))) return(c(NA_real_, NA_real_, NA_real_))
  c(unname(coef(m)["LogitP"]), unname(se(m)["LogitP"]), nobs(m))
}

# Validation populations: players on a 2002-2025 game-day roster, staff
# persons of 2002-2025 (as in the distribution figure), and the non-interim
# head coaches of 2010-2025 (each person once). Every head coach of 2010-2025
# has a Wikipedia article, so documentation is positive-only: undocumented
# head coaches count as non-Black in the "vs. all other head coaches" row
StaffIds15 <- unique(StaffSeason15$person_id)
HCPersons <- StaffSeason15 |>
  filter(IsHeadCoach, !IsInterimHC, season >= 2010) |>
  distinct(person_id) |>
  mutate(person_uid = paste0("staff:", person_id))

ValSamples <- list(
  players = PersonRace |> filter(entity == "player", person_id %in% RosterWeeks15$gsis_id),
  staff = PersonRace |> filter(entity == "staff", person_id %in% StaffIds15),
  head_coaches = PersonRace |> filter(person_uid %in% HCPersons$person_uid))
ScoreCols <- c(Pred = "p_black_any_pred", Bifsg = "p_black_bifsg",
               Prior = "prior_black_pred")
# Prior covariates (fixed effects of the conditional calibration slope)
PriorFE <- list(players = c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket",
                            "pred_college_type", "pred_county_available"),
                staff = c("pred_role_group_first", "pred_unit_first", "pred_first_era"),
                head_coaches = character())
# Black-white AUC only with at least this many documented whites
MinWhiteAUC <- 10L

# Deciles that keep tied scores together (bins can be unequal)
tie_decile <- function(x) pmin(10L, 1L + floor(10 * (rank(x, ties.method = "min") - 1) / length(x)))

# One column per entity x score: AUCs with bootstrap intervals (documented
# Black vs white; vs non-Black; Black alone non-Hispanic vs non-Black;
# excluding Hispanic persons of unstated race; head coaches: vs all others),
# calibration slopes, the reliability Var(p)/[E(p)(1 - E(p))] over all
# persons (under calibration the R^2 of race on p), mean score by documented
# group, the calibration-implied mean for Black persons E[p^2]/E[p] (under
# calibration E[p | Black] = E[p^2] / E[p]), and the share documented Black
# by decile of the score among documented persons (head coaches: among all
# head coaches, undocumented counted non-Black)
DocStats <- imap_dfr(ValSamples, \(d, ent) {
  doc <- filter(d, !is.na(DocGroup))
  if (ent == "head_coaches") {
    dec_base <- mutate(d, BlackY = as.integer(coalesce(DocGroup, "") == "Black"))
  } else {
    dec_base <- mutate(doc, BlackY = as.integer(DocGroup == "Black"))
  }
  bw <- filter(doc, DocGroup %in% c("Black", "White"))
  y_bw <- as.integer(bw$DocGroup == "Black")
  y_bnb <- as.integer(doc$DocGroup == "Black")
  # Black alone non-Hispanic vs non-Black (other documented Black dropped)
  alone <- filter(doc, DocBlackAlone | DocGroup != "Black")
  y_alone <- as.integer(alone$DocGroup == "Black")
  # Hispanic persons with no race stated dropped from the comparison group
  nohisp <- filter(doc, !DocHispNoRace)
  y_nohisp <- as.integer(nohisp$DocGroup == "Black")
  y_all <- as.integer(coalesce(d$DocGroup, "") == "Black")
  enough_white <- sum(doc$DocGroup == "White") >= MinWhiteAUC
  imap_dfr(ScoreCols, \(col, sc) {
    s <- d[[col]]
    dec <- dec_base |>
      filter(!is.na(.data[[col]])) |>
      mutate(Decile = tie_decile(.data[[col]])) |>
      group_by(Decile) |>
      summarise(ShareBlack = mean(BlackY), MeanScore = mean(.data[[col]]),
                N = n(), .groups = "drop")
    a_bw <- if (enough_white) c(auc(bw[[col]], y_bw), auc_ci(bw[[col]], y_bw)) else rep(NA_real_, 3)
    a_bnb <- c(auc(doc[[col]], y_bnb), auc_ci(doc[[col]], y_bnb))
    a_all <- if (ent == "head_coaches") c(auc(s, y_all), auc_ci(s, y_all)) else rep(NA_real_, 3)
    slope <- cal_slope(doc, col)
    slope_fe <- if (sc == "Pred" && length(PriorFE[[ent]]) > 0)
      cal_slope(doc, col, PriorFE[[ent]]) else rep(NA_real_, 3)
    m <- mean(s, na.rm = TRUE)
    tibble(entity = ent, score = sc,
           stat = c("auc_bw", "auc_bw_lo", "auc_bw_hi",
                    "auc_bnb", "auc_bnb_lo", "auc_bnb_hi",
                    "auc_alone", "auc_nohisp",
                    "auc_ball", "auc_ball_lo", "auc_ball_hi",
                    "cal_slope", "cal_slope_se", "cal_slope_n",
                    "cal_slope_fe", "cal_slope_fe_se", "cal_slope_fe_n",
                    "reliability",
                    "mean_black", "mean_black_alone", "mean_white", "mean_other",
                    "mean_undoc", "mean_all", "implied_black",
                    paste0("dec", dec$Decile, "_share"),
                    paste0("dec", dec$Decile, "_mean"),
                    paste0("dec", dec$Decile, "_n"),
                    "n_black", "n_black_alone", "n_white", "n_other",
                    "n_hisp_norace", "n_undoc"),
           value = c(a_bw, a_bnb,
                     auc(alone[[col]], y_alone), auc(nohisp[[col]], y_nohisp),
                     a_all, slope, slope_fe,
                     var(s, na.rm = TRUE) / (m * (1 - m)),
                     mean(doc[[col]][doc$DocGroup == "Black"], na.rm = TRUE),
                     mean(doc[[col]][doc$DocBlackAlone], na.rm = TRUE),
                     mean(doc[[col]][doc$DocGroup == "White"], na.rm = TRUE),
                     mean(doc[[col]][doc$DocGroup == "Other"], na.rm = TRUE),
                     mean(s[is.na(d$DocGroup)], na.rm = TRUE),
                     m,
                     sum(s^2, na.rm = TRUE) / sum(s, na.rm = TRUE),
                     dec$ShareBlack, dec$MeanScore, dec$N,
                     sum(doc$DocGroup == "Black"), sum(doc$DocBlackAlone),
                     sum(doc$DocGroup == "White"), sum(doc$DocGroup == "Other"),
                     sum(doc$DocHispNoRace), sum(is.na(d$DocGroup))))
  })
})
print(DocStats |> filter(!str_starts(stat, "dec")) |>
        pivot_wider(names_from = c(entity, score), values_from = value) |>
        mutate(across(where(is.double), \(x) round(x, 3))), n = 50, width = 200)

# Table layout: rows = statistics, columns = entity x score
f3 <- function(x) if_else(is.na(x), "", formatC(x, format = "f", digits = 3))
f2 <- function(x) if_else(is.na(x), "", formatC(x, format = "f", digits = 2))
fn <- function(x) if_else(is.na(x), "", formatC(x, format = "d", big.mark = ","))
DocCols <- expand_grid(entity = names(ValSamples), score = names(ScoreCols)) |>
  mutate(key = paste(entity, score, sep = "_"))
stat_values <- function(stat) {
  vals <- DocStats |> filter(stat == !!stat) |>
    mutate(key = paste(entity, score, sep = "_"))
  vals$value[match(DocCols$key, vals$key)]
}
stat_row <- function(label, stat, fmt = f3, first_only = FALSE) {
  out <- fmt(stat_values(stat))
  if (first_only) out[DocCols$score != "Pred"] <- ""
  as_tibble_row(set_names(c(label, out), c("Statistic", DocCols$key)))
}
# Row of bracketed intervals [lo, hi] or parenthesized SEs under a statistic
ci_row <- function(stat) {
  lo <- stat_values(paste0(stat, "_lo")); hi <- stat_values(paste0(stat, "_hi"))
  out <- if_else(is.na(lo), "", paste0("[", f3(lo), ", ", f3(hi), "]"))
  as_tibble_row(set_names(c("", out), c("Statistic", DocCols$key)))
}
se_row <- function(stat) {
  se <- stat_values(paste0(stat, "_se"))
  out <- if_else(is.na(se), "", paste0("(", f3(se), ")"))
  as_tibble_row(set_names(c("", out), c("Statistic", DocCols$key)))
}
decile_row <- function(k) {
  s <- stat_values(paste0("dec", k, "_share"))
  m <- stat_values(paste0("dec", k, "_mean"))
  out <- if_else(is.na(s), "", paste0(f2(s), " [", f2(m), "]"))
  as_tibble_row(set_names(c(paste("Decile", k), out), c("Statistic", DocCols$key)))
}

Table25Groups <- list(
  "Panel A. AUC (documented race; bootstrap 95\\% interval)" = bind_rows(
    stat_row("Black vs.\\ white", "auc_bw"), ci_row("auc_bw"),
    stat_row("Black vs.\\ non-Black", "auc_bnb"), ci_row("auc_bnb"),
    stat_row("Black alone, non-Hispanic vs.\\ non-Black", "auc_alone"),
    stat_row("Black vs.\\ non-Black, excl.\\ Hispanic of unstated race", "auc_nohisp"),
    stat_row("Black vs.\\ all other head coaches", "auc_ball"), ci_row("auc_ball")),
  "Panel B. Calibration slope and reliability" = bind_rows(
    stat_row("Calibration slope, logit on logit($p$)", "cal_slope"), se_row("cal_slope"),
    stat_row("Calibration slope, prior-covariate FE", "cal_slope_fe"), se_row("cal_slope_fe"),
    stat_row("Reliability: Var($p$)/[E($p$)(1 $-$ E($p$))]", "reliability")),
  "Panel C. Mean P(Black)" = bind_rows(
    stat_row("Documented Black", "mean_black"),
    stat_row("Documented Black alone, non-Hispanic", "mean_black_alone"),
    stat_row("Documented white", "mean_white"),
    stat_row("Documented other race or Hispanic", "mean_other"),
    stat_row("Undocumented", "mean_undoc"),
    stat_row("All persons", "mean_all"),
    stat_row("Calibration-implied, Black: E[$p^2$]/E[$p$]", "implied_black")),
  "Panel D. Share documented Black [mean P(Black)] by decile of P(Black)" =
    map_dfr(1:10, decile_row),
  "Panel E. Persons" = bind_rows(
    stat_row("Documented Black", "n_black", fn, TRUE),
    stat_row("of which Black alone, non-Hispanic", "n_black_alone", fn, TRUE),
    stat_row("Documented white", "n_white", fn, TRUE),
    stat_row("Documented other race or Hispanic", "n_other", fn, TRUE),
    stat_row("of which Hispanic, race unstated", "n_hisp_norace", fn, TRUE),
    stat_row("Undocumented", "n_undoc", fn, TRUE)))
Table25 <- bind_rows(Table25Groups)

# Facts on the documented sample quoted in the notes: documented whites (one
# count per human: a linked staff and player record count once) with a text
# (article-sentence) source, and text-labelled whites whose label rests on
# ancestry or national origin (European descent, X-American, ...)
DocWhite <- bind_rows(ValSamples$players, ValSamples$staff) |> filter(DocGroup == "White")
NWhiteHumans <- n_distinct(DocWhite$HumanId)
ShareWhiteText <- mean(str_detect(coalesce(DocWhite$documented_sources, ""), "text"))
TextLabels <- read_csv(file.path(root, "data", "derived", "race_text_labels.csv"),
                       show_col_types = FALSE)
TextWhite <- TextLabels |> filter(race == "white")
ShareWhiteOrigin <- mean(str_detect(str_to_lower(coalesce(TextWhite$note, "")),
                                    "origin|descent|ancestr|heritage|european|immigra|american|roots"))
message(glue("15: documented whites {nrow(DocWhite)} records ({NWhiteHumans} persons), ",
             "with a text source {round(100 * ShareWhiteText, 1)}%; text-labelled whites ",
             "citing a national origin {round(100 * ShareWhiteOrigin, 1)}% of {nrow(TextWhite)}"))

# Head-coach facts: documented Hispanic head coaches of unstated race, the
# head-coach Black-vs-non-Black AUC with them excluded, documented whites
hc_stat <- function(stat, score = "Pred") {
  DocStats |> filter(entity == "head_coaches", score == !!score, stat == !!stat) |> pull(value)
}
NHCHispNoRace <- hc_stat("n_hisp_norace")
NHCWhite <- hc_stat("n_white")
NHCBlackComb <- hc_stat("n_black") - hc_stat("n_black_alone")
NPop <- map_int(ValSamples, nrow)
f3c <- function(x) formatC(x, format = "f", digits = 3)

Table25Notes <- c(
  "This table evaluates the predicted probability of being non-Hispanic Black alone on persons",
  "whose race a public source states (Wikidata, Wikipedia categories, or a two-coder classification",
  "of article sentences). Documented race is used here only: the primary posterior (Pred.)",
  "does not use it. Names is the names-and-county BIFSG posterior with Census population priors;",
  "Prior is the NFL prior alone (players: entry position, rookie era, draft round, college type,",
  "county availability; staff: first role group, unit and era), before the name update.",
  glue("Populations: players on a 2002--2025 game-day roster ({scales::comma(NPop[['players']])}),"),
  glue("staff persons of 2002--2025 ({scales::comma(NPop[['staff']])}), and the non-interim head coaches"),
  glue("of 2010--2025, each counted once ({NPop[['head_coaches']]}). Players and staff are separate"),
  "person records (a staff member who played has both, each with its own prior).",
  "Documented Black is Black alone or in combination, including Black Hispanic and multiracial",
  glue("Black persons (head coaches: {NHCBlackComb} of {hc_stat('n_black')}; TIDES counts head coaches"),
  "of two or more races separately); the score targets non-Hispanic Black alone, so the",
  "Black-alone rows restrict documented Black to non-Hispanic Black alone. Documented other race",
  "includes Hispanic, Asian, Pacific Islander, American Indian and non-Black multiracial persons;",
  "a Hispanic person whose race no source states counts as other",
  glue("(head coaches: {NHCHispNoRace}); whether such a person is Black is unknown, so the row"),
  "excluding Hispanic persons of unstated race drops them. All head coaches have",
  "a Wikipedia article, so documentation is positive-only, and the last AUC row treats",
  "undocumented head coaches as non-Black. Panel A: the AUC is the probability that a randomly",
  "drawn documented Black person has a higher score than a randomly drawn comparison person;",
  glue("intervals resample persons within each group (999 draws); the Black-white AUC is left blank"),
  glue("with fewer than {MinWhiteAUC} documented whites (head coaches: {NHCWhite})."),
  "Panel B: the calibration slope is the coefficient of a logit of documented Black on logit($p$)",
  "among documented persons (heteroskedasticity-robust standard errors in parentheses); the second",
  "row adds the prior covariates as fixed effects (cells without variation in documented race drop",
  "out). A slope of one means calibrated relative odds, below one overconfident predictions.",
  "Selection of documented persons on race shifts only the intercept, so the slope is robust to it,",
  "but not to fame-based selection within race. Reliability is Var($p$)/[E($p$)(1 $-$ E($p$))]",
  "over all persons of the population: under calibration, the $R^2$ of the race indicator on $p$,",
  "which governs the precision of regressions on $p$.",
  "Panel C: under calibration, the mean P(Black) of Black persons equals E[$p^2$]/E[$p$] over all",
  "persons of the population; documented Black persons below that value indicate miscalibration or the",
  "selection of documented persons. Panel D: deciles of each score among documented persons",
  "(head coaches: among all head coaches), with tied scores kept in one decile, so the Prior",
  "deciles are unequal; a calibrated score has a share documented Black close to",
  "the mean score in brackets. Documentation is itself selected on race (Wikipedia race categories",
  "are mostly African-American categories), so documented persons are mostly Black and the share",
  "documented Black exceeds the mean score in the low deciles; Panel D therefore checks ranking",
  "within the documented sample, not calibration in the population.",
  "Limitations: documented persons are famous (documentation requires an article), so these",
  "statistics may not carry over to the undocumented majority; documented whites are few",
  glue("({nrow(DocWhite)} person records, {NWhiteHumans} persons), {round(100 * ShareWhiteText)}\\% of them"),
  glue("come from article sentences and {round(100 * ShareWhiteOrigin)}\\% of the white text labels rest"),
  "on European ancestry or national origin, so the Black-white AUC compares Black persons with",
  "whites of distinctive European surnames. Individual accuracy for head coaches is low:",
  glue("documented Black head coaches have a mean predicted P(Black) of {f3c(hc_stat('mean_black'))}"),
  glue("and the head-coach AUCs and calibration slopes rest on {hc_stat('n_black')} documented Black and"),
  glue("{hc_stat('n_white') + hc_stat('n_other')} other documented head coaches."),
  "Regression calibration with these probabilities requires, among other conditions, calibration",
  "given the prior covariates (notes/race-prediction-design.md, section 4).")

Table25Tex <- kbl(Table25, format = "latex", booktabs = TRUE, escape = FALSE,
                  linesep = "", align = c("l", rep("r", ncol(Table25) - 1)),
                  col.names = c("", rep(c("Pred.", "Names", "Prior"), 3)),
                  caption = paste0("Predicted Race and Documented Race: Discrimination and Calibration",
                                   " \\label{tab:race-prediction-documented}")) |>
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9) |>
  add_header_above(c(" " = 1, "Players" = 3, "Staff" = 3,
                     "Head coaches, 2010--2025" = 3))
Ends25 <- cumsum(map_int(Table25Groups, nrow))
for (i in seq_along(Table25Groups)) {
  Table25Tex <- pack_rows(Table25Tex, names(Table25Groups)[i],
                          Ends25[i] - nrow(Table25Groups[[i]]) + 1, Ends25[i],
                          bold = FALSE, italic = TRUE, escape = FALSE)
}
Table25Tex <- add_notes(Table25Tex, Table25Notes)
save_exhibit_tex(Table25Tex, "table-25-race-prediction-documented", ValMeasure)

# ---------------------------------------------------------------------------
# Figure: predicted vs TIDES season series
# ---------------------------------------------------------------------------

GroupLabels <- c(players = "Players (game-day, week-weighted)",
                 head_coaches = "Head coaches", assistant_coaches = "Assistant coaches")
SeriesLong <- Validation |>
  filter(Group %in% names(GroupLabels)) |>
  select(Group, season, Predicted = p_black_any_pred,
         `Predicted, documented (PredDoc)` = p_black_any_preddoc,
         `Names only (BIFSG)` = p_black_bifsg) |>
  pivot_longer(-c(Group, season), names_to = "Series", values_to = "Share")
TidesLong <- Validation |>
  filter(Group %in% names(GroupLabels)) |>
  select(Group, season, `TIDES African-American / Black alone` = Tides_black,
         `TIDES Black + two or more races` = Tides_black_2plus) |>
  pivot_longer(-c(Group, season), names_to = "Series", values_to = "Share") |>
  filter(!is.na(Share))

FigTides <- ggplot() +
  geom_vline(xintercept = 2017.5, linetype = "dotted", colour = "grey50") +
  geom_line(data = SeriesLong, aes(season, Share, colour = Series, linetype = Series),
            linewidth = 0.7) +
  geom_point(data = TidesLong, aes(season, Share, shape = Series), size = 1.8) +
  facet_wrap(~ factor(Group, names(GroupLabels), GroupLabels), scales = "free_y") +
  scale_colour_manual(values = c("Predicted" = "#1b4f72",
                                 "Predicted, documented (PredDoc)" = "#b9770e",
                                 "Names only (BIFSG)" = "grey55")) +
  scale_linetype_manual(values = c("Predicted" = "solid",
                                   "Predicted, documented (PredDoc)" = "dashed",
                                   "Names only (BIFSG)" = "dotdash")) +
  scale_shape_manual(values = c("TIDES African-American / Black alone" = 16,
                                "TIDES Black + two or more races" = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Season", y = "Black share", colour = NULL, linetype = NULL, shape = NULL,
       caption = paste("TIDES players: media-guide 'African-American' through 2016; self-identified",
                       "'Black' (excluding two or more races) from 2019 (dotted line: break).")) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  theme_customs(base_size = 11)
save_exhibit_figure(FigTides, "figure-race-prediction-tides", ValMeasure,
                    width = 10, height = 4.5)

# ---------------------------------------------------------------------------
# Figure: distribution of P(Black) by prior group, posterior vs prior
# ---------------------------------------------------------------------------

# Players on a 2002-2025 game-day roster (by entry position group) and staff
# persons of 2002-2025 (by first role group), each person once. Box = the
# posterior P(Black) (prior x name/county update); diamond = mean prior.
# The share of the variation in P(Black) that the prior covariates carry is
# summarised by the R^2 of P(Black) on the prior (and, for reference, on the
# saturated prior-covariate cells); the rest is the name/county update.
DistPlayers <- PersonRace |>
  filter(entity == "player", person_id %in% RosterWeeks15$gsis_id) |>
  transmute(Entity = "Players", Level = pred_pos_group, p = p_black_any_pred,
            prior = prior_black_pred,
            Cell = paste(pred_pos_group, pred_rookie_era, pred_draft_bucket,
                         pred_college_type, pred_county_available))
DistStaff <- PersonRace |>
  filter(entity == "staff", person_id %in% StaffIds15) |>
  transmute(Entity = "Staff", Level = str_replace_all(pred_role_group_first, "_", " "),
            p = p_black_any_pred, prior = prior_black_pred,
            Cell = paste(pred_role_group_first, pred_unit_first, pred_first_era))
Dist <- bind_rows(DistPlayers, DistStaff) |>
  filter(!is.na(p), !is.na(Level))
PriorShare <- Dist |>
  group_by(Entity) |>
  summarise(N = n(), VarPost = var(p), VarPrior = var(prior),
            R2Prior = summary(lm(p ~ prior))$r.squared,
            R2Cells = summary(lm(p ~ factor(Cell)))$r.squared,
            NCells = n_distinct(Cell), MeanP = mean(p), .groups = "drop")
print(PriorShare)
EntityLabels <- with(PriorShare, set_names(
  glue("{Entity} (N = {scales::comma(N)}; R-squared of P(Black) on the prior = ",
       "{formatC(R2Prior, format = 'f', digits = 2)})"), Entity))
DistMeans <- Dist |>
  group_by(Entity, Level) |>
  summarise(N = n(), MeanP = mean(p), MeanPrior = mean(prior), .groups = "drop")
Dist <- Dist |>
  left_join(DistMeans |> select(Entity, Level, MeanP), by = c("Entity", "Level")) |>
  mutate(LevelOrd = paste(Entity, Level))
LevelOrder <- DistMeans |> arrange(Entity, MeanP) |> mutate(o = paste(Entity, Level)) |> pull(o)

FigDist <- ggplot(Dist, aes(x = p, y = factor(LevelOrd, LevelOrder))) +
  geom_boxplot(outlier.shape = NA, fill = "#d6e4f0", colour = "#1b4f72",
               width = 0.6, linewidth = 0.4) +
  geom_point(data = DistMeans |> mutate(LevelOrd = paste(Entity, Level)),
             aes(x = MeanPrior), shape = 18, size = 2.6, colour = "#b9770e") +
  facet_wrap(~ factor(Entity, names(EntityLabels), EntityLabels), scales = "free_y") +
  scale_y_discrete(labels = \(x) str_remove(x, "^(Players|Staff) ")) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(x = "Predicted P(Black) (box: posterior; diamond: mean prior)", y = NULL) +
  theme_customs(base_size = 11)
save_exhibit_figure(FigDist, "figure-race-prediction-distribution", ValMeasure,
                    width = 10, height = 6)

# ---------------------------------------------------------------------------
# Tidy validation estimates (output/estimates)
# ---------------------------------------------------------------------------

ValidationEstimates <- bind_rows(
  Validation |>
    select(Group, season, N, all_of(RaceCols), Tides_black, Tides_black_2plus,
           Tides_not_disclosed, Tides_upper_disclosed, TidesSource) |>
    pivot_longer(c(all_of(RaceCols), starts_with("Tides_")), names_to = "stat",
                 values_to = "value") |>
    transmute(table = "tides_season", entity = Group, season, stat, score = NA_character_,
              value, n = N, source = TidesSource),
  DocStats |>
    transmute(table = "documented", entity, season = NA_real_, stat, score, value,
              n = NA_real_, source = NA_character_),
  PriorShare |>
    pivot_longer(c(VarPost, VarPrior, R2Prior, R2Cells, NCells, MeanP), names_to = "stat",
                 values_to = "value") |>
    transmute(table = "prior_r2", entity = Entity, season = NA_real_,
              stat, score = "Pred", value, n = N, source = NA_character_))
save_estimates(ValidationEstimates, "15-race-prediction-validation", ValMeasure)

options(nfl.race_primary = OldPrimary15)
db_disconnect(con)
message("15: done in ", round(difftime(Sys.time(), T0Script15, units = "secs")), "s")

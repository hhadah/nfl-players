# ============================================================================
# 07-table-summary-statistics.R
# Data exhibits (descriptive only; no race-gap regressions, no causal
# estimates) from the analysis samples in data/datasets/analysis:
#   Table 1  tab:sumstats-team-season   team-season sample by staff-source era
#   Table 2  tab:staff-coverage         staff coverage by season period
#   Table 3  tab:player-season-position player-seasons by position group
#   Table 4  tab:contracts-margin       contract sample by market margin
#   Table 5  tab:draft-signal-coverage  draft prospects: pre-draft signals
#   Table 6  tab:linkage-cohort         linkage coverage by rookie cohort
#   Figure   figure-staff-size-by-role  mean staff size by role group
#   Figure   figure-linkage-by-cohort   linkage coverage by rookie cohort
# Tables are .tex fragments (kableExtra, threeparttable notes) written to
# tables_wd and paper_tables; figures (.pdf, .png) to figures_wd.
# Race: hand coding has not started, so hand-coded race is NA throughout;
# Table 1 reports the provisional (positive-only Wikipedia) and name-based
# BIFSG measures separately, with caveats in the notes.
# Requires 01-06 to have run.
# Date: 2026-09-26
# ============================================================================

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Write a kableExtra LaTeX table to tables_wd and paper_tables as <name>.tex
save_tex <- function(tab, name) {
  for (d in c(tables_wd, paper_tables)) {
    writeLines(as.character(tab), file.path(d, paste0(name, ".tex")))
  }
  message(glue("07: wrote {name}.tex"))
  invisible(tab)
}

# Save a ggplot to figures_wd as <name>.pdf and <name>.png
# (showtext renders text at its own dpi, so match it to the png dpi)
save_figure <- function(plot, name, width = 7, height = 4.5, dpi = 300) {
  use_showtext <- requireNamespace("showtext", quietly = TRUE)
  for (ext in c("pdf", "png")) {
    if (use_showtext) showtext::showtext_opts(dpi = if (ext == "png") dpi else 96)
    ggsave(file.path(figures_wd, paste0(name, ".", ext)), plot,
           width = width, height = height, dpi = dpi)
  }
  if (use_showtext) showtext::showtext_opts(dpi = 96)
  message(glue("07: wrote {name}.pdf/.png"))
  invisible(plot)
}

# LaTeX table with booktabs, a \label{tab:...} in the caption, and
# threeparttable notes; `df` already holds formatted character columns
make_table <- function(df, title, label, notes, col_names = names(df),
                       align = NULL, font_size = NULL, scale_down = FALSE) {
  kbl(df, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
      col.names = col_names, align = align,
      caption = paste0(title, " \\label{tab:", label, "}")) |>
    kable_styling(latex_options = c("hold_position", if (scale_down) "scale_down"),
                  font_size = font_size) |>
    footnote(general = notes, general_title = "Notes:", footnote_as_chunk = TRUE,
             threeparttable = TRUE, escape = FALSE)
}

# Number formatting: fixed decimals with thousands separators; "" for NA
fmt_num <- function(x, digits = 2) {
  out <- map2_chr(x, rep_len(digits, length(x)),
                  \(v, d) formatC(v, format = "f", digits = d, big.mark = ","))
  if_else(is.na(x) | is.nan(x), "", out)
}
fmt_int <- function(x) fmt_num(x, 0)

# Share of non-missing (or TRUE / 1) values
share_true <- function(x) mean(as.numeric(x) == 1, na.rm = TRUE)
share_obs  <- function(x) mean(!is.na(x))

# ---------------------------------------------------------------------------
# Analysis samples
# ---------------------------------------------------------------------------

TeamSeason        <- read_parquet(file.path(analysis, "team_season.parquet"))
StaffPersonSeason <- read_parquet(file.path(analysis, "staff_person_season.parquet"))
PlayerSeason      <- read_parquet(file.path(analysis, "player_season.parquet"))
Contracts         <- read_parquet(file.path(analysis, "contracts.parquet"))
DraftProspects    <- read_parquet(file.path(analysis, "draft_prospects.parquet"))

# Team-seasons whose staff box is observed. In 1999-2006, 22 season articles
# have no staff box; their staff rows come only from the article infobox (head
# coach, owner, GM: 2-4 persons), so staff counts and composition measures for
# them describe the infobox, not the staff. Template-era seasons all have a box.
StaffBox <- StaffPersonSeason |>
  group_by(franchise_id, season) |>
  summarise(StaffBoxObserved = any(season >= 2007 | coalesce(in_season_article, FALSE)),
            .groups = "drop")
stopifnot(nrow(StaffBox) == nrow(TeamSeason))

# Previous season's box, for measures that compare the staff with season - 1
StaffBox <- StaffBox |>
  left_join(StaffBox |> transmute(franchise_id, season = season + 1L,
                                  PrevStaffBoxObserved = StaffBoxObserved),
            by = c("franchise_id", "season"))
stopifnot(nrow(StaffBox) == nrow(TeamSeason))

TeamSeason <- TeamSeason |>
  left_join(StaffBox, by = c("franchise_id", "season"))
stopifnot(nrow(TeamSeason) == nrow(StaffBox), !anyNA(TeamSeason$StaffBoxObserved))

# ---------------------------------------------------------------------------
# Table 1: team-season sample by staff-source era
# ---------------------------------------------------------------------------

# Variables (group, name, label in words); shares are 0-1
Table1Vars <- tribble(
  ~Group, ~variable, ~Label,
  "Outcomes (regular season)", "WinPct", "Win percentage",
  "Outcomes (regular season)", "PointDiffPerGame", "Point differential per game",
  "Outcomes (regular season)", "OffEPAPerPlay", "Offensive EPA per play",
  "Outcomes (regular season)", "DefEPAPerPlay", "Defensive EPA per play allowed",
  "Outcomes (regular season)", "ExpectedWins", "Market expected wins",
  "Outcomes (regular season)", "WinsOverExpected", "Wins over market expectation",
  "Outcomes (regular season)", "Playoffs", "Made playoffs",
  "Staff size (persons listed)", "NAllStaff", "All staff",
  "Staff size (persons listed)", "NCoaches", "On-field coaches",
  "Staff size (persons listed)", "NCoordinators", "Coordinators (OC, DC, STC)",
  "Staff size (persons listed)", "NPositionCoaches", "Position coaches",
  "Staff size (persons listed)", "NAssistants", "Assistant and quality-control coaches",
  "Staff size (persons listed)", "NFrontOffice", "Front office",
  "Staff size (persons listed)", "NPersonnel", "Personnel and scouting",
  "Staff composition", "CodedShareAllStaff", "Share of staff with hand-coded race",
  "Staff composition", "ShareBlackProvCoaches", "Share of coaches flagged Black, provisional$^a$",
  "Staff composition", "ShareBlackProvFrontOffice", "Share of front office flagged Black, provisional$^a$",
  "Staff composition", "MeanPBlackBifsgCoaches", "Mean BIFSG P(Black), coaches$^b$",
  "Staff composition", "MeanPBlackBifsgFrontOffice", "Mean BIFSG P(Black), front office$^b$",
  "Staff composition", "BlauBifsgCoaches", "Expected Blau index (BIFSG), coaches$^b$",
  "Staff composition", "HCBlackProv", "Head coach flagged Black, provisional$^a$",
  "Staff composition", "HCPBlackBifsg", "Head coach BIFSG P(Black)$^b$",
  "Staff composition", "ShareCoachesNewToFranchise", "Share of coaches new to the franchise"
)

# Staff-size and staff-composition rows use team-seasons with an observed staff
# box (head-coach rows use all: the infobox lists the head coach); the share
# new to the franchise also needs the season - 1 box
StaffBoxVars <- Table1Vars$variable[Table1Vars$Group != "Outcomes (regular season)" &
                                      !str_starts(Table1Vars$variable, "HC")]
Table1Data <- TeamSeason |>
  mutate(across(all_of(StaffBoxVars), \(x) if_else(StaffBoxObserved, as.numeric(x), NA_real_)),
         ShareCoachesNewToFranchise = if_else(coalesce(PrevStaffBoxObserved, FALSE),
                                              ShareCoachesNewToFranchise, NA_real_))

# Mean, SD and N (non-missing) of each variable within each era
Table1Long <- Table1Data |>
  mutate(Era = if_else(season <= 2006, "Article", "Template")) |>
  select(Era, all_of(Table1Vars$variable)) |>
  mutate(across(-Era, as.numeric)) |>
  pivot_longer(-Era, names_to = "variable") |>
  group_by(Era, variable) |>
  summarise(Mean = mean(value, na.rm = TRUE), SD = sd(value, na.rm = TRUE),
            N = sum(!is.na(value)), .groups = "drop")

Table1 <- Table1Long |>
  mutate(Digits = if_else(str_detect(variable, "^N[A-Z]"), 1, 3),
         Mean = fmt_num(Mean, Digits), SD = fmt_num(SD, Digits),
         N = fmt_int(N)) |>
  select(-Digits) |>
  pivot_wider(names_from = Era, values_from = c(Mean, SD, N)) |>
  right_join(Table1Vars, by = "variable") |>
  mutate(variable = factor(variable, levels = Table1Vars$variable)) |>
  arrange(variable)

# Row groups for pack_rows (in table order)
Table1Groups <- rle(as.character(Table1$Group))

Table1Tex <- Table1 |>
  select(Label, Mean_Article, SD_Article, N_Article,
         Mean_Template, SD_Template, N_Template) |>
  make_table(title = "Summary Statistics: Team-Season Sample",
             label = "sumstats-team-season",
             col_names = c("", "Mean", "SD", "N", "Mean", "SD", "N"),
             align = c("l", rep("r", 6)), font_size = 9,
             notes = paste(
               "The unit of observation is a franchise-season (regular season),",
               "1999--2025; 861 team-seasons. Staff in 1999--2006 come from the",
               "staff box of the Wikipedia team-season article, which lists a",
               "partial staff; staff in 2007--2025 come from three revisions",
               "(preseason, midseason, late season) of the Wikipedia team staff",
               "template, and a person is counted if listed in any of them. Staff",
               "counts are therefore not comparable across the 2007 source break.",
               "Twenty-two 1999--2006 season articles have no staff box (only the",
               "infobox head coach, owner and GM); staff-size and staff-composition",
               "rows exclude them (N = 231), while outcome and head-coach rows use",
               "all team-seasons. The share of coaches new to the franchise also",
               "requires an observed staff box in the previous season.",
               "Shares are proportions (0--1). Hand-coded race is not yet",
               "available (the share of staff with hand-coded race is zero in every",
               "team-season), so no composition measure uses it. $^a$Provisional measure: 1 if a Wikipedia category",
               "flags the person as Black, 0 otherwise; it is a positive-only screen",
               "(persons without an article or category count as not Black) and is a",
               "lower bound on the Black share. $^b$Name-only BIFSG posterior",
               "(Census surname and first-name lists; no geography); it",
               "misclassifies most Black coaches and understates the Black share. The",
               "expected Blau index is the probability that two distinct staff",
               "members drawn at random belong to different BIFSG categories.",
               "Neither measure is a substitute for the hand-coded race.")) |>
  add_header_above(c(" " = 1, "1999--2006 (season articles)" = 3,
                     "2007--2025 (staff templates)" = 3))
Table1Tex <- reduce2(Table1Groups$values, cumsum(Table1Groups$lengths),
                     \(tab, g, end) pack_rows(tab, g, end - Table1Groups$lengths[
                       Table1Groups$values == g] + 1, end),
                     .init = Table1Tex)
save_tex(Table1Tex, "table-01-summary-statistics-team-season")

# ---------------------------------------------------------------------------
# Table 2: staff coverage by season period
# ---------------------------------------------------------------------------

# Season periods: the 2007 source break, then four-season blocks
period_of <- function(season) {
  cut(season, breaks = c(1998, 2002, 2006, 2010, 2014, 2018, 2022, 2025),
      labels = c("1999--2002", "2003--2006", "2007--2010", "2011--2014",
                 "2015--2018", "2019--2022", "2023--2025"))
}

# Team-season level: staff size and whether each key role is listed; staff
# measures are set to NA in team-seasons without an observed staff box
StaffCoverageTeam <- TeamSeason |>
  transmute(Period = period_of(season), season, StaffSource, StaffBoxObserved,
            NAllStaff, HasOC = NOC > 0, HasDC = NDC > 0, HasSTC = NSTC > 0,
            HasGM = NGM > 0) |>
  mutate(across(c(NAllStaff, HasOC, HasDC, HasSTC, HasGM),
                \(x) if_else(StaffBoxObserved, as.numeric(x), NA_real_)))

# Person-season level (team-seasons with a staff box): a person_id that is a
# Wikipedia title has an article ('name:<key>' ids are unlinked names)
StaffCoveragePerson <- StaffPersonSeason |>
  semi_join(filter(StaffBox, StaffBoxObserved), by = c("franchise_id", "season")) |>
  transmute(Period = period_of(season),
            HasArticle = !str_starts(person_id, "name:"))

Table2 <- StaffCoverageTeam |>
  group_by(Period) |>
  summarise(Source = if_else(first(StaffSource) == "staff_template",
                             "Templates", "Articles"),
            TeamSeasons = n(),
            ShareBox = mean(StaffBoxObserved),
            MedianStaff = median(NAllStaff, na.rm = TRUE),
            across(c(HasOC, HasDC, HasSTC, HasGM), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  left_join(StaffCoveragePerson |>
              group_by(Period) |>
              summarise(PersonSeasons = n(), ShareArticle = mean(HasArticle)),
            by = "Period") |>
  mutate(across(c(TeamSeasons, PersonSeasons), fmt_int),
         MedianStaff = fmt_num(MedianStaff, 1),
         across(c(ShareBox, HasOC, HasDC, HasSTC, HasGM, ShareArticle), \(x) fmt_num(x, 3)),
         Period = as.character(Period))

Table2Tex <- Table2 |>
  select(Period, Source, TeamSeasons, ShareBox, MedianStaff, HasOC, HasDC,
         HasSTC, HasGM, PersonSeasons, ShareArticle) |>
  make_table(title = "Staff Coverage by Season",
             label = "staff-coverage",
             col_names = c("Seasons", "Source", "Team-seasons", "With staff box",
                           "Median staff", "OC", "DC", "STC", "GM",
                           "Person-seasons", "With article"),
             align = c("l", "l", rep("r", 9)), font_size = 9, scale_down = TRUE,
             notes = paste(
               "Team-seasons are franchise-seasons; person-seasons are",
               "franchise-season-person rows of the staff sample. With staff box",
               "is the share of team-seasons whose staff box is observed; 22",
               "season articles in 1999--2006 have none (only the infobox head",
               "coach, owner and GM), and all other columns are computed among",
               "team-seasons with a staff box. Median staff is",
               "the median number of distinct persons listed on the team's staff",
               "(coaches, strength and conditioning, support staff and front",
               "office). Columns OC--GM report the share of team-seasons in which",
               "at least one offensive coordinator, defensive coordinator,",
               "special-teams coordinator or general manager is listed; teams",
               "without a listed GM title count as not listed. With article is the share of person-seasons whose",
               "person links to a Wikipedia article (the source of the provisional",
               "race flag); the rest are unlinked names. Articles: staff box of the",
               "Wikipedia team-season article (a partial staff). Templates: three",
               "within-season revisions of the Wikipedia team staff template.")) |>
  add_header_above(c(" " = 5, "Share with role listed" = 4, "Staff persons" = 2))
save_tex(Table2Tex, "table-02-staff-coverage-by-season")

# ---------------------------------------------------------------------------
# Table 3: player-seasons by position group (2013-2025)
# ---------------------------------------------------------------------------

# Position groups in table order and the unit whose snaps measure usage
PositionOrder <- c("QB", "RB", "WR", "TE", "OL", "DL", "LB", "DB", "K", "P", "LS")
OffenseGroups <- c("QB", "RB", "WR", "TE", "OL")
DefenseGroups <- c("DL", "LB", "DB")
SkillGroups   <- c("QB", "RB", "WR", "TE")

# Restrict to the snap-count era (2013+), when snaps and OTC pay are observed
# for most player-seasons, and to player-seasons with a position group
Table3Data <- PlayerSeason |>
  filter(season >= 2013) |>
  filter(!is.na(PositionGroup)) |>
  mutate(SnapShareOwnUnit = case_when(PositionGroup %in% OffenseGroups ~ OffSnapPctMean,
                                      PositionGroup %in% DefenseGroups ~ DefSnapPctMean,
                                      TRUE ~ STSnapPctMean))

Table3 <- Table3Data |>
  group_by(PositionGroup) |>
  summarise(N = n(),
            SharePlayed = mean(GamesPlayed > 0),
            GamesPlayed = mean(GamesPlayed),
            Starts = mean(GamesStartedDepth),
            SnapShare = mean(SnapShareOwnUnit, na.rm = TRUE),
            FantasyPoints = mean(FantasyPointsPPR),
            Tackles = mean(Tackles),
            Sacks = mean(Sacks),
            SharePay = mean(HasPay == 1),
            MeanCash = mean(CashPaid, na.rm = TRUE),
            MedianCash = median(CashPaid, na.rm = TRUE),
            .groups = "drop") |>
  # Depth-chart starts cover offense and defense only (NA by design for K, P, LS)
  mutate(Starts = if_else(PositionGroup %in% c(OffenseGroups, DefenseGroups), Starts, NA_real_),
         FantasyPoints = if_else(PositionGroup %in% SkillGroups, FantasyPoints, NA_real_),
         across(c(Tackles, Sacks), \(x) if_else(PositionGroup %in% DefenseGroups, x, NA_real_)),
         PositionGroup = factor(PositionGroup, levels = PositionOrder)) |>
  arrange(PositionGroup) |>
  mutate(N = fmt_int(N),
         across(c(SharePlayed, SnapShare, SharePay), \(x) fmt_num(x, 3)),
         across(c(GamesPlayed, Starts, FantasyPoints, Tackles, Sacks), \(x) fmt_num(x, 1)),
         across(c(MeanCash, MedianCash), \(x) fmt_num(x, 2)))

Table3Tex <- Table3 |>
  make_table(title = "Player-Season Sample by Position Group, 2013--2025",
             label = "player-season-position",
             col_names = c("Position", "N", "Played", "Games", "Starts",
                           "Snap share", "PPR points", "Tackles", "Sacks",
                           "Pay record", "Mean", "Median"),
             align = c("l", rep("r", 11)), font_size = 8, scale_down = TRUE,
             notes = paste(
               "The unit of observation is a player-season (regular season) on an",
               "NFL weekly roster in any status, 2013--2025. Position is the modal",
               "weekly-roster position group. Played is the share of player-seasons",
               "with at least one game played; Games and Starts are means over all",
               "player-seasons (Starts are weeks listed as an offensive or defensive",
               "depth-chart starter, so they are blank for K, P and LS). Blank cells",
               "are not applicable.",
               "Snap share is the mean share of the unit's snaps over games with a",
               "snap-count row: offense for QB--OL, defense for DL--DB, special",
               "teams for K, P and LS. PPR points (QB--TE) and Tackles and Sacks",
               "(DL--DB) are regular-season means from nflverse player statistics.",
               "Pay record is the share of player-seasons with a realized",
               "OverTheCap cap-table year; cash paid (in millions of nominal",
               "dollars) is summarized among those player-seasons.")) |>
  add_header_above(c(" " = 2, "Usage" = 4, "Production" = 3,
                     " " = 1, "Cash paid, \\\\$M" = 2), escape = FALSE)
save_tex(Table3Tex, "table-03-player-season-by-position")

# ---------------------------------------------------------------------------
# Table 4: contract sample by market margin (SampleMain)
# ---------------------------------------------------------------------------

MarginOrder <- c("Rookie scale (drafted)", "UDFA entry", "Veteran free agent (UFA)",
                 "Re-sign/Extension", "Tag/Tender", "Other/SFA/Practice",
                 "Type unmatched")

# Main contract sample; contracts without an OTC type get their own row
Table4Data <- Contracts |>
  filter(SampleMain == 1) |>
  mutate(Margin = factor(coalesce(MarketMargin, "Type unmatched"), levels = MarginOrder),
         # Snap counts start in 2013: positive prior-season snaps (any unit)
         # among contracts signed 2014+; NA (by design) for earlier signings
         PriorSnapsPositive = if_else(year_signed >= 2014,
                                      coalesce(PriorOffSnaps + PriorDefSnaps + PriorSTSnaps, 0) > 0,
                                      NA),
         # OL and LS have no college box-score statistics (NA by design);
         # OTC market position is never missing
         BoxScorePos = !position %in% c("LT", "LG", "C", "RG", "RT", "LS"),
         CollegeStatsObserved = if_else(BoxScorePos, CollegeFinalObservable == 1, NA))
stopifnot(!anyNA(Table4Data$Margin), !anyNA(Table4Data$position))

Table4 <- Table4Data |>
  group_by(Margin) |>
  summarise(N = n(),
            MeanAPY = mean(apy), MedianAPY = median(apy),
            MeanGuaranteed = mean(guaranteed),
            ShareGuaranteedZero = mean(GuaranteedZero == 1),
            MeanYears = mean(years, na.rm = TRUE),
            SharePriorSeason = mean(InPanelPriorSeason == 1),
            SharePriorSnaps = mean(PriorSnapsPositive, na.rm = TRUE),
            SharePriorStats = mean(coalesce(PriorHasStatRow, 0L) == 1),
            ShareCollege = mean(CollegeStatsObserved, na.rm = TRUE),
            ShareRecruit = mean(HasRecruit == 1),
            .groups = "drop") |>
  mutate(N = fmt_int(N),
         across(c(MeanAPY, MedianAPY, MeanGuaranteed), \(x) fmt_num(x, 2)),
         MeanYears = fmt_num(MeanYears, 1),
         across(starts_with("Share"), \(x) fmt_num(x, 3)),
         Margin = as.character(Margin))

Table4Tex <- Table4 |>
  make_table(title = "Contract Sample by Market Margin",
             label = "contracts-margin",
             col_names = c("Market margin", "N", "Mean", "Median", "Mean",
                           "Share zero", "Years", "Rostered", "Snaps",
                           "Box score", "College stats", "Recruit"),
             align = c("l", rep("r", 11)), font_size = 8, scale_down = TRUE,
             notes = paste(
               "The unit of observation is an OverTheCap (OTC) contract in the main",
               "contract sample: signed 2011--2026, linked to an NFL player id, with",
               "a signing year, keeping the first of any rows with identical terms.",
               "The market margin groups the OTC contract type: rookie scale =",
               "Drafted; UDFA entry = undrafted free agent; veteran free agent =",
               "UFA; re-sign/extension = Extension; tag/tender = Franchise,",
               "Transition, RFA and ERFA; other = SFA, Practice and Other; type",
               "unmatched = no OTC contract-history type. APY is the average value",
               "per year and guarantees are in millions of nominal dollars; OTC does",
               "not separate zero from unknown guarantees (Share zero). Years is",
               "the mean contract length. Prior season (signing year minus one):",
               "Rostered is the share on a regular-season weekly roster; Snaps the",
               "share with positive offensive, defensive or special-teams snaps,",
               "among contracts signed 2014 or later (snap counts start in 2013);",
               "Box score the share with an",
               "nflverse or PFR statistics row. College stats is the share whose",
               "final pre-NFL college season has complete box scores (FBS 2009+,",
               "FCS 2022+), among players outside OL and LS (OTC position), which",
               "have no box-score statistics; Recruit the share linked to a 247Sports recruit",
               "profile. OTC records the signing year but not the signing date.")) |>
  add_header_above(c(" " = 2, "APY, \\\\$M" = 2, "Guarantees" = 2, " " = 1,
                     "Prior NFL season" = 3, "Pre-NFL signals" = 2), escape = FALSE)
save_tex(Table4Tex, "table-04-contracts-by-market-margin")

# ---------------------------------------------------------------------------
# Table 5: draft prospects, pre-draft signal coverage by class period and round
# ---------------------------------------------------------------------------

# Class periods follow source availability: CFBD pre-draft grades from the
# 2004 draft; CFBD college rosters from 2004 (classes 2005+); complete FBS box
# scores from 2009 (final season observable from about the 2010 class);
# college defensive statistics from 2016 (classes 2017+)
Table5Data <- DraftProspects |>
  mutate(ClassPeriod = cut(DraftClass, breaks = c(1999, 2004, 2009, 2016, 2026),
                           labels = c("Draft classes 2000--2004",
                                      "Draft classes 2005--2009",
                                      "Draft classes 2010--2016",
                                      "Draft classes 2017--2026")),
         RoundGroup = factor(case_when(Drafted == 0 ~ "Undrafted invitees",
                                       Round <= 2 ~ "Rounds 1--2",
                                       Round <= 4 ~ "Rounds 3--4",
                                       TRUE ~ "Rounds 5--7"),
                             levels = c("Rounds 1--2", "Rounds 3--4",
                                        "Rounds 5--7", "Undrafted invitees")),
         BoxScorePos = !PositionGroup %in% c("OL", "LS"))

Table5 <- Table5Data |>
  group_by(ClassPeriod, RoundGroup) |>
  summarise(N = n(),
            ShareGsis = share_obs(gsis_id),
            ShareCombine = share_obs(CombineRowId),
            ShareForty = share_obs(Forty),
            ShareGrade = share_obs(PreDraftGrade),
            ShareCollegeLink = mean(HasCollegeLink == 1),
            ShareCollegeStats = mean(coalesce(CollegeFinalObservable[BoxScorePos], 0L) == 1),
            ShareDefStats = mean(coalesce(CollegeDefFinalObservable[BoxScorePos &
                                   PositionGroup %in% DefenseGroups], 0L) == 1),
            ShareRecruit = mean(HasRecruit == 1),
            .groups = "drop") |>
  # Undrafted invitees have a combine row by construction and no pre-draft
  # grade by design: not applicable (blank)
  mutate(across(c(ShareCombine, ShareGrade),
                \(x) if_else(RoundGroup == "Undrafted invitees", NA_real_, x))) |>
  mutate(N = fmt_int(N), across(starts_with("Share"), \(x) fmt_num(x, 3)))
stopifnot(nrow(Table5) == 16, sum(Table5Data$DraftClass < 2000 | Table5Data$DraftClass > 2026) == 0)

Table5Tex <- Table5 |>
  select(-ClassPeriod) |>
  make_table(title = "Draft Prospects: Coverage of Pre-Draft Signals",
             label = "draft-signal-coverage",
             col_names = c("", "N", "NFL id", "Combine", "40 time",
                           "Pre-draft grade", "College link", "Final season",
                           "Defense", "Recruit"),
             align = c("l", rep("r", 9)), font_size = 8, scale_down = TRUE,
             notes = paste(
               "The unit of observation is a draft prospect: every drafted player",
               "and every NFL combine invitee who was not drafted (undrafted",
               "invitees), draft classes 2000--2026. Undrafted players not invited",
               "to the combine are outside the sample. Each column is the share of",
               "prospects with the signal. NFL id: linked to an NFL player id",
               "(needed for race measures and NFL outcomes). Combine: a combine",
               "record; 40 time: a recorded 40-yard dash. Pre-draft grade: CFBD",
               "pre-draft grade (available from the 2004 draft). Combine and",
               "pre-draft grade are blank for undrafted invitees (a combine record",
               "by construction; grades exist for drafted players only). College link: linked to a CFBD college player. Final",
               "season: the final pre-NFL college season has complete box scores",
               "(FBS 2009+, FCS 2022+), among prospects outside OL and LS, which",
               "have no box-score statistics. Defense: share of DL, LB and DB",
               "prospects whose final season has complete box scores and is 2016",
               "or later (college defensive statistics start in 2016). Recruit: linked to a 247Sports recruit profile.",
               "Class periods follow these source start dates."))
Table5Periods <- levels(Table5Data$ClassPeriod)
for (i in seq_along(Table5Periods)) {
  Table5Tex <- pack_rows(Table5Tex, Table5Periods[i], 4 * i - 3, 4 * i)
}
save_tex(Table5Tex, "table-05-draft-signal-coverage")

# ---------------------------------------------------------------------------
# Table 6: linkage coverage by rookie cohort (players, 2002-2025 entrants)
# ---------------------------------------------------------------------------

# One row per player who entered the NFL in 2002-2025 (RookieSeason), with
# the position group of his first panel season; linkage fields are
# time-invariant within player
PlayerCohort <- PlayerSeason |>
  filter(between(RookieSeason, 2002, 2025)) |>
  arrange(gsis_id, season) |>
  group_by(gsis_id) |>
  slice_head(n = 1) |>
  ungroup() |>
  transmute(gsis_id, RookieSeason, PositionGroup, Drafted = Undrafted == 0,
            CollegeLink = HasCollegeLink == 1,
            CollegeStats = coalesce(CollegeFinalObservable, 0L) == 1,
            BoxScorePos = !PositionGroup %in% c("OL", "LS"),
            Recruit = HasRecruit == 1,
            Combine = CombineInvite == 1,
            PreDraftGrade = !is.na(PreDraftGrade))

# Three-season cohort bins
Table6 <- PlayerCohort |>
  mutate(Cohort = cut(RookieSeason, breaks = seq(2001, 2025, 3),
                      labels = paste0(seq(2002, 2023, 3), "--", seq(2004, 2025, 3)))) |>
  group_by(Cohort) |>
  summarise(N = n(),
            ShareDrafted = mean(Drafted),
            ShareCollegeLink = mean(CollegeLink),
            ShareCollegeStats = mean(CollegeStats[BoxScorePos]),
            ShareRecruit = mean(Recruit),
            ShareCombine = mean(Combine),
            ShareGrade = mean(PreDraftGrade[Drafted]),
            .groups = "drop") |>
  mutate(N = fmt_int(N), across(starts_with("Share"), \(x) fmt_num(x, 3)),
         Cohort = as.character(Cohort))

Table6Tex <- Table6 |>
  make_table(title = "Linkage Coverage by Rookie Cohort",
             label = "linkage-cohort",
             col_names = c("Rookie cohort", "Players", "Drafted", "College link",
                           "Final season", "Recruit", "Combine",
                           "Pre-draft grade"),
             align = c("l", rep("r", 7)), font_size = 9,
             notes = paste(
               "The unit of observation is a player whose first NFL season",
               "(rookie season) is 2002--2025 and who appears on a regular-season",
               "weekly roster in any status; from about 2016 the rosters also list",
               "practice-squad players, which raises the count of (mostly",
               "undrafted) players. Each column is a share of players. College",
               "link: linked to a CFBD college player. Final season: the final",
               "pre-NFL college season has complete box scores (FBS 2009+, FCS",
               "2022+), among players outside OL and LS. Recruit: linked to a",
               "247Sports recruit profile. Combine: a linked NFL combine record.",
               "Pre-draft grade: CFBD pre-draft grade, among drafted players."))
save_tex(Table6Tex, "table-06-linkage-by-rookie-cohort")

# ---------------------------------------------------------------------------
# Figure: mean staff size by role group over seasons
# ---------------------------------------------------------------------------

# Collapse primary role groups into six groups for the figure
RoleGroupLabels <- c(head_coach = "Head coach and coordinators",
                     coordinator = "Head coach and coordinators",
                     position_coach = "Position coaches",
                     assistant_coach = "Assistant and QC coaches",
                     strength_conditioning = "S&C and support staff",
                     support_staff = "S&C and support staff",
                     general_manager = "GM, owners and executives",
                     owner_executive = "GM, owners and executives",
                     personnel_scouting = "Personnel, scouting, other front office",
                     other_front_office = "Personnel, scouting, other front office")

# Persons per team-season by group, averaged over team-seasons with an
# observed staff box (a team-season with no one in a group counts as 0)
StaffSizeByRole <- StaffPersonSeason |>
  semi_join(filter(StaffBox, StaffBoxObserved), by = c("franchise_id", "season")) |>
  mutate(RoleGroup = factor(RoleGroupLabels[PrimaryRoleGroup],
                            levels = unique(RoleGroupLabels))) |>
  count(franchise_id, season, RoleGroup, name = "NStaff") |>
  complete(nesting(franchise_id, season), RoleGroup, fill = list(NStaff = 0L)) |>
  group_by(season, RoleGroup) |>
  summarise(MeanStaff = mean(NStaff), .groups = "drop")
stopifnot(!anyNA(StaffSizeByRole$RoleGroup))

StaffSizePlot <- ggplot(StaffSizeByRole, aes(season, MeanStaff, colour = RoleGroup)) +
  geom_vline(xintercept = 2006.5, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  annotate("text", x = 2006.3, y = max(StaffSizeByRole$MeanStaff), hjust = 1,
           vjust = 1, size = 3, label = "Season articles | Staff templates") +
  scale_x_continuous(breaks = seq(1999, 2025, 2)) +
  labs(x = "Season", y = "Mean persons listed per team", colour = NULL,
       title = "Staff Size by Role Group",
       caption = str_wrap(paste("Mean number of persons per team-season by primary role.",
                       "1999-2006: staff box of the Wikipedia team-season article;",
                       "2007-2025: Wikipedia staff templates (three revisions per season).",
                       "Excludes the 22 article-era team-seasons without a staff box.",
                       "The 2007 dip in personnel and support staff is at the first",
                       "template season and is likely a source artifact."), 110)) +
  guides(colour = guide_legend(nrow = 2)) +
  theme_customs(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(StaffSizePlot, "figure-staff-size-by-role-group", width = 8, height = 5)

# ---------------------------------------------------------------------------
# Figure: linkage coverage by rookie cohort
# ---------------------------------------------------------------------------

LinkageByCohort <- PlayerCohort |>
  group_by(RookieSeason) |>
  summarise(`College link` = mean(CollegeLink),
            `College final-season box score` = mean(CollegeStats[BoxScorePos]),
            `Recruit profile` = mean(Recruit),
            `Combine record` = mean(Combine),
            .groups = "drop") |>
  pivot_longer(-RookieSeason, names_to = "Link", values_to = "Share") |>
  mutate(Link = fct_inorder(Link))

LinkagePlot <- ggplot(LinkageByCohort, aes(RookieSeason, Share, colour = Link)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_x_continuous(breaks = seq(2002, 2025, 2)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "Rookie season", y = "Share of players", colour = NULL,
       title = "Linkage Coverage by Rookie Cohort",
       caption = str_wrap(paste("Players with a first NFL season in 2002-2025 on a weekly roster.",
                       "Box score: complete box scores in the final pre-NFL college season",
                       "(players outside OL and LS)."), 110)) +
  guides(colour = guide_legend(nrow = 2)) +
  theme_customs(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(LinkagePlot, "figure-linkage-coverage-by-cohort", width = 8, height = 5)

# ============================================================================
# 04-player-season-sample.R
# Builds analysis/player_season: one row per gsis_id x season for every
# player on a 2002-2025 weekly roster in the REG season (any status). Combines
#   - identity (name, modal position group, age at Sep 1, experience, draft
#     slot, entry year),
#   - REG-season usage: roster weeks by status, primary franchise, games,
#     starts, snaps, injury weeks (nfl_season_usage),
#   - REG-season production: box scores, PFR advanced defense, NGS, PPR
#     points (nfl_season_production), and one-season lags of the main ones,
#   - realized pay and the governing contract (season_pay),
#   - time-invariant pre-NFL signals: college production and team context,
#     recruit profile, combine, CFBD pre-draft grade,
#   - race measures from load_person_race (kept separate, never imputed).
# Helpers: programs/00-player-functions.R.
# Date: 2026-09-26
# ============================================================================

source(file.path(programs, "00-player-functions.R"))

con <- db_connect()

# ---------------------------------------------------------------------------
# Universe: distinct REG (gsis_id, season) on the weekly rosters, 2002-2025
# ---------------------------------------------------------------------------

Universe <- tbl(con, "nfl_rosters_weekly") |>
  filter(season_type == "REG", !is.na(gsis_id), gsis_id != "",
         season >= 2002, season <= 2025) |>
  distinct(gsis_id, season) |>
  collect() |>
  mutate(season = as.integer(season))

# ---------------------------------------------------------------------------
# Identity
# ---------------------------------------------------------------------------

# nfl_players covers all but ~1,500 roster-only ids (mostly 2016+ camp and
# practice-squad players); for those, name and birth date come from the
# weekly rosters (latest non-missing value)
Players <- tbl(con, "nfl_players") |>
  select(gsis_id, display_name, birth_date, rookie_season, draft_year,
         draft_round, draft_pick) |>
  collect() |>
  mutate(birth_date = as.Date(birth_date))
RosterIdentity <- tbl(con, "nfl_rosters_weekly") |>
  filter(!is.na(gsis_id), gsis_id != "") |>
  select(gsis_id, season, week, full_name, birth_date, rookie_year) |>
  collect() |>
  arrange(gsis_id, desc(season), desc(week)) |>
  group_by(gsis_id) |>
  summarise(RosterName = full_name[!is.na(full_name)][1],
            RosterBirthDate = birth_date[!is.na(birth_date)][1],
            RosterRookieYear = rookie_year[!is.na(rookie_year)][1],
            FirstRosterSeason = min(season), .groups = "drop")
DraftPicks <- tbl(con, "nfl_draft_picks") |>
  filter(!is.na(gsis_id)) |>
  select(gsis_id, PickSeason = season, PickRound = round, PickOverall = pick) |>
  collect() |>
  arrange(gsis_id, PickSeason) |>
  distinct(gsis_id, .keep_all = TRUE)
EntryYear <- tbl(con, "player_xwalk") |>
  select(gsis_id, entry_year) |>
  collect()

Identity <- distinct(Universe, gsis_id) |>
  left_join(Players, by = "gsis_id") |>
  left_join(RosterIdentity, by = "gsis_id") |>
  left_join(DraftPicks, by = "gsis_id") |>
  left_join(EntryYear, by = "gsis_id") |>
  transmute(gsis_id,
            InNflPlayers = as.integer(!is.na(display_name) | gsis_id %in% Players$gsis_id),
            display_name = coalesce(display_name, RosterName),
            birth_date = coalesce(birth_date, as.Date(RosterBirthDate)),
            RookieSeason = as.integer(coalesce(rookie_season, RosterRookieYear)),
            DraftYear = as.integer(coalesce(draft_year, PickSeason)),
            DraftRound = as.integer(coalesce(draft_round, PickRound)),
            DraftPick = as.integer(coalesce(draft_pick, PickOverall)),
            Undrafted = as.integer(is.na(DraftRound)),
            entry_year = as.integer(entry_year),
            FirstRosterSeason = as.integer(FirstRosterSeason))

# ---------------------------------------------------------------------------
# Season usage, production and pay
# ---------------------------------------------------------------------------

Usage <- nfl_season_usage(con)
Production <- nfl_season_production(con)
Pay <- season_pay(con)

# Box-score counts (nfl_box_counts): a rostered player-season with no
# nflverse stats row recorded none of them (0), except in seasons the source
# does not record the stat (targets 2003-2008, TFL 2003-2011, QB hits
# 2003-2005), where the count is NA for everyone. EPA, CPOE, target share and
# rates stay NA. PFR advanced defense (2018+) and NGS stay NA when absent.
box_counts <- nfl_box_counts
Unrecorded <- attr(Production, "unrecorded")
print(Unrecorded |> group_by(Stat) |>
        summarise(Seasons = paste(range(season), collapse = "-"), N = n()))

PlayerSeason <- Universe |>
  left_join(Identity, by = "gsis_id") |>
  left_join(Usage, by = c("gsis_id", "season")) |>
  left_join(Production |> mutate(HasStatRow = 1L), by = c("gsis_id", "season")) |>
  left_join(Pay |> mutate(HasPay = 1L), by = c("gsis_id", "season")) |>
  mutate(HasStatRow = coalesce(HasStatRow, 0L),
         HasPay = coalesce(HasPay, 0L),
         across(all_of(box_counts), \(x) coalesce(as.numeric(x), 0)),
         Age = as.numeric(as.Date(paste0(season, "-09-01")) - birth_date) / 365.25,
         Experience = season - RookieSeason,
         IsRookie = as.integer(Experience == 0))
for (i in seq_len(nrow(Unrecorded))) {
  PlayerSeason[[Unrecorded$Stat[i]]][PlayerSeason$season == Unrecorded$season[i]] <- NA
}

# One-season lags of the main usage/production/pay measures (NA unless the
# player is in the sample in season t-1)
lag_vars <- c("GamesPlayed", "GamesStartedDepth", "WeeksGameDay", "OffSnaps",
              "DefSnaps", "STSnaps", "OffSnapPctMean", "DefSnapPctMean",
              "FantasyPointsPPR", "PassAtt", "PassYds", "PassTD", "PassInt",
              "PassEPA", "RushAtt", "RushYds", "RushTD", "RushEPA", "Targets",
              "Rec", "RecYds", "RecTD", "RecEPA", "Tackles", "TFL", "Sacks",
              "QBHits", "PassesDefended", "DefInt", "ForcedFumbles", "FGMade",
              "FGAtt", "PfrPressures", "PfrMissedTacklePct",
              "PfrYardsPerTargetAllowed", "InjuryWeeks", "CapNumber", "CashPaid")
# (vectorised equivalent of lag_within() by gsis_id: the previous row must be
# the same player in season - 1)
PlayerSeason <- PlayerSeason |>
  arrange(gsis_id, season) |>
  mutate(PrevIsTminus1 = lag(gsis_id) == gsis_id & lag(season) == season - 1L,
         PrevIsTminus1 = coalesce(PrevIsTminus1, FALSE),
         across(all_of(lag_vars), \(x) if_else(PrevIsTminus1, lag(x), x[NA_integer_]),
                .names = "Lag{.col}"),
         InSampleTminus1 = as.integer(PrevIsTminus1)) |>
  select(-PrevIsTminus1)

# ---------------------------------------------------------------------------
# Pre-NFL signals (time-invariant)
# ---------------------------------------------------------------------------

College <- college_production(con)
Recruit <- recruit_signals(con)
Combine <- combine_signals(con)
PreDraft <- predraft_signals(con)

PlayerSeason <- PlayerSeason |>
  left_join(College |> mutate(HasCollegeLink = 1L), by = "gsis_id") |>
  left_join(Recruit, by = "gsis_id") |>
  left_join(Combine, by = "gsis_id") |>
  left_join(PreDraft, by = "gsis_id") |>
  mutate(HasCollegeLink = coalesce(HasCollegeLink, 0L),
         CollegeCareerObservable = coalesce(CollegeCareerObservable, 0L),
         CollegeFinalObservable = coalesce(CollegeFinalObservable, 0L),
         CollegeDefCareerObservable = coalesce(CollegeDefCareerObservable, 0L),
         CollegeDefFinalObservable = coalesce(CollegeDefFinalObservable, 0L),
         HasRecruit = as.integer(!is.na(recruit_id)),
         CombineInvite = coalesce(CombineInvite, 0L)) |>
  select(-BoxScorePosition)

# ---------------------------------------------------------------------------
# Race measures (three measures kept separate; never imputed)
# ---------------------------------------------------------------------------

PersonRace <- load_person_race(con, hand_coded) |>
  filter(entity == "player") |>
  select(gsis_id = person_id, race, hispanic, black_any, nonwhite, race_source,
         black_provisional, black_provisional_source, wiki_cat_black,
         p_black_bifsg, p_white_bifsg, p_hispanic_bifsg, race_bifsg)

PlayerSeason <- PlayerSeason |>
  left_join(PersonRace, by = "gsis_id")

db_disconnect(con)

# ---------------------------------------------------------------------------
# Column order
# ---------------------------------------------------------------------------

PlayerSeason <- PlayerSeason |>
  select(gsis_id, season, display_name, PositionGroup, PrimaryFranchise,
         birth_date, Age, RookieSeason, Experience, IsRookie, DraftYear,
         DraftRound, DraftPick, Undrafted, entry_year, FirstRosterSeason,
         InNflPlayers, InSampleTminus1,
         starts_with("Weeks"), NFranchises, NFranchisesGameDay,
         GamesPlayed, GamesPlayedSnapBased, GamesStartedDepth, GamesStartedSnaps,
         OffSnaps, DefSnaps, STSnaps, OffSnapPctMean, DefSnapPctMean, STSnapPctMean,
         InjuryReportOutWeeks, InjuryWeeks,
         HasStatRow, StatGames, PassComp:NgsYACAboveExp,
         starts_with("Lag"),
         HasPay, NPayRows, CapNumberSuspect, CapNumber:OnRookieContract,
         HasCollegeLink, CollegeCareerObservable, CollegeFinalObservable,
         CollegeDefCareerObservable, CollegeDefFinalObservable,
         FirstCollegeSeason, FinalCollegeSeason, NCollegeSeasons,
         NCollegeSeasonsWithStats, FinalCollegeTeam, FinalCollegeConference,
         FinalCollegeClassification, FinalCollegePower, FinalCollegeHBCU,
         FinalCollegeSPRating, FinalCollegeSRS,
         starts_with("Coll"),
         HasRecruit, recruit_id, starts_with("Recruit"),
         CombineInvite, CombineYear, CombineHeight, CombineWeight, Forty,
         Vertical, Bench, BroadJump, Cone, Shuttle, AthleticScore,
         AthleticScoreN, AthleticSizeScore, AthleticSpeedScore,
         AthleticExplosionScore, AthleticAgilityScore, CombineLinkMethod,
         PreDraftRank, PreDraftPosRank, PreDraftGrade, CfbdDraftConference,
         race, hispanic, black_any, nonwhite, race_source, black_provisional,
         black_provisional_source, wiki_cat_black, p_black_bifsg, p_white_bifsg,
         p_hispanic_bifsg, race_bifsg) |>
  arrange(gsis_id, season)

# ---------------------------------------------------------------------------
# Codebook labels
# ---------------------------------------------------------------------------

IdentityLabels <- c(
  gsis_id = "NFL GSIS player id (key)",
  season = "NFL season (key); REG season",
  display_name = "Player name (nfl_players; weekly-roster name for ids not in nfl_players)",
  PositionGroup = "Modal weekly-roster position group in the season (most REG weeks; ties to the latest week)",
  PrimaryFranchise = "Franchise with the most game-day (Active/Inactive) REG weeks, then most weeks in any status, then latest week",
  birth_date = "Birth date (nfl_players, else weekly rosters)",
  Age = "Age in years on September 1 of the season",
  RookieSeason = "First NFL season (nfl_players.rookie_season, else weekly-roster rookie_year)",
  Experience = "season - RookieSeason (negative for roster seasons, e.g. practice squad or reserve, before the nflverse rookie season)",
  IsRookie = "1 if Experience == 0",
  DraftYear = "NFL draft year (nfl_players, else nfl_draft_picks); NA if undrafted",
  DraftRound = "NFL draft round; NA if undrafted",
  DraftPick = "Overall NFL draft pick; NA if undrafted",
  Undrafted = "1 if no draft record in nfl_players or nfl_draft_picks",
  entry_year = "NFL entry year from player_xwalk (college seasons < entry_year are pre-NFL)",
  FirstRosterSeason = "First season on a 2002-2025 weekly roster (left-censored at 2002)",
  InNflPlayers = "1 if the gsis_id is in nfl_players (roster-only ids have no race measures or links)",
  InSampleTminus1 = "1 if the player is in the sample in season - 1 (lagged measures observed)"
)

UsageLabels <- c(
  WeeksOnRoster = "Distinct REG weeks on any weekly roster (any status)",
  WeeksGameDay = "Distinct REG weeks with status Active or Inactive (game-day roster)",
  WeeksActive = "REG weeks with status ACT and not a game-day inactive (see WeeksInactive)",
  WeeksInactive = "REG weeks game-day inactive: INA, or ACT with status_description_abbr I* (abbr missing for many 2016-2020 rows, so undercounted then)",
  WeeksReserve = "REG weeks on a reserve list: RES (incl. injured reserve), PUP, NWT, RSN, RSR",
  WeeksPracticeSquad = "REG weeks on the practice squad (DEV); observable essentially from 2016-2017",
  WeeksCut = "REG weeks with status CUT",
  WeeksOther = "REG weeks with another status (SUS, EXE, RET, TRC/TRD/TRT, UFA/RFA/UDF, E01/E14, missing)",
  NFranchises = "Number of franchises with a weekly-roster row in the REG season (any status)",
  NFranchisesGameDay = "Number of franchises with at least one game-day (Active/Inactive) week",
  GamesPlayed = "REG games with a box-score row or any snap (snaps 2013+; before 2013 players without box-score stats, e.g. OL, are undercounted)",
  GamesPlayedSnapBased = "1 if season >= 2013 (GamesPlayed also uses snap counts)",
  GamesStartedDepth = "REG weeks listed as depth-chart starter (rank 1, offense/defense) in weeks the franchise played",
  GamesStartedSnaps = "REG games with offense or defense snap share >= 0.5 (2013+; NA before)",
  OffSnaps = "Offensive snaps, REG (2013+; NA before; 0 if no snap row)",
  DefSnaps = "Defensive snaps, REG (2013+; NA before; 0 if no snap row)",
  STSnaps = "Special-teams snaps, REG (2013+; NA before; 0 if no snap row)",
  OffSnapPctMean = "Mean offensive snap share over games with a snap-count row (2013+)",
  DefSnapPctMean = "Mean defensive snap share over games with a snap-count row (2013+)",
  STSnapPctMean = "Mean special-teams snap share over games with a snap-count row (2013+)",
  InjuryReportOutWeeks = "REG weeks listed Out or Doubtful on the injury report (2009+; NA before)",
  InjuryWeeks = "REG weeks Out/Doubtful on the injury report or on RES/PUP roster status (before 2009 roster status only)"
)

# Production: box-score counts are 0 when the player has no stats row
bx <- "REG, nflverse player stats; 0 if no stats row"
ProductionLabels <- c(
  HasStatRow = "1 if the player has an nflverse REG stats row or a PFR advanced-defense row",
  StatGames = "Games in the nflverse REG stats row (games with a recorded stat)",
  PassComp = glue("Pass completions ({bx})"), PassAtt = glue("Pass attempts ({bx})"),
  PassYds = glue("Passing yards ({bx})"), PassTD = glue("Passing TDs ({bx})"),
  PassInt = glue("Interceptions thrown ({bx})"), SacksTaken = glue("Sacks taken ({bx})"),
  PassEPA = "Passing EPA, REG (NA if no stats row)", PassCPOE = "Passing CPOE, REG (nflverse; NA if none)",
  RushAtt = glue("Rushing attempts ({bx})"), RushYds = glue("Rushing yards ({bx})"),
  RushTD = glue("Rushing TDs ({bx})"), RushEPA = "Rushing EPA, REG (NA if no stats row)",
  Targets = glue("Targets ({bx}; NA in 2003-2008, not recorded in the source)"), Rec = glue("Receptions ({bx})"),
  RecYds = glue("Receiving yards ({bx})"), RecTD = glue("Receiving TDs ({bx})"),
  RecEPA = "Receiving EPA, REG (NA if no stats row)", TargetShare = "Target share, REG (nflverse; NA in 2003-2008)",
  SoloTackles = glue("Solo tackles ({bx})"),
  Tackles = glue("Combined tackles = solo + with assist + assists ({bx})"),
  TFL = glue("Tackles for loss ({bx}; NA in 2003-2011, not recorded in the source)"), Sacks = glue("Sacks ({bx})"),
  QBHits = glue("QB hits ({bx}; NA in 2003-2005, not recorded in the source)"), PassesDefended = glue("Passes defended ({bx})"),
  DefInt = glue("Interceptions ({bx})"), ForcedFumbles = glue("Forced fumbles ({bx})"),
  DefTD = glue("Defensive TDs ({bx})"),
  FGMade = glue("Field goals made ({bx})"), FGAtt = glue("Field goals attempted ({bx})"),
  XPMade = glue("Extra points made ({bx})"), XPAtt = glue("Extra points attempted ({bx})"),
  Punts = glue("Punts ({bx})"), PuntYds = glue("Gross punt yards ({bx})"),
  PuntNetYds = glue("Net punt yards ({bx})"), PuntsInside20 = glue("Punts inside the 20 ({bx})"),
  PuntReturns = glue("Punt returns ({bx})"), KickReturns = glue("Kickoff returns ({bx})"),
  FantasyPointsPPR = glue("PPR fantasy points, skill-position summary ({bx})"),
  CompletionPct = "PassComp / PassAtt", YardsPerAttempt = "PassYds / PassAtt",
  YardsPerCarry = "RushYds / RushAtt", YardsPerReception = "RecYds / Rec",
  FGPct = "FGMade / FGAtt",
  PfrPressures = "PFR pressures (2018+; NA if no PFR defense row)",
  PfrHurries = "PFR hurries (2018+)", PfrQBKnockdowns = "PFR QB knockdowns (2018+)",
  PfrBlitzes = "PFR blitzes (2018+)", PfrCombTackles = "PFR combined tackles (2018+)",
  PfrMissedTackles = "PFR missed tackles (2018+)",
  PfrMissedTacklePct = "PFR missed tackles / (combined tackles + missed) (2018+)",
  PfrTargetsAllowed = "PFR targets allowed in coverage (2018+)",
  PfrCompAllowed = "PFR completions allowed in coverage (2018+)",
  PfrYardsAllowed = "PFR yards allowed in coverage (2018+)",
  PfrTDAllowed = "PFR TDs allowed in coverage (2018+)",
  PfrCompPctAllowed = "PfrCompAllowed / PfrTargetsAllowed (2018+)",
  PfrYardsPerTargetAllowed = "PfrYardsAllowed / PfrTargetsAllowed (2018+)",
  NgsCPOE = "NGS completion % above expectation, REG season (2016+, qualifiers)",
  NgsTimeToThrow = "NGS average time to throw, REG season (2016+, qualifiers)",
  NgsRYOEPerAtt = "NGS rush yards over expected per attempt, REG season (2016+, qualifiers)",
  NgsSeparation = "NGS average separation, REG season (2016+, qualifiers)",
  NgsYACAboveExp = "NGS average YAC above expectation, REG season (2016+, qualifiers)"
)

PayLabels <- c(
  HasPay = "1 if the player has a realized OverTheCap cap-table year",
  NPayRows = "Number of cap-table rows summed (2 when the gsis_id maps to two OTC ids)",
  CapNumberSuspect = "1 if a cap-table row has cap_percent > 1 (corrupt source row); its cap number and cap percent are set to NA",
  CapNumber = "Cap number, $ millions (OTC, realized year)",
  CashPaid = "Cash paid, $ millions",
  BaseSalary = "Base salary, $ millions",
  Bonuses = "Prorated + option + roster + workout + per-game roster + other bonuses, $ millions",
  ProratedBonus = "Prorated signing bonus, $ millions",
  GuaranteedSalary = "Guaranteed salary, $ millions (0 may mean unknown in the source)",
  CapPercent = "Cap number as a share (0-1) of the team cap (OTC; summed over rows)",
  PayFranchise = "Franchise of the cap-table row with the larger cap number",
  OtcContractId = "Contract OTC attaches to the cap-table year (latest contract with that franchise signed on or before the year)",
  GoverningContractId = "Contract in force in the season: OtcContractId unless it is an extension not yet in effect (then the contract it extends)",
  ExtensionPending = "1 if OTC attaches a signed-but-not-yet-effective extension to the year and GoverningContractId is the earlier contract",
  GoverningContractType = "OTC contract type of the governing contract (Drafted, UDFA, UFA, RFA, ERFA, Extension, Franchise, Transition, Practice, SFA, Other)",
  GoverningAPY = "APY of the governing contract, $ millions",
  GoverningYearSigned = "Year the governing contract was signed (0 = missing in source)",
  GoverningYears = "Length of the governing contract in years",
  GoverningValue = "Total value of the governing contract, $ millions",
  GoverningGuaranteed = "Guarantees of the governing contract, $ millions (0 may mean unknown)",
  OnRookieContract = "1 if the governing contract type is Drafted or UDFA (NA if no governing contract)"
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
  HasCollegeLink = "1 if the player links to at least one CFBD college id (player_college_xwalk)",
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

SignalLabels <- c(
  HasRecruit = "1 if the player links to a 247 recruit profile (player_xwalk.recruit_id)",
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
  RecruitLinkConfidence = "Confidence of the NFL-to-recruit link (high/medium)",
  CombineInvite = "1 if a linked NFL combine row exists (0 = no linked row; undercounts UDFA invitees)",
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
  CombineLinkMethod = "Combine-to-gsis link method (pfr_id, draft_slot, name_pos_year)",
  PreDraftRank = "CFBD pre-draft overall ranking (2004+ drafts, slot-linked picks)",
  PreDraftPosRank = "CFBD pre-draft position ranking",
  PreDraftGrade = "CFBD pre-draft grade",
  CfbdDraftConference = "College conference in the CFBD draft record"
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

AllLabels <- c(IdentityLabels, UsageLabels, ProductionLabels, PayLabels,
               CollegeLabels, SignalLabels, RaceLabels)
LagLabels <- setNames(paste0("One-season lag (t-1) of ", lag_vars, ": ",
                             AllLabels[lag_vars], " (NA unless in sample in t-1)"),
                      paste0("Lag", lag_vars))
AllLabels <- c(AllLabels, LagLabels)
AllLabels <- setNames(as.character(AllLabels), names(AllLabels))

# ---------------------------------------------------------------------------
# Write and validate
# ---------------------------------------------------------------------------

write_sample(PlayerSeason, "player_season", key = c("gsis_id", "season"),
             labels = AllLabels)

# Row count equals the distinct REG (gsis_id, season) pairs on the rosters
stopifnot(nrow(PlayerSeason) == nrow(Universe),
          !anyDuplicated(PlayerSeason[c("gsis_id", "season")]))

# Independent count of distinct REG (gsis_id, season) on the weekly rosters
con <- db_connect()
NRosterPairs <- DBI::dbGetQuery(con, "
  SELECT COUNT(*) AS n FROM (
    SELECT DISTINCT gsis_id, season FROM nfl_rosters_weekly
    WHERE season_type = 'REG' AND gsis_id IS NOT NULL AND gsis_id <> ''
      AND season BETWEEN 2002 AND 2025)")$n
db_disconnect(con)
stopifnot(nrow(PlayerSeason) == NRosterPairs)
message(glue("player_season: key unique; {nrow(PlayerSeason)} rows = distinct REG roster (gsis_id, season) pairs"))

# Coverage by season and by position group: share of player-seasons with
# snaps (2013+), any production row, realized pay, college stats (career or
# final season observable) and a recruit profile
coverage <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(N = n(),
              GameDay = mean(WeeksGameDay > 0),
              Snaps = mean(coalesce(OffSnaps + DefSnaps + STSnaps, 0) > 0),
              Production = mean(HasStatRow == 1),
              Pay = mean(HasPay == 1),
              CollegeStats = mean(CollegeCareerObservable == 1 | CollegeFinalObservable == 1),
              Recruit = mean(HasRecruit == 1),
              Combine = mean(CombineInvite == 1),
              BlackProvisional = mean(coalesce(black_provisional, 0L) == 1),
              BifsgObserved = mean(!is.na(p_black_bifsg)),
              .groups = "drop") |>
    mutate(across(GameDay:BifsgObserved, \(x) round(x, 3)))
}
CoverageSeason <- coverage(PlayerSeason, season)
CoveragePosition <- coverage(PlayerSeason, PositionGroup)
CoverageSeasonPosition <- coverage(PlayerSeason, season, PositionGroup)
write_csv(CoverageSeasonPosition, file.path(analysis, "coverage_player_season.csv"))
withr::with_options(list(width = 200), {
  print(as.data.frame(CoverageSeason), row.names = FALSE)
  print(as.data.frame(CoveragePosition), row.names = FALSE)
})

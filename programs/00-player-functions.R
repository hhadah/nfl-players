# ============================================================================
# 00-player-functions.R
# Shared helpers for the player-level analysis samples (04-player-season,
# 05-contract, 06-draft). Each helper takes a read-only DuckDB connection
# (db_connect()) and returns a tibble with one row per key:
#   - college_production(con)    gsis_id: pre-NFL college production, final
#                                college team context, PPA/usage
#   - recruit_signals(con)       gsis_id: 247 composite recruit profile
#   - combine_signals(con)       gsis_id: NFL combine measurables and the
#                                RAS-style athletic score
#   - combine_athletic_scores(con) nfl_combine row: RAS-style athletic score
#   - predraft_signals(con)      gsis_id: CFBD pre-draft ranking and grade
#   - nfl_season_usage(con)      gsis_id x season (REG): roster weeks by
#                                status, franchise, games, snaps, injuries
#   - nfl_season_production(con) gsis_id x season (REG): box scores, PFR
#                                advanced defense, NGS, fantasy points
#   - season_pay(con)            gsis_id x season: realized cap table and the
#                                governing contract
# Sourced at the top of each player script:
#   source(file.path(programs, "00-player-functions.R"))
# Expects 00-setup-functions.R (safe_div, mean_or_na) to be loaded.
# Date: 2026-09-26; athletic score added 2026-10-02
# ============================================================================

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

# Height string "6-2" (feet-inches) to inches; NA when unparseable
height_to_inches <- function(x) {
  ft <- suppressWarnings(as.numeric(str_extract(x, "^\\d+")))
  inch <- suppressWarnings(as.numeric(str_extract(x, "(?<=-)\\d+(\\.\\d+)?$")))
  if_else(is.na(ft) | is.na(inch), NA_real_, 12 * ft + inch)
}

# Sum that returns NA (not 0) when every value is missing
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# ---------------------------------------------------------------------------
# Recruit signals (247 composite; one row per gsis_id)
# ---------------------------------------------------------------------------

# Primary recruit profile of each NFL player (player_xwalk.recruit_id: HS
# before JUCO, then high-confidence link, then smallest id). The recruits
# table has no position or state rankings (the CFBD API does not provide
# them), so RecruitPosRank is computed here: rank by rating (ties share the
# best rank) among all profiles of the same recruit_class x recruit_type x
# 247 position. HS state and hometown county FIPS come from the profile.
recruit_signals <- function(con) {
  AllRecruits <- tbl(con, "recruits") |>
    select(recruit_id, recruit_type, recruit_class, position, position_group,
           ranking, stars, rating, state_province, country,
           hometown_info_fips_code, height_clean, weight_clean) |>
    collect() |>
    group_by(recruit_class, recruit_type, position) |>
    mutate(RecruitPosRank = if_else(is.na(rating) | is.na(position), NA_integer_,
                                    as.integer(min_rank(desc(rating)))),
           RecruitPosN = sum(!is.na(rating))) |>
    ungroup()

  tbl(con, "player_xwalk") |>
    filter(!is.na(recruit_id)) |>
    select(gsis_id, recruit_id, RecruitLinkMethod = recruit_link_method,
           RecruitLinkConfidence = recruit_link_confidence) |>
    collect() |>
    inner_join(AllRecruits, by = "recruit_id") |>
    transmute(gsis_id, recruit_id, RecruitType = recruit_type,
              RecruitClass = as.integer(recruit_class),
              RecruitPosition = position, RecruitStars = stars,
              RecruitRating = rating, RecruitNationalRank = ranking,
              RecruitPosRank, RecruitPosN,
              RecruitState = state_province, RecruitCountry = country,
              RecruitCountyFips = hometown_info_fips_code,
              RecruitHeight = height_clean, RecruitWeight = weight_clean,
              RecruitLinkMethod, RecruitLinkConfidence)
}

# ---------------------------------------------------------------------------
# Combine signals (one row per linked gsis_id)
# ---------------------------------------------------------------------------

# NFL combine measurables from nfl_combine (linked rows only). CombineInvite
# is 1 for every returned row; a player with no linked row gets 0 in the
# samples, which means "no linked combine row" (59% of undrafted invitees and
# 97% of drafted invitees link, so 0 undercounts invitations among UDFAs).
combine_signals <- function(con) {
  Scores <- combine_athletic_scores(con) |>
    filter(!is.na(gsis_id)) |>
    select(gsis_id, season, AthleticScore, AthleticScoreN, AthleticSizeScore,
           AthleticSpeedScore, AthleticExplosionScore, AthleticAgilityScore)
  tbl(con, "nfl_combine") |>
    filter(!is.na(gsis_id)) |>
    select(gsis_id, season, ht, wt, forty, vertical, bench, broad_jump, cone,
           shuttle, link_method) |>
    collect() |>
    mutate(season = as.integer(season)) |>
    left_join(Scores, by = c("gsis_id", "season")) |>
    transmute(gsis_id, CombineYear = season, CombineInvite = 1L,
              CombineHeight = height_to_inches(ht), CombineWeight = wt,
              Forty = forty, Vertical = vertical, Bench = bench,
              BroadJump = broad_jump, Cone = cone, Shuttle = shuttle,
              AthleticScore, AthleticScoreN, AthleticSizeScore,
              AthleticSpeedScore, AthleticExplosionScore, AthleticAgilityScore,
              CombineLinkMethod = link_method)
}

# ---------------------------------------------------------------------------
# Athletic score (RAS-style; one row per nfl_combine row)
# ---------------------------------------------------------------------------

# Combine position -> scoring position. EDGE is scored with DE, OLB/ILB with
# LB, SAF with S and G with OG. The generic labels OL, DL and DB (used in some
# 2016-2023 classes) are scored against the pooled position family.
athletic_position <- function(pos) {
  case_when(pos %in% c("OG", "G") ~ "OG",
            pos %in% c("DE", "EDGE") ~ "DE",
            pos %in% c("OLB", "ILB", "LB") ~ "LB",
            pos %in% c("S", "SAF") ~ "S",
            pos == "CB/WR" ~ "CB",
            TRUE ~ pos)
}
athletic_family <- function(position) {
  case_when(position %in% c("OT", "OG", "C", "OL") ~ "OL",
            position %in% c("DE", "DT", "DL") ~ "DL",
            position %in% c("CB", "S", "DB") ~ "DB",
            TRUE ~ position)
}

# 0-10 percentile score of x against the reference values ref (x is itself in
# ref): 10 * (#{ref worse than x} + 0.5 * #{other ref equal to x}) / (n - 1).
# NA when x is NA or fewer than 10 reference values exist.
athletic_percentile <- function(x, ref, higher_better = TRUE) {
  if (!higher_better) {
    x <- -x
    ref <- -ref
  }
  ref <- sort(ref[!is.na(ref)])
  n <- length(ref)
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (n < 10 || !any(ok)) return(out)
  below <- findInterval(x[ok], ref, left.open = TRUE)
  ties <- findInterval(x[ok], ref) - below - 1
  out[ok] <- pmin(pmax(10 * (below + 0.5 * pmax(ties, 0)) / (n - 1), 0), 10)
  out
}

# Athletic score on a 0-10 scale, following the published Relative Athletic
# Score method (Kent Lee Platte; ras.football/about, read 2026-10-02):
#   1. each measurable is scored 0-10 as a percentile among all combine
#      participants at the same scoring position from the first observed
#      class through the prospect's combine year: bigger is better for height
#      and weight, faster is better for the forty, cone and shuttle, and
#      higher is better for bench, vertical and broad jump;
#   2. a score needs at least 6 measured components; the raw score is the
#      mean of the component scores;
#   3. the raw score is scored again as a percentile among the raw scores at
#      the same position through that year.
# Differences from RAS: 8 of its 10 measurables (no 10- and 20-yard splits),
# combine results only (no pro days), and reference classes from 2000 (the
# first nflverse combine class) rather than 1987, so the earliest classes are
# scored against small reference sets. Hence the name AthleticScore, not RAS.
# Size/Speed/Explosion/Agility are means of their component scores.
# Returns one row per nfl_combine row (season, player_name, pos, school,
# gsis_id) with the scores.
combine_athletic_scores <- function(con) {
  Rows <- tbl(con, "nfl_combine") |>
    select(season, player_name, pos, school, gsis_id, ht, wt, forty, bench,
           vertical, broad_jump, cone, shuttle) |>
    collect() |>
    mutate(season = as.integer(season), HeightIn = height_to_inches(ht),
           AthleticPosition = athletic_position(pos),
           Generic = AthleticPosition %in% c("OL", "DL", "DB"),
           Family = athletic_family(AthleticPosition),
           RowId = row_number())
  components <- c(HeightIn = TRUE, wt = TRUE, forty = FALSE, bench = TRUE,
                  vertical = TRUE, broad_jump = TRUE, cone = FALSE, shuttle = FALSE)
  # Reference set of a row: same specific position (or the pooled family for
  # a generic label), classes up to and including the row's class
  score_against <- function(values, higher) {
    out <- rep(NA_real_, nrow(Rows))
    for (p in unique(Rows$AthleticPosition)) {
      in_pos <- which(Rows$AthleticPosition == p)
      in_ref <- if (p %in% c("OL", "DL", "DB")) which(Rows$Family == athletic_family(p)) else in_pos
      for (t in unique(Rows$season[in_pos])) {
        target <- in_pos[Rows$season[in_pos] == t]
        ref <- in_ref[Rows$season[in_ref] <= t]
        out[target] <- athletic_percentile(values[target], values[ref], higher)
      }
    }
    out
  }
  Scores <- imap(components, \(higher, v) score_against(Rows[[v]], higher)) |>
    as_tibble() |>
    set_names(paste0("Score", names(components)))
  Rows <- bind_cols(Rows, Scores) |>
    mutate(AthleticScoreN = rowSums(!is.na(pick(starts_with("Score")))),
           RawScore = if_else(AthleticScoreN >= 6,
                              rowMeans(pick(starts_with("Score")), na.rm = TRUE),
                              NA_real_))
  Rows$AthleticScore <- score_against(Rows$RawScore, TRUE)
  Rows |>
    mutate(AthleticSizeScore = rowMeans(pick(ScoreHeightIn, Scorewt), na.rm = TRUE),
           AthleticSpeedScore = Scoreforty,
           AthleticExplosionScore = rowMeans(pick(Scorebench, Scorevertical,
                                                  Scorebroad_jump), na.rm = TRUE),
           AthleticAgilityScore = rowMeans(pick(Scorecone, Scoreshuttle), na.rm = TRUE),
           across(c(AthleticSizeScore, AthleticExplosionScore, AthleticAgilityScore),
                  \(x) if_else(is.nan(x), NA_real_, x))) |>
    select(season, player_name, pos, school, gsis_id, AthleticPosition,
           AthleticScore, AthleticScoreN, AthleticSizeScore, AthleticSpeedScore,
           AthleticExplosionScore, AthleticAgilityScore)
}

# ---------------------------------------------------------------------------
# Pre-draft signals (CFBD draft records; one row per linked gsis_id)
# ---------------------------------------------------------------------------

# CFBD pre-draft overall ranking, position ranking and grade (2004+ drafts),
# with the CFBD college conference at the draft. gsis_id is attached in the
# DB only when the draft slot agrees; one row per gsis_id.
predraft_signals <- function(con) {
  tbl(con, "cfbd_draft_picks") |>
    filter(!is.na(gsis_id)) |>
    select(gsis_id, season, overall, pre_draft_ranking,
           pre_draft_position_ranking, pre_draft_grade, college_conference) |>
    collect() |>
    arrange(gsis_id, season) |>
    distinct(gsis_id, .keep_all = TRUE) |>
    transmute(gsis_id, PreDraftRank = pre_draft_ranking,
              PreDraftPosRank = pre_draft_position_ranking,
              PreDraftGrade = pre_draft_grade,
              CfbdDraftConference = college_conference)
}

# ---------------------------------------------------------------------------
# College production (one row per gsis_id with a college link)
# ---------------------------------------------------------------------------

# Power conferences by season (conference names as in college_teams):
#   2004-2013: the six BCS automatic-qualifier conferences (ACC, Big East,
#              Big Ten, Big 12, Pac-10/Pac-12, SEC);
#   2014-2023: the Power Five (ACC, Big Ten, Big 12, Pac-12, SEC);
#   2024+:     the Power Four (ACC, Big Ten, Big 12, SEC; the Pac-12 kept two
#              members after the 2024 realignment).
# Notre Dame (an FBS independent) is counted as Power in every season.
# HBCU conference: SWAC or MEAC (the two HBCU FCS conferences).
power_conference <- function(conference, season, team) {
  case_when(team == "Notre Dame" ~ 1L,
            is.na(conference) ~ NA_integer_,
            season <= 2013 & conference %in% c("ACC", "Big East", "Big Ten", "Big 12",
                                               "Pac-10", "Pac-12", "SEC") ~ 1L,
            season <= 2023 & conference %in% c("ACC", "Big Ten", "Big 12",
                                               "Pac-12", "SEC") ~ 1L,
            season >= 2024 & conference %in% c("ACC", "Big Ten", "Big 12", "SEC") ~ 1L,
            TRUE ~ 0L)
}

# CFBD stat components kept as career sums: category|stat_type -> name.
# Only stat_aggregation == 'sum' types; rates are recomputed from these.
college_stat_map <- c(
  "passing|ATT" = "PassAtt", "passing|COMPLETIONS" = "PassComp",
  "passing|YDS" = "PassYds", "passing|TD" = "PassTD", "passing|INT" = "PassInt",
  "rushing|CAR" = "RushAtt", "rushing|YDS" = "RushYds", "rushing|TD" = "RushTD",
  "receiving|REC" = "Rec", "receiving|YDS" = "RecYds", "receiving|TD" = "RecTD",
  "defensive|TOT" = "Tackles", "defensive|SOLO" = "SoloTackles",
  "defensive|TFL" = "TFL", "defensive|SACKS" = "Sacks",
  "defensive|QB HUR" = "QBHurries", "defensive|PD" = "PassesDefended",
  "interceptions|INT" = "DefInt",
  "fumbles|REC" = "FumblesRecovered",
  "kicking|FGM" = "FGMade", "kicking|FGA" = "FGAtt",
  "kicking|XPM" = "XPMade", "kicking|XPA" = "XPAtt",
  "punting|NO" = "Punts", "punting|YDS" = "PuntYds"
)
# Components that CFBD reports only from 2016 (defensive and fumbles)
college_stats_from_2016 <- c("Tackles", "SoloTackles", "TFL", "Sacks",
                             "QBHurries", "PassesDefended", "FumblesRecovered")

# Pre-NFL college production of each linked NFL player. Uses every CFBD id
# linked to the gsis_id (player_college_xwalk) and only seasons < entry_year
# (CFBD ids can carry stale roster rows after entry).
#
# Observability rules (production is NA, never 0, when not observable):
#   - A college team-season is box-score complete when college_team_stats
#     flags box_score_complete (FBS every season; FCS from 2022) AND the
#     season is 2009+ (CFBD player stats are complete for FBS only from 2009).
#     D-II/III/NAIA team-seasons are never complete. A team-season seen only
#     through a placeholder (negative) CFBD id is not complete either: those
#     ids never carry stats (e.g. Aaron Donald, Pitt 2010-13), so zeros would
#     be false. Likewise a player with no pre-entry stat row at all is not
#     observable: some positive ids are stat-less too (e.g. Will Fuller,
#     Notre Dame 2013-15, 144 catches), and an NFL player with no college
#     stat row in any category is far more likely missing than a true zero
#     (this drops a few pre-2016 defenders whose only stat would be DefInt).
#   - CollegeCareerObservable = 1 when every pre-entry team-season in which
#     the player appears (roster or stat rows) is complete. Career totals are
#     NA otherwise. The defensive and fumble components (college_stats_from_2016)
#     additionally require every such season to be 2016+.
#   - CollegeFinalObservable = 1 when the final pre-entry team-season is
#     complete (final-season stats are NA otherwise; defensive/fumble ones also
#     need final season >= 2016).
#   - OL and LS have no box-score production: all production is NA for them
#     (NFL position group from player_xwalk).
#   Within an observable season a missing component is a true 0 (the player
#   recorded none).
# Forced fumbles are not reported by CFBD and are not available.
# Seasons outside CFBD rosters/stats (e.g. JUCO or pre-2004) are not seen.
college_production <- function(con) {
  Links <- tbl(con, "player_college_xwalk") |>
    select(gsis_id, player_id, entry_year) |>
    collect()
  PositionGroup <- tbl(con, "player_xwalk") |>
    select(gsis_id, position_group) |>
    collect()

  # Box-score completeness by college team-season
  Complete <- tbl(con, "college_team_stats") |>
    select(season, team, box_score_complete) |>
    collect() |>
    mutate(Complete = box_score_complete & season >= 2009) |>
    select(season, team, Complete)

  # Long stat rows of linked ids, pre-entry seasons, summable types only
  StatsLong <- tbl(con, "college_player_stats") |>
    filter(player_id %in% local(unique(Links$player_id)),
           stat_aggregation == "sum") |>
    select(player_id, season, team, category, stat_type, stat) |>
    collect() |>
    inner_join(Links, by = "player_id", relationship = "many-to-many") |>
    filter(season < entry_year) |>
    mutate(Stat = unname(college_stat_map[paste(category, stat_type, sep = "|")]))

  # Pre-entry team-seasons of each player (roster or stat rows)
  Rosters <- tbl(con, "college_players") |>
    filter(player_id %in% local(unique(Links$player_id))) |>
    distinct(player_id, season, team) |>
    collect() |>
    inner_join(Links, by = "player_id", relationship = "many-to-many") |>
    filter(season < entry_year)
  NStatRows <- StatsLong |> count(gsis_id, season, team, name = "NStatRows")
  # A team-season seen only through a placeholder (negative) CFBD id is not
  # observable: placeholder ids never carry stat rows, so the player's stats
  # for that season (if any) sit under an unlinked id
  TeamSeasons <- bind_rows(select(Rosters, gsis_id, player_id, season, team),
                           distinct(StatsLong, gsis_id, player_id, season, team)) |>
    group_by(gsis_id, season, team) |>
    summarise(RealId = any(player_id > 0), .groups = "drop") |>
    left_join(NStatRows, by = c("gsis_id", "season", "team")) |>
    left_join(Complete, by = c("season", "team")) |>
    mutate(NStatRows = coalesce(NStatRows, 0L),
           Complete = coalesce(Complete, FALSE) & RealId)

  # Wide season totals per gsis_id x season (summed over teams and ids);
  # absent components are 0 within a season
  stat_names <- unname(college_stat_map)
  SeasonStats <- StatsLong |>
    filter(!is.na(Stat)) |>
    group_by(gsis_id, season, Stat) |>
    summarise(stat = sum(stat), .groups = "drop") |>
    pivot_wider(names_from = Stat, values_from = stat, values_fill = 0)
  for (s in setdiff(stat_names, names(SeasonStats))) SeasonStats[[s]] <- 0

  # Career observability and final college season/team
  Career <- TeamSeasons |>
    group_by(gsis_id) |>
    summarise(FirstCollegeSeason = as.integer(min(season)),
              FinalCollegeSeason = as.integer(max(season)),
              NCollegeSeasons = n_distinct(season),
              NCollegeSeasonsWithStats = n_distinct(season[NStatRows > 0]),
              CareerComplete = all(Complete),
              CareerFrom2016 = all(season >= 2016),
              .groups = "drop")
  FinalTeam <- TeamSeasons |>
    semi_join(Career, by = c("gsis_id", season = "FinalCollegeSeason")) |>
    arrange(gsis_id, desc(NStatRows), team) |>
    distinct(gsis_id, .keep_all = TRUE) |>
    select(gsis_id, FinalCollegeTeam = team)
  FinalComplete <- TeamSeasons |>
    semi_join(Career, by = c("gsis_id", season = "FinalCollegeSeason")) |>
    group_by(gsis_id) |>
    summarise(FinalComplete = all(Complete), .groups = "drop")

  # Career sums and final-season values
  CareerSums <- SeasonStats |>
    group_by(gsis_id) |>
    summarise(across(all_of(stat_names), sum), .groups = "drop")
  FinalStats <- SeasonStats |>
    semi_join(Career, by = c("gsis_id", season = "FinalCollegeSeason")) |>
    select(gsis_id, all_of(stat_names))

  NoBoxScore <- PositionGroup$gsis_id[PositionGroup$position_group %in% c("OL", "LS")]
  Out <- Career |>
    left_join(FinalTeam, by = "gsis_id") |>
    left_join(FinalComplete, by = "gsis_id") |>
    left_join(rename_with(CareerSums, \(x) paste0("Coll", x), -gsis_id), by = "gsis_id") |>
    left_join(rename_with(FinalStats, \(x) paste0("CollFinal", x), -gsis_id), by = "gsis_id") |>
    mutate(BoxScorePosition = !gsis_id %in% NoBoxScore,
           AnyStats = NCollegeSeasonsWithStats > 0,
           CollegeCareerObservable = as.integer(CareerComplete & BoxScorePosition & AnyStats),
           CollegeFinalObservable = as.integer(FinalComplete & BoxScorePosition & AnyStats),
           CollegeDefCareerObservable = as.integer(CollegeCareerObservable == 1 & CareerFrom2016),
           CollegeDefFinalObservable = as.integer(CollegeFinalObservable == 1 &
                                                    FinalCollegeSeason >= 2016))

  # Apply the observability rules: observable -> absent stats are 0;
  # unobservable -> NA
  career_cols <- paste0("Coll", stat_names)
  final_cols <- paste0("CollFinal", stat_names)
  def_career <- paste0("Coll", college_stats_from_2016)
  def_final <- paste0("CollFinal", college_stats_from_2016)
  Out <- Out |>
    mutate(across(all_of(career_cols),
                  \(x) if_else(CollegeCareerObservable == 1, coalesce(x, 0), NA_real_)),
           across(all_of(final_cols),
                  \(x) if_else(CollegeFinalObservable == 1, coalesce(x, 0), NA_real_)),
           across(all_of(def_career),
                  \(x) if_else(CollegeDefCareerObservable == 1, x, NA_real_)),
           across(all_of(def_final),
                  \(x) if_else(CollegeDefFinalObservable == 1, x, NA_real_)))

  # Rates recomputed from career components (never averaged across seasons)
  Out <- Out |>
    mutate(CollCompletionPct = safe_div(CollPassComp, CollPassAtt),
           CollYardsPerAttempt = safe_div(CollPassYds, CollPassAtt),
           CollYardsPerCarry = safe_div(CollRushYds, CollRushAtt),
           CollYardsPerReception = safe_div(CollRecYds, CollRec),
           CollFGPct = safe_div(CollFGMade, CollFGAtt),
           CollYardsPerPunt = safe_div(CollPuntYds, CollPunts),
           CollFinalCompletionPct = safe_div(CollFinalPassComp, CollFinalPassAtt),
           CollFinalYardsPerAttempt = safe_div(CollFinalPassYds, CollFinalPassAtt),
           CollFinalYardsPerCarry = safe_div(CollFinalRushYds, CollFinalRushAtt),
           CollFinalYardsPerReception = safe_div(CollFinalRecYds, CollFinalRec)) |>
    select(-CareerComplete, -CareerFrom2016, -FinalComplete, -AnyStats)

  # Final college team context in THAT season: conference and classification
  # (college_teams), Power and HBCU conference flags, SP+ and SRS
  TeamContext <- tbl(con, "college_teams") |>
    select(team, season, conference, classification) |>
    collect() |>
    distinct(team, season, .keep_all = TRUE)
  Ratings <- tbl(con, "college_team_ratings") |>
    select(team, season, sp_rating, srs_rating) |>
    collect()
  Out <- Out |>
    left_join(TeamContext, by = c(FinalCollegeTeam = "team", FinalCollegeSeason = "season")) |>
    left_join(Ratings, by = c(FinalCollegeTeam = "team", FinalCollegeSeason = "season")) |>
    mutate(FinalCollegePower = power_conference(conference, FinalCollegeSeason, FinalCollegeTeam),
           FinalCollegeHBCU = if_else(is.na(conference), NA_integer_,
                                      as.integer(conference %in% c("SWAC", "MEAC")))) |>
    rename(FinalCollegeConference = conference,
           FinalCollegeClassification = classification,
           FinalCollegeSPRating = sp_rating, FinalCollegeSRS = srs_rating)

  # Final-season PPA and usage (2013+; linked ids, final pre-entry season).
  # With two rows (two ids/teams) the one with the higher overall usage is kept.
  Ppa <- tbl(con, "college_player_ppa") |>
    select(player_id, season, team, average_ppa_all, average_ppa_pass,
           average_ppa_rush, total_ppa_all) |>
    collect()
  Usage <- tbl(con, "college_player_usage") |>
    select(player_id, season, team, usage_overall, usage_pass, usage_rush) |>
    collect()
  PpaUsage <- Links |>
    inner_join(select(Career, gsis_id, FinalCollegeSeason), by = "gsis_id") |>
    inner_join(full_join(Ppa, Usage, by = c("player_id", "season", "team")),
               by = c("player_id", FinalCollegeSeason = "season")) |>
    arrange(gsis_id, desc(usage_overall), desc(abs(total_ppa_all))) |>
    distinct(gsis_id, .keep_all = TRUE) |>
    transmute(gsis_id, CollFinalPPAPerPlay = average_ppa_all,
              CollFinalPPAPerPass = average_ppa_pass,
              CollFinalPPAPerRush = average_ppa_rush,
              CollFinalPPATotal = total_ppa_all,
              CollFinalUsage = usage_overall, CollFinalUsagePass = usage_pass,
              CollFinalUsageRush = usage_rush)

  Out |>
    left_join(PpaUsage, by = "gsis_id") |>
    mutate(FinalCollegeSeason = as.integer(FinalCollegeSeason))
}

# ---------------------------------------------------------------------------
# NFL season usage (one row per gsis_id x season, REG weeks)
# ---------------------------------------------------------------------------

# Weekly roster status groups (nfl_rosters_weekly, primary rows):
#   Active:        ACT, except game-day inactives (status_description_abbr I*)
#   Inactive:      INA (2019+) or ACT with abbr I* (2002-2015, 2021+). In
#                  2016-2020 many ACT rows carry no abbr, so inactives are
#                  partly counted as Active in those seasons (era break).
#   Reserve:       RES (injured reserve and other reserve lists), PUP, NWT,
#                  RSN, RSR
#   PracticeSquad: DEV (observable essentially from 2016/2017 only)
#   Cut:           CUT
#   Other:         SUS, EXE, RET, TRC/TRD/TRT, UFA/RFA/UDF, E01/E14, missing
roster_status_group <- function(status, abbr) {
  case_when(status == "ACT" & coalesce(str_starts(abbr, "I"), FALSE) ~ "Inactive",
            status == "ACT" ~ "Active",
            status == "INA" ~ "Inactive",
            status %in% c("RES", "PUP", "NWT", "RSN", "RSR") ~ "Reserve",
            status == "DEV" ~ "PracticeSquad",
            status == "CUT" ~ "Cut",
            TRUE ~ "Other")
}

# Roster weeks by status group, primary franchise, modal position group
roster_season_usage <- function(con) {
  Weekly <- tbl(con, "nfl_rosters_weekly") |>
    filter(season_type == "REG", is_key_primary, !is.na(gsis_id), gsis_id != "") |>
    select(gsis_id, season, week, franchise_id, status,
           status_description_abbr, position_group) |>
    collect() |>
    mutate(season = as.integer(season), week = as.integer(week),
           StatusGroup = roster_status_group(status, status_description_abbr),
           GameDay = StatusGroup %in% c("Active", "Inactive"))

  Weeks <- Weekly |>
    distinct(gsis_id, season, week, StatusGroup) |>
    count(gsis_id, season, StatusGroup) |>
    pivot_wider(names_from = StatusGroup, values_from = n, values_fill = 0L,
                names_prefix = "Weeks")
  for (g in paste0("Weeks", c("Active", "Inactive", "Reserve", "PracticeSquad",
                              "Cut", "Other"))) {
    if (!g %in% names(Weeks)) Weeks[[g]] <- 0L
  }

  # Primary franchise: most game-day (Active/Inactive) weeks, then most
  # weeks in any status, then the latest week, then franchise_id
  Franchise <- Weekly |>
    group_by(gsis_id, season, franchise_id) |>
    summarise(GameDayWeeks = n_distinct(week[GameDay]), AnyWeeks = n_distinct(week),
              LastWeek = max(week), .groups = "drop") |>
    arrange(gsis_id, season, desc(GameDayWeeks), desc(AnyWeeks), desc(LastWeek),
            franchise_id) |>
    group_by(gsis_id, season) |>
    summarise(PrimaryFranchise = first(franchise_id),
              NFranchises = n(),
              NFranchisesGameDay = sum(GameDayWeeks > 0),
              .groups = "drop")

  # Modal position group: most weeks, ties to the latest week's group
  Position <- Weekly |>
    filter(!is.na(position_group)) |>
    group_by(gsis_id, season, position_group) |>
    summarise(NWeeks = n_distinct(week), LastWeek = max(week), .groups = "drop") |>
    arrange(gsis_id, season, desc(NWeeks), desc(LastWeek), position_group) |>
    distinct(gsis_id, season, .keep_all = TRUE) |>
    select(gsis_id, season, PositionGroup = position_group)

  Weekly |>
    group_by(gsis_id, season) |>
    summarise(WeeksOnRoster = n_distinct(week),
              WeeksGameDay = n_distinct(week[GameDay]), .groups = "drop") |>
    left_join(Weeks, by = c("gsis_id", "season")) |>
    left_join(Franchise, by = c("gsis_id", "season")) |>
    left_join(Position, by = c("gsis_id", "season"))
}

# Season usage: roster weeks (roster_season_usage) plus
#   GamesPlayed:        REG weeks with a box-score row (nfl_player_stats_week)
#                       or any snap (nfl_snap_counts, 2013+). Before 2013,
#                       players without box-score stats (mostly OL) are
#                       undercounted: GamesPlayedSnapBased flags 2013+.
#   GamesStartedDepth:  REG weeks listed as a starter (depth rank 1 on offense
#                       or defense) in the weekly depth chart (2002-2025),
#                       counting only weeks in which the charted franchise
#                       played a REG game; a charted starter may not have
#                       played.
#   GamesStartedSnaps:  REG games with offense or defense snap share >= 0.5
#                       (alternative definition, 2013+; NA before).
#   Snaps (2013+, NA before; 0 when the player has no snap row that season):
#     Off/Def/STSnaps totals, and Off/Def/STSnapPctMean = mean snap share
#     over the games in which the player has a snap-count row.
#   Injury weeks: InjuryReportOutWeeks = weeks with injury-report status Out
#     or Doubtful (2009+, NA before); InjuryWeeks = weeks that are either
#     Out/Doubtful on the report or on a Reserve/PUP roster status (before
#     2009 only the roster part is observed).
nfl_season_usage <- function(con) {
  Roster <- roster_season_usage(con)

  StatWeeks <- tbl(con, "nfl_player_stats_week") |>
    filter(season_type == "REG") |>
    distinct(gsis_id, season, week) |>
    collect()
  Snaps <- tbl(con, "nfl_snap_counts") |>
    filter(game_type == "REG", !is.na(gsis_id)) |>
    select(gsis_id, season, week, offense_snaps, defense_snaps, st_snaps,
           offense_pct, defense_pct, st_pct) |>
    collect()
  PlayedWeeks <- bind_rows(
    StatWeeks,
    Snaps |> filter(offense_snaps + defense_snaps + st_snaps > 0) |>
      distinct(gsis_id, season, week)) |>
    distinct() |>
    count(gsis_id, season, name = "GamesPlayed")

  SnapSeason <- Snaps |>
    group_by(gsis_id, season) |>
    summarise(OffSnaps = sum(offense_snaps, na.rm = TRUE),
              DefSnaps = sum(defense_snaps, na.rm = TRUE),
              STSnaps = sum(st_snaps, na.rm = TRUE),
              OffSnapPctMean = mean_or_na(offense_pct),
              DefSnapPctMean = mean_or_na(defense_pct),
              STSnapPctMean = mean_or_na(st_pct),
              GamesStartedSnaps = n_distinct(week[coalesce(offense_pct, 0) >= 0.5 |
                                                  coalesce(defense_pct, 0) >= 0.5]),
              .groups = "drop")

  # Depth charts also list bye weeks and one week after the REG season as
  # 'REG'; keep only weeks in which the charted franchise played a REG game
  TeamGameWeeks <- tbl(con, "nfl_team_games") |>
    filter(game_type == "REG") |>
    distinct(franchise_id, season, week)
  DepthStarts <- tbl(con, "nfl_depth_charts") |>
    filter(season_type == "REG", starter, !is.na(gsis_id)) |>
    semi_join(TeamGameWeeks, by = c("franchise_id", "season", "week")) |>
    group_by(gsis_id, season) |>
    summarise(GamesStartedDepth = n_distinct(week), .groups = "drop") |>
    collect()

  # Injury report weeks (Out/Doubtful) and reserve roster weeks
  ReportOut <- tbl(con, "nfl_injuries") |>
    filter(season_type == "REG", report_status %in% c("Out", "Doubtful")) |>
    distinct(gsis_id, season, week) |>
    collect()
  ReserveWeeks <- tbl(con, "nfl_rosters_weekly") |>
    filter(season_type == "REG", is_key_primary, !is.na(gsis_id),
           status %in% c("RES", "PUP")) |>
    distinct(gsis_id, season, week) |>
    collect()
  Injury <- bind_rows(mutate(ReportOut, Report = TRUE), ReserveWeeks) |>
    group_by(gsis_id, season, week) |>
    summarise(Report = any(coalesce(Report, FALSE)), .groups = "drop") |>
    group_by(gsis_id, season) |>
    summarise(InjuryReportOutWeeks = sum(Report), InjuryWeeks = n(), .groups = "drop")

  Roster |>
    left_join(mutate(PlayedWeeks, season = as.integer(season)), by = c("gsis_id", "season")) |>
    left_join(mutate(DepthStarts, season = as.integer(season), GamesStartedDepth = as.integer(GamesStartedDepth)),
              by = c("gsis_id", "season")) |>
    left_join(mutate(SnapSeason, season = as.integer(season)), by = c("gsis_id", "season")) |>
    left_join(mutate(Injury, season = as.integer(season)), by = c("gsis_id", "season")) |>
    mutate(GamesPlayed = coalesce(GamesPlayed, 0L),
           GamesPlayedSnapBased = as.integer(season >= 2013),
           GamesStartedDepth = coalesce(GamesStartedDepth, 0L),
           across(c(OffSnaps, DefSnaps, STSnaps),
                  \(x) if_else(season >= 2013, coalesce(x, 0), NA_real_)),
           GamesStartedSnaps = if_else(season >= 2013, coalesce(GamesStartedSnaps, 0L), NA_integer_),
           InjuryWeeks = coalesce(InjuryWeeks, 0L),
           InjuryReportOutWeeks = if_else(season >= 2009,
                                          coalesce(InjuryReportOutWeeks, 0L), NA_integer_))
}

# ---------------------------------------------------------------------------
# NFL season production (one row per gsis_id x season, REG)
# ---------------------------------------------------------------------------

# Position-appropriate box scores from nfl_player_stats_season (REG; one row
# per gsis_id x season, summed over teams in the source). A player-season
# with no stats row has NA production here; samples set counts to 0 where
# the player is known to have been on a roster (see 04). Includes EPA and
# PPR fantasy points (skill-position summary).
#
# PFR advanced defense (2018+; nfl_pfr_advstats_season stat_type 'def'):
# season totals are the sum of the player's single-team rows; the PFR
# multi-team total row ('2TM'/'3TM') is used only when no single-team row
# exists (in some seasons the total row is attached to a different gsis_id
# than the team rows, so it is not trusted over them). Rates are recomputed:
#   PfrMissedTacklePct = missed / (combined tackles + missed)
#   PfrCompPctAllowed = completions allowed / targets
#   PfrYardsPerTargetAllowed = yards allowed / targets
#
# Next Gen Stats (2016+, REG season rows, week 0): QB CPOE and time to
# throw; rusher yards over expected per attempt; receiver separation and
# YAC above expectation.
nfl_season_production <- function(con) {
  Box <- tbl(con, "nfl_player_stats_season") |>
    filter(season_type == "REG") |>
    select(gsis_id, season, games,
           completions, attempts, passing_yards, passing_tds, passing_interceptions,
           sacks_suffered, passing_epa, passing_cpoe,
           carries, rushing_yards, rushing_tds, rushing_epa,
           targets, receptions, receiving_yards, receiving_tds, receiving_epa,
           target_share,
           def_tackles_solo, def_tackles_with_assist, def_tackle_assists,
           def_tackles_for_loss, def_sacks, def_qb_hits, def_pass_defended,
           def_interceptions, def_fumbles_forced, def_tds,
           fg_made, fg_att, pat_made, pat_att, pt_att, pt_yards, pt_net_yards,
           pt_inside_20, punt_returns, kickoff_returns,
           fantasy_points_ppr) |>
    collect() |>
    transmute(gsis_id, season = as.integer(season), StatGames = as.integer(games),
              PassComp = completions, PassAtt = attempts, PassYds = passing_yards,
              PassTD = passing_tds, PassInt = passing_interceptions,
              SacksTaken = sacks_suffered, PassEPA = passing_epa,
              PassCPOE = passing_cpoe,
              RushAtt = carries, RushYds = rushing_yards, RushTD = rushing_tds,
              RushEPA = rushing_epa,
              Targets = targets, Rec = receptions, RecYds = receiving_yards,
              RecTD = receiving_tds, RecEPA = receiving_epa, TargetShare = target_share,
              SoloTackles = def_tackles_solo,
              Tackles = def_tackles_solo + def_tackles_with_assist + def_tackle_assists,
              TFL = def_tackles_for_loss, Sacks = def_sacks, QBHits = def_qb_hits,
              PassesDefended = def_pass_defended, DefInt = def_interceptions,
              ForcedFumbles = def_fumbles_forced, DefTD = def_tds,
              FGMade = fg_made, FGAtt = fg_att, XPMade = pat_made, XPAtt = pat_att,
              Punts = pt_att, PuntYds = pt_yards, PuntNetYds = pt_net_yards,
              PuntsInside20 = pt_inside_20, PuntReturns = punt_returns,
              KickReturns = kickoff_returns,
              FantasyPointsPPR = fantasy_points_ppr) |>
    mutate(CompletionPct = safe_div(PassComp, PassAtt),
           YardsPerAttempt = safe_div(PassYds, PassAtt),
           YardsPerCarry = safe_div(RushYds, RushAtt),
           YardsPerReception = safe_div(RecYds, Rec),
           FGPct = safe_div(FGMade, FGAtt),
           # the source has a few infinite target shares (zero team targets)
           TargetShare = if_else(is.finite(TargetShare), TargetShare, NA_real_))

  # Seasons in which the source does not record a stat (nflverse pbp gaps:
  # targets 2003-2008, tackles for loss 2003-2011, QB hits 2003-2005): the
  # stat is NA for every player that season, not 0 (see unrecorded_box_seasons)
  Unrecorded <- unrecorded_box_seasons(Box)
  for (i in seq_len(nrow(Unrecorded))) {
    v <- Unrecorded$Stat[i]
    Box[[v]][Box$season == Unrecorded$season[i]] <- NA
  }

  # PFR advanced defense: single-team rows summed; the multi-team total row
  # only when no single-team row exists
  PfrDef <- tbl(con, "nfl_pfr_advstats_season") |>
    filter(stat_type == "def", !is.na(gsis_id)) |>
    select(gsis_id, season, team, prss, hrry, qbkd, sk, bltz, comb, m_tkl,
           tgt, cmp, yds, td, int) |>
    collect() |>
    mutate(season = as.integer(season), TotalRow = str_detect(team, "^\\dTM$")) |>
    group_by(gsis_id, season) |>
    filter(!TotalRow | all(TotalRow)) |>
    summarise(across(c(prss, hrry, qbkd, bltz, comb, m_tkl, tgt, cmp, yds, td),
                     sum_or_na), .groups = "drop") |>
    transmute(gsis_id, season, PfrPressures = prss, PfrHurries = hrry,
              PfrQBKnockdowns = qbkd, PfrBlitzes = bltz, PfrCombTackles = comb,
              PfrMissedTackles = m_tkl,
              PfrMissedTacklePct = safe_div(m_tkl, comb + m_tkl),
              PfrTargetsAllowed = tgt, PfrCompAllowed = cmp,
              PfrYardsAllowed = yds, PfrTDAllowed = td,
              PfrCompPctAllowed = safe_div(cmp, tgt),
              PfrYardsPerTargetAllowed = safe_div(yds, tgt))

  # Next Gen Stats season rows (REG, week 0), 2016+
  ngs <- function(type, ...) {
    tbl(con, "nfl_nextgen_stats") |>
      filter(stat_type == type, season_type == "REG", week == 0, !is.na(gsis_id)) |>
      select(gsis_id, season, ...) |>
      collect() |>
      mutate(season = as.integer(season)) |>
      distinct(gsis_id, season, .keep_all = TRUE)
  }
  NgsPass <- ngs("passing", NgsCPOE = completion_percentage_above_expectation,
                 NgsTimeToThrow = avg_time_to_throw)
  NgsRush <- ngs("rushing", NgsRYOEPerAtt = rush_yards_over_expected_per_att)
  NgsRec <- ngs("receiving", NgsSeparation = avg_separation,
                NgsYACAboveExp = avg_yac_above_expectation)

  Out <- Box |>
    full_join(PfrDef, by = c("gsis_id", "season")) |>
    left_join(NgsPass, by = c("gsis_id", "season")) |>
    left_join(NgsRush, by = c("gsis_id", "season")) |>
    left_join(NgsRec, by = c("gsis_id", "season"))
  # the (Stat, season) pairs the source does not record, for samples that
  # fill absent counts with 0
  attr(Out, "unrecorded") <- Unrecorded
  Out
}

# Box-score counts that samples set to 0 when a rostered player has no
# nflverse stats row (he recorded none), unless unrecorded that season
nfl_box_counts <- c("PassComp", "PassAtt", "PassYds", "PassTD", "PassInt", "SacksTaken",
                    "RushAtt", "RushYds", "RushTD", "Targets", "Rec", "RecYds", "RecTD",
                    "SoloTackles", "Tackles", "TFL", "Sacks", "QBHits", "PassesDefended",
                    "DefInt", "ForcedFumbles", "DefTD", "FGMade", "FGAtt", "XPMade",
                    "XPAtt", "Punts", "PuntYds", "PuntNetYds", "PuntsInside20",
                    "PuntReturns", "KickReturns", "FantasyPointsPPR")

# Data-driven detection of seasons in which the source does not record a
# box-score count: a (Stat, season) is unrecorded when the number of players
# with a nonzero value is below 25% of the median over seasons. TargetShare
# follows Targets. Returns a tibble (Stat, season).
unrecorded_box_seasons <- function(Box, vars = nfl_box_counts) {
  Out <- Box |>
    group_by(season) |>
    summarise(across(all_of(vars), \(x) sum(coalesce(x, 0) != 0)), .groups = "drop") |>
    pivot_longer(-season, names_to = "Stat", values_to = "NNonzero") |>
    group_by(Stat) |>
    filter(NNonzero < 0.25 * median(NNonzero)) |>
    ungroup() |>
    select(Stat, season)
  bind_rows(Out, Out |> filter(Stat == "Targets") |> mutate(Stat = "TargetShare"))
}

# ---------------------------------------------------------------------------
# Season pay (one row per gsis_id x season with a realized cap-table year)
# ---------------------------------------------------------------------------

# Effective first season of each contract. OTC's cap table attaches a year to
# the latest contract signed on or before it, so an extension governs its
# signing season although the new years start after the contract it extends
# (e.g. Mahomes' 2020 extension: 2020 is still a rookie-deal season). Rule:
#   EffStart = year_signed, except for type Extension:
#   EffStart = max(year_signed, min(PrevEnd + 1, year_signed + 2)), where
#   PrevEnd = EffStart + years - 1 of the previous contract of the player
#   with the same franchise (ordered by year_signed, contract_seq, id).
# The two-season cap covers a rookie deal extended after year 3 (one year
# plus the fifth-year option left) and guards against chained veteran
# extensions whose OTC 'years' count the replaced years. Fifth-year option
# seasons are not in the rookie contract's 'years', so an extension signed in
# the option season governs it. Contracts with a missing year_signed or
# franchise keep EffStart = year_signed (NA if missing).
contract_effective_start <- function(Contracts) {
  C <- Contracts |>
    filter(!is.na(gsis_id), !is.na(franchise_id), !coalesce(year_signed_missing, FALSE),
           !is.na(year_signed), year_signed > 0) |>
    arrange(gsis_id, franchise_id, year_signed, contract_seq, contract_id)
  n <- nrow(C)
  EffStart <- C$year_signed
  EffEnd <- rep(NA_integer_, n)
  same_prev <- c(FALSE, C$gsis_id[-1] == C$gsis_id[-n] & C$franchise_id[-1] == C$franchise_id[-n])
  is_ext <- coalesce(C$contract_type == "Extension", FALSE)
  for (i in seq_len(n)) {
    if (is_ext[i] && same_prev[i] && !is.na(EffEnd[i - 1])) {
      EffStart[i] <- max(C$year_signed[i], min(EffEnd[i - 1] + 1L, C$year_signed[i] + 2L))
    }
    if (!is.na(C$years[i]) && C$years[i] >= 1) EffEnd[i] <- EffStart[i] + C$years[i] - 1L
  }
  tibble(contract_id = C$contract_id, gsis_id = C$gsis_id, franchise_id = C$franchise_id,
         year_signed = C$year_signed, contract_seq = C$contract_seq,
         EffStart = as.integer(EffStart))
}

# In-force contract of each gsis_id x season: OTC's attached contract, unless
# it has not yet taken effect (season < EffStart), in which case the latest
# earlier contract with the same franchise that has taken effect.
in_force_contract <- function(Governing, Contracts) {
  Eff <- contract_effective_start(Contracts)
  Otc <- Governing |>
    left_join(select(Eff, OtcContractId = contract_id, OtcEffStart = EffStart,
                     OtcFranchise = franchise_id, OtcSigned = year_signed,
                     OtcSeq = contract_seq), by = "OtcContractId")
  Pending <- Otc |> filter(!is.na(OtcEffStart), season < OtcEffStart)
  Replacement <- Pending |>
    select(gsis_id, season, OtcContractId, OtcFranchise, OtcSigned, OtcSeq) |>
    inner_join(select(Eff, gsis_id, franchise_id, contract_id, year_signed,
                      contract_seq, EffStart),
               by = c("gsis_id", OtcFranchise = "franchise_id"),
               relationship = "many-to-many") |>
    filter(year_signed < OtcSigned | (year_signed == OtcSigned & contract_seq < OtcSeq),
           EffStart <= season) |>
    arrange(gsis_id, season, desc(EffStart), desc(year_signed), desc(contract_seq)) |>
    distinct(gsis_id, season, .keep_all = TRUE) |>
    select(gsis_id, season, InForceId = contract_id)
  Otc |>
    left_join(Replacement, by = c("gsis_id", "season")) |>
    mutate(GoverningContractId = coalesce(InForceId, OtcContractId),
           ExtensionPending = as.integer(!is.na(InForceId))) |>
    select(gsis_id, season, PayFranchise, OtcContractId, GoverningContractId,
           ExtensionPending)
}

# Realized (is_projected = FALSE) OverTheCap cap-table years from
# nfl_contract_years, in millions of current dollars. A gsis_id that maps to
# two OTC ids in a year (a few players) has its rows summed; the governing
# contract is the one on the row with the larger cap number. Bonuses =
# prorated + option + roster + workout + per-game roster + other bonus.
# cap_percent is the sum of the rows' shares of the respective team's cap.
# OTC attaches each year to the latest contract signed with that franchise on
# or before the year (OtcContractId). The governing contract is the one in
# force that season (in_force_contract): an extension signed during a
# running contract governs only from its effective start, so the season of
# signing (and the option year) stays under the extended contract. It
# carries its OTC contract_type and APY. OnRookieContract = governing type
# Drafted or UDFA (NA when no governing contract is identified: left
# truncation before ~2011).
season_pay <- function(con) {
  Years <- tbl(con, "nfl_contract_years") |>
    filter(!is_projected, !is.na(gsis_id)) |>
    select(gsis_id, season = year, franchise_id, contract_id, cap_number,
           cash_paid, base_salary, prorated_bonus, option_bonus, roster_bonus,
           workout_bonus, per_game_roster_bonus, other_bonus, guaranteed_salary,
           cap_percent) |>
    collect() |>
    # Data check: cap_percent is the cap number as a share (0-1) of the
    # team's cap; a share above 1 is impossible and marks a corrupt row (two
    # rows: 2006 cap number 174.5 and 2009 2,147.5, an int-overflow value).
    # Those rows get cap_number and cap_percent set to NA.
    mutate(season = as.integer(season),
           CapNumberSuspect = coalesce(cap_percent > 1, FALSE)) |>
    mutate(cap_number = if_else(CapNumberSuspect, NA_real_, cap_number),
           cap_percent = if_else(CapNumberSuspect, NA_real_, cap_percent),
           Bonuses = rowSums(pick(prorated_bonus, option_bonus, roster_bonus,
                                  workout_bonus, per_game_roster_bonus, other_bonus),
                             na.rm = TRUE))
  Governing <- Years |>
    arrange(gsis_id, season, desc(coalesce(cap_number, 0)), desc(cash_paid)) |>
    distinct(gsis_id, season, .keep_all = TRUE) |>
    select(gsis_id, season, PayFranchise = franchise_id, OtcContractId = contract_id)
  Contracts <- tbl(con, "nfl_contracts") |>
    select(contract_id, gsis_id, franchise_id, contract_seq, contract_type, apy,
           year_signed, year_signed_missing, years, value, guaranteed) |>
    collect()
  Governing <- in_force_contract(Governing, Contracts)
  Contracts <- select(Contracts, contract_id, contract_type, apy, year_signed,
                      years, value, guaranteed)

  Years |>
    group_by(gsis_id, season) |>
    summarise(NPayRows = n(),
              CapNumberSuspect = as.integer(any(CapNumberSuspect)),
              CapNumber = sum(cap_number), CashPaid = sum(cash_paid),
              BaseSalary = sum(base_salary), Bonuses = sum(Bonuses),
              ProratedBonus = sum(prorated_bonus),
              GuaranteedSalary = sum(guaranteed_salary),
              CapPercent = sum(cap_percent), .groups = "drop") |>
    left_join(Governing, by = c("gsis_id", "season")) |>
    left_join(Contracts, by = c(GoverningContractId = "contract_id")) |>
    rename(GoverningContractType = contract_type, GoverningAPY = apy,
           GoverningYearSigned = year_signed, GoverningYears = years,
           GoverningValue = value, GoverningGuaranteed = guaranteed) |>
    mutate(GoverningContractType = na_if(GoverningContractType, ""),
           OnRookieContract = case_when(is.na(GoverningContractType) ~ NA_integer_,
                                        GoverningContractType %in% c("Drafted", "UDFA") ~ 1L,
                                        TRUE ~ 0L),
           GoverningYearSigned = as.integer(GoverningYearSigned),
           GoverningYears = as.integer(GoverningYears))
}

# ============================================================================
# 10-pay-analysis-sample.R
# Builds the pay estimation samples of notes/analysis-plan.md, section 1
# ("Samples" and "Quality blocks"):
#   - analysis_pay_contracts (key contract_id): contracts with SampleMain == 1,
#     InNflPlayers == 1, GsisLinkSuspect != 1, a non-missing OTC position and
#     year_signed 2014-2026; BargainedMarket (UFA + extension) marks the main
#     sample, VeteranMarket (adds tags and tenders) the robustness sample
#   - analysis_pay_player_season (key gsis_id, season): player-seasons with
#     HasPay == 1, seasons 2014-2025, InNflPlayers == 1 and Experience 0-15
#   - analysis/pay_control_blocks.csv: control dictionary (one row per
#     control: sample, block A-F, variable, slope_groups, label). Script 12
#     builds the position-group interactions from it.
# Quality blocks (all predetermined at signing / before season t):
#   A career stage, B prior-season (t-1) production, C career production,
#   D pre-NFL signals, E usage (coach-chosen: snaps, games started and career
#   depth-chart starts), F scout grade. Games played (B, C) are also partly
#   driven by playing time. Controls with
#   incomplete coverage are zero-filled with fill_missing() and get a
#   <var>Miss indicator, so no observation is dropped for a missing control.
# Race: the full person race vector from load_person_race() is attached
# (hand codes, Wikipedia flags, BIFSG, the predicted race p_*_pred with the
# preddoc sensitivity variant and documented race, and the prior covariates
# pred_* that regressions under a predicted measure include as FE), plus the
# race_predicted columns load_person_race() does not yet return (county
# availability, the preddoc prior's extra covariates, and the raked,
# draft-free and Black-or-multiracial posteriors), read directly; the race
# regressors are NOT computed here (script 12 uses person_race_regressors(),
# which depends on the race measure).
# Block D includes the RAS-style athletic score and its components.
# Inputs: analysis/contracts.parquet (05), analysis/player_season.parquet
# (04), their codebooks, load_person_race() (DuckDB + data/hand_coded) and
# DuckDB race_predicted (read-only).
# Outputs: analysis/analysis_pay_contracts.{parquet,csv},
# analysis/analysis_pay_player_season.{parquet,csv}, their codebooks, and
# analysis/pay_control_blocks.csv.
# Date: 2026-10-02
# ============================================================================

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

ContractsRaw <- read_parquet(file.path(analysis, "contracts.parquet"))
PlayerSeasonRaw <- read_parquet(file.path(analysis, "player_season.parquet"))

# Input codebook labels, reused for columns passed through unchanged
read_labels <- function(name) {
  read_csv(file.path(analysis, paste0("codebook_", name, ".csv")),
           show_col_types = FALSE) |>
    filter(!is.na(label)) |>
    select(variable, label) |>
    deframe()
}
ContractInputLabels <- read_labels("contracts")
PanelInputLabels <- read_labels("player_season")

# Race vector of every player (hand codes, provisional flag, Wikipedia
# categories, BIFSG, predicted race and its prior covariates); one row per
# gsis_id
race_cols_hand <- c("race", "hispanic", "black_any", "nonwhite", "race_source",
                    "black_provisional", "black_provisional_source", "wiki_cat_black",
                    "wiki_cat_hispanic_latino", "wiki_cat_asian",
                    "wiki_cat_pacific_islander", "wiki_cat_native_american",
                    "p_black_bifsg")
# Predicted race (primary measure, model-only), the documented sensitivity
# variant (preddoc) and the documented race (validation only)
race_cols_pred <- c("p_white_pred", "p_black_pred", "p_hispanic_pred", "p_api_pred",
                    "p_aian_pred", "p_multi_pred", "p_black_any_pred", "prior_black_pred",
                    "pred_method", "p_white_preddoc", "p_black_any_preddoc",
                    "p_hispanic_preddoc", "p_api_preddoc", "p_aian_preddoc",
                    "p_multi_preddoc", "pred_method_preddoc", "documented_race",
                    "documented_black_any")
# Predetermined covariates of the predicted-race prior (FE under a predicted
# measure; race_prior_controls(measure, "player"))
race_prior_cols <- c("pred_pos_group", "pred_rookie_era", "pred_draft_bucket",
                     "pred_college_type")
# race_predicted columns that load_person_race() does not (yet) return, read
# directly (read-only) from the same table: county availability (a covariate
# of the primary player prior), the preddoc prior's extra covariates (article
# found, career length), and the raked, draft-free and Black-or-multiracial
# variants of the posterior. Attaching them here keeps every race column of
# the sample from one 04e fit
race_extra_map <- c(pred_county_available = "county_available",
                    pred_has_wiki = "has_wiki", pred_career_bucket = "career_bucket",
                    p_white_pred_raked = "p_white_pred_raked",
                    p_black_pred_raked = "p_black_pred_raked",
                    p_white_pred_nodraft = "p_white_pred_nodraft",
                    p_black_pred_nodraft = "p_black_pred_nodraft",
                    p_black_or_multi_pred = "p_black_or_multi_pred")
race_extra_prior <- c("pred_county_available", "pred_has_wiki", "pred_career_bucket")
race_cols_load <- c(race_cols_hand, race_cols_pred, race_prior_cols)
con <- db_connect()
PlayerRace <- load_person_race(con, hand_coded) |>
  filter(entity == "player") |>
  select(gsis_id = person_id, all_of(race_cols_load))
PredExtra <- tbl(con, "race_predicted") |>
  filter(entity == "player") |>
  select(person_uid, all_of(unname(race_extra_map))) |>
  collect() |>
  transmute(gsis_id = sub("^player:", "", person_uid),
            !!!rlang::syms(race_extra_map))
db_disconnect(con)
stopifnot(!anyDuplicated(PredExtra$gsis_id))
race_cols <- c(race_cols_load, names(race_extra_map))
PlayerRace <- PlayerRace |>
  left_join(PredExtra, by = "gsis_id", relationship = "one-to-one") |>
  mutate(HasWikiArticle = as.integer(!is.na(wiki_cat_black)),
         # Prior covariates as character FE levels; NA becomes "unknown"
         across(all_of(c(race_prior_cols, race_extra_prior)),
                \(x) coalesce(as.character(x), "unknown")))
stopifnot(!anyDuplicated(PlayerRace$gsis_id))
message("Players without a predicted P(Black any): ",
        sum(is.na(PlayerRace$p_black_any_pred)), " of ", nrow(PlayerRace))

RaceLabels <- c(
  race = "Hand-coded race (load_person_race; NA until coded)",
  hispanic = "Hand-coded Hispanic (yes/no/unknown; NA until coded)",
  black_any = "Hand-coded Black alone or in combination (0/1; NA until coded)",
  nonwhite = "Hand-coded nonwhite (0/1; NA until coded or white with Hispanic unknown)",
  race_source = "Source of the hand code (coder_agree, single_coder, disputed, adjudicated)",
  black_provisional = "Hand-coded black_any when coded, else 1 if a Wikipedia category flags Black, else NA (positive-only lower bound)",
  black_provisional_source = "Source of black_provisional (hand-code source or wiki_category)",
  wiki_cat_black = "Wikipedia category flags the player as Black (NA without an article; 0 is not evidence of race)",
  wiki_cat_hispanic_latino = "Wikipedia category flags the player as Hispanic/Latino (NA without an article)",
  wiki_cat_asian = "Wikipedia category flags the player as Asian (NA without an article)",
  wiki_cat_pacific_islander = "Wikipedia category flags the player as Pacific Islander (NA without an article)",
  wiki_cat_native_american = "Wikipedia category flags the player as Native American (NA without an article)",
  p_black_bifsg = "BIFSG posterior P(Black) (name-based; descriptive only)",
  p_white_pred = "Predicted P(white, non-Hispanic) (race_predicted, model-only: BIFSG name/county likelihood x NFL prior estimated by EM on predetermined covariates)",
  p_black_pred = "Predicted P(Black alone, non-Hispanic) (race_predicted, model-only)",
  p_hispanic_pred = "Predicted P(Hispanic) (race_predicted, model-only)",
  p_api_pred = "Predicted P(Asian or Pacific Islander) (race_predicted, model-only)",
  p_aian_pred = "Predicted P(American Indian or Alaska Native) (race_predicted, model-only)",
  p_multi_pred = "Predicted P(multiracial) (race_predicted, model-only)",
  p_black_any_pred = "Predicted P(non-Hispanic Black alone) (equals p_black_pred despite the name; race_predicted, model-only; Black Hispanic and multiracial Black persons are in the Hispanic and multiracial categories); primary race regressor (regression calibration; control for the pred_* covariates)",
  prior_black_pred = "NFL prior P(Black any) from the EM model on the predetermined covariates (before the name/county likelihood)",
  pred_method = "Method of the model-only prediction (race_predicted.pred_method)",
  p_white_preddoc = "Sensitivity variant (preddoc): P(white); one-hot where a public source documents race, else the model prediction (fame-dependent)",
  p_black_any_preddoc = "Sensitivity variant (preddoc): 1/0 for documented Black alone or in combination where a public source documents race; for undocumented persons the model P(non-Hispanic Black alone) (fame-dependent)",
  p_hispanic_preddoc = "Sensitivity variant (preddoc): P(Hispanic); one-hot where documented, else the model prediction",
  p_api_preddoc = "Sensitivity variant (preddoc): P(Asian or Pacific Islander); one-hot where documented, else the model prediction",
  p_aian_preddoc = "Sensitivity variant (preddoc): P(American Indian or Alaska Native); one-hot where documented, else the model prediction",
  p_multi_preddoc = "Sensitivity variant (preddoc): P(multiracial); one-hot where documented, else the model prediction",
  pred_method_preddoc = "Method of the preddoc value (documented one-hot or model prediction)",
  documented_race = "Race stated by a public source (race_predicted; validation only, never a regressor; NA if undocumented)",
  documented_black_any = "1 if a public source documents the player as Black alone or in combination, 0 if documented otherwise, NA if undocumented (validation only)",
  pred_pos_group = "Prior covariate of the predicted race: position at NFL entry (fine position, 19 levels; K/P/LS pooled as ST) (character FE; NA recoded to \"unknown\")",
  pred_rookie_era = "Prior covariate of the predicted race: rookie-season era (character FE; NA recoded to \"unknown\")",
  pred_draft_bucket = "Prior covariate of the predicted race: draft-round bucket (character FE; NA recoded to \"unknown\")",
  pred_college_type = "Prior covariate of the predicted race: college type (character FE; NA recoded to \"unknown\")",
  pred_county_available = "Prior covariate of the predicted race: home-county likelihood available (yes/no) (race_predicted.county_available; character FE; NA recoded to \"unknown\")",
  pred_has_wiki = "Prior covariate of the preddoc variant only: Wikipedia article found (race_predicted.has_wiki; character FE; NA recoded to \"unknown\"); post-treatment (fame)",
  pred_career_bucket = "Prior covariate of the preddoc variant only: career-length bucket (race_predicted.career_bucket; character FE; NA recoded to \"unknown\"); post-treatment (career length is partly an outcome)",
  p_white_pred_raked = "Predicted P(white), TIDES-raked variant: Black log-odds shifted so the mean matches published TIDES player shares (race_predicted)",
  p_black_pred_raked = "Predicted P(non-Hispanic Black alone), TIDES-raked variant (race_predicted)",
  p_white_pred_nodraft = "Predicted P(white), draft-free variant: prior without the draft-round bucket (race_predicted)",
  p_black_pred_nodraft = "Predicted P(non-Hispanic Black alone), draft-free variant: prior without the draft-round bucket (race_predicted)",
  p_black_or_multi_pred = "Predicted P(non-Hispanic Black alone) + P(multiracial) (race_predicted, model-only)",
  HasWikiArticle = "1 if the player has a Wikipedia article with category signals (wiki_cat_black not NA)"
)

# Position-relevant production (block B; block C uses the same variables as
# career sums). slope_groups: position groups whose slope each variable gets.
ProductionSlopes <- tribble(
  ~stem,                    ~slope_groups,
  "PassAtt",                "QB",
  "PassYds",                "QB",
  "PassTD",                 "QB",
  "PassInt",                "QB",
  "PassEPA",                "QB",
  "SacksTaken",             "QB",
  "RushAtt",                "RB",
  "RushYds",                "QB;RB",
  "RushTD",                 "RB",
  "Targets",                "WR;TE",
  "Rec",                    "RB;WR;TE",
  "RecYds",                 "RB;WR;TE",
  "RecTD",                  "RB;WR;TE",
  "RecEPA",                 "WR;TE",
  "Tackles",                "DL;LB;DB",
  "Sacks",                  "DL;LB",
  "QBHits",                 "DL;LB",
  "TFL",                    "DL;LB",
  "ForcedFumbles",          "DL;LB",
  "PfrPressures",           "DL;LB",
  "PfrMissedTacklePct",     "DL;LB",
  "DefInt",                 "DB",
  "PassesDefended",         "DB",
  "PfrTargetsAllowed",      "DB",
  "PfrYardsPerTargetAllowed", "DB",
  "FGMade",                 "K",
  "FGAtt",                  "K",
  "XPMade",                 "K",
  "Punts",                  "P",
  "PuntNetYds",             "P"
)
# Ratio and PFR (2018+) measures are not summed over a career
CareerStems <- setdiff(ProductionSlopes$stem,
                       c("PfrPressures", "PfrMissedTacklePct", "PfrTargetsAllowed",
                         "PfrYardsPerTargetAllowed"))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Career-to-date sums: for each row of `keys` (gsis_id, RefYear), sums over the
# player's player_season panel seasons strictly before RefYear (2002-2025
# REG roster seasons). Counts that are NA in a season (Targets before 2009,
# TFL before 2012, QBHits before 2006; EPA without a stats row) contribute
# nothing to the sum, so career sums of those measures undercount for
# players with early seasons. Snap sums use 2013+ seasons only. A sum is NA
# when the player has no earlier panel season (snap sums: no earlier 2013+
# season; EPA sums: no earlier season with the measure).
career_sums <- function(keys, panel) {
  keys |>
    distinct(gsis_id, RefYear) |>
    inner_join(panel |>
                 mutate(OffDefSnaps = if_else(season >= 2013, OffSnaps + DefSnaps, NA_real_),
                        STSnapsPost = if_else(season >= 2013, STSnaps, NA_real_)) |>
                 select(gsis_id, season, GamesPlayed, GamesStartedDepth, InjuryWeeks,
                        OffDefSnaps, STSnapsPost, all_of(CareerStems)),
               by = "gsis_id", relationship = "many-to-many") |>
    filter(season < RefYear) |>
    group_by(gsis_id, RefYear) |>
    summarise(CareerPanelSeasonsCalc = n(),
              CareerGames = sum_or_na(GamesPlayed),
              CareerStartsDepth = sum_or_na(GamesStartedDepth),
              CareerInjuryWeeks = sum_or_na(InjuryWeeks),
              CareerOffDefSnaps = sum_or_na(OffDefSnaps),
              CareerSTSnaps = sum_or_na(STSnapsPost),
              across(all_of(CareerStems), sum_or_na, .names = "Career{.col}"),
              .groups = "drop")
}

# sum_or_na() as in programs/00-player-functions.R (not sourced here)
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Zero-fill `vars` with fill_missing(), then drop each <var>Miss indicator
# that is constant or identical to the `base` indicator column (e.g.
# NoPriorSeason): those carry no information beyond the base indicator.
# Returns the data; the names of the kept indicators are in attr "miss_kept".
fill_with_base <- function(df, vars, base = NULL) {
  df <- fill_missing(df, vars)
  kept <- character()
  for (v in vars) {
    m <- paste0(v, "Miss")
    redundant <- n_distinct(df[[m]]) == 1 ||
      (!is.null(base) && identical(df[[m]], as.integer(df[[base]])))
    if (redundant) df[[m]] <- NULL else kept <- c(kept, m)
  }
  attr(df, "miss_kept") <- kept
  df
}

# Experience bins of the CBA minimum schedule (as ExperienceBin in 05)
experience_bin <- function(x) {
  as.character(cut(x, breaks = c(-Inf, 0, 1, 2, 3, 6, Inf),
                   labels = c("0", "1", "2", "3", "4-6", "7+")))
}

# Report the row count after a restriction
report_n <- function(df, step) {
  message(glue("  {step}: {format(nrow(df), big.mark = ',')} rows"))
  df
}

# ---------------------------------------------------------------------------
# Contracts: restrictions (analysis-plan.md section 1)
# ---------------------------------------------------------------------------

message("analysis_pay_contracts restrictions:")
PayContracts <- ContractsRaw |>
  report_n("all contracts") |>
  filter(SampleMain == 1) |>
  report_n("SampleMain == 1") |>
  filter(InNflPlayers == 1) |>
  report_n("InNflPlayers == 1") |>
  filter(coalesce(GsisLinkSuspect, 0L) != 1) |>
  report_n("GsisLinkSuspect != 1") |>
  filter(!is.na(position), position != "") |>
  report_n("non-missing OTC position") |>
  filter(between(year_signed, 2014L, 2026L)) |>
  report_n("year_signed 2014-2026")

# Position group for slopes: the OTC position group of the contract
# (ED/IDL -> DL, CB/S -> DB, FB -> RB, C/LG/LT/RG/RT -> OL)
PositionMap <- count(PayContracts, position, position_group)
print(PositionMap, n = Inf)
stopifnot(!anyDuplicated(PositionMap$position),
          setequal(PositionMap$position_group,
                   c("QB", "RB", "WR", "TE", "OL", "DL", "LB", "DB", "K", "P", "LS")))

# Season-(year_signed - 1) measures missing from contracts (sacks taken,
# extra points), from player_season with the same timing as the Prior* columns
PriorExtra <- PlayerSeasonRaw |>
  transmute(gsis_id, year_signed = season + 1L, PriorSacksTaken = SacksTaken,
            PriorXPMade = XPMade)

# Career sums before year_signed from player_season (same definition as the
# Career* columns of 05); used for the block B variables 05 does not sum
ContractCareer <- career_sums(transmute(PayContracts, gsis_id, RefYear = year_signed),
                              PlayerSeasonRaw)
CareerFrom05 <- setdiff(intersect(names(ContractCareer), names(PayContracts)), "gsis_id")
CareerCheck <- PayContracts |>
  select(gsis_id, RefYear = year_signed, all_of(CareerFrom05)) |>
  left_join(ContractCareer |> select(gsis_id, RefYear, all_of(CareerFrom05)),
            by = c("gsis_id", "RefYear"), suffix = c("", ".calc"))
CareerMismatch <- map_int(CareerFrom05, \(v) {
  sum(abs(coalesce(CareerCheck[[v]], -1) - coalesce(CareerCheck[[paste0(v, ".calc")]], -1)) > 1e-6)
}) |> set_names(CareerFrom05)
message("Career sums recomputed from player_season vs the contracts' Career* columns ",
        "(rows that differ): ", paste(names(CareerMismatch), CareerMismatch,
                                      sep = "=", collapse = ", "))
CareerNew <- setdiff(names(ContractCareer), c(names(PayContracts), "gsis_id", "RefYear",
                                               "CareerPanelSeasonsCalc"))

# Race vector: drop the contracts' own copies, attach load_person_race()
PayContracts <- PayContracts |>
  select(-any_of(c(race_cols, "p_white_bifsg", "p_hispanic_bifsg", "race_bifsg",
                   "HasWikiArticle"))) |>
  left_join(PlayerRace, by = "gsis_id", relationship = "many-to-one") |>
  left_join(PriorExtra, by = c("gsis_id", "year_signed"), relationship = "many-to-one") |>
  left_join(ContractCareer |> select(gsis_id, RefYear, all_of(CareerNew)),
            by = c("gsis_id", "year_signed" = "RefYear"), relationship = "many-to-one")

# ---------------------------------------------------------------------------
# Contracts: outcomes, flags and quality blocks
# ---------------------------------------------------------------------------

# Control dictionary rows are collected here: one row per control variable
Dictionary <- list()
add_controls <- function(sample, block, vars, slope_groups, labels) {
  Dictionary[[length(Dictionary) + 1]] <<- tibble(
    sample = sample, block = block, variable = vars,
    slope_groups = rep_len(slope_groups, length(vars)),
    label = unname(labels[vars]))
}
# Indicator labels for fill_missing() columns
miss_labels <- function(miss) {
  setNames(paste0("1 if ", sub("Miss$", "", miss),
                  " was missing (the variable is set to 0 then)"), miss)
}
fill_note <- "; NA set to 0 by fill_missing() (see the Miss indicator or the block's base indicator)"

# Outcomes, sample flags and position group
PayContracts <- PayContracts |>
  mutate(PositionGroup = position_group,
         LogYears = log(years),
         NonRookieSample = if_else(is.na(ContractType), NA_integer_,
                                   as.integer(!ContractType %in% c("Drafted", "UDFA"))),
         UFAOnly = as.integer(ContractType == "UFA"),
         ResignExtension = as.integer(ContractType == "Extension"),
         # Freely bargained veteran pay: UFA and extension contracts (tags and
         # tenders are priced by a CBA formula; ERFA tenders are minimum deals)
         BargainedMarket = if_else(is.na(ContractType), NA_integer_,
                                   as.integer(ContractType %in% c("UFA", "Extension"))),
         LogValue = if_else(value > 0, log(value), NA_real_),
         # Block A: career stage
         AgeSq = AgeAtSigning^2,
         # Block B base indicator
         NoPriorSeason = 1L - InPanelPriorSeason,
         # Block C base indicator
         NoCareerSeasons = as.integer(CareerPanelSeasons == 0),
         # Block D: draft slot (undrafted: LogDraftPick = 0, absorbed by Undrafted)
         LogDraftPick = if_else(Undrafted == 1L, 0, log(DraftPick)),
         DraftRound = if_else(Undrafted == 1L, 0L, DraftRound))

# Variable lists by block
prior_prod <- paste0("Prior", ProductionSlopes$stem)
career_prod <- paste0("Career", CareerStems)
blockA_c <- c("AgeAtSigning", "AgeSq")
blockB_all <- c("PriorGamesPlayed", "PriorInjuryWeeks")
# Career depth-chart starts are coach-chosen playing time, so they enter with
# the usage block E (plan, "Bad controls"), not with career production C
blockC_all <- c("CareerGames", "CareerInjuryWeeks")
# RAS-style athletic score and its four components (combine_athletic_scores()
# in 00-player-functions.R; NA without enough combine measurables)
blockD_athletic <- c("AthleticScore", "AthleticSizeScore", "AthleticSpeedScore",
                     "AthleticExplosionScore", "AthleticAgilityScore")
blockD_slope <- c("RecruitRating", "RecruitStars", "Forty", "Vertical", "Bench",
                  "BroadJump", "Cone", "Shuttle", "CombineHeight", "CombineWeight",
                  blockD_athletic)
blockD_lin <- c("FinalCollegePower", "FinalCollegeSRS", "FinalCollegeHBCU")
blockE_prior <- c("PriorOffSnaps", "PriorDefSnaps", "PriorSTSnaps", "PriorGamesStartedSnaps")
blockE_career <- c("CareerStartsDepth", "CareerOffDefSnaps", "CareerSTSnaps")

# Zero-fill each block; drop indicators that duplicate the block's base
# indicator (Prior* are NA exactly when NoPriorSeason == 1; Career* when
# NoCareerSeasons == 1)
PayContracts <- fill_with_base(PayContracts, blockA_c)
MissA <- attr(PayContracts, "miss_kept")
PayContracts <- fill_with_base(PayContracts, c(blockB_all, prior_prod), base = "NoPriorSeason")
MissB <- attr(PayContracts, "miss_kept")
PayContracts <- fill_with_base(PayContracts, c(blockC_all, career_prod), base = "NoCareerSeasons")
MissC <- attr(PayContracts, "miss_kept")
PayContracts <- fill_with_base(PayContracts, c("LogDraftPick", blockD_slope, blockD_lin))
MissD <- attr(PayContracts, "miss_kept")
PayContracts <- fill_with_base(PayContracts, blockE_prior, base = "NoPriorSeason")
MissE <- attr(PayContracts, "miss_kept")
PayContracts <- fill_with_base(PayContracts, blockE_career, base = "NoCareerSeasons")
MissE <- c(MissE, attr(PayContracts, "miss_kept"))
PayContracts <- fill_with_base(PayContracts, "PreDraftGrade")
MissF <- attr(PayContracts, "miss_kept")
message("contracts: Miss indicators kept: ",
        paste(c(MissA, MissB, MissC, MissD, MissE, MissF), collapse = ", "))

# Labels: input codebook labels for pass-through columns, built labels for
# the added Prior/Career columns, and explicit labels for constructed ones
ContractLabels <- c(
  ContractInputLabels,
  PriorSacksTaken = paste0("Season year_signed - 1 (player_season): ", PanelInputLabels[["SacksTaken"]]),
  PriorXPMade = paste0("Season year_signed - 1 (player_season): ", PanelInputLabels[["XPMade"]]),
  setNames(paste0("Sum over panel seasons < year_signed (NA if none) of: ",
                  PanelInputLabels[sub("^Career", "", CareerNew)]), CareerNew),
  RaceLabels,
  PositionGroup = "Position group for slopes: OTC position group of the contract (QB, RB incl. FB, WR, TE, OL = C/LG/LT/RG/RT, DL = ED/IDL, LB, DB = CB/S, K, P, LS)",
  LogYears = "log(years) (contract length)",
  NonRookieSample = "1 if ContractType is not Drafted or UDFA (veteran market + Other/SFA/Practice; minimum-salary margin); NA if ContractType is NA",
  UFAOnly = "1 if ContractType is UFA (veteran free agent); NA if ContractType is NA",
  ResignExtension = "1 if ContractType is Extension (extension with the current team; UFAs who re-sign with their own team are UFAOnly == 1 with UFAResign == 1); NA if ContractType is NA",
  BargainedMarket = "1 if ContractType is UFA or Extension (freely bargained veteran pay; main sample of equation (1)); 0 for other types incl. tags and tenders; NA if ContractType is NA",
  LogValue = "log(value) (total contract value in millions of dollars; NA if 0 or NA)",
  AgeSq = "AgeAtSigning squared",
  NoPriorSeason = "1 - InPanelPriorSeason: not on a REG roster in year_signed - 1 (Prior* measures set to 0)",
  NoCareerSeasons = "1 if CareerPanelSeasons == 0: no 2002-2025 panel season before year_signed (Career* sums set to 0)",
  LogDraftPick = "log(DraftPick); 0 for undrafted players (Undrafted absorbs their level)",
  DraftRound = "NFL draft round; 0 = undrafted (for draft-round FE)"
)
# Filled controls carry a note; indicators get their own label
filled_c <- c(blockA_c, blockB_all, prior_prod, blockC_all, career_prod, "LogDraftPick",
              blockD_slope, blockD_lin, blockE_prior, blockE_career, "PreDraftGrade")
ContractLabels[filled_c] <- paste0(ContractLabels[filled_c], fill_note)
ContractLabels <- c(ContractLabels, miss_labels(c(MissA, MissB, MissC, MissD, MissE, MissF)))
ContractLabels <- ContractLabels[!duplicated(names(ContractLabels), fromLast = TRUE)]

# Control dictionary: contracts
slope_of <- \(stems) ProductionSlopes$slope_groups[match(stems, ProductionSlopes$stem)]
miss_of <- \(vars, miss) intersect(paste0(vars, "Miss"), miss)
add_controls("contracts", "A", c(blockA_c, "ExperienceLeftCensored", miss_of(blockA_c, MissA)),
             "none", ContractLabels)
add_controls("contracts", "B", c(blockB_all), "all", ContractLabels)
add_controls("contracts", "B", "NoPriorSeason", "none", ContractLabels)
add_controls("contracts", "B", prior_prod, slope_of(ProductionSlopes$stem), ContractLabels)
add_controls("contracts", "B", miss_of(prior_prod, MissB),
             slope_of(sub("^Prior(.*)Miss$", "\\1", miss_of(prior_prod, MissB))), ContractLabels)
add_controls("contracts", "C", blockC_all, "all", ContractLabels)
add_controls("contracts", "C", "NoCareerSeasons", "none", ContractLabels)
add_controls("contracts", "C", career_prod, slope_of(CareerStems), ContractLabels)
add_controls("contracts", "C", miss_of(career_prod, MissC),
             slope_of(sub("^Career(.*)Miss$", "\\1", miss_of(career_prod, MissC))), ContractLabels)
add_controls("contracts", "D", c("LogDraftPick", "Undrafted", miss_of("LogDraftPick", MissD)),
             "none", ContractLabels)
add_controls("contracts", "D", c(blockD_slope, miss_of(blockD_slope, MissD)), "all", ContractLabels)
add_controls("contracts", "D", c(blockD_lin, miss_of(blockD_lin, MissD)), "none", ContractLabels)
add_controls("contracts", "E", c(blockE_prior, blockE_career,
                                 miss_of(c(blockE_prior, blockE_career), MissE)),
             "all", ContractLabels)
add_controls("contracts", "F", c("PreDraftGrade", miss_of("PreDraftGrade", MissF)), "none",
             ContractLabels)

# Keep the identifiers, FE columns, flags, outcomes, controls and race vector
ContractControls <- bind_rows(Dictionary)$variable
PayContracts <- PayContracts |>
  select(contract_id, gsis_id, otc_id, player, year_signed, position, PositionGroup,
         SigningFranchise, franchise_id, ContractType, MarketMargin,
         VeteranMarket, BargainedMarket, NonRookieSample, UFAOnly, ResignExtension, NewTeam,
         UFAResign, LogAPYCapPct, LogAPY, LogValue, GuaranteeShare, GuaranteedZero, years, LogYears,
         NearMinimum, apy, apy_cap_pct, guaranteed, value,
         ExperienceAtSigning, ExperienceBin, RookieSeason, DraftRound, DraftPick,
         InPanelPriorSeason, CareerPanelSeasons, CombineInvite, AthleticScoreN, HasRecruit,
         HasCollegeLink, all_of(ContractControls), all_of(race_cols), HasWikiArticle) |>
  arrange(year_signed, contract_id)

# ---------------------------------------------------------------------------
# Player-season: lags and career sums on the full panel, then restrictions
# ---------------------------------------------------------------------------

# One-season lags 04 does not build, with 04's rule (the previous row must be
# the same player in season - 1; NA otherwise)
extra_lag_vars <- c("SacksTaken", "XPMade", "Punts", "PuntNetYds", "PfrTargetsAllowed",
                    "GamesStartedSnaps", "Sacks")
PanelLags <- PlayerSeasonRaw |>
  select(gsis_id, season, all_of(extra_lag_vars), LagSacks04 = LagSacks) |>
  arrange(gsis_id, season) |>
  mutate(PrevIsTminus1 = coalesce(lag(gsis_id) == gsis_id & lag(season) == season - 1L, FALSE),
         across(all_of(extra_lag_vars), \(x) if_else(PrevIsTminus1, lag(x), x[NA_integer_]),
                .names = "Lag{.col}"))
# Check: the rebuilt LagSacks reproduces 04's
stopifnot(identical(PanelLags$LagSacks, PanelLags$LagSacks04))
PanelLags <- select(PanelLags, gsis_id, season,
                    all_of(paste0("Lag", setdiff(extra_lag_vars, "Sacks"))))

message("analysis_pay_player_season restrictions:")
PayPanel <- PlayerSeasonRaw |>
  report_n("all player-seasons") |>
  filter(HasPay == 1) |>
  report_n("HasPay == 1") |>
  filter(between(season, 2014L, 2025L)) |>
  report_n("seasons 2014-2025") |>
  filter(InNflPlayers == 1) |>
  report_n("InNflPlayers == 1") |>
  filter(between(Experience, 0L, 15L)) |>
  report_n("Experience 0-15")

# Career-to-date sums over the player's panel seasons strictly before season t
PanelCareer <- career_sums(transmute(PayPanel, gsis_id, RefYear = season), PlayerSeasonRaw)

PayPanel <- PayPanel |>
  select(-any_of(c(race_cols, "p_white_bifsg", "p_hispanic_bifsg", "race_bifsg",
                   "HasWikiArticle", names(PanelCareer)[startsWith(names(PanelCareer), "Career")]))) |>
  left_join(PlayerRace, by = "gsis_id", relationship = "many-to-one") |>
  left_join(PanelLags, by = c("gsis_id", "season"), relationship = "one-to-one") |>
  left_join(select(PanelCareer, -CareerPanelSeasonsCalc),
            by = c("gsis_id", "season" = "RefYear"), relationship = "one-to-one") |>
  mutate(
    # Outcomes (log of 0 or negative pay is NA). LogCapNumber is the main
    # outcome: OTC rounds CapPercent to 0.001 of the league cap, so every cap
    # hit below about 0.05% of the cap has CapPercent = 0 and LogCapPct would
    # truncate low-pay seasons; position group x season FE absorb the cap
    LogCapNumber = if_else(CapNumber > 0, log(CapNumber), NA_real_),
    LogCapPct = if_else(CapPercent > 0, log(CapPercent), NA_real_),
    LogCashPaid = if_else(CashPaid > 0, log(CashPaid), NA_real_),
    LogGoverningAPY = if_else(GoverningAPY > 0, log(GoverningAPY), NA_real_),
    # Flags
    BargainedPay = as.integer(OnRookieContract == 0),
    # Freely bargained veteran pay: governing contract is a UFA or extension deal
    VeteranBargainedPay = if_else(is.na(OnRookieContract), NA_integer_,
                                  as.integer(GoverningContractType %in% c("UFA", "Extension"))),
    # Block A
    ExperienceBin = experience_bin(Experience),
    AgeSq = Age^2,
    # Base indicators of blocks B and C
    NoPriorSeason = 1L - InSampleTminus1,
    NoCareerSeasons = as.integer(is.na(CareerGames)),
    CareerLeftCensored = as.integer(RookieSeason < 2002),
    # Block D
    LogDraftPick = if_else(Undrafted == 1L, 0, log(DraftPick)),
    DraftRound = if_else(Undrafted == 1L, 0L, DraftRound))
stopifnot(nrow(PayPanel) == nrow(distinct(PayPanel, gsis_id, season)))

# ---------------------------------------------------------------------------
# Player-season: quality blocks
# ---------------------------------------------------------------------------

lag_prod <- paste0("Lag", ProductionSlopes$stem)
blockA_p <- c("Age", "AgeSq")
blockB_all_p <- c("LagGamesPlayed", "LagInjuryWeeks")
blockE_lag <- c("LagOffSnaps", "LagDefSnaps", "LagSTSnaps", "LagGamesStartedSnaps")
stopifnot(all(c(lag_prod, blockB_all_p, blockE_lag) %in% names(PayPanel)))

PayPanel <- fill_with_base(PayPanel, blockA_p)
PMissA <- attr(PayPanel, "miss_kept")
PayPanel <- fill_with_base(PayPanel, c(blockB_all_p, lag_prod), base = "NoPriorSeason")
PMissB <- attr(PayPanel, "miss_kept")
PayPanel <- fill_with_base(PayPanel, c(blockC_all, career_prod), base = "NoCareerSeasons")
PMissC <- attr(PayPanel, "miss_kept")
PayPanel <- fill_with_base(PayPanel, c("LogDraftPick", blockD_slope, blockD_lin))
PMissD <- attr(PayPanel, "miss_kept")
PayPanel <- fill_with_base(PayPanel, blockE_lag, base = "NoPriorSeason")
PMissE <- attr(PayPanel, "miss_kept")
PayPanel <- fill_with_base(PayPanel, blockE_career, base = "NoCareerSeasons")
PMissE <- c(PMissE, attr(PayPanel, "miss_kept"))
PayPanel <- fill_with_base(PayPanel, "PreDraftGrade")
PMissF <- attr(PayPanel, "miss_kept")
message("player_season: Miss indicators kept: ",
        paste(c(PMissA, PMissB, PMissC, PMissD, PMissE, PMissF), collapse = ", "))

# Labels
lag_new <- paste0("Lag", setdiff(extra_lag_vars, "Sacks"))
career_all <- c(blockC_all, career_prod, blockE_career)
PanelLabels <- c(
  PanelInputLabels,
  setNames(paste0("One-season lag (t-1) of ", sub("^Lag", "", lag_new), ": ",
                  PanelInputLabels[sub("^Lag", "", lag_new)], " (NA unless in sample in t-1)"),
           lag_new),
  CareerGames = "Sum of REG games played over panel seasons < season (pre-2013 OL undercounted)",
  CareerStartsDepth = "Sum of depth-chart starts over panel seasons < season",
  CareerInjuryWeeks = "Injury weeks (Out/Doubtful or RES/PUP) over panel seasons < season",
  CareerOffDefSnaps = "Offensive + defensive snaps over 2013+ panel seasons < season (NA if none)",
  CareerSTSnaps = "Special-teams snaps over 2013+ panel seasons < season (NA if none)",
  setNames(paste0("Sum over panel seasons < season (NA if none; seasons where the measure is NA add 0) of: ",
                  PanelInputLabels[CareerStems]), career_prod),
  RaceLabels,
  LogCapNumber = "log(CapNumber), cap number in millions of dollars (NA if CapNumber <= 0 or NA); main player-season outcome (position group x season FE absorb the league cap)",
  LogCapPct = "log(CapPercent) (NA if CapPercent is 0 or NA); CapPercent is OTC's cap number over the league salary cap, rounded to 0.001, so low cap hits are 0 (use LogCapNumber)",
  VeteranBargainedPay = "1 if the governing contract is a UFA or Extension contract (freely bargained veteran pay); 0 otherwise; NA if no governing contract",
  LogCashPaid = "log(CashPaid) (NA if CashPaid is 0 or NA)",
  LogGoverningAPY = "log(GoverningAPY), $ millions (NA if 0 or NA); with season FE equivalent to log(GoverningAPY / season cap)",
  BargainedPay = "1 if OnRookieContract == 0 (governing contract is not a Drafted/UDFA deal); NA if no governing contract",
  ExperienceBin = "Experience bin: 0, 1, 2, 3, 4-6, 7+ (same bins as the contracts' ExperienceBin)",
  AgeSq = "Age squared",
  NoPriorSeason = "1 - InSampleTminus1: not in the panel in season - 1 (Lag* measures set to 0)",
  NoCareerSeasons = "1 if the player has no panel season before season t (Career* sums set to 0)",
  CareerLeftCensored = "1 if RookieSeason < 2002 (seasons before the 2002 panel start are not in the career sums)",
  LogDraftPick = "log(DraftPick); 0 for undrafted players (Undrafted absorbs their level)",
  DraftRound = "NFL draft round; 0 = undrafted (for draft-round FE)"
)
filled_p <- c(blockA_p, blockB_all_p, lag_prod, blockC_all, career_prod, "LogDraftPick",
              blockD_slope, blockD_lin, blockE_lag, blockE_career, "PreDraftGrade")
PanelLabels[filled_p] <- paste0(PanelLabels[filled_p], fill_note)
PanelLabels <- c(PanelLabels, miss_labels(c(PMissA, PMissB, PMissC, PMissD, PMissE, PMissF)))
PanelLabels <- PanelLabels[!duplicated(names(PanelLabels), fromLast = TRUE)]

# Control dictionary: player-season
NContractRows <- length(Dictionary)
add_controls("player_season", "A", c("Experience", blockA_p, miss_of(blockA_p, PMissA)),
             "none", PanelLabels)
add_controls("player_season", "B", blockB_all_p, "all", PanelLabels)
add_controls("player_season", "B", "NoPriorSeason", "none", PanelLabels)
add_controls("player_season", "B", lag_prod, slope_of(ProductionSlopes$stem), PanelLabels)
add_controls("player_season", "B", miss_of(lag_prod, PMissB),
             slope_of(sub("^Lag(.*)Miss$", "\\1", miss_of(lag_prod, PMissB))), PanelLabels)
add_controls("player_season", "C", blockC_all, "all", PanelLabels)
add_controls("player_season", "C", c("NoCareerSeasons", "CareerLeftCensored"), "none", PanelLabels)
add_controls("player_season", "C", career_prod, slope_of(CareerStems), PanelLabels)
add_controls("player_season", "C", miss_of(career_prod, PMissC),
             slope_of(sub("^Career(.*)Miss$", "\\1", miss_of(career_prod, PMissC))), PanelLabels)
add_controls("player_season", "D", c("LogDraftPick", "Undrafted", miss_of("LogDraftPick", PMissD)),
             "none", PanelLabels)
add_controls("player_season", "D", c(blockD_slope, miss_of(blockD_slope, PMissD)), "all", PanelLabels)
add_controls("player_season", "D", c(blockD_lin, miss_of(blockD_lin, PMissD)), "none", PanelLabels)
add_controls("player_season", "E", c(blockE_lag, blockE_career,
                                     miss_of(c(blockE_lag, blockE_career), PMissE)),
             "all", PanelLabels)
add_controls("player_season", "F", c("PreDraftGrade", miss_of("PreDraftGrade", PMissF)), "none",
             PanelLabels)

PanelControls <- bind_rows(Dictionary[-seq_len(NContractRows)])$variable
PayPanel <- PayPanel |>
  select(gsis_id, season, display_name, PositionGroup, PrimaryFranchise, PayFranchise,
         OnRookieContract, BargainedPay, VeteranBargainedPay, GoverningContractType,
         GamesPlayed, WeeksPracticeSquad,
         GoverningContractId, LogCapNumber, LogCapPct, LogCashPaid, LogGoverningAPY, CapPercent, CashPaid, CapNumber,
         GoverningAPY, ExperienceBin, RookieSeason, DraftRound, DraftPick,
         InSampleTminus1, CombineInvite, AthleticScoreN, HasRecruit, HasCollegeLink,
         all_of(PanelControls), all_of(race_cols), HasWikiArticle) |>
  arrange(season, gsis_id)

# ---------------------------------------------------------------------------
# Control dictionary: validate, drop exact duplicates, write
# ---------------------------------------------------------------------------

# Within a sample, drop an indicator that is identical to an earlier listed
# control with the same slope groups (e.g. AgeSqMiss = AgeAtSigningMiss; the
# combine indicators of players without a combine row); its interactions
# would be perfectly collinear. The columns stay in the samples.
dedupe_controls <- function(dict, df) {
  keep <- rep(TRUE, nrow(dict))
  for (i in seq_len(nrow(dict))[-1]) {
    if (!endsWith(dict$variable[i], "Miss")) next
    earlier <- which(keep[seq_len(i - 1)] & dict$slope_groups[seq_len(i - 1)] == dict$slope_groups[i])
    if (any(map_lgl(dict$variable[earlier], \(v) identical(df[[v]], df[[dict$variable[i]]])))) {
      keep[i] <- FALSE
    }
  }
  if (any(!keep)) message("  dropped duplicate indicators: ",
                          paste(dict$variable[!keep], collapse = ", "))
  dict[keep, ]
}
ControlBlocks <- bind_rows(Dictionary)
ControlBlocks <- bind_rows(
  dedupe_controls(filter(ControlBlocks, sample == "contracts"), PayContracts),
  dedupe_controls(filter(ControlBlocks, sample == "player_season"), PayPanel))

# Every control exists, is numeric, has no NA and a label
for (smp in c("contracts", "player_season")) {
  df <- if (smp == "contracts") PayContracts else PayPanel
  vars <- ControlBlocks$variable[ControlBlocks$sample == smp]
  stopifnot(all(vars %in% names(df)), !anyDuplicated(vars),
            all(map_lgl(df[vars], is.numeric)),
            all(map_int(df[vars], \(x) sum(is.na(x))) == 0))
}
stopifnot(!anyNA(ControlBlocks$label),
          all(unlist(strsplit(setdiff(ControlBlocks$slope_groups, c("all", "none")), ";")) %in%
                PositionMap$position_group))
write_csv(ControlBlocks, file.path(analysis, "pay_control_blocks.csv"), na = "")
message(glue("pay_control_blocks.csv: {nrow(ControlBlocks)} controls ",
             "({sum(ControlBlocks$sample == 'contracts')} contracts, ",
             "{sum(ControlBlocks$sample == 'player_season')} player_season)"))

# ---------------------------------------------------------------------------
# Write the samples
# ---------------------------------------------------------------------------

write_sample(PayContracts, "analysis_pay_contracts", key = "contract_id",
             labels = ContractLabels[names(ContractLabels) %in% names(PayContracts)])
write_sample(PayPanel, "analysis_pay_player_season", key = c("gsis_id", "season"),
             labels = PanelLabels[names(PanelLabels) %in% names(PayPanel)])

# ---------------------------------------------------------------------------
# Summary: sample sizes, race coverage, raw gaps
# ---------------------------------------------------------------------------

# N by sample flag
ContractFlagN <- map_dfr(c("VeteranMarket", "BargainedMarket", "NonRookieSample", "UFAOnly", "ResignExtension",
                           "NewTeam", "UFAResign"),
                         \(f) tibble(flag = f, n_flag1 = sum(PayContracts[[f]] == 1, na.rm = TRUE),
                                     n_na = sum(is.na(PayContracts[[f]]))))
print(ContractFlagN)
PanelFlagN <- PayPanel |>
  # (NoGoverningContract first: later summaries must not mask the columns)
  summarise(n = n(), NoGoverningContract = sum(is.na(OnRookieContract)),
            NOnRookieContract = sum(OnRookieContract == 1, na.rm = TRUE),
            NBargainedPay = sum(BargainedPay == 1, na.rm = TRUE),
            NVeteranBargainedPay = sum(VeteranBargainedPay == 1, na.rm = TRUE),
            LogCapPctObserved = sum(!is.na(LogCapPct)),
            LogCapNumberObserved = sum(!is.na(LogCapNumber)),
            CapNumberNonPositive = sum(CapNumber <= 0, na.rm = TRUE),
            CapPercentZeroCapNumberPositive = sum(CapPercent == 0 & CapNumber > 0, na.rm = TRUE))
print(as.data.frame(PanelFlagN))

# Race coverage: share with a Wikipedia article, share flagged Black among
# them, and hand-code coverage
race_coverage <- function(df) {
  df |>
    summarise(n = n(), ShareWikiArticle = mean(HasWikiArticle),
              ShareFlaggedBlackAmongArticle = mean(wiki_cat_black[HasWikiArticle == 1] == 1),
              ShareHandCoded = mean(!is.na(black_any)),
              ShareBlackAnyAmongCoded = mean(black_any[!is.na(black_any)] == 1))
}
print(bind_rows(contracts_all = race_coverage(PayContracts),
                contracts_veteran = race_coverage(filter(PayContracts, VeteranMarket == 1)),
                player_season = race_coverage(PayPanel), .id = "sample"))

# Predicted race: mean P(Black any) and mean prior in the main sample
# (BargainedMarket) by position group
PayContracts |>
  filter(BargainedMarket == 1) |>
  group_by(PositionGroup) |>
  summarise(n = n(), MeanPBlackAnyPred = mean(p_black_any_pred, na.rm = TRUE),
            MeanPriorBlackPred = mean(prior_black_pred, na.rm = TRUE),
            ShareDocumented = mean(!is.na(documented_black_any)), .groups = "drop") |>
  print(n = Inf)
message(glue("BargainedMarket: mean p_black_any_pred = ",
             "{round(mean(PayContracts$p_black_any_pred[PayContracts$BargainedMarket %in% 1], na.rm = TRUE), 3)}; ",
             "missing p_black_any_pred: contracts {sum(is.na(PayContracts$p_black_any_pred))}, ",
             "player_season {sum(is.na(PayPanel$p_black_any_pred))}"))

# Athletic score coverage (share non-missing before zero-fill)
print(tibble(sample = c("contracts", "contracts_bargained", "player_season"),
             ShareAthleticScore = c(1 - mean(PayContracts$AthleticScoreMiss),
                                    1 - mean(PayContracts$AthleticScoreMiss[PayContracts$BargainedMarket %in% 1]),
                                    1 - mean(PayPanel$AthleticScoreMiss))))

# Raw mean LogAPYCapPct in the veteran market by Wikipedia Black flag
PayContracts |>
  filter(VeteranMarket == 1) |>
  mutate(WikiBlackFlag = case_when(is.na(wiki_cat_black) ~ "no article",
                                   wiki_cat_black == 1 ~ "flagged Black",
                                   TRUE ~ "article, not flagged")) |>
  group_by(WikiBlackFlag) |>
  summarise(n = n(), MeanLogAPYCapPct = mean(LogAPYCapPct, na.rm = TRUE),
            SDLogAPYCapPct = sd(LogAPYCapPct, na.rm = TRUE), .groups = "drop") |>
  print()

rm(ContractsRaw, PlayerSeasonRaw, PanelLags, PanelCareer, ContractCareer, CareerCheck)

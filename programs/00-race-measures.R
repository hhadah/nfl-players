# ============================================================================
# 00-race-measures.R
# Person-level race/ethnicity measures for coaches, front-office staff and
# players, combining (in order of priority):
#   1. human hand-coding (data/hand_coded/race_coding/: coder_A.csv,
#      coder_B.csv, adjudicated.csv; protocol in notes/race-coding-protocol.md)
#   2. Wikipedia category signals (positive-only screening aid; absence of a
#      category is NOT evidence of race)
#   3. name-based BIFSG posteriors (race_bifsg; secondary measure, known to
#      misclassify most Black individuals in this population)
#   4. PREDICTED race (race_predicted, scripts/04e_predict_race.py; design in
#      notes/race-prediction-design.md): the primary measure while hand codes
#      are absent. pred = model-only posterior (BIFSG likelihood x an NFL prior
#      estimated by EM on predetermined covariates); preddoc = documented race
#      where a public source states it, else a prior for the undocumented
#      (fame-dependent; sensitivity only).
# Every analysis sample takes race from load_person_race().
# Date: 2026-09-26; predicted race added 2026-10-02
# ============================================================================

race_levels <- c("black", "white", "asian", "pacific_islander",
                 "american_indian", "multiracial", "unknown")
coding_cols <- c("race", "multiracial_components", "hispanic", "basis",
                 "source_url", "confidence", "notes")

# Read one coder's sheet; keep only rows with a race code
read_coder_sheet <- function(path, coder) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, col_types = cols(.default = col_character())) |>
    select(person_uid, any_of(coding_cols)) |>
    mutate(across(c(race, hispanic), \(x) str_to_lower(str_trim(x)))) |>
    filter(!is.na(race), race != "") |>
    mutate(coder = coder)
}

# Hand-coded race: adjudicated code first, then agreeing coders, then a single
# coder. Disagreements without adjudication are returned as NA ("disputed").
hand_coded_race <- function(hand_coded_dir) {
  dir <- file.path(hand_coded_dir, "race_coding")
  sheets <- bind_rows(read_coder_sheet(file.path(dir, "coder_A.csv"), "A"),
                      read_coder_sheet(file.path(dir, "coder_B.csv"), "B"))
  adjudicated <- read_coder_sheet(file.path(dir, "adjudicated.csv"), "adjudicated")

  empty <- tibble(person_uid = character(), race = character(),
                  multiracial_components = character(), hispanic = character(),
                  race_source = character())
  if (is.null(sheets) || nrow(sheets) == 0) sheets <- NULL

  coders <- if (is.null(sheets)) empty else {
    sheets |>
      group_by(person_uid) |>
      summarise(n_coders = n(),
                agree = n_distinct(race) == 1 &
                  n_distinct(coalesce(hispanic, "unknown")) == 1,
                race = if_else(agree, first(race), NA_character_),
                multiracial_components = if_else(agree, first(multiracial_components),
                                                 NA_character_),
                hispanic = if_else(agree, first(hispanic), NA_character_),
                .groups = "drop") |>
      mutate(race_source = case_when(n_coders >= 2 & agree ~ "coder_agree",
                                     n_coders == 1 ~ "single_coder",
                                     TRUE ~ "disputed")) |>
      select(-n_coders, -agree)
  }

  if (!is.null(adjudicated) && nrow(adjudicated) > 0) {
    adj <- adjudicated |>
      distinct(person_uid, .keep_all = TRUE) |>
      transmute(person_uid, race, multiracial_components, hispanic,
                race_source = "adjudicated")
    coders <- bind_rows(adj, anti_join(coders, adj, by = "person_uid"))
  }

  coders |>
    mutate(race = if_else(race %in% race_levels, race, NA_character_),
           black_any = case_when(race == "black" ~ 1L,
                                 str_detect(coalesce(multiracial_components, ""),
                                            "black") ~ 1L,
                                 race %in% c("unknown") | is.na(race) ~ NA_integer_,
                                 TRUE ~ 0L),
           nonwhite = case_when(is.na(race) | race == "unknown" ~ NA_integer_,
                                race == "white" & hispanic == "no" ~ 0L,
                                race == "white" & hispanic %in% c("unknown", NA) ~ NA_integer_,
                                TRUE ~ 1L))
}

# Wikipedia category flags (staff and, when built, players)
wiki_signals <- function(con) {
  flags <- c("cat_black", "cat_hispanic_latino", "cat_asian",
             "cat_pacific_islander", "cat_native_american")
  pull_signals <- function(table, id_col, prefix) {
    if (!dbExistsTable(con, table)) return(NULL)
    tbl(con, table) |>
      select(all_of(c(id_col, flags))) |>
      collect() |>
      transmute(person_uid = paste0(prefix, .data[[id_col]]),
                across(all_of(flags), as.integer)) |>
      rename_with(\(x) paste0("wiki_", x), all_of(flags))
  }
  bind_rows(pull_signals("staff_person_wiki_signals", "person_id", "staff:"),
            pull_signals("player_wiki_signals", "gsis_id", "player:"))
}

# BIFSG posteriors (name-based)
bifsg_measures <- function(con) {
  tbl(con, "race_bifsg") |>
    filter(entity %in% c("staff_persons", "nfl_players")) |>
    select(entity, entity_id, p_white, p_black, p_hispanic, p_api, p_aian,
           p_multi, race_bifsg) |>
    collect() |>
    transmute(person_uid = paste0(if_else(entity == "staff_persons", "staff:", "player:"),
                                  entity_id),
              across(c(p_white, p_black, p_hispanic, p_api, p_aian, p_multi),
                     \(x) x, .names = "{.col}_bifsg"),
              race_bifsg)
}

# Predicted race (race_predicted): probabilities under the primary model-only
# variant (*_pred) and the documented sensitivity variant (*_preddoc), the
# documented race used for validation, and the predetermined covariates of the
# EM prior (prefixed pred_; regressions under the predicted measure control for
# them, as regression calibration requires). NULL when the table is absent.
predicted_race_measures <- function(con) {
  if (!dbExistsTable(con, "race_predicted")) return(NULL)
  tbl(con, "race_predicted") |>
    select(person_uid, p_white_pred, p_black_pred, p_hispanic_pred, p_api_pred,
           p_aian_pred, p_multi_pred, p_black_any_pred, pred_method,
           prior_black_pred, p_white_preddoc, p_black_preddoc, p_hispanic_preddoc,
           p_api_preddoc, p_aian_preddoc, p_multi_preddoc, p_black_any_preddoc,
           pred_method_preddoc, documented_race, documented_black_any,
           documented_hispanic, documented_sources,
           pred_pos_group = pos_group, pred_rookie_era = rookie_era,
           pred_draft_bucket = draft_bucket, pred_college_type = college_type,
           pred_role_group_first = role_group_first, pred_unit_first = unit_first,
           pred_first_era = first_era, pred_former_player = former_player) |>
    collect() |>
    mutate(documented_black_any = as.integer(documented_black_any),
           pred_former_player = as.character(pred_former_player))
}

# One row per person: person_uid ('staff:<person_id>' or 'player:<gsis_id>'),
# hand-coded race (race, hispanic, black_any, nonwhite, race_source), Wikipedia
# flags (wiki_cat_*), BIFSG posteriors (p_*_bifsg, race_bifsg), and
# black_provisional = hand-coded black_any when coded, else 1 when a Wikipedia
# category flags the person as Black, else NA (never 0 from a missing flag),
# and the predicted-race columns of predicted_race_measures().
load_person_race <- function(con, hand_coded_dir) {
  persons <- bind_rows(
    tbl(con, "staff_persons") |> select(id = person_id) |> collect() |>
      transmute(person_uid = paste0("staff:", id), entity = "staff", person_id = id),
    tbl(con, "nfl_players") |> select(id = gsis_id) |> collect() |>
      transmute(person_uid = paste0("player:", id), entity = "player", person_id = id))

  # Staff who also played are coded once, as staff; their player rows inherit
  # that code (Wikidata-verified links only)
  hand <- hand_coded_race(hand_coded_dir)
  links_path <- file.path(hand_coded_dir, "race_coding", "person_links.csv")
  if (file.exists(links_path)) {
    inherited <- read_csv(links_path, col_types = cols(.default = col_character())) |>
      filter(link_method == "wikidata") |>
      transmute(staff_uid = paste0("staff:", person_id),
                person_uid = paste0("player:", gsis_id)) |>
      inner_join(hand, by = c("staff_uid" = "person_uid")) |>
      select(-staff_uid) |>
      anti_join(hand, by = "person_uid")
    hand <- bind_rows(hand, inherited)
  }

  predicted <- predicted_race_measures(con)
  if (is.null(predicted)) predicted <- tibble(person_uid = character())

  persons |>
    left_join(hand, by = "person_uid") |>
    left_join(wiki_signals(con), by = "person_uid") |>
    left_join(bifsg_measures(con), by = "person_uid") |>
    left_join(predicted, by = "person_uid") |>
    mutate(black_provisional = case_when(!is.na(black_any) ~ black_any,
                                         wiki_cat_black == 1L ~ 1L,
                                         TRUE ~ NA_integer_),
           black_provisional_source = case_when(!is.na(black_any) ~ race_source,
                                                wiki_cat_black == 1L ~ "wiki_category",
                                                TRUE ~ NA_character_))
}

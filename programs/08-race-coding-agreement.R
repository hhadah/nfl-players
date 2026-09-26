# ============================================================================
# 08-race-coding-agreement.R
# Inter-coder reliability of the race/ethnicity hand-coding
# (notes/race-coding-protocol.md) and accuracy of the machine signals.
#   - Cohen's kappa and raw agreement between coders A and B for race,
#     hispanic and black_any: overall, by tier and by entity
#   - data/hand_coded/race_coding/adjudication_needed.csv: disagreements and
#     codes with confidence 1 that are not yet adjudicated
#   - misclassification of the machine signals (Wikipedia category flag,
#     name-based BIFSG) against the hand codes, and sensitivity/specificity
#     for Black
# Tables: output/tables (and my_paper/tables) race-coding-agreement.tex,
# race-machine-signal-accuracy.tex and race-bifsg-misclassification.tex,
# written only once coded rows exist.
# Sourced by 95-make-all.R (directory objects root, hand_coded, tables_wd,
# paper_tables, db_path; read_coder_sheet() and load_person_race() from
# 00-race-measures.R).
# Date: 2026-09-26
# ============================================================================

race_coding_dir <- file.path(hand_coded, "race_coding")

# Cohen's kappa for two raters' nominal codes (pairs with a missing code dropped)
cohen_kappa <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) == 0) return(NA_real_)
  lv <- union(x, y)
  p_a <- table(factor(x, levels = lv)) / length(x)
  p_b <- table(factor(y, levels = lv)) / length(y)
  p_o <- mean(x == y)
  p_e <- sum(p_a * p_b)
  if (p_e == 1) return(NA_real_)
  (p_o - p_e) / (1 - p_e)
}

# black_any as defined in the protocol (NA when race is unknown or missing)
derive_black_any <- function(race, multiracial_components) {
  case_when(race == "black" ~ 1L,
            race == "multiracial" &
              str_detect(coalesce(multiracial_components, ""), "black") ~ 1L,
            is.na(race) | race == "unknown" ~ NA_integer_,
            TRUE ~ 0L)
}

# Coder sheets (rows with a race code) and person identity
CoderA <- read_coder_sheet(file.path(race_coding_dir, "coder_A.csv"), "A")
CoderB <- read_coder_sheet(file.path(race_coding_dir, "coder_B.csv"), "B")
Adjudicated <- read_coder_sheet(file.path(race_coding_dir, "adjudicated.csv"),
                                "adjudicated")

# Identity columns from the coder sheets (they keep persons who left the
# universe, flagged stale = 1)
read_identity <- function(path) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, col_types = cols(.default = col_character())) |>
    select(person_uid, tier, entity, display_name, context, stale)
}
PersonsToCode <- bind_rows(read_identity(file.path(race_coding_dir, "coder_A.csv")),
                           read_identity(file.path(race_coding_dir, "coder_B.csv"))) |>
  distinct(person_uid, .keep_all = TRUE)

# Coder A and B side by side, one row per person coded by at least one coder
pair_codes <- function(coder_a, coder_b) {
  empty <- tibble(person_uid = character(), race = character(),
                  multiracial_components = character(), hispanic = character(),
                  confidence = character())
  prep <- function(d, suffix) {
    (if (is.null(d)) empty else d) |>
      mutate(black_any = derive_black_any(race, multiracial_components)) |>
      select(person_uid, race, hispanic, black_any, multiracial_components,
             confidence) |>
      rename_with(\(x) paste0(x, "_", suffix), -person_uid)
  }
  full_join(prep(coder_a, "A"), prep(coder_b, "B"), by = "person_uid") |>
    left_join(PersonsToCode, by = "person_uid")
}

# Raw agreement and kappa by measure, for one group of double-coded persons
agreement_row <- function(d, group) {
  map(c(race = "race", hispanic = "hispanic", black_any = "black_any"), \(m) {
    a <- as.character(d[[paste0(m, "_A")]])
    b <- as.character(d[[paste0(m, "_B")]])
    both <- !is.na(a) & !is.na(b)
    tibble(Group = group, Measure = m, N = sum(both),
           Agreement = if (any(both)) mean(a[both] == b[both]) else NA_real_,
           Kappa = cohen_kappa(a, b))
  }) |>
    list_rbind()
}

# Agreement overall, by tier and by entity (persons coded by both coders)
agreement_table <- function(pairs) {
  DoubleCoded <- pairs |>
    filter(!is.na(race_A), !is.na(race_B))
  bind_rows(
    agreement_row(DoubleCoded, "Overall"),
    DoubleCoded |>
      arrange(as.integer(tier)) |>
      group_split(tier) |>
      map(\(d) agreement_row(d, paste("Tier", first(d$tier)))) |>
      list_rbind(),
    DoubleCoded |>
      group_split(entity) |>
      map(\(d) agreement_row(d, if_else(first(d$entity) == "staff", "Staff", "Players"))) |>
      list_rbind())
}

# Persons needing adjudication: coders disagree on race, hispanic or the
# multiracial components, or either code has confidence 1; minus persons
# already in adjudicated.csv
adjudication_needed <- function(pairs, adjudicated) {
  done <- if (is.null(adjudicated)) character() else adjudicated$person_uid
  pairs |>
    mutate(DisagreeRace = !is.na(race_A) & !is.na(race_B) & race_A != race_B,
           DisagreeHispanic = !is.na(race_A) & !is.na(race_B) &
             coalesce(hispanic_A, "unknown") != coalesce(hispanic_B, "unknown"),
           DisagreeComponents = !DisagreeRace & coalesce(race_A == "multiracial", FALSE) &
             coalesce(multiracial_components_A, "") != coalesce(multiracial_components_B, ""),
           LowConfidence = coalesce(confidence_A == "1", FALSE) |
             coalesce(confidence_B == "1", FALSE),
           reason = pmap_chr(list(DisagreeRace, DisagreeHispanic, DisagreeComponents,
                                  LowConfidence),
                             \(r, h, m, l) paste(c("race", "hispanic",
                                                   "multiracial_components",
                                                   "confidence_1")[c(r, h, m, l)],
                                                 collapse = ";"))) |>
    filter(reason != "", !person_uid %in% done) |>
    arrange(as.integer(tier), display_name) |>
    select(person_uid, tier, entity, display_name, context, reason,
           race_A, multiracial_components_A, hispanic_A, confidence_A,
           race_B, multiracial_components_B, hispanic_B, confidence_B)
}

# Hand-coded race in the BIFSG categories (Hispanic of any race -> hispanic)
hand_race_bifsg_scale <- function(race, hispanic) {
  case_when(hispanic == "yes" ~ "hispanic",
            race == "black" ~ "black",
            race == "white" ~ "white",
            race %in% c("asian", "pacific_islander") ~ "api",
            race == "american_indian" ~ "aian",
            race == "multiracial" ~ "multi",
            TRUE ~ NA_character_)
}

# Sensitivity/specificity of a binary Black signal against hand-coded black_any
black_accuracy <- function(d, signal, label) {
  d |>
    filter(!is.na(black_any), !is.na(.data[[signal]])) |>
    summarise(Signal = label, N = n(), `N Black` = sum(black_any == 1L),
              Sensitivity = mean(.data[[signal]][black_any == 1L] == 1L),
              Specificity = mean(.data[[signal]][black_any == 0L] == 0L),
              `Positive predictive value` = mean(black_any[.data[[signal]] == 1L] == 1L)) |>
    mutate(across(where(is.double), \(x) if_else(is.nan(x), NA_real_, x)))
}

# Machine signals against the hand codes, among hand-coded persons
machine_signal_accuracy <- function(person_race) {
  Coded <- person_race |>
    filter(!is.na(race), race != "unknown") |>
    mutate(HandRace = hand_race_bifsg_scale(race, hispanic),
           # Wikipedia flag: positive-only, so 'no flag' counts as 0 among persons
           # with an article and stays NA without one
           WikiBlack = wiki_cat_black,
           BifsgBlack = as.integer(race_bifsg == "black"))
  Misclassification <- Coded |>
    filter(!is.na(HandRace), !is.na(race_bifsg)) |>
    count(HandRace, race_bifsg) |>
    group_by(HandRace) |>
    mutate(RowShare = n / sum(n)) |>
    ungroup()
  Accuracy <- bind_rows(
    black_accuracy(Coded, "WikiBlack", "Wikipedia category (persons with an article)"),
    black_accuracy(Coded |> filter(entity == "staff"), "WikiBlack",
                   "Wikipedia category, staff"),
    black_accuracy(Coded |> filter(entity == "player"), "WikiBlack",
                   "Wikipedia category, players"),
    black_accuracy(Coded, "BifsgBlack", "BIFSG label (argmax)"),
    black_accuracy(Coded |> filter(entity == "staff"), "BifsgBlack", "BIFSG label, staff"),
    black_accuracy(Coded |> filter(entity == "player"), "BifsgBlack", "BIFSG label, players"))
  list(misclassification = Misclassification, accuracy = Accuracy,
       n_coded = nrow(Coded))
}

# Write a LaTeX table (kableExtra, threeparttable notes) to tables_wd and paper_tables
write_race_table <- function(df, file, title, label, note) {
  Tex <- df |>
    mutate(across(where(is.double), \(x) if_else(is.na(x), "", sprintf("%.3f", x)))) |>
    kbl(format = "latex", booktabs = TRUE, linesep = "",
        caption = paste0(title, " \\label{tab:", label, "}"), escape = TRUE) |>
    kable_styling(latex_options = "hold_position") |>
    footnote(general = note, general_title = "Notes:", threeparttable = TRUE,
             footnote_as_chunk = TRUE)
  for (d in c(tables_wd, paper_tables)) writeLines(as.character(Tex), file.path(d, file))
}

# ---------------------------------------------------------------------------
# Run (skipped while both coder sheets are blank)
# ---------------------------------------------------------------------------

CodedRows <- bind_rows(CoderA, CoderB)

if (nrow(CodedRows) == 0) {
  message("08-race-coding-agreement: no coded rows yet; skipping")
} else {
  # Coverage and evidence basis
  message(glue("08-race-coding-agreement: {n_distinct(CoderA$person_uid)} persons coded ",
               "by A, {n_distinct(CoderB$person_uid)} by B"))
  CodedRows |> count(coder, basis) |> print()

  # Agreement statistics
  Pairs <- pair_codes(CoderA, CoderB)
  Agreement <- agreement_table(Pairs)
  print(Agreement, n = Inf)

  # Adjudication queue
  AdjudicationNeeded <- adjudication_needed(Pairs, Adjudicated)
  write_csv(AdjudicationNeeded, file.path(race_coding_dir, "adjudication_needed.csv"),
            na = "")
  message(glue("adjudication_needed.csv: {nrow(AdjudicationNeeded)} persons"))

  if (any(Agreement$N > 0)) {
    Agreement |>
      pivot_wider(id_cols = Group, names_from = Measure,
                  values_from = c(N, Agreement, Kappa), names_glue = "{Measure}_{.value}") |>
      transmute(Group, N = race_N,
                `Race: agreement` = race_Agreement, `Race: kappa` = race_Kappa,
                `Hispanic: agreement` = hispanic_Agreement, `Hispanic: kappa` = hispanic_Kappa,
                `Black: agreement` = black_any_Agreement, `Black: kappa` = black_any_Kappa) |>
      write_race_table("race-coding-agreement.tex",
                       "Inter-coder agreement on race and ethnicity",
                       "race-coding-agreement",
                       paste("N is the number of persons coded by both coders. Agreement is",
                             "the share of identical codes; kappa is Cohen's kappa. Race has",
                             "seven categories (including unknown); Hispanic is yes/no/unknown;",
                             "Black equals one when race is Black or a multiracial code includes",
                             "Black (unknown race excluded). Tiers follow the coding protocol."))
  }

  # Machine signals against the hand codes
  con <- db_connect()
  PersonRace <- load_person_race(con, hand_coded)
  db_disconnect(con)
  MachineAccuracy <- machine_signal_accuracy(PersonRace)
  print(MachineAccuracy$misclassification, n = Inf)
  print(MachineAccuracy$accuracy)
  if (MachineAccuracy$n_coded > 0) {
    write_race_table(MachineAccuracy$accuracy, "race-machine-signal-accuracy.tex",
                     "Accuracy of machine race signals against the hand codes",
                     "race-machine-signal-accuracy",
                     paste("Hand-coded persons with a known race (race_source adjudicated,",
                           "coder_agree or single_coder). The Wikipedia signal is a",
                           "category flag and is defined only for persons with an article;",
                           "the BIFSG signal is the name-based argmax label. Sensitivity is",
                           "the share of hand-coded Black persons the signal flags;",
                           "specificity the share of non-Black persons it does not flag."))
    # Misclassification matrix of the BIFSG label: rows hand code, columns label
    MachineAccuracy$misclassification |>
      mutate(Cell = sprintf("%d (%.2f)", n, RowShare)) |>
      pivot_wider(id_cols = HandRace, names_from = race_bifsg, values_from = Cell,
                  values_fill = "0", names_sort = TRUE) |>
      rename(`Hand code` = HandRace) |>
      write_race_table("race-bifsg-misclassification.tex",
                       "Misclassification of the BIFSG race label against the hand codes",
                       "race-bifsg-misclassification",
                       paste("Rows are the hand code (Hispanic of any race coded as Hispanic,",
                             "Asian and Pacific Islander pooled as api); columns are the",
                             "name-based BIFSG argmax label. Cells give the number of persons",
                             "and, in parentheses, the row share."))
  }
}

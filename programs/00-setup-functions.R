# ============================================================================
# 00-setup-functions.R
# Shared helpers for every analysis script sourced by 95-make-all.R:
#   - db_connect() / db_disconnect(): read-only DuckDB connection to db_path
#   - write_sample(): key check + parquet/csv + codebook for an analysis sample
#   - small numeric helpers (safe_div, lag_within, blau_expected, ...)
#   - theme_customs(): ggplot2 theme (LM Roman / Fira Sans when installed)
# Expects the directory objects defined in 95-make-all.R (db_path, analysis).
# Date: 2026-09-26
# ============================================================================

# ---------------------------------------------------------------------------
# Database connection
# ---------------------------------------------------------------------------

# Open the project DuckDB read-only (analysis scripts never write to it)
db_connect <- function(path = db_path) {
  if (!file.exists(path)) stop("DuckDB not found: ", path)
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
}

# Close a connection and shut down its DuckDB instance
db_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Writing analysis samples
# ---------------------------------------------------------------------------

# Stop unless the columns in `key` uniquely identify the rows of df
check_key <- function(df, key, name = deparse(substitute(df))) {
  missing_cols <- setdiff(key, names(df))
  if (length(missing_cols) > 0) {
    stop(name, ": key columns not found: ", paste(missing_cols, collapse = ", "))
  }
  n_na <- sum(!complete.cases(df[key]))
  if (n_na > 0) stop(name, ": ", n_na, " rows have a missing key value")
  n_dup <- sum(duplicated(df[key]))
  if (n_dup > 0) {
    stop(name, ": key (", paste(key, collapse = ", "), ") is not unique: ",
         n_dup, " duplicated rows")
  }
  invisible(TRUE)
}

# Codebook: one row per variable with type, share non-missing, min, max and
# label. min/max are reported for numeric, logical and date columns (as text).
make_codebook <- function(df, labels = character()) {
  summarise_col <- function(x) {
    ok <- !is.na(x)
    rng <- if (any(ok) && (is.numeric(x) || is.logical(x) || inherits(x, "Date"))) {
      r <- range(if (is.logical(x)) as.integer(x[ok]) else x[ok])
      if (is.numeric(r)) r <- signif(r, 6)
      as.character(r)
    } else c(NA_character_, NA_character_)
    tibble(type = class(x)[1], share_nonmissing = round(mean(ok), 4),
           min = rng[1], max = rng[2])
  }
  tibble(variable = names(df)) |>
    bind_cols(map_dfr(df, summarise_col)) |>
    mutate(label = unname(labels[variable]))
}

# Check the key, then write analysis/<name>.parquet, analysis/<name>.csv and
# analysis/codebook_<name>.csv. `labels` is a named character vector
# (variable = label); variables without a label are reported.
write_sample <- function(df, name, key, labels = character(), out_dir = analysis) {
  check_key(df, key, name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, file.path(out_dir, paste0(name, ".parquet")))
  readr::write_csv(df, file.path(out_dir, paste0(name, ".csv")), na = "")
  codebook <- make_codebook(df, labels)
  readr::write_csv(codebook, file.path(out_dir, paste0("codebook_", name, ".csv")),
                   na = "")
  unlabelled <- codebook$variable[is.na(codebook$label)]
  if (length(unlabelled) > 0) {
    message(name, ": ", length(unlabelled), " variables without a label: ",
            paste(unlabelled, collapse = ", "))
  }
  message(glue::glue("{name}: wrote {nrow(df)} rows x {ncol(df)} columns to {out_dir}"))
  invisible(codebook)
}

# ---------------------------------------------------------------------------
# Numeric helpers
# ---------------------------------------------------------------------------

# Division that returns NA (not Inf/NaN) when the denominator is 0 or NA
safe_div <- function(num, den) {
  if_else(is.na(den) | den == 0, NA_real_, as.numeric(num) / as.numeric(den))
}

# Mean that returns NA (not NaN) when no value is observed
mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# Lag of x within a group ordered by `time`, requiring consecutive time
# (a gap in time returns NA rather than the value from an earlier period).
# Use inside group_by(): mutate(LagY = lag_within(Y, season)).
lag_within <- function(x, time, n = 1L) {
  ord <- order(time)
  out <- rep(x[NA_integer_], length(x))
  lagged <- dplyr::lag(x[ord], n)
  lag_time <- dplyr::lag(time[ord], n)
  lagged[is.na(lag_time) | time[ord] - lag_time != n] <- NA
  out[ord] <- lagged
  out
}

# Expected Blau index from individual probability vectors: the probability
# that two DISTINCT members drawn at random (without replacement) belong to
# different categories, treating members' categories as independent draws
# from their probability vectors.
#   1 - sum_k [ (sum_i p_ik)^2 - sum_i p_ik^2 ] / (n (n - 1))
# P is an n x K numeric matrix (rows sum to 1). NA when n < 2.
blau_expected <- function(P) {
  P <- as.matrix(P)
  n <- nrow(P)
  if (n < 2) return(NA_real_)
  1 - sum(colSums(P)^2 - colSums(P^2)) / (n * (n - 1))
}

# Categorical Blau index for observed categories (same without-replacement
# definition as blau_expected with one-hot rows). NA values are dropped;
# NA when fewer than 2 categorised members remain.
blau_categorical <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(NA_real_)
  counts <- table(x)
  1 - sum(counts * (counts - 1)) / (n * (n - 1))
}

# ---------------------------------------------------------------------------
# ggplot2 theme
# ---------------------------------------------------------------------------

# Register a locally installed font family with showtext (no network); return
# the family name when found, else NULL.
register_local_font <- function(family, regular_pattern = "Regular") {
  if (!requireNamespace("showtext", quietly = TRUE) ||
      !requireNamespace("systemfonts", quietly = TRUE)) return(NULL)
  fonts <- systemfonts::system_fonts()
  hit <- fonts[fonts$family == family, ]
  if (nrow(hit) == 0) return(NULL)
  regular <- hit[grepl(regular_pattern, hit$style, ignore.case = TRUE), ]
  path <- if (nrow(regular) > 0) regular$path[1] else hit$path[1]
  bold <- hit[grepl("^Bold$", hit$style), ]
  sysfonts::font_add(family, regular = path,
                     bold = if (nrow(bold) > 0) bold$path[1] else path)
  family
}

# Project theme on theme_minimal(): LM Roman (or Fira Sans) through showtext
# when installed locally, otherwise the default sans family; bold titles.
theme_customs <- function(base_size = 12) {
  family <- getOption("nfl.theme_family")
  if (is.null(family)) {
    family <- register_local_font("Latin Modern Roman") %||%
      register_local_font("Fira Sans") %||% ""
    if (family != "") showtext::showtext_auto()
    options(nfl.theme_family = family)
  }
  theme_minimal(base_size = base_size, base_family = family) +
    theme(plot.title = element_text(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(size = rel(0.95)),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.caption = element_text(hjust = 0, size = rel(0.8)))
}

# ---------------------------------------------------------------------------
# Shared sample builders
# ---------------------------------------------------------------------------

# Map head-coach names (nflverse schedules) to staff person_id. Order:
# (1) exact name among the franchise-season's HC entries in staff_team_season;
# (2) same last name among those entries (e.g. 'Jay Rosburg' -> Jerry Rosburg);
# (3) a unique exact name among staff_persons who were ever head coach.
# `hc` needs franchise_id, season, HeadCoachName; adds HeadCoachPersonId and
# HCMatchMethod (NA when no rule matches, e.g. an unparsed article season).
match_hc_person <- function(hc, con) {
  last_name <- function(x) str_to_lower(word(str_remove(x, ",? (Jr\\.|Sr\\.|II|III)$"), -1))
  entries <- tbl(con, "staff_team_season") |>
    filter(role_std == "HC") |>
    distinct(franchise_id, season, person_id, person_name) |>
    collect() |>
    mutate(season = as.integer(season), LastName = last_name(person_name))
  global <- tbl(con, "staff_persons") |>
    filter(ever_head_coach) |>
    select(person_id, person_name) |>
    collect() |>
    add_count(person_name) |>
    filter(n == 1) |>
    select(HeadCoachName = person_name, GlobalId = person_id)
  exact <- entries |>
    distinct(franchise_id, season, HeadCoachName = person_name, .keep_all = TRUE) |>
    select(franchise_id, season, HeadCoachName, ExactId = person_id)
  by_last <- entries |>
    add_count(franchise_id, season, LastName) |>
    filter(n == 1) |>
    select(franchise_id, season, LastName, LastId = person_id)
  hc |>
    mutate(LastName = last_name(HeadCoachName)) |>
    left_join(exact, by = c("franchise_id", "season", "HeadCoachName")) |>
    left_join(by_last, by = c("franchise_id", "season", "LastName")) |>
    left_join(global, by = "HeadCoachName") |>
    mutate(HeadCoachPersonId = coalesce(ExactId, LastId, GlobalId),
           HCMatchMethod = case_when(!is.na(ExactId) ~ "franchise_season_exact",
                                     !is.na(LastId) ~ "franchise_season_last_name",
                                     !is.na(GlobalId) ~ "global_unique_name",
                                     TRUE ~ NA_character_)) |>
    select(-LastName, -ExactId, -LastId, -GlobalId)
}

# Interim head coaches that nflverse misses (template era). For a
# franchise-season with one nflverse head coach P, an override is created when
# a staff-template snapshot lists an interim head coach I != P and no longer
# lists P (a firing; interim spells with P still listed, e.g. IND 2012 or
# CLE 2021, are left to nflverse). The firing date is bracketed by
#   LowerDate = target date of the last snapshot still listing P, and
#   UpperDate = date of the first revision listing I (the revision in force
#               at the first snapshot that lists I).
# I is assigned to games on or after UpperDate; games strictly between the two
# dates keep P and are flagged HCChangeWindow (the change date is unknown).
interim_hc_overrides <- function(con) {
  snaps <- tbl(con, "staff_snapshots") |>
    filter(source == "staff_template") |>
    select(franchise_id, season, snapshot, target_date, revision_timestamp)
  hc_rows <- tbl(con, "staff_entries") |>
    filter(role_std == "HC", !is.na(person_id)) |>
    select(franchise_id, season, snapshot, person_id, person_name, interim) |>
    inner_join(snaps, by = c("franchise_id", "season", "snapshot")) |>
    collect() |>
    mutate(season = as.integer(season),
           UpperCandidate = as.Date(revision_timestamp, tz = "America/New_York"))
  single_nflverse <- tbl(con, "staff_hc_reconciliation") |>
    filter(wiki_parse_ok, n_nflverse_hc == 1) |>
    select(franchise_id, season, NflverseHC = nflverse_hc) |>
    collect() |>
    mutate(season = as.integer(season),
           NflverseHC = str_remove(NflverseHC, " \\(\\d+ g\\)$"))
  hc_rows |>
    inner_join(single_nflverse, by = c("franchise_id", "season")) |>
    group_by(franchise_id, season) |>
    group_modify(\(d, key) {
      p_dates <- d$target_date[d$person_name == d$NflverseHC[1]]
      firing <- d |>
        filter(interim, person_name != NflverseHC) |>
        group_by(snapshot, target_date, UpperCandidate) |>
        filter(!any(d$person_name[d$snapshot == first(snapshot)] == NflverseHC[1])) |>
        ungroup() |>
        arrange(target_date) |>
        slice_head(n = 1)
      if (nrow(firing) == 0) return(tibble())
      tibble(InterimPersonId = firing$person_id, InterimName = firing$person_name,
             FiredHC = d$NflverseHC[1],
             LowerDate = max(p_dates[p_dates < firing$target_date]),
             UpperDate = firing$UpperCandidate)
    }) |>
    ungroup()
}

# In-season head-coach changes with a verified date. nflverse schedules (a)
# keep the fired coach for the rest of the season in template-era firings and
# (b) put the change in the wrong week in ARI/DET/WAS 2000 and ATL 2007; the
# Wikipedia snapshot bounds of interim_hc_overrides() lag some firings by
# weeks. Each row: games of the franchise-season with gameday > AfterDate (and
# <= UntilDate when given, for a temporary acting HC) are coached by ActingHC;
# the franchise-season's other games by ReplacedHC. AfterDate is the dismissal
# date, or the date of the replaced coach's last game when the article states
# the change by game. Source: the Wikipedia season article named in Source
# (text checked 2026-09-26; cached in data/raw/wikipedia/hc_change_verify).
# Hand-maintained in data/hand_coded/hc_spell_corrections.csv.
HCSpellCorrections <- read_csv(file.path(hand_coded, "hc_spell_corrections.csv"),
                               col_types = cols(season = col_integer(),
                                                AfterDate = col_date(),
                                                UntilDate = col_date(),
                                                .default = col_character()))

# Head coach of every team-game (REG + POST): the nflverse schedule coach,
# corrected in this order:
#   1. HCSpellCorrections (verified dates) for the franchise-seasons it lists;
#   2. otherwise interim_hc_overrides() (Wikipedia snapshots) for games on or
#      after UpperDate, with HCChangeWindow flagging games inside the unknown
#      firing window (kept with the fired HC).
# Returns one row per franchise_id x game_id with HeadCoachName,
# HeadCoachPersonId, HCMatchMethod, HCSource ('nflverse',
# 'verified_date_correction' or 'wiki_interim_override') and HCChangeWindow.
load_game_head_coaches <- function(con) {
  overrides <- interim_hc_overrides(con) |>
    anti_join(HCSpellCorrections, by = c("franchise_id", "season"))
  tbl(con, "nfl_team_week_head_coach") |>
    select(franchise_id, season, game_id, game_type, week, gameday, head_coach) |>
    collect() |>
    mutate(season = as.integer(season), gameday = as.Date(gameday)) |>
    left_join(HCSpellCorrections |> select(-Source), by = c("franchise_id", "season")) |>
    left_join(overrides, by = c("franchise_id", "season")) |>
    mutate(InActingSpell = gameday > AfterDate & (is.na(UntilDate) | gameday <= UntilDate),
           Corrected = !is.na(AfterDate) &
             (head_coach == ReplacedHC | head_coach == ActingHC),
           CorrectedName = if_else(InActingSpell, ActingHC, ReplacedHC),
           Override = !Corrected & !is.na(UpperDate) & head_coach == FiredHC &
             gameday >= UpperDate,
           HCChangeWindow = !Corrected & !is.na(UpperDate) & head_coach == FiredHC &
             gameday > LowerDate & gameday < UpperDate,
           HeadCoachName = case_when(Corrected ~ CorrectedName,
                                     Override ~ InterimName,
                                     TRUE ~ head_coach),
           HCSource = case_when(HeadCoachName == head_coach ~ "nflverse",
                                Corrected ~ "verified_date_correction",
                                TRUE ~ "wiki_interim_override")) |>
    match_hc_person(con) |>
    mutate(HeadCoachPersonId = if_else(Override, InterimPersonId, HeadCoachPersonId),
           HCMatchMethod = if_else(Override, "wiki_interim_override", HCMatchMethod)) |>
    select(franchise_id, season, game_id, game_type, week, gameday, HeadCoachName,
           HeadCoachPersonId, HCMatchMethod, HCSource, HCChangeWindow)
}

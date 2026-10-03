#!/usr/bin/env Rscript
# Source-backed NFL policy indicators. Sourced by 95 before any sample builder.
# Season indicators describe the policy environment, not assigned treatment.
# March 2022 rules were in force for opening staffs but followed most HC hires.
# Results -> policy fields in team, coach-spell and hiring analysis samples.

StaffPolicyRegistry <- read.csv(
  file.path(root, "data", "reference", "nfl_staff_policies.csv"),
  stringsAsFactors = FALSE, na.strings = ""
)
stopifnot(!anyDuplicated(StaffPolicyRegistry$policy_id),
          !anyDuplicated(StaffPolicyRegistry$indicator),
          all(StaffPolicyRegistry$first_full_offseason >=
                StaffPolicyRegistry$first_season),
          all(is.na(StaffPolicyRegistry$last_season) |
                StaffPolicyRegistry$last_season >=
                  StaffPolicyRegistry$first_season))

rooney_era <- function(season) {
  # Calendar cohorts bundle simultaneous provisions. These labels do not
  # identify the effect of any one rule, subsidy or interview requirement.
  ifelse(season < 2003L, "pre_rule",
    ifelse(season < 2021L, "rule_2003",
      ifelse(season < 2022L, "amend_2020",
        ifelse(season < 2025L, "amend_2022", "post_mandate_2025"))))
}

add_rooney_policies <- function(df, season_col = "season",
                                timing = c("opening_staff", "offseason_hire")) {
  timing <- match.arg(timing)
  if (!season_col %in% names(df)) {
    stop(sprintf("Missing policy season column: %s", season_col),
         call. = FALSE)
  }
  seasons <- df[[season_col]]
  if (!is.numeric(seasons) || any(seasons != trunc(seasons), na.rm = TRUE)) {
    stop("Policy seasons must be integer-valued years.", call. = FALSE)
  }
  first_col <- if (timing == "opening_staff") {
    "first_season"
  } else {
    "first_full_offseason"
  }
  for (i in seq_len(nrow(StaffPolicyRegistry))) {
    row <- StaffPolicyRegistry[i, ]
    active <- seasons >= row[[first_col]]
    if (!is.na(row$last_season)) active <- active & seasons <= row$last_season
    df[[row$indicator]] <- as.integer(active)
  }
  df$RooneyEra <- rooney_era(seasons)
  df$PolicyTimingConvention <- timing
  df
}

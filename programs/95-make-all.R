# ============================================================================
# 95-make-all.R
# Master script: builds the analytical samples, summary-statistics tables,
# race-coding agreement statistics, estimation samples, and exhibits for pay,
# roster/staff performance, Rooney policies, and player employment transitions
# from scripts/99_run_all.py's DuckDB (data/datasets/nfl_research.duckdb).
# Run from the project root:  Rscript programs/95-make-all.R
# fwildclusterboot is not on CRAN; install it with
#   install.packages("fwildclusterboot",
#                    repos = c("https://s3alfisc.r-universe.dev", "https://cloud.r-project.org"))
# Date: 2026-09-26; estimation scripts added 2026-10-02
# ============================================================================

pacman::p_load(tidyverse, DBI, duckdb, arrow, data.table, janitor, glue,
               fixest, modelsummary, kableExtra, here, fwildclusterboot, haven)

set.seed(20260926)

# Directory objects
root       <- here::here()
programs   <- file.path(root, "programs")
datasets   <- file.path(root, "data", "datasets")
raw        <- file.path(root, "data", "raw")
hand_coded <- file.path(root, "data", "hand_coded")
analysis   <- file.path(datasets, "analysis")
tables_wd  <- file.path(root, "output", "tables")
figures_wd <- file.path(root, "output", "figures")
paper_tables <- file.path(root, "my_paper", "tables")
db_path    <- file.path(datasets, "nfl_research.duckdb")

for (d in c(analysis, tables_wd, figures_wd, paper_tables)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Shared helpers (DB connection, writers, ggplot theme) and race measures
source(file.path(programs, "00-setup-functions.R"))
source(file.path(programs, "00-race-measures.R"))
source(file.path(programs, "00-analysis-functions.R"))
source(file.path(programs, "00-policy-functions.R"))

# Analytical samples
source(file.path(programs, "01-staff-person-season.R"))
source(file.path(programs, "02-team-season-sample.R"))
source(file.path(programs, "03-team-game-sample.R"))
source(file.path(programs, "04-player-season-sample.R"))
source(file.path(programs, "05-contract-sample.R"))
source(file.path(programs, "06-draft-sample.R"))

# Exhibits and diagnostics
source(file.path(programs, "07-table-summary-statistics.R"))
source(file.path(programs, "08-race-coding-agreement.R"))

# Estimation samples (see notes/analysis-plan.md)
source(file.path(programs, "09-roster-composition-sample.R"))
source(file.path(programs, "10-pay-analysis-sample.R"))
source(file.path(programs, "11-team-analysis-sample.R"))
# New sample builders read saved inputs and run in isolated environments.
source(file.path(programs, "16-coach-policy-sample.R"),
       local = new.env(parent = globalenv()))
source(file.path(programs, "18-player-retention-sample.R"),
       local = new.env(parent = globalenv()))

# Temporal and censoring invariants are checked against the rebuilt data.
source(file.path(programs, "94-verify-analysis.R"),
       local = new.env(parent = globalenv()))

# Estimation exhibits. The primary race measure is hand-coded race when it
# covers at least 80% of a script's sample, else predicted race
# (notes/race-prediction-design.md); NFL_RACE_MEASURE=preddoc|provisional|...
# runs a sensitivity measure (exhibits suffixed -<measure>, output/ only).
source(file.path(programs, "12-table-pay-gap.R"))
source(file.path(programs, "12a-pay-gap-by-year.R"),
       local = new.env(parent = globalenv()))
source(file.path(programs, "13-table-roster-diversity-performance.R"))
source(file.path(programs, "14-table-staff-diversity-performance.R"))
source(file.path(programs, "15-table-race-prediction-validation.R"))
source(file.path(programs, "17-table-rooney-policy.R"),
       local = new.env(parent = globalenv()))
source(file.path(programs, "19-table-player-retention.R"),
       local = new.env(parent = globalenv()))
source(file.path(programs, "20-write-results-memo.R"),
       local = new.env(parent = globalenv()))

# ============================================================================
# 95-make-all.R
# Master script: builds the analytical samples, summary-statistics tables and
# race-coding agreement statistics from the DuckDB written by
# scripts/99_run_all.py (data/datasets/nfl_research.duckdb).
# Run from the project root:  Rscript programs/95-make-all.R
# Date: 2026-09-26
# ============================================================================

pacman::p_load(tidyverse, DBI, duckdb, arrow, data.table, janitor, glue,
               fixest, modelsummary, kableExtra, here)

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

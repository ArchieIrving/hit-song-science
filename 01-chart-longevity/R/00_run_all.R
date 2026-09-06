# R/00_run_all.R ------------------------------------------------------------
# Runs the full analysis pipeline end-to-end from the project root.

# R/paths.R resolves every path from the repository root, so these scripts run
# correctly whatever the working directory is. Raw input is shared with the
# other analysis and lives at data/raw/.


options(tibble.width = Inf, scipen = 999)
source(here::here("01-chart-longevity", "R", "paths.R"), local = TRUE)


run_ts <- Sys.time()
cat("============================================================\n")
cat("Pipeline run:", format(run_ts), "\n")
cat("============================================================\n\n")

source(ap("R/helpers.R"), local = TRUE)
ensure_dirs(c(ap("clean"), ap("outputs/figures"), ap("outputs/descriptives"), ap("outputs/models")))

cat("[1/4] Running data cleaning (R/01_data_cleaning.R)...\n")
source(ap("R/01_data_cleaning.R"), local = TRUE)
cat("[1/4] Done.\n\n")

cat("[2/4] Running EDA (R/02_eda.R)...\n")
source(ap("R/02_eda.R"), local = TRUE)
cat("[2/4] Done.\n\n")

cat("[3/4] Fitting models (R/03_fit_nb_models.R)...\n")
source(ap("R/03_fit_nb_models.R"), local = TRUE)
cat("[3/4] Done.\n\n")

cat("[4/4] Generating reporting outputs (R/04_report_nb_outputs.R)...\n")
source(ap("R/04_report_nb_outputs.R"), local = TRUE)
cat("[4/4] Done.\n\n")

elapsed <- difftime(Sys.time(), run_ts, units = "secs")

cat("============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Elapsed time (seconds): ", round(as.numeric(elapsed), 1), "\n", sep = "")
cat("============================================================\n")

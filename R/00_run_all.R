# 00_run_all.R ------------------------------------------------------------
# Runs the full analysis pipeline end-to-end from the project root.

options(tibble.width = Inf, scipen = 999)

run_ts <- Sys.time()
cat("============================================================\n")
cat("Pipeline run:", format(run_ts), "\n")
cat("============================================================\n\n")

source("R/helpers.R")
ensure_dirs(c("clean", "outputs/figures", "outputs/descriptives", "outputs/models"))

cat("[1/4] Running data cleaning (01_data_cleaning.R)...\n")
source("01_data_cleaning.R")
cat("[1/4] Done.\n\n")

cat("[2/4] Running EDA (02_eda.R)...\n")
source("02_eda.R")
cat("[2/4] Done.\n\n")

cat("[3/4] Fitting models (03_fit_nb_models.R)...\n")
source("03_fit_nb_models.R")
cat("[3/4] Done.\n\n")

cat("[4/4] Generating reporting outputs (04_report_nb_outputs.R)...\n")
source("04_report_nb_outputs.R")
cat("[4/4] Done.\n\n")

elapsed <- difftime(Sys.time(), run_ts, units = "secs")

cat("============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Elapsed time (seconds): ", round(as.numeric(elapsed), 1), "\n", sep = "")
cat("============================================================\n")

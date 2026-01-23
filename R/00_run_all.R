# R/00_run_all.R ------------------------------------------------------------
# Runs the full analysis pipeline end-to-end from the project root.

#All scripts assume the working directory is set to the parent folder of the project root (i.e. the directory containing raw/, clean/, outputs/, and R/).


options(tibble.width = Inf, scipen = 999)

run_ts <- Sys.time()
cat("============================================================\n")
cat("Pipeline run:", format(run_ts), "\n")
cat("============================================================\n\n")

source("R/helpers.R")
ensure_dirs(c("clean", "outputs/figures", "outputs/descriptives", "outputs/models"))

cat("[1/4] Running data cleaning (R/01_data_cleaning.R)...\n")
source("R/01_data_cleaning.R")
cat("[1/4] Done.\n\n")

cat("[2/4] Running EDA (R/02_eda.R)...\n")
source("R/02_eda.R")
cat("[2/4] Done.\n\n")

cat("[3/4] Fitting models (R/03_fit_nb_models.R)...\n")
source("R/03_fit_nb_models.R")
cat("[3/4] Done.\n\n")

cat("[4/4] Generating reporting outputs (R/04_report_nb_outputs.R)...\n")
source("R/04_report_nb_outputs.R")
cat("[4/4] Done.\n\n")

elapsed <- difftime(Sys.time(), run_ts, units = "secs")

cat("============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Elapsed time (seconds): ", round(as.numeric(elapsed), 1), "\n", sep = "")
cat("============================================================\n")

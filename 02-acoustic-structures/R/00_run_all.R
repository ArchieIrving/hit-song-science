# R/00_run_all.R ------------------------------------------------------------
# Runs the full analysis pipeline end-to-end from the project root.

#All scripts assume the working directory is this folder (02-acoustic-structures/), containing clean/, outputs/, and R/.
#Raw input data is shared and lives one level up, at ../data/raw/.


options(tibble.width = Inf, scipen = 999)
source(here::here("02-acoustic-structures", "R", "paths.R"), local = TRUE)


run_ts <- Sys.time()
cat("============================================================\n")
cat("Pipeline run:", format(run_ts), "\n")
cat("============================================================\n\n")

source(ap("R/helpers.R"), local = TRUE)
invisible(lapply(c(ap("clean"), ap("outputs/eda"), ap("outputs/figures")), ensure_dirs))

cat("[1/3] Running data cleaning (R/01_data_cleaning.R)...\n")
source(ap("R/01_data_cleaning.R"), local = TRUE)
cat("[1/3] Done.\n\n")

cat("[2/3] Running EDA (R/02_eda.R)...\n")
source(ap("R/02_eda.R"), local = TRUE)
cat("[2/3] Done.\n\n")

cat("[3/3] Building figures (R/03_build_figures.R)...\n")
source(ap("R/03_build_figures.R"), local = TRUE)
cat("[3/3] Done.\n\n")

elapsed <- difftime(Sys.time(), run_ts, units = "secs")

cat("============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Elapsed time (seconds): ", round(as.numeric(elapsed), 1), "\n", sep = "")
cat("============================================================\n")

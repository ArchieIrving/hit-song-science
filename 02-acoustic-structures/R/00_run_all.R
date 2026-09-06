# R/00_run_all.R ------------------------------------------------------------
# Runs the full analysis pipeline end-to-end from the project root.

#All scripts assume the working directory is this folder (02-acoustic-structures/), containing clean/, outputs/, and R/.
#Raw input data is shared and lives one level up, at ../data/raw/.


options(tibble.width = Inf, scipen = 999)

run_ts <- Sys.time()
cat("============================================================\n")
cat("Pipeline run:", format(run_ts), "\n")
cat("============================================================\n\n")

source("R/helpers.R")
invisible(lapply(c("clean", "outputs/eda", "outputs/figures"), ensure_dir))

cat("[1/3] Running data cleaning (R/01_data_cleaning.R)...\n")
source("R/01_data_cleaning.R")
cat("[1/3] Done.\n\n")

cat("[2/3] Running EDA (R/02_eda.R)...\n")
source("R/02_eda.R")
cat("[2/3] Done.\n\n")

cat("[3/3] Building figures (R/03_build_figures.R)...\n")
source("R/03_build_figures.R")
cat("[3/3] Done.\n\n")

elapsed <- difftime(Sys.time(), run_ts, units = "secs")

cat("============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Elapsed time (seconds): ", round(as.numeric(elapsed), 1), "\n", sep = "")
cat("============================================================\n")

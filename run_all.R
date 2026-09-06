# run_all.R -----------------------------------------------------------------
# Runs both analyses end to end from the repository root.
#
# Open hit-song-science.Rproj, then:
#   source("run_all.R")
#
# Each analysis is self-contained and can also be run on its own:
#   source(here::here("01-chart-longevity", "R", "00_run_all.R"))
#   source(here::here("02-acoustic-structures", "R", "00_run_all.R"))
#
# Both read from the shared data/raw/ directory and write only into their own
# clean/ and outputs/ directories.

started <- Sys.time()

cat("\n############################################################\n")
cat("Hit Song Science: running both analyses\n")
cat("############################################################\n")

source(here::here("01-chart-longevity",     "R", "00_run_all.R"))
source(here::here("02-acoustic-structures", "R", "00_run_all.R"))

cat("\n############################################################\n")
cat("Both analyses complete.\n")
cat("Total elapsed (seconds): ",
    round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), "\n", sep = "")
cat("############################################################\n")

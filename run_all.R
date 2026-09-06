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

# Each analysis runs inside its own environment. Both pipelines define
# constants, helpers and data frames under the same names, so run together in
# one environment they would overwrite each other and leave their objects
# behind afterwards. Every script sources its dependencies with local = TRUE,
# so nested files land in the same environment instead of escaping to the
# global one. The environment is a function frame, which keeps on.exit and
# withr behaving as they do when an analysis is run on its own.
run_analysis <- function(.runner) {
  source(.runner, local = TRUE)
  invisible(NULL)
}

cat("\n############################################################\n")
cat("Hit Song Science: running both analyses\n")
cat("############################################################\n")

run_analysis(here::here("01-chart-longevity",     "R", "00_run_all.R"))
run_analysis(here::here("02-acoustic-structures", "R", "00_run_all.R"))

cat("\n############################################################\n")
cat("Both analyses complete.\n")
cat("Total elapsed (seconds): ",
    round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), "\n", sep = "")
cat("############################################################\n")

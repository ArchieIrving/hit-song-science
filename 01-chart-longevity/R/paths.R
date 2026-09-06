# R/paths.R -----------------------------------------------------------------
# Resolves every path from the repository root, so scripts run correctly
# regardless of the current working directory.

ANALYSIS_DIR <- "01-chart-longevity"

# Path inside this analysis
ap <- function(...) here::here(ANALYSIS_DIR, ...)

# Path inside the shared data directory
dp <- function(...) here::here("data", ...)

# R/03_build_figures.R -------------------------------------------------------
# Purpose:
# - Orchestrate figure generation (no figure code lives here).
# - Load shared inputs once (clustered songs + cluster feature profiles).
# - Define figure-wide constants used across figure scripts.
# - Source individual figure scripts in a fixed order.


suppressPackageStartupMessages({
  library(tidyverse)
  library(ggridges)
  library(ggbeeswarm)
})
source(here::here("02-acoustic-structures", "R", "paths.R"), local = TRUE)


source(ap("R/helpers.R"), local = TRUE)

DIR_FIG <- ap("outputs/figures")
ensure_dirs(DIR_FIG)

# ---- Load shared inputs ---------------------------------------------------

song_df <- readr::read_csv(ap("clean/song_df_clustered.csv"), show_col_types = FALSE)

cluster_profile_df <- read_csv(ap("clean/cluster_feature_profiles.csv"), show_col_types = FALSE)

# ---- Figure-wide constants ------------------------------------------------
# Keep figure scripts free of hard-coded settings.

BASE_SIZE <- 13
DPI_FIG   <- 300

# ---- Run figure scripts ---------------------------------------------------
# Each file should:
# - assume song_df / cluster_profile_df exist
# - build p_XX
# - save to DIR_FIG
# - not read data again

source(ap("R/figures/F1_cluster_profiles.R"), local = TRUE)
source(ap("R/figures/F2_cluster_persistence.R"), local = TRUE)
source(ap("R/figures/F3_cluster_rankings.R"), local = TRUE)
source(ap("R/figures/F4_cluster_duration.R"), local = TRUE)

message("Figures complete. Outputs written to outputs/figures/")

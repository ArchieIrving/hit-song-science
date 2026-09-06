# R/03_build_figures.R -------------------------------------------------------
# Purpose:
# - Orchestrate figure generation (no figure code lives here).
# - Load shared inputs once (clustered data + exported PCA artefacts).
# - Define figure-wide constants used across figure scripts.
# - Source individual figure scripts in a fixed order.


suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(ggridges)
  library(patchwork)
  library(ggbeeswarm)
})
source(here::here("02-acoustic-structures", "R", "paths.R"))


source(ap("R/helpers.R"))

DIR_FIG <- ap("outputs/figures")
ensure_dirs(DIR_FIG)

# ---- Load shared inputs ---------------------------------------------------

song_df <- readr::read_csv(ap("clean/song_df_clustered.csv"), show_col_types = FALSE)


pca_core <- readRDS(ap("clean/pca_core.rds"))

pca_variance <- readr::read_csv(ap("clean/pca_variance.csv"), show_col_types = FALSE)
pca_loadings <- readr::read_csv(ap("clean/pca_loadings.csv"), show_col_types = FALSE)

cluster_profile_df <- read_csv(ap("clean/cluster_feature_profiles.csv"), show_col_types = FALSE)

# ---- Figure-wide constants ------------------------------------------------
# Keep figure scripts free of hard-coded settings.

K_PCS_CLUSTER <- K_PCS_CLUSTER  # clustering space (EDA-locked; helpers.R)
K_PCS_PLOT    <- 2              # default PCA projection (PC1–PC2)

BASE_SIZE <- 13
DPI_FIG   <- 300

CLUSTER_LEVELS_FIG <- CLUSTER_LEVELS

# ---- Run figure scripts ---------------------------------------------------
# Each file should:
# - assume song_df / pca_core / pca_variance / pca_loadings exist
# - build p_XX
# - save to DIR_FIG
# - not read data again

source(ap("R/figures/F1_pca_loadings.R"))
source(ap("R/figures/F2_cluster_persistence.R"))
source(ap("R/figures/F3_cluster_rankings.R"))
source(ap("R/figures/F4_cluster_duration.R"))

message("Figures complete. Outputs written to outputs/figures/")

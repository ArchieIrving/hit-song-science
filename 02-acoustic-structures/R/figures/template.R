# ============================================================================
# Figure XX: <short figure title>
# Purpose:
# - <one line: what it shows>
#
# Inputs (from 03_build_figures.R):
# - song_df, DIR_FIG, BASE_SIZE, DPI_FIG, CLUSTER_LEVELS_FIG
# - (optional) pca_core / pca_variance / pca_loadings
#
# Output:
# - outputs/figures/XX_<slug>.png
# ============================================================================

# ---- Local parameters -----------------------------------------------------

# (Only constants specific to this figure)
# e.g., WIDTH_XX <- 12
#       HEIGHT_XX <- 6

# ---- Data prep ------------------------------------------------------------

# df_XX <- ...

# ---- Plot -----------------------------------------------------------------

# p_XX <- ggplot(...) + ...

# ---- Save -----------------------------------------------------------------

save_fig(
  p_XX,
  file.path(DIR_FIG, "XX_<slug>.png"),
  width  = WIDTH_XX,
  height = HEIGHT_XX,
  dpi    = DPI_FIG
)

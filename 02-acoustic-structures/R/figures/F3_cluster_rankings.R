# ============================================================================
# Figure 03: Median peak chart rank by cluster-year (entry year)
# ============================================================================

# ---- Local parameters ------------------------------------------------------

WIDTH_03  <- 10
HEIGHT_03 <- 6.8

POINT_ALPHA <- 0.95
POINT_SIZE  <- 1.60
SWARM_WIDTH <- 0.35   # horizontal spread within cluster

# ---- Data prep ------------------------------------------------------------

df_03 <- song_df %>%
  filter(!is.na(cluster), !is.na(song_rank_peak), !is.na(entry_year)) %>%
  mutate(
    cluster = factor(cluster, levels = CLUSTER_LEVELS),
    entry_year = as.integer(entry_year),
    
    # Convert reversed rank back to chart rank (1 = best)
    peak_rank_chart = 101 - song_rank_peak
  ) %>%
  group_by(cluster, entry_year) %>%
  summarise(
    peak_rank_med = median(peak_rank_chart),
    .groups = "drop"
  )

# Zoom to observed yearly medians (+ small buffer)
y_min <- min(df_03$peak_rank_med, na.rm = TRUE) - 3
y_max <- max(df_03$peak_rank_med, na.rm = TRUE) + 3

# Per-cluster summary across years (median + IQR of yearly medians)
sum_03 <- df_03 %>%
  group_by(cluster) %>%
  summarise(
    med = median(peak_rank_med),
    q25 = quantile(peak_rank_med, 0.25),
    q75 = quantile(peak_rank_med, 0.75),
    .groups = "drop"
  )

# ---- Plot -----------------------------------------------------------------
# Requires ggbeeswarm

p_03 <- ggplot(df_03, aes(x = cluster, y = peak_rank_med, colour = cluster)) +
  ggbeeswarm::geom_quasirandom(
    width = SWARM_WIDTH,
    groupOnX = TRUE,
    alpha = POINT_ALPHA,
    size  = POINT_SIZE
  ) +
  geom_errorbar(
    data = sum_03,
    aes(x = cluster, ymin = q25, ymax = q75),
    inherit.aes = FALSE,
    width = 0.20,
    linewidth = 0.9,
    colour = "black"
  ) +
  geom_point(
    data = sum_03,
    aes(x = cluster, y = med),
    inherit.aes = FALSE,
    size = 3.0,
    colour = "black"
  ) +
  scale_y_reverse(
    breaks = seq(
      floor(y_min / 10) * 10,
      ceiling(y_max / 10) * 10,
      by = 10
    ),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(y_max, y_min)) +
  scale_colour_cluster() +
  labs(
    title = "Peak chart rank by acoustic structure",
    subtitle = "Points are yearly medians (entry year). Black dot shows the median across years; black bar shows the IQR (25th–75th percentile).",
    colour = "Acoustic cluster",
    x = NULL,
    y = "Peak chart rank (1 = best)"
  ) +
  theme_project(base_size = BASE_SIZE)

# ---- Save -----------------------------------------------------------------

save_fig(
  p_03,
  file.path(DIR_FIG, "03_cluster_rankings.png"),
  width  = WIDTH_03,
  height = HEIGHT_03,
  dpi    = DPI_FIG
)

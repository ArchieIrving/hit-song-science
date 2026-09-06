# R/figures/F2_cluster_persistence.R -----------------------------------------
# ============================================================================
# Figure 02: Cluster prevalence over time (shares by year; line chart)
# Output: 02_cluster_persistence.png
# ============================================================================

# ---- Local parameters ------------------------------------------------------

WIDTH_02  <- 13.5
HEIGHT_02 <- 7.8

YEAR_MIN_02 <- 1964
YEAR_MAX_02 <- 2018

# ---- Data preparation ------------------------------------------------------

ts_df_02 <- song_df %>%
  tidyr::separate_rows(years_on_chart, sep = ";") %>%
  mutate(
    year    = as.integer(years_on_chart),
    cluster = factor(cluster, levels = names(CLUSTER_PAL))
  ) %>%
  filter(year >= YEAR_MIN_02, year <= YEAR_MAX_02) %>%
  count(year, cluster, name = "n_songs") %>%
  group_by(year) %>%
  mutate(share = n_songs / sum(n_songs)) %>%
  ungroup()

# ---- Plot ------------------------------------------------------------------

p_02 <- ggplot(ts_df_02, aes(x = year, y = share, colour = cluster)) +
  geom_line(linewidth = 1.6, lineend = "round") +
  scale_x_continuous(
    breaks = seq(YEAR_MIN_02, YEAR_MAX_02, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.6),                 # cap at 60%
    breaks = seq(0, 0.6, by = 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_colour_manual(
    values = CLUSTER_PAL,
    drop   = FALSE
  ) +
  labs(
    title    = "Cluster prevalence over time",
    subtitle = "Yearly share of charting songs",
    x        = "Year",
    y        = "Share of songs"
  ) +
  theme_project(base_size = BASE_SIZE) +
  theme(
    panel.grid.minor = element_blank(),
    legend.title     = element_blank(),
    legend.position  = "right"
  )

# ---- Save ------------------------------------------------------------------

ggsave(
  filename = file.path(DIR_FIG, "02_cluster_persistence.png"),
  plot     = p_02,
  width    = WIDTH_02,
  height   = HEIGHT_02,
  dpi      = DPI_FIG,
  bg       = "white"
)
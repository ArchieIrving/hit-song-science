# ============================================================================
# Figure 01: Acoustic feature profiles by cluster (TRUE RADAR; facets)
# ============================================================================

# ---- Local parameters ------------------------------------------------------

WIDTH_01  <- 10
HEIGHT_01 <- 8.6

Y_MIN_01    <- -1.0
Y_MAX_01    <-  1.0
Y_BREAKS_01 <- c(-1.0, -0.5, 0, 0.5, 1.0)

feature_order <- c(
  "tempo","danceability","energy","valence",
  "acousticness","instrumentalness","liveness","speechiness"
)

feature_labels <- c(
  tempo            = "Tempo",
  danceability     = "Dance",
  energy           = "Energy",
  valence          = "Valence",
  acousticness     = "Acoustic",
  instrumentalness = "Instrumentalness",
  liveness         = "Live",
  speechiness      = "Speechiness"
)

# ---- Data preparation ------------------------------------------------------

cluster_profile_df <- read_csv(
  "clean/cluster_feature_profiles.csv",
  show_col_types = FALSE
)

profile_df_01 <- cluster_profile_df %>%
  mutate(
    feature         = factor(feature, levels = feature_order),
    idx             = as.integer(feature),
    mean_value_plot = pmin(pmax(mean_z, Y_MIN_01), Y_MAX_01)
  ) %>%
  arrange(cluster, idx)

# ---- True radar geometry ---------------------------------------------------

K_01   <- length(feature_order)
R_MAX  <- Y_MAX_01 - Y_MIN_01              # total radial span
R_RINGS <- (Y_BREAKS_01 - Y_MIN_01)        # include z = 0 ring

profile_xy_01 <- profile_df_01 %>%
  mutate(
    theta = pi/2 - 2*pi*(idx - 1)/K_01,
    r     = mean_value_plot - Y_MIN_01,     # non-negative radius
    x     = r * cos(theta),
    y     = r * sin(theta)
  ) %>%
  arrange(cluster, idx)

# Close each polygon explicitly
path_xy_01 <- profile_xy_01 %>%
  group_by(cluster) %>%
  arrange(idx, .by_group = TRUE) %>%
  dplyr::reframe(
    x = c(x, first(x)),
    y = c(y, first(y))
  )

# Rings (reference circles)
ring_df_01 <- tidyr::expand_grid(
  r = R_RINGS,
  t = seq(0, 2*pi, length.out = 361)
) %>%
  mutate(
    x = r * cos(t),
    y = r * sin(t)
  )

# Spokes + labels
spokes_df_01 <- tibble(
  feature = factor(feature_order, levels = feature_order),
  idx     = seq_along(feature_order)
) %>%
  mutate(
    theta = pi/2 - 2*pi*(idx - 1)/K_01,
    x0    = 0,
    y0    = 0,
    x1    = R_MAX * cos(theta),
    y1    = R_MAX * sin(theta),
    xl    = (R_MAX * 1.14) * cos(theta),
    yl    = (R_MAX * 1.14) * sin(theta),
    lab   = unname(feature_labels[as.character(feature)])
  )

# Canvas limits (protect label space)
LIM <- R_MAX * 1.22

# ---- Plot ------------------------------------------------------------------

p_01 <- ggplot() +
  geom_path(
    data = ring_df_01,
    aes(x, y, group = r),
    colour = "grey82",
    linewidth = 0.75
  ) +
  geom_segment(
    data = spokes_df_01,
    aes(x = x0, y = y0, xend = x1, yend = y1),
    colour = "grey88",
    linewidth = 0.6
  ) +
  geom_polygon(
    data = path_xy_01,
    aes(x, y, group = cluster, fill = cluster),
    alpha = 0.25
  ) +
  scale_fill_cluster() +
  geom_path(
    data = path_xy_01,
    aes(x, y, group = cluster, colour = cluster),
    linewidth = 1.35,
    linejoin = "mitre"
  ) +
  geom_point(
    data = profile_xy_01,
    aes(x, y, colour = cluster),
    size = 2.8
  ) +
  geom_text(
    data = spokes_df_01,
    aes(x = xl, y = yl, label = lab),
    size = 3.2
  ) +
  coord_fixed(
    xlim = c(-LIM, LIM),
    ylim = c(-LIM, LIM)
  ) +
  facet_wrap(~cluster, ncol = 3) +
  scale_colour_cluster() +
  labs(
    title    = "Acoustic feature profiles by cluster",
    subtitle = "Points show average feature levels for each group. Extreme values are capped for readability."
  ) +
  theme_project(base_size = BASE_SIZE) +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.title      = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    strip.text      = element_text(face = "bold", size = 12),
    panel.spacing   = unit(0.3, "lines")
  )

# ---- Save ------------------------------------------------------------------

ggsave(
  filename = file.path(DIR_FIG, "01_cluster_profiles.png"),
  plot     = p_01,
  width    = WIDTH_01,
  height   = HEIGHT_01,
  dpi      = DPI_FIG,
  bg       = "white"
)

# ============================================================================
# Figure 04: Song longevity by cluster (ridgeline; raw scale)
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggridges)
})

source("R/helpers.R")

WIDTH_04  <- 12
HEIGHT_04 <- 7

# ---- Data preparation ------------------------------------------------------

df_04 <- song_df %>%
  mutate(cluster = factor(cluster, levels = CLUSTER_LEVELS)) %>%
  filter(!is.na(cluster), !is.na(weeks_on_chart)) %>%
  filter(is.finite(weeks_on_chart), weeks_on_chart > 0)

x_p99 <- as.numeric(
  quantile(df_04$weeks_on_chart, 0.99, na.rm = TRUE)
)



# ---- Plot ------------------------------------------------------------------

p_04 <- ggplot(df_04, aes(x = weeks_on_chart, y = cluster, fill = cluster)) +
  geom_density_ridges(
    scale = 1,
    alpha = 0.4,
    colour = "white",
    linewidth = 0.25
  ) +
  coord_cartesian(xlim = c(0, x_p99)) +
  scale_fill_cluster() +
  theme_project(base_size = BASE_SIZE) +
  theme(legend.position = "none") +
  labs(
    title = "Acoustic Structure Chart longevity",
    subtitle = "Ridgeline densities of song chart duration (trimmed at 99th percentile)",
    x = "Weeks on chart",
    y = "Acoustic Structure"
  )


# ---- Save ------------------------------------------------------------------

ensure_dir(DIR_FIG)

ggsave(
  filename = file.path(DIR_FIG, "04_cluster_duration.png"),
  plot     = p_04,
  width    = WIDTH_04,
  height   = HEIGHT_04,
  dpi      = DPI_FIG,
  bg       = "white"
)

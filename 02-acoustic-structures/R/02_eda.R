# R/02_eda.R ---------------------------------------------------------------
# EDA:
# - Data integrity checks
# - PCA fitting and diagnostics
# - Cluster selection and sensitivity checks
# - Final k-means solution
# - Cluster summaries for justification and figures

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(cluster)
})

source("R/helpers.R")

DIR_EDA <- "outputs/eda"
DIR_EDA_TABLES <- file.path(DIR_EDA, "tables")

ensure_dir(DIR_EDA)
ensure_dir(DIR_EDA_TABLES)

song_df <- read_csv("clean/song_df.csv", show_col_types = FALSE)

# ---- Local helper: PCA + k-means projection (EDA only) --------------------

make_pca_plot <- function(df, vars, k_clusters, title, out_file) {
  X <- df %>% select(all_of(vars)) %>% as.data.frame()
  pca <- prcomp(X, center = TRUE, scale. = TRUE)
  
  k_pcs <- min(K_PCS_CLUSTER, ncol(pca$x))
  pc_scores <- as.data.frame(pca$x[, 1:k_pcs, drop = FALSE])
  
  km <- run_kmeans(pc_scores, k = k_clusters)
  
  label_map <- derive_cluster_label_map(df %>% mutate(cluster = km$cluster), vars = vars)
  
  plot_df <- as.data.frame(pca$x[, 1:2, drop = FALSE])
  names(plot_df) <- c("PC1", "PC2")
  
  ve <- (pca$sdev^2) / sum(pca$sdev^2)
  
  plot_df$cluster <- km$cluster
  plot_df <- apply_cluster_labels(plot_df, label_map)
  
  p <- ggplot(plot_df, aes(PC1, PC2, colour = cluster)) +
    geom_point(alpha = 0.55, size = 1.1) +
    labs(
      title = title,
      x = sprintf("PC1 (%.1f%%)", 100 * ve[1]),
      y = sprintf("PC2 (%.1f%%)", 100 * ve[2])
    ) +
    scale_colour_cluster() +
    theme_project()
  
  save_fig(p, out_file, width = 9.5, height = 6.5, dpi = 300)
  invisible(p)
}

# ---- Run EDA -------------------------------------------------------------

with_log(file.path(DIR_EDA, "eda_log.txt"), {
  
  log_h1("EDA")
  
  # ------------------------------------------------------------------------
  # 01) Data integrity
  # ------------------------------------------------------------------------
  
  log_h2("01) Data integrity")
  
  eda_01_integrity <- song_df %>%
    summarise(
      n_songs = n(),
      missing_any = sum(!complete.cases(.)),
      min_entry_year = min(entry_year, na.rm = TRUE),
      max_exit_year  = max(exit_year,  na.rm = TRUE),
      min_weeks_on_chart = min(weeks_on_chart, na.rm = TRUE),
      max_weeks_on_chart = max(weeks_on_chart, na.rm = TRUE),
      min_peak_rank = min(song_rank_peak, na.rm = TRUE),
      max_peak_rank = max(song_rank_peak, na.rm = TRUE)
    )
  
  log_table(eda_01_integrity, digits = 0)
  
  stopifnot(anyDuplicated(song_df$song_id) == 0)
  stopifnot(all(song_df$weeks_on_chart >= 1))
  stopifnot(all(song_df$entry_week <= song_df$exit_week))
  stopifnot(all(song_df$entry_year <= song_df$exit_year))
  
  log_h2("01B) Popularity summary")
  
  eda_01b_pop <- song_df %>%
    summarise(
      across(
        all_of(pop_vars),
        list(
          min = ~ min(.x, na.rm = TRUE),
          med = ~ median(.x, na.rm = TRUE),
          p99 = ~ as.numeric(quantile(.x, 0.99, na.rm = TRUE)),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(everything(), names_to = "stat", values_to = "value")
  
  log_table(eda_01b_pop, digits = 3)
  
  # ------------------------------------------------------------------------
  # 02) Core PCA
  # ------------------------------------------------------------------------
  
  log_h2("02) PCA (core features)")
  
  pca_out <- fit_pca_core(song_df, vars = vars_core, k_pcs = K_PCS_CLUSTER)
  
  pca_core   <- pca_out$pca
  ve         <- pca_out$ve
  cum_ve     <- pca_out$cum_ve
  pc_scores  <- pca_out$scores
  
  eda_02_var <- tibble(
    PC = paste0("PC", seq_along(ve)),
    var_explained = ve,
    cum_var = cum_ve
  )
  
  log_table(eda_02_var, digits = 3)
  
  saveRDS(pca_core, "clean/pca_core.rds")
  write_eda_table(eda_02_var, "clean/pca_variance.csv", digits = 6)
  
  loadings_df <- as.data.frame(pca_core$rotation) %>%
    tibble::rownames_to_column("feature") %>%
    pivot_longer(-feature, names_to = "PC", values_to = "loading")
  
  write_eda_table(loadings_df, "clean/pca_loadings.csv", digits = 6)
  
  # ------------------------------------------------------------------------
  # 03) Cluster diagnostics
  # ------------------------------------------------------------------------
  
  log_h2("03) Cluster diagnostics")
  
  elbow_tbl <- tibble(
    k = K_RANGE_DIAG,
    tot_withinss = sapply(K_RANGE_DIAG, function(k) {
      run_kmeans(pc_scores, k = k)$tot.withinss
    })
  )
  
  log_table(elbow_tbl, digits = 2)
  
  set.seed(SEED)
  n_sil <- min(N_SIL_SAMPLE, nrow(pc_scores))
  idx <- sample(seq_len(nrow(pc_scores)), n_sil)
  d_sub <- dist(pc_scores[idx, , drop = FALSE])
  
  sil_tbl <- tibble(
    k = K_RANGE_DIAG,
    avg_sil_width = sapply(K_RANGE_DIAG, function(k) {
      km <- run_kmeans(pc_scores[idx, , drop = FALSE], k = k)
      mean(silhouette(km$cluster, d_sub)[, "sil_width"])
    })
  )
  
  log_table(sil_tbl, digits = 3)
  
  write_eda_table(
    left_join(elbow_tbl, sil_tbl, by = "k"),
    file.path(DIR_EDA_TABLES, "03_cluster_diagnostics_elbow_silhouette.csv"),
    digits = 6
  )
  
  # ------------------------------------------------------------------------
  # 04) PCA sensitivity checks
  # ------------------------------------------------------------------------
  
  log_h2("04) PCA sensitivity")
  
  make_pca_plot(
    song_df, vars_core, K_CLUSTERS_FINAL,
    "PCA clusters (excluding loudness)",
    file.path(DIR_EDA, "eda_pca_clusters_no_loudness.png")
  )
  
  make_pca_plot(
    song_df, c(vars_core, "loudness"), K_CLUSTERS_FINAL,
    "PCA clusters (including loudness)",
    file.path(DIR_EDA, "eda_pca_clusters_with_loudness.png")
  )
  
  make_pca_plot(
    song_df, setdiff(vars_core, "tempo"), K_CLUSTERS_FINAL,
    "PCA clusters (excluding tempo)",
    file.path(DIR_EDA, "eda_pca_no_tempo.png")
  )
  
  # ------------------------------------------------------------------------
  # 05) Final clustering
  # ------------------------------------------------------------------------
  
  log_h2("05) Final cluster solution")
  
  km_final <- run_kmeans(pc_scores, k = K_CLUSTERS_FINAL)
  
  label_map_final <- derive_cluster_label_map(
    song_df %>% mutate(cluster = km_final$cluster),
    vars = vars_core
  )
  
  eda_df <- song_df %>%
    select(all_of(vars_core)) %>%
    mutate(cluster = km_final$cluster) %>%
    apply_cluster_labels(label_map_final)
  
  log_table(count(eda_df, cluster), digits = 0)
  
  # ---- Cluster summaries (EDA justification) ------------------------------
  
  eda_df_scaled <- eda_df %>%
    mutate(across(all_of(vars_core), ~ as.numeric(scale(.x))))
  
  write_eda_table(
    eda_df_scaled %>%
      group_by(cluster) %>%
      summarise(across(all_of(vars_core), mean), .groups = "drop"),
    file.path(DIR_EDA_TABLES, "05b_cluster_profiles_mean_z.csv"),
    digits = 3
  )
  
  write_eda_table(
    eda_df %>%
      group_by(cluster) %>%
      summarise(across(all_of(vars_core), median), .groups = "drop"),
    file.path(DIR_EDA_TABLES, "05c_cluster_profiles_medians.csv"),
    digits = 3
  )
  
  log_table(
    as.data.frame(pc_scores) %>%
      mutate(cluster = km_final$cluster) %>%
      apply_cluster_labels(label_map_final) %>%
      group_by(cluster) %>%
      summarise(across(where(is.numeric), mean), .groups = "drop"),
    digits = 3
  )
  
  assign("km_final", km_final, envir = .GlobalEnv)
  assign("label_map_final", label_map_final, envir = .GlobalEnv)
})

# --------------------------------------------------------------------------
# 06) Materialise clustered data
# --------------------------------------------------------------------------

song_df_clustered <- song_df %>%
  mutate(cluster = km_final$cluster) %>%
  apply_cluster_labels(label_map_final)

write_csv(song_df_clustered, "clean/song_df_clustered.csv")

# --------------------------------------------------------------------------
# 07) Cluster feature profiles (long; figures)
# --------------------------------------------------------------------------

cluster_profile_df <- eda_df_scaled %>%
  pivot_longer(
    cols = all_of(vars_core),
    names_to = "feature",
    values_to = "value_z"
  ) %>%
  group_by(cluster, feature) %>%
  summarise(mean_z = mean(value_z, na.rm = TRUE), .groups = "drop")

write_csv(cluster_profile_df, "clean/cluster_feature_profiles.csv")

message("EDA complete.")

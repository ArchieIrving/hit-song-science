# R/helpers.R ---------------------------------------------------------------
# Shared plot styling, project constants, and small reusable utilities

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(scales)
})

options(
  pillar.min_chars = 6,
  pillar.max_width = Inf,
  pillar.max_extra_cols = Inf
)

# ---- Analysis spec --------------------------------------------------------
# Centralise fixed settings to keep EDA + figure scripts consistent.

SEED <- 123

K_PCS_CLUSTER    <- 5      # number of PCs used for k-means
K_CLUSTERS_FINAL <- 5      # chosen number of clusters (k)
K_RANGE_DIAG     <- 2:10   # k range for elbow/silhouette diagnostics

N_SIL_SAMPLE <- 4000       # silhouette subsample size (prevents dist() O(n^2) hang)

KMEANS_NSTART <- 50        # k-means initialisations (stability)

# ---- Variables ------------------------------------------------------------

vars_core <- c(
  "acousticness",
  "danceability",
  "energy",
  "valence",
  "tempo",
  "speechiness",
  "liveness",
  "instrumentalness"
)

pop_vars <- c("weeks_on_chart", "song_rank_peak", "song_rank_entry", "song_rank_final")

# ---- Cluster labels + colours --------------------------------------------
# Semantic labels applied to k-means clusters (see derive_cluster_label_map).

CLUSTER_LEVELS <- c(
  "Melodic-positive",
  "High-tempo vocal",
  "Speech-dominant",
  "Instrumental-dominant",
  "Mellow acoustic"
)

# Feature used to identify each archetype from a cluster's own profile.
CLUSTER_SIGNATURE_FEATURE <- c(
  "Melodic-positive"      = "valence",
  "High-tempo vocal"      = "tempo",
  "Speech-dominant"       = "speechiness",
  "Instrumental-dominant" = "instrumentalness",
  "Mellow acoustic"       = "acousticness"
)

CLUSTER_PAL <- setNames(
  hue_pal(l = 65, c = 100)(5),
  CLUSTER_LEVELS
)

# Match each cluster's mean feature profile to its closest archetype.
derive_cluster_label_map <- function(df, vars = vars_core) {
  profile <- df %>%
    mutate(across(all_of(vars), ~ as.numeric(scale(.x)))) %>%
    group_by(cluster) %>%
    summarise(across(all_of(vars), mean), .groups = "drop")

  cluster_ids <- as.character(profile$cluster)

  # Skip archetypes whose signature feature isn't in `vars` this run.
  sig_available <- CLUSTER_SIGNATURE_FEATURE[CLUSTER_SIGNATURE_FEATURE %in% vars]
  archetypes    <- names(sig_available)

  label_map <- setNames(character(length(cluster_ids)), cluster_ids)
  remaining_clusters   <- cluster_ids
  remaining_archetypes <- archetypes

  if (length(archetypes) > 0) {
    score <- sapply(archetypes, function(a) profile[[sig_available[[a]]]])
    score <- matrix(score, nrow = length(cluster_ids), dimnames = list(cluster_ids, archetypes))

    while (length(remaining_archetypes) > 0) {
      sub  <- score[remaining_clusters, remaining_archetypes, drop = FALSE]
      best <- which(sub == max(sub), arr.ind = TRUE)[1, , drop = TRUE]
      cl   <- remaining_clusters[best["row"]]
      arc  <- remaining_archetypes[best["col"]]

      label_map[cl] <- arc
      remaining_clusters   <- setdiff(remaining_clusters, cl)
      remaining_archetypes <- setdiff(remaining_archetypes, arc)
    }
  }

  # Fallback: pair any leftover archetypes/clusters in a fixed order.
  leftover_archetypes <- setdiff(names(CLUSTER_SIGNATURE_FEATURE), archetypes)
  leftover_archetypes <- CLUSTER_LEVELS[CLUSTER_LEVELS %in% leftover_archetypes]
  leftover_clusters   <- sort(remaining_clusters)

  if (length(leftover_archetypes) > 0) {
    label_map[leftover_clusters] <- leftover_archetypes[seq_along(leftover_clusters)]
  }

  label_map
}

# label_map must come from derive_cluster_label_map() on this clustering.
apply_cluster_labels <- function(df, label_map) {
  df %>%
    mutate(
      cluster = factor(
        unname(label_map[as.character(cluster)]),
        levels = CLUSTER_LEVELS
      )
    )
}

scale_fill_cluster <- function(...) {
  scale_fill_manual(values = CLUSTER_PAL, breaks = CLUSTER_LEVELS, drop = FALSE, ...)
}

scale_colour_cluster <- function(...) {
  scale_colour_manual(values = CLUSTER_PAL, breaks = CLUSTER_LEVELS, drop = FALSE, ...)
}

# ---- PCA helper: variance explained labels --------------------------------

pca_ve_labels <- function(ve, k = 5) {
  k <- min(k, length(ve))
  cum <- cumsum(ve)
  
  tibble(
    PC = paste0("PC", 1:k),
    ve = ve[1:k],
    cum = cum[1:k],
    label = sprintf("PC%d: %.1f%% (cum %.1f%%)", 1:k, 100 * ve[1:k], 100 * cum[1:k])
  )
}

# ---- PCA helper: fit + extract core outputs -------------------------------
# Pure helper (no IO). Useful to keep PCA outputs consistent across scripts.

fit_pca_core <- function(df, vars = vars_core, k_pcs = K_PCS_CLUSTER) {
  X <- df %>% dplyr::select(all_of(vars)) %>% as.data.frame()
  
  pca <- prcomp(X, center = TRUE, scale. = TRUE)
  
  ve <- (pca$sdev^2) / sum(pca$sdev^2)
  cum_ve <- cumsum(ve)
  
  k_pcs <- min(k_pcs, ncol(pca$x))
  
  scores <- as.data.frame(pca$x[, 1:k_pcs, drop = FALSE])
  colnames(scores) <- paste0("PC", seq_len(k_pcs))
  
  list(
    pca = pca,
    ve = ve,
    cum_ve = cum_ve,
    scores = scores
  )
}

# ---- k-means helper -------------------------------------------------------
# Ensures deterministic clustering runs across scripts.

# MacQueen avoids Hartigan-Wong's step-cap warning on large n.
run_kmeans <- function(X, k, nstart = KMEANS_NSTART, seed = SEED, iter.max = 100) {
  set.seed(seed)
  kmeans(X, centers = k, nstart = nstart, iter.max = iter.max, algorithm = "MacQueen")
}

# ---- Feature label helpers ------------------------------------------------

feature_labels_core <- c(
  acousticness     = "Acousticness",
  danceability     = "Danceability",
  energy           = "Energy",
  instrumentalness = "Instrumentalness",
  liveness         = "Liveness",
  speechiness      = "Speechiness",
  valence          = "Valence",
  tempo            = "Tempo"
)

label_features_core <- function(x) {
  unname(feature_labels_core[x])
}

# ---- Shared utilities -----------------------------------------------------

source(here::here("R", "common", "utils.R"))

# ---- Output logging helper -----------------------------------------------

# Progress meters report machine-dependent throughput. Inside a sink they are
# captured into the log file, so the log would differ between runs on identical
# data. Suppressed here and restored when the log is closed.
with_log <- function(file, expr) {
  ensure_dirs(dirname(file))
  old_opts <- options(readr.show_progress = FALSE, vroom.show_progress = FALSE)
  sink(file, type = "output")
  on.exit({
    sink(type = "output")
    options(old_opts)
  }, add = TRUE)
  force(expr)
}

# ---- Log formatting helpers ----------------------------------------------

log_h1 <- function(text) {
  cat(
    "\n\n",
    strrep("=", 80), "\n",
    text, "\n",
    strrep("=", 80), "\n",
    sep = ""
  )
}

log_h2 <- function(text) {
  cat(
    "\n",
    text, "\n",
    strrep("-", nchar(text)), "\n",
    sep = ""
  )
}

# ---- Log table helper -----------------------------------------------------

log_table <- function(x, digits = 3) {
  x <- x |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, digits))
    )
  
  print(
    x,
    n = Inf,
    width = Inf
  )
}

# ---- EDA table export helper ---------------------------------------------

write_eda_table <- function(x, filename, digits = 3) {
  ensure_dirs(dirname(filename))
  x |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, digits))
    ) |>
    readr::write_csv(filename)
}

# ---- Theme ----------------------------------------------------------------

theme_acoustic <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# ---- Save helper ----------------------------------------------------------

save_fig <- function(plot, filename, width = 9.5, height = 6.5, dpi = 300) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  
  ggsave(
    filename = filename,
    plot = plot + theme_acoustic(),
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

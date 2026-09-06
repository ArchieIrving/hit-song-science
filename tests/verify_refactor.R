# tests/verify_refactor.R ---------------------------------------------------
# Checks that a pipeline run still reproduces the same analysis.
#
# Usage, after running run_all.R:
#   source("tests/verify_refactor.R")
#
# Compares sentinel values scraped from the current outputs against
# tests/expected_results.csv, which records the pre-refactor baseline.
# Refactors should change plumbing, not results: every check must pass.
# Any failure raises an error, so a failed run cannot be mistaken for a pass.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

expected <- read_csv(here::here("tests", "expected_results.csv"), show_col_types = FALSE)

a1 <- here::here("01-chart-longevity")
a2 <- here::here("02-acoustic-structures")

model_summary <- readLines(file.path(a1, "outputs/models/model_summary.txt"), warn = FALSE)
eda_summary   <- readLines(file.path(a1, "outputs/descriptives/eda_summary.txt"), warn = FALSE)
profiles      <- read_csv(file.path(a2, "clean/cluster_feature_profiles.csv"), show_col_types = FALSE)
pca_var       <- read_csv(file.path(a2, "clean/pca_variance.csv"), show_col_types = FALSE)
clustered     <- read_csv(file.path(a2, "clean/song_df_clustered.csv"), show_col_types = FALSE)

# ---- scrapers -------------------------------------------------------------

num_after <- function(lines, pattern) {
  hit <- grep(pattern, lines, value = TRUE)[1]
  if (is.na(hit)) return(NA_real_)
  as.numeric(sub(".*?(-?[0-9]+\\.?[0-9]*).*", "\\1", sub(pattern, "", hit)))
}

irr_of <- function(term) {
  hit <- grep(paste0("^", term, " "), model_summary, value = TRUE)[1]
  if (is.na(hit)) return(NA_real_)
  as.numeric(strsplit(trimws(hit), " +")[[1]][4])
}

vif_of <- function(term) {
  hit <- grep(paste0("^ *", term, " +[0-9]"), model_summary, value = TRUE)[1]
  if (is.na(hit)) return(NA_real_)
  as.numeric(strsplit(trimws(hit), " +")[[1]][2])
}

# The model-comparison block is "<label>  <AIC>  <BIC>". Labels contain spaces
# and regex metacharacters ("NB main (frozen)", "NB + categoricals"), so match
# the label literally and take the first decimal number on the line as the AIC,
# rather than relying on a fixed whitespace-field position.
aic_of <- function(label) {
  hit <- model_summary[startsWith(model_summary, label)][1]
  if (is.na(hit)) return(NA_real_)
  nums <- as.numeric(regmatches(hit, gregexpr("[0-9]+\\.[0-9]+", hit))[[1]])
  if (length(nums) == 0) NA_real_ else nums[1]
}

sig_z <- function(cluster_name, feature_name) {
  profiles |>
    filter(cluster == cluster_name, feature == feature_name) |>
    pull(mean_z) |>
    first()
}

cluster_size <- function(cluster_name) sum(clustered$cluster == cluster_name)

# ---- observed values ------------------------------------------------------

observed <- c(
  a1_n                          = num_after(eda_summary, "N songs: "),
  a1_outcome_mean               = num_after(eda_summary, "Outcome mean: "),
  a1_outcome_variance           = num_after(eda_summary, "Outcome variance: "),
  a1_poisson_dispersion         = num_after(model_summary, "statistic = "),
  a1_aic_poisson                = aic_of("Poisson"),
  a1_aic_nb_main                = aic_of("NB main"),
  a1_irr_entry_score            = irr_of("song_rank_entry"),
  a1_irr_prior_longevity        = irr_of("artist_avg_longevity_prior"),
  a1_irr_prior_peak_score       = irr_of("artist_avg_peak_rank_prior"),
  a1_irr_prior_appearances      = irr_of("artist_prior_n_songs"),
  a1_irr_danceability           = irr_of("danceability"),
  a1_irr_speechiness            = irr_of("speechiness"),
  a1_vif_prior_longevity        = vif_of("artist_avg_longevity_prior"),
  a2_n                          = nrow(clustered),
  a2_k                          = dplyr::n_distinct(profiles$cluster),
  a2_pc1_var_explained          = pca_var$var_explained[1],
  a2_pc5_cum_var                = pca_var$cum_var[5],
  a2_z_instrumental_dominant    = sig_z("Instrumental-dominant", "instrumentalness"),
  a2_z_speech_dominant          = sig_z("Speech-dominant", "speechiness"),
  a2_z_mellow_acoustic          = sig_z("Mellow acoustic", "acousticness"),
  a2_z_high_tempo_vocal         = sig_z("High-tempo vocal", "tempo"),
  a2_z_melodic_positive         = sig_z("Melodic-positive", "valence"),
  a2_size_melodic_positive      = cluster_size("Melodic-positive"),
  a2_size_mellow_acoustic       = cluster_size("Mellow acoustic"),
  a2_size_high_tempo_vocal      = cluster_size("High-tempo vocal"),
  a2_size_speech_dominant       = cluster_size("Speech-dominant"),
  a2_size_instrumental_dominant = cluster_size("Instrumental-dominant")
)

# ---- compare --------------------------------------------------------------

results <- expected |>
  mutate(
    observed = unname(observed[metric]),
    diff     = observed - value,
    pass     = !is.na(observed) & abs(diff) <= tolerance
  )

cat("\n============================================================\n")
cat("Refactor verification\n")
cat("============================================================\n\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf(
    "  %-32s %-12s %-12s %s\n",
    r$metric,
    format(r$value),
    if (is.na(r$observed)) "NOT FOUND" else format(r$observed),
    if (isTRUE(r$pass)) "ok" else "**FAIL**"
  ))
}

n_fail <- sum(!results$pass)

# On failure, name the artefact each value was read from. A failure means the
# observed value, the way it was extracted, and the recorded baseline no longer
# agree; it does not on its own establish which of the three is wrong. The
# source path is the fastest way to tell.
if (n_fail > 0) {
  cat("\n------------------------------------------------------------\n")
  cat("Failure detail\n")
  cat("------------------------------------------------------------\n")
  for (i in which(!results$pass)) {
    r <- results[i, ]
    cat("\n  ", r$metric, "\n", sep = "")
    cat("    expected:  ", format(r$value), "\n", sep = "")
    cat("    observed:  ", if (is.na(r$observed)) "NOT FOUND" else format(r$observed), "\n", sep = "")
    cat("    tolerance: ", format(r$tolerance), "\n", sep = "")
    cat("    source:    ", r$source, "\n", sep = "")
  }
}

cat("\n------------------------------------------------------------\n")
if (n_fail > 0) {
  cat("Verification FAILED: ", n_fail, " of ", nrow(results), " checks did not match.\n", sep = "")
  cat("------------------------------------------------------------\n")
  stop(
    "Verification failed for: ",
    paste(results$metric[!results$pass], collapse = ", "),
    ". Compare each value against the source artefact listed above before ",
    "assuming the analysis itself changed.",
    call. = FALSE
  )
}

cat("All ", nrow(results), " checks passed. The analysis is unchanged.\n", sep = "")
cat("------------------------------------------------------------\n")

invisible(results)

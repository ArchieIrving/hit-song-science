# R/analysis_setup.R ------------------------------------------------------
# Analysis definitions, variable sets, and labelling conventions.

suppressPackageStartupMessages({
  library(dplyr)
  library(withr)
  library(stringr)
  library(tibble)
})

withr::local_options(list(tibble.width = Inf, scipen = 999))

# ======================================================================
# Core constants
# ======================================================================

OUTCOME_VAR <- "weeks_on_chart"

# ======================================================================
# Variable sets (match clean/song_df.csv)
# ======================================================================

core_vars <- c(
  "song_rank_entry",
  "n_artists_collab",
  "artist_prior_n_songs",
  "artist_avg_longevity_prior",
  "artist_avg_peak_rank_prior"
)

audio_vars <- c(
  "duration_ms",
  "acousticness", "danceability", "energy", "instrumentalness",
  "liveness", "loudness", "speechiness", "valence", "tempo"
)

cat_vars <- c(
  "key", "mode", "time_signature", "song_type"
)

# ======================================================================
# Labels
# ======================================================================

NICE_NAMES <- c(
  song_rank_entry            = "Chart entry position",
  n_artists_collab           = "Number of collaborating artists",
  artist_prior_n_songs       = "Prior chart appearances",
  artist_avg_longevity_prior = "Average chart longevity",
  artist_avg_peak_rank_prior = "Average peak chart position",
  duration_ms                = "Duration (ms)",
  loudness                   = "Loudness (dB)",
  tempo                      = "Tempo (BPM)",
  acousticness               = "Acousticness",
  danceability               = "Danceability",
  energy                     = "Energy",
  instrumentalness           = "Instrumentalness",
  liveness                   = "Liveness",
  speechiness                = "Speechiness",
  valence                    = "Valence",
  key                        = "Key",
  mode                       = "Mode",
  time_signature             = "Time signature",
  song_type                  = "Song type",
  weeks_on_chart             = "Weeks on chart"
)

label_from_map <- function(x, mapping = NICE_NAMES) {
  x_chr <- as.character(x)
  dplyr::recode(x_chr, !!!as.list(mapping), .default = x_chr)
}

named_labels <- function(vars, mapping = NICE_NAMES) {
  vars <- as.character(vars)
  setNames(label_from_map(vars, mapping), vars)
}

# ======================================================================
# Encoding
# ======================================================================

encode_factors <- function(df) {
  stopifnot(is.data.frame(df))
  stopifnot(all(cat_vars %in% names(df)))
  
  df |>
    dplyr::mutate(
      key = dplyr::na_if(key, -1),
      key = factor(key),
      
      mode = factor(mode, levels = c(0, 1), labels = c("minor", "major")),
      mode = stats::relevel(mode, ref = "minor"),
      
      song_type = factor(song_type, levels = c("Solo", "Collaboration")),
      song_type = stats::relevel(song_type, ref = "Solo"),
      
      time_signature = as.character(time_signature),
      time_signature = dplyr::if_else(
        time_signature %in% c("3", "4"),
        time_signature,
        "Other"
      ),
      time_signature = factor(time_signature, levels = c("4", "3", "Other"))
    )
}

# ======================================================================
# Scaling
# ======================================================================

standardise_predictors <- function(df, core_vars, audio_vars, exclude = character()) {
  scale_vars <- intersect(c(core_vars, audio_vars), names(df))
  scale_vars <- setdiff(scale_vars, exclude)
  
  df_scaled <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(scale_vars), ~ as.vector(scale(.x)))
    )
  
  list(
    df_scaled = df_scaled,
    scale_vars = scale_vars
  )
}

# ======================================================================
# Effect-size increments
# ======================================================================

INC_MAP <- tibble::tibble(
  term = c(
    "song_rank_entry",
    "artist_prior_n_songs",
    "artist_avg_longevity_prior",
    "artist_avg_peak_rank_prior",
    "duration_ms",
    "tempo",
    "loudness",
    "acousticness",
    "danceability",
    "energy",
    "instrumentalness",
    "liveness",
    "speechiness",
    "valence"
  ),
  inc = c(
    10, 5, 5, 10,
    10000, 10, 5,
    0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10
  )
)

label_increment_suffix <- function(term) {
  dplyr::case_when(
    term == "song_rank_entry"            ~ " (per 10 chart positions)",
    term == "artist_prior_n_songs"       ~ " (per 5 appearances)",
    term == "artist_avg_longevity_prior" ~ " (per 5 weeks)",
    term == "artist_avg_peak_rank_prior" ~ " (per 10 chart positions)",
    term %in% c(
      "acousticness", "danceability", "energy", "instrumentalness",
      "liveness", "speechiness", "valence"
    )                                   ~ " (per 0.1)",
    term == "tempo"                     ~ " (per 10 BPM)",
    term == "loudness"                  ~ " (per 5 dB)",
    term == "duration_ms"               ~ " (per 10s)",
    TRUE                                ~ ""
  )
}

# ======================================================================
# Model-term labelling
# ======================================================================

label_model_term <- function(term) {
  term_clean <- as.character(term)[1]
  term_clean <- stringr::str_replace_all(term_clean, "`", "")
  
  if (term_clean %in% names(NICE_NAMES)) {
    return(label_from_map(term_clean))
  }
  
  if (stringr::str_starts(term_clean, "key")) {
    return(paste0("Key: ", stringr::str_remove(term_clean, "^key")))
  }
  if (stringr::str_starts(term_clean, "mode")) {
    return(paste0("Mode: ", stringr::str_remove(term_clean, "^mode")))
  }
  if (stringr::str_starts(term_clean, "time_signature")) {
    return(paste0("Time signature: ", stringr::str_remove(term_clean, "^time_signature")))
  }
  if (stringr::str_starts(term_clean, "song_type")) {
    return(paste0("Song type: ", stringr::str_remove(term_clean, "^song_type")))
  }
  
  stringr::str_replace_all(term_clean, "_", " ")
}

label_model_term_with_increment <- function(term) {
  base <- label_model_term(term)
  paste0(base, label_increment_suffix(as.character(term)))
}

# ======================================================================
# Formula builder
# ======================================================================

build_nb_formula <- function() {
  rhs <- c(core_vars, audio_vars, cat_vars)
  stats::as.formula(
    paste0(OUTCOME_VAR, " ~ ", paste(rhs, collapse = " + "))
  )
}

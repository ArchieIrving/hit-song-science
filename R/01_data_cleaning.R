# R/01_data_cleaning.R ------------------------------------------------------
# Construct a song-level dataset for modelling chart longevity.
# Output: clean/song_df.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
})

source("R/helpers.R")
source("R/analysis_setup.R")
ensure_dirs("clean")

# ----------------------------------------------------------------------
# Load inputs
# ----------------------------------------------------------------------

acoustic_features <- readr::read_tsv("raw/features/acoustic_features.csv", show_col_types = FALSE)
song_chart        <- readr::read_tsv("raw/popularity/song_chart.csv", show_col_types = FALSE)
songs             <- readr::read_tsv("raw/metadata/songs.csv", show_col_types = FALSE)

stopifnot(anyDuplicated(acoustic_features$song_id) == 0)

# ----------------------------------------------------------------------
# Exclude songs observed only part-way through a chart run
# ----------------------------------------------------------------------

valid_song_ids <- song_chart |>
  transmute(
    song_id,
    week,
    weeks_on_chart = as.integer(weeks_on_chart)
  ) |>
  group_by(song_id) |>
  summarise(
    entry_week = min(week, na.rm = TRUE),
    entry_weeks_on_chart = weeks_on_chart[which.min(week)],
    .groups = "drop"
  ) |>
  filter(!is.na(entry_weeks_on_chart), entry_weeks_on_chart %in% c(0L, 1L)) |>
  pull(song_id)

song_chart <- song_chart |>
  filter(song_id %in% valid_song_ids)

songs <- songs |>
  filter(song_id %in% valid_song_ids)

acoustic_features <- acoustic_features |>
  filter(song_id %in% valid_song_ids)

# ----------------------------------------------------------------------
# Parse credited artists from songs metadata
# ----------------------------------------------------------------------

parse_artists_dict <- function(song_id_vec, artists_str_vec) {
  purrr::map2_dfr(song_id_vec, artists_str_vec, function(sid, s) {
    if (is.na(s) || !nzchar(s)) {
      return(tibble(song_id = sid, artist_id = NA_character_))
    }
    
    m <- stringr::str_match_all(
      s,
      "'([A-Za-z0-9]{22})'\\s*:\\s*'[^']*'"
    )[[1]]
    
    if (nrow(m) == 0) {
      return(tibble(song_id = sid, artist_id = NA_character_))
    }
    
    tibble(song_id = sid, artist_id = m[, 2])
  })
}

songs_artists_long <- parse_artists_dict(songs$song_id, songs$artists) |>
  filter(!is.na(artist_id)) |>
  distinct(song_id, artist_id)

artist_lists <- songs_artists_long |>
  group_by(song_id) |>
  summarise(
    n_artists = n_distinct(artist_id),
    artist_id = paste(unique(artist_id), collapse = ";"),
    .groups = "drop"
  ) |>
  mutate(
    n_artists_collab = pmax(n_artists - 1L, 0L)
  )

# ----------------------------------------------------------------------
# Song-level chart summary statistics
# ----------------------------------------------------------------------

song_chart_summary <- song_chart |>
  transmute(
    song_id,
    week,
    weeks_on_chart = if_else(weeks_on_chart == 0, 1L, as.integer(weeks_on_chart)),
    rank_score
  ) |>
  group_by(song_id) |>
  summarise(
    song_entry_week = min(week, na.rm = TRUE),
    song_longevity  = max(weeks_on_chart, na.rm = TRUE),
    song_rank_peak  = max(rank_score, na.rm = TRUE),
    .groups = "drop"
  )

song_rank_entry <- song_chart |>
  transmute(
    song_id,
    week,
    rank_score
  ) |>
  left_join(song_chart_summary |> dplyr::select(song_id, song_entry_week), by = "song_id") |>
  filter(week == song_entry_week) |>
  group_by(song_id) |>
  summarise(
    song_rank_entry = max(rank_score, na.rm = TRUE),
    .groups = "drop"
  )

song_chart_summary <- song_chart_summary |>
  left_join(song_rank_entry, by = "song_id") |>
  mutate(weeks_on_chart = song_longevity) |>
  dplyr::select(song_id, song_entry_week, weeks_on_chart, song_rank_peak, song_rank_entry)

# ----------------------------------------------------------------------
# Artist prior-chart history (restricted to prior songs only)
# ----------------------------------------------------------------------

artist_song <- songs_artists_long |>
  left_join(song_chart_summary, by = "song_id") |>
  arrange(artist_id, song_entry_week, song_id)

artist_song_prior <- artist_song |>
  group_by(artist_id) |>
  mutate(
    prior_n_songs = row_number() - 1L,
    
    cum_longevity = cumsum(weeks_on_chart),
    prior_sum_longevity = lag(cum_longevity),
    artist_avg_longevity_prior_artist =
      if_else(prior_n_songs > 0,
              as.numeric(prior_sum_longevity / prior_n_songs),
              0),
    
    cum_peak_rank = cumsum(song_rank_peak),
    prior_sum_peak_rank = lag(cum_peak_rank),
    artist_avg_peak_rank_prior_artist =
      if_else(prior_n_songs > 0,
              as.numeric(prior_sum_peak_rank / prior_n_songs),
              0)
  ) |>
  ungroup() |>
  dplyr::select(
    song_id,
    artist_id,
    prior_n_songs,
    artist_avg_longevity_prior_artist,
    artist_avg_peak_rank_prior_artist
  )

song_artist_history <- artist_song_prior |>
  group_by(song_id) |>
  summarise(
    artist_prior_n_songs =
      if (all(is.na(prior_n_songs))) 0 else mean(prior_n_songs, na.rm = TRUE),
    
    artist_avg_longevity_prior =
      if (all(is.na(artist_avg_longevity_prior_artist))) 0 else mean(artist_avg_longevity_prior_artist, na.rm = TRUE),
    
    artist_avg_peak_rank_prior =
      if (all(is.na(artist_avg_peak_rank_prior_artist))) 0 else mean(artist_avg_peak_rank_prior_artist, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  mutate(
    artist_avg_longevity_prior  = round(artist_avg_longevity_prior, 2),
    artist_avg_peak_rank_prior  = round(artist_avg_peak_rank_prior, 2)
  )

# ----------------------------------------------------------------------
# Assemble final song-level dataset
# ----------------------------------------------------------------------

song_df <- songs |>
  dplyr::select(song_id, song_name, song_type) |>
  left_join(song_chart_summary |> dplyr::select(song_id, weeks_on_chart, song_rank_entry), by = "song_id") |>
  left_join(artist_lists |> dplyr::select(song_id, artist_id, n_artists_collab), by = "song_id") |>
  left_join(
    song_artist_history |>
      dplyr::select(
        song_id,
        artist_prior_n_songs,
        artist_avg_longevity_prior,
        artist_avg_peak_rank_prior
      ),
    by = "song_id"
  ) |>
  left_join(
    acoustic_features |>
      dplyr::select(
        song_id,
        duration_ms, key, mode, time_signature,
        acousticness, danceability, energy, instrumentalness,
        liveness, loudness, speechiness, valence, tempo
      ),
    by = "song_id"
  ) |>
  mutate(
    tempo = na_if(tempo, 0),
    
    # Set missing artist-history values to zero
    artist_prior_n_songs       = if_else(is.na(artist_prior_n_songs), 0, artist_prior_n_songs),
    artist_avg_longevity_prior = if_else(is.na(artist_avg_longevity_prior), 0, artist_avg_longevity_prior),
    artist_avg_peak_rank_prior = if_else(is.na(artist_avg_peak_rank_prior), 0, artist_avg_peak_rank_prior)
  ) |>
  dplyr::select(
    song_id, song_name,
    song_type,
    weeks_on_chart,
    song_rank_entry,
    artist_id, n_artists_collab,
    artist_prior_n_songs,
    artist_avg_longevity_prior, artist_avg_peak_rank_prior,
    duration_ms, key, mode, time_signature,
    acousticness, danceability, energy, instrumentalness,
    liveness, loudness, speechiness, valence, tempo
  )

# Drop observations with missing values in modelling variables
model_cols <- c(OUTCOME_VAR, core_vars, audio_vars, cat_vars)

song_df <- song_df |>
  tidyr::drop_na(dplyr::all_of(model_cols))

# ----------------------------------------------------------------------
# Save output
# ----------------------------------------------------------------------

stopifnot(all(!is.na(song_df$weeks_on_chart)))
stopifnot(all(song_df$weeks_on_chart >= 1))
stopifnot(all(abs(song_df$weeks_on_chart - round(song_df$weeks_on_chart)) < 1e-8))
stopifnot(anyDuplicated(song_df$song_id) == 0)

write_csv(song_df, "clean/song_df.csv")
message("Data cleaning complete. Output saved to clean/song_df.csv")

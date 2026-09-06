# R/01_data_cleaning.R ------------------------------------------------------
# Song-level dataset for clustering + popularity/persistence analyses.
# Output: clean/song_df.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source(here::here("02-acoustic-structures", "R", "paths.R"))


source(ap("R/helpers.R"))
ensure_dirs(ap("clean"))

# ---- Load inputs ----------------------------------------------------------

acoustic_features <- read_tsv(dp("raw/features/acoustic_features.csv"), show_col_types = FALSE)
song_chart        <- read_tsv(dp("raw/popularity/song_chart.csv"), show_col_types = FALSE)
songs             <- read_tsv(dp("raw/metadata/songs.csv"), show_col_types = FALSE)

# ---- Prepare chart table (recode + derive year) ---------------------------

song_chart_recoded <- song_chart |>
  transmute(
    song_id,
    week = as.Date(week),  # format like "1989-09-09"
    year = as.integer(format(as.Date(week), "%Y")),
    weeks_on_chart = if_else(weeks_on_chart == 0, 1L, as.integer(weeks_on_chart)),
    rank_score
  )

# ---- Restrict to songs observed from chart entry --------------------------
# Keep songs whose first observed weeks_on_chart is 1 (avoids left-truncated runs)

song_chart_recoded <- song_chart_recoded |>
  group_by(song_id) |>
  filter(min(weeks_on_chart, na.rm = TRUE) == 1L) |>
  ungroup()

# ---- Compact song-year presence info (for Figure 2) -----------------------

song_year_info <- song_chart_recoded |>
  distinct(song_id, year) |>
  group_by(song_id) |>
  summarise(
    entry_year = min(year, na.rm = TRUE),
    exit_year  = max(year, na.rm = TRUE),
    n_years_on_chart = n(),
    years_on_chart = paste(sort(unique(year)), collapse = ";"),
    .groups = "drop"
  )

# ---- Song-level chart summary --------------------------------------------

song_df <- song_chart_recoded |>
  group_by(song_id) |>
  arrange(weeks_on_chart, week) |>
  mutate(
    entry_week      = first(week),
    exit_week       = last(week),
    song_rank_entry = first(rank_score),
    song_rank_peak  = max(rank_score, na.rm = TRUE)
  ) |>
  slice_tail(n = 1) |>
  ungroup() |>
  rename(song_rank_final = rank_score) |>
  left_join(
    songs |>
      dplyr::select(song_id, song_name, song_type, explicit),
    by = "song_id"
  ) |>
  left_join(
    acoustic_features |>
      dplyr::select(
        song_id, duration_ms,
        acousticness, danceability, energy, instrumentalness,
        liveness, loudness, speechiness, valence, tempo
      ) |>
      filter(tempo != 0),
    by = "song_id"
  ) |>
  left_join(
    song_year_info,
    by = "song_id"
  ) |>
  dplyr::select(
    song_id, song_name, song_type, explicit,
    entry_year, exit_year, n_years_on_chart, years_on_chart,
    weeks_on_chart,
    song_rank_entry, song_rank_peak, song_rank_final,
    entry_week, exit_week,
    duration_ms,
    acousticness, danceability, energy, instrumentalness,
    liveness, loudness, speechiness, valence, tempo
  ) |>
  filter(if_all(everything(), ~ !is.na(.)))

write_csv(song_df, ap("clean/song_df.csv"))
message("Data cleaning complete: clean/song_df.csv")
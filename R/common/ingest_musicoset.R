# R/common/ingest_musicoset.R -----------------------------------------------
# Reads the MusicOSet source files and returns them at their natural grains.
#
# This layer reads and validates. It does not decide anything analytical:
# chart-entry rules, tempo filtering, complete-case exclusions, artist-history
# construction and yearly presence all stay inside the analysis that needs
# them, because the two analyses retain analysis-specific preparation rules and
# output schemas.
#
# Reading is separated from materialisation. read_musicoset() is a pure
# reader; write_processed_tables() is an explicit side effect producing
# inspectable generated base tables that are never read back as pipeline
# inputs.
#
# Calls are namespace-qualified throughout: analysis 1 attaches MASS, which
# masks dplyr::select, and shared infrastructure must not depend on whatever
# happens to be on the search path.

source(here::here("R", "common", "utils.R"), local = TRUE)

# ---- File reading ---------------------------------------------------------

# MusicOSet ships tab-delimited files with a .csv extension. That quirk is
# recorded here, once, rather than in each analysis.
read_musicoset_file <- function(path) {
  if (!file.exists(path)) {
    stop("MusicOSet file not found: ", path, call. = FALSE)
  }
  readr::read_tsv(path, show_col_types = FALSE)
}

# ---- Validation -----------------------------------------------------------

require_columns <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(what, " is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

require_unique_key <- function(df, key, what) {
  if (anyDuplicated(df[[key]]) != 0) {
    stop(what, " has duplicate values in key column '", key, "'.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Artist parsing -------------------------------------------------------

# The metadata table stores credited artists as a Python-dict-shaped string.
# Extracting the Spotify IDs is source-format parsing, so it belongs here.
# Collaboration counts and artist history are analytical and stay in A1.
parse_song_artists <- function(songs) {
  purrr::map2_dfr(songs$song_id, songs$artists, function(sid, s) {
    if (is.na(s) || !nzchar(s)) {
      return(tibble::tibble(song_id = sid, artist_id = NA_character_))
    }

    m <- stringr::str_match_all(s, "'([A-Za-z0-9]{22})'\\s*:")[[1]]

    if (nrow(m) == 0) {
      return(tibble::tibble(song_id = sid, artist_id = NA_character_))
    }

    tibble::tibble(song_id = sid, artist_id = m[, 2])
  }) |>
    dplyr::filter(!is.na(artist_id)) |>
    dplyr::distinct(song_id, artist_id)
}

# ---- Reader ---------------------------------------------------------------

# Returns the source tables at their natural grains, in source row order:
#
#   songs             one row per song, metadata
#   acoustic_features one row per song, Spotify audio features
#   chart_weekly      one row per song and chart week
#   song_artists      one row per song and credited artist
#
# Row order is preserved deliberately. Analysis 2's k-means is seeded, so a
# change to the input ordering can produce a different clustering solution.
# Reordering here is therefore a change to the analysis and needs to be
# reviewed as one, not treated as a neutral tidy-up.
#
# artists = FALSE skips the parse for analyses that never use it.
read_musicoset <- function(data_dir = here::here("data", "raw"), artists = TRUE) {

  acoustic_features <- read_musicoset_file(
    file.path(data_dir, "features", "acoustic_features.csv"))
  chart_weekly <- read_musicoset_file(
    file.path(data_dir, "popularity", "song_chart.csv"))
  songs <- read_musicoset_file(
    file.path(data_dir, "metadata", "songs.csv"))

  require_columns(
    acoustic_features,
    c("song_id", "duration_ms", "key", "mode", "time_signature",
      "acousticness", "danceability", "energy", "instrumentalness",
      "liveness", "loudness", "speechiness", "valence", "tempo"),
    "acoustic_features.csv")

  require_columns(
    songs,
    c("song_id", "song_name", "song_type", "explicit", "artists"),
    "songs.csv")

  require_columns(
    chart_weekly,
    c("song_id", "rank_score", "peak_position", "weeks_on_chart", "week"),
    "song_chart.csv")

  require_unique_key(acoustic_features, "song_id", "acoustic_features.csv")
  require_unique_key(songs, "song_id", "songs.csv")

  if (anyNA(chart_weekly$song_id)) {
    stop("song_chart.csv has missing song_id join keys.", call. = FALSE)
  }

  list(
    songs             = songs,
    acoustic_features = acoustic_features,
    chart_weekly      = chart_weekly,
    song_artists      = if (artists) parse_song_artists(songs) else NULL
  )
}

# ---- Materialisation (inspectable copies only) ----------------------------

# Writes the base tables to data/processed/ so they can be inspected. These
# are never read back: every run rebuilds from data/raw/, so there is no
# cache to go stale.
write_processed_tables <- function(base, dir = here::here("data", "processed")) {
  ensure_dirs(dir)

  for (nm in names(base)) {
    if (!is.null(base[[nm]])) {
      saveRDS(base[[nm]], file.path(dir, paste0(nm, ".rds")))
    }
  }

  invisible(dir)
}

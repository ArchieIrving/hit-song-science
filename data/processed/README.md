# Processed base tables

Files written here are **diagnostic by-products**, not pipeline inputs.

`R/common/ingest_musicoset.R` reads and validates the raw MusicOSet files and
returns four tables at their natural grains. `write_processed_tables()` then
saves them here so they can be inspected between runs.

Nothing reads them back. Every run rebuilds from `data/raw/`, so there is no
cache that can go stale and no way for an edit to the ingestion code to be
silently ignored.

They are gitignored because they are fully derivable from `data/raw/`.

| File | Grain |
|---|---|
| `songs.rds` | one row per song, metadata |
| `acoustic_features.rds` | one row per song, Spotify audio features |
| `chart_weekly.rds` | one row per song and chart week |
| `song_artists.rds` | one row per song and credited artist |

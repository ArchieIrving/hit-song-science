# Raw Data

This directory contains the raw input data used in the analysis. All files are provided in their original form and are not modified manually. Data cleaning and aggregation are performed programmatically by the analysis pipeline.

## Files used in the analysis

### features/acoustic_features.csv
Spotify-derived acoustic features for each song.

Key variables include:
- song_id: Spotify identifier for the song
- duration_ms: track duration in milliseconds
- key, mode, time_signature: musical key, modality, and time signature
- acousticness, danceability, energy, instrumentalness, liveness, speechiness, valence: audio feature scores
- loudness: overall track loudness (dB)
- tempo: estimated tempo (BPM)

### metadata/songs.csv
Song-level metadata and artist information.

Key variables include:
- song_id: Spotify identifier for the song
- name: song title
- artists: credited performing artists
- explicit: indicator for explicit content
- song_type: solo or collaborative song classification

### popularity/song_chart.csv
Weekly Billboard chart performance for songs.

Key variables include:
- id: Spotify identifier for the song
- rank_score: chart entry rank score
- peak_position: highest chart position achieved
- weeks_on_chart: number of weeks on chart
- week: chart date

## Notes

- The analysis pipeline assumes the directory structure and filenames shown above.
- Only the files listed here are read by the analysis scripts.

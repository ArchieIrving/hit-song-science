# Hit Song Science

**Why does research keep disagreeing about what makes a successful song?**

Using 20,000+ Billboard songs and Spotify-derived audio features, this project tests whether some of Hit Song Science's inconsistency comes from how success and musical characteristics are represented. Two reproducible R analyses examine chart longevity using Negative Binomial regression and acoustic structure using PCA and clustering.

## Project overview

| | |
|---|---|
| **Dataset** | 20,000+ Billboard songs with Spotify-derived audio features |
| **Language** | R |
| **Analysis 1** | Negative Binomial regression, diagnostics, feature engineering |
| **Analysis 2** | PCA, k-means clustering, sensitivity analysis, data visualisation |
| **Focus** | Statistical modelling, dimensionality reduction, reproducible analysis |
| **Outputs** | Automated figures, model summaries, cleaned datasets and academic reports |

### Key findings

- **Artist history dominated acoustic features:** a five-week increase in prior average chart longevity was associated with ~**17% greater expected longevity**.
- **Acoustic features formed five interpretable profiles** whose prevalence changed substantially over time.
- **Analytical choices changed the picture:** chart persistence revealed patterns obscured by peak-based measures, while grouping correlated acoustic features exposed broader structure.

[Analysis 1: Chart longevity →](01-chart-longevity/) ·
[Analysis 2: Acoustic structures →](02-acoustic-structures/)

---

## 1. Redefining musical success

Most Hit Song Science research focuses on static outcomes such as whether a song charts or how high it peaks.

That collapses an important distinction. A song that disappears after one week and a song that remains on the chart for a year have both simply "charted".

The first analysis instead models **chart longevity — the number of weeks a song remains on the chart**.

A Negative Binomial model compares two broad sources of explanation:

- **Artist history and prior exposure**, including previous chart persistence and prior appearances.
- **Song characteristics**, including acoustic features such as danceability, energy, loudness and speechiness.

Artist history is much more strongly associated with sustained chart performance than individual acoustic characteristics.

A five-week increase in an artist's prior average chart longevity is associated with approximately **17% greater expected longevity** for the next song. Most practically scaled acoustic effects are much smaller.

### Percentage change in expected chart longevity by predictor

![Coefficient plot comparing artist-history and acoustic predictors](01-chart-longevity/outputs/figures/nb_effects_percent_significant_only.png)

*Note: MusicOSet uses an inverted chart-rank score, so higher values indicate better chart performance.*

The result is not that audio characteristics are irrelevant. Several show statistically detectable associations with longevity, but their effects are modest once artist history and exposure-related factors are taken into account.

**[Explore the chart-longevity analysis →](01-chart-longevity/)**

---

## 2. Acoustic structures, not isolated features

The second analysis asks why acoustic-feature findings are so unstable across studies.

Features such as tempo, energy, danceability and acousticness are often entered into models separately. But these characteristics overlap with one another and may partly reflect broader genre and production conventions.

Rather than modelling them independently, this analysis uses **principal component analysis followed by clustering** to identify broader acoustic structures.

Five interpretable profiles emerge:

- Instrumental-dominant
- Speech-dominant
- Mellow acoustic
- High-tempo vocal
- Melodic-positive

Their prevalence also changes substantially over time.

### Acoustic-structure prevalence among charting songs

![Line chart of acoustic-profile shares from the 1960s to 2018](02-acoustic-structures/outputs/figures/02_cluster_persistence.png)

This matters because a dataset covering several decades is not sampling from one stable musical environment. It pools periods with very different acoustic compositions.

A relationship estimated across the entire period may therefore conceal substantial historical variation.

The clusters also relate differently to alternative definitions of success. Peak chart positions overlap heavily between acoustic profiles, while clearer differences emerge in the distributions of **chart longevity**.

This suggests that acoustic structure may be more informative about **persistence after chart entry** than about initial peak performance.

The changing prevalence of these structures provides one plausible mechanism for inconsistent feature-level effects across datasets and time periods.

**[Explore the acoustic-structure analysis →](02-acoustic-structures/)**

---

## Overall takeaway

The analyses suggest that unstable Hit Song Science findings may partly reflect analytical choices: binary success measures discard persistence, correlated acoustic features contain higher-level structure, and pooled historical datasets combine changing musical environments. These are observational analyses, so they do not establish that musical content causes commercial success, and key industry mechanisms such as promotion and label support are not observed in the data.

---

## Repository structure

```text
.
├── hit-song-science.Rproj     # Open this: all paths resolve from the root
├── run_all.R                  # Runs both analyses end to end
│
├── R/
│   └── common/                # Shared MusicOSet ingestion and utilities
│
├── tests/                     # Recorded baseline and verification script
│
├── data/
│   ├── raw/                   # Shared MusicOSet source files
│   └── processed/             # Diagnostic by-products (gitignored)
│
├── 01-chart-longevity/        # Modelling sustained chart presence
│   ├── R/
│   ├── clean/
│   └── outputs/
│
├── 02-acoustic-structures/    # PCA and clustering of acoustic features
│   ├── R/
│   │   └── figures/           # One script per figure
│   ├── clean/
│   └── outputs/
│
└── writeups/                  # Full academic reports
```

Each analysis has its own README containing the methodology, diagnostics, results, figure rationale and reproduction instructions.

## Data

Both analyses use MusicOSet, an open dataset linking Billboard chart history with song metadata and Spotify-derived audio features.

The source data contain more than 20,000 songs spanning several decades up to 2018. Both analyses apply the same entry criteria and arrive at the same **20,303 songs**: the two cleaned datasets contain exactly the same song IDs. Their schemas differ, because each derives only the variables its own model needs — the longevity analysis adds artist-history and musical-category variables, the clustering analysis adds chart-timing variables and alternative rank measures.

MusicOSet is released under CC BY 4.0.

## Quick start

Everything is written in **R** and runs from the repository root. Open `hit-song-science.Rproj` in RStudio; every path resolves from the project root, so there is no working directory to set.

Install the packages once:

```r
install.packages(c(
  "here", "tidyverse", "withr", "rlang", "scales",
  "MASS", "AER", "performance", "DHARMa", "broom", "scico",
  "cluster", "ggridges", "ggbeeswarm"
))
```

Then run both analyses and check the results against the recorded baseline:

```r
source("run_all.R")                # both analyses end to end, about two minutes
source("tests/verify_results.R")   # 27 sentinel checks, errors if any fail
```

Each analysis also runs on its own from the same project:

```r
source(here::here("01-chart-longevity", "R", "00_run_all.R"))
source(here::here("02-acoustic-structures", "R", "00_run_all.R"))
```

See the individual analysis READMEs for methodology, processing steps and generated outputs:

- [01 — Redefining Musical Success: Chart Longevity](01-chart-longevity/)
- [02 — Acoustic Structures, Not Features](02-acoustic-structures/)

## Licence

Code in this repository is released under the MIT Licence.

Dataset licensing remains with the original MusicOSet authors under CC BY 4.0.

Completed as part of the MSc Data Science programme at the University of Sheffield.

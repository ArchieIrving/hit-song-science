# Modelling Song Chart Longevity

## Project Overview

The aim of this project was to extend research in *Hit Song Science* by adopting an explanatory modelling approach to musical success. Rather than treating success as a binary outcome (chart entry versus non-entry), the study operationalised success as chart longevity, measured by the number of weeks a song remained on the *Billboard* charts.

## Data and Methods

The analysis used data from MusicOSet, an open, curated dataset integrating *Billboard* chart performance with song-level metadata and Spotify-derived acoustic features. The dataset contains 20,405 songs from the U.S. popular music industry spanning 1962–2018.

To examine sustained chart presence, a Negative Binomial regression model was estimated to address two research questions:

- **RQ1:** To what extent do artist history, prior popularity, and exposure-related factors explain sustained chart presence?  
- **RQ2:** What role do song-level characteristics play in shaping sustained chart presence, conditional on artist and exposure factors?

## Key Findings

The results indicate that artist prior chart longevity is the strongest positive predictor of weeks on chart, while entry rank exhibits a smaller but positive association. In contrast, prior peak chart position and repeated chart exposure are negatively associated with chart longevity, suggesting diminishing returns to peak-oriented or repeated success.

Song-level acoustic features were statistically significant but exhibited **modest effect sizes**. Higher danceability and loudness were positively associated with chart longevity, whereas greater speechiness, acousticness, liveness, instrumentalness, and energy were negatively associated. Overall, the findings reinforce long-standing conclusions in Hit Song Science that audio features play a limited explanatory role relative to artist history and exposure dynamics.

## How to Run the Analysis

### Requirements
- R (version 4.x)
- R packages: tidyverse, MASS, AER, performance, DHARMa, broom

### How to Run the Analysis

1. Clone or download this repository to your local machine and open the project in R or RStudio.

2. Set the project root directory as your working directory. The project root is the top-level folder containing the `R/`, `raw/`, `clean/`, and `outputs/` directories.

3. Ensure all required R packages are installed (see the package loading statements at the top of the scripts).

4. Run the full analysis pipeline from the project root using:

```r
source("R/00_run_all.R")
```

Running `00_run_all.R` executes the entire analysis pipeline end-to-end. This includes data cleaning and aggregation, exploratory data analysis, model estimation, and the generation of tables, figures, and diagnostics. All outputs are written automatically to the `clean/` and `outputs/` directories. Progress messages are printed to the console during execution.




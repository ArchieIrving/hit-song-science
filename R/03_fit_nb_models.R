# 03_fit_nb_models.R ------------------------------------------------------
# Model fitting for song chart longevity (weeks_on_chart).
#
# Input:
#   - clean/song_df.csv
#
# Output:
#   - outputs/models/all_models.rds
#   - outputs/models/model_fit_meta.rds

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(MASS) # glm.nb (masks select())
  library(AER)  # dispersiontest
})

source("R/analysis_setup.R")
source("R/helpers.R")

# ======================================================================
# Output paths
# ======================================================================

DIR_MODELS  <- "outputs/models"
PATH_MODELS <- file.path(DIR_MODELS, "all_models.rds")
PATH_META   <- file.path(DIR_MODELS, "model_fit_meta.rds")

ensure_dirs(DIR_MODELS)

# ======================================================================
# Model formulas
# ======================================================================

# Main reporting model: core predictors + acoustic features
f_main <- stats::as.formula(
  paste0(
    OUTCOME_VAR, " ~ ",
    paste(c(core_vars, audio_vars), collapse = " + ")
  )
)

# Alternative specification including musical categorical variables
f_cat <- stats::update(
  f_main,
  stats::as.formula(paste(". ~ . +", paste(cat_vars, collapse = " + ")))
)

# Poisson model fitted for comparison only
f_pois <- f_cat

# ======================================================================
# Data
# ======================================================================

song_df <- readr::read_csv("clean/song_df.csv", show_col_types = FALSE)
song_df <- encode_factors(song_df)

model_vars <- all.vars(f_pois)

model_df <- song_df |>
  dplyr::select(dplyr::all_of(model_vars)) |>
  tidyr::drop_na()

std <- standardise_predictors(
  df = model_df,
  core_vars = core_vars,
  audio_vars = audio_vars,
  exclude = "n_artists_collab"
)

model_df_scaled <- std$df_scaled
scale_vars      <- std$scale_vars

# ======================================================================
# Design matrix check (richest specification)
# ======================================================================

X <- stats::model.matrix(f_pois, data = model_df_scaled)
design_rank <- qr(X)$rank
design_p    <- ncol(X)

# ======================================================================
# Fit models
# ======================================================================

m_pois <- stats::glm(
  f_pois,
  data = model_df_scaled,
  family = stats::poisson(link = "log")
)

disp_test <- AER::dispersiontest(m_pois)

m_nb_main <- MASS::glm.nb(f_main, data = model_df_scaled)
m_nb_cat  <- MASS::glm.nb(f_cat,  data = model_df_scaled)

models <- list(
  poisson = m_pois,
  nb_main = m_nb_main,
  nb_with_categoricals = m_nb_cat
)

# ======================================================================
# Save fitted models and metadata
# ======================================================================

save_rds_safe(models, PATH_MODELS)

fit_meta <- list(
  run_time = Sys.time(),
  n = nrow(model_df_scaled),
  dropped_na_n = nrow(song_df) - nrow(model_df_scaled),
  scale_vars = scale_vars,
  design_rank = design_rank,
  design_p = design_p,
  poisson_overdispersion = list(
    statistic = unname(disp_test$statistic),
    p_value   = unname(disp_test$p.value)
  )
)

save_rds_safe(fit_meta, PATH_META)

message("Model fitting completed.")

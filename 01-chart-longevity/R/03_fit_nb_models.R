# R/03_fit_nb_models.R ------------------------------------------------------
# Fit count models for chart longevity (weeks_on_chart).
# Input:  clean/song_df.csv
# Output: outputs/models/all_models.rds; outputs/models/model_fit_meta.rds

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(MASS) # glm.nb (masks dplyr::select())
  library(AER)  # dispersiontest
})
source(here::here("01-chart-longevity", "R", "paths.R"), local = TRUE)


source(ap("R/analysis_setup.R"), local = TRUE)
source(ap("R/helpers.R"), local = TRUE)

# ---- Output paths ---------------------------------------------------------

DIR_MODELS  <- ap("outputs/models")
PATH_MODELS <- file.path(DIR_MODELS, "all_models.rds")
PATH_META   <- file.path(DIR_MODELS, "model_fit_meta.rds")

ensure_dirs(DIR_MODELS)

# ---- Model formulas -------------------------------------------------------

# A formula carries the environment it was built in, and that environment is
# serialised alongside every model fitted from it. Building these in the global
# environment keeps all_models.rds identical whether the pipeline is run on its
# own or through run_all.R, and stops a saved model from dragging the analysis
# workspace with it. Every model variable comes from `data`, so nothing is
# looked up here.
f_main <- stats::as.formula(
  paste0(OUTCOME_VAR, " ~ ", paste(c(core_vars, audio_vars), collapse = " + ")),
  env = globalenv()
)

# Comparison model including musical categoricals
f_cat <- stats::update(
  f_main,
  stats::as.formula(paste(". ~ . +", paste(cat_vars, collapse = " + ")))
)
environment(f_cat) <- globalenv()

# Poisson is fitted for overdispersion comparison on the richest specification
f_pois <- f_cat

# ---- Data -----------------------------------------------------------------

song_df <- read_csv(ap("clean/song_df.csv"), show_col_types = FALSE)
song_df <- encode_factors(song_df)

model_vars <- all.vars(f_pois)

model_df <- song_df |>
  dplyr::select(all_of(model_vars)) |>
  drop_na()

std <- standardise_predictors(
  df = model_df,
  core_vars = core_vars,
  audio_vars = audio_vars,
  exclude = "n_artists_collab"
)

model_df_scaled <- std$df_scaled
scale_vars      <- std$scale_vars

# ---- Design check (richest specification) --------------------------------

X <- stats::model.matrix(f_pois, data = model_df_scaled)
design_rank <- qr(X)$rank
design_p    <- ncol(X)

# ---- Fit models -----------------------------------------------------------

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

# ---- Save models and metadata --------------------------------------------

# A fitted model holds references to the environment it was fitted in, and
# saveRDS writes out any environment that is not a named one. Fitted inside
# run_all.R rather than at the top level, that would put the whole analysis
# workspace into all_models.rds and quadruple the file. Two kinds of reference
# have to be re-anchored:
#
#   - formula and terms objects, which carry an .Environment attribute;
#   - the Poisson family. stats::poisson() never forces its `link` argument,
#     so the closures it returns keep its evaluation frame alive, and that
#     frame holds an unforced promise pointing back at the caller. Rebuilding
#     the family from the model's own recorded call, in the global
#     environment, drops that link and leaves the family itself unchanged.
#
# Nothing is ever looked up through these environments: each model carries its
# own data, model frame and terms.
anchor_model_env <- function(model) {
  for (slot in c("formula", "terms")) {
    if (!is.null(model[[slot]])) environment(model[[slot]]) <- globalenv()
  }
  if (!is.null(attr(model$model, "terms"))) {
    environment(attr(model$model, "terms")) <- globalenv()
  }
  if (!is.null(model$call$family)) {
    model$family <- eval(model$call$family, globalenv())
  }
  model
}

models <- lapply(models, anchor_model_env)

save_rds_safe(models, PATH_MODELS)

fit_meta <- list(
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

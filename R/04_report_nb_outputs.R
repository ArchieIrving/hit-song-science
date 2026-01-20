# R/04_report_nb_outputs.R --------------------------------------------------
# Reporting outputs for Negative Binomial modelling of song longevity.
#
# Inputs:
#   - outputs/models/all_models.rds
#   - outputs/models/model_fit_meta.rds
#   - clean/song_df.csv
#
# Outputs:
#   - outputs/models/model_summary.txt
#   - outputs/models/final_model_predictor_summary_continuous.csv
#   - outputs/figures/nb_dharma_residuals.png
#   - outputs/figures/nb_effects_percent_significant_only.png
#
# Notes:
# - nb_main is used for reporting; nb_with_categoricals is used for model comparison.
# - Continuous predictors are standardised in fitting (03); effects are rescaled to
#   raw-unit increments using the complete-case modelling sample.


suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(performance) # check_collinearity
  library(broom)       # tidy()
  library(DHARMa)      # simulateResiduals
  library(tidyr)
  library(tibble)
})

source("R/analysis_setup.R")
source("R/helpers.R")

# ======================================================================
# Parameters + dirs
# ======================================================================

DHARMA_N     <- 1000
DHARMA_SEED  <- 123
IRR_P_CUTOFF <- 0.05

DIR_MODELS  <- "outputs/models"
DIR_FIGURES <- "outputs/figures"
ensure_dirs(c(DIR_MODELS, DIR_FIGURES))

# ======================================================================
# Load models + metadata
# ======================================================================

models   <- readRDS(file.path(DIR_MODELS, "all_models.rds"))
fit_meta <- readRDS(file.path(DIR_MODELS, "model_fit_meta.rds"))

required_models <- c("poisson", "nb_main", "nb_with_categoricals")
missing_models  <- setdiff(required_models, names(models))
if (length(missing_models) > 0) {
  stop("Missing expected models in all_models.rds: ", paste(missing_models, collapse = ", "), call. = FALSE)
}

m_pois <- models$poisson
m_nb   <- models$nb_main
m_nb_c <- models$nb_with_categoricals

# Null model for omnibus LRT (required for model_summary.txt)
m_nb_null <- update(m_nb, . ~ 1)

# ======================================================================
# Helpers (local to this script)
# ======================================================================

tidy_irr <- function(m, conf_level = 0.95) {
  broom::tidy(m, conf.int = TRUE, conf.level = conf_level) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      IRR      = exp(estimate),
      IRR_low  = exp(conf.low),
      IRR_high = exp(conf.high)
    ) |>
    dplyr::select(term, estimate, std.error, statistic, p.value, IRR, IRR_low, IRR_high)
}

p_txt <- function(p) {
  if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))
}

extract_lrt <- function(a) {
  cn <- trimws(colnames(a))
  colnames(a) <- cn
  list(
    df   = as.integer(a[2, "df"]),
    stat = as.numeric(a[2, "LR stat."]),
    p    = as.numeric(a[2, "Pr(Chi)"])
  )
}

# ======================================================================
# Complete-case raw sample (single rebuild for SD conversion + descriptives)
# ======================================================================

song_df_raw <- readr::read_csv("clean/song_df.csv", show_col_types = FALSE) |>
  encode_factors()

# Use the richest spec (Poisson) to match the complete-case drop used in fitting
model_vars_rich <- all.vars(stats::formula(m_pois))

df_cc_raw <- song_df_raw |>
  dplyr::select(dplyr::all_of(model_vars_rich)) |>
  tidyr::drop_na()

# numeric predictors available in the complete-case modelling sample (raw units)
num_vars_raw <- names(df_cc_raw)[vapply(df_cc_raw, is.numeric, logical(1))]
num_vars_raw <- setdiff(num_vars_raw, OUTCOME_VAR)

scale_vars_fit <- unique(as.character(fit_meta$scale_vars))

sd_lookup <- df_cc_raw |>
  dplyr::summarise(dplyr::across(dplyr::all_of(scale_vars_fit), sd)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "term", values_to = "sd_raw")

# ======================================================================
# IRR table objects (frozen model only)
# ======================================================================

# Full (for plotting + CSV)
nb_irr_full <- tidy_irr(m_nb)

# Text output (rounded, fixed-width; includes b and SE)
nb_irr_txt <- nb_irr_full |>
  dplyr::mutate(
    b   = round(estimate, 3),
    SE  = round(std.error, 3),
    IRR = round(IRR, 3),
    IRR_low  = round(IRR_low, 3),
    IRR_high = round(IRR_high, 3),
    p_txt = format.pval(p.value, digits = 3, eps = 0.001)
  ) |>
  dplyr::select(term, b, SE, IRR, IRR_low, IRR_high, p_txt)

irr_hdr <- sprintf(
  "%-36s %7s %7s %8s %8s %8s %10s",
  "term", "b", "SE", "IRR", "low", "high", "p"
)

irr_rows <- sprintf(
  "%-36s %7.3f %7.3f %8.3f %8.3f %8.3f %10s",
  nb_irr_txt$term,
  nb_irr_txt$b,
  nb_irr_txt$SE,
  nb_irr_txt$IRR,
  nb_irr_txt$IRR_low,
  nb_irr_txt$IRR_high,
  nb_irr_txt$p_txt
)

irr_lines <- c(
  "----- IRR table (NB main; 95% CI) -----",
  "",
  irr_hdr,
  strrep("-", nchar(irr_hdr)),
  irr_rows,
  ""
)

# ======================================================================
# Model summary (text-only)
# ======================================================================

lrt_cat  <- suppressWarnings(anova(m_nb, m_nb_c, test = "LRT"))
lrt_cat  <- extract_lrt(lrt_cat)

lrt_line <- paste0(
  "LRT (NB main vs NB + categoricals): \u03C7\u00B2(",
  lrt_cat$df, ") = ",
  signif(lrt_cat$stat, 4),
  ", ",
  p_txt(lrt_cat$p)
)

lrt_omni <- suppressWarnings(anova(m_nb_null, m_nb, test = "LRT"))
lrt_omni <- extract_lrt(lrt_omni)

omni_line <- paste0(
  "Omnibus LRT (NB main vs null): \u03C7\u00B2(",
  lrt_omni$df, ") = ",
  signif(lrt_omni$stat, 4),
  ", ",
  p_txt(lrt_omni$p)
)

design_rank <- fit_meta$design_rank
design_p    <- fit_meta$design_p
n_obs       <- fit_meta$n

scaled <- unique(as.character(fit_meta$scale_vars))
cont_all <- intersect(c(core_vars, audio_vars), names(stats::model.frame(m_nb)))
not_scaled <- setdiff(cont_all, scaled)

od <- fit_meta$poisson_overdispersion
overdisp_line <- paste0(
  "Dispersion test (Poisson): statistic = ",
  signif(od$statistic, 4),
  ", ",
  p_txt(od$p_value)
)

ic_tbl <- tibble::tibble(
  Model = c("Poisson", "NB main (frozen)", "NB + categoricals"),
  AIC   = c(AIC(m_pois), AIC(m_nb), AIC(m_nb_c)),
  BIC   = c(BIC(m_pois), BIC(m_nb), BIC(m_nb_c))
)

ic_lines <- c(
  "----- Model comparison (AIC/BIC; lower is better) -----",
  "",
  sprintf("%-28s %12s %12s", "Model", "AIC", "BIC"),
  strrep("-", 56),
  sprintf("%-28s %12.2f %12.2f", ic_tbl$Model, ic_tbl$AIC, ic_tbl$BIC)
)

cook <- stats::cooks.distance(m_nb)
cook_thr <- 4 / length(cook)

cook_max <- max(cook, na.rm = TRUE)
cook_n_above <- sum(cook > cook_thr, na.rm = TRUE)

ord <- order(cook, decreasing = TRUE, na.last = NA)
top_idx <- head(ord, 5)
top_pairs <- if (length(top_idx) == 0) "<none>" else {
  paste0(top_idx, ":", signif(cook[top_idx], 4), collapse = ", ")
}

lines <- c(
  "============================================================",
  paste0("Report run: ", format(Sys.time())),
  "============================================================",
  "",
  "",
  "----- Sample size / design matrix (richest spec used in fitting) -----",
  paste0("N = ", n_obs),
  paste0("rank(X) = ", design_rank, " ; ncol(X) = ", design_p),
  if (design_rank < design_p) {
    "WARNING: rank deficiency detected (perfect collinearity / aliasing likely)."
  } else {
    "OK: full rank design matrix."
  },
  "",
  "----- Scaling -----",
  paste0("Scaled (1 SD): ", paste(scaled, collapse = ", ")),
  paste0("Not scaled: ", if (length(not_scaled) == 0) "<none>" else paste(not_scaled, collapse = ", ")),
  "",
  "----- Omnibus model test -----",
  omni_line,
  "",
  "----- Poisson overdispersion test -----",
  overdisp_line,
  "",
  ic_lines,
  "",
  "----- Likelihood ratio test (nested NB models) -----",
  lrt_line,
  "",
  "----- Collinearity (NB main) -----",
  paste(capture.output(performance::check_collinearity(m_nb)), collapse = "\n"),
  "",
  "----- Influence (Cook's distance; NB main) -----",
  paste0("Rule-of-thumb threshold: 4/n = ", signif(cook_thr, 4)),
  paste0("Max Cook's distance: ", signif(cook_max, 4)),
  paste0(
    "N above threshold: ",
    cook_n_above,
    " (",
    round(100 * cook_n_above / n_obs, 1),
    "%)"
  ),
  paste0("Top Cook's distances (index:value): ", top_pairs),
  ""
)

lines <- c(
  lines,
  "----- Model identity check -----",
  paste0("fit_meta run_time: ", fit_meta$run_time),
  paste0("nobs(m_nb): ", stats::nobs(m_nb)),
  paste0("formula(m_nb): ", paste(deparse(stats::formula(m_nb)), collapse = " ")),
  ""
)

# Append IRR table to the end of model_summary.txt
lines <- c(lines, irr_lines)

write_lines_safe(lines, file.path(DIR_MODELS, "model_summary.txt"))

# ======================================================================
# CONTINUOUS PREDICTOR SUMMARY (RAW DISTRIBUTION + SCALING INFO)
# ======================================================================

predictor_summary_continuous <- df_cc_raw |>
  dplyr::select(dplyr::all_of(num_vars_raw)) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      list(
        mean   = mean,
        sd     = sd,
        min    = min,
        max    = max,
        median = median,
        q25    = ~ quantile(.x, 0.25, na.rm = TRUE),
        q75    = ~ quantile(.x, 0.75, na.rm = TRUE),
        p10    = ~ quantile(.x, 0.10, na.rm = TRUE),
        p90    = ~ quantile(.x, 0.90, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("term", "stat"),
    names_sep = "__",
    values_to = "value"
  ) |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  dplyr::mutate(IQR = q75 - q25) |>
  dplyr::left_join(INC_MAP, by = "term") |>
  dplyr::mutate(
    inc_over_SD  = inc / sd,
    inc_over_IQR = inc / IQR
  ) |>
  dplyr::arrange(term)

readr::write_csv(
  predictor_summary_continuous,
  file.path(DIR_MODELS, "final_model_predictor_summary_continuous.csv")
)

# ======================================================================
# Figures
# ======================================================================

# ----------------------------------------------------------------------
# DHARMa diagnostics (frozen model)
# ----------------------------------------------------------------------

set.seed(DHARMA_SEED)
sim <- DHARMa::simulateResiduals(m_nb, n = DHARMA_N)

out_resid <- file.path(DIR_FIGURES, "nb_dharma_residuals.png")
grDevices::png(out_resid, width = 1400, height = 1000, res = 200)
plot(sim)
grDevices::dev.off()

# ----------------------------------------------------------------------
# IRR plot (significant terms only; frozen model)
# ----------------------------------------------------------------------

sig <- nb_irr_full |>
  dplyr::filter(!is.na(p.value), p.value < IRR_P_CUTOFF) |>
  dplyr::mutate(
    group = dplyr::case_when(
      term %in% c(
        "song_rank_entry",
        "artist_prior_n_songs",
        "artist_avg_longevity_prior",
        "artist_avg_peak_rank_prior"
      ) ~ "Pre-entry factors",
      term %in% audio_vars ~ "Acoustic features",
      TRUE ~ "Controls"
    ),
    group = factor(group, levels = c("Pre-entry factors", "Acoustic features", "Controls")),
    label = vapply(term, label_model_term_with_increment, character(1))
  ) |>
  dplyr::left_join(INC_MAP, by = "term") |>
  dplyr::mutate(inc = if_else(is.na(inc), 1, inc)) |>
  dplyr::left_join(sd_lookup, by = "term") |>
  dplyr::mutate(
    expo = dplyr::case_when(
      term %in% scale_vars_fit ~ inc / sd_raw,
      TRUE ~ inc
    ),
    
    log_irr      = log(IRR),
    log_irr_low  = log(IRR_low),
    log_irr_high = log(IRR_high),
    
    irr_inc      = exp(log_irr      * expo),
    irr_low_inc  = exp(log_irr_low  * expo),
    irr_high_inc = exp(log_irr_high * expo),
    
    pct      = (irr_inc - 1) * 100,
    pct_low  = (irr_low_inc - 1) * 100,
    pct_high = (irr_high_inc - 1) * 100,
    
    ord = abs(pct)
  ) |>
  dplyr::group_by(group) |>
  dplyr::mutate(label = forcats::fct_reorder(label, ord)) |>
  dplyr::ungroup() |>
  dplyr::arrange(group, desc(ord)) |>
  dplyr::mutate(
    x_lab = if_else(pct >= 0, pct_high + 1.2, pct_low - 1.2),
    hjust = if_else(pct >= 0, 0, 1),
    pct_label = sprintf("%+.0f%%", pct)
  )

x_lim <- max(abs(c(sig$pct_low, sig$pct_high)), na.rm = TRUE) + 3

p_sig <- ggplot(sig, aes(y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, colour = COL_REF_LINE) +
  geom_errorbarh(
    aes(xmin = pct_low, xmax = pct_high),
    height = 0.18,
    linewidth = 0.7,
    colour = "grey55"
  ) +
  geom_point(aes(x = pct), size = 3.0, colour = COL_PRIMARY) +
  geom_text(
    aes(x = x_lab, label = pct_label, hjust = hjust),
    size = 3.6,
    colour = COL_NEUTRAL
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(
    limits = c(-x_lim, x_lim),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Effects on Chart Longevity",
    x = "Percent change in expected weeks on chart (per stated increment)",
    y = NULL
  ) +
  theme_project() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey92", colour = NA),
    strip.text.y.left = element_text(angle = 90, face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 16, 12, 10)
  )

ggsave(
  filename = file.path(DIR_FIGURES, "nb_effects_percent_significant_only.png"),
  plot = p_sig,
  width = 9.5,
  height = 7.2,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Reporting complete. Outputs written to outputs/models and outputs/figures.")

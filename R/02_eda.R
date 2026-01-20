# 02_eda.R -----------------------------------------------------------
# Exploratory analysis for chart longevity (weeks_on_chart).
# Inputs:  clean/song_df.csv
# Outputs: outputs/figures/*.png; outputs/descriptives/*.csv; eda_summary.txt


suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(rlang)
})

source("R/analysis_setup.R")
source("R/helpers.R")

# Output directories
ensure_dirs(c("outputs/figures", "outputs/descriptives"))

# ======================================================================
# Correlation heatmap (continuous predictors)
# ======================================================================

plot_corr_heatmap <- function(df,
                              vars,
                              title = "Correlation Heatmap (Continuous Predictors)",
                              label_cutoff = 0.70) {
  vars <- intersect(vars, names(df))
  stopifnot(length(vars) >= 2)
  
  df_num <- df |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  
  cmat <- stats::cor(df_num, use = "pairwise.complete.obs", method = "pearson")
  
  cor_long <- as.data.frame(cmat) |>
    tibble::rownames_to_column("var1") |>
    tidyr::pivot_longer(-var1, names_to = "var2", values_to = "corr") |>
    dplyr::mutate(
      var1 = factor(var1, levels = vars),
      var2 = factor(var2, levels = vars),
      i = as.integer(var1),
      j = as.integer(var2)
    ) |>
    dplyr::filter(j > i) |>
    dplyr::mutate(
      label = dplyr::if_else(
        !is.na(corr) & abs(corr) >= label_cutoff,
        sprintf("%.2f", corr),
        ""
      )
    )
  
  axis_labels <- named_labels(vars)
  
  ggplot(cor_long, aes(x = var2, y = var1, fill = corr)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(limits = c(-1, 1), name = "corr") +
    scale_x_discrete(labels = axis_labels) +
    scale_y_discrete(labels = axis_labels) +
    labs(title = title, x = NULL, y = NULL) +
    theme_project() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank()
    )
}

# ======================================================================
# Main EDA routine
# ======================================================================

run_eda <- function(df,
                    outcome_var = OUTCOME_VAR,
                    core_vars = character(),
                    audio_vars = character(),
                    cat_vars = character(),
                    main_cat = c("song_type"),
                    n_bins = 25) {
  cont_all  <- intersect(c(core_vars, audio_vars), names(df))
  cat_all   <- intersect(cat_vars, names(df))
  deep_cont <- intersect(core_vars, names(df))
  deep_cat  <- intersect(main_cat, names(df))
  
  # ------------------------------------------------------------
  # Outcome distribution
  # ------------------------------------------------------------
  y <- df[[outcome_var]]
  
  p_outcome <- ggplot(df, aes(x = .data[[outcome_var]])) +
    geom_histogram(
      binwidth = 1,
      boundary = 0,
      colour = COL_OUTLINE,
      fill = COL_PRIMARY
    ) +
    geom_vline(
      xintercept = mean(y, na.rm = TRUE),
      linetype = "dashed",
      colour = COL_REF_LINE
    ) +
    scale_x_continuous(
      breaks = seq(1, max(y, na.rm = TRUE), by = 10),
      limits = c(1, NA)
    ) +
    labs(
      title = "Distribution of Weeks on Chart",
      x = "Weeks on Chart",
      y = "Number of Songs"
    ) +
    theme_project()
  
  save_plot("01_outcome_distribution_histogram.png", p_outcome)
  
  # ------------------------------------------------------------
  # Correlation heatmap
  # ------------------------------------------------------------
  if (length(cont_all) >= 2) {
    save_plot(
      "02_predictor_correlation_heatmap.png",
      plot_corr_heatmap(df, cont_all),
      width = 11,
      height = 8.5
    )
  }
  
  # ------------------------------------------------------------
  # Predictor summaries (descriptive statistics)
  # ------------------------------------------------------------
  
  if (length(cont_all) > 0) {
    cont_tab <- df |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(cont_all),
          list(
            mean   = ~ mean(., na.rm = TRUE),
            sd     = ~ sd(., na.rm = TRUE),
            median = ~ stats::median(., na.rm = TRUE),
            min    = ~ min(., na.rm = TRUE),
            max    = ~ max(., na.rm = TRUE),
            p25    = ~ stats::quantile(., 0.25, na.rm = TRUE),
            p75    = ~ stats::quantile(., 0.75, na.rm = TRUE)
          ),
          .names = "{.col}__{.fn}"
        )
      ) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "stat", values_to = "value") |>
      tidyr::separate(stat, into = c("variable", "stat"), sep = "__") |>
      tidyr::pivot_wider(names_from = stat, values_from = value) |>
      dplyr::mutate(
        IQR = p75 - p25,
        Variable = label_from_map(variable)
      ) |>
      dplyr::select(Variable, mean, sd, median, min, max, IQR) |>
      dplyr::arrange(Variable)
    
    readr::write_csv(cont_tab, "outputs/descriptives/predictor_summary_continuous.csv")
  }
  
  if (length(cat_all) > 0) {
    cat_tab <- df |>
      dplyr::select(dplyr::all_of(cat_all)) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "category") |>
      tidyr::drop_na(category) |>
      dplyr::mutate(
        Variable = label_from_map(variable),
        category = as.character(category)
      ) |>
      dplyr::count(Variable, category, name = "count") |>
      dplyr::group_by(Variable) |>
      dplyr::mutate(proportion = count / sum(count)) |>
      dplyr::ungroup() |>
      dplyr::arrange(Variable, dplyr::desc(count))
    
    readr::write_csv(cat_tab, "outputs/descriptives/predictor_summary_categorical.csv")
  }
  
  # ------------------------------------------------------------
  # Musical categoricals: category proportions (sparsity / imbalance check)
  # ------------------------------------------------------------
  
  mus_cat <- intersect(c("key", "mode", "time_signature"), names(df))
  
  if (length(mus_cat) > 0) {
    df_props <- df |>
      dplyr::select(dplyr::all_of(mus_cat)) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "category") |>
      tidyr::drop_na(category) |>
      dplyr::mutate(
        variable = factor(variable, levels = c("time_signature", "mode", "key")),
        variable_label = label_from_map(variable),
        category = as.character(category)
      ) |>
      dplyr::count(variable, variable_label, category, name = "count") |>
      dplyr::group_by(variable, variable_label) |>
      dplyr::mutate(proportion = count / sum(count)) |>
      dplyr::ungroup() |>
      dplyr::group_by(variable) |>
      dplyr::mutate(category = forcats::fct_reorder(category, proportion)) |>
      dplyr::ungroup()
    
    p_props <- ggplot(df_props, aes(x = category, y = proportion)) +
      geom_col(fill = "grey70", colour = "grey40", linewidth = 0.2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      facet_wrap(~ variable_label, scales = "free_x", ncol = 1) +
      labs(
        title = "Distribution of Musical Categorical Variables",
        subtitle = "Category proportions (used to assess sparsity / imbalance)",
        x = NULL,
        y = "Proportion of songs"
      ) +
      theme_project() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    save_plot("05_musical_categorical_proportions.png", p_props, width = 9, height = 9)
  }
  
  # ------------------------------------------------------------
  # Binned mean outcome by core predictors (shape check)
  # ------------------------------------------------------------
  
  if (length(deep_cont) > 0 && outcome_var %in% names(df)) {
    make_quantile_breaks <- function(x, n_bins) {
      brks <- unique(stats::quantile(x, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
      if (length(brks) < 5) brks <- pretty(range(x, na.rm = TRUE), n = 10)
      brks
    }
    
    df_long <- df |>
      dplyr::select(dplyr::all_of(c(deep_cont, outcome_var))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(deep_cont),
        names_to = "predictor",
        values_to = "x"
      ) |>
      dplyr::mutate(y = .data[[outcome_var]]) |>
      dplyr::filter(is.finite(x), is.finite(y))
    
    d_binned <- df_long |>
      dplyr::group_by(predictor) |>
      dplyr::group_modify(function(d, key) {
        brks <- make_quantile_breaks(d$x, n_bins)
        
        d |>
          dplyr::mutate(bin = cut(x, breaks = brks, include.lowest = TRUE)) |>
          dplyr::group_by(bin) |>
          dplyr::summarise(
            x_mid = mean(x, na.rm = TRUE),
            mean_y = mean(y, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::filter(is.finite(x_mid), is.finite(mean_y))
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        predictor_label = label_from_map(predictor),
        predictor_label = factor(predictor_label, levels = label_from_map(deep_cont))
      )
    
    p_binned <- ggplot(d_binned, aes(x = x_mid, y = mean_y)) +
      geom_line(linewidth = 0.8, colour = COL_PRIMARY) +
      geom_point(size = 1.8, colour = COL_PRIMARY) +
      facet_wrap(~ predictor_label, scales = "free_x", ncol = 2) +
      labs(
        title = "Binned Mean Outcome vs Core Predictors",
        subtitle = "Binned means across predictor quantiles",
        x = NULL,
        y = paste0("Mean ", label_from_map(outcome_var))
      ) +
      theme_project()
    
    save_plot("03_bivariate_binned_mean_main_predictors.png", p_binned, width = 12, height = 8)
  }
  
  # ------------------------------------------------------------
  # Outcome by category
  # ------------------------------------------------------------
  
  if (length(deep_cat) > 0) {
    for (v in deep_cat) {
      p_cat <- ggplot(df, aes(x = .data[[v]], y = .data[[outcome_var]])) +
        geom_boxplot(outlier.alpha = 0.25, fill = "grey90", colour = "grey40") +
        labs(
          title = paste0(label_from_map(outcome_var), " by ", label_from_map(v)),
          x = label_from_map(v),
          y = label_from_map(outcome_var)
        ) +
        theme_project()
      
      save_plot(paste0("04_response_by_", v, ".png"), p_cat, width = 7.5, height = 5.5)
    }
  }
  
  # ------------------------------------------------------------
  # EDA summary (key outcome diagnostics)
  # ------------------------------------------------------------
  
  mean_y <- mean(df[[outcome_var]], na.rm = TRUE)
  var_y <- var(df[[outcome_var]], na.rm = TRUE)
  disp_ratio <- var_y / mean_y
  p1 <- mean(df[[outcome_var]] == 1, na.rm = TRUE)
  p2 <- mean(df[[outcome_var]] <= 2, na.rm = TRUE)
  
  write_lines_safe(
    c(
      paste0("EDA run: ", format(Sys.time())),
      "",
      paste0("N songs: ", nrow(df)),
      "",
      paste0("Outcome mean: ", round(mean_y, 2)),
      paste0("Outcome variance: ", round(var_y, 2)),
      paste0("Dispersion ratio (Var/Mean): ", round(disp_ratio, 2)),
      "",
      paste0("Share of songs lasting 1 week: ", round(100 * p1, 1), "%"),
      paste0("Share of songs lasting <= 2 weeks: ", round(100 * p2, 1), "%"),
      "",
      "Dispersion ratio (Var/Mean) indicates whether variance materially exceeds the mean."
    ),
    "outputs/descriptives/eda_summary.txt"
  )
  
  invisible(TRUE)
}

# ======================================================================
# Run
# ======================================================================

song_df <- readr::read_csv("clean/song_df.csv", show_col_types = FALSE)
song_df <- encode_factors(song_df)

run_eda(
  df = song_df,
  outcome_var = OUTCOME_VAR,
  core_vars = core_vars,
  audio_vars = audio_vars,
  cat_vars = cat_vars,
  n_bins = 25
)

# End of file -------------------------------------------------------------

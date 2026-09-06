# R/helpers.R --------------------------------------------------------------
# Small helper utilities shared across the analysis pipeline:
# - directory creation
# - saving plots / text / objects
# - a project-wide ggplot theme

suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
})

# ---- Directories ---------------------------------------------------------

ensure_dirs <- function(paths) {
  stopifnot(is.character(paths), length(paths) >= 1)
  
  paths <- unique(paths)
  
  for (p in paths) {
    if (dir.exists(p)) next
    dir.create(p, showWarnings = FALSE, recursive = TRUE)
  }
  
  invisible(TRUE)
}

# ---- Saving helpers ------------------------------------------------------

# Write text output to disk, creating directories if needed.
write_lines_safe <- function(lines, path) {
  stopifnot(is.character(path), length(path) == 1)
  stopifnot(is.character(lines))
  
  ensure_dirs(dirname(path))
  readr::write_lines(lines, path)
  
  invisible(TRUE)
}

# Save an R object as .rds, ensuring the output directory exists.
save_rds_safe <- function(object, path) {
  stopifnot(is.character(path), length(path) == 1)
  
  ensure_dirs(dirname(path))
  saveRDS(object, path)
  
  invisible(TRUE)
}

# Save a ggplot with consistent export settings.
save_plot <- function(filename,
                      plot_obj,
                      dir = "outputs/figures",
                      width = 7,
                      height = 5,
                      dpi = 300,
                      units = "in") {
  stopifnot(is.character(filename), length(filename) == 1)
  stopifnot(is.character(dir), length(dir) == 1)
  
  ensure_dirs(dir)
  
  ggplot2::ggsave(
    filename = file.path(dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
  
  invisible(TRUE)
}


# ---- Plot theme ----------------------------------------------------------

# Project palette / styling constants
COL_PRIMARY  <- "#0072B2"
COL_NEUTRAL  <- "grey30"
COL_GRID     <- "grey92"
COL_OUTLINE  <- "grey90"
COL_REF_LINE <- "grey55"

theme_project <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = COL_NEUTRAL),
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = COL_GRID, linewidth = 0.25),
      axis.text = ggplot2::element_text(colour = COL_NEUTRAL),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
    )
}

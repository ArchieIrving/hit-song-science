# R/common/utils.R ----------------------------------------------------------
# Utilities used by both analyses.
#
# Calls here are namespace-qualified where a package function is involved, so
# these behave the same regardless of which packages an analysis happens to
# have attached. Analysis 1 attaches MASS, which masks dplyr::select, and that
# kind of leakage should never reach shared infrastructure.

# Create one or more directories if they do not already exist. Accepts a
# vector, so it covers both the "make one directory" and "make several" cases.
ensure_dirs <- function(paths) {
  stopifnot(is.character(paths), length(paths) >= 1)

  for (p in unique(paths)) {
    if (!dir.exists(p)) dir.create(p, showWarnings = FALSE, recursive = TRUE)
  }

  invisible(TRUE)
}

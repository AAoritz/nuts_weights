# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     _nuts_source.R   (helper, sourced by the other scripts)
# │  Objective  Pin the release of the `nuts` R package that all scripts use, so
# │             that every number in the article comes from one explicit version.
# │             - require_nuts_version(): stops unless the installed package is
# │               exactly NUTS_VERSION. All analysis takes the conversion
# │               tables from the package (data(cross_walks)).
# │             - nuts_matrices_dir(): downloads the raw csv tables of that
# │               release from the package repository and returns the folder
# │               that holds them. Only the description of the csv files in
# │               Data Records uses them (codes/intext_numbers.R). Files are
# │               cached, so they are downloaded only once.
# │  Output     data/cache/nuts_v<version>/*.csv   (git-ignored cache)
# └────────────────────────────────────────────────────────────────────────────

NUTS_VERSION <- "1.3.0"
NUTS_TAG     <- paste0("v", NUTS_VERSION)
NUTS_REPO    <- "ropensci/nuts"

# Folder of the raw conversion tables inside the package repository
NUTS_RAW_DIR <- "data-raw/_JRC_NUTSconverter_Matrices_rel.2025.08"

# The tables cover every pair of the six NUTS versions at the three levels
NUTS_VERSIONS <- c(2006, 2010, 2013, 2016, 2021, 2024)
NUTS_LEVELS   <- 1:3


require_nuts_version <- function() {
  install_hint <- paste0('remotes::install_github("', NUTS_REPO, "@", NUTS_TAG, '")')
  if (!requireNamespace("nuts", quietly = TRUE)) {
    stop("The nuts package is not installed. Install version ", NUTS_VERSION,
         " with\n  ", install_hint, call. = FALSE)
  }
  installed <- as.character(utils::packageVersion("nuts"))
  if (installed != NUTS_VERSION) {
    stop("The analysis requires nuts ", NUTS_VERSION, " but version ", installed,
         " is installed. Install the required version with\n  ", install_hint,
         call. = FALSE)
  }
  invisible(TRUE)
}


nuts_matrix_files <- function() {
  pairs <- utils::combn(NUTS_VERSIONS, 2)
  unlist(lapply(NUTS_LEVELS, function(l) {
    sprintf("n%d_cm_v%d_v%d.csv", l, pairs[1, ], pairs[2, ])
  }))
}


nuts_matrices_dir <- function(cache_root = file.path("data", "cache")) {
  dest_dir <- file.path(cache_root, paste0("nuts_", NUTS_TAG))
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)

  files   <- nuts_matrix_files()
  missing <- files[!file.exists(file.path(dest_dir, files))]

  if (length(missing) > 0) {
    message("Downloading ", length(missing), " conversion table(s) of nuts ",
            NUTS_TAG, " to ", dest_dir)
    base_url <- paste0("https://raw.githubusercontent.com/", NUTS_REPO, "/",
                       NUTS_TAG, "/", NUTS_RAW_DIR)
    for (f in missing) {
      dest <- file.path(dest_dir, f)
      tmp  <- paste0(dest, ".part")
      # download to a temporary name so an interrupted download is not cached
      status <- tryCatch(
        utils::download.file(paste0(base_url, "/", f), tmp, mode = "wb",
                             quiet = TRUE),
        error = function(e) e
      )
      if (inherits(status, "error") || status != 0 || !file.exists(tmp) ||
          file.size(tmp) == 0) {
        unlink(tmp)
        stop("Could not download ", f, " from ", base_url, call. = FALSE)
      }
      file.rename(tmp, dest)
    }
  }

  dest_dir
}

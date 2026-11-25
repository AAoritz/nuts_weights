################################################################################
# run_all.R — Master script for the NUTS conversion-weights article
#
# Runs every figure/data-generating script in codes/ in a clean environment,
# from the project root, with per-script timing and error handling.
#
# Usage (from the project root):
#   Rscript codes/run_all.R              # run everything
#   Rscript codes/run_all.R figures      # only scripts that write a figure
#   Rscript codes/run_all.R validation   # only the validation and robustness scripts
#   Rscript codes/run_all.R nuts_revisions_map validation_eurostat
#                                         # named scripts (with or without .R)
#
# Inside an interactive R session you can instead do:
#   source("codes/run_all.R")            # defines run_all(); does not auto-run
#   run_all()                            # run everything
#   run_all("validation_eurostat")       # run a subset
#
# Outputs are written to figs/ and data/ (see the `outputs` column below).
################################################################################

# ── Locate the project root ────────────────────────────────────────────────────
# Scripts use paths relative to the project root (e.g. "figs/...", here::here()),
# so all of them must run with the working directory set there. We anchor on the
# folder that contains this file (codes/) and step up one level.

find_project_root <- function() {
  # When run via `Rscript codes/run_all.R`, the script path is on the cmd line.
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(file_arg) == 1 && nzchar(file_arg)) {
    return(normalizePath(file.path(dirname(file_arg), ".."), mustWork = FALSE))
  }
  # When source()d interactively, fall back to the working directory, assuming
  # it is either the project root or codes/.
  wd <- normalizePath(getwd(), mustWork = FALSE)
  if (file.exists(file.path(wd, "nuts_article.qmd"))) return(wd)
  if (basename(wd) == "codes")                        return(dirname(wd))
  wd
}

PROJECT_ROOT <- find_project_root()

# ── Registry of scripts ─────────────────────────────────────────────────────────
# Order matters: country_coverage runs first because it documents the inputs;
# the heavier figure scripts follow. `group` is used by the CLI filters.

# validation_heterogeneity reads the region-level errors saved by
# validation_eurostat, so it must follow it.
# intext_numbers runs last: it consolidates the stats files written by the
# validation/robustness scripts (plus the conversion-table counts) into the
# single data file the manuscript reads, so it must come after its producers.
scripts <- data.frame(
  name = c("country_coverage", "nuts_revisions_map", "robustness_slivers_sim",
           "validation_eurostat", "validation_gdp_pop", "validation_heterogeneity",
           "merge_example_figures",
           "intext_numbers"),
  # Space-separated groups; a script can belong to several.
  group = c("data", "figures", "figures validation",
            "figures validation", "figures validation", "figures validation",
            "figures", "data"),
  outputs = c(
    "data/country_coverage_summary.csv, data/country_coverage_matrix.csv",
    "figs/european_nuts_changes.png",
    "figs/robustness_slivers_sim.png, data/stats/slivers_stats.rds",
    "figs/validation_eurostat.png, data/stats/roundtrip_{stats,regions}.rds",
    "figs/validation_gdp_pop_*.png, data/stats/gdp_pop_stats.{csv,rds}",
    "figs/validation_heterogeneity.png, data/stats/heterogeneity_stats.rds",
    "figs/example_absolute.png, figs/example_relative.png",
    "data/stats/intext_numbers.rds"
  ),
  stringsAsFactors = FALSE
)

# ── Runner ──────────────────────────────────────────────────────────────────────

# Source one script in a fresh environment so objects don't leak between scripts.
run_one <- function(name, root = PROJECT_ROOT) {
  path <- file.path(root, "codes", paste0(name, ".R"))
  if (!file.exists(path)) {
    message(sprintf("  ! SKIP  %-24s (file not found: %s)", name, path))
    return(invisible(list(name = name, ok = NA, secs = 0)))
  }

  message(strrep("-", 78))
  message(sprintf("  >>> %s", name))
  message(strrep("-", 78))

  t0 <- Sys.time()
  ok <- tryCatch({
    # Each script assumes the project root as the working directory and a clean
    # global state (several start with rm(list = ls())). new.env() keeps the
    # master script's own variables out of their reach.
    old_wd <- setwd(root)
    on.exit(setwd(old_wd), add = TRUE)
    sys.source(path, envir = new.env(parent = globalenv()), chdir = FALSE)
    TRUE
  }, error = function(e) {
    message(sprintf("  ! ERROR in %s: %s", name, conditionMessage(e)))
    FALSE
  })
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  message(sprintf("  %s %-24s (%.1fs)",
                  if (isTRUE(ok)) "OK  " else "FAIL", name, secs))
  invisible(list(name = name, ok = ok, secs = secs))
}

# Resolve a vector of selectors (script names and/or group names) to a list of
# script names, preserving the registry order and dropping duplicates.
resolve_selection <- function(selectors) {
  if (length(selectors) == 0) return(scripts$name)
  selectors  <- sub("\\.R$", "", selectors)          # tolerate "foo.R"
  group_list <- strsplit(scripts$group, " ", fixed = TRUE)
  all_groups <- unique(unlist(group_list))
  groups     <- intersect(selectors, all_groups)
  named      <- intersect(selectors, scripts$name)
  unknown    <- setdiff(selectors, c(groups, named))
  if (length(unknown) > 0) {
    # Fail instead of silently running nothing.
    stop("Unknown selector(s): ", paste(unknown, collapse = ", "),
         "\n  Valid scripts: ", paste(scripts$name, collapse = ", "),
         "\n  Valid groups:  ", paste(all_groups, collapse = ", "),
         call. = FALSE)
  }
  in_group <- vapply(group_list, function(g) any(g %in% groups), logical(1))
  scripts$name[scripts$name %in% named | in_group]
}

# Main entry point. `...` accepts script names and/or group names as strings.
run_all <- function(...) {
  selection <- resolve_selection(c(...))

  message("\n", strrep("=", 78))
  message("  NUTS article — running ", length(selection), " script(s)")
  message("  Project root: ", PROJECT_ROOT)
  message(strrep("=", 78), "\n")

  results <- lapply(selection, run_one)

  # ── Summary ──────────────────────────────────────────────────────────────────
  message("\n", strrep("=", 78))
  message("  SUMMARY")
  message(strrep("=", 78))
  for (r in results) {
    status <- if (isTRUE(r$ok)) "OK  " else if (is.na(r$ok)) "SKIP" else "FAIL"
    message(sprintf("  %s  %-24s %6.1fs", status, r$name, r$secs))
  }
  total <- sum(vapply(results, function(r) r$secs, numeric(1)))
  n_fail <- sum(vapply(results, function(r) isFALSE(r$ok), logical(1)))
  message(strrep("-", 78))
  message(sprintf("  %d script(s), %d failure(s), %.1fs total",
                  length(results), n_fail, total))
  message(strrep("=", 78), "\n")

  invisible(results)
}

# ── Auto-run when invoked with Rscript ──────────────────────────────────────────
# (When source()d into an interactive session, only the functions are defined.)

if (!interactive() && sys.nframe() == 0L) {
  cli_args <- commandArgs(trailingOnly = TRUE)
  results  <- do.call(run_all, as.list(cli_args))
  # Non-zero exit status if any script failed, so CI/Make can detect it.
  if (any(vapply(results, function(r) isFALSE(r$ok), logical(1)))) {
    quit(status = 1L)
  }
}

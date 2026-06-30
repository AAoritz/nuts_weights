# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     intext_numbers.R
# │  Objective  Consolidate every number quoted in the body of nuts_article.qmd
# │             into a single R data file, so the manuscript can display them
# │             via inline R instead of hard-coded literals. Pulls the validation
# │             statistics from the per-script stats files and adds the
# │             data-records counts and the Methods worked-example values.
# │  Inputs     data/stats/roundtrip_stats.rds   (validation_eurostat.R)
# │             data/stats/gdp_pop_stats.rds      (validation_gdp_pop.R)
# │             data/stats/slivers_stats.rds      (robustness_slivers_sim.R)
# │             data/jrc_nuts_converter_matrices/ (conversion tables)
# │  Output     data/stats/intext_numbers.rds     (named list `nums`)
# └────────────────────────────────────────────────────────────────────────────

library(tidyverse)

stats_dir <- "data/stats"

need <- c("roundtrip_stats.rds", "gdp_pop_stats.rds", "slivers_stats.rds")
missing <- need[!file.exists(file.path(stats_dir, need))]
if (length(missing) > 0) {
  stop("Missing stats file(s): ", paste(missing, collapse = ", "),
       "\n  Run the producing scripts first (validation_eurostat.R, ",
       "validation_gdp_pop.R, robustness_slivers_sim.R).")
}

rt <- readRDS(file.path(stats_dir, "roundtrip_stats.rds"))   # round-trip
gp <- readRDS(file.path(stats_dir, "gdp_pop_stats.rds"))      # Eurostat GDP/pop
sl <- readRDS(file.path(stats_dir, "slivers_stats.rds"))      # sliver robustness


# ── Data Records: shape of the conversion tables ───────────────────────────────
nuts_files <- list.files("data/jrc_nuts_converter_matrices", full.names = TRUE)
m_example  <- read_csv(nuts_files[1], show_col_types = FALSE)
nr_files   <- length(nuts_files)
nr_cols    <- ncol(m_example)


# ── Eurostat GDP/pop validation: pull the single-year vectors by weight ────────
# gp$mape is named e.g. "pop1y_pop21", "gdp1y_area"; reshape to weight-keyed
# vectors (area, surf12, surf18, buvol, pop11, pop21).
pick_by_weight <- function(vec, prefix) {
  v <- vec[grepl(paste0("^", prefix, "_"), names(vec))]
  names(v) <- sub(paste0("^", prefix, "_"), "", names(v))
  v
}
gp_mape_pop <- pick_by_weight(gp$mape, "pop1y")   # single-year population
gp_mape_gdp <- pick_by_weight(gp$mape, "gdp1y")   # single-year GDP

# Mean gap between GDP and population MAPE across the six weights (pp).
gp_gdp_minus_pop_pp <- mean(gp_mape_gdp - gp_mape_pop[names(gp_mape_gdp)])


# ── Methods: worked examples (absolute & relative conversion) ──────────────────
# These illustrative values match the slide figures. Derived quantities are
# computed here so the arithmetic shown in the text can never drift.
src_pop  <- c(CA111 = 500000, CA112 = 300000)   # source-region populations
abs_val  <- c(CA111 = 85000,  CA112 = 50000)    # absolute variable of interest
pct_val  <- c(CA111 = 8,      CA112 = 11)       # relative variable (%)

# Population flows across the revised boundaries
pop_111_113 <- 390000           # CA111 -> CA113
pop_111_114 <- 110000           # CA111 -> CA114
pop_112_114 <- 300000           # CA112 -> CA114 (all of CA112)

# Absolute conversion (proportional allocation)
abs_113        <- abs_val["CA111"] * pop_111_113 / src_pop["CA111"]  # 66,300
abs_114_f111   <- abs_val["CA111"] * pop_111_114 / src_pop["CA111"]  # 18,700
abs_114_f112   <- abs_val["CA112"] * pop_112_114 / src_pop["CA112"]  # 50,000
abs_114        <- abs_114_f111 + abs_114_f112                        # 68,700
pop_113        <- pop_111_113                                        # 390,000
pop_114        <- pop_111_114 + pop_112_114                          # 410,000

# Relative conversion (population-weighted average)
pct_113        <- pct_val["CA111"]                                   # 8.0%
pct_114_num1   <- pct_val["CA111"] * pop_111_114                     # 880,000
pct_114_num2   <- pct_val["CA112"] * pop_112_114                     # 3,300,000
pct_114        <- (pct_114_num1 + pct_114_num2) / pop_114            # 10.2%

ex <- list(
  pop_ca111 = unname(src_pop["CA111"]), pop_ca112 = unname(src_pop["CA112"]),
  abs_ca111 = unname(abs_val["CA111"]), abs_ca112 = unname(abs_val["CA112"]),
  pct_ca111 = unname(pct_val["CA111"]), pct_ca112 = unname(pct_val["CA112"]),
  pop_111_113 = pop_111_113, pop_111_114 = pop_111_114, pop_112_114 = pop_112_114,
  abs_113 = unname(abs_113), abs_114_f111 = unname(abs_114_f111),
  abs_114_f112 = unname(abs_114_f112), abs_114 = unname(abs_114),
  pop_113 = pop_113, pop_114 = pop_114,
  pct_113 = unname(pct_113), pct_114 = unname(pct_114),
  pct_114_num1 = unname(pct_114_num1), pct_114_num2 = unname(pct_114_num2),
  pct_114_num_sum = unname(pct_114_num1 + pct_114_num2)
)


# ── Assemble the flat list used by the manuscript ──────────────────────────────
nums <- list(
  # Data Records ---------------------------------------------------------------
  nr_files = nr_files,
  nr_cols  = nr_cols,

  # Technical Validation 1: round-trip (validation_eurostat.R) -----------------
  rt_n_area           = rt$n_area,
  rt_n_pop            = rt$n_pop,
  rt_n_area_unchanged = rt$n_area_unchanged,
  rt_n_pop_unchanged  = rt$n_pop_unchanged,
  rt_n_steps          = rt$n_steps,
  rt_mape_area        = rt$mape_area,   # all-regions MAPE (%), keyed by weight code
  rt_mape_pop         = rt$mape_pop,

  # Technical Validation 2: Eurostat GDP/pop (validation_gdp_pop.R) -------------
  gp_n_regions        = unname(gp$n_regions["pop_singleyear"]),
  gp_pop_year         = gp$year_range$pop_singleyear,   # c(from, to)
  gp_gdp_year         = gp$year_range$gdp_singleyear,
  gp_mape_pop         = gp_mape_pop,    # single-year MAPE (%), keyed by weight
  gp_mape_gdp         = gp_mape_gdp,
  gp_gdp_minus_pop_pp = gp_gdp_minus_pop_pp,

  # Technical Validation 3: sliver robustness (robustness_slivers_sim.R) --------
  sl_baseline_mape_pct = sl$baseline_mape_pct,
  sl_rise_pp           = sl$rise_pp,
  sl_n_min             = sl$n_min,
  sl_n_max             = sl$n_max,
  sl_n_step            = sl$n_step,
  sl_n_reps            = sl$n_reps,
  sl_sliver_area_max   = sl$sliver_area_max,
  sl_cw_n_entries      = sl$cw_n_entries,

  # Methods worked examples ----------------------------------------------------
  ex = ex
)

dir.create(stats_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(nums, file.path(stats_dir, "intext_numbers.rds"))
cat("Consolidated in-text numbers saved to ",
    file.path(stats_dir, "intext_numbers.rds"), "\n", sep = "")

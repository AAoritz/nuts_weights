# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     validation_gdp_pop.R
# │  Objective  Validation (version transition): convert NUTS-3 GDP and
# │             population from NUTS 2021 to 2024 with each of the six weights
# │             and compare against observed 2024 figures, restricted to
# │             regions that actually changed between the two versions.
# │  Output     figs/validation_gdp_pop_multiyear.png
# │             figs/validation_gdp_pop_singleyear.png
# │             data/stats/gdp_pop_stats.{csv,rds}
# └────────────────────────────────────────────────────────────────────────────

rm(list = ls())

library(tidyverse)
library(readxl)
library(nuts)
library(patchwork)
library(scales)
library(extrafont)

loadfonts(quiet = TRUE)

weight_names  <- c("areaKm", "pop11", "pop21", "artif_surf18", "artif_surf12", "bu_vol")
weight_labels <- c(
  "areaKm"       = "Area",
  "pop11"        = "Population 2011",
  "pop21"        = "Population 2021",
  "artif_surf18" = "Artificial Surface 2018",
  "artif_surf12" = "Artificial Surface 2012",
  "bu_vol"       = "Built-up Volume"
)

DATA_DIR <- "data/gdp_population_nuts_3_v2021_v2024"


# =============================================================================
# 1. HELPERS
# =============================================================================

# Read a Eurostat bulk-download Excel file.
# Row 1 contains the header: NUTS | Country | <year1> | <year2> | ...
# Returns a tidy long data frame: geo | year | <value_col>
read_eurostat_excel <- function(path, value_col) {
  sheet <- excel_sheets(path)[1]
  peek <- suppressMessages(
    read_excel(path, sheet = sheet, n_max = 5, col_names = FALSE)
  )
  header_row <- 1L
  for (i in seq_len(nrow(peek))) {
    if (any(grepl("^\\d{4}$", as.character(peek[i, ])))) {
      header_row <- i
      break
    }
  }
  raw <- suppressMessages(
    read_excel(path, sheet = sheet, skip = header_row - 1L)
  )
  names(raw)[1] <- "geo_code"
  if (ncol(raw) > 1 && !grepl("^\\d{4}$", names(raw)[2])) {
    raw <- raw[, -2]
  }
  raw <- filter(raw, nchar(trimws(geo_code)) == 5)
  raw$geo_code <- trimws(raw$geo_code)
  year_cols <- grep("^\\d{4}$", names(raw), value = TRUE)
  raw %>%
    select(geo = geo_code, all_of(year_cols)) %>%
    pivot_longer(-geo, names_to = "year", values_to = value_col) %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(.data[[value_col]]), .data[[value_col]] > 0)
}


# =============================================================================
# 2. LOAD DATA
# =============================================================================

cat("Reading Excel files...\n")

pop_v2021 <- read_eurostat_excel(
  file.path(DATA_DIR,
    "SNPTN - TOTAL - NR - Population on 1st January by broad age group and sex - NUTS_2021.xlsx"),
  "pop")
pop_v2024 <- read_eurostat_excel(
  file.path(DATA_DIR,
    "SNPTN - TOTAL - NR - Population on 1st January by broad age group and sex - NUTS_2024.xlsx"),
  "pop")
gdp_v2021 <- read_eurostat_excel(
  file.path(DATA_DIR,
    "SUVGD - MIO_EUR - GDP at current market prices - NUTS_2021.xlsx"),
  "gdp")
gdp_v2024 <- read_eurostat_excel(
  file.path(DATA_DIR,
    "SUVGD - MIO_EUR - GDP at current market prices - NUTS_2024.xlsx"),
  "gdp")

cat("pop_v2021:", nrow(pop_v2021), "rows | years:",
    paste(sort(unique(pop_v2021$year)), collapse = ", "), "\n")
cat("pop_v2024:", nrow(pop_v2024), "rows | years:",
    paste(sort(unique(pop_v2024$year)), collapse = ", "), "\n")
cat("gdp_v2021:", nrow(gdp_v2021), "rows | years:",
    paste(sort(unique(gdp_v2021$year)), collapse = ", "), "\n")
cat("gdp_v2024:", nrow(gdp_v2024), "rows | years:",
    paste(sort(unique(gdp_v2024$year)), collapse = ", "), "\n")


# =============================================================================
# 3. IDENTIFY CHANGED REGIONS (NUTS 2021 -> 2024)
# =============================================================================

data("cross_walks")

cw_21_24 <- cross_walks %>%
  filter(level == 3, from_version == 2021, to_version == 2024) %>%
  group_by(from_code) %>% mutate(w_area = areaKm / sum(areaKm)) %>% ungroup() %>%
  group_by(from_code) %>% mutate(n_from = n()) %>% ungroup() %>%
  group_by(to_code)   %>% mutate(n_to   = n()) %>% ungroup() %>%
  mutate(change_type = case_when(
    from_code == to_code         ~ "Unchanged",
    w_area == 1 & n_to == 1     ~ "Recoded",
    w_area == 1 & n_to  > 1     ~ "Merged",
    w_area  > 0 & w_area < 1    ~ "Boundary Shifted",
    TRUE                         ~ "Other"
  ))

cat("\nChange type distribution (NUTS 2021 -> 2024, NUTS 3):\n")
print(count(cw_21_24, change_type))

changed_from_codes <- cw_21_24 %>%
  filter(change_type %in% c("Boundary Shifted", "Merged")) %>%
  pull(from_code) %>% unique()

changed_to_codes <- cw_21_24 %>%
  filter(change_type %in% c("Boundary Shifted", "Merged")) %>%
  pull(to_code) %>% unique()

countries_with_changes <- cw_21_24 %>%
  filter(change_type %in% c("Boundary Shifted", "Merged")) %>%
  count(country)

cat("\nChanged source regions (NUTS 2021):", length(changed_from_codes), "\n")
cat("Changed target regions (NUTS 2024):", length(changed_to_codes), "\n")
cat("Countries affected by changes:\n")
print(countries_with_changes)

# Diagnose: which changed target regions also receive contributions from
# NUTS 2021 source regions that are NOT in changed_from_codes?
mixed_targets <- cw_21_24 %>%
  filter(to_code %in% changed_to_codes) %>%
  group_by(to_code) %>%
  summarise(
    n_sources        = n(),
    n_changed_src    = sum(from_code %in% changed_from_codes),
    n_unchanged_src  = sum(!from_code %in% changed_from_codes),
    pct_area_changed = sum(areaKm[from_code %in% changed_from_codes]) /
                       sum(areaKm) * 100,
    .groups = "drop"
  ) %>%
  filter(n_unchanged_src > 0)

cat("\nChanged targets that also draw from unchanged sources:\n")
print(mixed_targets, n = 50)


# =============================================================================
# 4. SELECT ALL VALID YEARS FOR EACH VARIABLE
# =============================================================================
# Use every year where both the NUTS 2021 source file and the NUTS 2024 truth
# file contain observations for the changed regions. Cap at 2023 to avoid
# provisional figures; require at least 5 jointly-covered regions per year.
# GDP data at NUTS 3 is published with a lag, so its window is narrower.

valid_years <- function(source_df, from_codes, truth_df, to_codes,
                        max_year = 2023, min_regions = 5L) {
  yrs <- sort(unique(source_df$year[source_df$year <= max_year]))
  keep <- vapply(yrs, function(yr) {
    n_s <- length(intersect(from_codes, source_df$geo[source_df$year == yr]))
    n_t <- length(intersect(to_codes,   truth_df$geo[truth_df$year   == yr]))
    min(n_s, n_t) >= min_regions
  }, logical(1))
  yrs[keep]
}

YEARS_POP <- valid_years(pop_v2021, changed_from_codes, pop_v2024, changed_to_codes)
YEARS_GDP <- valid_years(gdp_v2021, changed_from_codes, gdp_v2024, changed_to_codes)

cat("\nValid years (population):", paste(YEARS_POP, collapse = ", "), "\n")
cat("Valid years (GDP):       ", paste(YEARS_GDP, collapse = ", "), "\n")


# =============================================================================
# 5. PREPARE FULL TIME-SERIES SOURCE AND TRUTH DATA
# =============================================================================
# Provide ALL NUTS 2021 regions as source so that target regions which receive
# contributions from both changed and unchanged sources are fully covered.
# Errors are evaluated only for changed_to_codes AND only for (target, year)
# pairs where every contributing NUTS 2021 source region has data — otherwise
# missing_rm = TRUE silently drops contributions and produces spurious -100%
# errors (converted value = 0 because no source had data that year).

pop_source <- pop_v2021 %>% filter(year %in% YEARS_POP)
gdp_source <- gdp_v2021 %>% filter(year %in% YEARS_GDP)

# (target, year) pairs with 100% area-weighted source coverage
fully_covered <- function(source_df, cw_all, to_codes, years) {
  present <- source_df %>% distinct(geo, year)
  cw_all %>%
    filter(to_code %in% to_codes) %>%
    select(to_code, from_code, w_area) %>%
    crossing(year = years) %>%
    mutate(has_data = paste(from_code, year) %in%
             paste(present$geo, present$year)) %>%
    group_by(to_code, year) %>%
    summarise(pct_covered = sum(w_area[has_data]) / sum(w_area) * 100,
              .groups = "drop") %>%
    filter(pct_covered == 100) %>%
    select(geo = to_code, year)
}

pop_covered <- fully_covered(pop_source, cw_21_24, changed_to_codes, YEARS_POP)
gdp_covered <- fully_covered(gdp_source, cw_21_24, changed_to_codes, YEARS_GDP)

pop_truth <- pop_v2024 %>%
  filter(year %in% YEARS_POP, geo %in% changed_to_codes) %>%
  rename(pop_truth = pop) %>%
  semi_join(pop_covered, by = c("geo", "year"))

gdp_truth <- gdp_v2024 %>%
  filter(year %in% YEARS_GDP, geo %in% changed_to_codes) %>%
  rename(gdp_truth = gdp) %>%
  semi_join(gdp_covered, by = c("geo", "year"))

cat("\npop truth region-years (fully covered):", nrow(pop_truth), "\n")
cat("gdp truth region-years (fully covered):", nrow(gdp_truth), "\n")
cat("\nGDP coverage by target and year:\n")
gdp_covered %>% count(year) %>% print()


# =============================================================================
# 6. CONVERT NUTS 2021 -> 2024 FOR EACH WEIGHT, ACROSS ALL YEARS
# =============================================================================
# Classify all years at once with group_vars = "year", then convert in a
# single call. The year column is preserved through classification and
# conversion, so we can join directly with the truth data on (geo, year).
# Only changed_to_codes are retained via the inner_join with truth.

convert_and_compare <- function(source_df, var_col, truth_df, truth_col, w) {
  cl <- nuts_classify(source_df, nuts_code = "geo", group_vars = "year")
  cl$data$from_version <- "2021"

  converted <- nuts_convert_version(
    cl,
    to_version        = "2024",
    weight            = w,
    variables         = setNames("absolute", var_col),
    missing_rm        = TRUE,
    multiple_versions = "most_frequent"
  ) %>%
    select(geo = to_code, year, value = !!sym(var_col))

  converted %>%
    inner_join(truth_df, by = c("geo", "year")) %>%
    rename(truth = !!sym(truth_col)) %>%
    filter(!is.na(value), !is.na(truth), truth > 0) %>%
    mutate(
      rel_error     = (value - truth) / truth * 100,
      abs_rel_error = abs(rel_error),
      weight        = w,
      weight_label  = weight_labels[w]
    )
}

cat("\nRunning conversions (6 weights x 2 variables x all years)...\n")

results_pop <- map_dfr(weight_names,
  ~convert_and_compare(pop_source, "pop", pop_truth, "pop_truth", .x))

results_gdp <- map_dfr(weight_names,
  ~convert_and_compare(gdp_source, "gdp", gdp_truth, "gdp_truth", .x))

n_obs_pop <- nrow(results_pop) / length(weight_names)
n_obs_gdp <- nrow(results_gdp) / length(weight_names)

cat("Population:", n_obs_pop, "region-year observations per weight",
    "(", n_distinct(results_pop$geo), "regions x",
    n_distinct(results_pop$year), "years)\n")
cat("GDP:       ", n_obs_gdp, "region-year observations per weight",
    "(", n_distinct(results_gdp$geo), "regions x",
    n_distinct(results_gdp$year), "years)\n")


# =============================================================================
# 7. SUMMARY STATISTICS
# =============================================================================

summarise_errors <- function(df, label) {
  cat("\n===", label, "===\n")
  df %>%
    group_by(weight_label) %>%
    summarise(
      n_obs      = n(),
      n_regions  = n_distinct(geo),
      n_years    = n_distinct(year),
      MAPE       = round(mean(abs_rel_error,              na.rm = TRUE), 3),
      median_APE = round(median(abs_rel_error,            na.rm = TRUE), 3),
      p90_APE    = round(quantile(abs_rel_error, 0.90,   na.rm = TRUE), 3),
      bias       = round(mean(rel_error,                  na.rm = TRUE), 3),
      .groups    = "drop"
    ) %>%
    arrange(MAPE) %>%
    print()
}

summarise_errors(results_pop, "Population (NUTS 2021 -> 2024, all years)")
summarise_errors(results_gdp, "GDP (NUTS 2021 -> 2024, all years)")


# =============================================================================
# 8. FIGURES
#   fig_multiyear: Population | GDP, full time series (x-axis per plot)
#   fig_singleyear: Population | GDP, single year   (x-axis per plot)
# =============================================================================

# Single-year truth: most recent fully-covered year per region
latest_year_per_region <- function(covered_df) {
  covered_df %>%
    group_by(geo) %>%
    slice_max(year, n = 1, with_ties = FALSE) %>%
    ungroup()
}

pop_truth_1y <- pop_truth %>%
  semi_join(latest_year_per_region(pop_covered), by = c("geo", "year"))
gdp_truth_1y <- gdp_truth %>%
  semi_join(latest_year_per_region(gdp_covered), by = c("geo", "year"))

cat("\nSingle-year pop truth:", nrow(pop_truth_1y), "regions | years used:\n")
print(count(pop_truth_1y, year))
cat("Single-year gdp truth:", nrow(gdp_truth_1y), "regions | years used:\n")
print(count(gdp_truth_1y, year))

results_pop_1y <- map_dfr(weight_names,
  ~convert_and_compare(pop_source, "pop", pop_truth_1y, "pop_truth", .x))
results_gdp_1y <- map_dfr(weight_names,
  ~convert_and_compare(gdp_source, "gdp", gdp_truth_1y, "gdp_truth", .x))

summarise_errors(results_pop_1y, "Population single-year")
summarise_errors(results_gdp_1y, "GDP single-year")

# Weight order: shared across all plots (worst-to-best by pooled MAPE)
weight_order <- bind_rows(results_pop, results_gdp,
                          results_pop_1y, results_gdp_1y) %>%
  group_by(weight_label) %>%
  summarise(mape = mean(abs_rel_error, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mape)) %>%
  pull(weight_label)

# x-axis limits: log scale of |rel_error|, floored to avoid log(0) and
# capped at the 99th percentile across both panels for a shared sense of scale
xlim_for <- function(df) {
  v <- quantile(df$abs_rel_error[df$abs_rel_error > 0], 0.99, na.rm = TRUE)
  c(0.01, max(v, 1))
}

make_error_plot <- function(df) {
  xl <- xlim_for(df)

  mape_labels <- df %>%
    filter(!is.na(abs_rel_error)) %>%
    group_by(weight_label) %>%
    summarise(mape = mean(abs_rel_error, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      weight_label = factor(weight_label, levels = weight_order),
      label        = sprintf("MAPE %.1f%%", mape)
    )

  df %>%
    filter(!is.na(abs_rel_error), abs_rel_error >= xl[1]) %>%
    mutate(weight_label = factor(weight_label, levels = weight_order)) %>%
    ggplot(aes(x = abs_rel_error, y = weight_label, fill = weight_label)) +
    geom_boxplot(
      alpha         = 0.75,
      outlier.size  = 0.5,
      outlier.alpha = 0.3,
      width         = 0.55,
      show.legend   = FALSE
    ) +
    geom_text(
      data        = mape_labels,
      aes(x = xl[2], y = weight_label, label = label),
      hjust       = 1,
      vjust       = -0.55,
      size        = 3,
      colour      = "grey30",
      family      = "Times New Roman",
      inherit.aes = FALSE
    ) +
    coord_cartesian(xlim = xl) +
    scale_x_log10(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      x        = "Absolute relative error (%, log scale)",
      y        = NULL
    ) +
    theme_minimal(base_family = "Times New Roman", base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(size = 9)
    )
}

# ---------------------------------------------------------------------------
# Figure 1: full time series
# ---------------------------------------------------------------------------
gg_pop_multi <- make_error_plot(results_pop)
gg_gdp_multi <- make_error_plot(results_gdp)

fig_multiyear <- gg_pop_multi | gg_gdp_multi

ggsave(
  filename = "figs/validation_gdp_pop_multiyear.png",
  plot     = fig_multiyear,
  width    = 14,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)
cat("\nFigure saved to figs/validation_gdp_pop_multiyear.png\n")

# ---------------------------------------------------------------------------
# Figure 2: single year per region
# ---------------------------------------------------------------------------
gg_pop_1y <- make_error_plot(results_pop_1y)
gg_gdp_1y <- make_error_plot(results_gdp_1y)

fig_singleyear <- gg_pop_1y | gg_gdp_1y

ggsave(
  filename = "figs/validation_gdp_pop_singleyear.png",
  plot     = fig_singleyear,
  width    = 14,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)
cat("Figure saved to figs/validation_gdp_pop_singleyear.png\n")


# =============================================================================
# 10. EXPORT SUMMARY STATISTICS
# =============================================================================
# Saves a tidy CSV (human-readable) and an RDS named list (Quarto inline use).
# In the Quarto doc: stats <- readRDS("data/stats/gdp_pop_stats.rds")
# Then inline:       `r round(stats$mape["pop_pop21"], 1)`

dir.create("data/stats", showWarnings = FALSE, recursive = TRUE)

# --- Tidy table (one row per variable x weight) ----------------------------
make_summary_tbl <- function(df, variable) {
  df %>%
    group_by(weight, weight_label) %>%
    summarise(
      n_obs      = n(),
      n_regions  = n_distinct(geo),
      n_years    = n_distinct(year),
      mape       = round(mean(abs_rel_error,             na.rm = TRUE), 3),
      median_ape = round(median(abs_rel_error,           na.rm = TRUE), 3),
      p90_ape    = round(quantile(abs_rel_error, 0.90,   na.rm = TRUE), 3),
      bias       = round(mean(rel_error,                 na.rm = TRUE), 3),
      .groups    = "drop"
    ) %>%
    mutate(variable = variable, .before = 1)
}

stats_tbl <- bind_rows(
  make_summary_tbl(results_pop,    "population_multiyear"),
  make_summary_tbl(results_gdp,    "gdp_multiyear"),
  make_summary_tbl(results_pop_1y, "population_singleyear"),
  make_summary_tbl(results_gdp_1y, "gdp_singleyear")
)

write_csv(stats_tbl, "data/stats/gdp_pop_stats.csv")
cat("\nSummary table saved to data/stats/gdp_pop_stats.csv\n")
print(stats_tbl, n = 40)

# --- Named list for Quarto inline use --------------------------------------
# Naming convention: <variable>_<weight_short>
# e.g. mape["pop_pop21"], mape["gdp_area"]
weight_short <- c(
  "areaKm"       = "area",
  "pop11"        = "pop11",
  "pop21"        = "pop21",
  "artif_surf18" = "surf18",
  "artif_surf12" = "surf12",
  "bu_vol"       = "buvol"
)

make_named_vec <- function(df, prefix) {
  tbl <- df %>%
    group_by(weight) %>%
    summarise(mape       = round(mean(abs_rel_error,           na.rm = TRUE), 3),
              median_ape = round(median(abs_rel_error,         na.rm = TRUE), 3),
              p90_ape    = round(quantile(abs_rel_error, 0.90, na.rm = TRUE), 3),
              bias       = round(mean(rel_error,               na.rm = TRUE), 3),
              .groups    = "drop")
  list(
    mape       = setNames(tbl$mape,       paste0(prefix, "_", weight_short[tbl$weight])),
    median_ape = setNames(tbl$median_ape, paste0(prefix, "_", weight_short[tbl$weight])),
    p90_ape    = setNames(tbl$p90_ape,    paste0(prefix, "_", weight_short[tbl$weight])),
    bias       = setNames(tbl$bias,       paste0(prefix, "_", weight_short[tbl$weight]))
  )
}

pop_stats    <- make_named_vec(results_pop,    "pop")
gdp_stats    <- make_named_vec(results_gdp,    "gdp")
pop_1y_stats <- make_named_vec(results_pop_1y, "pop1y")
gdp_1y_stats <- make_named_vec(results_gdp_1y, "gdp1y")

stats <- list(
  mape       = c(pop_stats$mape,       gdp_stats$mape,
                 pop_1y_stats$mape,     gdp_1y_stats$mape),
  median_ape = c(pop_stats$median_ape, gdp_stats$median_ape,
                 pop_1y_stats$median_ape, gdp_1y_stats$median_ape),
  p90_ape    = c(pop_stats$p90_ape,    gdp_stats$p90_ape,
                 pop_1y_stats$p90_ape,  gdp_1y_stats$p90_ape),
  bias       = c(pop_stats$bias,       gdp_stats$bias,
                 pop_1y_stats$bias,     gdp_1y_stats$bias),
  # Sample sizes
  n_obs = c(
    pop_multiyear  = nrow(results_pop)    / length(weight_names),
    gdp_multiyear  = nrow(results_gdp)    / length(weight_names),
    pop_singleyear = nrow(results_pop_1y) / length(weight_names),
    gdp_singleyear = nrow(results_gdp_1y) / length(weight_names)
  ),
  n_regions = c(
    pop_multiyear  = n_distinct(results_pop$geo),
    gdp_multiyear  = n_distinct(results_gdp$geo),
    pop_singleyear = n_distinct(results_pop_1y$geo),
    gdp_singleyear = n_distinct(results_gdp_1y$geo)
  ),
  year_range = list(
    pop_multiyear  = range(results_pop$year),
    gdp_multiyear  = range(results_gdp$year),
    pop_singleyear = range(results_pop_1y$year),
    gdp_singleyear = range(results_gdp_1y$year)
  )
)

saveRDS(stats, "data/stats/gdp_pop_stats.rds")
cat("Named list saved to data/stats/gdp_pop_stats.rds\n")

cat("\nExample inline usage in Quarto:\n")
cat('  stats <- readRDS("data/stats/gdp_pop_stats.rds")\n')
cat("  Population MAPE (pop21 weight):  ", stats$mape["pop_pop21"],  "\n")
cat("  Population MAPE (area weight):   ", stats$mape["pop_area"],   "\n")
cat("  GDP MAPE (pop21 weight):         ", stats$mape["gdp_pop21"],  "\n")
cat("  GDP MAPE (area weight):          ", stats$mape["gdp_area"],   "\n")

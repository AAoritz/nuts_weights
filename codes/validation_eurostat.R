# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     validation_eurostat.R
# │  Objective  Validation (round-trip): convert NUTS-3 land area and population
# │             through the full version chain (2024→…→2006→…→2024) and back,
# │             measuring cumulative error after ten steps by interpolation
# │             weight. Residual deviation gauges information loss.
# │  Output     figs/validation_eurostat.png
# └────────────────────────────────────────────────────────────────────────────

rm(list = ls())

library(tidyverse)
library(nuts)
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

# Full version chain
CHAIN <- c("2024", "2021", "2016", "2013", "2010", "2006",
           "2010", "2013", "2016", "2021", "2024")


# =============================================================================
# DATA LOADING: Land Area and Population from the Crosswalk
# =============================================================================
#
# Summing the crosswalk's own areaKm and pop21 fields within each NUTS 2024
# source region gives exact NUTS 2024 totals with complete coverage.

data("cross_walks")

cw_2024_n3 <- cross_walks %>%
  filter(level == 3, from_version == 2024) %>%
  group_by(from_code) %>%
  summarise(
    area_km2 = sum(areaKm, na.rm = TRUE),
    pop21    = sum(pop21,  na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  rename(geo = from_code)

area_2024 <- cw_2024_n3 %>% select(geo, area_km2) %>% filter(area_km2 > 0)
pop_2024  <- cw_2024_n3 %>% select(geo, pop = pop21) %>% filter(pop > 0)

cat("NUTS 3 regions in crosswalk (land area):", nrow(area_2024), "\n")
cat("NUTS 3 regions in crosswalk (population):", nrow(pop_2024), "\n")


# =============================================================================
# ROUND-TRIP FUNCTION
# =============================================================================

do_round_trip <- function(start_dat, var_col, w) {

  dat       <- start_dat %>% rename(value = !!sym(var_col))
  vars_spec <- c(value = "absolute")

  for (i in seq(2, length(CHAIN))) {
    cl  <- nuts_classify(dat, nuts_code = "geo")
    cl$data$from_version <- CHAIN[i-1]

    dat <- nuts_convert_version(
      cl,
      to_version        = CHAIN[i],
      weight            = w,
      variables         = vars_spec,
      missing_rm        = TRUE,
      multiple_versions = "most_frequent"
    ) %>%
      select(geo = to_code, value)
  }

  start_dat %>%
    rename(orig = !!sym(var_col)) %>%
    inner_join(rename(dat, rt = value), by = "geo") %>%
    filter(!is.na(orig), !is.na(rt), orig > 0) %>%
    mutate(
      rel_error     = (rt - orig) / orig * 100,
      abs_rel_error = abs(rel_error),
      weight        = w,
      weight_label  = weight_labels[w],
      variable      = var_col
    )
}


# =============================================================================
# RUN ROUND-TRIP FOR ALL WEIGHT x VARIABLE COMBINATIONS
# =============================================================================

cat("\nRunning round-trip conversions (6 weights x 2 variables)...\n")

results_area <- map_dfr(weight_names, ~do_round_trip(area_2024, "area_km2", w = .x))
results_pop  <- map_dfr(weight_names, ~do_round_trip(pop_2024,  "pop",      w = .x))

results_b <- bind_rows(results_area, results_pop) %>%
  mutate(
    variable_label = case_when(
      variable == "area_km2" ~ "Land area (km2)",
      variable == "pop"      ~ "Population"
    )
  )


# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

# Threshold to identify genuinely changed regions (versus floating-point noise)
CHANGED_THRESHOLD <- 0.001   # percent

# Region counts
n_area         <- n_distinct(filter(results_b, variable == "area_km2")$geo)
n_pop          <- n_distinct(filter(results_b, variable == "pop")$geo)
n_area_changed <- n_distinct(filter(results_b, variable == "area_km2",
                                    abs_rel_error >= CHANGED_THRESHOLD)$geo)
n_pop_changed  <- n_distinct(filter(results_b, variable == "pop",
                                    abs_rel_error >= CHANGED_THRESHOLD)$geo)

cat("\n=== Round-trip error: ALL regions ===\n")
results_b %>%
  filter(!is.na(abs_rel_error)) %>%
  group_by(variable_label, weight_label) %>%
  summarise(
    MAPE       = round(mean(abs_rel_error,           na.rm = TRUE), 3),
    median_APE = round(median(abs_rel_error,         na.rm = TRUE), 3),
    p90_APE    = round(quantile(abs_rel_error, 0.90, na.rm = TRUE), 3),
    n_regions  = n_distinct(geo),
    .groups    = "drop"
  ) %>%
  arrange(variable_label, MAPE) %>%
  print(n = 50)

cat("\n=== Round-trip error: CHANGED regions only (abs_rel_error >= 0.001%) ===\n")
results_b %>%
  filter(!is.na(abs_rel_error), abs_rel_error >= CHANGED_THRESHOLD) %>%
  group_by(variable_label, weight_label) %>%
  summarise(
    MAPE       = round(mean(abs_rel_error,           na.rm = TRUE), 3),
    median_APE = round(median(abs_rel_error,         na.rm = TRUE), 3),
    p90_APE    = round(quantile(abs_rel_error, 0.90, na.rm = TRUE), 3),
    n_regions  = n_distinct(geo),
    .groups    = "drop"
  ) %>%
  arrange(variable_label, MAPE) %>%
  print(n = 50)


# =============================================================================
# FIGURE: ALL REGIONS — DOTS WITH LOG SCALE AND MAPE LABELS
# =============================================================================

var_levels <- c("Land area (km2)", "Population")

# Weight order based on all-regions MAPE; arrange(mape) = best first
# → level 1 = bottom of y-axis, worst at top
weight_order <- results_b %>%
  filter(!is.na(abs_rel_error)) %>%
  group_by(weight_label) %>%
  summarise(mape = mean(abs_rel_error, na.rm = TRUE), .groups = "drop") %>%
  arrange(mape) %>%
  pull(weight_label)

# MAPE per weight x variable — ALL regions (including unchanged ~0% ones)
mape_labels_all <- results_b %>%
  filter(!is.na(abs_rel_error)) %>%
  group_by(weight_label, variable_label) %>%
  summarise(mape = mean(abs_rel_error, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    weight_label   = factor(weight_label,   levels = weight_order),
    variable_label = factor(variable_label, levels = var_levels),
    label          = sprintf("%.2f%%", mape)
  )

# Dot data: only changed regions can be shown on log scale
# (unchanged regions have floating-point noise <1e-10% and cannot be
# meaningfully displayed on a log axis)
plot_data <- results_b %>%
  filter(!is.na(abs_rel_error), abs_rel_error >= 0.001) %>%
  mutate(
    weight_label   = factor(weight_label,   levels = weight_order),
    variable_label = factor(variable_label, levels = var_levels)
  )

gg_all <- ggplot(plot_data,
                 aes(x = abs_rel_error, y = weight_label, colour = variable_label)) +
  geom_point(
    position = position_jitterdodge(
      dodge.width   = 0.7,
      jitter.width  = 0,
      jitter.height = 0.12,
      seed          = 42
    ),
    size = 0.7, alpha = 0.18
  ) +
  geom_point(
    data     = mape_labels_all,
    aes(x = mape, colour = variable_label),
    position = position_dodge(width = 0.7),
    shape = 18, size = 4.5, alpha = 0.95
  ) +
  geom_text(
    data        = mape_labels_all,
    aes(x = mape, label = label, colour = variable_label),
    position    = position_dodge(width = 0.7),
    vjust       = -0.7,
    size        = 2.7,
    family      = "Times New Roman",
    show.legend = FALSE
  ) +
  scale_x_log10(
    limits = c(0.001, 1500),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_colour_manual(
    values = c("Land area (km2)" = "#4C72B0", "Population" = "#DD8452"),
    name   = "Variable"
  ) +
  labs(
    x       = "Absolute relative error (%, log scale)",
    y       = NULL
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 9)
  )

ggsave(
  filename = "figs/validation_eurostat.png",
  plot     = gg_all,
  width    = 9,
  height   = 5.5,
  dpi      = 300,
  bg       = "white"
)
cat("\nFigure saved to figs/validation_eurostat.png\n")


# =============================================================================
# IN-TEXT NUMBERS
# =============================================================================

cat("\n=== In-text numbers ===\n")
cat("Land area regions total / changed:", n_area, "/", n_area_changed, "\n")
cat("Population regions total / changed:", n_pop, "/", n_pop_changed, "\n")
cat("Conversion chain:", paste(CHAIN, collapse = "->"), "\n\n")

# All-regions MAPE (Panel A)
mape_all <- results_b %>%
  filter(!is.na(abs_rel_error)) %>%
  group_by(variable_label, weight_label) %>%
  summarise(mape = mean(abs_rel_error, na.rm = TRUE), .groups = "drop") %>%
  arrange(variable_label, mape)

cat("--- Panel A: MAPE over all regions ---\n")
for (v in unique(mape_all$variable_label)) {
  sub <- filter(mape_all, variable_label == v)
  cat(sprintf(
    "%s: best = %s (MAPE %.3f%%), worst = %s (MAPE %.2f%%)\n",
    v,
    sub$weight_label[1],         sub$mape[1],
    sub$weight_label[nrow(sub)], sub$mape[nrow(sub)]
  ))
}

# Changed-regions MAPE (Panel B)
mape_chg <- results_b %>%
  filter(!is.na(abs_rel_error), abs_rel_error >= CHANGED_THRESHOLD) %>%
  group_by(variable_label, weight_label) %>%
  summarise(mape = mean(abs_rel_error, na.rm = TRUE), .groups = "drop") %>%
  arrange(variable_label, mape)

cat("\n--- Panel B: MAPE over changed regions only ---\n")
for (v in unique(mape_chg$variable_label)) {
  sub <- filter(mape_chg, variable_label == v)
  cat(sprintf(
    "%s: best = %s (MAPE %.3f%%), worst = %s (MAPE %.2f%%)\n",
    v,
    sub$weight_label[1],         sub$mape[1],
    sub$weight_label[nrow(sub)], sub$mape[nrow(sub)]
  ))
}

# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     validation_heterogeneity.R
# │  Objective  Validation (heterogeneity): relate the region-level errors of
# │             the round-trip exercise to the characteristics of the regions
# │             — type of boundary change, country, share of the contributing
# │             territory retained, region size and contiguity — to show
# │             where conversion errors concentrate.
# │  Inputs     data/stats/roundtrip_regions.rds  (validation_eurostat.R)
# │  Output     figs/validation_heterogeneity.png
# │             data/stats/heterogeneity_stats.rds
# │             data/nuts3_2024_contiguity.csv    (cached geometry summary)
# └────────────────────────────────────────────────────────────────────────────

rm(list = ls())

library(tidyverse)
library(nuts)
source("codes/_nuts_source.R")
require_nuts_version()
library(patchwork)
library(scales)
library(extrafont)
loadfonts(quiet = TRUE)

weight_labels <- c(
  "areaKm"       = "Area",
  "artif_surf12" = "Artificial Surface 2012",
  "artif_surf18" = "Artificial Surface 2018",
  "bu_vol"       = "Built-up Volume",
  "pop11"        = "Population 2011",
  "pop21"        = "Population 2021"
)

# Errors below this value (in %) are floating-point noise; they are counted as
# zero and plotted at the floor on logarithmic axes.
ERR_FLOOR <- 0.001

need <- "data/stats/roundtrip_regions.rds"
if (!file.exists(need)) stop("Run validation_eurostat.R first.")
rt <- readRDS(need)

data("cross_walks")
cw3 <- filter(cross_walks, level == 3)


# =============================================================================
# 1. REGION CHARACTERISTICS (NUTS 3, version 2024)
# =============================================================================
# Each NUTS 2024 region is compared with every earlier version v. With respect
# to v the region is
#   Unchanged       one partner region in v, which maps only to this region;
#   Merger          a union of several complete regions of v;
#   Split           part of a single region of v that was divided;
#   Boundary shift  any other (many-to-many) relation.
# The retained share is the part of the partner regions' area that lies inside
# the region: 1 for unchanged and merged regions, small for a region that
# receives a minor piece of a larger territory.
# The round trip passes through all versions, so each region is summarised by
# its most complex relation and its lowest retained share across versions.

earlier <- c("2006", "2010", "2013", "2016", "2021")

classify_against <- function(v) {
  # Area and number of 2024 "children" of each region in version v
  parent <- cw3 %>%
    filter(from_version == v, to_version == "2024") %>%
    group_by(to_code_v = from_code) %>%
    summarise(n_children = n(), parent_area = sum(areaKm), .groups = "drop")

  cw3 %>%
    filter(from_version == "2024", to_version == v) %>%
    left_join(parent, by = c("to_code" = "to_code_v")) %>%
    group_by(geo = from_code, country) %>%
    summarise(
      n_partners   = n(),
      max_children = max(n_children),
      retained     = sum(areaKm) / sum(parent_area),
      .groups      = "drop"
    ) %>%
    mutate(
      version = v,
      type = case_when(
        n_partners == 1 & max_children == 1 ~ "Unchanged",
        n_partners  > 1 & max_children == 1 ~ "Merger",
        n_partners == 1 & max_children  > 1 ~ "Split",
        TRUE                                ~ "Boundary shift"
      )
    )
}

type_levels <- c("Unchanged", "Merger", "Boundary shift", "Split")

regions <- map_dfr(earlier, classify_against) %>%
  mutate(type = factor(type, levels = type_levels)) %>%
  group_by(geo, country) %>%
  summarise(
    # most complex relation: Split > Boundary shift > Merger > Unchanged
    type       = type_levels[max(as.integer(type))],
    retained   = pmin(1, min(retained)),
    .groups    = "drop"
  ) %>%
  mutate(type = factor(type, levels = type_levels))

# Region size: 2021 population and area of the NUTS 2024 region
size <- cw3 %>%
  filter(from_version == "2024", to_version == "2021") %>%
  group_by(geo = from_code) %>%
  summarise(pop21 = sum(pop21), area_km2 = sum(areaKm), .groups = "drop")

regions <- left_join(regions, size, by = "geo")

# Contiguity: share of a region's area in its largest polygon. Derived from the
# GISCO geometries (1:10 million) and cached, so the download happens once.
contig_file <- "data/nuts3_2024_contiguity.csv"
if (!file.exists(contig_file)) {
  contig <- tryCatch({
    library(sf)
    shp <- giscoR::gisco_get_nuts(year = "2024", nuts_level = 3,
                                  resolution = "10", epsg = "3035")
    parts <- suppressWarnings(st_cast(st_cast(shp, "MULTIPOLYGON"), "POLYGON"))
    parts$part_area <- as.numeric(st_area(parts))
    parts %>%
      st_set_geometry(NULL) %>%
      group_by(geo = NUTS_ID) %>%
      summarise(n_parts = n(),
                largest_part_share = max(part_area) / sum(part_area),
                .groups = "drop")
  }, error = function(e) {
    message("Contiguity not computed (", conditionMessage(e), ")")
    NULL
  })
  if (!is.null(contig)) write_csv(contig, contig_file)
}
if (file.exists(contig_file)) {
  regions <- left_join(regions, read_csv(contig_file, show_col_types = FALSE),
                       by = "geo") %>%
    # non-contiguous: more than 5% of the area lies outside the largest polygon
    mutate(noncontiguous = largest_part_share < 0.95)
} else {
  regions <- mutate(regions, noncontiguous = NA)
}

cat("\nNUTS 3 regions (2024) by type of change:\n")
print(count(regions, type))


# =============================================================================
# 2. ROUND-TRIP ERRORS BY REGION CHARACTERISTICS
# =============================================================================
# Variable: 2021 population. The Population 2021 weight reproduces it exactly
# by construction, so the comparison uses the five other weights, which mimic
# the usual situation where the variable of interest is not the covariate.

rt_pop <- rt %>%
  filter(variable == "pop", weight != "pop21") %>%
  select(geo, weight, abs_rel_error) %>%
  inner_join(regions, by = "geo") %>%
  mutate(
    weight_label = weight_labels[weight],
    affected     = abs_rel_error >= ERR_FLOOR,
    err_plot     = pmax(abs_rel_error, ERR_FLOOR)
  )

by_type <- rt_pop %>%
  group_by(weight, type) %>%
  summarise(
    n          = n(),
    share_aff  = mean(affected) * 100,
    mape       = mean(abs_rel_error),
    median_ape = median(abs_rel_error),
    p90_ape    = quantile(abs_rel_error, 0.90),
    .groups    = "drop"
  )
cat("\nRound-trip population error by type of change and weight:\n")
print(by_type, n = 40)

# Regions that changed in a way that requires redistribution
changed <- rt_pop %>% filter(type %in% c("Split", "Boundary shift"))

by_country <- changed %>%
  filter(weight %in% c("areaKm", "pop11")) %>%
  group_by(country, weight, weight_label) %>%
  summarise(n = n(), mape = mean(abs_rel_error),
            median_ape = median(abs_rel_error), .groups = "drop")
cat("\nChanged regions by country:\n")
print(by_country %>% select(-weight_label) %>%
        pivot_wider(names_from = weight, values_from = c(mape, median_ape)) %>%
        arrange(desc(mape_areaKm)), n = 40)

# Rank correlations within changed regions
spearman <- function(x, y) unname(cor(x, y, method = "spearman", use = "complete.obs"))
cors <- changed %>%
  group_by(weight) %>%
  summarise(
    rho_retained = spearman(abs_rel_error, retained),
    rho_pop      = spearman(abs_rel_error, pop21),
    rho_area     = spearman(abs_rel_error, area_km2),
    .groups      = "drop"
  )
cat("\nSpearman correlation of error with region characteristics (changed regions):\n")
print(cors)

# Error by retained share (below / above one half)
by_retained <- changed %>%
  mutate(retained_low = retained < 0.5) %>%
  group_by(weight, retained_low) %>%
  summarise(n = n(), mape = mean(abs_rel_error),
            median_ape = median(abs_rel_error), .groups = "drop")
cat("\nChanged regions by retained share (< 0.5 vs >= 0.5):\n")
print(by_retained, n = 20)

# Contiguity
by_contig <- changed %>%
  filter(!is.na(noncontiguous)) %>%
  group_by(weight, noncontiguous) %>%
  summarise(n = n(), mape = mean(abs_rel_error),
            median_ape = median(abs_rel_error), .groups = "drop")
cat("\nChanged regions by contiguity:\n")
print(by_contig, n = 20)


# =============================================================================
# 3. FIGURE
# =============================================================================

col_type <- c("Boundary shift" = "#4C72B0", "Split" = "#DD8452")
col_wt   <- c("Area" = "#55A868", "Population 2011" = "#8172B3")
pct_lab  <- function(x) paste0(x, "%")
log_breaks <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)

theme_article <- theme_minimal(base_family = "Times New Roman", base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 11, face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 9),
    legend.margin    = margin(0, 0, 0, 0)
  )

# --- A: by type of change and weight ----------------------------------------
weight_order <- changed %>%
  group_by(weight_label) %>%
  summarise(m = mean(abs_rel_error), .groups = "drop") %>%
  arrange(m) %>% pull(weight_label)

p_a <- changed %>%
  mutate(weight_label = factor(weight_label, levels = weight_order),
         type = factor(type, levels = names(col_type))) %>%
  ggplot(aes(x = err_plot, y = weight_label, fill = type)) +
  geom_boxplot(alpha = 0.75, width = 0.65, outlier.size = 0.5,
               outlier.alpha = 0.35, position = position_dodge(width = 0.75)) +
  scale_x_log10(breaks = log_breaks, labels = pct_lab) +
  scale_fill_manual(values = col_type, name = "Type of change") +
  labs(title = "a  Type of boundary change",
       x = "Absolute relative error (%, log scale)",
       y = "Interpolation weight") +
  theme_article +
  theme(panel.grid.major.y = element_blank())

# --- B: by country -----------------------------------------------------------
country_n <- changed %>% distinct(geo, country) %>% count(country, name = "n_changed")

country_dat <- by_country %>%
  left_join(country_n, by = "country") %>%
  filter(n_changed >= 3) %>%
  mutate(mape_plot = pmax(mape, ERR_FLOOR))

country_order <- country_dat %>%
  filter(weight == "areaKm") %>% arrange(mape) %>%
  mutate(lab = sprintf("%s (%d)", country, n_changed))

p_b <- country_dat %>%
  mutate(country = factor(country, levels = country_order$country,
                          labels = country_order$lab)) %>%
  ggplot(aes(x = mape_plot, y = country, colour = weight_label)) +
  geom_line(aes(group = country), colour = "grey75", linewidth = 0.4) +
  geom_point(size = 2) +
  scale_x_log10(breaks = log_breaks, labels = pct_lab) +
  scale_colour_manual(values = col_wt, name = "Interpolation weight") +
  labs(title = "b  Country (number of changed regions)",
       x = "MAPE of changed regions (%, log scale)", y = NULL) +
  theme_article +
  theme(axis.text.y = element_text(size = 8))

# --- C / D: retained share and region size ----------------------------------
scatter_dat <- changed %>%
  filter(weight %in% c("areaKm", "pop11"))

scatter_panel <- function(xvar, title, xlab, log_x = FALSE) {
  g <- ggplot(scatter_dat,
              aes(x = .data[[xvar]], y = err_plot, colour = weight_label)) +
    geom_point(size = 0.8, alpha = 0.35) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                linewidth = 0.8, span = 1) +
    scale_y_log10(breaks = log_breaks, labels = pct_lab) +
    scale_colour_manual(values = col_wt, name = "Interpolation weight") +
    labs(title = title, x = xlab,
         y = "Absolute relative error (%, log scale)") +
    theme_article
  if (log_x) g + scale_x_log10(labels = label_comma()) else
    g + scale_x_continuous(labels = label_percent(accuracy = 1))
}

p_c <- scatter_panel("retained",
  "c  Share of contributing territory retained",
  "Retained share of the area of the partner regions")
p_d <- scatter_panel("pop21", "d  Region size",
  "Population of the region in 2021 (log scale)", log_x = TRUE)

fig <- (p_a | p_b) /
  ((p_c | p_d) + plot_layout(guides = "collect") & theme(legend.position = "bottom"))

ggsave("figs/validation_heterogeneity.png", fig,
       width = 10, height = 8.5, dpi = 300, bg = "white")
cat("\nFigure saved to figs/validation_heterogeneity.png\n")


# =============================================================================
# 4. EXPORT IN-TEXT NUMBERS
# =============================================================================

pick <- function(tbl, col, ...) {
  x <- filter(tbl, ...)
  stopifnot(nrow(x) == 1)
  x[[col]]
}
by_w <- function(tbl, col, ...) {
  x <- filter(tbl, ...)
  setNames(x[[col]], x$weight)
}

heterogeneity_stats <- list(
  # counts of NUTS 3 regions by type of change
  n_type          = setNames(count(regions, type, .drop = FALSE)$n, type_levels),
  n_changed       = n_distinct(changed$geo),
  # round trip, population, by type: MAPE / median / share affected, by weight
  mape_split      = by_w(by_type, "mape",       type == "Split"),
  mape_shift      = by_w(by_type, "mape",       type == "Boundary shift"),
  median_split    = by_w(by_type, "median_ape", type == "Split"),
  median_shift    = by_w(by_type, "median_ape", type == "Boundary shift"),
  p90_split       = by_w(by_type, "p90_ape",    type == "Split"),
  p90_shift       = by_w(by_type, "p90_ape",    type == "Boundary shift"),
  max_merger      = max(filter(rt_pop, type == "Merger")$abs_rel_error),
  n_unchanged_aff = n_distinct(filter(rt_pop, type == "Unchanged", affected)$geo),
  # correlations within changed regions, by weight
  rho_retained    = setNames(cors$rho_retained, cors$weight),
  rho_pop         = setNames(cors$rho_pop,      cors$weight),
  rho_area        = setNames(cors$rho_area,     cors$weight),
  # retained share below / above one half
  n_retained_low  = pick(by_retained, "n",    weight == "areaKm",  retained_low),
  mape_ret_low    = by_w(by_retained, "mape",       retained_low),
  mape_ret_high   = by_w(by_retained, "mape",       !retained_low),
  median_ret_low  = by_w(by_retained, "median_ape", retained_low),
  median_ret_high = by_w(by_retained, "median_ape", !retained_low),
  # contiguity (NA if geometries were unavailable)
  n_noncontig     = if (nrow(by_contig)) pick(by_contig, "n", weight == "areaKm", noncontiguous) else NA,
  median_noncontig = if (nrow(by_contig)) by_w(by_contig, "median_ape", noncontiguous) else NA,
  median_contig    = if (nrow(by_contig)) by_w(by_contig, "median_ape", !noncontiguous) else NA,
  # countries, changed regions (area and Population 2011 weights)
  country_tbl     = by_country
)

dir.create("data/stats", showWarnings = FALSE, recursive = TRUE)
saveRDS(heterogeneity_stats, "data/stats/heterogeneity_stats.rds")
cat("In-text numbers saved to data/stats/heterogeneity_stats.rds\n")

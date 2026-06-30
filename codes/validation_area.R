# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     validation_area.R
# │  Objective  Validation (area conservation): convert Eurostat NUTS-3 land
# │             area to a common version and verify it stays constant over
# │             time — a region's area should not move when only codes change.
# │  Output     console diagnostics only (no figure)
# └────────────────────────────────────────────────────────────────────────────

rm(list = ls())

library(tidyverse)
library(eurostat)
library(nuts)
library(ggpubr)
library(patchwork)
library(extrafont)

loadfonts(quiet = TRUE)


# -----------------------------------------------------------------------------
# 1. LOAD CONVERSION TABLE AND IDENTIFY CHANGED REGIONS
# -----------------------------------------------------------------------------

data("cross_walks")

# NUTS 3 regions that actually changed (boundary shift or merger) between
nuts3_changes <- cross_walks %>%
  filter(level == 3) %>%
  group_by(from_code, to_version) %>%
  mutate(w_area = areaKm / sum(areaKm)) %>%
  ungroup() %>%
  group_by(from_code, from_version) %>%
  mutate(n_from = n()) %>%          # how many target regions this source feeds
  ungroup() %>%
  group_by(to_code, to_version) %>%
  mutate(n_to = n()) %>%            # how many source regions feed this target
  ungroup() %>%
  mutate(change_type = case_when(
    from_code == to_code              ~ "Unchanged",
    w_area == 1 & n_to == 1          ~ "Recoded",
    w_area == 1 & n_to  > 1          ~ "Merged",
    w_area  > 0 & w_area < 1         ~ "Boundary Shifted",
    TRUE                             ~ "Other"
  ))

# Codes of regions that truly changed in geographic extent
nuts3_changes %>% 
  filter(change_type == "Other")

changed_from <- nuts3_changes %>%
  filter(change_type %in% c("Boundary Shifted", "Merged")) %>%
  pull(from_code) %>% unique()

changed_to <- nuts3_changes %>%
  filter(change_type %in% c("Boundary Shifted", "Merged")) %>%
  pull(to_code) %>% unique()

changed_from
changed_to

# -----------------------------------------------------------------------------
# 2. DOWNLOAD EUROSTAT DATA
# -----------------------------------------------------------------------------

# --- 2a. Land area (km²) at NUTS 3 -------------------------------------------
area <- get_eurostat("reg_area3", time_format = "num") %>%
  filter(landuse == "TOTAL",
         nchar(geo) == 5) %>%                     
  select(geo, year = TIME_PERIOD, area_km2 = values)


# demo <- get_eurostat("demo_r_pjangrp3", time_format = "num") %>%
#   filter(nchar(geo) == 5) %>% 
#   select(geo, year = TIME_PERIOD, names(.))

# Check changes over time
area %>% 
  group_by(geo) %>% 
  mutate(d_area_km2 = area_km2 - lag(area_km2),
         g_area_km2 = d_area_km2 / lag(area_km2)) %>% 
  ungroup() %>% 
  group_by(geo) %>% 
  summarise(tot_g_area_km2 = sum(abs(g_area_km2), na.rm = T)) %>% 
  ungroup() %>% 
  summary()

# How many years per geo code
area %>%
  mutate(country = substr(geo, 0, 2)) %>% 
  count(country, geo)  %>% 
  count(country, n) %>% 
  print(n = 200)
  


# -----------------------------------------------------------------------------
# 3. CONVERT AREA TO COMMON VERSION
# -----------------------------------------------------------------------------
# Idea: Area by year should be constant if every region is converted correctly
area_class <- area %>% 
  nuts_classify(nuts_code = "geo", group_vars = "year") 

# Check versions
area_class$versions_data %>% 
  group_by(country, year) %>%  
  slice(1) %>% 
  ungroup() %>% 
  count(from_version)

# Convert to common version
area_conv <- area_class %>% 
  nuts_convert_version(
    to_version        = "2024",
    variables         = c("area_km2" = "absolute"),
    missing_rm        = FALSE,
    multiple_versions = "most_frequent"
  )

# Check differences over time
area_conv <- area_conv %>% 
  group_by(to_code) %>% 
  mutate(d_area_km2 = area_km2 - lag(area_km2),
         g_area_km2 = d_area_km2 / lag(area_km2)) %>% 
  ungroup() 

# Check if this is the case for each region
area_conv %>% 
  summary()

# Check only regions that actually changed
area_conv %>% 
  filter(to_code %in% unique(changed_from, changed_to)) %>% 
  summary()

# Cumulative relative deviations from 0
area_conv_sum <- area_conv %>% 
  group_by(to_code, country) %>% 
  summarise(tot_g_area_km2 = sum(abs(g_area_km2), na.rm = T)) %>% 
  ungroup() 

area_conv_sum

# Report deviation by country
area_conv_sum %>% 
  group_by(country) %>% 
  summarise(tot_g_area_km2 = mean(tot_g_area_km2, na.rm = T)) %>% 
  ungroup() %>% 
  print(n = 200)






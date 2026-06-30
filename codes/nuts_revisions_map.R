# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     nuts_revisions_map.R
# │  Objective  Map source vs. target NUTS-3 boundaries across each successive
# │             version transition (2006→2010→…→2024, plus the 2006→2024 jump)
# │             to show where regional geographies actually changed.
# │  Output     figs/european_nuts_changes.png
# └────────────────────────────────────────────────────────────────────────────

rm(list = ls())

# Get data from Eurostat
library(eurostat)
library(tidyverse)
library(nuts)
library(sf)
library(giscoR)
library(viridis)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(janitor)
library(patchwork)  # for combining plots
library(ggtext)
library(readxl)


# LOAD NUTS DATA
#=====================
# Which countries changed their codes the most?
data("cross_walks")
cross_walks %>% 
  filter(nchar(from_code) == 4) %>% 
  filter(from_code != to_code) %>% 
  group_by(country) %>% 
  summarise(pop21 = sum(pop21)) %>% 
  ungroup() %>%
  arrange(desc(pop21)) %>% 
  print(n = 200)

data("cross_walks")
cross_walks %>% 
  filter(nchar(from_code) == 5) %>% 
  filter(from_code != to_code) %>% 
  group_by(country) %>% 
  summarise(pop21 = sum(pop21)) %>% 
  ungroup() %>%
  arrange(desc(pop21)) %>% 
  print(n = 200)

cross_walks %>% 
  filter(nchar(from_code) == 4) %>% 
  filter(from_code != to_code) %>% 
  filter(country == "Poland") %>% 
  group_by(from_version, to_version) %>% 
  tally() %>% print(n = 200)

# Country totals
country_totals <- cross_walks %>% 
  filter(from_version == 2021, to_version == 2021) %>% 
  filter(nchar(from_code) == 3) %>% 
  group_by(country) %>% 
  summarise(tot_pop21 = sum(pop21), 
            tot_areaKm = sum(areaKm)) %>% 
  ungroup() 

  
# ALL NUTS 3 VERSIONS
#--------------------
res = "10"
versions <- cross_walks %>% 
  distinct(from_version) %>% 
  pull(from_version)
versions

eu_3_maps  <- map(versions, \(x) 
    gisco_get_nuts(nuts_level = 3, epsg = "3857", resolution = res, year = x) %>% 
      clean_names() %>% mutate(from_version = x)
    )
names(eu_3_maps) <- versions

# Build names
nuts_names <- map(eu_3_maps, \(x) x %>% 
      st_set_geometry(NULL) %>% 
      select(geo, nuts_name, name_latn, from_version)) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  rename(code = geo) %>% 
  arrange(code)

# NUTS 1 WITH UK
eu_1_2021 <- eu_3_maps[["2021"]] %>% 
  mutate(country = substr(geo, 0, 2)) %>% 
  group_by(country) %>% 
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# NUTS 1 WITHOUT UK
eu_1_2024 <- eu_3_maps[["2024"]] %>% 
  mutate(country = substr(geo, 0, 2)) %>% 
  group_by(country) %>% 
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# Regex pattern to identify French overseas departments and Iberian islands
overseas_pattern <- "^(FR[9Y]|ES70|PT[23]0)"

# Sanity checks
cross_walks %>% 
  filter(level == 3) %>% 
  anti_join(nuts_names, by = c("from_code" = "code", "from_version")) 

cross_walks %>% 
  filter(level == 3) %>% 
  anti_join(nuts_names, ., by = c("code" = "from_code", "from_version")) %>% 
  distinct(code, nuts_name) %>% print(n = 200)

# Build reduced version of cross-walk
nuts_3_area_changes <- cross_walks %>% 
  filter(level == 3) %>% 
  # Keep only changes from one version to next and cumulative
  filter((from_version == "2006" & to_version == "2010") |
           (from_version == "2010" & to_version == "2013") |
           (from_version == "2013" & to_version == "2016") |
           (from_version == "2016" & to_version == "2021") |
           (from_version == "2021" & to_version == "2024") |
           (from_version == "2006" & to_version == "2024") ) %>%
  group_by(from_code, from_version, to_version) %>% 
  mutate(w_pop21 = pop21 / sum(pop21),
         w_areaKm = areaKm / sum(areaKm)) %>%
  ungroup() %>% 
  select(from_code, to_code, from_version, to_version, country, w_areaKm, names(.)) %>% 
  # Create label for facet
  mutate(version_flow_label = paste0(from_version, " → ", to_version), 
         version_flow_label = fct_relevel(version_flow_label, "2006 → 2024", after = Inf)) %>% 
  mutate(overseas = ifelse(grepl(overseas_pattern, from_code) | 
                             grepl(overseas_pattern, to_code), TRUE, FALSE)) 



# NUTS REVISIONS MAP
#===================================
nuts_3_area_changes <- nuts_3_area_changes %>%   
  # Keep only changes that affected area
  filter(w_areaKm < 1) 

# Filter Norway until glitch is cleared
nuts_3_area_changes <- nuts_3_area_changes %>% 
  filter(!(from_version == "2013" & to_version == "2016" & substr(from_code, 0, 2) == "NO"))

nuts_3_area_changes %>% 
  filter(from_version == "2006" & to_version == "2024") %>% 
  group_by(country) %>% 
  tally() %>% 
  print(n = 200)

from_version_year = "2006"
to_version_year = "2024"

nuts_3_area_changes %>% 
  filter(country == "Norway") %>% 
  print(n = 200)

nuts_3_area_changes %>% 
  filter(from_version == "2013" & to_version == "2016") %>% 
  filter(country == "Norway")
  
eu_3_maps[["2016"]] %>% 
  filter(cntr_code == "NO") %>% print(n = 200)


# Modified function with ETRS89-extended / LAEA Europe projection (EPSG:3035)
# This is the standard projection for European statistical mapping
plot_eu_from_to = function(from_version_year, to_version_year){
  
  # EU Base map
  # Kicks out UK in 2024 version
  eu_1 <- if(to_version_year == 2024){
    eu_1 <- eu_1_2024
  } else {
    eu_1 <- eu_1_2021 
  }
  
  nuts_3_area_changes_filtered <- nuts_3_area_changes %>% 
    filter(from_version == from_version_year & to_version == to_version_year)
  
  eu_from <- eu_3_maps[[from_version_year]] %>%   
    mutate(from_code = geo) %>%  
    inner_join(nuts_3_area_changes_filtered, by = c("from_code", "from_version"))
  
  eu_to <- eu_3_maps[[to_version_year]] %>%   
    # Crucial renamings
    mutate(to_code = geo) %>%  
    rename(to_version = from_version) %>% 
    inner_join(nuts_3_area_changes_filtered, by = c("to_code", "to_version")) %>% 
    distinct(to_code, .keep_all = T)
  
  # Transform to EPSG:3035 (ETRS89-extended / LAEA Europe)
  eu_1 <- st_transform(eu_1, crs = 3035)
  eu_from <- st_transform(eu_from, crs = 3035)
  eu_to <- st_transform(eu_to, crs = 3035)
  
  # Continental Europe bounds for EPSG:3035 (LAEA Europe)
  continental_bounds <- list(
    xmin = 2200000, xmax = 6500000,
    ymin = 1380000, ymax = 5500000
  )
  
  # Define color palette - using complementary colors that create attractive overlap
  from_color <- "#1f77b4"  # Professional blue
  to_color <- "#ff7f0e"    # Professional orange
  base_color <- "#F8F9FA"  # Light grey background
  border_color <- "#E1E5E9"  # Subtle border
  
  gg <- ggplot() + 
    geom_sf(data = eu_1, fill = base_color, color = border_color, linewidth = 0.2) + 
    geom_sf(data = eu_to, aes(color = "Target version"), 
            fill = alpha(to_color, 0.5), linewidth = 0.3) + 
    geom_sf(data = eu_from, aes(color = "Source version"), 
            fill = NA, linewidth = 0.4) + 
    coord_sf(
      xlim = c(continental_bounds$xmin, continental_bounds$xmax),
      ylim = c(continental_bounds$ymin, continental_bounds$ymax),
      crs = 3035,  # ETRS89-extended / LAEA Europe
      expand = FALSE
    ) +
    # Manual color scale for legend
    scale_color_manual(
      name = "",
      values = c("Source version" = from_color, "Target version" = to_color),
      breaks = c("Source version", "Target version")
    ) +
    theme_void() +
    theme(
      plot.title = element_markdown(size = 11, hjust = 0.5, margin = margin(b = 5)),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    # Labels
    #labs(title = paste(eu_from$from_version[1], "→", eu_to$to_version[1]))
    labs(title = paste0("Versions: <span style='color:#1f77b4'>**", eu_from$from_version[1], 
    "**</span> → <span style='color:#ff7f0e'>**",  eu_to$to_version[1],"**</span>"))
    
    
  return(gg)
}

# Create all 6 plots
p1 <- plot_eu_from_to("2006", "2010")
p2 <- plot_eu_from_to("2010", "2013")
p3 <- plot_eu_from_to("2013", "2016")
p4 <- plot_eu_from_to("2016", "2021")
p5 <- plot_eu_from_to("2021", "2024")
p6 <- plot_eu_from_to("2006", "2024")

# Arrange in 2 columns, 3 rows using patchwork
combined_plot <- (p1 | p2) / 
  (p3 | p4) / 
  (p5 | p6) + 
  plot_layout(guides = "collect") &  # This collects all legends into one
  theme(legend.position = "bottom")  # Position the single legend at bottom


ggsave(
  filename = "figs/european_nuts_changes.png",
  plot = combined_plot,
  width = 12,      # inches
  height = 16,     # inches  
  dpi = 300,       # high resolution for publication
  bg = "white",    # white background
  device = "png"
)


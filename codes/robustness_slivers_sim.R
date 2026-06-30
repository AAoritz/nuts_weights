# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     robustness_slivers_sim.R
# │  Objective  Stress-test the crosswalk by injecting spurious "sliver" flows
# │             and tracking how population-conversion error (MAPE) grows with
# │             the number of false positives added.
# │  Output     figs/robustness_slivers_sim.png
# └────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(nuts)

set.seed(42)

data(cross_walks)

# Ground truth: 2021 population within NUTS 2024 boundaries
pop_truth <- cross_walks %>%
  # Take any source version
  filter(from_version == 2010) %>%
  group_by(to_code, to_version) %>%
  summarise(pop_2021 = sum(pop21), .groups = "drop") %>%
  filter(to_version == 2024, nchar(to_code) == 5) %>%
  select(to_code, pop_2021)

# Population in 2006 NUTS3 regions (source for conversion)
pop_2006 <- cross_walks %>%
  # Take any source version
  filter(from_version == 2010) %>%
  group_by(to_code, to_version) %>%
  summarise(pop_2021 = sum(pop21), .groups = "drop") %>%
  filter(to_version == 2006, nchar(to_code) == 5) %>%
  select(from_code = to_code, pop_2021)

# True crosswalk: 2006 -> 2024 at NUTS3
cw_true <- cross_walks %>%
  filter(from_version == 2006, to_version == 2024, nchar(from_code) == 5)

# All unique (from_code, to_code) pairs available for sampling
all_pairs <- cw_true %>%
  select(from_code, to_code, country) %>%
  distinct()

# Area-weighted conversion (manual, so we can inject noise)
convert_pop <- function(cw, pop_source) {
  cw %>%
    group_by(from_code) %>%
    mutate(weight = areaKm / sum(areaKm)) %>%
    ungroup() %>%
    inner_join(pop_source, by = "from_code") %>%
    mutate(pop_allocated = pop_2021 * weight) %>%
    group_by(to_code) %>%
    summarise(pop_conv = sum(pop_allocated), .groups = "drop")
}

# Simulation
n_values <- seq(0, 2000, by = 100)
n_reps   <- 50

results <- map_dfr(n_values, function(n) {
  map_dfr(seq_len(n_reps), function(rep) {
    # Sample n random flows and assign uniform random areas in (0, 2)
    fake_flows <- all_pairs %>%
      slice_sample(n = n, replace = TRUE) %>%
      mutate(
        from_version = "2006",
        to_version   = "2024",
        areaKm       = runif(n, 0, 2)
      )

    cw_noisy <- bind_rows(cw_true, fake_flows)

    pop_conv <- convert_pop(cw_noisy, pop_2006)

    inner_join(pop_truth, pop_conv, by = "to_code") %>%
      mutate(rel_error = (pop_conv - pop_2021) / pop_2021) %>%
      summarise(mae = mean(abs(rel_error)), .groups = "drop") %>%
      mutate(n = n, rep = rep)
  })
})



# Summarise across replications
results_summary <- results %>%
  group_by(n) %>%
  summarise(
    mae_mean = mean(mae),
    mae_p25  = quantile(mae, 0.25),
    mae_p75  = quantile(mae, 0.75),
    mae_p05  = quantile(mae, 0.05),
    mae_p95  = quantile(mae, 0.95),
    .groups  = "drop"
  )

# Baseline MAE (no noise) — placed at n = 0
pop_conv_base <- convert_pop(cw_true, pop_2006)
baseline_mae  <- inner_join(pop_truth, pop_conv_base, by = "to_code") %>%
  mutate(rel_error = (pop_conv - pop_2021) / pop_2021) %>%
  summarise(mae = mean(abs(rel_error))) %>%
  pull(mae)

baseline_row <- tibble(
  n        = 0,
  mae_mean = baseline_mae,
  mae_p25  = baseline_mae,
  mae_p75  = baseline_mae,
  mae_p05  = baseline_mae,
  mae_p95  = baseline_mae
)

results_plot <- bind_rows(baseline_row, results_summary)

# Figure
p <- ggplot(results_plot, aes(x = n)) +
  geom_ribbon(aes(ymin = mae_p05, ymax = mae_p95), fill = "#4E84C4", alpha = 0.15) +
  geom_ribbon(aes(ymin = mae_p25, ymax = mae_p75), fill = "#4E84C4", alpha = 0.30) +
  geom_line(aes(y = mae_mean), color = "#4E84C4", linewidth = 0.8) +
  scale_x_continuous(breaks = c(0, n_values)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    x     = "Number of false positive slivers added",
    y     = "Mean absolute percentage error (MAPE)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figs/robustness_slivers_sim.png", p, width = 7, height = 4.5, dpi = 300)

cat("Baseline MAE (no noise):", round(baseline_mae * 100, 4), "%\n")
print(results_summary)

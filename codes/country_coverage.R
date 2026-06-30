# ┌────────────────────────────────────────────────────────────────────────────
# │  NUTS CONVERSION WEIGHTS
# │
# │  Script     country_coverage.R
# │  Objective  Document which countries appear in each NUTS version-pair
# │             conversion matrix; flag partial coverage, the duplicate Greece
# │             code (GR/EL), and expected exclusions (EU candidate countries).
# │  Output     data/country_coverage_summary.csv
# │             data/country_coverage_matrix.csv
# └────────────────────────────────────────────────────────────────────────────

library(tidyverse)

# ── 1. Country metadata ────────────────────────────────────────────────────────

# All countries that ever appear in any matrix file, with their status at the
# time of each NUTS revision. EU candidate countries (e.g. AL, ME, MK, RS, TR)
# are not expected in the data because Eurostat does not publish NUTS codes for
# candidate countries — only for EU member states, EFTA, and (until Brexit) UK.

country_meta <- tribble(
  ~code,  ~name,                   ~status,
  "AT",   "Austria",               "EU member",
  "BE",   "Belgium",               "EU member",
  "BG",   "Bulgaria",              "EU member (since 2007)",
  "CH",   "Switzerland",           "EFTA",
  "CY",   "Cyprus",                "EU member (since 2004)",
  "CZ",   "Czechia",               "EU member (since 2004)",
  "DE",   "Germany",               "EU member",
  "DK",   "Denmark",               "EU member",
  "EE",   "Estonia",               "EU member (since 2004)",
  "EL",   "Greece",                "EU member",
  "ES",   "Spain",                 "EU member",
  "FI",   "Finland",               "EU member",
  "FR",   "France",                "EU member",
  "GR",   "Greece (legacy code)",  "EU member — alias for EL, used in NUTS 2006",
  "HR",   "Croatia",               "EU member (since 2013)",
  "HU",   "Hungary",               "EU member (since 2004)",
  "IE",   "Ireland",               "EU member",
  "IS",   "Iceland",               "EFTA",
  "IT",   "Italy",                 "EU member",
  "LI",   "Liechtenstein",         "EFTA",
  "LT",   "Lithuania",             "EU member (since 2004)",
  "LU",   "Luxembourg",            "EU member",
  "LV",   "Latvia",                "EU member (since 2004)",
  "MT",   "Malta",                 "EU member (since 2004)",
  "NL",   "Netherlands",           "EU member",
  "NO",   "Norway",                "EFTA",
  "PL",   "Poland",                "EU member (since 2004)",
  "PT",   "Portugal",              "EU member",
  "RO",   "Romania",               "EU member (since 2007)",
  "SE",   "Sweden",                "EU member",
  "SI",   "Slovenia",              "EU member (since 2004)",
  "SK",   "Slovakia",              "EU member (since 2004)",
  "UK",   "United Kingdom",        "EU member until Brexit (2020)"
)

# ── 2. Read all NUTS-3 matrix files and extract country codes ──────────────────

matrix_dir <- here::here("data", "jrc_nuts_converter_matrices")

files_n3 <- list.files(matrix_dir, pattern = "^n3_cm_v.*\\.csv$", full.names = TRUE)

extract_versions <- function(filename) {
  # e.g. "n3_cm_v2016_v2021.csv" -> c("2016", "2021")
  stringr::str_extract_all(basename(filename), "\\d{4}")[[1]]
}

coverage_long <- map_dfr(files_n3, function(f) {
  versions <- extract_versions(f)
  df <- read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE)
  countries <- union(
    substr(df[[1]], 1, 2),  # source codes
    substr(df[[2]], 1, 2)   # target codes
  ) |> unique() |> sort()
  tibble(
    version_from = versions[1],
    version_to   = versions[2],
    country_code = countries
  )
})

# ── 3. Build a wide presence matrix ───────────────────────────────────────────

version_pairs <- coverage_long |>
  distinct(version_from, version_to) |>
  mutate(pair = paste0(version_from, "→", version_to))

all_codes <- sort(unique(coverage_long$country_code))

presence_wide <- coverage_long |>
  mutate(pair = paste0(version_from, "→", version_to), present = TRUE) |>
  select(country_code, pair, present) |>
  pivot_wider(names_from = pair, values_from = present, values_fill = FALSE)

# ── 4. Summarise overall country coverage ─────────────────────────────────────

n_pairs <- nrow(version_pairs)

country_summary <- presence_wide |>
  mutate(
    n_pairs_present = rowSums(across(-country_code)),
    coverage        = n_pairs_present / n_pairs,
    full_coverage   = n_pairs_present == n_pairs
  ) |>
  left_join(country_meta, by = c("country_code" = "code")) |>
  arrange(desc(full_coverage), country_code) |>
  select(country_code, name, status, n_pairs_present, coverage, full_coverage,
         everything())

# ── 5. Print reports ───────────────────────────────────────────────────────────

cat("========================================================================\n")
cat("NUTS CONVERSION MATRIX — COUNTRY COVERAGE REPORT\n")
cat("========================================================================\n\n")

cat(sprintf("Total version-pair tables (NUTS-3): %d\n", n_pairs))
cat(sprintf("Total unique country codes found:     %d\n\n", length(all_codes)))

# 5a. Full coverage
cat("── COUNTRIES WITH FULL COVERAGE (present in all", n_pairs, "matrices) ──────────\n")
full <- country_summary |> filter(full_coverage)
print(full |> select(country_code, name, status), n = Inf)
cat(sprintf("\n%d countries with full coverage.\n\n", nrow(full)))

# 5b. Partial coverage
cat("── COUNTRIES WITH PARTIAL COVERAGE ─────────────────────────────────────\n")
partial <- country_summary |> filter(!full_coverage)
print(partial |> select(country_code, name, status, n_pairs_present, coverage), n = Inf)
cat("\n")

# 5c. Detail: which pairs is each partial-coverage country missing from?
cat("── MISSING PAIRS PER PARTIAL-COVERAGE COUNTRY ───────────────────────────\n")
pair_cols <- names(presence_wide)[-1]  # all pair columns
for (cc in partial$country_code) {
  row <- presence_wide |> filter(country_code == cc)
  missing_pairs <- pair_cols[!unlist(row[pair_cols])]
  cat(sprintf("  %s: absent from -> %s\n", cc, paste(missing_pairs, collapse = ", ")))
}
cat("\n")

# 5d. GR / EL note
cat("── DUPLICATE CODE NOTE: GR vs EL (Greece) ───────────────────────────────\n")
gr_pairs <- coverage_long |> filter(country_code == "GR") |>
  mutate(pair = paste0(version_from, "→", version_to)) |> pull(pair)
el_pairs <- coverage_long |> filter(country_code == "EL") |>
  mutate(pair = paste0(version_from, "→", version_to)) |> pull(pair)
cat("  GR (legacy Eurostat code for Greece) appears in:\n")
cat(sprintf("    %s\n", paste(gr_pairs, collapse = ", ")))
cat("  EL (current Eurostat code for Greece) appears in:\n")
cat(sprintf("    %s\n", paste(el_pairs, collapse = ", ")))
cat("  NOTE: GR and EL refer to the same country. GR is used in the source\n")
cat("  boundary files for NUTS 2006; EL is used from NUTS 2010 onward.\n")
cat("  Code that extracts country prefixes should treat GR as equivalent to EL.\n\n")

# 5e. Expected countries not found
cat("── EXPECTED COUNTRIES NOT FOUND IN ANY MATRIX ───────────────────────────\n")
# EU candidate countries as of NUTS 2024 revision cycle
candidates <- tribble(
  ~code, ~name,             ~reason_absent,
  "AL",  "Albania",         "EU candidate — no NUTS codes assigned",
  "BA",  "Bosnia-Herzegovina", "EU candidate — no NUTS codes assigned",
  "ME",  "Montenegro",      "EU candidate — no NUTS codes assigned",
  "MK",  "North Macedonia", "EU candidate — no NUTS codes assigned",
  "MD",  "Moldova",         "EU candidate — no NUTS codes assigned",
  "RS",  "Serbia",          "EU candidate — no NUTS codes assigned",
  "TR",  "Turkey",          "EU candidate — no NUTS codes assigned",
  "UA",  "Ukraine",         "EU candidate — no NUTS codes assigned",
  "XK",  "Kosovo",          "Potential candidate — no NUTS codes assigned"
)
print(candidates, n = Inf)
cat("\n  NOTE: Candidate countries are excluded because Eurostat does not\n")
cat("  publish official NUTS codes for them. Their regional statistics are\n")
cat("  reported under separate nomenclatures (e.g. NUTS-like codes for\n")
cat("  Western Balkans), and they are therefore outside the scope of this\n")
cat("  dataset.\n\n")

# ── 6. Summary table for article ──────────────────────────────────────────────

cat("========================================================================\n")
cat("SUMMARY TABLE (for use in article)\n")
cat("========================================================================\n\n")

article_table <- country_summary |>
  mutate(
    coverage_note = case_when(
      country_code == "UK" ~
        "Present in matrices up to NUTS 2021; absent from all NUTS 2024 pairs (post-Brexit)",
      country_code == "LI" ~
        "Present in matrices not involving NUTS 2021; absent from all NUTS 2021 pairs",
      country_code == "GR" ~
        "Legacy code for Greece used in NUTS 2006 source files; superseded by EL",
      full_coverage ~ "Full coverage",
      TRUE ~ paste0("Partial: ", n_pairs_present, "/", n_pairs, " matrices")
    )
  ) |>
  # Merge GR into the EL row for cleanliness
  filter(country_code != "GR") |>
  select(country_code, name, status, coverage_note)

print(article_table, n = Inf)

# ── 7. Save outputs ────────────────────────────────────────────────────────────

write_csv(article_table, here::here("data", "country_coverage_summary.csv"))
write_csv(presence_wide, here::here("data", "country_coverage_matrix.csv"))

cat("\nOutputs written to:\n")
cat("  data/country_coverage_summary.csv\n")
cat("  data/country_coverage_matrix.csv\n")

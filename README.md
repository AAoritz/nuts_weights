# Conversion of NUTS codes

## Introduction

This repository contains the source for a data descriptor article on **conversion
weights for European regional NUTS data** (Nomenclature of Territorial Units for
Statistics), targeting submission to *Nature Scientific Data*. The weights make it
possible to convert statistics between different NUTS versions and across the NUTS
1/2/3 levels, so that regional time series remain comparable despite the periodic
boundary revisions of the classification.

The conversion weights are distributed through the **`nuts` R package**, available
on rOpenSci: <https://github.com/ropensci/nuts>. This repository holds the article
itself (Quarto source and rendered output) together with the R scripts that
generate every figure and data summary it contains.

## Repository content

- `nuts_article.qmd` — Quarto source file for the data descriptor (submit this one).
- `nuts_article.pdf` — Rendered data descriptor.
- `bibliography.bib` — References in BibTeX format.
- `codes/` — R scripts that generate the figures and data summaries.
- `data/` — Input data and shapefiles, generated CSV summaries, and the
  `data/stats/` files that feed the in-text numbers of the manuscript.
- `figs/` — Figure outputs plus source files (SVG, MMD).

## Rendering the article

```bash
# Render to PDF (main output)
quarto render nuts_article.qmd --to scientific-data-pdf

# Render to Word
quarto render nuts_article.qmd --to docx
```

## Running the code

All figure- and data-generating scripts live in `codes/` and are driven by a master
script, `codes/run_all.R`. Run it from the **project root** so that the relative
paths (`figs/`, `data/`) resolve correctly. Outputs are written to `figs/` and
`data/`.

```bash
# Run every script
Rscript codes/run_all.R

# Run only scripts that produce a figure
Rscript codes/run_all.R figures

# Run only the validation scripts
Rscript codes/run_all.R validation

# Run one or more named scripts (with or without the .R extension)
Rscript codes/run_all.R nuts_revisions_map validation_eurostat
```

Inside an interactive R session you can instead source the master script and call
`run_all()`:

```r
source("codes/run_all.R")   # defines run_all(); does not auto-run
run_all()                   # run everything
run_all("validation_eurostat")  # run a subset
```

Individual scripts can also be run on their own from the project root:

| Script | Output |
| --- | --- |
| `codes/country_coverage.R` | `data/country_coverage_summary.csv`, `data/country_coverage_matrix.csv` |
| `codes/nuts_revisions_map.R` | `figs/european_nuts_changes.png` |
| `codes/robustness_slivers_sim.R` | `figs/robustness_slivers_sim.png`, `data/stats/slivers_stats.rds` |
| `codes/validation_eurostat.R` | `figs/validation_eurostat.png`, `data/stats/roundtrip_stats.rds` |
| `codes/validation_gdp_pop.R` | `figs/validation_gdp_pop_multiyear.png`, `figs/validation_gdp_pop_singleyear.png`, `data/stats/gdp_pop_stats.{csv,rds}` |
| `codes/intext_numbers.R` | `data/stats/intext_numbers.rds` |

```bash
Rscript codes/validation_eurostat.R
```

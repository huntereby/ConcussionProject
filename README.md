# Concussion Project Overview

This repository contains analysis scripts and generated figures examining the relationship between concussion history and various psychiatric outcomes. The project processes Kaplan‑Meier results for different concussion exposures and produces survival curves and forest plots.

## Figures (v4)

Below are the latest v4 figures produced from the current data:

- **Forest Plot**: `forest_plot_v4.svg`
- **Survival Curves**:
  - `survival_mood_disorder_v4.svg`
  - `survival_anxiety_disorder_v4.svg`
  - `survival_adhd_v4.svg`

These figures visualize the risk of mood disorder, anxiety disorder, and ADHD over time following concussion exposures, as well as the comparative risk ratios across concussion counts.

The scripts `EbySurvCurves.R` and `Forest Plot.R` demonstrate how the plots are generated from the CSV files in the `Results` directory. Run them in an R environment with the necessary packages installed to reproduce the figures.

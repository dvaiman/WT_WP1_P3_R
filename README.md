# Workplace health assessments and short to long-term cardiovascular risk: A propensity score-matched emulated target trial in Swedish workers

## Abstract

This study examines the association between Health Profile Assessments (HPA) and
cardiovascular disease (CVD) risk using an emulated target-trial design with
propensity-score matching in a large Swedish worker population. Workers who
completed an HPA are matched 1:4 to general-population controls on sex, birth
year, education and wage income, and followed for incident CVD.

## Study design

- **Design**: Emulated target trial with yearly 1:4 propensity-score matching.
- **Population**: Swedish workers, 1995–2021 (see the live cohort-flow number below).
- **Exposure**: Health Profile Assessment (HPA) participation, classified at baseline.
- **Outcome**: Incident cardiovascular disease (arterial; `art`).
- **Primary analysis**: M3, a cause-specific Cox proportional-hazards model with
  death treated as a censoring event.
- **Estimand**: the intention-to-treat effect of HPA participation (classification
  at baseline, regardless of later behaviour), in the treated population (ATT, from
  1:4 matching), as a cause-specific hazard ratio with death as a censoring event.

## The single primary model (M3)

M3 is the one pre-specified primary model. Its covariate set is defined **once** in
`script/02_functions_source_file.R` (`m3_covariates`, `m3_rhs`,
`build_m3_formula()`, `m3_formula_10yr`) and reused everywhere, so the primary
model can never drift between scripts:

```
treated + birth_cohort + Age + Sex + comorbidity +
  cvd_art_before_HPA_flag + IncomeLevel_CSFVI
```

Everything else is explicitly secondary:

- **Sensitivity (adjustment set)**: M1, M2, M4, M5 — incremental confounder sets.
- **Effect modification (pre-specified)**: sex, age, education.
- **Effect modification (exploratory)**: income, occupation.
- **Bias probes (exploratory, HPA-only)**: VO₂max, BMI, exercise, self-rated
  health, and number of assessments. These are measured only in HPA participants
  (controls inherit the matched participant's value), so they cannot estimate a
  causal effect and are reported only as probes of selection/bias gradients.

## How to run

Open the project in Positron/RStudio so `here::here()` resolves to the repo root.
The `.qmd` analysis files are `eval: false`, so render does not execute them — run
the R code directly. Paste this into the R console:

```r
library(here)

scripts <- c(
  "09_cohort_flow.qmd",                          # final N + strict 1:4 check
  "26_survival_models_time_restricted.qmd",      # fit + save M3 etc., main HR table
  "26b_bias_probes.qmd",                         # exploratory lifestyle / n-assessment probes
  "27_survival_models_sensitivity_analyses.qmd", # 1-yr landmark + prior-CVD + 18-65
  "27b_competing_risks.qmd",                     # Fine-Gray sHR + cumulative incidence
  "28_survival_models_main.qmd",                 # marginal HRs (reads 26 output)
  "35_basic_stats.qmd",                          # E-value + absolute risk difference
  "32_figure_forrestplot.qmd",                   # forest plot (reads 26 + 26b)
  "40_reviewer_results_report.qmd"               # consolidated report (reads all above)
)

run_qmd <- function(qmd) {
  message("\n=== RUNNING ", qmd, " (", format(Sys.time(), "%H:%M:%S"), ") ===")
  t0 <- Sys.time()
  tmp <- tempfile(fileext = ".R")
  knitr::purl(here::here("script", qmd), output = tmp, quiet = TRUE)  # extract R code
  source(tmp, echo = FALSE)
  message("=== DONE ", qmd, " in ",
          round(difftime(Sys.time(), t0, units = "mins"), 1), " min ===")
}

for (s in scripts) run_qmd(s)
```

Dependencies between scripts: `28` needs `26` (reads `cox_ten_year_models.rds`);
`32` needs `26` + `26b`; `40` needs `09`, `27`, `27b`, `35`. The order above
respects these.

## Repository structure

```
WT_WP1_P3_R/
├── script/        analysis scripts (numbered, run in order)
├── docs/          rendered HTML (published via GitHub Pages)
├── figures and tables/   manuscript-ready figures and tables
├── README.md
└── _quarto.yml
../data/           input data (NOT in version control — see data & ethics)
../results/        outputs written by the scripts
    ├── models/    fitted model objects + main/biasprobe HR CSVs
    ├── tables/    cohort flow, sensitivity, E-value, ARR, report CSVs
    └── figures/   forest plot, competing-risk CIF, etc.
```

## Script pipeline

### Sourced helpers (not run on their own)
| File | Purpose |
|------|---------|
| `01_data_source_file.R` | Builds the analytic data frame `data_cvd`: reads the CVD file, joins cluster IDs and the number-of-tests/lifestyle variables, recodes covariates, and **enforces strict 1:4 matching** (keeps only clusters with exactly 1 HPA + 4 controls). Sourced by every analysis script. |
| `02_functions_source_file.R` | Single source of truth for **M3** (`m3_covariates`, `build_m3_formula()`, `m3_formula_10yr`) plus `standsurv` helpers (`quick_standsurv`, `fast_stand`) for RMST/hazard contrasts. |
| `03_data_source_file_change.R` | Builds the lifestyle-change sub-cohort (first→second test) used by the repeated-assessment analysis. |

### Data preparation
| File | Purpose |
|------|---------|
| `10_propensity_score_matching.qmd` | Yearly 1:4 nearest-neighbour PS matching (MatchIt) of HPA vs general population; writes the matched cohort and balance statistics (`combined_balance_stats.csv`). |
| `11_scb_data_merge.qmd` | Merges Statistics Sweden (SCB) register variables. |
| `12_sos_icd_conversion.qmd` | Converts National Board of Health & Welfare (SoS) ICD codes. |
| `13_sos_data_merge.qmd` | Merges SoS patient/cause-of-death register data. |
| `14_sos_charlson_comorbidity_index.qmd` | Computes the weighted Charlson comorbidity index (`CCIw`). |
| `15_n_tests.qmd` | Counts HPA assessments within 5 years and derives first/second/last-test lifestyle variables. |
| `16_data_check.qmd` | Data validation / sanity checks. |
| `09_cohort_flow.qmd` | **Cohort flow (CONSORT)**: computes the exclusion cascade and emits the final N to `results/tables/cohort_flow.csv`; asserts strict 1:4. |

### Analysis
| File | Purpose |
|------|---------|
| `17_ph_test_nested_models.qmd` | Proportional-hazards diagnostics for the nested models. |
| `25_survival_models_unrestricted.qmd` | Cox models over unrestricted (full) follow-up. |
| `26_survival_models_time_restricted.qmd` | **Main pipeline**: fits M3 (primary) + sensitivity + effect-modification models as a *named* container (`cox_ten_year_models.rds`) and writes the main HR table (`cox_ten_year_main_HR.csv`). |
| `26b_bias_probes.qmd` | Exploratory HPA-only lifestyle / number-of-assessment interaction probes → `ten_yr_biasprobe_interaction_HR.csv`. |
| `27_survival_models_sensitivity_analyses.qmd` | One-year landmark (delayed entry) **plus** M3 refits excluding prior CVD and restricted to ages 18–65 → `m3_sensitivity_summary.csv`. |
| `27b_competing_risks.qmd` | Competing-risk sensitivity: Fine–Gray subdistribution HR and cumulative incidence functions with death as a competing event. |
| `28_survival_models_main.qmd` | Population-averaged (marginal) HRs and pairwise effect-modification contrasts, reading the named models by name. |
| `29_repeated_tests.qmd` | Repeated-assessment / lifestyle-change analysis (second test). |
| `35_basic_stats.qmd` | Descriptive statistics, person-years, incidence rates, the **E-value** (seeded from the live M3 estimate), and the descriptive absolute risk difference. |

### Figures and tables
| File | Purpose |
|------|---------|
| `30_table_1.qmd` | Table 1 — baseline characteristics by arm. |
| `31_survival_curve.qmd` | Kaplan–Meier / survival curves. |
| `32_figure_forrestplot.qmd` | Forest plot of all HRs (pre-specified vs exploratory blocks; ckbplotr). |
| `33_figure_time_varying.qmd` | Time-varying hazard ratio figure (flexible parametric models). |
| `34_figure_RMST.qmd` | Restricted mean survival time difference figure. |
| `40_reviewer_results_report.qmd` | **Consolidated report**: gathers the key estimates (final N, M3 HR, E-value, sHR, sensitivity HRs, ARR, SMD) and the estimand statement into one table. |

## Key outputs to check

| Output | Written by |
|--------|-----------|
| `results/tables/cohort_flow.csv` | `09` — final analytic N (exclusion cascade) |
| `results/tables/evalue_m3.csv` | `35` — E-value seeded from the live M3 estimate |
| `results/tables/m3_sensitivity_summary.csv` | `27` — all sensitivity HRs in one table |
| `results/tables/competing_risks_shr.csv` | `27b` — Fine–Gray subdistribution HR |
| `results/models/cox_ten_year_main_HR.csv` | `26` — main + effect-modification HRs for the forest plot |
| `results/figures/forest_plot_…` | `32` |

## Performance notes

- The bottleneck is `marginaleffects::avg_comparisons()` on ~2 million rows in
  `26`, `26b` and `28`; the interaction models in particular can take a long time.
  Fitted models are cached to `cox_ten_year_models.rds`, so `28` does **not** refit
  — only the marginal-effects computation is slow.
- Every analysis script re-sources `01`, which re-reads the ~130 MB analytic CSV
  and re-applies the 1:4 filter. Running scripts back-to-back repeats this read.

## Software & dependencies

- R (tidyverse, base pipe `|>`). Key analysis packages: `survival`,
  `marginaleffects`, `flexsurv`, `broom`, `MatchIt`, `cobalt`, `gtsummary`,
  `EValue`, `ckbplotr`, `fastcmprsk` (+ `dynpred`).
- `ckbplotr` is not on CRAN — install from R-universe:
  `install.packages("ckbplotr", repos = "https://neilstats.r-universe.dev")`.
- `fastcmprsk` (full-data Fine–Gray in `27b`) and its dependency `dynpred` are
  CRAN-archived — install the archived source builds with `remotes`:
  `remotes::install_version("dynpred"); remotes::install_version("fastcmprsk")`.
  If `fastcmprsk` is absent, `27b` automatically falls back to a memory-safe
  subsample Fine–Gray, so the pipeline still runs.
- Paths are built with `here::here()`; do not use `setwd()`.

## Data & ethics

Individual-level Swedish registry data are **not** included in this repository and
must not be committed (`data/`, `*.csv`, `*.parquet`, `*.sav`, `*.dta`, `*.rds` are
git-ignored). The data are identifiable and held under ethical approval and GDPR;
they may only be processed inside the secure environment. Scripts display
aggregates and model summaries, never row-level identifiable data.

## Naming conventions

- `art` = arterial/atherosclerotic CVD outcome; `*_10yrs` = 10-year follow-up
  window; `*_unrestricted` = full follow-up; `*_1y` / `tstart_1y` / `tstop_1y` =
  one-year landmark (delayed entry).
- `treated` is a factor with levels `Control`, `HPA`. `id_cluster` identifies a
  matched 1:4 set.

## Results and article

Numerical results, figures and rendered reports are **not** included in this
repository while the manuscript is unpublished. A link to the published article and
to the rendered results will be added here once the paper is out.

# TIMSS 2023 Grade 8 dissertation analysis

This R project implements the first runnable analysis for the dissertation:

> Is Data and Probability more resource-stratified than Number, Algebra, and
> Geometry and Measurement in TIMSS 2023 Grade 8?

It is designed for the official TIMSS 2023 Grade 8 international database. It
uses all five plausible values for each mathematics content domain, the total
student weight, and TIMSS jackknife repeated replication (JRR) variables. The
variance code implements TIMSS 2023 JK2-full: two replicate subsamples for each
of 125 variance zones (250 replicate weights), with the official 0.5 scaling
factor.

## 1. Add the official data

Download the official **TIMSS 2023 Grade 8 SPSS data** or **R data** from the
TIMSS 2023 International Database page:

<https://timss2023.org/data/>

Extract the archive and place the unrenamed Grade 8 student files (`BSG*`) under:

```text
data/raw/
```

The reader accepts `.sav`, `.rds`, `.rda`, and `.RData` files. School files are
not yet required because this first analysis uses the school identifier already
present in the student file and student-reported resource variables.

## 2. Install R packages

Run once in R:

```r
install.packages(c(
  "haven",
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "survey",
  "tibble",
  "tidyr"
))
```

## 3. Run

Set the R working directory to this project folder, then run:

```r
source("run_all.R")
```

For a quicker first check, run only:

```r
source("01_build_analytic_data.R")
```

This verifies the files, resolves the variables, records missingness, and
creates the analytic data before any model is fitted.

## Variable decisions implemented

| Construct | TIMSS 2023 variables | Coding in this project |
|---|---|---|
| Number achievement | `BSMNUM01`–`BSMNUM05` | All five plausible values |
| Algebra achievement | `BSMALG01`–`BSMALG05` | All five plausible values |
| Geometry and Measurement achievement | `BSMGEO01`–`BSMGEO05` | All five plausible values |
| Data and Probability achievement | `BSMDAT01`–`BSMDAT05` | All five plausible values |
| Home Educational Resources | `BSBGHER`; `BSDGHER` | Continuous scale; category retained for descriptive checks |
| Own/shared computer or tablet | `BSBG05A`; `BSBG05B` | Binary access indicators |
| Smartphone | `BSBG05C` | Binary access indicator |
| Internet access | `BSBG05D` | Binary access indicator |
| Internet use for mathematics/science schoolwork | `BSBG14A`–`BSBG14F` | Reversed so a higher index means more frequent use |
| Survey design | `TOTWGT`; `JKZONE`; `JKREP` | Total student weight and JRR variance estimation |

The script checks both variable codes and labels. It stops if required
achievement or survey-design variables are missing, and records optional
variables that are absent or ambiguous in `outputs/variable_resolution.csv`.

## Model separation

The main preliminary models are deliberately separated:

1. Home Educational Resources model;
2. home ICT access model;
3. internet use for mathematics/science schoolwork model.

`BSBGHER` is not entered alongside the home ICT access index in the main model.
The TIMSS 2023 Home Educational Resources scale includes home study supports,
including internet-related support, so entering both as if they were independent
constructs would create direct measurement overlap. The ICT models instead use
books at home and parental education as background controls when those variables
have acceptable availability.

## Cross-system weighting

Country-specific estimates use `TOTWGT`. Pooled preliminary estimates rescale
`TOTWGT` so that each participating education system contributes equally and
include education-system fixed effects. This prevents systems with larger target
populations from dominating the pooled coefficient.

The pooled models are diagnostic, not the final multilevel models. The final
dissertation analysis still needs a defensible decision between:

- separate two-level student-within-school models by education system;
- a three-level student-within-school-within-system model; or
- a justified meta-analytic synthesis of system-specific coefficients.

Treating all education systems as one two-level sample without modelling the
education-system level is not recommended.

## Outputs

The main files created under `outputs/` are:

- `variable_resolution.csv`
- `sample_flow.csv`
- `missingness_by_country.csv`
- `domain_descriptives_by_country.csv`
- `resource_descriptives_by_country.csv`
- `preliminary_regressions_pooled.csv`
- `preliminary_domain_interactions.csv`
- `preliminary_regressions_by_country.csv` (when enabled)
- `model_run_log.csv`

In `preliminary_domain_interactions.csv`, Data and Probability is the reference
domain. A negative domain-by-resource interaction means that the estimated
resource association is stronger in Data and Probability than in that comparison
domain.

## Configuration

Edit `config/analysis_config.R` to restrict education systems or turn off
country-specific models for a faster meeting-preparation run.

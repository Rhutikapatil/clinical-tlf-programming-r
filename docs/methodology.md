# Clinical TLF Programming Methodology

## Overview

This repository demonstrates an end-to-end clinical statistical programming workflow in R for developing Tables, Listings, and Figures (TLFs) from analysis-ready clinical datasets.

The project focuses on reproducible programming, statistical analysis, clinical data summarization, quality control, and formatted report generation.

This is a sanitized portfolio repository. No confidential clinical trial data, patient-level data, sponsor information, proprietary study specifications, or confidential reference outputs are included.

---

## Repository Programs

The `R/` directory contains the following programming examples:

- `demographics_table.R`
- `adverse_events_table.R`
- `pfs_by_remission_table.R`
- `km_survival_plot.R`
- `lab_shift_table.R`
- `best_overall_response_by_age.R`
- `viral_load_summary.R`

Each program demonstrates a different component of clinical statistical programming and includes data preparation, analysis, quality-control checks, and formatted output generation.

---

## General Programming Workflow

The overall programming workflow used throughout this project is:

1. Read analysis-ready clinical data.
2. Apply the appropriate analysis population.
3. Perform data-quality and duplicate checks.
4. Standardize treatment and analysis variables.
5. Derive analysis variables required for reporting.
6. Calculate statistical summaries.
7. Perform programmatic quality-control checks.
8. Format results for clinical reporting.
9. Generate tables or figures.
10. Save validation information and execution logs locally.

The programs are designed to demonstrate reproducible clinical reporting practices while keeping data processing, statistical calculations, and output generation clearly separated.

---

## Demographic and Baseline Characteristics

### Program

`demographics_table.R`

### Purpose

This program summarizes demographic and baseline characteristics by treatment group for an analysis population.

### Methods Demonstrated

The analysis includes subject-level filtering and treatment-specific population denominators.

Continuous variables are summarized using descriptive statistics such as:

- N
- Mean
- Standard deviation
- Median
- Minimum
- Maximum

Categorical variables are summarized using subject counts and percentages.

Examples of demographic or baseline variables that may be summarized include:

- Age
- Age category
- Sex
- Ethnicity
- Race
- Baseline weight
- Performance status
- Remission status
- Prior-treatment duration

Percentages are calculated using the applicable treatment-group denominator.

The program also demonstrates half-up percentage rounding for formatted clinical tables.

### Quality Control

Programmatic checks are used to verify:

- One subject-level record per participant
- Treatment-group reconciliation
- Analysis population totals
- Valid categorical values
- Missing values in analysis variables
- Correct percentage formatting

The final formatted table is generated using the `flextable` and `officer` packages.

---

## Treatment-Emergent Adverse Events

### Program

`adverse_events_table.R`

### Purpose

This program summarizes treatment-emergent adverse events by MedDRA System Organ Class and Preferred Term, including the highest observed NCI CTCAE toxicity grade.

### Analysis Population

The analysis uses the safety-evaluable population and actual treatment assignment.

### Treatment-Emergent Events

Only treatment-emergent adverse events are included in the analysis.

### Highest-Grade Derivation

When a subject experiences multiple occurrences of the same adverse event, the subject is counted once using the highest observed NCI CTCAE grade.

This logic is applied at multiple reporting levels:

- Any adverse event
- System Organ Class
- Preferred Term

This prevents subjects with repeated events from being counted multiple times within the same analysis category.

### Adverse Event Summaries

The program calculates:

- Number of subjects with an event
- Percentage of subjects with an event
- Highest toxicity grade
- Treatment-specific incidence

System Organ Classes and Preferred Terms are ordered by descending incidence, with alphabetical ordering used to resolve ties.

### Quality Control

QC checks include:

- Valid treatment groups
- Valid toxicity grades
- Removal of duplicate subject/category combinations
- Population reconciliation
- Highest-grade derivation checks
- SOC and Preferred Term sorting checks

The program also generates a formatted adverse-event table using `flextable` and `officer`.

---

## Progression-Free Survival by Remission Status

### Program

`pfs_by_remission_table.R`

### Purpose

This program performs a time-to-event analysis of Progression-Free Survival and summarizes results by remission subgroup and treatment.

### Survival Data Preparation

The analysis uses progression-free survival time and censoring information from an analysis-ready time-to-event dataset.

The event indicator is derived from the censoring variable.

Conceptually:

- Event = disease progression or qualifying event
- Censored = no qualifying event observed before censoring

### Kaplan-Meier Analysis

Kaplan-Meier estimation is used to calculate survival-distribution statistics.

Reported statistics include:

- Number of subjects
- Number of events
- Number of censored observations
- Median Progression-Free Survival
- 95% confidence interval for the median
- 25th percentile
- 75th percentile
- Minimum observed analysis time
- Maximum observed analysis time

### Cox Proportional Hazards Model

A Cox proportional hazards model is used to estimate the treatment effect.

The analysis produces:

- Hazard ratio
- 95% confidence interval

The Efron method is used for handling tied event times.

### Treatment Comparison Tests

Treatment groups are compared using:

- Log-rank test
- Wilcoxon test

Both unstratified and stratified analyses are demonstrated.

The stratified analysis incorporates remission status as a stratification factor.

### Quality Control

Programmatic QC includes:

- One time-to-event record per subject
- Treatment validation
- Event-indicator validation
- Nonmissing analysis times
- Hazard-ratio validity
- Confidence-interval ordering
- P-value range checks

---

## Kaplan-Meier Survival Figure

### Program

`km_survival_plot.R`

### Purpose

This program creates a Kaplan-Meier Progression-Free Survival figure with treatment-specific survival curves and statistical annotations.

### Methods Demonstrated

The program includes:

- Kaplan-Meier survival estimation
- Treatment-specific survival curves
- Censoring marks
- Cox proportional hazards modeling
- Hazard-ratio calculation
- 95% confidence intervals
- Log-rank testing
- Wilcoxon testing
- Median survival estimation

### Number-at-Risk Table

A number-at-risk table is calculated dynamically from the time-to-event data.

Risk counts are generated at defined time points and displayed with the Kaplan-Meier figure.

### Statistical Annotations

Statistical results are calculated programmatically and displayed within the plot.

These may include:

- Hazard ratio
- Confidence interval
- Log-rank p-value
- Median survival information

Plotting limits and analysis ranges are determined from the available analysis data rather than relying only on hard-coded graphical values.

### Quality Control

QC checks confirm:

- Valid treatment groups
- Valid event indicators
- Nonmissing survival times
- Positive and finite hazard ratios
- Ordered confidence intervals
- Valid statistical-test p-values

The visualization is created using `ggplot2`.

---

## Laboratory Shift Analysis

### Program

`lab_shift_table.R`

### Purpose

This program evaluates changes in laboratory toxicity grade from baseline to the worst post-baseline observation.

The analysis demonstrates clinical laboratory shift-table programming using NCI CTCAE grading.

### Baseline Definition

Baseline is based on the last eligible nonmissing laboratory observation prior to treatment.

When allowed by the analysis rules, an observation collected on the treatment start date may also be considered for baseline determination.

### Post-Baseline Analysis

All eligible post-baseline observations are evaluated.

Repeated and unscheduled laboratory measurements are included when determining the worst post-baseline toxicity grade.

The analysis derives one worst post-baseline result per subject and laboratory parameter.

### Laboratory Parameters

The programming example demonstrates shift-table logic across several types of clinical laboratory tests, including examples from:

- Electrolytes
- Liver chemistry
- Renal chemistry
- Urine protein

### Toxicity Direction

Special handling is demonstrated for laboratory parameters that may have both low and high toxicity directions.

For selected electrolyte parameters, low and high toxicity grades are evaluated separately.

For other laboratory parameters, the applicable high-toxicity grade is summarized.

### Shift Table

Subjects are classified according to:

- Baseline toxicity category
- Worst post-baseline toxicity category

Counts and percentages are generated for each treatment group.

### Quality Control

QC checks include:

- Required laboratory parameters
- Subject-level uniqueness
- Baseline availability
- Post-baseline availability
- Valid toxicity grades
- Worst-grade derivation
- Row-level count reconciliation
- Treatment-specific denominator checks

---

## Best Overall Response by Age Category

### Program

`best_overall_response_by_age.R`

### Purpose

This program summarizes investigator-assessed Best Overall Response by age subgroup and treatment group.

### Response Categories

The analysis demonstrates response categories such as:

- Complete Response (CR)
- Partial Response (PR)
- Stable Disease (SD)
- Progressive Disease (PD)
- Not Evaluable or Unable to Determine (NE)

### Age Subgroups

Subjects are summarized using age-based subgroups.

The programming example demonstrates comparison of subjects below 65 years of age with subjects 65 years of age or older.

### Objective Response Rate

Objective Response Rate is calculated as:

**ORR = CR + PR**

The ORR is presented as:

**Number of responders / Number of subjects (%)**

### Exact Confidence Interval

A 95% confidence interval for the Objective Response Rate is calculated using the Clopper-Pearson exact binomial method.

The R function `binom.test()` is used to calculate the exact confidence interval.

### Quality Control

QC checks include:

- One Best Overall Response record per subject
- Valid treatment groups
- Valid age categories
- Valid response categories
- Response-reason consistency
- Treatment denominator reconciliation
- Verification that response categories sum to the appropriate population denominator

---

## Viral Load Analysis

### Program

`viral_load_summary.R`

### Purpose

This program demonstrates longitudinal clinical endpoint analysis using viral-load measurements.

Both unadjusted and adjusted summaries are produced.

### Unadjusted Viral Load Summary

Viral-load measurements are summarized by treatment and analysis visit.

The program calculates:

- N
- Arithmetic mean
- Standard deviation
- Standard error

Standard error is calculated as:

**SE = SD / sqrt(N)**

The program organizes longitudinal results by visit and treatment for formatted reporting.

### Adjusted Mean Analysis

Adjusted treatment means are estimated using a fixed-effects model.

The model structure is:

**AVAL ~ STAGE + TRT**

where:

- `AVAL` represents the analysis value
- `STAGE` represents disease-stage category
- `TRT` represents treatment group

The model is fitted separately for selected analysis visits.

### Adjusted Treatment Means

Treatment-specific adjusted means are calculated using equal weighting across the defined stage categories.

The analysis produces:

- Adjusted mean
- Adjusted standard error
- Treatment-specific sample size

### Quality Control

QC checks include:

- One analysis record per subject and visit
- Valid treatment groups
- Required analysis visits
- Nonmissing analysis values
- Required disease-stage categories
- Positive treatment sample sizes
- Finite means
- Finite standard errors
- Correct visit-treatment result structure

---

## Quality-Control Strategy

Quality control is incorporated directly into the R programs rather than being treated only as a final manual review step.

Common R functions used for QC include:

```r
stopifnot()
all()
identical()
isTRUE()
n_distinct()
```
Examples of programmatic QC include:

Duplicate detection
Subject-level uniqueness
Population reconciliation
Treatment-group validation
Analysis-category validation
Event and censoring validation
Laboratory-grade validation
Count reconciliation
Percentage checks
Confidence-interval validation
Statistical-model result checks

When a required condition is not satisfied, the program is designed to stop rather than silently continue with potentially incorrect results.
Output Generation

Clinical reporting outputs are generated using R packages such as:
```r
flextable
officer
ggplot2
```
flextable is used for formatted clinical tables.

officer is used to create Microsoft Word output.

ggplot2 is used for graphical reporting such as Kaplan-Meier survival figures.

The programs demonstrate separation between:

Analysis calculations
Display formatting
Quality control
Final report generation
R Packages

The project uses packages including:
```r
haven
readxl
dplyr
tidyr
stringr
tibble
survival
ggplot2
flextable
officer
```
These packages support clinical-data import, data manipulation, statistical analysis, survival analysis, visualization, and report generation.

Data Protection

Clinical datasets are not stored in this public repository.

Local source datasets should be placed in:

data/

Generated outputs that are created using private or patient-level data should remain in:

local_outputs/

Both directories are excluded from version control through .gitignore.

Additional clinical data file types such as XPT, SAS datasets, Excel files, and CSV files are also excluded from public version control where appropriate.

Reproducibility

The programming examples are structured so that the analysis workflow can be followed from source-data preparation through statistical analysis, quality control, and reporting.

The repository demonstrates:

Clear program organization
Reusable functions
Explicit analysis-population selection
Transparent statistical methods
Automated QC checks
Reproducible report generation
Separation of private data from public code
Portfolio Disclaimer

This repository is intended solely as a professional portfolio demonstration of clinical statistical programming skills.

All programming examples have been sanitized for public use.

No real patient-level data, confidential clinical trial datasets, sponsor-specific identifiers, proprietary specifications, or confidential study results are included.

The repository is intended to demonstrate programming methodology, clinical reporting concepts, statistical reasoning, R programming, and quality-control practices.


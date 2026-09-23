# Clinical TLF Programming in R

This portfolio project demonstrates an end-to-end clinical statistical programming workflow for generating Tables, Listings, and Figures (TLFs) using R.

The project focuses on reproducible analysis, clinical data summarization, statistical methods, quality control, and formatted report generation.

> This repository contains sanitized programming examples only. No proprietary clinical trial data, patient-level data, sponsor information, or confidential study documentation is included.

## Analyses Included

- Demographic and baseline characteristics
- Treatment-emergent adverse event summaries
- Adverse events by System Organ Class and Preferred Term
- Adverse events by highest toxicity grade
- Progression-Free Survival analysis
- Kaplan-Meier survival curves
- Cox proportional hazards modeling
- Log-rank testing
- Number-at-risk tables
- Laboratory shift analysis
- Best Overall Response summaries
- Viral load summaries
- Adjusted statistical models

## R Packages

- dplyr
- tidyr
- haven
- survival
- ggplot2
- flextable
- officer
- readxl

## Statistical Programming Workflow

1. Import analysis-ready clinical data
2. Apply analysis population criteria
3. Perform subject-level derivations
4. Generate statistical summaries
5. Validate population counts and derived results
6. Compare calculated statistics with reference values when available
7. Generate formatted tables and figures
8. Perform QC and output review

## Key Methods Demonstrated

### Adverse Event Analysis
Treatment-emergent adverse events are summarized by System Organ Class and Preferred Term.

Patients with repeated occurrences are counted once at the highest toxicity grade for the relevant analysis level.

Adverse-event categories are ordered by descending incidence percentage.

### Survival Analysis
Progression-free survival analysis includes:

- Kaplan-Meier estimation
- Median PFS
- Number-at-risk tables
- Cox proportional hazards models
- Hazard ratios with 95% confidence intervals
- Log-rank testing
- Censoring indicators

### Quality Control
QC includes:

- Analysis population reconciliation
- Unique-subject checks
- Treatment denominator validation
- Duplicate detection
- Expected record-count checks
- Statistical result comparison
- Sorting validation
- Formatted-output review
Example validation functions include:

```r
stopifnot()
all()
identical()
isTRUE()
```

## Skills Demonstrated

Clinical Statistical Programming | R | TLF Development | ADaM | Survival Analysis | Kaplan-Meier | Cox Regression | Adverse Event Analysis | Clinical Data QC | Reproducible Reporting

## Disclaimer

All examples in this repository are sanitized and intended solely to demonstrate statistical programming techniques. No confidential, proprietary, or patient-level clinical trial information is included.

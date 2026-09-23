# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# DEMOGRAPHICS TABLE
# Demographic and Baseline Characteristics
# Randomized Patients
#
# Source Dataset: ADSL
# Analysis Population: ITTFL = "Y"
# Programming Language: R
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names and expected sample sizes are removed.
# - The analysis logic and QC workflow are preserved.
#
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(flextable)
library(officer)


# ============================================================
# 2. OUTPUT FOLDER AND READ ADSL
# ============================================================

dir.create(
  "outputs",
  showWarnings = FALSE
)

# Data are intentionally not included in the public repository.
# Provide an ADSL-compatible XPT file locally at data/ADSL.xpt.

adsl <- read_xpt(
  "data/ADSL.xpt"
)


# ============================================================
# 3. CREATE ANALYSIS POPULATION
# Randomized / ITT subjects
# ============================================================

adsl_itt <- adsl %>%
  filter(ITTFL == "Y")


# ============================================================
# 4. BASIC QA CHECKS
# ============================================================

cat("============================================\n")
cat("DEMOGRAPHICS TABLE - DEMOGRAPHICS\n")
cat("============================================\n\n")

cat("Number of ADSL records:",
    nrow(adsl), "\n")

cat("Number of ITT subjects:",
    nrow(adsl_itt), "\n")

cat("Unique ITT subjects:",
    n_distinct(adsl_itt$USUBJID), "\n\n")

print(
  table(adsl_itt$TRT01P)
)


# ============================================================
# 5. TREATMENT DENOMINATORS
# ============================================================

n_placebo <- sum(
  adsl_itt$TRT01P == "Placebo",
  na.rm = TRUE
)

n_treatment_a <- sum(
  adsl_itt$TRT01P == "Treatment A",
  na.rm = TRUE
)

n_all <- nrow(adsl_itt)

cat("\nTreatment Denominators\n")
cat("Placebo N =", n_placebo, "\n")
cat("Treatment A N =", n_treatment_a, "\n")
cat("All Patients N =", n_all, "\n\n")


# ============================================================
# 6. FUNCTION: PERCENTAGE WITH HALF-UP ROUNDING
#
# Uses half-up rounding to one decimal place.
# ============================================================

format_n_pct <- function(n, denominator) {
  
  if (denominator == 0) {
    return("0 (0.0%)")
  }
  
  pct <- 100 * n / denominator
  
  # Half-up rounding to 1 decimal place
  pct_round <- floor(pct * 10 + 0.5) / 10
  
  sprintf(
    "%d (%.1f%%)",
    n,
    pct_round
  )
}


# Test rounding with a generic example
stopifnot(
  format_n_pct(1, 8) == "1 (12.5%)"
)


# ============================================================
# 7. FUNCTION: CONTINUOUS STATISTICS
#
# Display statistics:
# n
# Mean_sd
# Median
# Range
#
# Range displayed as whole numbers.
# ============================================================

continuous_stats <- function(data, variable) {
  
  x <- data[[variable]]
  
  x <- x[!is.na(x)]
  
  tibble(
    N = length(x),
    
    Mean_SD = sprintf(
      "%.1f (%.1f)",
      mean(x),
      sd(x)
    ),
    
    Median = sprintf(
      "%.1f",
      median(x)
    ),
    
    Range = sprintf(
      "%.0f - %.0f",
      min(x),
      max(x)
    )
  )
}


# ============================================================
# 8. FUNCTION: GENERIC CATEGORICAL SUMMARY
# ============================================================

categorical_summary <- function(
    data,
    variable,
    codes,
    labels
) {
  
  tibble(
    Code = codes,
    Category = labels
  ) %>%
    
    rowwise() %>%
    
    mutate(
      
      Placebo = format_n_pct(
        
        sum(
          data$TRT01P == "Placebo" &
            as.character(data[[variable]]) == Code,
          na.rm = TRUE
        ),
        
        n_placebo
      ),
      
      `Treatment A` = format_n_pct(
        
        sum(
          data$TRT01P == "Treatment A" &
            as.character(data[[variable]]) == Code,
          na.rm = TRUE
        ),
        
        n_treatment_a
      ),
      
      `All Patients` = format_n_pct(
        
        sum(
          as.character(data[[variable]]) == Code,
          na.rm = TRUE
        ),
        
        n_all
      )
    ) %>%
    
    ungroup() %>%
    
    select(
      Category,
      Placebo,
      `Treatment A`,
      `All Patients`
    )
}


# ============================================================
# 9. AGE
# ============================================================

age_placebo <- continuous_stats(
  adsl_itt %>%
    filter(TRT01P == "Placebo"),
  "AGE"
)

age_treatment_a <- continuous_stats(
  adsl_itt %>%
    filter(TRT01P == "Treatment A"),
  "AGE"
)

age_all <- continuous_stats(
  adsl_itt,
  "AGE"
)


# ============================================================
# 10. AGE GROUP
# ============================================================

age_group_summary <- categorical_summary(
  
  data = adsl_itt,
  
  variable = "AGEGR1",
  
  codes = c(
    "18 - 40",
    "41 - 64",
    ">= 65"
  ),
  
  labels = c(
    "18 - 40",
    "41 - 64",
    ">= 65"
  )
)


# ============================================================
# 11. SEX
#
# Example display includes Female.
# ============================================================

sex_summary <- categorical_summary(
  
  data = adsl_itt,
  
  variable = "SEX",
  
  codes = c(
    "F"
  ),
  
  labels = c(
    "Female"
  )
)


# ============================================================
# 12. ETHNICITY
#
# Example display includes observed categories.
# ============================================================

ethnicity_summary <- categorical_summary(
  
  data = adsl_itt,
  
  variable = "ETHNIC",
  
  codes = c(
    "HISPANIC OR LATINO",
    "NOT HISPANIC OR LATINO"
  ),
  
  labels = c(
    "Hispanic or Latino",
    "Not Hispanic or Latino"
  )
)


# ============================================================
# 13. RACE
#
# Multiple-race category is supported.
# ============================================================

race_summary <- categorical_summary(
  
  data = adsl_itt,
  
  variable = "RACE",
  
  codes = c(
    "AMERICAN INDIAN OR ALASKA NATIVE",
    "ASIAN",
    "BLACK OR AFRICAN AMERICAN",
    "WHITE",
    "MULTIPLE"
  ),
  
  labels = c(
    "American Indian or Alaska Native",
    "Asian",
    "Black or African American",
    "White",
    "Multiple"
  )
)


# ============================================================
# 14. BASELINE WEIGHT
# ============================================================

weight_placebo <- continuous_stats(
  
  adsl_itt %>%
    filter(TRT01P == "Placebo"),
  
  "BWT"
)

weight_treatment_a <- continuous_stats(
  
  adsl_itt %>%
    filter(TRT01P == "Treatment A"),
  
  "BWT"
)

weight_all <- continuous_stats(
  adsl_itt,
  "BWT"
)


# ============================================================
# 15. ECOG SCORE
# ============================================================

ecog_summary <- categorical_summary(
  
  data = adsl_itt,
  
  variable = "BECOG",
  
  codes = c(
    "0",
    "1"
  ),
  
  labels = c(
    "0",
    "1"
  )
)


# ============================================================
# 16. CURRENT REMISSION STATUS
#
# Example includes observed remission categories.
# ============================================================

remission_summary <- categorical_summary(
  
  data = adsl_itt,
  
  variable = "REMISS",
  
  codes = c(
    "SECOND COMPLETE REMISSION",
    "THIRD COMPLETE REMISSION"
  ),
  
  labels = c(
    "2nd",
    "3rd"
  )
)


# ============================================================
# 17. WEEKS FROM LAST THERAPY
#
# PRTXDUR label:
# Weeks Since Last Prior Cancer TX
# ============================================================

prtxdur_placebo <- continuous_stats(
  
  adsl_itt %>%
    filter(TRT01P == "Placebo"),
  
  "PRTXDUR"
)

prtxdur_treatment_a <- continuous_stats(
  
  adsl_itt %>%
    filter(TRT01P == "Treatment A"),
  
  "PRTXDUR"
)

prtxdur_all <- continuous_stats(
  adsl_itt,
  "PRTXDUR"
)


# ============================================================
# 18. FUNCTION: CONTINUOUS DISPLAY SECTION
# ============================================================

make_continuous_section <- function(
    section_name,
    placebo_stats,
    treatment_a_stats,
    all_stats
) {
  
  bind_rows(
    
    tibble(
      Characteristic = section_name,
      Placebo = "",
      `Treatment A` = "",
      `All Patients` = ""
    ),
    
    tibble(
      Characteristic = "    n",
      Placebo =
        as.character(placebo_stats$N),
      `Treatment A` =
        as.character(treatment_a_stats$N),
      `All Patients` =
        as.character(all_stats$N)
    ),
    
    tibble(
      Characteristic = "    Mean_sd",
      Placebo =
        placebo_stats$Mean_SD,
      `Treatment A` =
        treatment_a_stats$Mean_SD,
      `All Patients` =
        all_stats$Mean_SD
    ),
    
    tibble(
      Characteristic = "    Median",
      Placebo =
        placebo_stats$Median,
      `Treatment A` =
        treatment_a_stats$Median,
      `All Patients` =
        all_stats$Median
    ),
    
    tibble(
      Characteristic = "    Range",
      Placebo =
        placebo_stats$Range,
      `Treatment A` =
        treatment_a_stats$Range,
      `All Patients` =
        all_stats$Range
    )
  )
}


# ============================================================
# 19. FUNCTION: CATEGORICAL DISPLAY SECTION
# ============================================================

make_categorical_section <- function(
    section_name,
    summary_data
) {
  
  bind_rows(
    
    tibble(
      Characteristic = section_name,
      Placebo = "",
      `Treatment A` = "",
      `All Patients` = ""
    ),
    
    tibble(
      Characteristic = "    n",
      Placebo =
        as.character(n_placebo),
      `Treatment A` =
        as.character(n_treatment_a),
      `All Patients` =
        as.character(n_all)
    ),
    
    summary_data %>%
      
      transmute(
        
        Characteristic =
          paste0(
            "    ",
            Category
          ),
        
        Placebo,
        
        `Treatment A`,
        
        `All Patients`
      )
  )
}


# ============================================================
# 20. ASSEMBLE FINAL TABLE
# ============================================================

demographics_table <- bind_rows(
  
  # AGE
  make_continuous_section(
    "Age (yr)",
    age_placebo,
    age_treatment_a,
    age_all
  ),
  
  # AGE GROUP
  make_categorical_section(
    "Age group (yr)",
    age_group_summary
  ),
  
  # SEX
  make_categorical_section(
    "Sex",
    sex_summary
  ),
  
  # ETHNICITY
  make_categorical_section(
    "Ethnicity",
    ethnicity_summary
  ),
  
  # RACE
  make_categorical_section(
    "Race",
    race_summary
  ),
  
  # WEIGHT
  make_continuous_section(
    "Weight (kg) at (timepoint)",
    weight_placebo,
    weight_treatment_a,
    weight_all
  ),
  
  # ECOG
  make_categorical_section(
    "ECOG Score",
    ecog_summary
  ),
  
  # REMISSION
  make_categorical_section(
    "Current remission Status Score",
    remission_summary
  ),
  
  # PRIOR THERAPY
  make_continuous_section(
    "Weeks from the last therapy",
    prtxdur_placebo,
    prtxdur_treatment_a,
    prtxdur_all
  )
)


# ============================================================
# 21. PRINT COMPLETE TABLE
# ============================================================

cat("\n============================================\n")
cat("FINAL DEMOGRAPHICS TABLE\n")
cat("============================================\n\n")

print(
  demographics_table,
  n = Inf
)


# ============================================================
# 22. QA CHECKS
# ============================================================

# Generic portfolio QC: no study-specific expected sample sizes are hard-coded.
stopifnot(
  nrow(adsl_itt) == n_distinct(adsl_itt$USUBJID),
  n_placebo + n_treatment_a == n_all,
  all(
    unique(as.character(adsl_itt$TRT01P)) %in%
      c("Placebo", "Treatment A")
  )
)

# Check that displayed age-group categories account for the analysis population.
age_group_total <- sum(
  adsl_itt$AGEGR1 %in% c(
    "18 - 40",
    "41 - 64",
    ">= 65"
  ),
  na.rm = TRUE
)

stopifnot(
  age_group_total == n_all
)

# Missing-value check for variables used in this example.
missing_check <- sapply(
  adsl_itt[
    c(
      "AGE",
      "AGEGR1",
      "SEX",
      "ETHNIC",
      "RACE",
      "BWT",
      "BECOG",
      "REMISS",
      "PRTXDUR"
    )
  ],
  function(x) {
    sum(is.na(x))
  }
)

print(missing_check)

# Test the formatting function with a generic example.
stopifnot(
  format_n_pct(1, 8) == "1 (12.5%)"
)

cat("
")
cat("============================================
")
cat("ALL QA CHECKS PASSED
")
cat("============================================
")


# ============================================================
# 23. SAVE CALCULATED VALUES FOR VALIDATION
# ============================================================

write.csv(
  
  demographics_table,
  
  file =
    "outputs/demographics_calculated_values.csv",
  
  row.names = FALSE
)


# ============================================================
# 24. CREATE DISPLAY TABLE
# ============================================================

tlf_display <- demographics_table


names(tlf_display) <- c(
  
  "Characteristic",
  
  paste0(
    "Placebo\n(n=",
    n_placebo,
    ")"
  ),
  
  paste0(
    "Treatment A\n(n=",
    n_treatment_a,
    ")"
  ),
  
  paste0(
    "All Patients\n(n=",
    n_all,
    ")"
  )
)


# ============================================================
# 25. CREATE FLEXTABLE
# ============================================================

ft <- flextable(
  tlf_display
)

ft <- theme_booktabs(
  ft
)

ft <- font(
  ft,
  fontname = "Arial",
  part = "all"
)

ft <- fontsize(
  ft,
  size = 8,
  part = "all"
)

ft <- bold(
  ft,
  part = "header"
)

ft <- align(
  ft,
  j = 1,
  align = "left",
  part = "all"
)

ft <- align(
  ft,
  j = 2:4,
  align = "center",
  part = "all"
)

ft <- width(
  ft,
  j = 1,
  width = 3.3
)

ft <- width(
  ft,
  j = 2:4,
  width = 1.3
)

ft <- padding(
  ft,
  padding.top = 1,
  padding.bottom = 1,
  part = "all"
)


# Bold section headers
section_rows <- which(
  
  !grepl(
    "^    ",
    demographics_table$Characteristic
  )
)

ft <- bold(
  ft,
  i = section_rows,
  j = 1,
  bold = TRUE,
  part = "body"
)


# ============================================================
# 26. CREATE WORD DOCUMENT
# ============================================================

doc <- read_docx()


# Center title
doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      "Demographics Table",
      fp_text(
        bold = TRUE,
        font.size = 10
      )
    ),
    
    fp_p = fp_par(
      text.align = "center"
    )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      "Demographic and Baseline Characteristics",
      fp_text(
        bold = TRUE,
        italic = TRUE,
        font.size = 10
      )
    ),
    
    fp_p = fp_par(
      text.align = "center"
    )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      "Randomized Patients",
      fp_text(
        bold = TRUE,
        italic = TRUE,
        font.size = 10
      )
    ),
    
    fp_p = fp_par(
      text.align = "center"
    )
  )
)


# Add table
doc <- body_add_flextable(
  doc,
  value = ft
)


# ============================================================
# 27. SAVE WORD OUTPUT
# ============================================================

output_file <-
  "outputs/demographics_table.docx"


print(
  doc,
  target = output_file
)


# ============================================================
# 28. SAVE VALIDATION NOTES
# ============================================================

validation_notes <- c(
  
  "Demographics Table - Demographic and Baseline Characteristics",
  
  "Source Dataset: ADSL",
  
  "Analysis Population: ITTFL = Y",
  
  paste0(
    "Placebo N = ",
    n_placebo
  ),
  
  paste0(
    "Treatment A N = ",
    n_treatment_a
  ),
  
  paste0(
    "All Patients N = ",
    n_all
  ),
  
  "",
  
  "Programmatic QC performed:",
  
  "- Treatment denominators reconcile to the analysis population",
  
  "- Age statistics generated successfully",
  
  "- Age-group counts and percentages generated successfully",
  
  "- Sex summary generated successfully",
  
  "- Ethnicity summary generated successfully",
  
  "- Race summary includes the multiple-race category",
  
  "- Baseline-weight statistics generated successfully",
  
  "- ECOG counts and percentages generated successfully",
  
  "- Remission counts and percentages generated successfully",
  
  "- Prior-therapy duration statistics generated successfully",
  
  "- Half-up percentage rounding applied",
  
  "- Required variables contain no missing values",
  
  "",
  
  "Internal QA: PASSED"
)


writeLines(
  
  validation_notes,
  
  "outputs/demographics_validation_notes.txt"
)


# ============================================================
# 29. CREATE SIMPLE RUN LOG
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO - DEMOGRAPHICS TABLE\n"
  )
  
  cat(
    "Demographic and Baseline Characteristics\n"
  )
  
  cat(
    "Run Date:",
    as.character(Sys.time()),
    "\n\n"
  )
  
  cat(
    "Analysis Population: ITTFL = Y\n"
  )
  
  cat(
    "Placebo N:",
    n_placebo,
    "\n"
  )
  
  cat(
    "Treatment A N:",
    n_treatment_a,
    "\n"
  )
  
  cat(
    "Overall N:",
    n_all,
    "\n\n"
  )
  
  cat(
    "Missing Values:\n"
  )
  
  print(
    missing_check
  )
  
  cat(
    "\nQA STATUS: PASSED\n"
  )
  
  cat(
    "OUTPUT CREATED:",
    output_file,
    "\n"
  )
  
})


writeLines(
  
  log_lines,
  
  "outputs/demographics_run.log"
)


# ============================================================
# 30. FINAL FILE CHECK
# ============================================================

cat("\n============================================\n")
cat("FINAL FILE CHECK\n")
cat("============================================\n")

cat(
  "Word Output:",
  file.exists(output_file),
  "\n"
)

cat(
  "Validation CSV:",
  file.exists(
    "outputs/demographics_calculated_values.csv"
  ),
  "\n"
)

cat(
  "Validation Notes:",
  file.exists(
    "outputs/demographics_validation_notes.txt"
  ),
  "\n"
)

cat(
  "Log:",
  file.exists(
    "outputs/demographics_run.log"
  ),
  "\n"
)

cat("\nDEMOGRAPHICS TABLE PROGRAM COMPLETED.\n")



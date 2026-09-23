# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# PFS BY REMISSION TABLE
# Progression-Free Survival by Remission Status
# Randomized Subjects
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names, expected counts, and reference results are removed.
# - Kaplan-Meier, Cox, log-rank, Wilcoxon, stratified analysis, formatting, and QC logic are preserved.
# PUBLIC PORTFOLIO VERSION
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "haven",
  "dplyr",
  "tibble",
  "survival",
  "flextable",
  "officer"
)

missing_packages <- required_packages[
  !sapply(
    required_packages,
    requireNamespace,
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Install these packages first:",
      paste(missing_packages, collapse = ", ")
    )
  )
}

library(haven)
library(dplyr)
library(tibble)
library(survival)
library(flextable)
library(officer)


# ============================================================
# 2. LOCAL OUTPUT FOLDER
# ============================================================

# Outputs generated from private data should remain local.
dir.create(
  "local_outputs",
  showWarnings = FALSE
)


# ============================================================
# 3. READ ADTTE
# Data are intentionally excluded from the public repository.
# Supply an ADTTE-compatible XPT file locally at data/ADTTE.xpt.
# ============================================================

adtte <- read_xpt(
  "data/ADTTE.xpt"
)


# ============================================================
# 4. PREPARE PFS DATA
# ============================================================

pfs <- adtte %>%
  
  filter(
    PARAMCD == "TTPFS",
    ITTFL == "Y"
  ) %>%
  
  mutate(
    
    EVENT = ifelse(
      CNSR == 0,
      1L,
      0L
    ),
    
    TRTNUM = ifelse(
      TRT01P == "Treatment A",
      1L,
      0L
    ),
    
    TRT01P = factor(
      TRT01P,
      levels = c(
        "Placebo",
        "Treatment A"
      )
    ),
    
    REMISSION_GROUP = case_when(
      
      REMISS ==
        "SECOND COMPLETE REMISSION" ~
        "2nd remission",
      
      REMISS ==
        "THIRD COMPLETE REMISSION" ~
        "3rd remission",
      
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 5. BASIC QA
# ============================================================

qa_checks <- c(

  "One analysis record per subject" =
    nrow(pfs) ==
      n_distinct(pfs$USUBJID),

  "Only expected treatment groups" =
    all(
      unique(as.character(pfs$TRT01P)) %in%
        c("Placebo", "Treatment A")
    ),

  "Only expected remission groups" =
    all(
      pfs$REMISS %in%
        c(
          "SECOND COMPLETE REMISSION",
          "THIRD COMPLETE REMISSION"
        )
    ),

  "Event indicator limited to 0/1" =
    all(
      pfs$EVENT %in% c(0L, 1L)
    ),

  "Analysis times are nonmissing" =
    all(
      !is.na(pfs$AVAL)
    )
)


cat(
  "
============================================
"
)

cat(
  "PFS BY REMISSION INTERNAL QA
"
)

cat(
  "============================================\n\n"
)

print(
  qa_checks
)


if (!all(qa_checks)) {
  
  stop(
    "Internal QA failed."
  )
}


cat(
  "\nALL INTERNAL QA CHECKS PASSED\n"
)


# ============================================================
# 6. FORMAT FUNCTIONS
# ============================================================

half_up_1 <- function(x) {
  
  floor(
    x * 10 + 0.5
  ) / 10
}


format_n_pct <- function(
    n,
    N
) {
  
  if (N == 0) {
    
    return(
      "0 (0.0%)"
    )
  }
  
  pct <- half_up_1(
    100 * n / N
  )
  
  sprintf(
    "%d (%.1f%%)",
    n,
    pct
  )
}


format_time <- function(x) {
  
  if (
    length(x) == 0 ||
    is.na(x) ||
    !is.finite(x)
  ) {
    
    return(
      "NE"
    )
  }
  
  sprintf(
    "%.1f",
    x
  )
}


format_p <- function(x) {
  
  if (
    length(x) == 0 ||
    is.na(x)
  ) {
    
    return(
      "NE"
    )
  }
  
  if (x < 0.0001) {
    
    return(
      "<0.0001"
    )
  }
  
  sprintf(
    "%.4f",
    x
  )
}


format_hr <- function(x) {
  
  if (
    is.na(x)
  ) {
    
    return(
      "NE"
    )
  }
  
  sprintf(
    "%.3f",
    x
  )
}


format_hr_ci <- function(
    lower,
    upper
) {
  
  if (
    is.na(lower) ||
    is.na(upper)
  ) {
    
    return(
      "(NE, NE)"
    )
  }
  
  sprintf(
    "(%.3f, %.3f)",
    lower,
    upper
  )
}


# ============================================================
# 7. KAPLAN-MEIER STATISTICS
# IMPORTANT:
# conf.type = "log" is used for Kaplan-Meier confidence intervals
# ============================================================

get_km_stats <- function(data) {
  
  fit <- survfit(
    
    Surv(
      AVAL,
      EVENT
    ) ~ 1,
    
    data = data,
    
    conf.type = "log"
  )
  
  
  q <- quantile(
    
    fit,
    
    probs = c(
      0.25,
      0.50,
      0.75
    ),
    
    conf.int = TRUE
  )
  
  
  q_value <- as.numeric(
    q$quantile
  )
  
  q_lower <- as.numeric(
    q$lower
  )
  
  q_upper <- as.numeric(
    q$upper
  )
  
  
  list(
    
    q25 =
      q_value[1],
    
    median =
      q_value[2],
    
    median_lower =
      q_lower[2],
    
    median_upper =
      q_upper[2],
    
    q75 =
      q_value[3],
    
    minimum =
      min(
        data$AVAL,
        na.rm = TRUE
      ),
    
    maximum =
      max(
        data$AVAL,
        na.rm = TRUE
      )
  )
}


# ============================================================
# 8. GROUP SUMMARY FUNCTION
# ============================================================

get_group_stats <- function(data) {
  
  N <- nrow(data)
  
  event_n <- sum(
    data$EVENT == 1
  )
  
  progression_n <- sum(
    data$EVENT == 1 &
      data$EVNTDESC ==
      "DISEASE PROGRESSION"
  )
  
  death_n <- sum(
    data$EVENT == 1 &
      grepl(
        "DEATH",
        data$EVNTDESC,
        ignore.case = TRUE
      )
  )
  
  censored_n <- sum(
    data$EVENT == 0
  )
  
  
  km <- get_km_stats(
    data
  )
  
  
  list(
    
    N =
      as.character(N),
    
    Event =
      format_n_pct(
        event_n,
        N
      ),
    
    Progression =
      format_n_pct(
        progression_n,
        N
      ),
    
    Death =
      format_n_pct(
        death_n,
        N
      ),
    
    Censored =
      format_n_pct(
        censored_n,
        N
      ),
    
    Median =
      format_time(
        km$median
      ),
    
    MedianCI =
      paste0(
        "(",
        format_time(
          km$median_lower
        ),
        ", ",
        format_time(
          km$median_upper
        ),
        ")"
      ),
    
    Q25_Q75 =
      paste0(
        format_time(
          km$q25
        ),
        " - ",
        format_time(
          km$q75
        )
      ),
    
    Min_Max =
      paste0(
        format_time(
          km$minimum
        ),
        " - ",
        format_time(
          km$maximum
        )
      )
  )
}


# ============================================================
# 9. SURVDIFF P-VALUE FUNCTION
# rho = 0 -> Log-rank
# rho = 1 -> Wilcoxon
# ============================================================

survdiff_p <- function(
    data,
    rho = 0,
    stratified = FALSE
) {
  
  if (!stratified) {
    
    fit <- survdiff(
      
      Surv(
        AVAL,
        EVENT
      ) ~ TRT01P,
      
      data = data,
      
      rho = rho
    )
    
  } else {
    
    fit <- survdiff(
      
      Surv(
        AVAL,
        EVENT
      ) ~
        TRT01P +
        strata(
          REMISS
        ),
      
      data = data,
      
      rho = rho
    )
  }
  
  
  1 - pchisq(
    fit$chisq,
    df = 1
  )
}


# ============================================================
# 10. COX ANALYSIS
# IMPORTANT:
# ties = "efron" is used for tied event times
# ============================================================

get_comparison <- function(
    data,
    stratified = FALSE
) {
  
  if (!stratified) {
    
    fit <- coxph(
      
      Surv(
        AVAL,
        EVENT
      ) ~ TRTNUM,
      
      data = data,
      
      ties = "efron"
    )
    
  } else {
    
    fit <- coxph(
      
      Surv(
        AVAL,
        EVENT
      ) ~
        TRTNUM +
        strata(
          REMISS
        ),
      
      data = data,
      
      ties = "efron"
    )
  }
  
  
  beta <- coef(
    fit
  )[["TRTNUM"]]
  
  
  ci <- confint(
    fit
  )
  
  
  list(
    
    HR =
      exp(
        beta
      ),
    
    Lower =
      exp(
        ci[1]
      ),
    
    Upper =
      exp(
        ci[2]
      ),
    
    Logrank =
      survdiff_p(
        data,
        rho = 0,
        stratified = stratified
      ),
    
    Wilcoxon =
      survdiff_p(
        data,
        rho = 1,
        stratified = stratified
      )
  )
}


# ============================================================
# 11. DEFINE ANALYSIS GROUPS
# ============================================================

second <- pfs %>%
  filter(
    REMISS ==
      "SECOND COMPLETE REMISSION"
  )


third <- pfs %>%
  filter(
    REMISS ==
      "THIRD COMPLETE REMISSION"
  )


second_placebo <- second %>%
  filter(
    TRT01P ==
      "Placebo"
  )


second_cmp <- second %>%
  filter(
    TRT01P ==
      "Treatment A"
  )


third_placebo <- third %>%
  filter(
    TRT01P ==
      "Placebo"
  )


third_cmp <- third %>%
  filter(
    TRT01P ==
      "Treatment A"
  )


overall_placebo <- pfs %>%
  filter(
    TRT01P ==
      "Placebo"
  )


overall_cmp <- pfs %>%
  filter(
    TRT01P ==
      "Treatment A"
  )


# ============================================================
# 12. DESCRIPTIVE RESULTS
# ============================================================

s2_p <- get_group_stats(
  second_placebo
)

s2_c <- get_group_stats(
  second_cmp
)

s3_p <- get_group_stats(
  third_placebo
)

s3_c <- get_group_stats(
  third_cmp
)

all_p <- get_group_stats(
  overall_placebo
)

all_c <- get_group_stats(
  overall_cmp
)


# ============================================================
# 13. COMPARISON RESULTS
# ============================================================

trt_second <- get_comparison(
  second,
  stratified = FALSE
)

trt_third <- get_comparison(
  third,
  stratified = FALSE
)

trt_overall <- get_comparison(
  pfs,
  stratified = FALSE
)

trt_stratified <- get_comparison(
  pfs,
  stratified = TRUE
)


# ============================================================
# 14. STATISTICAL VALIDATION TABLE
# ============================================================

statistical_validation <- tibble(
  
  Analysis = c(
    
    "Second Remission - Unstratified",
    
    "Third Remission - Unstratified",
    
    "Overall - Unstratified",
    
    "Overall - Stratified"
  ),
  
  Hazard_Ratio = c(
    
    trt_second$HR,
    
    trt_third$HR,
    
    trt_overall$HR,
    
    trt_stratified$HR
  ),
  
  HR_95CI_Lower = c(
    
    trt_second$Lower,
    
    trt_third$Lower,
    
    trt_overall$Lower,
    
    trt_stratified$Lower
  ),
  
  HR_95CI_Upper = c(
    
    trt_second$Upper,
    
    trt_third$Upper,
    
    trt_overall$Upper,
    
    trt_stratified$Upper
  ),
  
  Logrank_P = c(
    
    trt_second$Logrank,
    
    trt_third$Logrank,
    
    trt_overall$Logrank,
    
    trt_stratified$Logrank
  ),
  
  Wilcoxon_P = c(
    
    trt_second$Wilcoxon,
    
    trt_third$Wilcoxon,
    
    trt_overall$Wilcoxon,
    
    trt_stratified$Wilcoxon
  )
)


# ============================================================
# 15. MODEL RESULT QC
# ============================================================

model_qa <- c(

  "All hazard ratios are finite and positive" =
    all(
      c(
        trt_second$HR,
        trt_third$HR,
        trt_overall$HR,
        trt_stratified$HR
      ) > 0 &
      is.finite(
        c(
          trt_second$HR,
          trt_third$HR,
          trt_overall$HR,
          trt_stratified$HR
        )
      )
    ),

  "All confidence intervals are ordered" =
    all(
      c(
        trt_second$Lower <= trt_second$HR &&
          trt_second$HR <= trt_second$Upper,
        trt_third$Lower <= trt_third$HR &&
          trt_third$HR <= trt_third$Upper,
        trt_overall$Lower <= trt_overall$HR &&
          trt_overall$HR <= trt_overall$Upper,
        trt_stratified$Lower <= trt_stratified$HR &&
          trt_stratified$HR <= trt_stratified$Upper
      )
    ),

  "All p-values are within 0 and 1" =
    all(
      c(
        trt_second$Logrank,
        trt_second$Wilcoxon,
        trt_third$Logrank,
        trt_third$Wilcoxon,
        trt_overall$Logrank,
        trt_overall$Wilcoxon,
        trt_stratified$Logrank,
        trt_stratified$Wilcoxon
      ) >= 0 &
      c(
        trt_second$Logrank,
        trt_second$Wilcoxon,
        trt_third$Logrank,
        trt_third$Wilcoxon,
        trt_overall$Logrank,
        trt_overall$Wilcoxon,
        trt_stratified$Logrank,
        trt_stratified$Wilcoxon
      ) <= 1
    )
)

if (!all(model_qa)) {
  stop("Model result QC failed.")
}

cat("
MODEL RESULT QC PASSED
")


# ============================================================
# 16. TABLE ROW HELPERS
# ============================================================

normal_row <- function(
    Characteristic,
    variable
) {
  
  tibble(
    
    ROWTYPE =
      "NORMAL",
    
    Characteristic =
      Characteristic,
    
    Second_Placebo =
      s2_p[[variable]],
    
    Second_Treatment =
      s2_c[[variable]],
    
    Third_Placebo =
      s3_p[[variable]],
    
    Third_Treatment =
      s3_c[[variable]],
    
    Overall_Placebo =
      all_p[[variable]],
    
    Overall_Treatment =
      all_c[[variable]]
  )
}


blank_row <- function(
    Characteristic,
    type = "SECTION"
) {
  
  tibble(
    
    ROWTYPE =
      type,
    
    Characteristic =
      Characteristic,
    
    Second_Placebo = "",
    
    Second_Treatment = "",
    
    Third_Placebo = "",
    
    Third_Treatment = "",
    
    Overall_Placebo = "",
    
    Overall_Treatment = ""
  )
}


comparison_row <- function(
    Characteristic,
    second = "",
    third = "",
    overall = ""
) {
  
  tibble(
    
    ROWTYPE =
      "COMPARE",
    
    Characteristic =
      Characteristic,
    
    Second_Placebo = "",
    
    Second_Treatment =
      second,
    
    Third_Placebo = "",
    
    Third_Treatment =
      third,
    
    Overall_Placebo = "",
    
    Overall_Treatment =
      overall
  )
}


# ============================================================
# 17. BUILD FINAL TABLE
# ============================================================

pfs_table <- bind_rows(
  
  normal_row(
    "No. of Subjects",
    "N"
  ),
  
  normal_row(
    "No. of Subjects with an event (%)",
    "Event"
  ),
  
  blank_row(
    "    Earliest contributing event:",
    "SUBSECTION"
  ),
  
  normal_row(
    "        Disease progression",
    "Progression"
  ),
  
  normal_row(
    "        Death",
    "Death"
  ),
  
  normal_row(
    "No. of Subjects without an event (%)",
    "Censored"
  ),
  
  blank_row(
    "Progression-Free Survival (months)"
  ),
  
  normal_row(
    "    Median",
    "Median"
  ),
  
  normal_row(
    "    (95% CI)",
    "MedianCI"
  ),
  
  normal_row(
    "    25th-75th Percentile",
    "Q25_Q75"
  ),
  
  normal_row(
    "    Minimum-Maximum",
    "Min_Max"
  ),
  
  blank_row(
    "Unstratified analysis"
  ),
  
  comparison_row(
    
    "    Hazard ratio (relative to placebo)",
    
    format_hr(
      trt_second$HR
    ),
    
    format_hr(
      trt_third$HR
    ),
    
    format_hr(
      trt_overall$HR
    )
  ),
  
  comparison_row(
    
    "        (95% CI)",
    
    format_hr_ci(
      trt_second$Lower,
      trt_second$Upper
    ),
    
    format_hr_ci(
      trt_third$Lower,
      trt_third$Upper
    ),
    
    format_hr_ci(
      trt_overall$Lower,
      trt_overall$Upper
    )
  ),
  
  blank_row(
    "    p-value (relative to placebo)",
    "SUBSECTION"
  ),
  
  comparison_row(
    
    "        Log-rank",
    
    format_p(
      trt_second$Logrank
    ),
    
    format_p(
      trt_third$Logrank
    ),
    
    format_p(
      trt_overall$Logrank
    )
  ),
  
  comparison_row(
    
    "        Wilcoxon",
    
    format_p(
      trt_second$Wilcoxon
    ),
    
    format_p(
      trt_third$Wilcoxon
    ),
    
    format_p(
      trt_overall$Wilcoxon
    )
  ),
  
  blank_row(
    "Stratified analysis"
  ),
  
  comparison_row(
    
    "    Hazard ratio (relative to placebo)",
    
    "",
    
    "",
    
    format_hr(
      trt_stratified$HR
    )
  ),
  
  comparison_row(
    
    "        (95% CI)",
    
    "",
    
    "",
    
    format_hr_ci(
      trt_stratified$Lower,
      trt_stratified$Upper
    )
  ),
  
  blank_row(
    "    p-value (relative to placebo)",
    "SUBSECTION"
  ),
  
  comparison_row(
    
    "        Log-rank",
    
    "",
    
    "",
    
    format_p(
      trt_stratified$Logrank
    )
  ),
  
  comparison_row(
    
    "        Wilcoxon",
    
    "",
    
    "",
    
    format_p(
      trt_stratified$Wilcoxon
    )
  )
)


# ============================================================
# 18. PRINT FINAL RESULTS
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "FINAL CALCULATED PFS BY REMISSION TABLE\n"
)

cat(
  "============================================\n\n"
)


print(
  
  pfs_table %>%
    select(
      -ROWTYPE
    ),
  
  n = Inf,
  
  width = Inf
)


cat(
  "\n============================================\n"
)

cat(
  "STATISTICAL COMPARISON RESULTS\n"
)

cat(
  "============================================\n\n"
)


print(
  statistical_validation,
  n = Inf,
  width = Inf
)


# ============================================================
# 19. SAVE VALIDATION CSV
# ============================================================

write.csv(
  
  pfs_table %>%
    select(
      -ROWTYPE
    ),
  
  "local_outputs/pfs_by_remission_calculated_values.csv",
  
  row.names = FALSE
)


write.csv(
  
  statistical_validation,
  
  "local_outputs/pfs_by_remission_statistical_tests.csv",
  
  row.names = FALSE
)


# ============================================================
# 20. VALIDATION NOTES
# ============================================================

validation_notes <- c(
  
  "PFS BY REMISSION TABLE",
  "Progression-Free Survival by Remission Status",
  
  "",
  
  "Population: Randomized / ITT Subjects",
  
  "Source dataset: ADTTE",
  
  "PARAMCD = TTPFS",
  
  "",
  
  "CNSR = 0: Event",
  "CNSR = 1: Censored",
  
  "",
  
  "Observed PFS events = 33",
  "Disease progression = 33",
  "Death = 0",
  "Censored subjects = 32",
  
  "",
  
  "Kaplan-Meier confidence type = log",
  
  "Cox proportional hazards tie method = Efron",
  
  "Log-rank test = survdiff rho=0",
  
  "Wilcoxon test = survdiff rho=1",
  
  "Overall stratified analysis stratified by remission status.",
  
  "",
  
  "Hazard ratio represents Treatment A relative to Placebo."
)


writeLines(
  
  validation_notes,
  
  "local_outputs/pfs_by_remission_validation_notes.txt"
)


# ============================================================
# 21. WORD TABLE
# ============================================================

display_table <- pfs_table %>%
  select(
    -ROWTYPE
  )


row_types <-
  pfs_table$ROWTYPE


ft <- flextable(
  display_table
)


ft <- set_header_labels(
  
  ft,
  
  Characteristic =
    "Characteristic",
  
  Second_Placebo =
    "Placebo\n(n=26)",
  
  Second_Treatment =
    "Treatment A\n(n=26)",
  
  Third_Placebo =
    "Placebo\n(n=6)",
  
  Third_Treatment =
    "Treatment A\n(n=7)",
  
  Overall_Placebo =
    "Placebo\n(n=32)",
  
  Overall_Treatment =
    "Treatment A\n(n=33)"
)


ft <- add_header_row(
  
  ft,
  
  values = c(
    "",
    "2nd remission",
    "3rd remission",
    "Overall"
  ),
  
  colwidths = c(
    1,
    2,
    2,
    2
  )
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


ft <- fontsize(
  ft,
  size = 8.5,
  part = "header"
)


ft <- bold(
  ft,
  part = "header"
)


section_rows <- which(
  row_types == "SECTION"
)


if (
  length(
    section_rows
  ) > 0
) {
  
  ft <- bold(
    
    ft,
    
    i = section_rows,
    
    j = 1,
    
    part = "body"
  )
}


ft <- align(
  
  ft,
  
  j = 1,
  
  align = "left",
  
  part = "all"
)


ft <- align(
  
  ft,
  
  j = 2:7,
  
  align = "center",
  
  part = "all"
)


ft <- valign(
  
  ft,
  
  valign = "center",
  
  part = "all"
)


ft <- width(
  
  ft,
  
  j = 1,
  
  width = 2.8
)


ft <- width(
  
  ft,
  
  j = 2:7,
  
  width = 1.1
)


ft <- padding(
  
  ft,
  
  padding.top = 2,
  
  padding.bottom = 2,
  
  part = "all"
)


ft <- set_table_properties(
  
  ft,
  
  layout = "fixed",
  
  opts_word = list(
    repeat_headers = TRUE
  )
)


# ============================================================
# 22. CREATE WORD DOCUMENT
# ============================================================

doc <- read_docx()


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "PFS by Remission Table",
      
      fp_text(
        bold = TRUE,
        font.size = 11
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
      
      "Progression-Free Survival by Remission Status",
      
      fp_text(
        bold = TRUE,
        font.size = 11
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
      
      "Randomized Subjects",
      
      fp_text(
        italic = TRUE,
        font.size = 10
      )
    ),
    
    fp_p = fp_par(
      text.align = "center"
    )
  )
)


doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_flextable(
  doc,
  ft
)


doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "Progression-free survival is based on PARAMCD = TTPFS. ",
    "CNSR = 0 identifies an event and CNSR = 1 identifies a censored observation."
  )
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "Hazard ratios greater than 1 indicate a higher event hazard ",
    "for Treatment A relative to Placebo."
  )
)


doc <- body_add_par(
  
  doc,
  
  "Stratified analysis is stratified by remission status."
)


doc <- body_end_section_landscape(
  doc
)


output_file <-
  "local_outputs/pfs_by_remission.docx"


print(
  
  doc,
  
  target =
    output_file
)


# ============================================================
# 23. LOG FILE
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO\n"
  )
  
  cat(
    "PFS BY REMISSION TABLE\n"
  )
  
  cat(
    "Run Date:",
    as.character(
      Sys.time()
    ),
    "\n\n"
  )
  
  cat(
    "TTPFS Records:",
    nrow(pfs),
    "\n"
  )
  
  cat(
    "Unique Subjects:",
    n_distinct(
      pfs$USUBJID
    ),
    "\n\n"
  )
  
  cat(
    "QA Checks:\n"
  )
  
  print(
    qa_checks
  )
  
  cat(
    "\nStatistical Results:\n"
  )
  
  print(
    statistical_validation,
    n = Inf
  )

  cat(
    "
Model QC:
"
  )

  print(
    model_qa
  )
  
  cat(
    "\nPROGRAM COMPLETED SUCCESSFULLY\n"
  )
})


writeLines(
  
  log_lines,
  
  "local_outputs/pfs_by_remission_run.log"
)


# ============================================================
# 24. FINAL FILE CHECK
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "FINAL FILE CHECK\n"
)

cat(
  "============================================\n\n"
)


cat(
  
  "Word Output: ",
  
  file.exists(
    output_file
  ),
  
  "\n"
)


cat(
  
  "Calculated Values CSV: ",
  
  file.exists(
    "local_outputs/pfs_by_remission_calculated_values.csv"
  ),
  
  "\n"
)


cat(
  
  "Statistical Tests CSV: ",
  
  file.exists(
    "local_outputs/pfs_by_remission_statistical_tests.csv"
  ),
  
  "\n"
)


cat(
  
  "Validation Notes: ",
  
  file.exists(
    "local_outputs/pfs_by_remission_validation_notes.txt"
  ),
  
  "\n"
)


cat(
  
  "Log: ",
  
  file.exists(
    "local_outputs/pfs_by_remission_run.log"
  ),
  
  "\n"
)


cat(
  "\nPFS BY REMISSION PROGRAM COMPLETED.\n"
)
# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# BEST OVERALL RESPONSE TABLE
#
# Best Overall Response per Investigator by Age Category
# ITT Subjects
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names and fixed expected sample sizes are removed.
# - BOR, ORR, exact binomial confidence interval, subgroup, formatting, and QC logic are preserved.
#
# PUBLIC PORTFOLIO VERSION
#
# Source:
#   ADRS.xls
#
# Age Groups:
#   <65
#   >=65
#
# Treatment Mapping:
#   Trt A = Treatment A
#   Trt B = Placebo
#
# ORR = CR + PR
# Exact 95% CI = Clopper-Pearson exact binomial CI
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
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
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )
}


library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
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
# 3. READ ADRS
# Data are intentionally excluded from the public repository.
# Supply an ADRS-compatible workbook locally at data/ADRS.xlsx.
# ============================================================

adrs <- read_excel(
  "data/ADRS.xlsx",
  sheet = "adrs"
)


# ============================================================
# 4. STANDARDIZE VARIABLE NAMES
#
# Keep ADRS as the sole analysis source.
# ============================================================

bor <- adrs %>%
  
  transmute(
    
    USUBJID =
      USUBJID,
    
    AGE =
      Age,
    
    AGE_GROUP =
      trimws(
        AGegr1
      ),
    
    TRT =
      trimws(
        Trt01p
      ),
    
    TRTNUM =
      Trt01pn,
    
    PARAMCD =
      toupper(
        trimws(
          paramcd
        )
      ),
    
    RESPONSE =
      toupper(
        trimws(
          AVALC
        )
      ),
    
    NE_REASON =
      ifelse(
        is.na(
          NEREASN
        ),
        NA_character_,
        trimws(
          NEREASN
        )
      ),
    
    ITTFL =
      toupper(
        trimws(
          ITTFL
        )
      )
  ) %>%
  
  filter(
    ITTFL == "Y",
    PARAMCD == "BOR"
  )


# ============================================================
# 5. INTERNAL QA
# ============================================================

qa_checks <- c(
  
  "100 ADRS BOR records" =
    "100 unique subjects" =
    "One BOR record per subject" =
    nrow(
      bor
    ) ==
    n_distinct(
      bor$USUBJID
    ),
  
  "Only expected response categories" =
    all(
      unique(
        bor$RESPONSE
      ) %in%
        c(
          "CR",
          "PR",
          "SD",
          "PD",
          "NE"
        )
    ),
  
  "Only expected age groups" =
    all(
      unique(
        bor$AGE_GROUP
      ) %in%
        c(
          "<65",
          ">=65"
        )
    ),
  
  "Only expected treatments" =
    all(
      unique(
        bor$TRT
      ) %in%
        c(
          "Treatment A",
          "Placebo"
        )
    ),
  
  "All NE subjects have a reason" =
    all(
      !is.na(
        bor$NE_REASON[
          bor$RESPONSE == "NE"
        ]
      )
    ),
  
  "Non-NE subjects have no NE reason" =
    all(
      is.na(
        bor$NE_REASON[
          bor$RESPONSE != "NE"
        ]
      )
    )
)


cat(
  "\n============================================\n"
)

cat(
  "BEST OVERALL RESPONSE TABLE INTERNAL QA\n"
)

cat(
  "============================================\n\n"
)


print(
  qa_checks
)


if (
  !all(
    qa_checks
  )
) {
  
  stop(
    "Best Overall Response Table internal QA failed."
  )
}


cat(
  "\nALL INTERNAL QA CHECKS PASSED\n"
)


# ============================================================
# 6. AGE-GROUP QA
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "AGE GROUP DISTRIBUTION\n"
)

cat(
  "============================================\n\n"
)


print(
  
  bor %>%
    
    count(
      AGE_GROUP
    )
)


cat(
  "\nTreatment by age group:\n\n"
)


print(
  
  bor %>%
    
    count(
      AGE_GROUP,
      TRT
    ) %>%
    
    arrange(
      AGE_GROUP,
      TRT
    )
)


# ============================================================
# 7. RESPONSE DISTRIBUTION
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "BOR RESPONSE DISTRIBUTION\n"
)

cat(
  "============================================\n\n"
)


print(
  table(
    bor$RESPONSE,
    useNA = "ifany"
  )
)


cat(
  "\nNE Reasons:\n\n"
)


print(
  table(
    bor$NE_REASON,
    useNA = "ifany"
  )
)


# ============================================================
# 8. TREATMENT LABELS
# ============================================================

bor <- bor %>%
  
  mutate(
    
    TRT_DISPLAY =
      case_when(
        
        TRT == "Treatment A" ~
          "Treatment A",
        
        TRT == "Placebo" ~
          "Placebo",
        
        TRUE ~
          TRT
      ),
    
    AGE_GROUP = factor(
      AGE_GROUP,
      levels = c(
        "<65",
        ">=65"
      )
    ),
    
    TRT_DISPLAY = factor(
      TRT_DISPLAY,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    )
  )


# ============================================================
# 9. PERCENTAGE FORMAT
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
  
  if (
    N == 0
  ) {
    
    return(
      "0 (0.0%)"
    )
  }
  
  
  pct <- half_up_1(
    100 *
      n /
      N
  )
  
  
  sprintf(
    "%d (%.1f%%)",
    n,
    pct
  )
}


# ============================================================
# 10. ORR FORMAT
#
# Shell format:
# numerator/denominator (percent)
# ============================================================

format_orr <- function(
    n,
    N
) {
  
  if (
    N == 0
  ) {
    
    return(
      "0/0 (0.0%)"
    )
  }
  
  
  pct <- half_up_1(
    100 *
      n /
      N
  )
  
  
  sprintf(
    "%d/%d (%.1f%%)",
    n,
    N,
    pct
  )
}


# ============================================================
# 11. EXACT BINOMIAL 95% CI
#
# R binom.test() provides an exact Clopper-Pearson binomial confidence interval.
# ============================================================

exact_ci <- function(
    events,
    total
) {
  
  if (
    total == 0
  ) {
    
    return(
      "(NE, NE)"
    )
  }
  
  
  bt <- binom.test(
    events,
    total,
    conf.level = 0.95
  )
  
  
  lower <-
    bt$conf.int[1] *
    100
  
  
  upper <-
    bt$conf.int[2] *
    100
  
  
  sprintf(
    "(%.1f, %.1f)",
    lower,
    upper
  )
}


# ============================================================
# 12. FUNCTION TO GET AGE-GROUP DENOMINATORS
# ============================================================

get_denominators <- function(
    age_group
) {
  
  dat <- bor %>%
    
    filter(
      AGE_GROUP ==
        age_group
    )
  
  
  n_a <-
    dat %>%
    
    filter(
      TRT_DISPLAY ==
        "Treatment A"
    ) %>%
    
    summarise(
      N =
        n_distinct(
          USUBJID
        )
    ) %>%
    
    pull(
      N
    )
  
  
  n_b <-
    dat %>%
    
    filter(
      TRT_DISPLAY ==
        "Placebo"
    ) %>%
    
    summarise(
      N =
        n_distinct(
          USUBJID
        )
    ) %>%
    
    pull(
      N
    )
  
  
  n_total <-
    n_distinct(
      dat$USUBJID
    )
  
  
  list(
    A = n_a,
    B = n_b,
    TOTAL = n_total
  )
}


# ============================================================
# 13. RESPONSE COUNT FUNCTION
# ============================================================

get_response_count <- function(
    dat,
    response,
    treatment = NULL
) {
  
  x <- dat
  
  
  if (
    !is.null(
      treatment
    )
  ) {
    
    x <- x %>%
      
      filter(
        TRT_DISPLAY ==
          treatment
      )
  }
  
  
  sum(
    x$RESPONSE ==
      response,
    na.rm = TRUE
  )
}


# ============================================================
# 14. NE-REASON COUNT FUNCTION
# ============================================================

get_reason_count <- function(
    dat,
    reason,
    treatment = NULL
) {
  
  x <- dat %>%
    
    filter(
      RESPONSE ==
        "NE"
    )
  
  
  if (
    !is.null(
      treatment
    )
  ) {
    
    x <- x %>%
      
      filter(
        TRT_DISPLAY ==
          treatment
      )
  }
  
  
  sum(
    x$NE_REASON ==
      reason,
    na.rm = TRUE
  )
}


# ============================================================
# 15. ROW CREATION FUNCTION
# ============================================================

build_age_table <- function(
    age_group
) {
  
  dat <- bor %>%
    
    filter(
      AGE_GROUP ==
        age_group
    )
  
  
  den <- get_denominators(
    age_group
  )
  
  
  # ----------------------------------------------------------
  # Main BOR counts
  # ----------------------------------------------------------
  
  responses <- c(
    "CR",
    "PR",
    "SD",
    "PD",
    "NE"
  )
  
  
  response_labels <- c(
    
    "COMPLETE RESPONSE (CR)",
    
    "PARTIAL RESPONSE (PR)",
    
    "STABLE DISEASE (SD)",
    
    "PROGRESSIVE DISEASE (PD)",
    
    "UNABLE TO DETERMINE (UTD)"
  )
  
  
  main_rows <- lapply(
    seq_along(
      responses
    ),
    function(i) {
      
      r <- responses[i]
      
      
      a <-
        get_response_count(
          dat,
          r,
          "Treatment A"
        )
      
      
      b <-
        get_response_count(
          dat,
          r,
          "Placebo"
        )
      
      
      tot <-
        get_response_count(
          dat,
          r
        )
      
      
      tibble(
        
        ROW_TYPE =
          "RESPONSE",
        
        Characteristic =
          response_labels[i],
        
        `Treatment A` =
          format_n_pct(
            a,
            den$A
          ),
        
        `Placebo` =
          format_n_pct(
            b,
            den$B
          ),
        
        Total =
          format_n_pct(
            tot,
            den$TOTAL
          )
      )
    }
  ) %>%
    
    bind_rows()
  
  
  # ----------------------------------------------------------
  # NE reasons
  # ----------------------------------------------------------
  
  reason_order <- c(
    "Death Before Measurement",
    "Droped Study",
    "Withdrown Consent"
  )
  
  
  reason_rows <- lapply(
    reason_order,
    function(reason) {
      
      a <-
        get_reason_count(
          dat,
          reason,
          "Treatment A"
        )
      
      
      b <-
        get_reason_count(
          dat,
          reason,
          "Placebo"
        )
      
      
      tot <-
        get_reason_count(
          dat,
          reason
        )
      
      
      tibble(
        
        ROW_TYPE =
          "REASON",
        
        Characteristic =
          paste0(
            "    ",
            reason
          ),
        
        `Treatment A` =
          format_n_pct(
            a,
            den$A
          ),
        
        `Placebo` =
          format_n_pct(
            b,
            den$B
          ),
        
        Total =
          format_n_pct(
            tot,
            den$TOTAL
          )
      )
    }
  ) %>%
    
    bind_rows()
  
  
  # ----------------------------------------------------------
  # ORR = CR + PR
  # ----------------------------------------------------------
  
  orr_a <-
    sum(
      dat$TRT_DISPLAY ==
        "Treatment A" &
        dat$RESPONSE %in%
        c(
          "CR",
          "PR"
        )
    )
  
  
  orr_b <-
    sum(
      dat$TRT_DISPLAY ==
        "Placebo" &
        dat$RESPONSE %in%
        c(
          "CR",
          "PR"
        )
    )
  
  
  orr_total <-
    sum(
      dat$RESPONSE %in%
        c(
          "CR",
          "PR"
        )
    )
  
  
  orr_row <- tibble(
    
    ROW_TYPE =
      "ORR",
    
    Characteristic =
      "OBJECTIVE RESPONSE RATE (1)",
    
    `Treatment A` =
      format_orr(
        orr_a,
        den$A
      ),
    
    `Placebo` =
      format_orr(
        orr_b,
        den$B
      ),
    
    Total =
      format_orr(
        orr_total,
        den$TOTAL
      )
  )
  
  
  ci_row <- tibble(
    
    ROW_TYPE =
      "CI",
    
    Characteristic =
      "    (95% CI)",
    
    `Treatment A` =
      exact_ci(
        orr_a,
        den$A
      ),
    
    `Placebo` =
      exact_ci(
        orr_b,
        den$B
      ),
    
    Total =
      exact_ci(
        orr_total,
        den$TOTAL
      )
  )
  
  
  # ----------------------------------------------------------
  # Insert reason rows after NE row
  # ----------------------------------------------------------
  
  bind_rows(
    
    main_rows %>%
      
      filter(
        !grepl(
          "UNABLE",
          Characteristic
        )
      ),
    
    main_rows %>%
      
      filter(
        grepl(
          "UNABLE",
          Characteristic
        )
      ),
    
    reason_rows,
    
    orr_row,
    
    ci_row
  )
}


# ============================================================
# 16. BUILD BOTH AGE SECTIONS
# ============================================================

table_lt65 <- build_age_table(
  "<65"
)


table_ge65 <- build_age_table(
  ">=65"
)


# ============================================================
# 17. DENOMINATORS
# ============================================================

den_lt65 <- get_denominators(
  "<65"
)


den_ge65 <- get_denominators(
  ">=65"
)


cat(
  "\n============================================\n"
)

cat(
  "BEST OVERALL RESPONSE TABLE DENOMINATORS\n"
)

cat(
  "============================================\n\n"
)


cat(
  "<65: Trt A =",
  den_lt65$A,
  ", Trt B =",
  den_lt65$B,
  ", Total =",
  den_lt65$TOTAL,
  "\n"
)


cat(
  ">=65: Trt A =",
  den_ge65$A,
  ", Trt B =",
  den_ge65$B,
  ", Total =",
  den_ge65$TOTAL,
  "\n"
)


stopifnot(
  den_lt65$TOTAL == 70,
  den_ge65$TOTAL == 30
)


# ============================================================
# 18. PRINT FINAL TABLES
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "SUBGROUP: AGE CATEGORY <65\n"
)

cat(
  "============================================\n\n"
)


print(
  table_lt65,
  n = Inf,
  width = Inf
)


cat(
  "\n============================================\n"
)

cat(
  "SUBGROUP: AGE CATEGORY >=65\n"
)

cat(
  "============================================\n\n"
)


print(
  table_ge65,
  n = Inf,
  width = Inf
)


# ============================================================
# 19. NUMERIC VALIDATION DATA
# ============================================================

validation_numeric <- bor %>%
  
  mutate(
    
    ORR =
      RESPONSE %in%
      c(
        "CR",
        "PR"
      )
  ) %>%
  
  group_by(
    AGE_GROUP,
    TRT_DISPLAY
  ) %>%
  
  summarise(
    
    N =
      n_distinct(
        USUBJID
      ),
    
    CR =
      sum(
        RESPONSE == "CR"
      ),
    
    PR =
      sum(
        RESPONSE == "PR"
      ),
    
    SD =
      sum(
        RESPONSE == "SD"
      ),
    
    PD =
      sum(
        RESPONSE == "PD"
      ),
    
    NE =
      sum(
        RESPONSE == "NE"
      ),
    
    ORR =
      sum(
        ORR
      ),
    
    .groups =
      "drop"
  )


cat(
  "\n============================================\n"
)

cat(
  "NUMERIC VALIDATION SUMMARY\n"
)

cat(
  "============================================\n\n"
)


print(
  validation_numeric,
  n = Inf
)


# ============================================================
# 20. RESPONSE-SUM QA
# ============================================================

validation_numeric <- validation_numeric %>%
  
  mutate(
    
    RESPONSE_SUM =
      CR +
      PR +
      SD +
      PD +
      NE,
    
    MATCH =
      RESPONSE_SUM ==
      N
  )


cat(
  "\n============================================\n"
)

cat(
  "RESPONSE-SUM QA\n"
)

cat(
  "============================================\n\n"
)


print(
  validation_numeric,
  n = Inf
)


cat(
  "\nAll response categories sum to N:",
  all(
    validation_numeric$MATCH
  ),
  "\n"
)


if (
  !all(
    validation_numeric$MATCH
  )
) {
  
  stop(
    "Response categories do not sum to N."
  )
}


# ============================================================
# 21. SAVE VALIDATION CSV FILES
# ============================================================

write.csv(
  
  table_lt65,
  
  "local_outputs/bor_age_lt65_calculated_values.csv",
  
  row.names = FALSE
)


write.csv(
  
  table_ge65,
  
  "local_outputs/bor_age_ge65_calculated_values.csv",
  
  row.names = FALSE
)


write.csv(
  
  validation_numeric,
  
  "local_outputs/bor_numeric_validation.csv",
  
  row.names = FALSE
)


# ============================================================
# 22. WORD TABLE FUNCTION
# ============================================================

make_ft <- function(
    x,
    den
) {
  
  display <- x %>%
    
    select(
      -ROW_TYPE
    )
  
  
  ft <- flextable(
    display
  )
  
  
  ft <- set_header_labels(
    
    ft,
    
    Characteristic =
      "",
    
    `Treatment A` =
      paste0(
        "Trt A\n(N=",
        den$A,
        ")"
      ),
    
    `Placebo` =
      paste0(
        "Trt B\n(N=",
        den$B,
        ")"
      ),
    
    Total =
      paste0(
        "Total\n(N=",
        den$TOTAL,
        ")"
      )
  )
  
  
  ft <- add_header_row(
    
    ft,
    
    values = c(
      "",
      "Number of Subjects (%)"
    ),
    
    colwidths = c(
      1,
      3
    )
  )
  
  
  ft <- theme_booktabs(
    ft
  )
  
  
  ft <- font(
    ft,
    fontname = "Courier New",
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
  
  
  ft <- valign(
    ft,
    valign = "center",
    part = "all"
  )
  
  
  ft <- width(
    ft,
    j = 1,
    width = 3.4
  )
  
  
  ft <- width(
    ft,
    j = 2:4,
    width = 1.8
  )
  
  
  ft <- padding(
    ft,
    padding.top = 2,
    padding.bottom = 2,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  )
  
  
  orr_rows <- which(
    x$ROW_TYPE ==
      "ORR"
  )
  
  
  if (
    length(
      orr_rows
    ) > 0
  ) {
    
    ft <- bold(
      ft,
      i = orr_rows,
      part = "body"
    )
  }
  
  
  ft <- set_table_properties(
    
    ft,
    
    layout =
      "fixed",
    
    opts_word =
      list(
        repeat_headers =
          TRUE
      )
  )
  
  
  ft
}


# ============================================================
# 23. CREATE WORD DOCUMENT
# ============================================================

doc <- read_docx()


# ============================================================
# 24. TITLE
# ============================================================

doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Best Overall Response by Age Category",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align =
          "center"
      )
  )
)


doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Best Overall Response per Investigator by Age Category",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align =
          "center"
      )
  )
)


doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "ITT Subjects",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align =
          "center"
      )
  )
)


doc <- body_add_par(
  doc,
  ""
)


# ============================================================
# 25. <65 SECTION
# ============================================================

doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Subgroup: Age Category = < 65",
      
      fp_text(
        font.size = 8,
        bold = TRUE
      )
    )
  )
)


doc <- body_add_flextable(
  
  doc,
  
  make_ft(
    table_lt65,
    den_lt65
  )
)


doc <- body_add_par(
  doc,
  ""
)


# ============================================================
# 26. >=65 SECTION
# ============================================================

doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Subgroup: Age Category = >= 65",
      
      fp_text(
        font.size = 8,
        bold = TRUE
      )
    )
  )
)


doc <- body_add_flextable(
  
  doc,
  
  make_ft(
    table_ge65,
    den_ge65
  )
)


# ============================================================
# 27. FOOTNOTES
# ============================================================

doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "(1) Objective Response Rate = Complete Response + ",
    "Partial Response."
  )
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "95% confidence intervals for Objective Response Rate ",
    "are exact Clopper-Pearson binomial confidence intervals."
  )
)


doc <- body_add_par(
  
  doc,
  
  "Trt A = Treatment A; Trt B = Placebo."
)


# ============================================================
# 28. SAVE WORD OUTPUT
# ============================================================

output_file <-
  "local_outputs/best_overall_response_by_age.docx"


print(
  
  doc,
  
  target =
    output_file
)


# ============================================================
# 29. VALIDATION NOTES
# ============================================================

validation_notes <- c(
  
  "BEST OVERALL RESPONSE TABLE",
  
  "Best Overall Response per Investigator by Age Category",
  
  "",
  
  "Population: ITT Subjects",
  
  "Source dataset: ADRS.xls",
  
  "PARAMCD = BOR",
  
  "",
  
  "ADRS contains 100 unique subjects.",
  
  "",
  
  "Age subgroup variable: AGegr1",
  
  
  
  
  
  "",
  
  "Treatment mapping:",
  
  "Trt A = Treatment A",
  
  "Trt B = Placebo",
  
  "",
  
  "BOR categories:",
  
  "CR = Complete Response",
  
  "PR = Partial Response",
  
  "SD = Stable Disease",
  
  "PD = Progressive Disease",
  
  "NE = Unable to Determine",
  
  "",
  
  "Unable-to-Determine reasons:",
  
  "Death Before Measurement",
  
  "Droped Study",
  
  "Withdrown Consent",
  
  "",
  
  "ORR = CR + PR.",
  
  paste0(
    "95% CI for ORR calculated using exact ",
    "Clopper-Pearson binomial confidence intervals."
  ),
  
  "",
  
  paste0(
    "All response categories sum to treatment subgroup N = ",
    all(
      validation_numeric$MATCH
    )
  )
)


writeLines(
  
  validation_notes,
  
  "local_outputs/bor_validation_notes.txt"
)


# ============================================================
# 30. LOG
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO\n"
  )
  
  cat(
    "BEST OVERALL RESPONSE TABLE\n"
  )
  
  cat(
    "BEST OVERALL RESPONSE BY AGE CATEGORY\n\n"
  )
  
  
  cat(
    "Run date/time:",
    as.character(
      Sys.time()
    ),
    "\n\n"
  )
  
  
  cat(
    "Analysis subjects:",
    n_distinct(
      bor$USUBJID
    ),
    "\n"
  )
  
  
  cat(
    "\nAge Groups:\n"
  )
  
  
  print(
    table(
      bor$AGE_GROUP
    )
  )
  
  
  cat(
    "\nTreatment / Age Denominators:\n"
  )
  
  
  print(
    
    bor %>%
      
      count(
        AGE_GROUP,
        TRT_DISPLAY
      )
  )
  
  
  cat(
    "\nBOR Distribution:\n"
  )
  
  
  print(
    table(
      bor$RESPONSE
    )
  )
  
  
  cat(
    "\nNE Reasons:\n"
  )
  
  
  print(
    table(
      bor$NE_REASON,
      useNA = "ifany"
    )
  )
  
  
  cat(
    "\nNumeric Validation:\n"
  )
  
  
  print(
    validation_numeric,
    n = Inf
  )
  
  
  cat(
    "\nAll response categories sum to N:\n"
  )
  
  
  print(
    all(
      validation_numeric$MATCH
    )
  )
  
  
  cat(
    "\nPROGRAM COMPLETED SUCCESSFULLY\n"
  )
})


writeLines(
  
  log_lines,
  
  "local_outputs/bor_run.log"
)


# ============================================================
# 31. FINAL FILE CHECK
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
  "<65 Validation CSV: ",
  file.exists(
    "local_outputs/bor_age_lt65_calculated_values.csv"
  ),
  "\n"
)


cat(
  ">=65 Validation CSV: ",
  file.exists(
    "local_outputs/bor_age_ge65_calculated_values.csv"
  ),
  "\n"
)


cat(
  "Numeric Validation CSV: ",
  file.exists(
    "local_outputs/bor_numeric_validation.csv"
  ),
  "\n"
)


cat(
  "Validation Notes: ",
  file.exists(
    "local_outputs/bor_validation_notes.txt"
  ),
  "\n"
)


cat(
  "Log: ",
  file.exists(
    "local_outputs/bor_run.log"
  ),
  "\n"
)


cat(
  "\nBEST OVERALL RESPONSE PROGRAM COMPLETED.\n"
)
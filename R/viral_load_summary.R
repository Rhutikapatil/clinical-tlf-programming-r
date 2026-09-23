# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# VIRAL LOAD SUMMARY TABLES
#
# TABLE 11:
# Summary of Viral Load over time
#
# TABLE 12:
# Summary of Adjusted Mean of Viral Load
# at Week 12 and Week 16
#
# Population: ITT
# Source: ADEFF.xls
# PARAMCD = VLOAD
# Analysis Value = AVAL
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names and fixed expected sample sizes are removed.
# - Longitudinal summaries, adjusted-means modeling, formatting, and QC logic are preserved.
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "tibble",
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
# 3. READ ADEFF
# Data are intentionally excluded from the public repository.
# Supply an ADEFF-compatible workbook locally at data/ADEFF.xlsx.
# ============================================================

adeff_raw <- read_excel(
  "data/ADEFF.xlsx",
  sheet = "ADEFF"
)


# ============================================================
# 4. STANDARDIZE ANALYSIS DATA
#
# Excel contains two columns originally both called AVISITN:
#
# AVISITN...5 = numeric visit/week
# AVISITN...6 = visit label
# ============================================================

adeff <- adeff_raw %>%
  
  transmute(
    
    USUBJID =
      USUBJID,
    
    PARAM =
      Param,
    
    PARAMCD =
      toupper(
        trimws(
          PARAMCD
        )
      ),
    
    STAGE =
      Stage,
    
    WEEK =
      as.integer(
        AVISITN...5
      ),
    
    VISIT =
      trimws(
        AVISITN...6
      ),
    
    ANL01FL =
      toupper(
        trimws(
          ANL01FL
        )
      ),
    
    AVAL =
      AVAL,
    
    TRT =
      trimws(
        Trt01p
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
    PARAMCD == "VLOAD",
    ANL01FL == "Y",
    !is.na(
      AVAL
    )
  ) %>%
  
  mutate(
    
    TRT = factor(
      TRT,
      levels = c(
        "Treatment A",
        "Treatment B"
      )
    ),
    
    STAGE = factor(
      STAGE,
      levels = c(
        1,
        2,
        3,
        4
      )
    )
  )


# ============================================================
# 5. INTERNAL QA
# ============================================================

duplicate_check <- adeff %>%
  
  count(
    USUBJID,
    WEEK
  ) %>%
  
  filter(
    n > 1
  )


qa_checks <- c(
  
  "500 analysis records" =
    nrow(
      adeff
    ) > 0,
  
  "100 unique subjects" =
    n_distinct(
      adeff$USUBJID
    ) > 0,
  
  "One record per subject per week" =
    nrow(
      duplicate_check
    ) == 0,
  
  "Expected analysis visits are present" =
    identical(
      sort(
        unique(
          adeff$WEEK
        )
      ),
      c(
        1L,
        4L,
        8L,
        12L,
        16L
      )
    ),
  
  "Only expected treatment groups" =
    all(
      unique(
        as.character(
          adeff$TRT
        )
      ) %in%
        c(
          "Treatment A",
          "Treatment B"
        )
    ),
  
  "All AVAL values nonmissing" =
    all(
      !is.na(
        adeff$AVAL
      )
    )
)


cat(
  "\n============================================\n"
)

cat(
  "TABLE 11/12 INTERNAL QA\n"
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
    "Table 11/12 internal QA failed."
  )
}


cat(
  "\nALL INTERNAL QA CHECKS PASSED\n"
)


# ============================================================
# 6. WEEK / TREATMENT COUNTS
# ============================================================

week_trt_counts <- adeff %>%
  
  count(
    WEEK,
    TRT,
    name = "N"
  ) %>%
  
  arrange(
    WEEK,
    TRT
  )


cat(
  "\n============================================\n"
)

cat(
  "WEEK / TREATMENT COUNTS\n"
)

cat(
  "============================================\n\n"
)


print(
  week_trt_counts,
  n = Inf
)


# ============================================================
# 7. STAGE DISTRIBUTION
# ============================================================

stage_distribution <- adeff %>%
  
  count(
    WEEK,
    TRT,
    STAGE,
    name = "N"
  ) %>%
  
  arrange(
    WEEK,
    TRT,
    STAGE
  )


cat(
  "\n============================================\n"
)

cat(
  "STAGE DISTRIBUTION\n"
)

cat(
  "============================================\n\n"
)


print(
  stage_distribution,
  n = Inf
)


# ============================================================
# TABLE 11
# SUMMARY OF VIRAL LOAD OVER TIME
# ============================================================


# ============================================================
# 8. TABLE 11 NUMERIC CALCULATIONS
#
# N    = number of nonmissing AVAL values
# Mean = arithmetic mean
# SE   = sample SD / sqrt(N)
# ============================================================

table11_numeric <- adeff %>%
  
  filter(
    WEEK %in%
      c(
        1,
        4,
        8,
        12,
        16
      )
  ) %>%
  
  group_by(
    WEEK,
    TRT
  ) %>%
  
  summarise(
    
    N =
      n(),
    
    MEAN =
      mean(
        AVAL
      ),
    
    SD =
      sd(
        AVAL
      ),
    
    SE =
      SD /
      sqrt(
        N
      ),
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    WEEK,
    TRT
  )


# ============================================================
# 9. TABLE 11 DISPLAY DATA
#
# Display Week only on the first treatment row for compact presentation.
# ============================================================

table11_display <- table11_numeric %>%
  
  group_by(
    WEEK
  ) %>%
  
  mutate(
    
    `Test Day` =
      if_else(
        row_number() == 1,
        paste(
          "Week",
          WEEK
        ),
        ""
      )
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    Treatment =
      as.character(
        TRT
      ),
    
    `Unadjusted Mean (SE)` =
      sprintf(
        "%.3f (%.3f)",
        MEAN,
        SE
      )
  ) %>%
  
  select(
    `Test Day`,
    Treatment,
    N,
    `Unadjusted Mean (SE)`
  )


cat(
  "\n============================================\n"
)

cat(
  "TABLE 11 CALCULATED OUTPUT\n"
)

cat(
  "============================================\n\n"
)


print(
  table11_display,
  n = Inf,
  width = Inf
)


# ============================================================
# TABLE 12
# ADJUSTED MEAN OF VIRAL LOAD
# WEEK 12 AND WEEK 16
# ============================================================


# ============================================================
# 10. FUNCTION TO CALCULATE ADJUSTED MEANS
#
# Fixed-effects model with STAGE and TREATMENT as factors:
#
# AVAL ~ STAGE + TRT
#
# Model is fitted separately at each requested week.
#
# Treatment LSMean is calculated using equal weighting
# across Stage 1, 2, 3, and 4.
# ============================================================

get_adjusted_means <- function(
    input_data,
    week_value
) {
  
  dat <- input_data %>%
    
    filter(
      WEEK ==
        week_value
    ) %>%
    
    mutate(
      
      STAGE = factor(
        STAGE,
        levels = c(
          1,
          2,
          3,
          4
        )
      ),
      
      TRT = factor(
        TRT,
        levels = c(
          "Treatment A",
          "Treatment B"
        )
      )
    )
  
  
  # ----------------------------------------------------------
  # QA
  # ----------------------------------------------------------
  
  observed_stages <- unique(
    as.character(
      dat$STAGE
    )
  )
  
  
  if (
    !all(
      c(
        "1",
        "2",
        "3",
        "4"
      ) %in%
      observed_stages
    )
  ) {
    
    stop(
      paste(
        "Not all Stage levels are present at Week",
        week_value
      )
    )
  }
  
  
  if (
    n_distinct(
      dat$TRT
    ) != 2
  ) {
    
    stop(
      paste(
        "Both treatments are not present at Week",
        week_value
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Fit fixed-effects model
  # ----------------------------------------------------------
  
  fit <- lm(
    AVAL ~ STAGE + TRT,
    data = dat
  )
  
  
  beta <- coef(
    fit
  )
  
  
  vc <- vcov(
    fit
  )
  
  
  # ----------------------------------------------------------
  # Build all Stage x Treatment combinations
  # ----------------------------------------------------------
  
  newdata <- expand.grid(
    
    STAGE = factor(
      c(
        1,
        2,
        3,
        4
      ),
      levels = c(
        1,
        2,
        3,
        4
      )
    ),
    
    TRT = factor(
      c(
        "Treatment A",
        "Treatment B"
      ),
      levels = c(
        "Treatment A",
        "Treatment B"
      )
    )
  )
  
  
  X <- model.matrix(
    ~ STAGE + TRT,
    data = newdata
  )
  
  
  results <- lapply(
    
    levels(
      dat$TRT
    ),
    
    function(
    trt_value
    ) {
      
      idx <-
        newdata$TRT ==
        trt_value
      
      
      # Equal weighting across four Stage levels
      L <- colMeans(
        X[
          idx,
          ,
          drop = FALSE
        ]
      )
      
      
      adjusted_mean <-
        as.numeric(
          L %*%
            beta
        )
      
      
      adjusted_variance <-
        as.numeric(
          t(
            L
          ) %*%
            vc %*%
            L
        )
      
      
      adjusted_se <-
        sqrt(
          adjusted_variance
        )
      
      
      group_n <-
        dat %>%
        
        filter(
          TRT ==
            trt_value
        ) %>%
        
        summarise(
          N =
            n()
        ) %>%
        
        pull(
          N
        )
      
      
      tibble(
        
        WEEK =
          week_value,
        
        TRT =
          trt_value,
        
        N =
          group_n,
        
        ADJ_MEAN =
          adjusted_mean,
        
        ADJ_SE =
          adjusted_se
      )
    }
  ) %>%
    
    bind_rows()
  
  
  list(
    
    results =
      results,
    
    model =
      fit
  )
}


# ============================================================
# 11. FIT WEEK 12 MODEL
# ============================================================

week12_model <- get_adjusted_means(
  adeff,
  12
)


# ============================================================
# 12. FIT WEEK 16 MODEL
# ============================================================

week16_model <- get_adjusted_means(
  adeff,
  16
)


# ============================================================
# 13. COMBINE TABLE 12 RESULTS
# ============================================================

table12_numeric <- bind_rows(
  
  week12_model$results,
  
  week16_model$results
  
) %>%
  
  arrange(
    WEEK,
    TRT
  )


# ============================================================
# 14. TABLE 12 DISPLAY DATA
# ============================================================

table12_display <- table12_numeric %>%
  
  group_by(
    WEEK
  ) %>%
  
  mutate(
    
    `Test Day` =
      if_else(
        row_number() == 1,
        paste(
          "Week",
          WEEK
        ),
        ""
      )
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    Treatment =
      as.character(
        TRT
      ),
    
    `Adjusted Mean (SE)` =
      sprintf(
        "%.3f (%.3f)",
        ADJ_MEAN,
        ADJ_SE
      )
  ) %>%
  
  select(
    `Test Day`,
    Treatment,
    N,
    `Adjusted Mean (SE)`
  )


cat(
  "\n============================================\n"
)

cat(
  "TABLE 12 CALCULATED OUTPUT\n"
)

cat(
  "============================================\n\n"
)


print(
  table12_display,
  n = Inf,
  width = Inf
)


# ============================================================
# 15. WEEK 12 MODEL SUMMARY
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "WEEK 12 MODEL SUMMARY\n"
)

cat(
  "============================================\n\n"
)


print(
  summary(
    week12_model$model
  )
)


# ============================================================
# 16. WEEK 16 MODEL SUMMARY
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "WEEK 16 MODEL SUMMARY\n"
)

cat(
  "============================================\n\n"
)


print(
  summary(
    week16_model$model
  )
)


# ============================================================
# 17. TABLE 11 QA
# ============================================================

table11_qa <- c(
  
  "Table 11 has one row per visit-treatment combination" =
    nrow(
      table11_numeric
    ) ==
      nrow(
        distinct(
          adeff,
          WEEK,
          TRT
        ) %>%
          filter(
            WEEK %in% c(1, 4, 8, 12, 16)
          )
      ),
  
  "All Table 11 N positive" =
    all(
      table11_numeric$N > 0
    ),
  
  "All Table 11 means finite" =
    all(
      is.finite(
        table11_numeric$MEAN
      )
    ),
  
  "All Table 11 SE finite" =
    all(
      is.finite(
        table11_numeric$SE
      )
    )
)


# ============================================================
# 18. TABLE 12 QA
# ============================================================

table12_qa <- c(
  
  "Table 12 has one row per requested visit-treatment combination" =
    nrow(
      table12_numeric
    ) ==
      nrow(
        distinct(
          adeff,
          WEEK,
          TRT
        ) %>%
          filter(
            WEEK %in% c(12, 16)
          )
      ),
  
  "All Table 12 N positive" =
    all(
      table12_numeric$N > 0
    ),
  
  "All adjusted means finite" =
    all(
      is.finite(
        table12_numeric$ADJ_MEAN
      )
    ),
  
  "All adjusted SE finite" =
    all(
      is.finite(
        table12_numeric$ADJ_SE
      )
    )
)


cat(
  "\n============================================\n"
)

cat(
  "TABLE 11/12 FINAL QA\n"
)

cat(
  "============================================\n\n"
)


cat(
  "TABLE 11 QA:\n"
)

print(
  table11_qa
)


cat(
  "\nTABLE 12 QA:\n"
)

print(
  table12_qa
)


if (
  !all(
    table11_qa
  )
) {
  
  stop(
    "Table 11 QA failed."
  )
}


if (
  !all(
    table12_qa
  )
) {
  
  stop(
    "Table 12 QA failed."
  )
}


cat(
  "\nALL TABLE 11/12 QA CHECKS PASSED\n"
)


# ============================================================
# 19. SAVE TABLE 11 VALIDATION CSV
# ============================================================

write.csv(
  
  table11_numeric,
  
  "local_outputs/viral_load_unadjusted_numeric.csv",
  
  row.names = FALSE
)


write.csv(
  
  table11_display,
  
  "local_outputs/Table_11_calculated_values.csv",
  
  row.names = FALSE
)


# ============================================================
# 20. SAVE TABLE 12 VALIDATION CSV
# ============================================================

write.csv(
  
  table12_numeric,
  
  "local_outputs/viral_load_adjusted_numeric.csv",
  
  row.names = FALSE
)


write.csv(
  
  table12_display,
  
  "local_outputs/Table_12_calculated_values.csv",
  
  row.names = FALSE
)


write.csv(
  
  stage_distribution,
  
  "local_outputs/Table_12_stage_distribution.csv",
  
  row.names = FALSE
)


# ============================================================
# 21. WORD TABLE FUNCTION
# ============================================================

make_simple_ft <- function(
    x
) {
  
  ft <- flextable(
    x
  )
  
  
  ft <- theme_booktabs(
    ft
  )
  
  
  ft <- font(
    ft,
    fontname = "Times New Roman",
    part = "all"
  )
  
  
  ft <- fontsize(
    ft,
    size = 10,
    part = "all"
  )
  
  
  ft <- bold(
    ft,
    part = "header"
  )
  
  
  ft <- align(
    ft,
    j = 1:2,
    align = "left",
    part = "all"
  )
  
  
  ft <- align(
    ft,
    j = 3:4,
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
    width = 1.6
  )
  
  
  ft <- width(
    ft,
    j = 2,
    width = 1.8
  )
  
  
  ft <- width(
    ft,
    j = 3,
    width = 1
  )
  
  
  ft <- width(
    ft,
    j = 4,
    width = 2.4
  )
  
  
  ft <- padding(
    
    ft,
    
    padding.top =
      4,
    
    padding.bottom =
      4,
    
    padding.left =
      3,
    
    padding.right =
      3,
    
    part =
      "all"
  )
  
  
  ft <- set_table_properties(
    
    ft,
    
    layout =
      "fixed"
  )
  
  
  ft
}


# ============================================================
# 22. CREATE TABLE 11 WORD DOCUMENT
# ============================================================

doc11 <- read_docx()


doc11 <- body_add_fpar(
  
  doc11,
  
  fpar(
    
    ftext(
      
      "Table 11:",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc11 <- body_add_par(
  doc11,
  ""
)


doc11 <- body_add_fpar(
  
  doc11,
  
  fpar(
    
    ftext(
      
      "Summary of Viral Load over time",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc11 <- body_add_par(
  doc11,
  ""
)


doc11 <- body_add_fpar(
  
  doc11,
  
  fpar(
    
    ftext(
      
      "ITT Population",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc11 <- body_add_par(
  doc11,
  ""
)


doc11 <- body_add_flextable(
  
  doc11,
  
  make_simple_ft(
    table11_display
  )
)


doc11 <- body_add_par(
  doc11,
  ""
)


doc11 <- body_add_par(
  
  doc11,
  
  "Calculated using PROC MEANS-equivalent summary statistics."
)


table11_file <-
  "local_outputs/viral_load_unadjusted.docx"


print(
  
  doc11,
  
  target =
    table11_file
)


# ============================================================
# 23. CREATE TABLE 12 WORD DOCUMENT
# ============================================================

doc12 <- read_docx()


doc12 <- body_add_fpar(
  
  doc12,
  
  fpar(
    
    ftext(
      
      "Table 12:",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc12 <- body_add_par(
  doc12,
  ""
)


doc12 <- body_add_fpar(
  
  doc12,
  
  fpar(
    
    ftext(
      
      "Summary of Adjusted Mean of Viral Load at Week 12 and 16",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc12 <- body_add_par(
  doc12,
  ""
)


doc12 <- body_add_fpar(
  
  doc12,
  
  fpar(
    
    ftext(
      
      "ITT Population",
      
      fp_text(
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc12 <- body_add_par(
  doc12,
  ""
)


doc12 <- body_add_flextable(
  
  doc12,
  
  make_simple_ft(
    table12_display
  )
)


doc12 <- body_add_par(
  doc12,
  ""
)


doc12 <- body_add_par(
  
  doc12,
  
  paste0(
    "Adjusted mean is calculated using a model with ",
    "STAGE and TREATMENT as factors."
  )
)


doc12 <- body_add_par(
  doc12,
  ""
)


doc12 <- body_add_par(
  
  doc12,
  
  "*Programming Note: Use AVAL"
)


table12_file <-
  "local_outputs/t_12_adjusted_viral_load_FINAL.docx"


print(
  
  doc12,
  
  target =
    table12_file
)


# ============================================================
# 24. TABLE 11 VALIDATION NOTES
# ============================================================

table11_notes <- c(
  
  "TABLE 11",
  
  "Summary of Viral Load over time",
  
  "",
  
  "Population: ITT",
  
  "Source dataset: ADEFF.xls",
  
  "PARAMCD = VLOAD",
  
  "Analysis variable = AVAL",
  
  "",
  
  "Visits:",
  
  "Week 1",
  
  "Week 4",
  
  "Week 8",
  
  "Week 12",
  
  "Week 16",
  
  "",
  
  "Statistics:",
  
  "N = number of nonmissing AVAL observations.",
  
  "Unadjusted Mean = arithmetic mean of AVAL.",
  
  "SE = sample standard deviation divided by square root of N."
)


writeLines(
  
  table11_notes,
  
  "local_outputs/viral_load_unadjusted_validation_notes.txt"
)


# ============================================================
# 25. TABLE 12 VALIDATION NOTES
# ============================================================

table12_notes <- c(
  
  "TABLE 12",
  
  "Summary of Adjusted Mean of Viral Load at Week 12 and 16",
  
  "",
  
  "Population: ITT",
  
  "Source dataset: ADEFF.xls",
  
  "PARAMCD = VLOAD",
  
  "Analysis variable = AVAL",
  
  "",
  
  "Weeks analyzed:",
  
  "Week 12",
  
  "Week 16",
  
  "",
  
  "Model fitted separately at each week:",
  
  "AVAL = STAGE + TREATMENT",
  
  "",
  
  "STAGE is categorical.",
  
  "TREATMENT is categorical.",
  
  paste0(
    "Adjusted treatment means are least-squares means ",
    "calculated using equal weighting across Stage levels 1, 2, 3, and 4."
  )
)


writeLines(
  
  table12_notes,
  
  "local_outputs/viral_load_adjusted_validation_notes.txt"
)


# ============================================================
# 26. LOG FILE
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO\n"
  )
  
  cat(
    "VIRAL LOAD SUMMARY TABLES\n"
  )
  
  cat(
    "VIRAL LOAD ANALYSIS\n\n"
  )
  
  
  cat(
    "Run date/time:",
    as.character(
      Sys.time()
    ),
    "\n\n"
  )
  
  
  cat(
    "Analysis records:",
    nrow(
      adeff
    ),
    "\n"
  )
  
  
  cat(
    "Unique subjects:",
    n_distinct(
      adeff$USUBJID
    ),
    "\n\n"
  )
  
  
  cat(
    "TABLE 11:\n"
  )
  
  
  print(
    table11_display,
    n = Inf
  )
  
  
  cat(
    "\nTABLE 12:\n"
  )
  
  
  print(
    table12_display,
    n = Inf
  )
  
  
  cat(
    "\nTABLE 11 QA:\n"
  )
  
  
  print(
    table11_qa
  )
  
  
  cat(
    "\nTABLE 12 QA:\n"
  )
  
  
  print(
    table12_qa
  )
  
  
  cat(
    "\nWEEK 12 MODEL:\n"
  )
  
  
  print(
    summary(
      week12_model$model
    )
  )
  
  
  cat(
    "\nWEEK 16 MODEL:\n"
  )
  
  
  print(
    summary(
      week16_model$model
    )
  )
  
  
  cat(
    "\nPROGRAM COMPLETED SUCCESSFULLY\n"
  )
})


writeLines(
  
  log_lines,
  
  "local_outputs/viral_load_run.log"
)


# ============================================================
# 27. FINAL FILE CHECK
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
  "Table 11 Word Output: ",
  file.exists(
    table11_file
  ),
  "\n"
)


cat(
  "Table 12 Word Output: ",
  file.exists(
    table12_file
  ),
  "\n"
)


cat(
  "Table 11 Numeric CSV: ",
  file.exists(
    "local_outputs/viral_load_unadjusted_numeric.csv"
  ),
  "\n"
)


cat(
  "Table 11 Display CSV: ",
  file.exists(
    "local_outputs/Table_11_calculated_values.csv"
  ),
  "\n"
)


cat(
  "Table 12 Numeric CSV: ",
  file.exists(
    "local_outputs/viral_load_adjusted_numeric.csv"
  ),
  "\n"
)


cat(
  "Table 12 Display CSV: ",
  file.exists(
    "local_outputs/Table_12_calculated_values.csv"
  ),
  "\n"
)


cat(
  "Stage Distribution CSV: ",
  file.exists(
    "local_outputs/Table_12_stage_distribution.csv"
  ),
  "\n"
)


cat(
  "Table 11 Validation Notes: ",
  file.exists(
    "local_outputs/viral_load_unadjusted_validation_notes.txt"
  ),
  "\n"
)


cat(
  "Table 12 Validation Notes: ",
  file.exists(
    "local_outputs/viral_load_adjusted_validation_notes.txt"
  ),
  "\n"
)


cat(
  "Log: ",
  file.exists(
    "local_outputs/viral_load_run.log"
  ),
  "\n"
)


cat(
  "\nVIRAL LOAD SUMMARY TABLES PROGRAM COMPLETED.\n"
)
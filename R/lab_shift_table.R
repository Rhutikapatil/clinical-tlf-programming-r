# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# LABORATORY SHIFT TABLE
#
# Change in Laboratory Events:
# Shift in NCI-CTC Grade from Baseline to Worst Post-Baseline Level
# Safety Evaluable Patients
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names and fixed expected sample sizes are removed.
# - Baseline, worst post-baseline, shift-table, formatting, and QC logic are preserved.
#
# PUBLIC PORTFOLIO VERSION
#
# IMPORTANT SPECIAL RULE:
# Sodium / Potassium / Magnesium:
#
#   Baseline rows:
#       Low   Grade 4
#             Grade 3
#             Grade 2
#             Grade 1
#             Grade 0
#       High  Grade 1-4 pooled
#
#   Post-baseline:
#       Columns 0-4 = LOW toxicity grades
#       Other (>ULN) = HIGH Grades 1-4 pooled
#
# Other selected labs:
#       Baseline / post-baseline Grade 0-4 = HIGH toxicity
#       Other column = blank
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "haven",
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


library(haven)
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
# 3. READ ADLBSI
# Data are intentionally excluded from the public repository.
# Supply an ADLBSI-compatible XPT file locally at data/ADLBSI.xpt.
# ============================================================

adlbsi <- read_xpt(
  "data/ADLBSI.xpt"
)


# ============================================================
# 4. LABORATORY SPECIFICATION
# ============================================================

lab_specs <- tribble(
  
  ~ORDER,
  ~PARAMCD,
  ~LAB_PARAMETER,
  ~DEFAULT_EVENT,
  ~SPECIAL_LOW,
  
  1,
  "CSODIUMS",
  "Sodium (mmol/L)",
  "Low",
  TRUE,
  
  2,
  "CKS",
  "Potassium (mmol/L)",
  "Low",
  TRUE,
  
  3,
  "CMGS",
  "Magnesium (mmol/L)",
  "Low",
  TRUE,
  
  4,
  "CALPS",
  "Alkaline phosphatase (U/L)",
  "High",
  FALSE,
  
  5,
  "CASTS",
  "SGOT/AST (U/L)",
  "High",
  FALSE,
  
  6,
  "CALTS",
  "SGPT/ALT (U/L)",
  "High",
  FALSE,
  
  7,
  "CBILIS",
  "Total Bilirubin (umol/L)",
  "High",
  FALSE,
  
  8,
  "CBUNS",
  "Blood Urea Nitrogen (BUN) (mmol/L)",
  "High",
  FALSE,
  
  9,
  "CCREATS",
  "Creatinine (umol/L)",
  "High",
  FALSE,
  
  10,
  "UPROTN",
  "Urine Protein",
  "High",
  FALSE
)


# ============================================================
# 5. CHECK REQUIRED PARAMETERS
# ============================================================

missing_params <- setdiff(
  lab_specs$PARAMCD,
  unique(
    adlbsi$PARAMCD
  )
)


if (length(missing_params) > 0) {
  
  stop(
    paste(
      "Missing required PARAMCD:",
      paste(
        missing_params,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 6. SAFETY POPULATION
# ============================================================

safety_subjects <- adlbsi %>%
  
  filter(
    SAFFL == "Y"
  ) %>%
  
  distinct(
    USUBJID,
    TRT01A
  )


n_treatment_a <- safety_subjects %>%
  
  filter(
    TRT01A == "Treatment A"
  ) %>%
  
  summarise(
    N = n_distinct(
      USUBJID
    )
  ) %>%
  
  pull(
    N
  )


n_placebo <- safety_subjects %>%
  
  filter(
    TRT01A == "Placebo"
  ) %>%
  
  summarise(
    N = n_distinct(
      USUBJID
    )
  ) %>%
  
  pull(
    N
  )


cat(
  "\n============================================\n"
)

cat(
  "LABORATORY SHIFT TABLE SAFETY POPULATION\n"
)

cat(
  "============================================\n\n"
)


cat(
  "Treatment A N =",
  n_treatment_a,
  "\n"
)

cat(
  "Placebo N =",
  n_placebo,
  "\n"
)


stopifnot(
  n_treatment_a + n_placebo ==
    n_distinct(safety_subjects$USUBJID),
  all(
    unique(safety_subjects$TRT01A) %in%
      c("Treatment A", "Placebo")
  )
)


# ============================================================
# 7. SELECT REQUIRED LABS
# ============================================================

labs <- adlbsi %>%
  
  filter(
    SAFFL == "Y",
    PARAMCD %in%
      lab_specs$PARAMCD
  ) %>%
  
  left_join(
    lab_specs,
    by = "PARAMCD"
  ) %>%
  
  mutate(
    
    ATOXGR_NUM =
      suppressWarnings(
        as.integer(
          trimws(
            ATOXGR
          )
        )
      ),
    
    BTOXGR_NUM =
      suppressWarnings(
        as.integer(
          trimws(
            BTOXGR
          )
        )
      ),
    
    ATOXDIR =
      toupper(
        trimws(
          ATOXDIR
        )
      ),
    
    BTOXDIR =
      toupper(
        trimws(
          BTOXDIR
        )
      )
  )


# ============================================================
# 8. BASELINE RECORDS
#
# ABLFL was already validated during ADLBSI creation.
#
# Specification:
# - Last eligible nonmissing value prior to treatment.
# - Treatment-date result may be used when needed.
# ============================================================

baseline <- labs %>%
  
  filter(
    ABLFL == "Y"
  ) %>%
  
  arrange(
    USUBJID,
    PARAMCD,
    ADT,
    SRCSEQ
  ) %>%
  
  group_by(
    USUBJID,
    PARAMCD
  ) %>%
  
  slice_tail(
    n = 1
  ) %>%
  
  ungroup() %>%
  
  transmute(
    
    USUBJID,
    
    TRT01A,
    
    PARAMCD,
    
    ORDER,
    
    LAB_PARAMETER,
    
    DEFAULT_EVENT,
    
    SPECIAL_LOW,
    
    BASE_DATE =
      ADT,
    
    BASE_GRADE_NUM =
      ATOXGR_NUM,
    
    BASE_DIRECTION =
      ATOXDIR
  )


# ============================================================
# 9. DERIVE BASELINE EVENT + BASELINE GRADE
#
# SPECIAL TESTS:
#
# Sodium / Potassium / Magnesium
#
# LOW baseline:
#   L Grade 1-4 -> corresponding grade
#   Normal / no low toxicity -> Grade 0
#
# HIGH baseline:
#   H Grade 1-4 -> pooled baseline row "1-4"
#
# OTHER LABS:
#
# HIGH Grade 1-4 -> corresponding grade
# Otherwise -> Grade 0
# ============================================================

baseline <- baseline %>%
  
  mutate(
    
    BASE_EVENT = case_when(
      
      SPECIAL_LOW &
        BASE_DIRECTION == "H" &
        BASE_GRADE_NUM %in%
        1:4 ~
        "High",
      
      SPECIAL_LOW ~
        "Low",
      
      TRUE ~
        "High"
    ),
    
    
    BASE_GRADE_LABEL = case_when(
      
      # Special tests - High pooled baseline row
      SPECIAL_LOW &
        BASE_EVENT == "High" ~
        "1-4",
      
      # Special tests - Low Grades 1-4
      SPECIAL_LOW &
        BASE_EVENT == "Low" &
        BASE_DIRECTION == "L" &
        BASE_GRADE_NUM %in%
        1:4 ~
        as.character(
          BASE_GRADE_NUM
        ),
      
      # Special tests - normal/no low toxicity
      SPECIAL_LOW &
        BASE_EVENT == "Low" ~
        "0",
      
      # Other labs - High Grades 1-4
      !SPECIAL_LOW &
        BASE_DIRECTION == "H" &
        BASE_GRADE_NUM %in%
        1:4 ~
        as.character(
          BASE_GRADE_NUM
        ),
      
      # Other labs - Grade 0
      TRUE ~
        "0"
    )
  )


# ============================================================
# 10. BASELINE HIGH POOLED QA
# ============================================================

special_baseline_high <- baseline %>%
  
  filter(
    SPECIAL_LOW,
    BASE_EVENT == "High"
  ) %>%
  
  count(
    TRT01A,
    ORDER,
    LAB_PARAMETER,
    name = "BASELINE_HIGH_N"
  ) %>%
  
  arrange(
    factor(
      TRT01A,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    ),
    ORDER
  )


cat(
  "\n============================================\n"
)

cat(
  "SPECIAL BASELINE HIGH 1-4 POOLED CHECK\n"
)

cat(
  "============================================\n\n"
)


if (
  nrow(
    special_baseline_high
  ) == 0
) {
  
  cat(
    "No baseline High Grade 1-4 subjects found.\n"
  )
  
} else {
  
  print(
    special_baseline_high,
    n = Inf
  )
}


# ============================================================
# 11. POST-BASELINE RECORDS
#
# Per shell:
# Post-baseline begins Baseline Date + 1 day.
#
# Therefore:
# ADT > BASE_DATE
# ============================================================

post <- labs %>%
  
  inner_join(
    
    baseline %>%
      
      select(
        USUBJID,
        PARAMCD,
        BASE_DATE
      ),
    
    by = c(
      "USUBJID",
      "PARAMCD"
    )
  ) %>%
  
  filter(
    
    ANL01FL == "Y",
    
    !is.na(
      ADT
    ),
    
    !is.na(
      BASE_DATE
    ),
    
    ADT >
      BASE_DATE
  )


# ============================================================
# 12. ELIGIBLE SUBJECTS
#
# N requires:
# - Baseline
# - At least one post-baseline value
# ============================================================

eligible <- post %>%
  
  distinct(
    USUBJID,
    PARAMCD
  )


baseline_eligible <- baseline %>%
  
  inner_join(
    eligible,
    by = c(
      "USUBJID",
      "PARAMCD"
    )
  )


# ============================================================
# 13. WORST POST-BASELINE:
# SODIUM / POTASSIUM / MAGNESIUM
#
# Grade columns 0-4 represent LOW events.
#
# High Grade 1-4 pooled into:
# Other (value > ULN)
#
# Patient counted only once at highest CTCAE grade.
#
# If highest High grade > highest Low grade:
#     OTHER
#
# Otherwise use highest Low grade.
#
# Tie rule has no effect if no ties exist.
# ============================================================

special_post <- post %>%
  
  filter(
    SPECIAL_LOW
  ) %>%
  
  group_by(
    USUBJID,
    TRT01A,
    PARAMCD
  ) %>%
  
  summarise(
    
    WORST_LOW = {
      
      x <- ATOXGR_NUM[
        ATOXDIR == "L" &
          ATOXGR_NUM %in%
          1:4
      ]
      
      if (
        length(x) == 0
      ) {
        
        0L
        
      } else {
        
        max(
          x,
          na.rm = TRUE
        )
      }
    },
    
    
    WORST_HIGH = {
      
      x <- ATOXGR_NUM[
        ATOXDIR == "H" &
          ATOXGR_NUM %in%
          1:4
      ]
      
      if (
        length(x) == 0
      ) {
        
        0L
        
      } else {
        
        max(
          x,
          na.rm = TRUE
        )
      }
    },
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    POST_CATEGORY = case_when(
      
      WORST_HIGH >
        WORST_LOW ~
        "OTHER",
      
      WORST_LOW %in%
        1:4 ~
        as.character(
          WORST_LOW
        ),
      
      TRUE ~
        "0"
    )
  )


# ============================================================
# 14. LOW/HIGH TIE QA
# ============================================================

special_ties <- special_post %>%
  
  filter(
    WORST_LOW > 0,
    WORST_HIGH > 0,
    WORST_LOW ==
      WORST_HIGH
  )


cat(
  "\n============================================\n"
)

cat(
  "SPECIAL LOW/HIGH TIE CHECK\n"
)

cat(
  "============================================\n\n"
)


cat(
  "Subjects with equal worst Low and High grade:",
  nrow(
    special_ties
  ),
  "\n"
)


if (
  nrow(
    special_ties
  ) > 0
) {
  
  print(
    special_ties,
    n = Inf
  )
}


# ============================================================
# 15. WORST POST-BASELINE:
# OTHER SELECTED LABS
#
# Grade 0-4 columns represent HIGH toxicity.
#
# Other column is blank.
# ============================================================

high_post <- post %>%
  
  filter(
    !SPECIAL_LOW
  ) %>%
  
  group_by(
    USUBJID,
    TRT01A,
    PARAMCD
  ) %>%
  
  summarise(
    
    WORST_HIGH = {
      
      x <- ATOXGR_NUM[
        ATOXDIR == "H" &
          ATOXGR_NUM %in%
          1:4
      ]
      
      if (
        length(x) == 0
      ) {
        
        0L
        
      } else {
        
        max(
          x,
          na.rm = TRUE
        )
      }
    },
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    POST_CATEGORY =
      as.character(
        WORST_HIGH
      )
  )


# ============================================================
# 16. COMBINE POST-BASELINE RESULTS
# ============================================================

worst_post <- bind_rows(
  
  special_post %>%
    
    select(
      USUBJID,
      TRT01A,
      PARAMCD,
      POST_CATEGORY
    ),
  
  high_post %>%
    
    select(
      USUBJID,
      TRT01A,
      PARAMCD,
      POST_CATEGORY
    )
)


# ============================================================
# 17. SUBJECT-LEVEL SHIFT DATA
#
# One record per subject / parameter
# ============================================================

shift_subject <- baseline_eligible %>%
  
  select(
    
    USUBJID,
    
    TRT01A,
    
    PARAMCD,
    
    ORDER,
    
    LAB_PARAMETER,
    
    DEFAULT_EVENT,
    
    SPECIAL_LOW,
    
    BASE_EVENT,
    
    BASE_GRADE_LABEL
  ) %>%
  
  left_join(
    
    worst_post,
    
    by = c(
      "USUBJID",
      "TRT01A",
      "PARAMCD"
    )
  )


# ============================================================
# 18. DUPLICATE QA
# ============================================================

duplicate_check <- shift_subject %>%
  
  count(
    USUBJID,
    PARAMCD
  ) %>%
  
  filter(
    n > 1
  )


stopifnot(
  nrow(
    duplicate_check
  ) == 0
)


stopifnot(
  all(
    !is.na(
      shift_subject$POST_CATEGORY
    )
  )
)


# ============================================================
# 19. PERCENTAGE FORMATTING
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
# 20. SHELL ROWS FOR SPECIAL LOW TESTS
#
# Required order:
#
# Low   4
#       3
#       2
#       1
#       0
# High  1-4
# ============================================================

special_rows <- tibble(
  
  ROW_ORDER =
    1:6,
  
  BASE_EVENT = c(
    "Low",
    "Low",
    "Low",
    "Low",
    "Low",
    "High"
  ),
  
  BASE_GRADE_LABEL = c(
    "4",
    "3",
    "2",
    "1",
    "0",
    "1-4"
  ),
  
  LAB_EVENT_DISPLAY = c(
    "Low",
    "",
    "",
    "",
    "",
    "High"
  )
)


# ============================================================
# 21. SHELL ROWS FOR OTHER HIGH TESTS
# ============================================================

high_rows <- tibble(
  
  ROW_ORDER =
    1:5,
  
  BASE_EVENT = rep(
    "High",
    5
  ),
  
  BASE_GRADE_LABEL = c(
    "4",
    "3",
    "2",
    "1",
    "0"
  ),
  
  LAB_EVENT_DISPLAY = c(
    "High",
    "",
    "",
    "",
    ""
  )
)


# ============================================================
# 22. BUILD COMPLETE SHELL
#
# Special tests:
# 3 x 6 rows = 18
#
# Other tests:
# 7 x 5 rows = 35
#
# Total per treatment = 53
# Total two treatments = 106
# ============================================================

special_shell <- tidyr::crossing(
  
  TRT01A = c(
    "Treatment A",
    "Placebo"
  ),
  
  lab_specs %>%
    
    filter(
      SPECIAL_LOW
    ) %>%
    
    select(
      ORDER,
      PARAMCD,
      LAB_PARAMETER,
      SPECIAL_LOW
    ),
  
  special_rows
)


high_shell <- tidyr::crossing(
  
  TRT01A = c(
    "Treatment A",
    "Placebo"
  ),
  
  lab_specs %>%
    
    filter(
      !SPECIAL_LOW
    ) %>%
    
    select(
      ORDER,
      PARAMCD,
      LAB_PARAMETER,
      SPECIAL_LOW
    ),
  
  high_rows
)


shell <- bind_rows(
  special_shell,
  high_shell
) %>%
  
  arrange(
    
    factor(
      TRT01A,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    ),
    
    ORDER,
    
    ROW_ORDER
  )


stopifnot(
  nrow(shell) > 0,
  nrow(shell) ==
    nrow(
      distinct(
        shell,
        TRT01A,
        PARAMCD,
        BASE_EVENT,
        BASE_GRADE_LABEL
      )
    )
)


# ============================================================
# 23. DENOMINATOR N
#
# Denominator defined by:
#
# Treatment
# Parameter
# Baseline Event
# Baseline Grade
# ============================================================

denominators <- shift_subject %>%
  
  count(
    
    TRT01A,
    
    PARAMCD,
    
    BASE_EVENT,
    
    BASE_GRADE_LABEL,
    
    name = "N"
  )


# ============================================================
# 24. POST-BASELINE COUNTS
# ============================================================

post_counts <- shift_subject %>%
  
  count(
    
    TRT01A,
    
    PARAMCD,
    
    BASE_EVENT,
    
    BASE_GRADE_LABEL,
    
    POST_CATEGORY,
    
    name = "COUNT"
  ) %>%
  
  pivot_wider(
    
    names_from =
      POST_CATEGORY,
    
    values_from =
      COUNT,
    
    values_fill =
      0,
    
    names_prefix =
      "POST_"
  )


# ============================================================
# 25. ENSURE ALL REQUIRED POST COLUMNS EXIST
# ============================================================

needed_post_cols <- c(
  "POST_0",
  "POST_1",
  "POST_2",
  "POST_3",
  "POST_4",
  "POST_OTHER"
)


for (
  v in needed_post_cols
) {
  
  if (
    !v %in%
    names(
      post_counts
    )
  ) {
    
    post_counts[[v]] <-
      0L
  }
}


# ============================================================
# 26. BUILD NUMERIC TABLE
# ============================================================

numeric_table <- shell %>%
  
  left_join(
    
    denominators,
    
    by = c(
      "TRT01A",
      "PARAMCD",
      "BASE_EVENT",
      "BASE_GRADE_LABEL"
    )
  ) %>%
  
  left_join(
    
    post_counts,
    
    by = c(
      "TRT01A",
      "PARAMCD",
      "BASE_EVENT",
      "BASE_GRADE_LABEL"
    )
  ) %>%
  
  mutate(
    
    across(
      
      c(
        N,
        POST_0,
        POST_1,
        POST_2,
        POST_3,
        POST_4,
        POST_OTHER
      ),
      
      ~ replace_na(
        .x,
        0L
      )
    )
  )


# ============================================================
# 27. ROW-SUM QA
#
# Every subject must appear in exactly one
# post-baseline category.
# ============================================================

numeric_table <- numeric_table %>%
  
  mutate(
    
    POST_SUM =
      POST_0 +
      POST_1 +
      POST_2 +
      POST_3 +
      POST_4 +
      POST_OTHER,
    
    ROW_MATCH =
      N ==
      POST_SUM
  )


cat(
  "\n============================================\n"
)

cat(
  "ROW-SUM QA\n"
)

cat(
  "============================================\n\n"
)


cat(
  "All baseline-grade rows reconcile to N:",
  all(
    numeric_table$ROW_MATCH
  ),
  "\n"
)


if (
  !all(
    numeric_table$ROW_MATCH
  )
) {
  
  print(
    
    numeric_table %>%
      
      filter(
        !ROW_MATCH
      ),
    
    n = Inf,
    
    width = Inf
  )
  
  
  stop(
    "Lab shift row counts do not reconcile."
  )
}


# ============================================================
# 28. ADDITIONAL SPECIAL-TEST QA
# ============================================================

special_eligible_check <- shift_subject %>%
  
  filter(
    SPECIAL_LOW
  ) %>%
  
  count(
    TRT01A,
    ORDER,
    LAB_PARAMETER,
    BASE_EVENT,
    name = "N"
  ) %>%
  
  arrange(
    
    factor(
      TRT01A,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    ),
    
    ORDER,
    
    BASE_EVENT
  )


cat(
  "\n============================================\n"
)

cat(
  "SPECIAL TEST BASELINE EVENT CHECK\n"
)

cat(
  "============================================\n\n"
)


print(
  special_eligible_check,
  n = Inf
)


# ============================================================
# 29. FORMAT DISPLAY TABLE
# ============================================================

display_table <- numeric_table %>%
  
  mutate(
    
    `0` =
      mapply(
        format_n_pct,
        POST_0,
        N
      ),
    
    `1` =
      mapply(
        format_n_pct,
        POST_1,
        N
      ),
    
    `2` =
      mapply(
        format_n_pct,
        POST_2,
        N
      ),
    
    `3` =
      mapply(
        format_n_pct,
        POST_3,
        N
      ),
    
    `4` =
      mapply(
        format_n_pct,
        POST_4,
        N
      ),
    
    Other =
      if_else(
        
        SPECIAL_LOW,
        
        mapply(
          format_n_pct,
          POST_OTHER,
          N
        ),
        
        ""
      )
  ) %>%
  
  arrange(
    
    factor(
      TRT01A,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    ),
    
    ORDER,
    
    ROW_ORDER
  ) %>%
  
  group_by(
    TRT01A,
    PARAMCD
  ) %>%
  
  mutate(
    
    Lab_Parameter =
      if_else(
        row_number() == 1,
        LAB_PARAMETER,
        ""
      ),
    
    Lab_Event =
      LAB_EVENT_DISPLAY
  ) %>%
  
  ungroup() %>%
  
  select(
    
    Treatment =
      TRT01A,
    
    Lab_Parameter,
    
    Lab_Event,
    
    `Baseline Grade` =
      BASE_GRADE_LABEL,
    
    N,
    
    `0`,
    
    `1`,
    
    `2`,
    
    `3`,
    
    `4`,
    
    `Other (value > ULN)` =
      Other
  )


# ============================================================
# 30. PRINT FINAL CALCULATED TABLE
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "LABORATORY SHIFT TABLE CALCULATED OUTPUT\n"
)

cat(
  "============================================\n\n"
)


print(
  display_table,
  n = Inf,
  width = Inf
)


# ============================================================
# 31. ELIGIBLE SUBJECTS BY PARAMETER
# ============================================================

parameter_summary <- shift_subject %>%
  
  count(
    
    TRT01A,
    
    ORDER,
    
    LAB_PARAMETER,
    
    name =
      "ELIGIBLE_N"
  ) %>%
  
  arrange(
    
    factor(
      TRT01A,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    ),
    
    ORDER
  )


cat(
  "\n============================================\n"
)

cat(
  "ELIGIBLE SUBJECTS BY LAB PARAMETER\n"
)

cat(
  "============================================\n\n"
)


print(
  parameter_summary,
  n = Inf
)


# ============================================================
# 32. SAVE VALIDATION CSV FILES
# ============================================================

write.csv(
  
  numeric_table %>%
    
    select(
      
      TRT01A,
      
      ORDER,
      
      PARAMCD,
      
      LAB_PARAMETER,
      
      BASE_EVENT,
      
      BASE_GRADE_LABEL,
      
      N,
      
      POST_0,
      
      POST_1,
      
      POST_2,
      
      POST_3,
      
      POST_4,
      
      POST_OTHER,
      
      POST_SUM,
      
      ROW_MATCH
    ),
  
  "local_outputs/lab_shift_numeric_counts.csv",
  
  row.names = FALSE
)


write.csv(
  
  display_table,
  
  "local_outputs/lab_shift_calculated_values.csv",
  
  row.names = FALSE
)


write.csv(
  
  parameter_summary,
  
  "local_outputs/lab_shift_parameter_population.csv",
  
  row.names = FALSE
)


write.csv(
  
  special_eligible_check,
  
  "local_outputs/lab_shift_special_baseline_event_check.csv",
  
  row.names = FALSE
)


# ============================================================
# 33. FUNCTION TO CREATE TREATMENT WORD TABLE
# ============================================================

make_treatment_table <- function(
    treatment
) {
  
  x <- display_table %>%
    
    filter(
      Treatment ==
        treatment
    ) %>%
    
    select(
      -Treatment
    )
  
  
  ft <- flextable(
    x
  )
  
  
  ft <- set_header_labels(
    
    ft,
    
    Lab_Parameter =
      "Lab Parameter",
    
    Lab_Event =
      "Lab Event",
    
    `Baseline Grade` =
      "Baseline Grade",
    
    N =
      "N",
    
    `0` =
      "0",
    
    `1` =
      "1",
    
    `2` =
      "2",
    
    `3` =
      "3",
    
    `4` =
      "4",
    
    `Other (value > ULN)` =
      "Other\n(value > ULN)"
  )
  
  
  ft <- add_header_row(
    
    ft,
    
    values = c(
      "",
      "",
      "",
      "",
      "Post-Baseline NCI CTCAE Grade"
    ),
    
    colwidths = c(
      1,
      1,
      1,
      1,
      6
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
    size = 7,
    part = "all"
  )
  
  
  ft <- fontsize(
    ft,
    size = 7.5,
    part = "header"
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
    j = 3:ncol(
      x
    ),
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
    width = 1.8
  )
  
  
  ft <- width(
    ft,
    j = 2,
    width = 0.65
  )
  
  
  ft <- width(
    ft,
    j = 3,
    width = 0.75
  )
  
  
  ft <- width(
    ft,
    j = 4,
    width = 0.4
  )
  
  
  ft <- width(
    ft,
    j = 5:9,
    width = 0.95
  )
  
  
  ft <- width(
    ft,
    j = 10,
    width = 1.25
  )
  
  
  ft <- padding(
    
    ft,
    
    padding.top =
      1,
    
    padding.bottom =
      1,
    
    padding.left =
      2,
    
    padding.right =
      2,
    
    part =
      "all"
  )
  
  
  ft <- set_table_properties(
    
    ft,
    
    layout =
      "fixed",
    
    opts_word =
      list(
        repeat_headers = TRUE
      )
  )
  
  
  ft
}


# ============================================================
# 34. CREATE WORD DOCUMENT
# ============================================================

doc <- read_docx()


# ============================================================
# 35. Treatment A TITLE
# ============================================================

doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Laboratory Shift Table",
      
      fp_text(
        bold = TRUE,
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      paste0(
        "Change in Laboratory Events: Shift in NCI-CTC Grade ",
        "from Baseline to Worst Post-Baseline Level"
      ),
      
      fp_text(
        bold = TRUE,
        font.size = 9
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Safety Evaluable Patients",
      
      fp_text(
        bold = TRUE,
        font.size = 9
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_par(
  
  doc,
  
  "Treatment: Treatment A"
)


doc <- body_add_flextable(
  
  doc,
  
  make_treatment_table(
    "Treatment A"
  )
)


# ============================================================
# 36. PAGE BREAK
# ============================================================

doc <- body_add_break(
  doc
)


# ============================================================
# 37. PLACEBO TITLE
# ============================================================

doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Laboratory Shift Table",
      
      fp_text(
        bold = TRUE,
        font.size = 10
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      paste0(
        "Change in Laboratory Events: Shift in NCI-CTC Grade ",
        "from Baseline to Worst Post-Baseline Level"
      ),
      
      fp_text(
        bold = TRUE,
        font.size = 9
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Safety Evaluable Patients",
      
      fp_text(
        bold = TRUE,
        font.size = 9
      )
    ),
    
    fp_p =
      fp_par(
        text.align = "center"
      )
  )
)


doc <- body_add_par(
  doc,
  ""
)


doc <- body_add_par(
  
  doc,
  
  "Treatment: Placebo"
)


doc <- body_add_flextable(
  
  doc,
  
  make_treatment_table(
    "Placebo"
  )
)


# ============================================================
# 38. FOOTNOTES
# ============================================================

doc <- body_add_par(
  doc,
  ""
)


footnotes <- c(
  
  paste0(
    "N = Number of subjects with a baseline and at least one ",
    "post-baseline value."
  ),
  
  paste0(
    "Baseline is the last non-missing laboratory observation ",
    "prior to initiation of study medication. If it is missing ",
    "but a result is available on the first treatment date, ",
    "that non-missing treatment-date result may be used as baseline."
  ),
  
  paste0(
    "All post-baseline laboratory values were used when determining ",
    "the highest NCI CTCAE grade laboratory event, including repeats ",
    "and unscheduled visits."
  ),
  
  paste0(
    "For Sodium, Potassium, and Magnesium, post-baseline Grade 1-4 ",
    "columns represent Low events. High Grade 1-4 values are pooled ",
    "in the Other (value > ULN) column."
  ),
  
  paste0(
    "For Sodium, Potassium, and Magnesium, baseline High Grade 1-4 ",
    "subjects are displayed in a separate pooled High / 1-4 row."
  ),
  
  paste0(
    "For the remaining selected laboratory tests, Grade 1-4 columns ",
    "represent High events and the Other column is blank."
  ),
  
  "Percentages are based on the number of subjects in the N column."
)


for (
  f in footnotes
) {
  
  doc <- body_add_par(
    doc,
    f
  )
}


doc <- body_end_section_landscape(
  doc
)


# ============================================================
# 39. SAVE WORD OUTPUT
# ============================================================

output_file <-
  "local_outputs/lab_shift_table.docx"


print(
  
  doc,
  
  target =
    output_file
)


# ============================================================
# 40. VALIDATION NOTES
# ============================================================

validation_notes <- c(
  
  "LABORATORY SHIFT TABLE",
  
  "Change in Laboratory Events: Shift in NCI-CTC Grade from Baseline to Worst Post-Baseline Level",
  
  "",
  
  "Population: Safety Evaluable Patients",
  
  "Source: ADLBSI",
  
  "Treatment variable: TRT01A",
  
  "",
  
  paste0(
    "Treatment A safety population = ",
    n_treatment_a
  ),
  
  paste0(
    "Placebo safety population = ",
    n_placebo
  ),
  
  "",
  
  "Selected tests in specification order:",
  
  "1. Sodium",
  
  "2. Potassium",
  
  "3. Magnesium",
  
  "4. Alkaline phosphatase",
  
  "5. SGOT/AST",
  
  "6. SGPT/ALT",
  
  "7. Total Bilirubin",
  
  "8. BUN",
  
  "9. Creatinine",
  
  "10. Urine Protein",
  
  "",
  
  "Special baseline rule:",
  
  paste0(
    "Sodium, Potassium and Magnesium Low events are displayed ",
    "for baseline Grades 4,3,2,1,0."
  ),
  
  paste0(
    "Baseline High Grade 1-4 subjects for these three tests are ",
    "displayed in a separate pooled High / 1-4 row."
  ),
  
  "",
  
  "Special post-baseline rule:",
  
  paste0(
    "For Sodium, Potassium and Magnesium, Low grades populate ",
    "Grade 0-4 columns and High Grades 1-4 populate Other (>ULN)."
  ),
  
  "",
  
  paste0(
    "Low/High equal worst-grade ties = ",
    nrow(
      special_ties
    )
  ),
  
  "",
  
  paste0(
    "All rows reconcile to N = ",
    all(
      numeric_table$ROW_MATCH
    )
  )
)


writeLines(
  
  validation_notes,
  
  "local_outputs/lab_shift_validation_notes.txt"
)


# ============================================================
# 41. LOG FILE
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO\n"
  )
  
  cat(
    "LABORATORY SHIFT TABLE\n"
  )
  
  cat(
    "LAB SHIFT TABLE\n\n"
  )
  
  
  cat(
    "Run date/time:",
    as.character(
      Sys.time()
    ),
    "\n\n"
  )
  
  
  cat(
    "ADLBSI dimensions:",
    paste(
      dim(
        adlbsi
      ),
      collapse = " x "
    ),
    "\n"
  )
  
  
  cat(
    "Safety subjects:",
    n_distinct(
      safety_subjects$USUBJID
    ),
    "\n"
  )
  
  
  cat(
    "Treatment A:",
    n_treatment_a,
    "\n"
  )
  
  
  cat(
    "Placebo:",
    n_placebo,
    "\n\n"
  )
  
  
  cat(
    "Special baseline High pooled rows:\n"
  )
  
  
  print(
    special_baseline_high,
    n = Inf
  )
  
  
  cat(
    "\nSpecial Low/High tie check:\n"
  )
  
  
  print(
    special_ties,
    n = Inf
  )
  
  
  cat(
    "\nEligible subjects by parameter:\n"
  )
  
  
  print(
    parameter_summary,
    n = Inf
  )
  
  
  cat(
    "\nAll rows reconcile to N:\n"
  )
  
  
  print(
    all(
      numeric_table$ROW_MATCH
    )
  )
  
  
  cat(
    "\nTotal shell rows:\n"
  )
  
  
  print(
    nrow(
      numeric_table
    )
  )
  
  
  cat(
    "\nPROGRAM COMPLETED SUCCESSFULLY\n"
  )
})


writeLines(
  
  log_lines,
  
  "local_outputs/lab_shift_run.log"
)


# ============================================================
# 42. FINAL FILE CHECK
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
  "Numeric Counts CSV: ",
  file.exists(
    "local_outputs/lab_shift_numeric_counts.csv"
  ),
  "\n"
)


cat(
  "Calculated Values CSV: ",
  file.exists(
    "local_outputs/lab_shift_calculated_values.csv"
  ),
  "\n"
)


cat(
  "Parameter Population CSV: ",
  file.exists(
    "local_outputs/lab_shift_parameter_population.csv"
  ),
  "\n"
)


cat(
  "Special Baseline Check CSV: ",
  file.exists(
    "local_outputs/lab_shift_special_baseline_event_check.csv"
  ),
  "\n"
)


cat(
  "Validation Notes: ",
  file.exists(
    "local_outputs/lab_shift_validation_notes.txt"
  ),
  "\n"
)


cat(
  "Log: ",
  file.exists(
    "local_outputs/lab_shift_run.log"
  ),
  "\n"
)


cat(
  "\nExpected final table rows: 106\n"
)


cat(
  "Actual final table rows: ",
  nrow(
    numeric_table
  ),
  "\n"
)


cat(
  "\nLABORATORY SHIFT PROGRAM COMPLETED.\n"
)
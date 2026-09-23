# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# ADVERSE EVENT TABLE
#
# Patients with Treatment-Emergent Adverse Events
# by Highest NCI CTCAE Grade
# Safety-Evaluable Patients
#
# Source: ADAE + ADSL
# Programming Language: R
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names and fixed expected counts are removed.
# - Analysis, sorting, formatting, and QC logic are preserved.
#
#
# UPDATED:
# SOC and Preferred Terms sorted by
# descending incidence percentage.
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

required_packages <- c(
  "haven",
  "dplyr",
  "tidyr",
  "stringr",
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


library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(flextable)
library(officer)


# ============================================================
# 2. LOCAL OUTPUT FOLDER
# ============================================================

# Public repository note:
# - Clinical data are not included in this repository.
# - Run-time outputs created from private data should remain local.
dir.create(
  "local_outputs",
  showWarnings = FALSE
)


# ============================================================
# 3. READ DATA
# Data files are intentionally excluded from the public repository.
# Supply compatible ADAE and ADSL XPT files locally under data/.
# ============================================================

adae <- read_xpt(
  "data/ADAE.xpt"
)

adsl <- read_xpt(
  "data/ADSL.xpt"
)


# ============================================================
# 4. TREATMENT ORDER
# ============================================================

trt_order <- c(
  "Placebo",
  "Treatment A"
)


# ============================================================
# 5. SAFETY POPULATION
#
# Safety analyses use ACTUAL treatment.
# ============================================================

safety <- adsl %>%
  
  filter(
    SAFFL == "Y",
    TRT01A %in% trt_order
  )


n_placebo <- safety %>%
  
  filter(
    TRT01A == "Placebo"
  ) %>%
  
  summarise(
    n = n_distinct(
      USUBJID
    )
  ) %>%
  
  pull(
    n
  )


n_treatment_a <- safety %>%
  
  filter(
    TRT01A == "Treatment A"
  ) %>%
  
  summarise(
    n = n_distinct(
      USUBJID
    )
  ) %>%
  
  pull(
    n
  )


total_safety_n <-
  n_placebo +
  n_treatment_a


cat(
  "\n============================================\n"
)

cat(
  "ADVERSE EVENT TABLE\n"
)

cat(
  "SAFETY POPULATION DENOMINATORS\n"
)

cat(
  "============================================\n\n"
)


cat(
  "Placebo N =",
  n_placebo,
  "\n"
)


cat(
  "Treatment A N =",
  n_treatment_a,
  "\n"
)


cat(
  "Total Safety N =",
  total_safety_n,
  "\n"
)


stopifnot(
  total_safety_n == nrow(safety),
  n_placebo + n_treatment_a == total_safety_n,
  n_distinct(safety$USUBJID) == total_safety_n
)


# ============================================================
# 6. HALF-UP PERCENTAGE ROUNDING
# ============================================================

format_n_pct <- function(
    n,
    denominator
) {
  
  if (
    denominator == 0
  ) {
    
    return(
      "0 (0.0%)"
    )
  }
  
  
  pct <-
    100 *
    n /
    denominator
  
  
  pct_round <-
    floor(
      pct * 10 + 0.5
    ) /
    10
  
  
  sprintf(
    "%d (%.1f%%)",
    n,
    pct_round
  )
}


# ============================================================
# 7. INSPECT TRTEM AND TOXICITY GRADES
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "TRTEM VALUES\n"
)

cat(
  "============================================\n"
)


print(
  table(
    adae$TRTEM,
    useNA = "ifany"
  )
)


cat(
  "\n============================================\n"
)

cat(
  "AETOXGR VALUES\n"
)

cat(
  "============================================\n"
)


print(
  table(
    adae$AETOXGR,
    useNA = "ifany"
  )
)


cat(
  "\n============================================\n"
)

cat(
  "AETOXGRN VALUES\n"
)

cat(
  "============================================\n"
)


print(
  table(
    adae$AETOXGRN,
    useNA = "ifany"
  )
)


# ============================================================
# 8. PREPARE TREATMENT-EMERGENT AE DATA
# ============================================================

ae <- adae %>%
  
  mutate(
    
    TRTEM_C =
      toupper(
        str_trim(
          as.character(
            TRTEM
          )
        )
      ),
    
    SOC =
      str_trim(
        as.character(
          AEBODSYS
        )
      ),
    
    PT =
      str_trim(
        as.character(
          AEDECOD
        )
      ),
    
    GRADE =
      suppressWarnings(
        as.numeric(
          AETOXGRN
        )
      )
  ) %>%
  
  filter(
    
    SAFFL == "Y",
    
    TRT01A %in%
      trt_order,
    
    TRTEM_C == "Y",
    
    !is.na(
      SOC
    ),
    
    SOC != "",
    
    !is.na(
      PT
    ),
    
    PT != ""
  )


cat(
  "\n============================================\n"
)

cat(
  "FILTERED TEAE DATA\n"
)

cat(
  "============================================\n\n"
)


cat(
  "TEAE records =",
  nrow(
    ae
  ),
  "\n"
)


cat(
  "Subjects with >=1 TEAE =",
  n_distinct(
    ae$USUBJID
  ),
  "\n"
)


# ============================================================
# 9. CHECK GRADES
# ============================================================

unexpected_grades <-
  sort(
    unique(
      ae$GRADE[
        !is.na(
          ae$GRADE
        ) &
          !(
            ae$GRADE %in%
              1:5
          )
      ]
    )
  )


if (
  length(
    unexpected_grades
  ) > 0
) {
  
  stop(
    paste(
      "Unexpected NCI CTCAE grade(s):",
      paste(
        unexpected_grades,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 10. FUNCTION:
# HIGHEST GRADE FOR EACH SUBJECT
#
# Subject is counted once at highest grade.
# ============================================================

highest_grade <- function(
    data,
    grouping_vars
) {
  
  data %>%
    
    group_by(
      across(
        all_of(
          c(
            "USUBJID",
            "TRT01A",
            grouping_vars
          )
        )
      )
    ) %>%
    
    summarise(
      
      HIGH_GRADE =
        
        if (
          all(
            is.na(
              GRADE
            )
          )
        ) {
          
          NA_real_
          
        } else {
          
          max(
            GRADE,
            na.rm = TRUE
          )
        },
      
      .groups =
        "drop"
    )
}


# ============================================================
# 11. ANY AE
# ============================================================

any_ae_high <- ae %>%
  
  group_by(
    USUBJID,
    TRT01A
  ) %>%
  
  summarise(
    
    HIGH_GRADE =
      
      if (
        all(
          is.na(
            GRADE
          )
        )
      ) {
        
        NA_real_
        
      } else {
        
        max(
          GRADE,
          na.rm = TRUE
        )
      },
    
    .groups =
      "drop"
  )


# ============================================================
# 12. SOC
# ============================================================

soc_high <- highest_grade(
  ae,
  "SOC"
)


# ============================================================
# 13. PREFERRED TERM
# ============================================================

pt_high <- highest_grade(
  ae,
  c(
    "SOC",
    "PT"
  )
)


# ============================================================
# 14. QA - NO DUPLICATES AFTER COLLAPSING
# ============================================================

stopifnot(
  
  nrow(
    any_ae_high
  ) ==
    nrow(
      distinct(
        any_ae_high,
        USUBJID,
        TRT01A
      )
    )
)


stopifnot(
  
  nrow(
    soc_high
  ) ==
    nrow(
      distinct(
        soc_high,
        USUBJID,
        TRT01A,
        SOC
      )
    )
)


stopifnot(
  
  nrow(
    pt_high
  ) ==
    nrow(
      distinct(
        pt_high,
        USUBJID,
        TRT01A,
        SOC,
        PT
      )
    )
)


# ============================================================
# 15. FUNCTION TO COUNT SUBJECTS
# ============================================================

get_n <- function(
    data,
    treatment,
    grade = NULL,
    not_graded = FALSE
) {
  
  temp <- data %>%
    
    filter(
      TRT01A ==
        treatment
    )
  
  
  if (
    !is.null(
      grade
    )
  ) {
    
    temp <- temp %>%
      
      filter(
        HIGH_GRADE ==
          grade
      )
  }
  
  
  if (
    isTRUE(
      not_graded
    )
  ) {
    
    temp <- temp %>%
      
      filter(
        is.na(
          HIGH_GRADE
        )
      )
  }
  
  
  n_distinct(
    temp$USUBJID
  )
}


# ============================================================
# 16. FUNCTION:
# CREATE ALL-GRADES + GRADE 5 TO 1 ROWS
# ============================================================

make_grade_rows <- function(
    data,
    row_label
) {
  
  # ----------------------------------------------------------
  # ALL GRADES
  # ----------------------------------------------------------
  
  p_all <-
    get_n(
      data,
      "Placebo"
    )
  
  
  c_all <-
    get_n(
      data,
      "Treatment A"
    )
  
  
  result <- tibble(
    
    Characteristic =
      row_label,
    
    Grade =
      "- All grades -",
    
    Placebo =
      format_n_pct(
        p_all,
        n_placebo
      ),
    
    `Treatment A` =
      format_n_pct(
        c_all,
        n_treatment_a
      )
  )
  
  
  # ----------------------------------------------------------
  # INDIVIDUAL GRADES
  # ----------------------------------------------------------
  
  for (
    g in 5:1
  ) {
    
    p_n <-
      get_n(
        data,
        "Placebo",
        grade = g
      )
    
    
    treatment_a_n <-
      get_n(
        data,
        "Treatment A",
        grade = g
      )
    
    
    result <-
      bind_rows(
        
        result,
        
        tibble(
          
          Characteristic =
            "",
          
          Grade =
            as.character(
              g
            ),
          
          Placebo =
            format_n_pct(
              p_n,
              n_placebo
            ),
          
          `Treatment A` =
            format_n_pct(
              treatment_a_n,
              n_treatment_a
            )
        )
      )
  }
  
  
  # ----------------------------------------------------------
  # NOT GRADED
  # ----------------------------------------------------------
  
  has_not_graded <-
    any(
      is.na(
        data$HIGH_GRADE
      )
    )
  
  
  if (
    has_not_graded
  ) {
    
    p_ng <-
      get_n(
        data,
        "Placebo",
        not_graded = TRUE
      )
    
    
    treatment_a_ng <-
      get_n(
        data,
        "Treatment A",
        not_graded = TRUE
      )
    
    
    result <-
      bind_rows(
        
        result,
        
        tibble(
          
          Characteristic =
            "",
          
          Grade =
            "Not graded",
          
          Placebo =
            format_n_pct(
              p_ng,
              n_placebo
            ),
          
          `Treatment A` =
            format_n_pct(
              treatment_a_ng,
              n_treatment_a
            )
        )
      )
  }
  
  
  result
}


# ============================================================
# 17. DESCENDING INCIDENCE SORT ORDER
#
# Portfolio sorting rule:
# Present System Organ Classes and Preferred Terms in descending
# incidence percentage, using alphabetical order for ties.
#
# Sorting key:
# Overall incidence across the safety population.
#
# SOC:
# highest incidence % to lowest.
#
# PT within SOC:
# highest incidence % to lowest.
#
# Ties:
# alphabetical.
# ============================================================


# ============================================================
# 17A. SOC SORT ORDER
# ============================================================

soc_sort <- soc_high %>%
  
  group_by(
    SOC
  ) %>%
  
  summarise(
    
    TOTAL_N =
      n_distinct(
        USUBJID
      ),
    
    OVERALL_PCT =
      100 *
      TOTAL_N /
      total_safety_n,
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    desc(
      OVERALL_PCT
    ),
    SOC
  )


soc_order <-
  soc_sort$SOC


# ============================================================
# 17B. PT SORT ORDER
# ============================================================

pt_sort <- pt_high %>%
  
  group_by(
    SOC,
    PT
  ) %>%
  
  summarise(
    
    TOTAL_N =
      n_distinct(
        USUBJID
      ),
    
    OVERALL_PCT =
      100 *
      TOTAL_N /
      total_safety_n,
    
    .groups =
      "drop"
  ) %>%
  
  mutate(
    
    SOC_ORDER =
      match(
        SOC,
        soc_order
      )
  ) %>%
  
  arrange(
    SOC_ORDER,
    desc(
      OVERALL_PCT
    ),
    PT
  )


# ============================================================
# 17C. PRINT SORTING QA
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "SOC DESCENDING INCIDENCE ORDER\n"
)

cat(
  "============================================\n\n"
)


print(
  soc_sort,
  n = Inf
)


cat(
  "\n============================================\n"
)

cat(
  "PREFERRED TERM DESCENDING INCIDENCE ORDER\n"
)

cat(
  "============================================\n\n"
)


print(
  pt_sort %>%
    select(
      SOC,
      PT,
      TOTAL_N,
      OVERALL_PCT
    ),
  n = Inf
)


# ============================================================
# 18. QA SORTING
# ============================================================

soc_sort_qa <-
  all(
    diff(
      soc_sort$OVERALL_PCT
    ) <=
      0
  )


pt_sort_qa <- pt_sort %>%
  
  group_by(
    SOC
  ) %>%
  
  summarise(
    
    SORT_OK =
      all(
        diff(
          OVERALL_PCT
        ) <=
          0
      ),
    
    .groups =
      "drop"
  )


cat(
  "\n============================================\n"
)

cat(
  "SORTING QA\n"
)

cat(
  "============================================\n\n"
)


cat(
  "SOC descending order:",
  soc_sort_qa,
  "\n"
)


print(
  pt_sort_qa,
  n = Inf
)


stopifnot(
  soc_sort_qa
)


stopifnot(
  all(
    pt_sort_qa$SORT_OK
  )
)


# ============================================================
# 19. BEGIN FINAL TABLE
# ============================================================

ae_table <-
  make_grade_rows(
    
    any_ae_high,
    
    "- Any adverse events -"
  )


# ============================================================
# 20. ADD SOC AND PREFERRED TERMS
# ============================================================

for (
  soc_value in soc_order
) {
  
  # ----------------------------------------------------------
  # SOC HEADER
  # ----------------------------------------------------------
  
  ae_table <-
    bind_rows(
      
      ae_table,
      
      tibble(
        
        Characteristic =
          soc_value,
        
        Grade =
          "",
        
        Placebo =
          "",
        
        `Treatment A` =
          ""
      )
    )
  
  
  # ----------------------------------------------------------
  # SOC OVERALL
  # ----------------------------------------------------------
  
  soc_data <- soc_high %>%
    
    filter(
      SOC ==
        soc_value
    )
  
  
  ae_table <-
    bind_rows(
      
      ae_table,
      
      make_grade_rows(
        soc_data,
        "-Overall-"
      )
    )
  
  
  # ----------------------------------------------------------
  # PT ORDER WITHIN SOC
  # ----------------------------------------------------------
  
  pts <- pt_sort %>%
    
    filter(
      SOC ==
        soc_value
    ) %>%
    
    pull(
      PT
    )
  
  
  # ----------------------------------------------------------
  # EACH PREFERRED TERM
  # ----------------------------------------------------------
  
  for (
    pt_value in pts
  ) {
    
    pt_data <- pt_high %>%
      
      filter(
        SOC ==
          soc_value,
        PT ==
          pt_value
      )
    
    
    ae_table <-
      bind_rows(
        
        ae_table,
        
        make_grade_rows(
          
          pt_data,
          
          paste0(
            "    ",
            pt_value
          )
        )
      )
  }
}


# ============================================================
# 21. PRINT TABLE
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "ADVERSE EVENT CALCULATED OUTPUT\n"
)

cat(
  "============================================\n\n"
)


print(
  ae_table,
  n = Inf
)


# ============================================================
# 22. ADDITIONAL QA
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "GRADE DISTRIBUTION - HIGHEST ANY AE\n"
)

cat(
  "============================================\n"
)


print(
  table(
    any_ae_high$HIGH_GRADE,
    any_ae_high$TRT01A,
    useNA = "ifany"
  )
)


stopifnot(
  
  n_distinct(
    any_ae_high$USUBJID
  ) <=
    nrow(
      safety
    )
)


cat(
  "\n============================================\n"
)

cat(
  "ALL INTERNAL QA CHECKS PASSED\n"
)

cat(
  "============================================\n"
)


# ============================================================
# 23. SAVE CALCULATED VALUES
# ============================================================

write.csv(
  
  ae_table,
  
  "local_outputs/adverse_events_calculated_values.csv",
  
  row.names = FALSE
)


# ============================================================
# 24. SAVE SORT ORDER VALIDATION
# ============================================================

write.csv(
  
  soc_sort,
  
  "local_outputs/adverse_events_soc_sort_order.csv",
  
  row.names = FALSE
)


write.csv(
  
  pt_sort %>%
    select(
      SOC,
      PT,
      TOTAL_N,
      OVERALL_PCT
    ),
  
  "local_outputs/adverse_events_pt_sort_order.csv",
  
  row.names = FALSE
)


# ============================================================
# 25. CREATE DISPLAY COPY
# ============================================================

display_table <-
  ae_table


names(
  display_table
) <- c(
  
  "MedDRA System Organ Class and\nPreferred Term",
  
  "NCI CTCAE\nGrade",
  
  paste0(
    "Placebo\n(n=",
    n_placebo,
    ")"
  ),
  
  paste0(
    "Treatment A\n(n=",
    n_treatment_a,
    ")"
  )
)


# ============================================================
# 26. CREATE FLEXTABLE
# ============================================================

ft <- flextable(
  display_table
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
  size = 7.5,
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
  width = 3.4
)


ft <- width(
  ft,
  j = 2,
  width = 1.0
)


ft <- width(
  ft,
  j = 3:4,
  width = 1.35
)


ft <- padding(
  ft,
  padding.top = 1,
  padding.bottom = 1,
  part = "all"
)


# ============================================================
# 27. BOLD SOC HEADERS
# ============================================================

soc_header_rows <-
  which(
    
    ae_table$Grade == "" &
      
      ae_table$Placebo == "" &
      
      ae_table$`Treatment A` == ""
  )


if (
  length(
    soc_header_rows
  ) > 0
) {
  
  ft <- bold(
    
    ft,
    
    i =
      soc_header_rows,
    
    j =
      1,
    
    bold =
      TRUE,
    
    part =
      "body"
  )
}


# Bold Any AE row

ft <- bold(
  ft,
  i = 1,
  j = 1,
  bold = TRUE,
  part = "body"
)


ft <- set_table_properties(
  ft,
  layout = "fixed"
)


# ============================================================
# 28. CREATE WORD DOCUMENT
# ============================================================

doc <- read_docx()


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Adverse Event Table",
      
      fp_text(
        bold = TRUE,
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


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Patients with Treatment-Emergent Adverse Events by Highest NCI CTCAE Grade",
      
      fp_text(
        bold = TRUE,
        font.size = 9
      )
    ),
    
    fp_p =
      fp_par(
        text.align =
          "center"
      )
  )
)


doc <- body_add_fpar(
  
  doc,
  
  fpar(
    
    ftext(
      
      "Safety-Evaluable Patients",
      
      fp_text(
        italic = TRUE,
        font.size = 9
      )
    ),
    
    fp_p =
      fp_par(
        text.align =
          "center"
      )
  )
)


doc <- body_add_flextable(
  doc,
  ft
)


# ============================================================
# 29. FOOTNOTES
# ============================================================

doc <- body_add_par(
  
  doc,
  
  paste0(
    "NCI CTCAE = National Cancer Institute Common Terminology ",
    "Criteria for Adverse Events."
  ),
  
  style = "Normal"
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "A patient with multiple occurrences of an adverse event ",
    "is counted once at the highest NCI CTCAE grade."
  ),
  
  style = "Normal"
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "Percentages are based on the number of safety-evaluable ",
    "patients in each actual treatment group."
  ),
  
  style = "Normal"
)


doc <- body_add_par(
  
  doc,
  
  paste0(
    "System Organ Classes and Preferred Terms are presented ",
    "in descending order of overall incidence percentage; ",
    "alphabetical order is used for ties."
  ),
  
  style = "Normal"
)


# ============================================================
# 30. SAVE WORD OUTPUT
# ============================================================

output_file <-
  "local_outputs/adverse_events_table.docx"


print(
  doc,
  target = output_file
)


# ============================================================
# 31. VALIDATION NOTES
# ============================================================

validation_notes <- c(
  
  "Adverse Event Table",
  
  "Patients with Treatment-Emergent Adverse Events by Highest NCI CTCAE Grade",
  
  "",
  
  "Source Dataset: ADAE",
  
  "Population: SAFFL = Y",
  
  "Treatment variable: TRT01A",
  
  "Treatment-emergent condition: TRTEM = Y",
  
  paste0(
    "Placebo denominator = ",
    n_placebo
  ),
  
  paste0(
    "Treatment A denominator = ",
    n_treatment_a
  ),
  
  paste0(
    "Total safety population = ",
    total_safety_n
  ),
  
  "",
  
  "Counting Rule:",
  
  "Each subject is counted once at the highest NCI CTCAE grade.",
  
  "This rule is applied at Any AE, SOC, and Preferred Term level.",
  
  "",
  
  "Sorting Rule:",
  
  "System Organ Classes are sorted by decreasing overall incidence percentage.",
  
  "Preferred Terms within each System Organ Class are sorted by decreasing overall incidence percentage.",
  
  "Overall incidence percentage uses the total safety population as denominator.",
  
  "Alphabetical order is used to break incidence-percentage ties.",
  
  "",
  
  "Sorting QC: AE categories are presented in descending incidence percentage."
)


writeLines(
  
  validation_notes,
  
  "local_outputs/adverse_events_validation_notes.txt"
)


# ============================================================
# 32. LOG
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO\n"
  )
  
  cat(
    "ADVERSE EVENT TABLE\n"
  )
  
  cat(
    "Patients with Treatment-Emergent Adverse Events\n\n"
  )
  
  
  cat(
    "Run Date:",
    as.character(
      Sys.time()
    ),
    "\n\n"
  )
  
  
  cat(
    "ADAE Records:",
    nrow(
      adae
    ),
    "\n"
  )
  
  
  cat(
    "TEAE Records:",
    nrow(
      ae
    ),
    "\n"
  )
  
  
  cat(
    "TEAE Subjects:",
    n_distinct(
      ae$USUBJID
    ),
    "\n\n"
  )
  
  
  cat(
    "Placebo Safety N:",
    n_placebo,
    "\n"
  )
  
  
  cat(
    "Treatment A Safety N:",
    n_treatment_a,
    "\n"
  )
  
  
  cat(
    "Total Safety N:",
    total_safety_n,
    "\n\n"
  )
  
  
  cat(
    "SOC Sorting:\n"
  )
  
  
  print(
    soc_sort,
    n = Inf
  )
  
  
  cat(
    "\nPT Sorting:\n"
  )
  
  
  print(
    pt_sort %>%
      select(
        SOC,
        PT,
        TOTAL_N,
        OVERALL_PCT
      ),
    n = Inf
  )
  
  
  cat(
    "\nSOC descending sort QA:",
    soc_sort_qa,
    "\n"
  )
  
  
  cat(
    "All PT descending sort QA:",
    all(
      pt_sort_qa$SORT_OK
    ),
    "\n"
  )
  
  
  cat(
    "\nQA STATUS: PASSED\n"
  )
  
  
  cat(
    "SORTING QC: PASSED\n"
  )
})


writeLines(
  
  log_lines,
  
  "local_outputs/adverse_events_run.log"
)


# ============================================================
# 33. FINAL FILE CHECK
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
  "Validation CSV: ",
  file.exists(
    "local_outputs/adverse_events_calculated_values.csv"
  ),
  "\n"
)


cat(
  "SOC Sort Validation: ",
  file.exists(
    "local_outputs/adverse_events_soc_sort_order.csv"
  ),
  "\n"
)


cat(
  "PT Sort Validation: ",
  file.exists(
    "local_outputs/adverse_events_pt_sort_order.csv"
  ),
  "\n"
)


cat(
  "Validation Notes: ",
  file.exists(
    "local_outputs/adverse_events_validation_notes.txt"
  ),
  "\n"
)


cat(
  "Log: ",
  file.exists(
    "local_outputs/adverse_events_run.log"
  ),
  "\n"
)


cat(
  "\n============================================\n"
)

cat(
  "ADVERSE EVENT PROGRAM COMPLETED.\n"
)

cat(
  "AE TABLE SORTED BY DESCENDING INCIDENCE PERCENTAGE.\n"
)

cat(
  "============================================\n"
)
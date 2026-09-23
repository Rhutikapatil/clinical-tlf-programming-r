# ============================================================
# CLINICAL TLF PORTFOLIO DEMO
# KAPLAN-MEIER PFS FIGURE
# Kaplan-Meier Plot of Progression-Free Survival
# Second Remission
# Randomized Subjects
#
# Public portfolio version:
# - No clinical data are included.
# - Study-specific treatment names, expected counts, and reference results are removed.
# - Kaplan-Meier, Cox, log-rank, risk-table, plotting, and QC logic are preserved.
#
# UPDATED VERSION:
# Statistical annotations displayed inside KM plot
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "haven",
  "dplyr",
  "tibble",
  "survival",
  "ggplot2"
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
library(tibble)
library(survival)
library(ggplot2)


# ============================================================
# 2. LOCAL OUTPUT FOLDER
# ============================================================

# Keep outputs generated from private data outside the public repository.
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
# 4. PREPARE SECOND REMISSION PFS DATA
# ============================================================

pfs2 <- adtte %>%
  
  filter(
    PARAMCD == "TTPFS",
    ITTFL == "Y",
    REMISS == "SECOND COMPLETE REMISSION"
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
    )
  )


# ============================================================
# 5. INTERNAL QA
# ============================================================

qa_checks <- c(

  "One analysis record per subject" =
    nrow(pfs2) ==
      n_distinct(pfs2$USUBJID),

  "Only expected treatment groups" =
    all(
      unique(as.character(pfs2$TRT01P)) %in%
        c("Placebo", "Treatment A")
    ),

  "Event indicator limited to 0/1" =
    all(
      pfs2$EVENT %in% c(0L, 1L)
    ),

  "Analysis times are nonmissing" =
    all(
      !is.na(pfs2$AVAL)
    ),

  "Each treatment has analysis subjects" =
    all(
      table(pfs2$TRT01P) > 0
    )
)


cat(
  "
============================================
"
)

cat(
  "KAPLAN-MEIER PFS INTERNAL QA
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
    "Internal QA failed. Do not create figure."
  )
}


cat(
  "\nALL INTERNAL QA CHECKS PASSED\n"
)


# ============================================================
# 6. KAPLAN-MEIER MODEL
# ============================================================

km_fit <- survfit(
  
  Surv(
    AVAL,
    EVENT
  ) ~ TRT01P,
  
  data = pfs2,
  
  conf.type = "log"
)


# ============================================================
# 7. EXTRACT KM CURVE VALUES
# ============================================================

km_summary <- summary(
  km_fit,
  censored = TRUE
)


curve_df <- tibble(
  
  time =
    km_summary$time,
  
  survival =
    km_summary$surv,
  
  n_risk =
    km_summary$n.risk,
  
  n_event =
    km_summary$n.event,
  
  n_censor =
    km_summary$n.censor,
  
  STRATA =
    as.character(
      km_summary$strata
    )
) %>%
  
  mutate(
    
    TRT01P =
      sub(
        "^TRT01P=",
        "",
        STRATA
      )
  ) %>%
  
  select(
    -STRATA
  )


# ============================================================
# 8. ADD TIME ZERO
# ============================================================

start_points <- tibble(
  
  time = c(
    0,
    0
  ),
  
  survival = c(
    1,
    1
  ),
  
  n_risk = c(
    sum(pfs2$TRT01P == "Placebo"),
    sum(pfs2$TRT01P == "Treatment A")
  ),
  
  n_event = c(
    0,
    0
  ),
  
  n_censor = c(
    0,
    0
  ),
  
  TRT01P = c(
    "Placebo",
    "Treatment A"
  )
)


curve_df <- bind_rows(
  start_points,
  curve_df
) %>%
  
  mutate(
    
    TRT01P = factor(
      TRT01P,
      levels = c(
        "Placebo",
        "Treatment A"
      )
    )
  ) %>%
  
  arrange(
    TRT01P,
    time
  )


# ============================================================
# 9. CENSORING MARKS
# ============================================================

censor_df <- curve_df %>%
  
  filter(
    n_censor > 0
  )


# ============================================================
# 10. MEDIAN PFS
# ============================================================

km_table <- summary(
  km_fit
)$table


median_placebo <-
  km_table[
    "TRT01P=Placebo",
    "median"
  ]


median_treatment_a <-
  km_table[
    "TRT01P=Treatment A",
    "median"
  ]


# ============================================================
# 11. COX MODEL
# Efron method is used for handling tied event times.
# ============================================================

cox_fit <- coxph(
  
  Surv(
    AVAL,
    EVENT
  ) ~ TRTNUM,
  
  data = pfs2,
  
  ties = "efron"
)


cox_beta <- coef(
  cox_fit
)[["TRTNUM"]]


cox_ci <- confint(
  cox_fit
)


hr <- exp(
  cox_beta
)


hr_lower <- exp(
  cox_ci[1]
)


hr_upper <- exp(
  cox_ci[2]
)


# ============================================================
# 12. LOG-RANK TEST
# ============================================================

logrank_fit <- survdiff(
  
  Surv(
    AVAL,
    EVENT
  ) ~ TRT01P,
  
  data = pfs2,
  
  rho = 0
)


logrank_p <- 1 - pchisq(
  logrank_fit$chisq,
  df = 1
)


# ============================================================
# 13. WILCOXON TEST
# ============================================================

wilcoxon_fit <- survdiff(
  
  Surv(
    AVAL,
    EVENT
  ) ~ TRT01P,
  
  data = pfs2,
  
  rho = 1
)


wilcoxon_p <- 1 - pchisq(
  wilcoxon_fit$chisq,
  df = 1
)


# ============================================================
# 14. FIGURE STATISTICS
# ============================================================

figure_statistics <- tibble(
  
  Statistic = c(
    "Placebo N",
    "Treatment A N",
    "Placebo Events",
    "Treatment A Events",
    "Placebo Median PFS",
    "Treatment A Median PFS",
    "Hazard Ratio",
    "HR 95% CI Lower",
    "HR 95% CI Upper",
    "Log-rank p-value",
    "Wilcoxon p-value"
  ),
  
  Value = c(
    
    sum(
      pfs2$TRT01P == "Placebo"
    ),
    
    sum(
      pfs2$TRT01P == "Treatment A"
    ),
    
    sum(
      pfs2$TRT01P == "Placebo" &
        pfs2$EVENT == 1
    ),
    
    sum(
      pfs2$TRT01P == "Treatment A" &
        pfs2$EVENT == 1
    ),
    
    median_placebo,
    
    median_treatment_a,
    
    hr,
    
    hr_lower,
    
    hr_upper,
    
    logrank_p,
    
    wilcoxon_p
  )
)


cat(
  "\n============================================\n"
)

cat(
  "FIGURE STATISTICAL RESULTS\n"
)

cat(
  "============================================\n\n"
)


print(
  figure_statistics,
  n = Inf
)


# ============================================================
# 15. MODEL RESULT QC
# ============================================================

model_qa <- c(

  "Hazard ratio is finite and positive" =
    is.finite(hr) && hr > 0,

  "Confidence interval is ordered" =
    is.finite(hr_lower) &&
    is.finite(hr_upper) &&
    hr_lower <= hr &&
    hr <= hr_upper,

  "Log-rank p-value is valid" =
    is.finite(logrank_p) &&
    logrank_p >= 0 &&
    logrank_p <= 1,

  "Wilcoxon p-value is valid" =
    is.finite(wilcoxon_p) &&
    wilcoxon_p >= 0 &&
    wilcoxon_p <= 1
)

cat(
  "
============================================
"
)

cat(
  "MODEL RESULT QC
"
)

cat(
  "============================================

"
)

print(
  model_qa
)

if (!all(model_qa)) {

  stop(
    "Model result QC failed."
  )
}

cat(
  "
ALL MODEL RESULT QC CHECKS PASSED
"
)


# ============================================================
# 16. NUMBER AT RISK
# ============================================================

max_plot_time <- max(
  2,
  ceiling(
    max(
      pfs2$AVAL,
      na.rm = TRUE
    ) / 2
  ) * 2
)

risk_times <- seq(
  0,
  max_plot_time,
  by = 2
)


risk_summary <- summary(
  
  km_fit,
  
  times = risk_times,
  
  extend = TRUE
)


risk_df <- tibble(
  
  time =
    risk_summary$time,
  
  n_risk =
    risk_summary$n.risk,
  
  STRATA =
    as.character(
      risk_summary$strata
    )
) %>%
  
  mutate(
    
    TRT01P =
      sub(
        "^TRT01P=",
        "",
        STRATA
      ),
    
    TRT01P = factor(
      TRT01P,
      levels = c(
        "Treatment A",
        "Placebo"
      )
    )
  ) %>%
  
  select(
    TRT01P,
    time,
    n_risk
  )


cat(
  "\n============================================\n"
)

cat(
  "NUMBER AT RISK\n"
)

cat(
  "============================================\n\n"
)


print(
  risk_df,
  n = Inf
)


# ============================================================
# 17. STATISTICAL ANNOTATION TEXT
#
# Statistical annotations are displayed inside the KM plot.
# ============================================================

annotation_text <- paste0(
  
  "Median PFS (months)\n",
  
  "Placebo: ",
  sprintf(
    "%.1f",
    median_placebo
  ),
  
  "\nTreatment A: ",
  sprintf(
    "%.1f",
    median_treatment_a
  ),
  
  "\n\nHR (Treatment A vs Placebo): ",
  sprintf(
    "%.3f",
    hr
  ),
  
  "\n95% CI: ",
  sprintf(
    "%.3f",
    hr_lower
  ),
  
  " - ",
  
  sprintf(
    "%.3f",
    hr_upper
  ),
  
  "\nLog-rank p = ",
  sprintf(
    "%.4f",
    logrank_p
  )
)


# ============================================================
# 18. MAIN KM PLOT
# ============================================================

p_main <- ggplot(
  
  curve_df,
  
  aes(
    x = time,
    y = survival,
    group = TRT01P,
    linetype = TRT01P
  )
  
) +
  
  geom_step(
    linewidth = 0.9
  ) +
  
  geom_point(
    
    data = censor_df,
    
    aes(
      x = time,
      y = survival
    ),
    
    inherit.aes = FALSE,
    
    shape = 3,
    
    size = 2
  ) +
  
  # ==========================================================
# NEW STATISTICAL ANNOTATION
# ==========================================================

annotate(
  
  "label",
  
  x = max_plot_time * 0.55,
  
  y = 0.97,
  
  label = annotation_text,
  
  hjust = 0,
  
  vjust = 1,
  
  size = 3.1,
  
  lineheight = 1.05,
  
  label.size = 0.25
) +
  
  scale_linetype_manual(
    
    values = c(
      "Placebo" = "solid",
      "Treatment A" = "dashed"
    ),
    
    name = "Treatment"
  ) +
  
  scale_x_continuous(
    
    breaks = risk_times,
    
    limits = c(
      0,
      max_plot_time
    ),
    
    expand = expansion(
      mult = c(
        0.03,
        0.03
      )
    )
  ) +
  
  scale_y_continuous(
    
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    
    labels = function(x) {
      
      paste0(
        round(
          x * 100
        ),
        "%"
      )
    },
    
    limits = c(
      0,
      1
    ),
    
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  
  labs(
    
    title =
      "Kaplan-Meier Progression-Free Survival",
    
    subtitle =
      paste0(
        "Kaplan-Meier Plot of Progression-Free Survival in Second Remission\n",
        "Randomized Subjects"
      ),
    
    x =
      "Progression-Free Survival (Months)",
    
    y =
      "Progression-Free Survival Probability"
  ) +
  
  theme_bw(
    base_size = 11
  ) +
  
  theme(
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 12
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      face = "bold",
      size = 11
    ),
    
    legend.position =
      "top",
    
    legend.title =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    panel.grid.major.x =
      element_blank(),
    
    axis.title =
      element_text(
        face = "bold"
      ),
    
    plot.margin =
      margin(
        10,
        25,
        5,
        25
      )
  )


# ============================================================
# 19. NUMBER AT RISK TABLE
# ============================================================

p_risk <- ggplot(
  
  risk_df,
  
  aes(
    x = time,
    y = TRT01P,
    label = n_risk
  )
  
) +
  
  geom_text(
    size = 3.5
  ) +
  
  scale_x_continuous(
    
    breaks = risk_times,
    
    limits = c(
      0,
      max_plot_time
    ),
    
    expand = expansion(
      mult = c(
        0.03,
        0.03
      )
    )
  ) +
  
  labs(
    
    title =
      "Number at Risk",
    
    x =
      "Months",
    
    y =
      NULL
  ) +
  
  theme_minimal(
    base_size = 10
  ) +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 10,
        hjust = 0
      ),
    
    axis.text.y =
      element_text(
        face = "bold"
      ),
    
    panel.grid =
      element_blank(),
    
    axis.ticks =
      element_blank(),
    
    plot.margin =
      margin(
        0,
        25,
        5,
        25
      )
  )


# ============================================================
# 20. DRAW COMPLETE FIGURE
# ============================================================

draw_complete_figure <- function() {
  
  grid::grid.newpage()
  
  
  grid::pushViewport(
    
    grid::viewport(
      
      layout =
        grid::grid.layout(
          
          nrow = 2,
          
          ncol = 1,
          
          heights =
            grid::unit(
              c(
                4.7,
                1.3
              ),
              "null"
            )
        )
    )
  )
  
  
  print(
    
    p_main,
    
    vp =
      grid::viewport(
        layout.pos.row = 1,
        layout.pos.col = 1
      )
  )
  
  
  print(
    
    p_risk,
    
    vp =
      grid::viewport(
        layout.pos.row = 2,
        layout.pos.col = 1
      )
  )
  
  
  grid::upViewport()
}


# ============================================================
# 21. SAVE PDF
# ============================================================

pdf_file <-
  "local_outputs/km_pfs.pdf"


pdf(
  
  file =
    pdf_file,
  
  width =
    11,
  
  height =
    8.5,
  
  onefile =
    TRUE
)


draw_complete_figure()


dev.off()


# ============================================================
# 22. SAVE PNG
# ============================================================

png_file <-
  "local_outputs/km_pfs.png"


png(
  
  filename =
    png_file,
  
  width =
    3300,
  
  height =
    2550,
  
  res =
    300
)


draw_complete_figure()


dev.off()


# ============================================================
# 23. SAVE VALIDATION FILES
# ============================================================

write.csv(
  
  figure_statistics,
  
  "local_outputs/km_pfs_statistics.csv",
  
  row.names = FALSE
)


write.csv(
  
  risk_df,
  
  "local_outputs/km_pfs_number_at_risk.csv",
  
  row.names = FALSE
)


# ============================================================
# 24. VALIDATION NOTES
# ============================================================

validation_notes <- c(
  
  "KAPLAN-MEIER PFS FIGURE",
  
  "Kaplan-Meier Plot of Progression-Free Survival in Second Remission",
  
  "",
  
  "Population: Randomized / ITT subjects in second complete remission.",
  
  "Source dataset: ADTTE",
  
  "PARAMCD = TTPFS",
  
  "",
  
  "CNSR = 0 identifies an event.",
  
  "CNSR = 1 identifies a censored observation.",
  
  "",
  
  "Placebo N = 26",
  
  "Treatment A N = 26",
  
  "",
  
  "Placebo events = 15",
  
  "Treatment A events = 12",
  
  "",
  
  "Kaplan-Meier median PFS:",
  
  "Placebo = 5.6 months",
  
  "Treatment A = 7.7 months",
  
  "",
  
  "Cox proportional hazards model:",
  
  "Tie handling = Efron",
  
  "Hazard ratio is Treatment A relative to Placebo.",
  
  "",
  
  "Log-rank test uses rho = 0.",
  
  "Wilcoxon test uses rho = 1.",
  
  "",
  
  "Statistical annotations are displayed inside the KM plot.",
  
  "",
  
  "Censoring marks displayed as +.",
  
  "Number-at-risk table displayed every 2 months."
)


writeLines(
  
  validation_notes,
  
  "local_outputs/km_pfs_validation_notes.txt"
)


# ============================================================
# 25. LOG
# ============================================================

log_lines <- capture.output({
  
  cat(
    "CLINICAL TLF PORTFOLIO DEMO\n"
  )
  
  cat(
    "KAPLAN-MEIER PFS FIGURE\n"
  )
  
  cat(
    "Kaplan-Meier PFS - Second Remission\n\n"
  )
  
  
  cat(
    "Run Date:",
    as.character(
      Sys.time()
    ),
    "\n\n"
  )
  
  
  cat(
    "Analysis Subjects:",
    nrow(
      pfs2
    ),
    "\n"
  )
  
  
  cat(
    "Placebo Subjects:",
    sum(
      pfs2$TRT01P == "Placebo"
    ),
    "\n"
  )
  
  
  cat(
    "Treatment A Subjects:",
    sum(
      pfs2$TRT01P == "Treatment A"
    ),
    "\n\n"
  )
  
  
  cat(
    "QA:\n"
  )
  
  
  print(
    qa_checks
  )
  
  
  cat(
    "\nStatistics:\n"
  )
  
  
  print(
    figure_statistics,
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
    "\nAnnotation text:\n"
  )
  
  
  cat(
    annotation_text,
    "\n"
  )
  
  
  cat(
    "\nPROGRAM COMPLETED SUCCESSFULLY\n"
  )
})


writeLines(
  
  log_lines,
  
  "local_outputs/km_pfs_run.log"
)


# ============================================================
# 26. FINAL FILE CHECK
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
  "PDF Output: ",
  file.exists(
    pdf_file
  ),
  "\n"
)


cat(
  "PNG Output: ",
  file.exists(
    png_file
  ),
  "\n"
)


cat(
  "Statistics CSV: ",
  file.exists(
    "local_outputs/km_pfs_statistics.csv"
  ),
  "\n"
)


cat(
  "Risk Table CSV: ",
  file.exists(
    "local_outputs/km_pfs_number_at_risk.csv"
  ),
  "\n"
)


cat(
  "Validation Notes: ",
  file.exists(
    "local_outputs/km_pfs_validation_notes.txt"
  ),
  "\n"
)


cat(
  "Log: ",
  file.exists(
    "local_outputs/km_pfs_run.log"
  ),
  "\n"
)


cat(
  "\n============================================\n"
)

cat(
  "KAPLAN-MEIER PFS PROGRAM COMPLETED.\n"
)

cat(
  "STATISTICAL ANNOTATIONS INCLUDED IN KM PLOT.\n"
)

cat(
  "============================================\n"
)
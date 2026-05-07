# R version: 4.3+ recommended
# Required packages: readr, dplyr, stringr
#
# Computes:
# 1) E-values for reported relative risks and CI limits closest to the null.
# 2) Benjamini-Hochberg FDR correction across all 24 outcome-comparison tests.

set.seed(20260423)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

base_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(base_dir)

input_path <- file.path(base_dir, "results.csv")
if (!file.exists(input_path)) {
  stop("results.csv not found. Run make_figures.R first.", call. = FALSE)
}

e_value_rr <- function(rr) {
  rr <- as.numeric(rr)
  rr_for_evalue <- ifelse(is.na(rr), NA_real_, ifelse(rr < 1, 1 / rr, rr))
  ifelse(
    is.na(rr_for_evalue),
    NA_real_,
    ifelse(rr_for_evalue <= 1, 1, rr_for_evalue + sqrt(rr_for_evalue * (rr_for_evalue - 1)))
  )
}

e_value_ci <- function(rr, lo, hi) {
  rr <- as.numeric(rr)
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)

  dplyr::case_when(
    is.na(rr) | is.na(lo) | is.na(hi) ~ NA_real_,
    lo <= 1 & hi >= 1 ~ 1,
    rr >= 1 & lo > 1 ~ e_value_rr(lo),
    rr < 1 & hi < 1 ~ e_value_rr(hi),
    TRUE ~ 1
  )
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ NA_character_,
    p == 0 ~ "<0.001",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

format_num <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

results <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(
    comparison = as.character(comparison),
    outcome = as.character(outcome),
    raw_p = logrank_p,
    fdr_p = p.adjust(raw_p, method = "BH"),
    raw_p_display = format_p(raw_p),
    fdr_p_display = format_p(fdr_p),
    fdr_significant_05 = fdr_p < 0.05,
    e_value_rr = e_value_rr(rr),
    e_value_ci_limit = e_value_ci(rr, rr_lo, rr_hi),
    rr_display = sprintf("%.3f (%.3f, %.3f)", rr, rr_lo, rr_hi),
    e_value_display = sprintf("%.2f; CI limit %.2f", e_value_rr, e_value_ci_limit)
  ) %>%
  arrange(comparison, family_order, outcome_order)

write_csv(
  results %>%
    select(
      comparison,
      comparison_label,
      outcome,
      outcome_code,
      cohort1_label,
      cohort2_label,
      n1,
      n2,
      events1,
      events2,
      rr,
      rr_lo,
      rr_hi,
      raw_p,
      fdr_p,
      fdr_significant_05,
      e_value_rr,
      e_value_ci_limit,
      hr,
      hr_lo,
      hr_hi
    ),
  file.path(base_dir, "evalue_fdr_results.csv")
)

publication_table <- results %>%
  transmute(
    Comparison = comparison_label,
    Outcome = outcome,
    `Events, exposed` = events1,
    `Events, reference` = events2,
    `RR (95% CI)` = rr_display,
    `Raw p` = raw_p_display,
    `BH-FDR p` = fdr_p_display,
    `FDR q<0.05` = if_else(fdr_significant_05, "Yes", "No"),
    `E-value (estimate; CI limit)` = e_value_display
  )

write_csv(publication_table, file.path(base_dir, "evalue_fdr_publication_table.csv"))

primary_outcomes <- c("Mood disorders", "Anxiety disorders", "ADHD", "Generalized SUD")
primary_table <- results %>%
  filter(outcome %in% primary_outcomes) %>%
  transmute(
    comparison,
    outcome,
    rr_ci = rr_display,
    e_value_rr = round(e_value_rr, 2),
    e_value_ci_limit = round(e_value_ci_limit, 2),
    raw_p = raw_p_display,
    fdr_p = fdr_p_display,
    fdr_significant_05
  )

write_csv(primary_table, file.path(base_dir, "primary_outcome_evalues_fdr.csv"))

opioid_cx3 <- results %>%
  filter(comparison == "CX3_vs_CX2", outcome == "Opioid-related") %>%
  slice(1)

non_fdr <- results %>%
  filter(!fdr_significant_05) %>%
  transmute(
    summary = sprintf(
      "%s | %s | raw p=%s | BH-FDR p=%s | RR=%s",
      comparison,
      outcome,
      raw_p_display,
      fdr_p_display,
      rr_display
    )
  ) %>%
  pull(summary)

opioid_flag <- if (nrow(opioid_cx3) == 1) {
  sprintf(
    paste0(
      "Opioid-related CX3 vs CX2 sparse-event flag: %s vs %s events; RR=%s; ",
      "raw log-rank p=%s; BH-FDR p=%s; FDR significant=%s. ",
      "Interpret as exploratory/fragile if q>=0.05."
    ),
    opioid_cx3$events1,
    opioid_cx3$events2,
    opioid_cx3$rr_display,
    opioid_cx3$raw_p_display,
    opioid_cx3$fdr_p_display,
    ifelse(opioid_cx3$fdr_significant_05, "yes", "no")
  )
} else {
  "Opioid-related CX3 vs CX2 row not found."
}

summary_lines <- c(
  "E-value and multiple-testing sensitivity analysis",
  "=================================================",
  "",
  "Methods:",
  "- E-values were computed from the reported post-matching relative risks using the VanderWeele-Ding risk-ratio formula.",
  "- For associations below the null (RR < 1), the RR scale was inverted before computing the E-value.",
  "- The CI-limit E-value uses the confidence-limit closest to the null; if the CI includes 1.0, the CI-limit E-value is 1.00.",
  "- Benjamini-Hochberg FDR correction was applied globally across all 24 outcome-comparison log-rank p-values.",
  "",
  "Key sparse-event flag:",
  paste0("- ", opioid_flag),
  "",
  "Cells not significant after BH-FDR q<0.05:",
  if (length(non_fdr) == 0) "- None" else paste0("- ", non_fdr),
  "",
  "Suggested manuscript language:",
  paste0(
    "We quantified sensitivity to unmeasured confounding using E-values for the observed risk ratios and their ",
    "confidence limits closest to the null. We additionally controlled the false-discovery rate across the 24 ",
    "outcome-comparison tests using the Benjamini-Hochberg procedure. The sparse opioid-related CX3 vs CX2 result ",
    "should be interpreted cautiously if it does not remain significant after FDR adjustment."
  )
)

writeLines(summary_lines, file.path(base_dir, "evalue_fdr_summary.txt"))
writeLines(capture.output(sessionInfo()), file.path(base_dir, "evalue_fdr_session_info.txt"))

cat("\nWrote:\n")
cat("- evalue_fdr_results.csv\n")
cat("- evalue_fdr_publication_table.csv\n")
cat("- primary_outcome_evalues_fdr.csv\n")
cat("- evalue_fdr_summary.txt\n")

cat("\n", opioid_flag, "\n", sep = "")

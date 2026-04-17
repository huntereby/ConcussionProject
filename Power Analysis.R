library(tidyverse)

main_dir <- "Results/New Results"
output_csv <- "power_analysis_results.csv"
output_power_plot_svg <- "power_analysis_figure.svg"
output_power_plot_png <- "power_analysis_figure.png"

outcome_labels <- c(
  "Outcome_1" = "Substance Use Disorder",
  "Outcome_2" = "Alcohol Dependence",
  "Outcome_3" = "Cannabis Dependence",
  "Outcome_4" = "Mood [Affective] Disorder",
  "Outcome_5" = "Anxiety Disorder",
  "Outcome_6" = "ADHD",
  "Outcome_7" = "Psychosis",
  "Outcome_8" = "Opioid Use Disorder"
)

parse_km_table <- function(file_path) {
  lines <- read_lines(file_path)

  outcome_id <- str_extract(basename(file_path), "Outcome_\\d+")
  cohort_file <- str_extract(file_path, "Concuss_\\d+x")
  time_window <- case_when(
    str_detect(file_path, "1_year") ~ "1 year",
    str_detect(file_path, "5_year") ~ "5 year",
    str_detect(file_path, "3month") ~ "3 month",
    str_detect(file_path, "9month") ~ "9 month",
    TRUE ~ NA_character_
  )

  cohort_header_idx <- which(str_detect(lines, "^Cohort,Cohort Name,Patients in Cohort,Patients with Outcome"))
  hr_header_idx <- which(str_detect(lines, "^Hazard Ratio,95 % CI Lower,95 % CI Upper"))

  if (length(cohort_header_idx) == 0 || length(hr_header_idx) == 0) {
    return(tibble())
  }

  cohort1 <- str_split(lines[cohort_header_idx[1] + 1], ",", simplify = TRUE)
  cohort2 <- str_split(lines[cohort_header_idx[1] + 2], ",", simplify = TRUE)
  hr_vals <- str_split(lines[hr_header_idx[1] + 1], ",", simplify = TRUE)

  if (ncol(cohort1) < 4 || ncol(cohort2) < 4 || ncol(hr_vals) < 3) {
    return(tibble())
  }

  n1 <- suppressWarnings(as.numeric(cohort1[1, 3]))
  n2 <- suppressWarnings(as.numeric(cohort2[1, 3]))
  e1 <- suppressWarnings(as.numeric(cohort1[1, 4]))
  e2 <- suppressWarnings(as.numeric(cohort2[1, 4]))
  hr <- suppressWarnings(as.numeric(hr_vals[1, 1]))
  ci_lo <- suppressWarnings(as.numeric(hr_vals[1, 2]))
  ci_hi <- suppressWarnings(as.numeric(hr_vals[1, 3]))

  if (any(is.na(c(n1, n2, e1, e2, hr, ci_lo, ci_hi)))) {
    return(tibble())
  }

  d <- e1 + e2
  p_treat <- n1 / (n1 + n2)

  if (d <= 0 || p_treat <= 0 || p_treat >= 1 || hr <= 0) {
    return(tibble())
  }

  alpha <- 0.05
  z_alpha <- qnorm(1 - alpha / 2)
  information <- sqrt(d * p_treat * (1 - p_treat))
  mu <- abs(log(hr)) * information

  # Two-sided Wald test power for Cox model approximation
  achieved_power <- pnorm(-z_alpha + mu) + pnorm(-z_alpha - mu)

  mdhr <- function(target_power) {
    z_beta <- qnorm(target_power)
    exp((z_alpha + z_beta) / information)
  }

  tibble(
    Outcome = outcome_id,
    OutcomeLabel = recode(outcome_id, !!!outcome_labels),
    CohortFile = cohort_file,
    TimeWindow = time_window,
    N_Cohort1 = n1,
    N_Cohort2 = n2,
    Events_Cohort1 = e1,
    Events_Cohort2 = e2,
    Total_Events = d,
    Hazard_Ratio = hr,
    CI_Lower = ci_lo,
    CI_Upper = ci_hi,
    Achieved_Power = achieved_power,
    MDHR_80pct = mdhr(0.80),
    MDHR_90pct = mdhr(0.90)
  )
}

km_files <- list.files(
  path = main_dir,
  pattern = "Outcome_\\d+_Result_b_KM_table\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(km_files) == 0) {
  stop("No KM tables found under: ", main_dir)
}

power_results <- map_dfr(km_files, parse_km_table) %>%
  mutate(
    CohortLabel = recode(
      CohortFile,
      "Concuss_1x" = "1x Concussion",
      "Concuss_2x" = "2x Concussion",
      "Concuss_3x" = "3x Concussion"
    ),
    Power_Pct = round(Achieved_Power * 100, 1),
    Across_1_00 = CI_Lower <= 1 & CI_Upper >= 1
  ) %>%
  select(
    Outcome,
    OutcomeLabel,
    CohortLabel,
    TimeWindow,
    N_Cohort1,
    N_Cohort2,
    Events_Cohort1,
    Events_Cohort2,
    Total_Events,
    Hazard_Ratio,
    CI_Lower,
    CI_Upper,
    Power_Pct,
    MDHR_80pct,
    MDHR_90pct,
    Across_1_00
  ) %>%
  arrange(Outcome, CohortLabel)

if (nrow(power_results) == 0) {
  stop("Parsing completed but no valid rows were produced.")
}

write_csv(power_results, output_csv)

# Figure: achieved power (%) by outcome and concussion cohort
plot_df <- power_results %>%
  mutate(
    OutcomeLabel = factor(
      OutcomeLabel,
      levels = c(
        "Substance Use Disorder",
        "Alcohol Dependence",
        "Cannabis Dependence",
        "Opioid Use Disorder",
        "Mood [Affective] Disorder",
        "Anxiety Disorder",
        "ADHD",
        "Psychosis"
      )
    ),
    CohortLabel = factor(
      CohortLabel,
      levels = c("1x Concussion", "2x Concussion", "3x Concussion")
    )
  )

power_plot <- ggplot(
  plot_df,
  aes(x = OutcomeLabel, y = Power_Pct, color = CohortLabel, group = CohortLabel)
) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "gray45") +
  geom_point(size = 2.6, position = position_dodge(width = 0.5)) +
  geom_line(linewidth = 0.8, position = position_dodge(width = 0.5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  labs(
    title = "Achieved Power by Outcome and Concussion Cohort",
    x = NULL,
    y = "Power (%)",
    color = "Cohort"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(output_power_plot_svg, power_plot, width = 11, height = 6.5, units = "in")
ggsave(output_power_plot_png, power_plot, width = 11, height = 6.5, units = "in", dpi = 300)

print(power_results, n = nrow(power_results))
message("Saved: ", output_csv)
message("Saved: ", output_power_plot_svg)
message("Saved: ", output_power_plot_png)

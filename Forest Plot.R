library(tidyverse)

main_dir <- "Results/New Results"
output_svg <- "forest_plot_new_results.svg"
output_png <- "forest_plot_new_results.png"

 
outcome_labels <- c(
  "Outcome_1" = "Substance Use Disorder",
  "Outcome_2" = "Alcohol Dependence",
  "Outcome_3" = "Cannabis Dependence",
  "Outcome_4" = "Mood [Affective] Disorder",
  "Outcome_5" = "Anxiety Disorder",
  "Outcome_6" = "ADHD"
)

extract_moa_info <- function(file_path) {
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

  rr_header_idx <- which(str_detect(lines, "^Risk Ratio,95 % CI Lower,95 % CI Upper"))
  if (length(rr_header_idx) == 0 || rr_header_idx[1] >= length(lines)) {
    return(tibble())
  }

  values_line <- lines[rr_header_idx[1] + 1]
  values <- str_split(values_line, ",", simplify = TRUE)
  if (ncol(values) < 3) {
    return(tibble())
  }

  tibble(
    Outcome = outcome_id,
    CohortFile = cohort_file,
    TimeWindow = time_window,
    `Risk Ratio` = suppressWarnings(as.numeric(values[1, 1])),
    `95 % CI Lower` = suppressWarnings(as.numeric(values[1, 2])),
    `95 % CI Upper` = suppressWarnings(as.numeric(values[1, 3]))
  )
}

moa_files <- list.files(
  path = main_dir,
  pattern = "Outcome_\\d+_Result_a_MOA_table\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(moa_files) == 0) {
  stop("No MOA table files found under: ", main_dir)
}

plot_df <- map_dfr(moa_files, extract_moa_info) %>%
  filter(!is.na(`Risk Ratio`), !is.na(`95 % CI Lower`), !is.na(`95 % CI Upper`)) %>%
  filter(!Outcome %in% c("Outcome_7", "Outcome_8")) %>%
  mutate(
    OutcomeLabel = recode(Outcome, !!!outcome_labels),
    CohortLabel = recode(
      CohortFile,
      "Concuss_1x" = "1x Concussion",
      "Concuss_2x" = "2x Concussion",
      "Concuss_3x" = "3x Concussion"
    )
  ) %>%
  filter(!is.na(OutcomeLabel), !is.na(CohortLabel)) %>%
  mutate(
    OutcomeLabel = factor(
      OutcomeLabel,
      levels = c(
        "Substance Use Disorder", "Alcohol Dependence", "Cannabis Dependence",
        "Mood [Affective] Disorder", "Anxiety Disorder", "ADHD"
      )
    ),
    CohortLabel = factor(CohortLabel, levels = c("3x Concussion", "2x Concussion", "1x Concussion")),
    rr_ci = sprintf("RR %.2f (%.2f-%.2f)", `Risk Ratio`, `95 % CI Lower`, `95 % CI Upper`)
  ) %>%
  arrange(OutcomeLabel, CohortLabel)

if (nrow(plot_df) == 0) {
  stop("No valid rows parsed from KM tables in: ", main_dir)
}

lower_bound <- min(plot_df$`95 % CI Lower`, na.rm = TRUE)
upper_bound <- max(plot_df$`95 % CI Upper`, na.rm = TRUE)

x_min <- max(0.45, lower_bound * 0.9)
x_max <- upper_bound * 1.7

candidate_breaks <- c(0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12)
x_breaks <- candidate_breaks[candidate_breaks >= x_min & candidate_breaks <= x_max]
if (!1 %in% x_breaks) {
  x_breaks <- sort(unique(c(x_breaks, 1)))
}

plot_df <- plot_df %>%
  mutate(rr_label_x = x_max / 1.03)

p <- ggplot(plot_df, aes(x = `Risk Ratio`, y = CohortLabel)) +
  geom_errorbar(
    aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`),
    width = 0.15,
    orientation = "y",
    color = "gray35"
  ) +
  geom_point(size = 2.8, shape = 21, fill = "black") +
  geom_text(
    aes(x = rr_label_x, label = rr_ci),
    hjust = 1,
    size = 2.8
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ OutcomeLabel, ncol = 1, scales = "free_y", strip.position = "top") +
  scale_x_continuous(
    trans = "log2",
    breaks = x_breaks,
    limits = c(x_min, x_max)
  ) +
  labs(
    title = "Forest Plot of 5-Year Risk Ratios (Results/New Results)",
    x = "Risk Ratio (log2 scale, 95% CI)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.35, "lines"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(6, 6, 6, 6)
    
  )
p
ggsave(output_svg, p, width = 7.6, height = 9.2, units = "in")
ggsave(output_png, p, width = 7.6, height = 9.2, units = "in", dpi = 300)

message("Saved: ", output_svg)
message("Saved: ", output_png)

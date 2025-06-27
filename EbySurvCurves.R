# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)

# Path to the folder that contains the "Concuss_*" result directories
main_dir <- "Results"

# Identify only the 5-year result folders for 1x, 2x and 3x concussion
concuss_dirs <- list.dirs(main_dir, recursive = FALSE, full.names = TRUE)
concuss_dirs <- concuss_dirs[grepl("Concuss_[123]x_5_year", concuss_dirs)]

# Helper to read a single Kaplan-Meier CSV and return a tidy data frame
read_km_file <- function(dir, outcome_id) {
  file <- file.path(dir, sprintf("Outcome_%d_Result_b_KM_graph.csv", outcome_id))
  df <- read_csv(file, skip = 9, show_col_types = FALSE)
  
  # Label derived from folder name (e.g. "Concuss_1x" -> "1x Concussion")
  exposure_label <- sub("Concuss_([123]x).*", "\\1", basename(dir))
  exposure_label <- paste0(exposure_label, " Concussion")
  
  cohort1 <- df %>%
    select(Time = `Time (Days)`,
           Surv = `Cohort 1: Survival Probability`,
           Lower = `Cohort 1: Survival Probability 95 % CI Lower`,
           Upper = `Cohort 1: Survival Probability 95 % CI Upper`) %>%
    mutate(Cohort = "Concussion") %>%
    tidyr::fill(Surv, Lower, Upper, .direction = "downup") %>%
    mutate(
      Risk = 1 - Surv,
      LowerRisk = 1 - Upper,
      UpperRisk = 1 - Lower
    )
  
  cohort2 <- df %>%
    select(Time = `Time (Days)`,
           Surv = `Cohort 2: Survival Probability`,
           Lower = `Cohort 2: Survival Probability 95 % CI Lower`,
           Upper = `Cohort 2: Survival Probability 95 % CI Upper`) %>%
    mutate(Cohort = "Control") %>%
    tidyr::fill(Surv, Lower, Upper, .direction = "downup") %>%
    mutate(
      Risk = 1 - Surv,
      LowerRisk = 1 - Upper,
      UpperRisk = 1 - Lower
    )
  
  bind_rows(cohort1, cohort2) %>%
    mutate(Exposure = exposure_label,
           OutcomeID = outcome_id)
}

# Define outcome labels
outcome_labels <- c(
  `4` = "Mood Disorder",
  `5` = "Anxiety Disorder",
  `6` = "ADHD"
)

# Read all Kaplan-Meier data into one data frame
km_data <- map_dfr(concuss_dirs, function(d) {
  map_dfr(4:6, ~read_km_file(d, .x))
}) %>%
  mutate(Outcome = recode_factor(as.character(OutcomeID), !!!outcome_labels))

# Professional plot function without risk table
plot_survival <- function(df, outcome_label, pval = NA) {
  p <- ggplot(df, aes(x = Time / 365, y = Risk,
                      color = Exposure, linetype = Cohort)) +
    geom_ribbon(aes(ymin = LowerRisk, ymax = UpperRisk, fill = Exposure),
                alpha = 0.15, colour = NA, show.legend = FALSE) +
    geom_step(size = 1) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    scale_linetype_manual(values = c(Concussion = "solid", Control = "dashed")) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.30),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(
      breaks = seq(0, max(df$Time) / 365, by = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = outcome_label,
      x = "Time (years)",
      y = "Cumulative Incidence",
      color = "Exposure",
      linetype = "Cohort"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = element_text(size = 11),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.3, color = "gray85")
    )
  
  if (!is.na(pval) && is.numeric(pval)) {
    p <- p + annotate("text",
                      x = max(df$Time) / 365,
                      y = 0.28,
                      hjust = 1,
                      label = paste0("Log-rank p = ", signif(pval, 3)),
                      size = 3.2,
                      fontface = "italic")
  }
  
  return(p)
}

# Generate and save a plot for each outcome
for (lbl in unique(km_data$Outcome)) {
  df <- km_data %>% filter(Outcome == lbl)
  
  # Optional: insert actual p-value here
  pval <- NA
  
  p <- plot_survival(df, lbl, pval)
  ggsave(sprintf("survival_%sv4.svg", gsub(" ", "_", tolower(lbl))),
         plot = p, width = 7, height = 5, dpi = 300)
}

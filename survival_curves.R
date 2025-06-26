library(tidyverse)

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
    mutate(Cohort = "Concussion")

  cohort2 <- df %>%
    select(Time = `Time (Days)`,
           Surv = `Cohort 2: Survival Probability`,
           Lower = `Cohort 2: Survival Probability 95 % CI Lower`,
           Upper = `Cohort 2: Survival Probability 95 % CI Upper`) %>%
    mutate(Cohort = "Control")

  bind_rows(cohort1, cohort2) %>%
    mutate(Exposure = exposure_label,
           OutcomeID = outcome_id)
}

# Outcomes 4,5,6 correspond to Mood, Anxiety and ADHD
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

# Function to create a survival curve plot for a given outcome
plot_survival <- function(df, outcome_label) {
  ggplot(df, aes(x = Time / 365, y = Surv,
                 color = Exposure, linetype = Cohort)) +
    geom_step(size = 0.8) +
  csbjzh-codex/create-survival-curves-for-outcomes-4-5-6
    scale_linetype_manual(values = c(Concussion = "solid",
                                      Control = "longdash")) +
=======
main
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = outcome_label,
      x = "Time (years)",
      y = "Survival Probability",
      color = "Exposure",
      linetype = "Cohort"
    ) +
    theme_minimal()
}

# Generate and save a plot for each outcome
for (lbl in unique(km_data$Outcome)) {
  df <- km_data %>% filter(Outcome == lbl)
  p <- plot_survival(df, lbl)
  ggsave(sprintf("survival_%s.png", gsub(" ", "_", tolower(lbl))),
         plot = p, width = 7, height = 5, dpi = 300)
}

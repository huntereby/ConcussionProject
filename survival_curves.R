library(tidyverse)
library(survival)
library(survminer)
library(gridExtra)

# Path to the folder that contains the "Concuss_*" result directories
main_dir <- "Results"

# Identify only the 5-year result folders for 1x, 2x and 3x concussion
concuss_dirs <- list.dirs(main_dir, recursive = FALSE, full.names = TRUE)
concuss_dirs <- concuss_dirs[grepl("Concuss_[123]x_5_year", concuss_dirs)]

# Extract patient counts and p-value from the KM table CSV
read_km_meta <- function(dir, outcome_id) {
  meta_file <- file.path(dir, sprintf("Outcome_%d_Result_b_KM_table.csv", outcome_id))
  lines <- readLines(meta_file, warn = FALSE)
  header_idx <- grep("^Cohort,Cohort Name", lines)
  n1 <- as.numeric(strsplit(lines[header_idx + 1], ",")[[1]][3])
  n2 <- as.numeric(strsplit(lines[header_idx + 2], ",")[[1]][3])
  p_idx <- grep("^\u03c7\xb2,df,p", lines)
  pval <- as.numeric(strsplit(lines[p_idx + 1], ",")[[1]][3])
  list(n1 = n1, n2 = n2, pval = pval)
}

# Helper to read a single Kaplan-Meier CSV and return a tidy data frame
read_km_file <- function(dir, outcome_id) {
  file <- file.path(dir, sprintf("Outcome_%d_Result_b_KM_graph.csv", outcome_id))
  df <- read_csv(file, skip = 9, show_col_types = FALSE)
  meta <- read_km_meta(dir, outcome_id)

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
      UpperRisk = 1 - Lower,
      n0 = meta$n1,
      pval = meta$pval
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
      UpperRisk = 1 - Lower,
      n0 = meta$n2,
      pval = meta$pval
    )

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
calc_risk_table <- function(df, years = 0:5) {
  groups <- split(df, interaction(df$Exposure, df$Cohort))
  map_dfr(groups, function(g) {
    surv_vals <- approx(g$Time / 365, 1 - g$Risk, xout = years,
                        method = "constant", rule = 2)$y
    tibble(
      Exposure = g$Exposure[1],
      Cohort = g$Cohort[1],
      !!!set_names(round(surv_vals * g$n0[1]), paste0("Year", years))
    )
  })
}

plot_survival <- function(df, outcome_label) {
  pval <- unique(df$pval)

  main <- ggplot(df, aes(x = Time / 365, y = Risk,
                         color = Exposure, linetype = Cohort)) +
    geom_ribbon(aes(ymin = LowerRisk, ymax = UpperRisk, fill = Exposure),
                alpha = 0.2, colour = NA, show.legend = FALSE) +
    geom_step(size = 0.8) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    scale_linetype_manual(values = c(Concussion = "solid",
                                      Control = "longdash")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 0.30)) +
    labs(
      title = outcome_label,
      x = "Time (years)",
      y = "Cumulative Risk",
      color = "Exposure",
      linetype = "Cohort"
    ) +
    theme_minimal() +
    annotate("text", x = max(df$Time) / 365, y = 0.28, hjust = 1,
             label = paste0("Log-rank p = ", signif(pval, 3)), size = 3)

  tbl <- calc_risk_table(df) %>%
    arrange(Exposure, Cohort)
  tbl_grob <- tableGrob(tbl, rows = NULL, theme = ttheme_minimal(base_size = 8))

  gridExtra::arrangeGrob(main, tbl_grob, heights = c(4, 1))
}

# Generate and save a plot for each outcome
for (lbl in unique(km_data$Outcome)) {
  df <- km_data %>% filter(Outcome == lbl)
  g <- plot_survival(df, lbl)
  ggsave(sprintf("survival_%s.png", gsub(" ", "_", tolower(lbl))),
         plot = g, width = 7, height = 5, dpi = 300)
}

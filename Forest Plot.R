library(tidyverse)

# Define your parent directory that holds all "Concuss_Xx..." folders
main_dir <- "Results"

library(tidyverse)

# Set your top-level folder
main_dir <- "path/to/your/folder"

# Find all outcome result files recursively
outcome_files <- list.files(
  path = main_dir,
  pattern = "Outcome_\\d+_Result_a_MOA_table\\.csv",
  recursive = TRUE,
  full.names = TRUE
)

# Function to extract cohort stats, risk ratio, and time window
extract_outcome_info <- function(file) {
  lines <- readLines(file)
  
  # Extract tags from file path
  outcome <- str_extract(basename(file), "Outcome_\\d+")
  cohort_name <- str_extract(file, "Concuss_\\d+x")
  time_window <- case_when(
    str_detect(file, "1_year") ~ "1 year",
    str_detect(file, "5_year") ~ "5 year",
    str_detect(file, "3month") ~ "3 month",
    str_detect(file, "9month") ~ "9 month",
    TRUE ~ NA_character_
  )
  
  # Extract Cohort Statistics
  cohort_start <- which(str_detect(lines, "Cohort,Cohort Name"))
  cohort_block <- c(
    lines[cohort_start],
    lines[(cohort_start + 1):(cohort_start + 2)]
  )
  cohort_df <- read_csv(paste(cohort_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  # Extract Risk Ratio
  rr_start <- which(str_detect(lines, "^Risk Ratio,"))
  rr_block <- c(lines[rr_start], lines[rr_start + 1])
  rr_df <- read_csv(paste(rr_block, collapse = "\n"), show_col_types = FALSE) %>%
    mutate(Row = row_number())
  
  # Combine and annotate
  left_join(cohort_df, rr_df, by = "Row") %>%
    select(-Row) %>%
    mutate(
      Outcome = outcome,
      CohortFile = cohort_name,
      TimeWindow = time_window
    )
}

# Apply across all files
outcome_combined <- map_dfr(outcome_files, extract_outcome_info)

# Inspect the result
glimpse(outcome_combined)





outcome_combined <- outcome_combined %>%
  mutate(OutcomeLabel = case_when(
    Outcome == "Outcome_1" ~ "Substance Use Disorder",
    Outcome == "Outcome_2" ~ "Alcohol Dependence",
    Outcome == "Outcome_3" ~ "Cannabis Dependence",
    Outcome == "Outcome_4" ~ "Mood [Affective] Disorder",
    Outcome == "Outcome_5" ~ "Anxiety Disorder",
    Outcome == "Outcome_6" ~ "ADHD",
    Outcome == "Outcome_7" ~ "Psychosis",
    Outcome == "Outcome_8" ~ "Opioid Disorder",
    TRUE ~ NA_character_
  ))


library(tidyverse)
library(tidyverse)wri

# Prepare data: filter to 5-year only and drop NA
forest_data <- outcome_combined %>%
  filter(TimeWindow == "5 year") %>%
  filter(`Cohort Name` != "Unnamed", !is.na(`Risk Ratio`)) %>%
  mutate(
    OutcomeLabel = factor(OutcomeLabel,
                          levels = c(
                            "Substance Use Disorder", "Alcohol Dependence", "Cannabis Dependence",
                            "Opioid Disorder", "Mood [Affective] Disorder", "Anxiety Disorder",
                            "ADHD", "Psychosis"
                          )
    ),
    CohortLabel = recode(CohortFile,
                         "Concuss_1x" = "1x Concussion",
                         "Concuss_2x" = "2x Concussion",
                         "Concuss_3x" = "3x Concussion"
    ),
    Group = OutcomeLabel,
    Label = paste0("  ", CohortLabel)
  ) %>%
  arrange(OutcomeLabel, CohortFile)

# Create dummy "Reference" row for each outcome
ref_rows <- forest_data %>%
  group_by(Group) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    `Risk Ratio` = 1,
    `95 % CI Lower` = 1,
    `95 % CI Upper` = 1,
    Label = "Reference",
    is_ref = TRUE
  )

# Add a flag and combine
forest_data <- forest_data %>%
  mutate(is_ref = FALSE)

plot_df <- bind_rows(forest_data, ref_rows) %>%
  arrange(Group, desc(is_ref), CohortLabel) %>%
  mutate(
    Label = fct_inorder(Label),
    Group = fct_inorder(Group)
  )

# Create the plot
ggplot(plot_df, aes(x = `Risk Ratio`, y = Label)) +
  geom_point(aes(color = is_ref), size = 3, shape = 21, fill = "black") +
  geom_errorbarh(
    aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`),
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Group, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8)) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(
    title = "Forest Plot of 5-Year Risk Ratios by Outcome and Concussion Count",
    x = "Risk Ratio (log scale, 95% CI)"
  )

install.packages("svglite")  # needed for SVG output
library(svglite)
library(ggplot2)
ggsave("forest_plot.svg", plot = last_plot(), width = 8, height = 10, units = "in")


plot_df <- plot_df %>%
  mutate(
    Label = factor(Label, levels = c("  1x Concussion", "  2x Concussion", "  3x Concussion", "Reference")),
    Label = fct_rev(Label),  # so plot shows 1x on top
    Group = fct_inorder(Group)
  )


ggplot(plot_df, aes(x = `Risk Ratio`, y = Label)) +
  geom_point(aes(color = is_ref), size = 3, shape = 21, fill = "black") +
  geom_errorbarh(aes(xmin = `95 % CI Lower`, xmax = `95 % CI Upper`), height = 0.2) +
  geom_text(aes(label = rr_ci), hjust = -0.1, size = 3.2) +  # annotate on the right
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ OutcomeLabel, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_x_continuous(trans = "log2", breaks = c(0.5, 1, 2, 4, 8), expand = expansion(mult = c(0.01, 0.4))) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1, "lines")
  ) +
  labs(
    title = "Forest Plot of 5-Year Risk Ratios by Outcome and Concussion Count",
    x = "Risk Ratio (log scale, 95% CI)"
  )

ggsave("forest_plot3.0.svg", plot = last_plot(), width = 8, height = 10, units = "in")

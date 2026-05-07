# R version: 4.3+ recommended
# Required packages: readr, dplyr, tidyr, ggplot2, scales

set.seed(20260422)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

base_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(base_dir)
figure_family <- "Helvetica"

outcome_map <- tibble::tribble(
  ~outcome_number, ~outcome_label,              ~short_label,
  4L,              "Mood disorders (F30-F39)", "Mood disorders",
  5L,              "Anxiety disorders (F40-F48)", "Anxiety disorders",
  1L,              "Generalized SUD (F10-F19)", "Generalized SUD"
)

cohort_colors <- c("CX1" = "#173f8a", "CTL-FX" = "#8d99ae")

clean_lines <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  trimws(lines)
}

extract_graph_data <- function(path) {
  lines <- clean_lines(path)
  idx <- match("Time (Days),Cohort 1: Survival Probability,Cohort 1: Survival Probability 95 % CI Lower,Cohort 1: Survival Probability 95 % CI Upper,Cohort 2: Survival Probability,Cohort 2: Survival Probability 95 % CI Lower,Cohort 2: Survival Probability 95 % CI Upper", lines)
  if (is.na(idx)) {
    stop(sprintf("Graph data header not found in %s.", path), call. = FALSE)
  }
  text <- paste(c(lines[idx], lines[(idx + 1):length(lines)]), collapse = "\n")
  read.csv(text = text, check.names = FALSE, stringsAsFactors = FALSE)
}

extract_km_table <- function(path) {
  lines <- clean_lines(path)
  idx <- match("Hazard Ratio", lines)
  hr_text <- paste(lines[(idx + 1):(idx + 2)], collapse = "\n")
  hr_df <- read.csv(text = hr_text, check.names = FALSE, stringsAsFactors = FALSE)

  logrank_idx <- match("Log-Rank Test", lines)
  logrank_text <- paste(lines[(logrank_idx + 1):(logrank_idx + 2)], collapse = "\n")
  logrank_df <- read.csv(text = logrank_text, check.names = FALSE, stringsAsFactors = FALSE)

  cohort_idx <- match("Cohort Statistics", lines)
  cohort_text <- paste(lines[(cohort_idx + 1):(cohort_idx + 3)], collapse = "\n")
  cohort_df <- read.csv(text = cohort_text, check.names = FALSE, stringsAsFactors = FALSE)

  list(hr = hr_df, logrank = logrank_df, cohort = cohort_df)
}

km_df <- outcome_map %>%
  split(.$outcome_number) %>%
  purrr::map_dfr(function(row_df) {
    outcome_number <- row_df$outcome_number[[1]]
    graph_path <- file.path(base_dir, "Concuss_1x_5_year_V12_424528", sprintf("Outcome_%s_Result_b_KM_graph.csv", outcome_number))
    table_path <- file.path(base_dir, "Concuss_1x_5_year_V12_424528", sprintf("Outcome_%s_Result_b_KM_table.csv", outcome_number))

    graph_df <- extract_graph_data(graph_path)
    meta <- extract_km_table(table_path)

    wide_df <- tibble(
      time_days = graph_df$`Time (Days)`,
      surv_cx1 = graph_df$`Cohort 1: Survival Probability`,
      surv_cx1_lo = graph_df$`Cohort 1: Survival Probability 95 % CI Lower`,
      surv_cx1_hi = graph_df$`Cohort 1: Survival Probability 95 % CI Upper`,
      surv_ctl = graph_df$`Cohort 2: Survival Probability`,
      surv_ctl_lo = graph_df$`Cohort 2: Survival Probability 95 % CI Lower`,
      surv_ctl_hi = graph_df$`Cohort 2: Survival Probability 95 % CI Upper`
    ) %>%
      bind_rows(
        tibble(
          time_days = 0,
          surv_cx1 = 1, surv_cx1_lo = 1, surv_cx1_hi = 1,
          surv_ctl = 1, surv_ctl_lo = 1, surv_ctl_hi = 1
        ),
        .
      ) %>%
      arrange(time_days) %>%
      tidyr::fill(everything(), .direction = "down") %>%
      mutate(
        outcome = row_df$outcome_label[[1]],
        hr_label = sprintf("HR %.2f (%.2f-%.2f)", meta$hr$`Hazard Ratio`[1], meta$hr$`95 % CI Lower`[1], meta$hr$`95 % CI Upper`[1]),
        p_label = ifelse(meta$logrank$p[1] < 0.001, "Log-rank p<0.001", sprintf("Log-rank p=%.3f", meta$logrank$p[1]))
      )

    long_df <- bind_rows(
      wide_df %>%
        transmute(
          time_days,
          outcome,
          hr_label,
          p_label,
          cohort = "CX1",
          surv = surv_cx1,
          lo = surv_cx1_lo,
          hi = surv_cx1_hi
        ),
      wide_df %>%
        transmute(
          time_days,
          outcome,
          hr_label,
          p_label,
          cohort = "CTL-FX",
          surv = surv_ctl,
          lo = surv_ctl_lo,
          hi = surv_ctl_hi
        )
    ) %>%
      mutate(
        cuminc = 1 - surv,
        cuminc_lo = pmax(0, 1 - hi),
        cuminc_hi = pmin(1, 1 - lo),
        time_years = time_days / 365.25
      )

    long_df
  }) %>%
  mutate(
    outcome = factor(outcome, levels = outcome_map$outcome_label),
    cohort = factor(cohort, levels = c("CTL-FX", "CX1"))
  )

panel_annotations <- km_df %>%
  group_by(outcome) %>%
  summarise(
    hr_label = first(hr_label),
    p_label = first(p_label),
    .groups = "drop"
  )

figure_km <- ggplot(km_df, aes(x = time_years, y = cuminc, color = cohort, fill = cohort)) +
  geom_ribbon(aes(ymin = cuminc_lo, ymax = cuminc_hi), alpha = 0.12, linewidth = 0, show.legend = FALSE) +
  geom_step(linewidth = 0.9) +
  facet_wrap(~ outcome, nrow = 1) +
  geom_text(
    data = panel_annotations,
    aes(x = 0.12, y = 0.23, label = hr_label),
    inherit.aes = FALSE,
    hjust = 0,
    family = figure_family,
    size = 3.2,
    color = "#333333"
  ) +
  geom_text(
    data = panel_annotations,
    aes(x = 0.12, y = 0.20, label = p_label),
    inherit.aes = FALSE,
    hjust = 0,
    family = figure_family,
    size = 3.1,
    color = "#555555"
  ) +
  scale_color_manual(values = cohort_colors) +
  scale_fill_manual(values = cohort_colors) +
  scale_x_continuous(
    limits = c(0, 5),
    breaks = 0:5,
    labels = c("0", "1", "2", "3", "4", "5")
  ) +
  scale_y_continuous(
    limits = c(0, 0.24),
    breaks = seq(0, 0.24, by = 0.04),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    x = "Years since index event",
    y = "Cumulative incidence",
    title = "Five-year cumulative incidence after one concussion vs forearm-fracture controls",
    subtitle = "Flipped Kaplan-Meier curves shown as 1 - survival probability",
    color = NULL
  ) +
  theme_classic(base_size = 10.5, base_family = figure_family) +
  theme(
    strip.background = element_rect(fill = "#f3f5f8", color = "#d6dbe3", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "top",
    legend.justification = "left",
    legend.box.just = "left",
    plot.title = element_text(face = "bold", size = 11.5),
    plot.subtitle = element_text(size = 9.5),
    panel.spacing = unit(8, "mm"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

save_plot <- function(stem, plot_obj, width_in = 10.8, height_in = 3.9, dpi = 300) {
  ggsave(filename = paste0(stem, ".svg"), plot = plot_obj, width = width_in, height = height_in, units = "in", device = "svg")
  ggsave(filename = paste0(stem, ".pdf"), plot = plot_obj, width = width_in, height = height_in, units = "in", device = grDevices::cairo_pdf)
  ggsave(filename = paste0(stem, ".png"), plot = plot_obj, width = width_in, height = height_in, units = "in", dpi = dpi, bg = "white")
}

write_csv(km_df, file.path(base_dir, "km_flipped_1x_data.csv"))
save_plot(file.path(base_dir, "figure_km_flipped_1x"), figure_km)

writeLines(capture.output(sessionInfo()), file.path(base_dir, "km_flipped_1x_session_info.txt"))

cat("Saved outputs:\n")
cat("- figure_km_flipped_1x.svg/.pdf/.png\n")
cat("- km_flipped_1x_data.csv\n")

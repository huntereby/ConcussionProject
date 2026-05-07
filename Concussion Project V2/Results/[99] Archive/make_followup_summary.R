# R version: 4.3+ recommended
# Required packages: readr, dplyr, stringr, ggplot2, grid, gridExtra, gtable, svglite, scales

set.seed(20260421)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(gtable)
  library(svglite)
  library(scales)
})

base_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(base_dir)
figure_family <- "Helvetica"

comparison_map <- tibble::tribble(
  ~comparison,       ~dir_name,                              ~cohort1_label, ~cohort2_label, ~comparison_label,                      ~accent,
  "CX1_vs_CTLFX",    "Concuss_1x_5_year_V12_861501",         "CX1",          "CTL-FX",       "CX1 vs CTL-FX (n=68,375 per arm)",    "#173f8a",
  "CX2_vs_CX1",      "Concuss_2x_5_year_V12_(1)_(1)_820358", "CX2",          "CX1",          "CX2 vs CX1 (n=21,501 per arm)",       "#0d8785",
  "CX3_vs_CX2",      "Concuss_3x_5_year_V12_137070",         "CX3",          "CX2",          "CX3 vs CX2 (n=8,722 per arm)",        "#7a3e96"
)

clean_lines <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  trimws(lines)
}

extract_section_table <- function(lines, section_name, n_lines) {
  idx <- match(section_name, lines)
  if (is.na(idx)) {
    stop(sprintf("Section '%s' not found in %s.", section_name, paste(lines[1], collapse = "")), call. = FALSE)
  }
  text <- paste(lines[(idx + 1):(idx + n_lines)], collapse = "\n")
  utils::read.csv(text = text, check.names = FALSE, stringsAsFactors = FALSE)
}

parse_followup_table <- function(path) {
  lines <- clean_lines(path)
  extract_section_table(lines, "After Matching Follow-up Time Results", 3)
}

parse_followup_graph <- function(path) {
  lines <- clean_lines(path)
  idx <- match("Time (Days),Cohort1: Patients,Cohort2: Patients", lines)
  if (is.na(idx)) {
    stop(sprintf("Graph data table not found in %s.", path), call. = FALSE)
  }
  text <- paste(c(lines[idx], lines[(idx + 1):length(lines)]), collapse = "\n")
  utils::read.csv(text = text, check.names = FALSE, stringsAsFactors = FALSE)
}

followup_summary <- comparison_map %>%
  split(.$comparison) %>%
  purrr::map_dfr(function(comp_row) {
    table_path <- file.path(base_dir, comp_row$dir_name[[1]], "Follow_up_time_metrics_matched_results_table.csv")
    graph_path <- file.path(base_dir, comp_row$dir_name[[1]], "Follow_up_time_metrics_matched_results_graph.csv")

    table_df <- parse_followup_table(table_path)
    graph_df <- parse_followup_graph(graph_path)
    full_window <- graph_df %>% filter(`Time (Days)` == 1825)

    tibble(
      comparison = comp_row$comparison[[1]],
      comparison_label = comp_row$comparison_label[[1]],
      cohort = c(comp_row$cohort1_label[[1]], comp_row$cohort2_label[[1]]),
      mean_followup_days = c(table_df$`Mean Follow-up (Days)`[1], table_df$`Mean Follow-up (Days)`[2]),
      sd_days = c(table_df$`Standard Deviation`[1], table_df$`Standard Deviation`[2]),
      median_followup_days = c(table_df$`Median Follow-up (Days)`[1], table_df$`Median Follow-up (Days)`[2]),
      iqr_days = c(table_df$`Interquartile Range`[1], table_df$`Interquartile Range`[2]),
      full_1825d_n = c(full_window$`Cohort1: Patients`[1], full_window$`Cohort2: Patients`[1]),
      total_n = c(sum(graph_df$`Cohort1: Patients`), sum(graph_df$`Cohort2: Patients`)),
      accent = comp_row$accent[[1]]
    )
  }) %>%
  mutate(
    full_1825d_pct = full_1825d_n / total_n,
    mean_followup_years = mean_followup_days / 365.25,
    full_1825d_label = sprintf("%s (%.2f%%)", format(full_1825d_n, big.mark = ",", trim = TRUE), full_1825d_pct * 100)
  )

write_csv(followup_summary, file.path(base_dir, "followup_metrics_comparison_table.csv"))

table_display <- followup_summary %>%
  transmute(
    Comparison = comparison_label,
    Cohort = cohort,
    `Mean follow-up, d` = sprintf("%.2f", mean_followup_days),
    `SD, d` = sprintf("%.2f", sd_days),
    `Median, d` = sprintf("%.0f", median_followup_days),
    `IQR, d` = sprintf("%.0f", iqr_days),
    `Full 1,825-d follow-up` = full_1825d_label
  )

table_theme <- gridExtra::ttheme_minimal(
  base_size = 11.5,
  base_family = figure_family,
  core = list(
    fg_params = list(hjust = 0, x = 0.02, fontsize = 10.8, lineheight = 1.15, col = "#111111"),
    bg_params = list(fill = rep(c("#f7f8fb", "white"), length.out = nrow(table_display)), col = NA),
    padding = unit(c(6, 6), "mm")
  ),
  colhead = list(
    fg_params = list(fontface = "bold", fontsize = 11.4, col = "#111111"),
    bg_params = list(fill = "#e9eef6", col = NA),
    padding = unit(c(7, 6), "mm")
  )
)

table_grob <- gridExtra::tableGrob(table_display, rows = NULL, theme = table_theme)
table_grob$widths <- unit(c(2.55, 0.9, 1.2, 0.95, 1.0, 0.8, 1.9), "in")

title_grob <- textGrob(
  "Table S1. Matched Follow-up Metrics Across Pairwise Comparisons",
  x = 0.01,
  hjust = 0,
  gp = gpar(fontfamily = figure_family, fontface = "bold", fontsize = 16.5, col = "#111111")
)

subtitle_grob <- textGrob(
  "Follow-up time metrics after 1:1 propensity score matching. Full 1,825-day follow-up indicates the number and percentage of patients observed through the full 5-year window.",
  x = 0.01,
  hjust = 0,
  gp = gpar(fontfamily = figure_family, fontsize = 10.6, col = "#444444")
)

footnote_grob <- textGrob(
  "IQR indicates interquartile range; SD, standard deviation.",
  x = 0.01,
  hjust = 0,
  gp = gpar(fontfamily = figure_family, fontsize = 9.5, col = "#555555")
)

full_table_grob <- arrangeGrob(
  title_grob,
  subtitle_grob,
  table_grob,
  footnote_grob,
  ncol = 1,
  heights = unit.c(unit(0.38, "in"), unit(0.44, "in"), sum(table_grob$heights) + unit(0.10, "in"), unit(0.24, "in"))
)

save_grob_outputs <- function(stem, grob_obj, width_in, height_in, dpi = 300) {
  svglite::svglite(filename = paste0(stem, ".svg"), width = width_in, height = height_in, bg = "white")
  grid.newpage()
  grid.draw(grob_obj)
  dev.off()

  grDevices::cairo_pdf(file = paste0(stem, ".pdf"), width = width_in, height = height_in, bg = "white")
  grid.newpage()
  grid.draw(grob_obj)
  dev.off()

  grDevices::png(filename = paste0(stem, ".png"), width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
  grid.newpage()
  grid.draw(grob_obj)
  dev.off()
}

save_plot_outputs <- function(stem, plot_obj, width_in, height_in, dpi = 300) {
  ggsave(filename = paste0(stem, ".svg"), plot = plot_obj, width = width_in, height = height_in, units = "in", device = "svg")
  ggsave(filename = paste0(stem, ".pdf"), plot = plot_obj, width = width_in, height = height_in, units = "in", device = grDevices::cairo_pdf)
  ggsave(filename = paste0(stem, ".png"), plot = plot_obj, width = width_in, height = height_in, units = "in", dpi = dpi, bg = "white")
}

save_grob_outputs(file.path(base_dir, "table_followup_metrics"), full_table_grob, width_in = 10.4, height_in = 4.8)

figure_df <- followup_summary %>%
  mutate(
    comparison_label = factor(comparison_label, levels = comparison_map$comparison_label),
    cohort_order = case_when(
      comparison == "CX1_vs_CTLFX" & cohort == "CTL-FX" ~ 1L,
      comparison == "CX1_vs_CTLFX" & cohort == "CX1" ~ 2L,
      comparison == "CX2_vs_CX1" & cohort == "CX1" ~ 1L,
      comparison == "CX2_vs_CX1" & cohort == "CX2" ~ 2L,
      comparison == "CX3_vs_CX2" & cohort == "CX2" ~ 1L,
      comparison == "CX3_vs_CX2" & cohort == "CX3" ~ 2L,
      TRUE ~ 99L
    )
  )

figure_followup <- ggplot(figure_df, aes(x = cohort, group = comparison)) +
  geom_col(
    aes(y = full_1825d_pct, fill = comparison),
    width = 0.58,
    position = position_dodge(width = 0.68),
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(y = full_1825d_pct, label = percent(full_1825d_pct, accuracy = 0.1), color = comparison),
    position = position_dodge(width = 0.68),
    vjust = -0.6,
    size = 3.2,
    family = figure_family,
    show.legend = FALSE
  ) +
  geom_point(
    aes(y = mean_followup_days / 1825, color = comparison),
    position = position_dodge(width = 0.68),
    size = 2.9,
    stroke = 0.8
  ) +
  facet_wrap(~ comparison_label, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = setNames(comparison_map$accent, comparison_map$comparison), guide = "none") +
  scale_color_manual(values = setNames(comparison_map$accent, comparison_map$comparison), guide = "none") +
  scale_y_continuous(
    limits = c(0, 1.04),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Proportion of matched cohort",
    title = "Matched follow-up completeness across pairwise comparisons",
    subtitle = "Bars show the proportion of each matched cohort observed through the full 1,825-day window; points indicate mean follow-up as a fraction of 1,825 days"
  ) +
  theme_classic(base_size = 10.5, base_family = figure_family) +
  theme(
    strip.background = element_rect(fill = "#f3f5f8", color = "#d6dbe3", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 11.5),
    plot.subtitle = element_text(size = 9.5),
    axis.text.x = element_text(size = 9),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

save_plot_outputs(file.path(base_dir, "figure_followup_metrics"), figure_followup, width_in = 10.4, height_in = 3.9)

writeLines(capture.output(sessionInfo()), file.path(base_dir, "followup_summary_session_info.txt"))

cat("Saved outputs:\n")
cat("- followup_metrics_comparison_table.csv\n")
cat("- table_followup_metrics.svg/.pdf/.png\n")
cat("- figure_followup_metrics.svg/.pdf/.png\n")

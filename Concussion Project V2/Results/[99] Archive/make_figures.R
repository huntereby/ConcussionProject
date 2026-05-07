# R version: 4.3+ recommended
# Required packages: ggplot2, dplyr, readr, stringr, purrr, scales, grid, gtable
# This script parses TriNetX exports, writes results.csv, produces manuscript
# figures, writes power_table.csv, saves session_info.txt, and prints a power
# summary to stdout.

set.seed(20260421)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(scales)
  library(grid)
  library(gtable)
  library(patchwork)
})

base_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(base_dir)
figure_family <- "Helvetica"

comparison_map <- tibble::tribble(
  ~comparison,       ~dir_name,                                ~cohort1_label, ~cohort2_label, ~comparison_label,
  "CX1_vs_CTLFX",    "Concuss_1x_5_year_V12_861501",           "CX1",          "CTL-FX",       "CX1 vs CTL-FX (n=68,375 per arm)",
  "CX2_vs_CX1",      "Concuss_2x_5_year_V12_(1)_(1)_820358",   "CX2",          "CX1",          "CX2 vs CX1 (n=21,501 per arm)",
  "CX3_vs_CX2",      "Concuss_3x_5_year_V12_137070",           "CX3",          "CX2",          "CX3 vs CX2 (n=8,722 per arm)"
)

comparison_strip_labels <- c(
  "CX1 vs CTL-FX (n=68,375 per arm)" = "CX1 vs CTL-FX\n(n=68,375 per arm)",
  "CX2 vs CX1 (n=21,501 per arm)" = "CX2 vs CX1\n(n=21,501 per arm)",
  "CX3 vs CX2 (n=8,722 per arm)" = "CX3 vs CX2\n(n=8,722 per arm)"
)

comparison_axis_labels <- c(
  "CX1 vs CTL-FX (n=68,375 per arm)" = "CX1\nvs\nCTL-FX",
  "CX2 vs CX1 (n=21,501 per arm)" = "CX2\nvs\nCX1",
  "CX3 vs CX2 (n=8,722 per arm)" = "CX3\nvs\nCX2"
)

comparison_colors <- c(
  "CX1_vs_CTLFX" = "#173f8a",
  "CX2_vs_CX1" = "#0d8785",
  "CX3_vs_CX2" = "#7a3e96"
)

family_colors <- c(
  "Psychiatric disorders" = "#173f8a",
  "Substance use disorders" = "#0d8785"
)

outcome_strip_labels <- c(
  "Mood disorders" = "Mood disorders",
  "Anxiety disorders" = "Anxiety disorders",
  "ADHD" = "ADHD",
  "Unspecified psychosis" = "Psychosis",
  "Generalized SUD" = "Generalized SUD",
  "Alcohol-related" = "Alcohol-related",
  "Cannabis-related" = "Cannabis-related",
  "Opioid-related" = "Opioid-related"
)

outcome_map <- tibble::tribble(
  ~outcome_number, ~outcome,               ~outcome_code, ~family,                                ~family_order, ~outcome_order,
  4L,              "Mood disorders",       "F30-F39",     "Internalizing / neurodevelopmental",  1L,            1L,
  5L,              "Anxiety disorders",    "F40-F48",     "Internalizing / neurodevelopmental",  1L,            2L,
  6L,              "ADHD",                 "F90",         "Internalizing / neurodevelopmental",  1L,            3L,
  7L,              "Unspecified psychosis","F29",         "Internalizing / neurodevelopmental",  1L,            4L,
  1L,              "Generalized SUD",      "F10-F19",     "Substance use",                        2L,            5L,
  2L,              "Alcohol-related",      "F10",         "Substance use",                        2L,            6L,
  3L,              "Cannabis-related",     "F12",         "Substance use",                        2L,            7L,
  8L,              "Opioid-related",       "F11",         "Substance use",                        2L,            8L
)

figure2_caption <- paste(
  "Grouped forest plot of 5-year relative risks across three stepwise propensity-score-matched comparisons.",
  "Each panel shows one pairwise comparison. Filled circles indicate statistically significant associations",
  "(95% CI excludes 1.0); open circles indicate non-significant. Circle size is proportional to the square",
  "root of total events. RR = relative risk; CI = confidence interval; CTL-FX = non-concussion pediatric",
  "forearm-fracture controls."
)

figure3_caption <- paste(
  "Prospective statistical power to detect a minimum clinically important relative risk of 1.20",
  "(alpha = 0.05, two-sided), by outcome and comparison. Power was computed from the matched sample size",
  "and the observed reference-cohort event rate using the normal approximation to the two-proportion z-test.",
  "The dashed line at 0.80 indicates conventional adequacy; panels below this threshold for a given comparison",
  "indicate that the analysis would not reliably detect the specified effect magnitude even if present."
)

clean_lines <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  trimws(lines)
}

extract_section_table <- function(lines, section_name, n_lines) {
  idx <- match(section_name, lines)
  if (is.na(idx)) {
    stop(sprintf("Section '%s' not found.", section_name), call. = FALSE)
  }
  text <- paste(lines[(idx + 1):(idx + n_lines)], collapse = "\n")
  utils::read.csv(text = text, check.names = FALSE, stringsAsFactors = FALSE)
}

parse_moa_table <- function(path) {
  lines <- clean_lines(path)
  cohort_stats <- extract_section_table(lines, "Cohort Statistics", 3)
  rr_table <- extract_section_table(lines, "Risk Ratio", 2)
  list(cohort_stats = cohort_stats, rr = rr_table)
}

parse_km_table <- function(path) {
  lines <- clean_lines(path)
  logrank_table <- extract_section_table(lines, "Log-Rank Test", 2)
  hr_table <- extract_section_table(lines, "Hazard Ratio", 2)
  list(logrank = logrank_table, hr = hr_table)
}

build_results_table <- function() {
  comparison_map %>%
    split(.$comparison) %>%
    purrr::map_dfr(function(comp_row) {
      comp <- comp_row$comparison[[1]]
      dir_name <- comp_row$dir_name[[1]]
      cohort1 <- comp_row$cohort1_label[[1]]
      cohort2 <- comp_row$cohort2_label[[1]]
      dir_path <- file.path(base_dir, dir_name)
      moa_files <- list.files(dir_path, pattern = "^Outcome_[0-9]+_Result_a_MOA_table\\.csv$", full.names = TRUE)
      if (length(moa_files) != 8) {
        stop(sprintf("Expected 8 MOA files in %s, found %s.", dir_path, length(moa_files)), call. = FALSE)
      }

      purrr::map_dfr(moa_files, function(moa_file) {
        outcome_number <- as.integer(stringr::str_match(basename(moa_file), "^Outcome_([0-9]+)_")[, 2])
        km_file <- file.path(dirname(moa_file), sprintf("Outcome_%s_Result_b_KM_table.csv", outcome_number))
        if (!file.exists(km_file)) {
          stop(sprintf("Missing KM file for %s.", moa_file), call. = FALSE)
        }

        moa <- parse_moa_table(moa_file)
        km <- parse_km_table(km_file)
        meta <- outcome_map %>% filter(outcome_number == !!outcome_number)
        if (nrow(meta) != 1) {
          stop(sprintf("Outcome mapping missing for outcome number %s.", outcome_number), call. = FALSE)
        }

        tibble(
          comparison = comp,
          outcome = meta$outcome,
          outcome_code = meta$outcome_code,
          family = meta$family,
          family_order = meta$family_order,
          outcome_order = meta$outcome_order,
          cohort1_label = cohort1,
          cohort2_label = cohort2,
          n1 = as.integer(moa$cohort_stats$`Patients in Cohort`[1]),
          n2 = as.integer(moa$cohort_stats$`Patients in Cohort`[2]),
          events1 = as.integer(moa$cohort_stats$`Patients with Outcome`[1]),
          events2 = as.integer(moa$cohort_stats$`Patients with Outcome`[2]),
          risk1 = as.numeric(moa$cohort_stats$Risk[1]),
          risk2 = as.numeric(moa$cohort_stats$Risk[2]),
          rr = as.numeric(moa$rr$`Risk Ratio`[1]),
          rr_lo = as.numeric(moa$rr$`95 % CI Lower`[1]),
          rr_hi = as.numeric(moa$rr$`95 % CI Upper`[1]),
          hr = as.numeric(km$hr$`Hazard Ratio`[1]),
          hr_lo = as.numeric(km$hr$`95 % CI Lower`[1]),
          hr_hi = as.numeric(km$hr$`95 % CI Upper`[1]),
          logrank_p = as.numeric(km$logrank$p[1])
        )
      })
    }) %>%
    left_join(comparison_map %>% select(comparison, comparison_label), by = "comparison") %>%
    mutate(
      comparison = factor(comparison, levels = comparison_map$comparison),
      comparison_label = factor(comparison_label, levels = comparison_map$comparison_label),
      outcome = factor(outcome, levels = outcome_map %>% arrange(outcome_order) %>% pull(outcome)),
      family = factor(family, levels = c("Internalizing / neurodevelopmental", "Substance use"))
    ) %>%
    arrange(comparison, outcome_order)
}

power_two_prop_normal <- function(p_control, rr_alt, n_per_arm, alpha = 0.05) {
  p_exposed <- min(p_control * rr_alt, 0.999999)
  delta <- p_exposed - p_control
  p_bar <- (p_exposed + p_control) / 2
  z_alpha <- qnorm(1 - alpha / 2)
  se_null <- sqrt(2 * p_bar * (1 - p_bar) / n_per_arm)
  se_alt <- sqrt((p_exposed * (1 - p_exposed) / n_per_arm) + (p_control * (1 - p_control) / n_per_arm))
  crit_upper <- z_alpha * se_null
  crit_lower <- -crit_upper
  upper_tail <- 1 - pnorm((crit_upper - delta) / se_alt)
  lower_tail <- pnorm((crit_lower - delta) / se_alt)
  upper_tail + lower_tail
}

save_plot_all_formats <- function(plot_obj, stem, width_in, height_in, dpi = 300) {
  ggplot2::ggsave(
    filename = paste0(stem, ".svg"),
    plot = plot_obj,
    width = width_in,
    height = height_in,
    units = "in",
    device = "svg"
  )
  ggplot2::ggsave(
    filename = paste0(stem, ".pdf"),
    plot = plot_obj,
    width = width_in,
    height = height_in,
    units = "in",
    device = grDevices::cairo_pdf
  )
  ggplot2::ggsave(
    filename = paste0(stem, ".png"),
    plot = plot_obj,
    width = width_in,
    height = height_in,
    units = "in",
    dpi = dpi,
    bg = "white"
  )
  ggplot2::ggsave(
    filename = paste0(stem, ".tiff"),
    plot = plot_obj,
    width = width_in,
    height = height_in,
    units = "in",
    dpi = dpi,
    compression = "lzw",
    bg = "white"
  )
}

save_grob_all_formats <- function(grob_obj, stem, width_in, height_in, dpi = 300) {
  grDevices::cairo_pdf(filename = paste0(stem, ".pdf"), width = width_in, height = height_in)
  grid::grid.newpage()
  grid::grid.draw(grob_obj)
  grDevices::dev.off()

  grDevices::png(filename = paste0(stem, ".png"), width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
  grid::grid.newpage()
  grid::grid.draw(grob_obj)
  grDevices::dev.off()

  grDevices::tiff(filename = paste0(stem, ".tiff"), width = width_in, height = height_in, units = "in", res = dpi, compression = "lzw", bg = "white")
  grid::grid.newpage()
  grid::grid.draw(grob_obj)
  grDevices::dev.off()
}

results_df <- build_results_table()
write_csv(results_df %>% mutate(across(where(is.factor), as.character)), file.path(base_dir, "results.csv"))

results_for_plot <- results_df %>%
  mutate(
    outcome = factor(outcome, levels = outcome_map %>% arrange(outcome_order) %>% pull(outcome)),
    significant = rr_lo > 1 | rr_hi < 1,
    rr_label = sprintf("%.2f (%.2f, %.2f)", rr, rr_lo, rr_hi),
    comparison_color = comparison_colors[as.character(comparison)],
    text_x = pmin(rr_hi * 1.18, 2.85)
  ) %>%
  group_by(comparison_label) %>%
  arrange(outcome_order, .by_group = TRUE) %>%
  mutate(
    y_pos = c(9, 8, 7, 6, 4, 3, 2, 1)
  ) %>%
  ungroup()

guide_lines <- tibble(yintercept = c(1:4, 6:9))
separator_line <- tibble(yintercept = 5)
stripe_rows <- tibble(ymin = c(8.5, 6.5, 2.5, 0.5), ymax = c(9.5, 7.5, 3.5, 1.5))
label_rows <- tibble(
  outcome = factor(outcome_map$outcome, levels = outcome_map %>% arrange(outcome_order) %>% pull(outcome)),
  y_pos = c(9, 8, 7, 6, 4, 3, 2, 1)
)
header_rows <- comparison_map %>%
  transmute(
    comparison,
    x = 1.75,
    y = 10.15,
    label = unname(comparison_strip_labels[comparison_label]),
    fill = unname(comparison_colors[comparison])
  )

make_forest_panel <- function(panel_df) {
  comp_id <- as.character(panel_df$comparison[[1]])
  panel_color <- comparison_colors[[comp_id]]
  header_df <- header_rows %>% filter(comparison == comp_id)

  ggplot(panel_df, aes(x = rr, y = y_pos)) +
    geom_rect(
      data = stripe_rows,
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "#f7f8fb",
      color = NA
    ) +
    geom_hline(data = separator_line, aes(yintercept = yintercept), inherit.aes = FALSE, linewidth = 0.35, color = "#9aa0a6") +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.45, color = "#636363") +
    geom_errorbar(aes(xmin = rr_lo, xmax = rr_hi), orientation = "y", width = 0.12, linewidth = 0.6, color = panel_color) +
    geom_point(
      aes(fill = significant),
      shape = 21,
      size = 4.1,
      stroke = 0.8,
      color = panel_color
    ) +
    geom_text(
      aes(x = text_x, label = rr_label),
      hjust = 0,
      size = 3.0,
      color = panel_color,
      family = figure_family
    ) +
    geom_label(
      data = header_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      fill = panel_color,
      color = "white",
      fontface = "bold",
      size = 6.2 / .pt,
      label.r = unit(0.12, "lines"),
      label.padding = unit(c(0.32, 0.55, 0.32, 0.55), "lines"),
      linewidth = 0.4
    ) +
    scale_x_log10(
      limits = c(0.5, 5.0),
      breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
      labels = label_number(accuracy = 0.01, trim = TRUE)
    ) +
    scale_y_continuous(limits = c(0.4, 10.45), breaks = NULL, expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(values = c(`TRUE` = panel_color, `FALSE` = "white"), guide = "none") +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 10.5, base_family = figure_family) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      plot.margin = margin(t = 6, r = 22, b = 0, l = 10)
    )
}

label_panel <- ggplot(label_rows, aes(x = 1, y = y_pos)) +
  geom_hline(data = separator_line, aes(yintercept = yintercept), inherit.aes = FALSE, linewidth = 0.35, color = "#9aa0a6") +
  geom_text(
    data = label_rows %>% filter(y_pos >= 6),
    aes(label = outcome),
    hjust = 0,
    size = 5.1,
    family = figure_family,
    color = "#111111"
  ) +
  geom_text(
    data = label_rows %>% filter(y_pos <= 4),
    aes(label = outcome),
    hjust = 0,
    size = 5.1,
    family = figure_family,
    color = "#111111"
  ) +
  annotate(
    "text",
    x = 1,
    y = 9.75,
    label = "Psychiatric disorders",
    hjust = 0,
    family = figure_family,
    fontface = "bold",
    size = 4.9,
    color = family_colors[[1]]
  ) +
  annotate(
    "text",
    x = 1,
    y = 4.35,
    label = "Substance use disorders",
    hjust = 0,
    family = figure_family,
    fontface = "bold",
    size = 4.3,
    color = family_colors[[2]]
  ) +
  scale_x_continuous(limits = c(1, 1.02), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0.4, 10.45), breaks = NULL, expand = expansion(mult = c(0, 0))) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = figure_family) +
  theme(plot.margin = margin(t = 6, r = 0, b = 0, l = 0))

forest_panels <- results_for_plot %>%
  split(.$comparison) %>%
  purrr::map(make_forest_panel)

x_axis_label <- patchwork::wrap_elements(
  grid::textGrob("Relative risk (log scale)", gp = grid::gpar(fontfamily = figure_family, fontsize = 18))
)

figure2 <- (
  label_panel +
    forest_panels[[1]] +
    forest_panels[[2]] +
    forest_panels[[3]] +
    plot_layout(widths = c(2.55, 2.35, 2.35, 2.35))
) / x_axis_label +
  plot_layout(heights = c(24, 1.5)) +
  plot_annotation(
    title = "Five-year relative risks across stepwise matched comparisons",
    subtitle = "Filled circles indicate 95% CI excludes 1.0; open circles indicate non-significant associations",
    theme = theme(
      text = element_text(family = figure_family),
      plot.title = element_text(face = "bold", size = 18.5, hjust = 0.5),
      plot.subtitle = element_text(size = 12.5, hjust = 0.5),
      plot.margin = margin(t = 8, r = 10, b = 6, l = 10)
    )
  )

power_df <- results_df %>%
  transmute(
    comparison,
    comparison_label,
    outcome,
    family,
    family_order,
    outcome_order,
    n_per_arm = n1,
    p_control = risk2,
    p_exposed_at_MCID = pmin(risk2 * 1.20, 0.999999),
    power = power_two_prop_normal(risk2, 1.20, n1),
    meets_80pct_power = power >= 0.80
  ) %>%
  mutate(
    comparison_label = factor(comparison_label, levels = comparison_map$comparison_label),
    outcome = factor(outcome, levels = outcome_map %>% arrange(outcome_order) %>% pull(outcome)),
    family = factor(family, levels = c("Internalizing / neurodevelopmental", "Substance use")),
    family_fill = if_else(family == "Internalizing / neurodevelopmental", "#eaf2f8", "#f5efe6"),
    annotation = sprintf("n=%s / p0=%.3f", format(n_per_arm, big.mark = ","), p_control),
    show_annotation = power < 0.95
  ) %>%
  arrange(outcome_order, comparison)

write_csv(power_df %>% select(comparison, outcome, n_per_arm, p_control, p_exposed_at_MCID, power, meets_80pct_power), file.path(base_dir, "power_table.csv"))

panel_bg <- power_df %>%
  distinct(outcome, family_fill) %>%
  mutate(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

figure3 <- ggplot(power_df, aes(x = comparison_label, y = power)) +
  geom_rect(
    data = panel_bg,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = family_fill),
    inherit.aes = FALSE,
    alpha = 0.18,
    color = NA
  ) +
  geom_hline(yintercept = 0.80, linetype = "dashed", linewidth = 0.4, color = "grey45") +
  geom_line(aes(group = 1), linewidth = 0.35, color = "grey55") +
  geom_point(shape = 21, size = 2.7, stroke = 0.5, fill = "#495867", color = "grey20") +
  geom_text(
    data = power_df %>% filter(show_annotation),
    aes(label = annotation),
    vjust = -0.75,
    size = 2.45,
    family = figure_family,
    color = "grey20"
  ) +
  facet_wrap(~ outcome, nrow = 2, labeller = labeller(outcome = outcome_strip_labels)) +
  scale_fill_identity() +
  scale_x_discrete(labels = comparison_axis_labels) +
  scale_y_continuous(
    limits = c(0, 1.02),
    breaks = c(0, 0.25, 0.50, 0.75, 0.80, 1.00),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x = NULL,
    y = "Prospective power at RR = 1.20",
    title = "Prospective power to detect a minimum clinically important relative risk of 1.20",
    subtitle = "Annotations shown only for points with power < 0.95; observed reference-cohort risk used as p0"
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 10.5, base_family = figure_family) +
  theme(
    strip.background = element_rect(fill = "grey96", color = "grey80", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 9.8),
    axis.text.x = element_text(size = 8.2, lineheight = 0.92),
    panel.spacing = unit(7, "mm"),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9.5),
    plot.margin = margin(t = 12, r = 14, b = 14, l = 14)
  )

width_in <- 240 / 25.4
height_in <- 120 / 25.4

save_plot_all_formats(figure2, file.path(base_dir, "figure2_forest"), width_in, height_in, dpi = 300)
save_plot_all_formats(figure3, file.path(base_dir, "figure3_power"), width_in, height_in, dpi = 300)

writeLines(
  c(
    paste("Figure 2:", figure2_caption),
    "",
    paste("Figure 3:", figure3_caption)
  ),
  con = file.path(base_dir, "figure_captions.txt")
)

low_80 <- power_df %>%
  filter(power < 0.80) %>%
  mutate(summary = sprintf("%s | %s | power=%.3f", as.character(comparison), as.character(outcome), power)) %>%
  pull(summary)

low_50 <- power_df %>%
  filter(power < 0.50) %>%
  mutate(summary = sprintf("%s | %s | power=%.3f", as.character(comparison), as.character(outcome), power)) %>%
  pull(summary)

cat("\nFigure 2 caption:\n", figure2_caption, "\n", sep = "")
cat("\nFigure 3 caption:\n", figure3_caption, "\n", sep = "")

cat("\nCells with power < 0.80:\n", sep = "")
if (length(low_80) == 0) {
  cat("None\n")
} else {
  cat(paste0("- ", low_80, collapse = "\n"), "\n", sep = "")
}

cat("\nCells with power < 0.50:\n", sep = "")
if (length(low_50) == 0) {
  cat("None\n")
} else {
  cat(paste0("- ", low_50, collapse = "\n"), "\n", sep = "")
}

writeLines(capture.output(sessionInfo()), con = file.path(base_dir, "session_info.txt"))

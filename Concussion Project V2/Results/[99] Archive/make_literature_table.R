# R version: 4.3+ recommended
# Required packages: readr, dplyr, grid, gridExtra, gtable, svglite

set.seed(20260421)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(grid)
  library(gridExtra)
  library(gtable)
  library(svglite)
})

base_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(base_dir)
figure_family <- "Helvetica"

wrap_text <- function(x, width) {
  vapply(
    x,
    function(value) paste(strwrap(value, width = width), collapse = "\n"),
    character(1)
  )
}

lit_tbl <- read_csv(file.path(base_dir, "literature_review_table.csv"), show_col_types = FALSE) %>%
  mutate(
    age_range = wrap_text(age_range, 10),
    sample_size = wrap_text(sample_size, 16),
    follow_up_period = wrap_text(follow_up_period, 16),
    concussion_exposure = wrap_text(concussion_exposure, 18),
    outcome_measures = wrap_text(outcome_measures, 30),
    main_findings = wrap_text(main_findings, 43)
  )

display_tbl <- lit_tbl %>%
  rename(
    `Age range\n(years)` = age_range,
    `Sample size\n(n)` = sample_size,
    `Follow-up\nperiod` = follow_up_period,
    `Concussion\nexposure` = concussion_exposure,
    `Outcome measures` = outcome_measures,
    `Main findings` = main_findings
  )

table_theme <- gridExtra::ttheme_minimal(
  base_size = 12,
  base_family = figure_family,
  core = list(
    fg_params = list(hjust = 0, x = 0.02, lineheight = 1.15, fontsize = 11.8, col = "#111111"),
    bg_params = list(fill = rep(c("#f7f8fb", "white"), length.out = nrow(display_tbl)), col = NA),
    padding = unit(c(7, 7), "mm")
  ),
  colhead = list(
    fg_params = list(fontface = "bold", fontsize = 12.6, col = "#111111"),
    bg_params = list(fill = "#e9eef6", col = NA),
    padding = unit(c(8, 7), "mm")
  )
)

tab_grob <- gridExtra::tableGrob(display_tbl, rows = NULL, theme = table_theme)

for (row_idx in seq_len(nrow(display_tbl))) {
  bg_fill <- if (row_idx %% 2 == 1) "#f7f8fb" else "white"
  for (col_idx in seq_len(ncol(display_tbl))) {
    core_cell <- gtable::gtable_filter(tab_grob, "core-fg", trim = FALSE)
    cell_name <- sprintf("core-bg-%d-%d", row_idx, col_idx)
    match_idx <- which(tab_grob$layout$name == cell_name)
    if (length(match_idx) == 1) {
      tab_grob$grobs[[match_idx]]$gp <- gpar(fill = bg_fill, col = NA)
    }
  }
}

tab_grob$widths <- unit(c(1.05, 1.4, 1.55, 1.7, 2.7, 3.45), "in")

title_grob <- textGrob(
  "Table 1. Previous Literature on Pediatric Concussions",
  x = 0.01,
  hjust = 0,
  gp = gpar(fontfamily = figure_family, fontface = "bold", fontsize = 18, col = "#111111")
)

subtitle_grob <- textGrob(
  "Summary of prior studies examining psychiatric, behavioral, and substance-use outcomes after concussion exposure.",
  x = 0.01,
  hjust = 0,
  gp = gpar(fontfamily = figure_family, fontsize = 11.5, col = "#444444")
)

footnote_grob <- textGrob(
  "Abbreviations are retained as reported in the source table. This table reflects the literature entries visible in the provided image.",
  x = 0.01,
  hjust = 0,
  gp = gpar(fontfamily = figure_family, fontsize = 9.5, col = "#555555")
)

full_grob <- arrangeGrob(
  title_grob,
  subtitle_grob,
  tab_grob,
  footnote_grob,
  ncol = 1,
  heights = unit.c(unit(0.45, "in"), unit(0.36, "in"), sum(tab_grob$heights) + unit(0.16, "in"), unit(0.34, "in"))
)

save_table <- function(stem, grob_obj, width_in = 13.8, height_in = 10.2, dpi = 300) {
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

save_table(file.path(base_dir, "table1_previous_literature"), full_grob)

writeLines(capture.output(sessionInfo()), file.path(base_dir, "literature_table_session_info.txt"))

cat("Saved literature table to:\n")
cat("- table1_previous_literature.svg\n")
cat("- table1_previous_literature.pdf\n")
cat("- table1_previous_literature.png\n")

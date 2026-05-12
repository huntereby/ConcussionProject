# Builds a plain supplemental follow-up time table from the TriNetX exports.

comparison_map <- data.frame(
  comparison = c("CX1 vs CTL-FX", "CX2 vs CX1", "CX3 vs CX2"),
  dir_name = c(
    "Concuss_1x_5_year_V12_424528",
    "Concuss_2x_5_year_V12_(1)_(1)_677193",
    "Concuss_3x_5_year_V12_327389"
  ),
  cohort_1 = c("CX1", "CX2", "CX3"),
  cohort_2 = c("CTL-FX", "CX1", "CX2"),
  stringsAsFactors = FALSE
)

base_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

read_followup_table <- function(path, comparison, status, cohort_labels) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  header_idx <- grep("^Cohort,Cohort Name,Mean Follow-up", lines)

  if (length(header_idx) != 1) {
    stop(sprintf("Could not find follow-up header in %s", path), call. = FALSE)
  }

  raw <- utils::read.csv(
    text = paste(lines[header_idx:length(lines)], collapse = "\n"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  data.frame(
    Comparison = comparison,
    Match_Status = status,
    Cohort = cohort_labels[raw$Cohort],
    Mean_Follow_up_Days = round(raw[["Mean Follow-up (Days)"]], 1),
    SD_Days = round(raw[["Standard Deviation"]], 1),
    Median_Follow_up_Days = round(raw[["Median Follow-up (Days)"]], 0),
    IQR_Days = round(raw[["Interquartile Range"]], 0),
    stringsAsFactors = FALSE
  )
}

followup_df <- do.call(
  rbind,
  lapply(seq_len(nrow(comparison_map)), function(i) {
    comp <- comparison_map[i, ]
    cohort_labels <- c(comp$cohort_1, comp$cohort_2)
    dir_path <- file.path(base_dir, comp$dir_name)

    before <- read_followup_table(
      file.path(dir_path, "Follow_up_time_metrics_unmatched_results_table.csv"),
      comp$comparison,
      "Before matching",
      cohort_labels
    )
    after <- read_followup_table(
      file.path(dir_path, "Follow_up_time_metrics_matched_results_table.csv"),
      comp$comparison,
      "After matching",
      cohort_labels
    )

    rbind(before, after)
  })
)

out_csv <- file.path(base_dir, "supplement_followup_time_table.csv")
utils::write.csv(followup_df, out_csv, row.names = FALSE)

display_df <- followup_df
names(display_df) <- c(
  "Comparison",
  "Status",
  "Cohort",
  "Mean follow-up, days",
  "SD, days",
  "Median, days",
  "IQR, days"
)

display_df[["Mean follow-up, days"]] <- sprintf("%.1f", display_df[["Mean follow-up, days"]])
display_df[["SD, days"]] <- sprintf("%.1f", display_df[["SD, days"]])
display_df[["Median, days"]] <- sprintf("%.0f", display_df[["Median, days"]])
display_df[["IQR, days"]] <- sprintf("%.0f", display_df[["IQR, days"]])

draw_table <- function(df) {
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(df) + 2, ncol(df))))

  title_gp <- grid::gpar(fontfamily = "Helvetica", fontsize = 13, fontface = "bold")
  header_gp <- grid::gpar(fontfamily = "Helvetica", fontsize = 8.5, fontface = "bold")
  body_gp <- grid::gpar(fontfamily = "Helvetica", fontsize = 8)
  line_gp <- grid::gpar(col = "grey70", lwd = 0.7)

  grid::grid.text(
    "Supplemental Table. Follow-up time metrics before and after propensity-score matching",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5, "npc"),
    gp = title_gp,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(df))
  )

  for (j in seq_len(ncol(df))) {
    grid::grid.text(
      names(df)[j],
      x = grid::unit(0.03, "npc"),
      just = "left",
      gp = header_gp,
      vp = grid::viewport(layout.pos.row = 2, layout.pos.col = j)
    )
  }

  for (i in seq_len(nrow(df))) {
    row <- i + 2
    fill <- if (i %% 2 == 0) "#f7f7f7" else "white"
    grid::grid.rect(
      gp = grid::gpar(fill = fill, col = NA),
      vp = grid::viewport(layout.pos.row = row, layout.pos.col = 1:ncol(df))
    )

    for (j in seq_len(ncol(df))) {
      grid::grid.text(
        as.character(df[i, j]),
        x = grid::unit(0.03, "npc"),
        just = "left",
        gp = body_gp,
        vp = grid::viewport(layout.pos.row = row, layout.pos.col = j)
      )
    }
  }

  grid::grid.lines(
    x = grid::unit(c(0, 1), "npc"),
    y = grid::unit(c(0, 0), "npc"),
    gp = line_gp,
    vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1:ncol(df))
  )
  grid::popViewport()
}

save_table <- function(filename, width, height, device) {
  if (device == "pdf") {
    grDevices::cairo_pdf(filename, width = width, height = height)
  } else {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300, bg = "white")
  }
  draw_table(display_df)
  grDevices::dev.off()
}

save_table(file.path(base_dir, "supplement_followup_time_table.pdf"), 11, 5.8, "pdf")
save_table(file.path(base_dir, "supplement_followup_time_table.png"), 11, 5.8, "png")

message("Wrote: ", out_csv)
message("Wrote: ", file.path(base_dir, "supplement_followup_time_table.pdf"))
message("Wrote: ", file.path(base_dir, "supplement_followup_time_table.png"))

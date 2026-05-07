#!/usr/bin/env Rscript

# Build a combined forest plot from TriNetX outcome KM tables in Results/* (excluding Archive)
# using risk ratios computed from cohort event counts.

results_dir <- "Results"
out_png <- file.path(results_dir, "Forest_Plot_Risk_Ratios.png")
out_csv <- file.path(results_dir, "Forest_Plot_Risk_Ratios_Data.csv")

read_metadata <- function(readme_path) {
  lines <- readLines(readme_path, warn = FALSE)

  analysis <- sub("^ \\* Analysis Name: ", "", grep("^ \\* Analysis Name:", lines, value = TRUE)[1])
  run_on <- sub("^ \\* Run on: ", "", grep("^ \\* Run on:", lines, value = TRUE)[1])

  out_lines <- grep("^ \\* Outcome [0-9]+ Name:", lines, value = TRUE)
  outcome_map <- setNames(rep(NA_character_, length(out_lines)), rep(NA_character_, length(out_lines)))

  for (ln in out_lines) {
    num <- sub("^ \\* Outcome ([0-9]+) Name:.*$", "\\1", ln)
    name <- sub("^ \\* Outcome [0-9]+ Name: ", "", ln)
    outcome_map[num] <- name
  }

  list(analysis = analysis, run_on = run_on, outcome_map = outcome_map)
}

read_rr_from_km <- function(csv_path) {
  lines <- readLines(csv_path, warn = FALSE)
  idx <- grep("^Cohort,Cohort Name,Patients in Cohort,Patients with Outcome,Median Survival \\(Days\\),Survival Probability at End of Time Window$", lines)
  if (length(idx) == 0 || (idx[1] + 2) > length(lines)) {
    return(c(rr = NA_real_, lcl = NA_real_, ucl = NA_real_, events_1 = NA_real_, total_1 = NA_real_, events_2 = NA_real_, total_2 = NA_real_))
  }

  row_1 <- strsplit(lines[idx[1] + 1], ",", fixed = TRUE)[[1]]
  row_2 <- strsplit(lines[idx[1] + 2], ",", fixed = TRUE)[[1]]
  if (length(row_1) < 4 || length(row_2) < 4) {
    return(c(rr = NA_real_, lcl = NA_real_, ucl = NA_real_, events_1 = NA_real_, total_1 = NA_real_, events_2 = NA_real_, total_2 = NA_real_))
  }

  total_1 <- suppressWarnings(as.numeric(row_1[3]))
  events_1 <- suppressWarnings(as.numeric(row_1[4]))
  total_2 <- suppressWarnings(as.numeric(row_2[3]))
  events_2 <- suppressWarnings(as.numeric(row_2[4]))

  vals <- c(events_1, total_1, events_2, total_2)
  if (any(is.na(vals)) || any(vals <= 0) || events_1 > total_1 || events_2 > total_2) {
    return(c(rr = NA_real_, lcl = NA_real_, ucl = NA_real_, events_1 = events_1, total_1 = total_1, events_2 = events_2, total_2 = total_2))
  }

  risk_1 <- events_1 / total_1
  risk_2 <- events_2 / total_2
  rr <- risk_1 / risk_2

  # Katz log CI for the risk ratio.
  se_log_rr <- sqrt((1 / events_1) - (1 / total_1) + (1 / events_2) - (1 / total_2))
  lcl <- exp(log(rr) - 1.96 * se_log_rr)
  ucl <- exp(log(rr) + 1.96 * se_log_rr)

  c(rr = rr, lcl = lcl, ucl = ucl, events_1 = events_1, total_1 = total_1, events_2 = events_2, total_2 = total_2)
}

dirs <- list.dirs(results_dir, recursive = FALSE, full.names = TRUE)
dirs <- dirs[basename(dirs) != "Archive"]

all_rows <- list()

for (d in dirs) {
  readme <- file.path(d, "_Read_Me.txt")
  if (!file.exists(readme)) next

  meta <- read_metadata(readme)
  km_files <- list.files(d, pattern = "^Outcome_[0-9]+_Result_b_KM_table\\.csv$", full.names = TRUE)

  for (kf in km_files) {
    fname <- basename(kf)
    outcome_num <- sub("^Outcome_([0-9]+)_Result_b_KM_table\\.csv$", "\\1", fname)
    est <- read_rr_from_km(kf)

    all_rows[[length(all_rows) + 1]] <- data.frame(
      folder = basename(d),
      analysis = meta$analysis,
      run_on = meta$run_on,
      outcome_num = as.integer(outcome_num),
      outcome = unname(meta$outcome_map[outcome_num]),
      rr = as.numeric(est[["rr"]]),
      lcl = as.numeric(est[["lcl"]]),
      ucl = as.numeric(est[["ucl"]]),
      events_1 = as.numeric(est[["events_1"]]),
      total_1 = as.numeric(est[["total_1"]]),
      events_2 = as.numeric(est[["events_2"]]),
      total_2 = as.numeric(est[["total_2"]]),
      stringsAsFactors = FALSE
    )
  }
}

if (length(all_rows) == 0) {
  stop("No KM outcome tables found under Results/*")
}

plot_data <- do.call(rbind, all_rows)
plot_data <- plot_data[grepl("Concuss (2x|3x)", plot_data$analysis), ]
plot_data <- plot_data[order(plot_data$analysis, plot_data$outcome_num), ]

if (nrow(plot_data) == 0) {
  stop("No 2x or 3x analyses found under Results/*")
}

write.csv(plot_data, out_csv, row.names = FALSE)

analyses <- unique(plot_data$analysis)
n_analyses <- length(analyses)
n_outcomes <- length(unique(plot_data$outcome_num))
analysis_colors <- c("#1b9e77", "#d95f02", "#7570b3")
if (n_analyses > length(analysis_colors)) {
  analysis_colors <- grDevices::hcl.colors(n_analyses, palette = "Dark 3")
} else {
  analysis_colors <- analysis_colors[seq_len(n_analyses)]
}
names(analysis_colors) <- analyses

plot_data$analysis <- factor(plot_data$analysis, levels = analyses)
plot_data$outcome_num <- as.integer(plot_data$outcome_num)

base_y <- setNames(rev(seq_len(n_outcomes)), sort(unique(plot_data$outcome_num)))
offsets <- seq(0.24, -0.24, length.out = n_analyses)
names(offsets) <- analyses
plot_data$y <- base_y[as.character(plot_data$outcome_num)] + offsets[as.character(plot_data$analysis)]

label_df <- plot_data[!duplicated(plot_data$outcome_num), c("outcome_num", "outcome")]
label_df <- label_df[order(label_df$outcome_num), ]
label_y <- base_y[as.character(label_df$outcome_num)]
labels <- paste0(label_df$outcome_num, ". ", label_df$outcome)

x_min <- min(c(0.2, plot_data$lcl), na.rm = TRUE)
x_max <- max(c(2.5, plot_data$ucl), na.rm = TRUE)

png(filename = out_png, width = 2600, height = 1500, res = 220)
par(mar = c(5, 18, 6, 10))
plot(
  x = plot_data$rr,
  y = plot_data$y,
  xlim = c(x_min, x_max),
  ylim = c(0.5, n_outcomes + 0.5),
  log = "x",
  type = "n",
  xlab = "Risk Ratio (log scale)",
  ylab = "",
  yaxt = "n",
  main = "Forest Plot of Risk Ratios"
)

grid(nx = NULL, ny = NULL, col = "gray92", lty = 1)
abline(v = 1, lty = 2, col = "gray40")
axis(2, at = label_y, labels = labels, las = 2, cex.axis = 0.95)

for (an in analyses) {
  df <- plot_data[plot_data$analysis == an, ]
  col <- analysis_colors[[an]]
  segments(df$lcl, df$y, df$ucl, df$y, lwd = 2, col = col)
  points(df$rr, df$y, pch = 19, col = col, cex = 1.2)
}

legend("topright", inset = c(-0.08, 0), legend = analyses, col = analysis_colors, pch = 19, lwd = 2, bty = "n", xpd = TRUE, cex = 0.9)

usr <- par("usr")
text_x <- usr[2]
for (an in analyses) {
  df <- plot_data[plot_data$analysis == an, ]
  txt <- sprintf("%s: RR %.3f (%.3f, %.3f)", df$analysis, df$rr, df$lcl, df$ucl)
  text(x = text_x, y = df$y, labels = txt, pos = 2, xpd = TRUE, cex = 0.65, col = analysis_colors[[an]])
}

dev.off()

cat("Wrote:\n")
cat(out_png, "\n")
cat(out_csv, "\n")

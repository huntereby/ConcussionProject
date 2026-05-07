#!/usr/bin/env Rscript

# Build individual risk-ratio forest plots as SVGs from TriNetX KM tables.

results_dir <- "Results"
out_dir <- file.path(results_dir, "Forest_Plot_Risk_Ratios_SVG")

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
    return(c(rr = NA_real_, lcl = NA_real_, ucl = NA_real_))
  }

  row_1 <- strsplit(lines[idx[1] + 1], ",", fixed = TRUE)[[1]]
  row_2 <- strsplit(lines[idx[1] + 2], ",", fixed = TRUE)[[1]]
  if (length(row_1) < 4 || length(row_2) < 4) {
    return(c(rr = NA_real_, lcl = NA_real_, ucl = NA_real_))
  }

  total_1 <- suppressWarnings(as.numeric(row_1[3]))
  events_1 <- suppressWarnings(as.numeric(row_1[4]))
  total_2 <- suppressWarnings(as.numeric(row_2[3]))
  events_2 <- suppressWarnings(as.numeric(row_2[4]))

  vals <- c(events_1, total_1, events_2, total_2)
  if (any(is.na(vals)) || any(vals <= 0) || events_1 > total_1 || events_2 > total_2) {
    return(c(rr = NA_real_, lcl = NA_real_, ucl = NA_real_))
  }

  rr <- (events_1 / total_1) / (events_2 / total_2)
  se_log_rr <- sqrt((1 / events_1) - (1 / total_1) + (1 / events_2) - (1 / total_2))
  lcl <- exp(log(rr) - 1.96 * se_log_rr)
  ucl <- exp(log(rr) + 1.96 * se_log_rr)

  c(rr = rr, lcl = lcl, ucl = ucl)
}

sanitize_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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
      analysis = meta$analysis,
      run_on = meta$run_on,
      outcome_num = as.integer(outcome_num),
      outcome = unname(meta$outcome_map[outcome_num]),
      rr = as.numeric(est[["rr"]]),
      lcl = as.numeric(est[["lcl"]]),
      ucl = as.numeric(est[["ucl"]]),
      stringsAsFactors = FALSE
    )
  }
}

if (length(all_rows) == 0) {
  stop("No KM outcome tables found under Results/*")
}

plot_data <- do.call(rbind, all_rows)
plot_data <- plot_data[order(plot_data$analysis, plot_data$outcome_num), ]

analyses <- unique(plot_data$analysis)

for (an in analyses) {
  df <- plot_data[plot_data$analysis == an, ]
  df <- df[order(df$outcome_num), ]

  y <- rev(seq_len(nrow(df)))
  labels <- paste0(df$outcome_num, ". ", df$outcome)
  x_min <- min(c(0.2, df$lcl), na.rm = TRUE)
  x_max <- max(c(2.5, df$ucl), na.rm = TRUE)

  out_file <- file.path(out_dir, paste0(sanitize_filename(an), ".svg"))

  svg(filename = out_file, width = 8.5, height = 8.2, pointsize = 12)
  par(mar = c(5.2, 15.5, 4.8, 1.8))

  plot(
    x = df$rr,
    y = y,
    xlim = c(x_min, x_max),
    ylim = c(0.5, nrow(df) + 0.7),
    log = "x",
    pch = 19,
    xlab = "Risk Ratio (log scale)",
    ylab = "",
    yaxt = "n",
    main = an,
    cex.main = 1.35
  )

  grid(nx = NULL, ny = NULL, col = "gray90", lty = 1)
  abline(v = 1, lty = 2, col = "gray40")
  axis(2, at = y, labels = labels, las = 2, cex.axis = 1.05)

  segments(df$lcl, y, df$ucl, y, lwd = 2)
  points(df$rr, y, pch = 19, cex = 1.1)

  txt <- sprintf("RR %.3f (%.3f, %.3f)", df$rr, df$lcl, df$ucl)
  text_x <- exp(mean(log(c(x_min, x_max))))
  text(x = text_x, y = y, labels = txt, pos = 4, cex = 0.85)

  dev.off()
}

cat("Wrote SVGs to:\n")
cat(out_dir, "\n")

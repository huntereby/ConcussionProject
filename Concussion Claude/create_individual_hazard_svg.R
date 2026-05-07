#!/usr/bin/env Rscript

# Build individual hazard-ratio forest plots as SVGs from the saved HR data.

results_dir <- "Results"
input_csv <- file.path(results_dir, "Forest_Plot_Hazard_Ratios_Data.csv")
out_dir <- file.path(results_dir, "Forest_Plot_Hazard_Ratios_SVG")

if (!file.exists(input_csv)) {
  stop("Missing input file: ", input_csv)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plot_data <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)
plot_data <- plot_data[order(plot_data$analysis, plot_data$outcome_num), ]

sanitize_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

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
    x = df$hr,
    y = y,
    xlim = c(x_min, x_max),
    ylim = c(0.5, nrow(df) + 0.7),
    log = "x",
    pch = 19,
    xlab = "Hazard Ratio (log scale)",
    ylab = "",
    yaxt = "n",
    main = an,
    cex.main = 1.35
  )

  grid(nx = NULL, ny = NULL, col = "gray90", lty = 1)
  abline(v = 1, lty = 2, col = "gray40")
  axis(2, at = y, labels = labels, las = 2, cex.axis = 1.05)

  segments(df$lcl, y, df$ucl, y, lwd = 2)
  points(df$hr, y, pch = 19, cex = 1.1)

  txt <- sprintf("HR %.3f (%.3f, %.3f)", df$hr, df$lcl, df$ucl)
  text_x <- exp(mean(log(c(x_min, x_max))))
  text(x = text_x, y = y, labels = txt, pos = 4, cex = 0.85)

  dev.off()
}

cat("Wrote SVGs to:\n")
cat(out_dir, "\n")

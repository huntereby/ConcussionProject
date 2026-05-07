#!/usr/bin/env Rscript

# Poster-ready power analysis figure for the 2x and 3x comparisons.
# Power is computed for a two-sided two-sample proportions test at alpha = 0.05
# using the observed event rates and matched cohort sizes.

results_dir <- "Results"
input_csv <- file.path(results_dir, "Forest_Plot_Risk_Ratios_Data.csv")
out_svg <- file.path(results_dir, "Power_Analysis_2x_3x.svg")
out_png <- file.path(results_dir, "Power_Analysis_2x_3x.png")
out_csv <- file.path(results_dir, "Power_Analysis_2x_3x_Data.csv")

if (!file.exists(input_csv)) {
  stop("Missing input file: ", input_csv)
}

plot_data <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)
plot_data <- plot_data[grepl("Concuss (2x|3x)", plot_data$analysis), ]

if (nrow(plot_data) == 0) {
  stop("No 2x or 3x rows found in ", input_csv)
}

compute_power <- function(events_1, total_1, events_2, total_2, alpha = 0.05) {
  p1 <- events_1 / total_1
  p2 <- events_2 / total_2

  out <- suppressWarnings(
    power.prop.test(
      n = min(total_1, total_2),
      p1 = p1,
      p2 = p2,
      sig.level = alpha,
      alternative = "two.sided"
    )
  )

  out$power
}

shorten_analysis <- function(x) {
  x <- sub("^Concuss ", "", x)
  x <- sub(" V12.*$", "", x)
  x
}

shorten_outcome <- function(x) {
  x <- sub("^Alcohol Related Disorder$", "Alcohol", x)
  x <- sub("^cannabis related disorde$", "Cannabis", x)
  x <- sub("^Mood affect disorder$", "Mood", x)
  x <- sub("^opioid related disorder$", "Opioid", x)
  x
}

plot_data$power <- mapply(
  compute_power,
  plot_data$events_1,
  plot_data$total_1,
  plot_data$events_2,
  plot_data$total_2
)
plot_data$power_pct <- 100 * plot_data$power
plot_data$p1_pct <- 100 * plot_data$events_1 / plot_data$total_1
plot_data$p2_pct <- 100 * plot_data$events_2 / plot_data$total_2
plot_data$analysis_short <- shorten_analysis(plot_data$analysis)
plot_data$outcome_short <- shorten_outcome(plot_data$outcome)

plot_data <- plot_data[order(plot_data$analysis, plot_data$outcome_num), ]
write.csv(plot_data, out_csv, row.names = FALSE)

analyses <- unique(plot_data$analysis)
panel_fill <- c("#0f766e", "#b45309")
if (length(analyses) > length(panel_fill)) {
  panel_fill <- grDevices::hcl.colors(length(analyses), palette = "Temps")
}
names(panel_fill) <- analyses

draw_figure <- function(device_fun) {
  device_fun()
  par(bg = "white", mar = c(5.2, 8.5, 5, 1.5), oma = c(0, 0, 2, 0), mfrow = c(1, 2))

  for (an in analyses) {
    df <- plot_data[plot_data$analysis == an, ]
    df <- df[order(df$outcome_num), ]

    y <- rev(seq_len(nrow(df)))
    bar_cols <- ifelse(df$power_pct >= 80, panel_fill[[an]], "#d4d4d8")

    plot(
      NA,
      xlim = c(0, 100),
      ylim = c(0.5, nrow(df) + 1.25),
      xlab = "Observed power (%)",
      ylab = "",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      main = paste0(shorten_analysis(an), " Comparison"),
      cex.main = 1.35
    )

    rect(0, 0.5, 100, nrow(df) + 1.25, col = "#fafaf9", border = NA)
    abline(v = seq(0, 100, by = 20), col = "#e7e5e4", lty = 1)
    abline(v = 80, col = "#dc2626", lwd = 2, lty = 2)
    axis(1, at = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20), "%"), cex.axis = 0.95)
    axis(2, at = y, labels = df$outcome_short, las = 2, tick = FALSE, cex.axis = 1.05)

    segments(0, y, df$power_pct, y, lwd = 10, lend = "butt", col = bar_cols)
    points(df$power_pct, y, pch = 21, bg = bar_cols, col = "white", cex = 1.5, lwd = 1.4)

    power_lab <- sprintf("%.1f%%", df$power_pct)
    text(pmin(df$power_pct + 4, 97), y, labels = power_lab, pos = 4, cex = 0.9, col = "#111827")

    rr_lab <- sprintf("RR %.2f", df$rr)
    text(rep(1.5, length(y)), y + 0.28, labels = rr_lab, pos = 4, cex = 0.75, col = "#4b5563")

    n_lab <- sprintf("n = %s per cohort", format(df$total_1[1], big.mark = ",", scientific = FALSE))
    text(0, nrow(df) + 1, labels = n_lab, adj = c(0, 0.5), cex = 0.9, col = "#374151")
  }

  mtext("Power Analysis for 2x and 3x Comparisons", outer = TRUE, cex = 1.5, font = 2, line = 0.2)
  mtext("Two-sided alpha = 0.05; observed power based on matched cohort sizes and observed outcome rates", outer = TRUE, cex = 0.95, line = -1.2, col = "#4b5563")
  dev.off()
}

draw_figure(function() svg(out_svg, width = 12, height = 7.5, pointsize = 13))
draw_figure(function() png(out_png, width = 3000, height = 1900, res = 250))

cat("Wrote:\n")
cat(out_svg, "\n")
cat(out_png, "\n")
cat(out_csv, "\n")

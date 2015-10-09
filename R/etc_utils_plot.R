#
# matplot wrapper
#
.matplot_wrapper <- function(obj, curve_name, xlab, ylab,
                             data_type = "multiple") {

  # === Validate input arguments ===
  .validate(obj)

  # === Create a plot ===
  np <- attr(obj[[1]], "np")
  nn <- attr(obj[[1]], "nn")

  mats <- .make_matplot_mats(obj)
  if (data_type == "single") {
    line_col <- "black"
  } else {
    line_col <- rainbow(ncol(mats[["x"]]))
  }

  matplot(mats[["x"]], mats[["y"]], type = "l", lty = 1,
          col = line_col,
          main = paste0(curve_name, " - P: ", np, ", N: ", nn),
          xlab = xlab, ylab = ylab,
          ylim = c(0, 1), xlim = c(0, 1))
}

#
# Make matrices for matplot
#
.make_matplot_mats <- function(obj) {
  ncol <- length(obj)

  max_nrow <- max(unlist(lapply(obj, function(o) length(o[["x"]]))))

  x <- matrix(as.double(NA), nrow = max_nrow, ncol = ncol)
  y <- matrix(as.double(NA), nrow = max_nrow, ncol = ncol)

  for (i in seq_along(obj)) {
    x[1:length(obj[[i]][["x"]]), i] <- obj[[i]][["x"]]
    y[1:length(obj[[i]][["y"]]), i] <- obj[[i]][["y"]]
  }

  list(x = x, y = y)
}

#
# Plot average line with CI
#
.plot_avg <- function(obj, curve_name, xlab, ylab, show_ci) {
  # === Create a plot ===
  np <- attr(obj[[1]], "np")
  nn <- attr(obj[[1]], "nn")

  avgcurves <- attr(obj, "avgcurves")

  plot(1, type="l", main = paste0(curve_name, " - P: ", np, ", N: ", nn),
       xlab = xlab, ylab = ylab, ylim = c(0, 1), xlim = c(0, 1))

  if (length(avgcurves) == 1) {
    lcols <- "blue"
  } else {
    lcols <- rainbow(length(avgcurves))
  }

  for (i in 1:length(avgcurves)) {
    .add_curve_with_ci(avgcurves, i, "grey", lcols[i], show_ci)
  }
}

#
# Add a curve with CI
#
.add_curve_with_ci <- function(avgcurves, idx, pcol, lcol, show_ci) {
  x = avgcurves[[idx]][["x"]]
  y = avgcurves[[idx]][["y_avg"]]

  if (show_ci) {
    ymin = avgcurves[[idx]][["y_ci_l"]]
    ymax = avgcurves[[idx]][["y_ci_h"]]

    g <- col2rgb(pcol)
    polygon(c(x, rev(x)), c(ymin, rev(ymax)), border = FALSE,
            col = rgb(g[1], g[2], g[3], 180, maxColorValue = 255))
  }

  b <- col2rgb(lcol)
  lines(x, y, col = rgb(b[1], b[2], b[3], 200, maxColorValue = 255))
}

#
# Show legend
#
.show_legend <- function(obj, show_legend, gnames = "model_names") {
  if (show_legend) {
    old_mar <- par(mar = c(0, 0, 0, 0))
    on.exit(par(old_mar), add = TRUE)
    old_pty <- par(pty = "m")
    on.exit(par(old_pty), add = TRUE)

    gnames <- unique(attr(obj, gnames))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend(x = "top", lty = 1,
           legend = gnames,
           col = rainbow(length(gnames)),
           horiz = TRUE)
  }
}

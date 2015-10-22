#
# Plot ROC and Precision-Recall
#
.plot_multi <- function(x, curvetype = c("ROC", "PRC"), show_ci = FALSE,
                        all_curves = TRUE, add_np_nn = TRUE,
                        show_legend = FALSE, ...) {

  # === Validate input arguments ===
  .check_show_legend(show_legend)
  .validate(x)
  .check_curvetype(curvetype)

  # === Create a plot ===
  show_legend2 <- show_legend
  if ("ROC" %in% curvetype && "PRC" %in% curvetype) {
    if (show_legend) {
      m <- matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE)
      layout(mat = m, heights = c(0.85, 0.15))
    } else {
      m <- matrix(c(1, 2), nrow = 1, ncol = 2)
      layout(mat = m)
    }
    on.exit(layout(1), add = TRUE)
    show_legend2 <- FALSE
  }

  if ("ROC" %in% curvetype) {
    .plot_single(x, "ROC", show_ci = show_ci, all_curves = all_curves,
                 add_np_nn = add_np_nn, show_legend = show_legend2, ...)
  }

  if ("PRC" %in% curvetype) {
    .plot_single(x, "PRC", show_ci = show_ci, all_curves = all_curves,
                 add_np_nn = add_np_nn, show_legend = show_legend2, ...)
  }

  if ("ROC" %in% curvetype && "PRC" %in% curvetype) {
    .show_legend(x, show_legend)
  }
}

#
# matplot wrapper
#
.matplot_wrapper <- function(obj, curvetype, main, xlab, ylab) {

  # === Validate input arguments ===
  .validate(obj[[curvetype]])

  # === Create line colours ===
  model_type <- attr(obj, "model_type")

  if (model_type == "single") {
    line_col <- "black"
  } else {
    line_col <- .make_multi_colors(obj)
  }

  # === Create a plot ===
  mats <- .make_matplot_mats(obj[[curvetype]])

  matplot(mats[["x"]], mats[["y"]], type = "l", lty = 1,
          col = line_col,
          main = main, xlab = xlab, ylab = ylab,
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
# Make colours for multiple models and multiple datasets
#
.make_multi_colors <- function(obj) {
  uniq_modnames <- attr(obj, "uniq_modnames")
  modnames <- attr(obj, "data_info")[["modnames"]]

  uniq_col <- rainbow(length(uniq_modnames))
  modnams_idx <- as.numeric(factor(modnames, levels = uniq_modnames))
  unlist(lapply(seq_along(modnames), function(i) uniq_col[modnams_idx[i]]))
}

#
# Plot average line with CI
#
.plot_avg <- function(obj, curvetype, main, xlab, ylab, show_ci) {
  # === Create a plot ===
  avgcurves <- attr(obj[[curvetype]], "avgcurves")

  plot(1, type = "l", main = main, xlab = xlab, ylab = ylab,
       ylim = c(0, 1), xlim = c(0, 1))

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
# Plot ROC or Precision-Recall
#
.plot_single <- function(x, curvetype, show_ci = FALSE, all_curves = FALSE,
                         add_np_nn = TRUE, show_legend = TRUE, ...) {

  if (curvetype == "ROC") {
    main <- "ROC"
    xlab <- "1 - Specificity"
    ylab <- "Sensitivity"
    ctype <- "rocs"
  } else if (curvetype == "PRC") {
    main <- "Precision-Recall"
    xlab <- "Recall"
    ylab <- "Precision"
    ctype <- "prcs"
  }

  if (add_np_nn) {
    np <- attr(x[[ctype]][[1]], "np")
    nn <- attr(x[[ctype]][[1]], "nn")
    main = paste0(main, " - P: ", np, ", N: ", nn)
  }

  old_pty <- par(pty = "s")
  on.exit(par(old_pty), add = TRUE)

  if (show_legend) {
    m <- matrix(c(1, 2), nrow = 2, ncol = 1)
    layout(mat = m, heights = c(0.85, 0.15))
    on.exit(layout(1), add = TRUE)
  }

  # === Create a plot ===
  if (all_curves) {
    .matplot_wrapper(x, ctype, main, xlab, ylab)
  } else {
    .plot_avg(x, ctype,  main, xlab, ylab, show_ci)
  }

  if (curvetype == "ROC") {
    abline(a = 0, b = 1, col = "grey", lty = 3)

  } else if (curvetype == "PRC" && add_np_nn) {
    abline(h = np / (np + nn), col = "grey", lty = 3)
  }

  .show_legend(x, show_legend)
}

#
# Show legend
#
.show_legend <- function(obj, show_legend, gnames = "modnames") {
  if (show_legend) {
    old_mar <- par(mar = c(0, 0, 0, 0))
    on.exit(par(old_mar), add = TRUE)
    old_pty <- par(pty = "m")
    on.exit(par(old_pty), add = TRUE)

    gnames <- attr(obj, paste0("uniq_", gnames))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend(x = "top", lty = 1,
           legend = gnames,
           col = rainbow(length(gnames)),
           horiz = TRUE)
  }
}

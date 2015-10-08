#
# Plot a ROC curve
#
plot.roc_curve <- function(x, ...) {
  old_pty <- par(pty = "s")
  on.exit(par(old_pty), add = TRUE)

  # === Validate input arguments ===
  .validate(x)

  # === Create a plot ===
  np <- attr(x, "np")
  nn <- attr(x, "nn")

  plot(x[["x"]], x[["y"]], type = "l",
       main = paste("ROC - P: ", np, ", N: ", nn, sep=""),
       xlab = "1 - Specificity", ylab = "Sensitivity")
  abline(a = 0, b = 1, col = "grey", lty = 3)
}

#
# Plot a Precision-Recall curve
#
plot.prc_curve <- function(x, ...) {
  old_pty <- par(pty = "s")
  on.exit(par(old_pty), add = TRUE)

  # === Validate input arguments ===
  .validate(x)

  # === Create a plot ===
  np <- attr(x, "np")
  nn <- attr(x, "nn")

  plot(x[["x"]], x[["y"]], type = "l",
       main = paste("Precision-Recall - P: ", np, ", N: ", nn, sep=""),
       xlab = "Recall", ylab = "Precision",
       ylim = c(0, 1))
  abline(h = np / (np + nn), col = "grey", lty = 3)
}

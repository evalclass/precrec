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
# Show legend
#
.show_legend <- function(obj, show_legend, gnames = "model_names") {
  if (show_legend) {
    old_mar <- par(mar = c(0, 0, 0, 0))
    on.exit(par(old_mar), add = TRUE)
    old_pty <- par(pty = "m")
    on.exit(par(old_pty), add = TRUE)

    gnames <- attr(obj, gnames)
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend(x = "top", lty = 1,
           legend = gnames,
           col = rainbow(length(gnames)),
           horiz = TRUE)
  }
}

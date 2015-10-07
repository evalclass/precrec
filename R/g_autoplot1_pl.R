#
#  Plot score distibutions by rank
#
autoplot.fmdat <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = x))
  p <- p + ggplot2::geom_jitter()
  p <- p + ggplot2::coord_flip()
  p <- .geom_basic(p, "Score distributions by rank", NULL, "rank",
                   show_legend = FALSE)
}

#
# Plot TPs, FNs, FPs, TNs by threshold IDs
#
autoplot.cmats <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = group))
  p <- p + ggplot2::geom_line()
  p <- .geom_basic(p, "TPs, FNs, FPs, and TNs by threshold IDs",
                   "threshold ID", "count", show_legend = TRUE)
}

#
# Plot basic evaluation measures by threshold IDs
#
autoplot.pevals <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::geom_line()
  p <- p + ggplot2::facet_wrap(~ group, ncol = 2)

  p <- .geom_basic(p, "Evaluation measures by threshold IDs",
                   "threshold ID", "evaluation value", show_legend = FALSE)
}

#
# Plot a ROC curve
#
autoplot.roc_curve <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::geom_line()
  p <- .geom_basic_roc(p, object, show_legend = FALSE)
}

#
# Plot a Precision-Recall curve
#
autoplot.prc_curve <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::geom_line()
  p <- .geom_basic_prc(p, object, show_legend = FALSE)
}

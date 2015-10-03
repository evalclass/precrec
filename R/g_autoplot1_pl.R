#
#  Plot score distibutions by rank
#
autoplot.fmdat <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = x))
  p <- p + ggplot2::geom_jitter()
  p <- p + ggplot2::coord_flip()
  p <- .geom_basic(p, "Score distributions by rank", NULL, "rank")
  p <- p + ggplot2::theme(legend.position = "none")

  p
}

#
# Plot TPs, FNs, FPs, TNs by threshold IDs
#
autoplot.cmats <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = group))
  p <- .geom_line_wrapper(p, "TPs, FNs, FPs, and TNs by threshold IDs",
                          "threshold ID", "count")
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())

  p
}

#
# Plot basic evaluation measures by threshold IDs
#
autoplot.pevals <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::facet_wrap(~ group, ncol = 2)

  p <- .geom_line_wrapper(p, "Evaluation measures by threshold IDs",
                          "threshold ID", "evaluation value")

  p
}

#
# Plot a ROC curve
#
autoplot.roc_curve <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  p <- .geom_roc_line_wrapper(p, object)

  p
}

#
# Plot a Precision-Recall curve
#
autoplot.prc_curve <- function(object, ...) {
  df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
  p <- .geom_prc_line_wrapper(p, object)

  p
}

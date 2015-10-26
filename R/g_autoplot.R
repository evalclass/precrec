#
#  Plot score distibutions by rank
#
autoplot.fmdat <- function(object, ...) {
  curve_df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(curve_df, ggplot2::aes(x = x, y = y, color = x))
  p <- p + ggplot2::geom_jitter()
  p <- p + ggplot2::coord_flip()
  p <- .geom_basic(p, "Score distributions by rank", NULL, "rank",
                   show_legend = FALSE)
}

#
# Plot TPs, FNs, FPs, TNs by threshold IDs
#
autoplot.cmats <- function(object, ...) {
  curve_df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(curve_df, ggplot2::aes(x = x, y = y, color = group))
  p <- p + ggplot2::geom_line()
  p <- .geom_basic(p, "TPs, FNs, FPs, and TNs by threshold IDs",
                   "threshold ID", "count", show_legend = TRUE)
}

#
# Plot basic evaluation measures by threshold IDs
#
autoplot.pevals <- function(object, ...) {
  curve_df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(curve_df, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::geom_line()
  p <- p + ggplot2::facet_wrap(~ group, ncol = 2)

  p <- .geom_basic(p, "Evaluation measures by threshold IDs",
                   "threshold ID", "evaluation value", show_legend = FALSE)
}

#' @rdname autoplot
#' @export
autoplot.sscurves <- function(object, curvetype = c("ROC", "PRC"),
                              ret_grob = FALSE, ...) {

  .autoplot_multi(object, curvetype = curvetype, show_ci = FALSE,
                  all_curves = TRUE, show_legend = FALSE, add_np_nn = TRUE,
                  ret_grob = ret_grob)

}

#' @rdname autoplot
#' @export
autoplot.mscurves <- function(object, curvetype = c("ROC", "PRC"),
                              show_legend = TRUE, ret_grob = FALSE, ...) {

  .autoplot_multi(object, curvetype = curvetype, show_ci = FALSE,
                  all_curves = TRUE, show_legend = TRUE, add_np_nn = TRUE,
                  ret_grob = ret_grob)
}

#' @rdname autoplot
#' @export
autoplot.smcurves <- function(object, curvetype = c("ROC", "PRC"),
                              show_ci = TRUE, all_curves = FALSE,
                              ret_grob = FALSE, ...) {

  .autoplot_multi(object, curvetype = curvetype, show_ci = show_ci,
                  all_curves = all_curves, show_legend = FALSE, add_np_nn = TRUE,
                  ret_grob = ret_grob)
}

#' @rdname autoplot
#' @export
autoplot.mmcurves <- function(object, curvetype = c("ROC", "PRC"),
                              show_ci = FALSE, all_curves = FALSE,
                              show_legend = TRUE, ret_grob = FALSE, ...) {

  .autoplot_multi(object, curvetype = curvetype, show_ci = show_ci,
                  all_curves = all_curves, show_legend = show_legend,
                  add_np_nn = TRUE, ret_grob = ret_grob)
}

#' @rdname autoplot
#' @export
autoplot.sspoints <- function(object, type = "p+l",
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"),
                              ret_grob = FALSE, ...) {

  .autoplot_multi(object, type = type, curvetype = curvetype,
                  show_ci = FALSE, all_curves = TRUE,
                  show_legend = FALSE, add_np_nn = TRUE,
                  ret_grob = ret_grob)

}

#' @rdname autoplot
#' @export
autoplot.mspoints <- function(object, type = "p+l",
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"),
                              show_legend = TRUE, ret_grob = FALSE, ...) {

  .autoplot_multi(object, type = type, curvetype = curvetype,
                  show_ci = FALSE, all_curves = TRUE,
                  show_legend = TRUE, add_np_nn = TRUE,
                  ret_grob = ret_grob)
}

#' @rdname autoplot
#' @export
autoplot.smpoints <- function(object, type = "p+l",
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"),
                              show_ci = TRUE, all_curves = FALSE,
                              ret_grob = FALSE, ...) {

  .autoplot_multi(object, type = type, curvetype = curvetype,
                  show_ci = show_ci, all_curves = all_curves,
                  show_legend = FALSE, add_np_nn = TRUE,
                  ret_grob = ret_grob)
}

#' @rdname autoplot
#' @export
autoplot.mmpoints <- function(object, type = "p+l",
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"),
                              show_ci = FALSE, all_curves = FALSE,
                              show_legend = TRUE, ret_grob = FALSE, ...) {

  .autoplot_multi(object, type = type, curvetype = curvetype,
                  show_ci = show_ci, all_curves = all_curves,
                  show_legend = show_legend, add_np_nn = TRUE,
                  ret_grob = ret_grob)
}

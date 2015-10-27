#' @rdname plot
#' @export
plot.sscurves <- function(x, curvetype = c("ROC", "PRC"), ...) {

  .plot_multi(x, curvetype = curvetype, show_ci = FALSE, all_curves = TRUE,
              add_np_nn = TRUE, show_legend = FALSE, ...)

}

#' @rdname plot
#' @export
plot.mscurves <- function(x, curvetype = c("ROC", "PRC"),
                          show_legend = TRUE, ...) {

  .plot_multi(x, curvetype = curvetype, show_ci = FALSE, all_curves = TRUE,
              add_np_nn = TRUE, show_legend = show_legend, ...)
}

#' @rdname plot
#' @export
plot.smcurves <- function(x, curvetype = c("ROC", "PRC"), show_ci = TRUE,
                          all_curves = FALSE, ...) {

  .plot_multi(x, curvetype = curvetype, show_ci = show_ci,
              all_curves = all_curves, add_np_nn = TRUE,
              show_legend = FALSE, ...)

}

#' @rdname plot
#' @export
plot.mmcurves <- function(x, curvetype = c("ROC", "PRC"), show_ci = FALSE,
                          all_curves = FALSE, show_legend = TRUE, ...) {

  .plot_multi(x, curvetype = curvetype, show_ci = show_ci,
              all_curves = all_curves, add_np_nn = TRUE,
              show_legend = show_legend, ...)

}

#' @rdname plot
#' @export
plot.sspoints <- function(x, curvetype = c("error", "accuracy", "specificity",
                                           "sensitivity", "precision"),
                          type = "p", ...) {

  .plot_multi(x, curvetype = curvetype, show_ci = FALSE, all_curves = TRUE,
              add_np_nn = FALSE, show_legend = FALSE, ...)

}

#' @rdname plot
#' @export
plot.mspoints <- function(x, curvetype = c("error", "accuracy", "specificity",
                                           "sensitivity", "precision"),
                          type = "p", show_legend = TRUE, ...) {

  .plot_multi(x, curvetype = curvetype, type = type, show_ci = FALSE,
              all_curves = TRUE, add_np_nn = FALSE,
              show_legend = show_legend, ...)
}

#' @rdname plot
#' @export
plot.smpoints <- function(x, curvetype = c("error", "accuracy", "specificity",
                                           "sensitivity", "precision"),
                          type = "p", show_ci = TRUE,
                          all_curves = FALSE, ...) {

  .plot_multi(x, curvetype = curvetype, type = type, show_ci = show_ci,
              all_curves = all_curves, add_np_nn = FALSE,
              show_legend = FALSE, ...)

}

#' @rdname plot
#' @export
plot.mmpoints <- function(x, curvetype = c("error", "accuracy", "specificity",
                                           "sensitivity", "precision"),
                          type = "p", show_ci = FALSE, all_curves = FALSE,
                          show_legend = TRUE, ...) {

  .plot_multi(x, curvetype = curvetype, type = type, show_ci = show_ci,
              all_curves = all_curves, add_np_nn = FALSE,
              show_legend = show_legend, ...)

}

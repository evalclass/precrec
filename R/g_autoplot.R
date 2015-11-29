#
#  Plot score distributions by rank
#
autoplot.fmdat <- function(object, ...) {
  curve_df <- .prepare_autoplot(object)

  # === Create a ggplot object ===
  p <- ggplot2::ggplot(curve_df,
                       ggplot2::aes_string(x = 'x', y = 'y', color = 'x'))
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
  p <- ggplot2::ggplot(curve_df,
                       ggplot2::aes_string(x = 'x', y = 'y', color = 'group'))
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
  p <- ggplot2::ggplot(curve_df,
                       ggplot2::aes_string(x = 'x', y = 'y'))
  p <- p + ggplot2::geom_line()
  p <- p + ggplot2::facet_wrap(~ group, ncol = 2)

  p <- .geom_basic(p, "Evaluation measures by threshold IDs",
                   "threshold ID", "evaluation value", show_legend = FALSE)
}

#' @rdname autoplot
#' @export
autoplot.sscurves <- function(object, curvetype = c("ROC", "PRC"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "l",
                                   def_show_cb = FALSE, def_raw_curves = TRUE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = FALSE,
                                   def_ret_grob = FALSE, ...)
  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)

}

#' @rdname autoplot
#' @export
autoplot.mscurves <- function(object, curvetype = c("ROC", "PRC"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "l",
                                   def_show_cb = FALSE, def_raw_curves = TRUE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = TRUE,
                                   def_ret_grob = FALSE, ...)
  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)
}

#' @rdname autoplot
#' @export
autoplot.smcurves <- function(object, curvetype = c("ROC", "PRC"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "l",
                                   def_show_cb = TRUE, def_raw_curves = FALSE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = FALSE,
                                   def_ret_grob = FALSE, ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)
}

#' @rdname autoplot
#' @export
autoplot.mmcurves <- function(object, curvetype = c("ROC", "PRC"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "l",
                                   def_show_cb = FALSE, def_raw_curves = FALSE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = TRUE,
                                   def_ret_grob = FALSE, ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)
}

#' @rdname autoplot
#' @export
autoplot.sspoints <- function(object,
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "p",
                                   def_show_cb = FALSE, def_raw_curves = TRUE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = FALSE,
                                   def_ret_grob = FALSE, ...)
  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)

}

#' @rdname autoplot
#' @export
autoplot.mspoints <- function(object,
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "p",
                                   def_show_cb = FALSE, def_raw_curves = TRUE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = TRUE,
                                   def_ret_grob = FALSE, ...)
  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)
}

#' @rdname autoplot
#' @export
autoplot.smpoints <- function(object,
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "p",
                                   def_show_cb = TRUE, def_raw_curves = FALSE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = FALSE,
                                   def_ret_grob = FALSE, ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)
}

#' @rdname autoplot
#' @export
autoplot.mmpoints <- function(object,
                              curvetype = c("error", "accuracy", "specificity",
                                            "sensitivity", "precision"), ...) {

  arglist <- .get_autoplot_arglist(def_curvetype = curvetype, def_type = "p",
                                   def_show_cb = FALSE, def_raw_curves = FALSE,
                                   def_add_np_nn = TRUE,
                                   def_show_legend = TRUE,
                                   def_ret_grob = FALSE, ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .autoplot_multi(object, arglist)
}

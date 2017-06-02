#' @rdname plot
#' @export
plot.sscurves <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = c("ROC", "PRC"),
                               def_type = "l", def_show_cb = FALSE,
                               def_raw_curves = TRUE, def_add_np_nn = TRUE,
                               def_show_legend = FALSE, ...)

  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

#' @rdname plot
#' @export
plot.mscurves <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = c("ROC", "PRC"),
                               def_type = "l", def_show_cb = FALSE,
                               def_raw_curves = TRUE, def_add_np_nn = TRUE,
                               def_show_legend = TRUE, ...)

  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

#' @rdname plot
#' @export
plot.smcurves <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = c("ROC", "PRC"),
                               def_type = "l", def_show_cb = TRUE,
                               def_raw_curves = NULL, def_add_np_nn = TRUE,
                               def_show_legend = FALSE, ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

#' @rdname plot
#' @export
plot.mmcurves <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = c("ROC", "PRC"),
                               def_type = "l", def_show_cb = FALSE,
                               def_raw_curves = NULL, def_add_np_nn = TRUE,
                               def_show_legend = TRUE, ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_rocprc(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

#' @rdname plot
#' @export
plot.sspoints <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = .get_metric_names("basic"),
                               def_type = "p",
                               def_show_cb = FALSE, def_raw_curves = TRUE,
                               def_add_np_nn = TRUE, def_show_legend = FALSE,
                               ...)
  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .plot_multi(x, arglist)


}

#' @rdname plot
#' @export
plot.mspoints <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = .get_metric_names("basic"),
                               def_type = "p",
                               def_show_cb = FALSE, def_raw_curves = TRUE,
                               def_add_np_nn = TRUE, def_show_legend = TRUE,
                               ...)
  arglist[["raw_curves"]] <- TRUE
  arglist[["show_cb"]] <- FALSE
  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

#' @rdname plot
#' @export
plot.smpoints <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = .get_metric_names("basic"),
                               def_type = "p",
                               def_show_cb = TRUE, def_raw_curves = NULL,
                               def_add_np_nn = TRUE, def_show_legend = FALSE,
                               ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

#' @rdname plot
#' @export
plot.mmpoints <- function(x, y = NULL, ...) {

  arglist <- .get_plot_arglist(attr(x, "args"), y,
                               def_curvetype = .get_metric_names("basic"),
                               def_type = "p",
                               def_show_cb = FALSE, def_raw_curves = NULL,
                               def_add_np_nn = TRUE, def_show_legend = TRUE,
                               ...)

  arglist[["curvetype"]] <- .pmatch_curvetype_basic(arglist[["curvetype"]])

  .plot_multi(x, arglist)

}

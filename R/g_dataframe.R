#' @rdname as.data.frame
#' @export
as.data.frame.sscurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = TRUE, ...)

  .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.mscurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = TRUE, ...)

  .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.smcurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = raw_curves, ...)

  .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.mmcurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = raw_curves, ...)

  .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.sspoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = TRUE, ...)

  .dataframe_common(x, mode = "basic", raw_curves = arglist[["raw_curves"]],
                    ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.mspoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = TRUE, ...)

  .dataframe_common(x, mode = "basic", raw_curves = arglist[["raw_curves"]],
                    ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.smpoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = raw_curves, ...)

  .dataframe_common(x, mode = "basic", raw_curves = arglist[["raw_curves"]],
                    ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.mmpoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {

  arglist <- .get_dataframe_arglist(attr(x, "args"),
                                    def_raw_curves = raw_curves, ...)

  .dataframe_common(x, mode = "basic", raw_curves = arglist[["raw_curves"]],
                    ...)
}

#' @rdname as.data.frame
#' @export
as.data.frame.aucroc <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$uaucs
}


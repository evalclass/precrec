#' @rdname as.data.frame
#' @export
as.data.frame.sscurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = TRUE, ...
  )

  .as_plain_df(
    .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.mscurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = TRUE, ...
  )

  .as_plain_df(
    .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.smcurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.mmcurves <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(x, raw_curves = arglist[["raw_curves"]], ...)
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.sspoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = TRUE, ...
  )

  .as_plain_df(
    .dataframe_common(x,
      mode = "basic", raw_curves = arglist[["raw_curves"]],
      ...
    )
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.mspoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = TRUE, ...
  )

  .as_plain_df(
    .dataframe_common(x,
      mode = "basic", raw_curves = arglist[["raw_curves"]],
      ...
    )
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.smpoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(x,
      mode = "basic", raw_curves = arglist[["raw_curves"]],
      ...
    )
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.mmpoints <- function(x, row.names = NULL, optional = FALSE,
                                   raw_curves = NULL, ...) {
  arglist <- .get_dataframe_arglist(attr(x, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(x,
      mode = "basic", raw_curves = arglist[["raw_curves"]],
      ...
    )
  )
}

#' @rdname as.data.frame
#' @export
as.data.frame.aucroc <- function(x, row.names = NULL, optional = FALSE, ...) {
  .as_plain_df(x[["uaucs"]], copy = TRUE)
}

#' @rdname as.data.frame
#' @export
as.data.frame.ssxycurves <- function(x, row.names = NULL, optional = FALSE,
                                     ...) {
  .as_plain_df(.dataframe_xycurves(x, ...))
}

#' @rdname as.data.frame
#' @export
as.data.frame.msxycurves <- function(x, row.names = NULL, optional = FALSE,
                                     ...) {
  .as_plain_df(.dataframe_xycurves(x, ...))
}

#' @rdname as.data.frame
#' @export
as.data.frame.smxycurves <- function(x, row.names = NULL, optional = FALSE,
                                     ...) {
  .as_plain_df(.dataframe_xycurves(x, ...))
}

#' @rdname as.data.frame
#' @export
as.data.frame.mmxycurves <- function(x, row.names = NULL, optional = FALSE,
                                     ...) {
  .as_plain_df(.dataframe_xycurves(x, ...))
}

#
# Convert an object of metric_curve() to a data frame
#
# An xy curve is one x vector and one y vector per dataset, so there is no
# curve type to select and nothing to reduce - the C++ converter the curve
# and point objects go through has nothing to do here.
#
.dataframe_xycurves <- function(obj, check_ggplot = FALSE, ...) {
  if (check_ggplot) {
    .load_ggplot2()
  }
  .validate(obj)

  .xycurve_df(obj)
}

#
# Convert a fmdat object to a data frame for ggplot2
#
#' @exportS3Method
fortify.fmdat <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  data.frame(x = model[["labels"]], y = model[["ranks"]])
}

#
# Convert a cmats object to a data frame for ggplot2
#
#' @exportS3Method
fortify.cmats <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  n <- length(model[["ranks"]])
  data.frame(
    x = rep(seq_along(model[["ranks"]]), 4),
    y = c(
      model[["tp"]], model[["fn"]],
      model[["fp"]], model[["tn"]]
    ),
    group = factor(
      c(
        rep("TPs", n), rep("FNs", n),
        rep("FPs", n), rep("TNs", n)
      ),
      levels = c(
        "TPs", "FNs",
        "FPs", "TNs"
      )
    )
  )
}

#
# Convert a pevals object to a data frame for ggplot2
#
#' @exportS3Method
fortify.pevals <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  pb <- model[["basic"]]
  n <- length(pb[["error"]])

  # The measures the object holds, plus the one derived column that has no
  # place of its own in the table
  mnames <- names(.basic_metric_names())
  vals <- .map(mnames, function(m) pb[[m]])
  names(vals) <- mnames
  vals[["1 - specificity"]] <- 1 - pb[["specificity"]]

  # Kept where it has always sat, between specificity and precision
  gnames <- append(mnames, "1 - specificity",
    after = match("sensitivity", mnames)
  )

  data.frame(
    x = rep(1:n, length(gnames)),
    y = unlist(vals[gnames], use.names = FALSE),
    group = factor(rep(gnames, each = n), levels = gnames)
  )
}

#' @rdname fortify
#' @export
fortify.sscurves <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  # One dataset has no average to contrast a raw curve with.
  .ignore_unused_args(raw_curves)

  .as_plain_df(
    .dataframe_common(model,
      raw_curves = TRUE, reduce_points = reduce_points,
      check_ggplot = TRUE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.mscurves <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  # One dataset has no average to contrast a raw curve with.
  .ignore_unused_args(raw_curves)

  .as_plain_df(
    .dataframe_common(model,
      raw_curves = TRUE, reduce_points = reduce_points,
      check_ggplot = TRUE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.smcurves <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  arglist <- .get_fortify_arglist(attr(model, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(model,
      raw_curves = arglist[["raw_curves"]],
      reduce_points = reduce_points, check_ggplot = TRUE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.mmcurves <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  arglist <- .get_fortify_arglist(attr(model, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(model,
      raw_curves = arglist[["raw_curves"]],
      reduce_points = reduce_points, check_ggplot = TRUE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.sspoints <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  # One dataset has no average to contrast a raw curve with, and the basic
  # measures have no point reduction.
  .ignore_unused_args(raw_curves, reduce_points)

  .as_plain_df(
    .dataframe_common(model,
      mode = "basic", raw_curves = TRUE,
      check_ggplot = TRUE, reduce_points = FALSE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.mspoints <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  # One dataset has no average to contrast a raw curve with, and the basic
  # measures have no point reduction.
  .ignore_unused_args(raw_curves, reduce_points)

  .as_plain_df(
    .dataframe_common(model,
      mode = "basic", raw_curves = TRUE,
      check_ggplot = TRUE, reduce_points = FALSE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.smpoints <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  # The basic measures have no point reduction.
  .ignore_unused_args(reduce_points)

  arglist <- .get_fortify_arglist(attr(model, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(model,
      mode = "basic", raw_curves = arglist[["raw_curves"]],
      check_ggplot = TRUE, reduce_points = FALSE, ...
    )
  )
}

#' @rdname fortify
#' @export
fortify.mmpoints <- function(model, data, raw_curves = NULL,
                             reduce_points = FALSE,
                             ...) {
  # The basic measures have no point reduction.
  .ignore_unused_args(reduce_points)

  arglist <- .get_fortify_arglist(attr(model, "args"),
    def_raw_curves = raw_curves, ...
  )

  .as_plain_df(
    .dataframe_common(model,
      mode = "basic", raw_curves = arglist[["raw_curves"]],
      check_ggplot = TRUE, reduce_points = FALSE, ...
    )
  )
}

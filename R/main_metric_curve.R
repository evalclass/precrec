#' Draw one evaluation metric against another
#'
#' The `metric_curve` function takes the name of a metric for the x axis
#'   and the name of a metric for the y axis and calculates one curve per
#'   test dataset, in the manner of `ROCR::performance`. Every metric
#'   [evalmod()] can calculate is available on both axes.
#'
#' @param mdat An `S3` object created by the [mmdata()] function.
#'   It contains formatted scores and labels. The `metric_curve`
#'   function ignores `scores` and `labels` when `mdat`
#'   is specified. These arguments are internally passed to the
#'   [mmdata()] function when `mdat` is unspecified. In that case,
#'   both `scores` and `labels` must be at least specified.
#'
#' @param scores A numeric dataset of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list.
#'
#' @param labels A numeric, character, logical, or factor dataset
#'   of observed labels. It can be a vector, a matrix, an array,
#'   a data frame, or a list.
#'
#' @param x_metric A string that specifies the metric of the x axis. It
#'   accepts every name [evalmod()] accepts for `metrics`, together
#'   with the identifiers `ROCR` uses. The default `"fpr"` with
#'   the default `y_metric` reproduces `ROCR`'s most common call.
#'
#' @param y_metric A string that specifies the metric of the y axis.
#'
#' @param modnames A character vector for the names of the models.
#'
#' @param dsids A numeric vector for the dataset IDs.
#'
#' @param posclass A string or a numeric value to specify the label of
#'   positives.
#'
#' @param na_worst A Boolean value for the ties method of `NA`s.
#'
#' @param ties_method A string for the ties method.
#'
#' @param x_bins An integer for the number of supporting points of a
#'   registered pair. It is ignored for every other pair, which has no
#'   interpolation to place supporting points on.
#'
#' @param interpolate A Boolean value to specify whether or not
#'   interpolation of a registered pair is performed.
#'
#' @param cost_fp A numeric value for the cost of a false positive, used
#'   when one of the two axes is the `cost` metric. See [evalmod()].
#'
#' @param cost_fn A numeric value for the cost of a false negative.
#'
#' @param ... These additional arguments are passed to [mmdata()]
#'   for data preparation.
#'
#' @return The `metric_curve` function returns an `S3` object of
#'   one of the following classes, chosen the way [evalmod()] chooses
#'   between its own: `ssxycurves`, `msxycurves`,
#'   `smxycurves` and `mmxycurves`. The object holds one curve per
#'   test dataset, and works with `print`, `as.data.frame`,
#'   `fortify`, `plot` and `autoplot`.
#'
#' @section Which pairs are joined by a line:
#'
#' `precrec` exists because the points of a precision-recall curve must not
#'   be joined by straight lines. The metrics this function reads are raw
#'   per-cutoff values with no interpolation, so joining an arbitrary pair
#'   of them would be the very error the package was written to avoid.
#'
#' Two pairs have a defined interpolation, and only those two are drawn as
#'   curves: `x_metric = "fpr"` with `y_metric = "sensitivity"`,
#'   which is the ROC curve, and `x_metric = "sensitivity"` with
#'   `y_metric = "precision"`, which is the precision-recall curve. For
#'   those, `metric_curve` hands the work to the same code
#'   `evalmod(mode = "rocprc")` uses, so the two cannot disagree.
#'
#' Every other pair is drawn as points. Pass `type = "l"` to
#'   `plot` or `autoplot` to join them anyway, having decided that
#'   the straight lines mean something for the pair at hand.
#'
#' @section What this function does not do:
#'
#' `metric_curve` draws one curve per test dataset and does not average
#'   over them. An average needs a rule for interpolating between the
#'   points of each curve, which is exactly what an unregistered pair does
#'   not have. Use `evalmod(calc_avg = TRUE)` for averaged ROC and
#'   precision-recall curves.
#'
#' @seealso [evalmod()] for the metrics themselves and for averaged
#'   ROC and precision-recall curves. [mmdata()] for formatting input
#'   data. [autoplot()] and [plot()] for the plots.
#'
#' @examples
#'
#' ##################################################
#' ### The ROC curve, the way ROCR asks for it
#' ###
#' samps <- create_sim_samples(1, 50, 50, "good_er")
#' xy1 <- metric_curve(
#'   scores = samps[["scores"]], labels = samps[["labels"]],
#'   x_metric = "fpr", y_metric = "sensitivity"
#' )
#' xy1
#'
#' ##################################################
#' ### A pair with no interpolation, drawn as points
#' ###
#' xy2 <- metric_curve(
#'   scores = samps[["scores"]], labels = samps[["labels"]],
#'   x_metric = "predicted_positive_rate", y_metric = "lift"
#' )
#' xy2
#'
#' ##################################################
#' ### Multiple models and multiple test datasets
#' ###
#' samps2 <- create_sim_samples(3, 50, 50, c("poor_er", "good_er"))
#' mdat <- mmdata(samps2[["scores"]], samps2[["labels"]],
#'   modnames = samps2[["modnames"]], dsids = samps2[["dsids"]]
#' )
#' xy3 <- metric_curve(mdat, x_metric = "score", y_metric = "precision")
#' xy3
#'
#' @export
metric_curve <- function(mdat, scores = NULL, labels = NULL,
                         x_metric = "fpr", y_metric = "sensitivity",
                         modnames = NULL, dsids = NULL, posclass = NULL,
                         na_worst = TRUE, ties_method = "equiv",
                         x_bins = 1000, interpolate = TRUE,
                         cost_fp = 1, cost_fn = 1, ...) {
  # === Validate input arguments ===
  x_metric <- .validate_metric_arg(x_metric, "x_metric")
  y_metric <- .validate_metric_arg(y_metric, "y_metric")
  new_ties_method <- .pmatch_tiesmethod(ties_method, ...)
  new_na_worst <- .get_new_naworst(na_worst, ...)
  .validate_data_args(
    modnames, dsids, posclass, new_na_worst, new_ties_method
  )
  .validate_x_bins(x_bins)
  .validate_interpolate(interpolate)
  .validate_costs(cost_fp, cost_fn)

  if (x_bins == 0) {
    x_bins <- 1
  }

  # Create mdat if not provided
  if (missing(mdat)) {
    mdat <- mmdata(scores, labels,
      modnames = modnames, dsids = dsids, posclass = posclass,
      na_worst = new_na_worst, ties_method = new_ties_method, ...
    )
  }
  .validate(mdat)

  # === Project the two metrics ===
  curve <- .joinable_curve(x_metric, y_metric)
  if (is.na(curve)) {
    xy <- .xy_from_points(mdat, x_metric, y_metric, cost_fp, cost_fn)
  } else {
    xy <- .xy_from_curves(mdat, curve, x_bins, interpolate)
  }

  # === Create an S3 object ===
  model_type <- .get_single_or_multiple(mdat, "uniq_modnames")
  dataset_type <- .get_single_or_multiple(mdat, "uniq_dsids")
  class_name_pf <- .make_prefix(model_type, dataset_type)

  s3obj <- structure(list(xy = xy), class = c(
    paste0(class_name_pf, "xycurves"), "xycurve_info"
  ))

  attr(s3obj, "x_metric") <- x_metric
  attr(s3obj, "y_metric") <- y_metric
  attr(s3obj, "curve") <- curve
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "dataset_type") <- dataset_type
  attr(s3obj, "args") <- list(
    mode = "xycurve",
    x_metric = x_metric,
    y_metric = y_metric,
    x_bins = x_bins,
    interpolate = interpolate,
    cost_fp = cost_fp,
    cost_fn = cost_fn
  )
  attr(s3obj, "validated") <- FALSE

  .validate(s3obj)
}

#
# Metric pairs whose points have a defined interpolation
#
# Only a pair listed here may be joined into a curve; every other pair is
# drawn as points, because joining raw per-cutoff points with straight lines
# is exactly the error this package exists to avoid. Adding a row here is how
# a new joinable pair is registered - the code below reads this table and
# nothing else, and a registered pair is calculated by the curve pipeline
# rather than by anything of this function's own, so the two cannot drift
# apart.
#
.joinable_pairs <- function() {
  data.frame(
    x = c("fpr", "sensitivity"),
    y = c("sensitivity", "precision"),
    curve = c("ROC", "PRC"),
    stringsAsFactors = FALSE
  )
}

#
# The curve type of a metric pair, or NA when it has no interpolation
#
.joinable_curve <- function(x_metric, y_metric) {
  pairs <- .joinable_pairs()
  hit <- which(pairs$x == x_metric & pairs$y == y_metric)
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  pairs$curve[[hit[1]]]
}

#
# Resolve and check one of the two axis arguments
#
.validate_metric_arg <- function(metric, arg) {
  .assert_string(metric, arg)
  metric <- .pmatch_curvetype_basic(metric)
  .assert_string(metric, arg, .get_metric_names("basic_all"))

  metric
}

#
# Project two basic metrics against each other, one curve per dataset
#
.xy_from_points <- function(mdat, x_metric, y_metric, cost_fp = 1,
                            cost_fn = 1) {
  metrics <- unique(c(x_metric, y_metric))
  # `cb_alpha = NULL` because `calc_avg = FALSE` otherwise warns that the
  # default confidence level is being ignored - which it is, and which this
  # caller has no way of not asking for
  points <- evalmod(mdat,
    mode = "basic", metrics = metrics,
    calc_avg = FALSE, cb_alpha = NULL,
    cost_fp = cost_fp, cost_fn = cost_fn
  )
  short <- .basic_metric_names(.get_obj_metrics(points))

  .map_idx(attr(mdat, "data_info")[["modnames"]], function(i) {
    list(
      x = points[[short[[x_metric]]]][[i]][["y"]],
      y = points[[short[[y_metric]]]][[i]][["y"]]
    )
  })
}

#
# Take a registered pair from the curve pipeline
#
# The whole point of the registry: a pair that has an interpolation is
# calculated by the code that already implements it, so `metric_curve()`
# cannot ship a second, differing ROC or precision-recall curve.
#
.xy_from_curves <- function(mdat, curve, x_bins, interpolate) {
  ctype <- if (curve == "ROC") "rocs" else "prcs"
  curves <- evalmod(mdat,
    mode = "rocprc", calc_avg = FALSE, cb_alpha = NULL,
    x_bins = x_bins, interpolate = interpolate
  )

  .map(curves[[ctype]], function(cv) list(x = cv[["x"]], y = cv[["y"]]))
}

#
# Validate an xycurves object generated by metric_curve()
#
.validate_xycurves_common <- function(x, class_name) {
  # Need to validate only once
  if (methods::is(x, class_name) && attr(x, "validated")) {
    return(x)
  }

  attr_names <- c(
    "x_metric", "y_metric", "curve", "data_info", "uniq_modnames",
    "uniq_dsids", "model_type", "dataset_type", "args", "validated"
  )
  arg_names <- c(
    "mode", "x_metric", "y_metric", "x_bins", "interpolate",
    "cost_fp", "cost_fn"
  )
  .validate_basic(x, class_name, "metric_curve", "xy", attr_names, arg_names)

  .assert_internal(
    length(x[["xy"]]) == length(attr(x, "data_info")[["modnames"]])
  )
  for (cv in x[["xy"]]) {
    .assert_internal(
      is.numeric(cv[["x"]]),
      is.numeric(cv[["y"]]),
      length(cv[["x"]]) == length(cv[["y"]])
    )
  }

  attr(x, "validated") <- TRUE
  x
}

#
# Validate 'ssxycurves' object generated by metric_curve()
#
.validate.ssxycurves <- function(x) {
  .validate_xycurves_common(x, "ssxycurves")
}

#
# Validate 'msxycurves' object generated by metric_curve()
#
.validate.msxycurves <- function(x) {
  .validate_xycurves_common(x, "msxycurves")
}

#
# Validate 'smxycurves' object generated by metric_curve()
#
.validate.smxycurves <- function(x) {
  .validate_xycurves_common(x, "smxycurves")
}

#
# Validate 'mmxycurves' object generated by metric_curve()
#
.validate.mmxycurves <- function(x) {
  .validate_xycurves_common(x, "mmxycurves")
}

#
# The data frame behind every one of this object's methods
#
.xycurve_df <- function(obj) {
  data_info <- attr(obj, "data_info")
  modnames <- data_info[["modnames"]]
  dsids <- data_info[["dsids"]]

  parts <- .map_idx(obj[["xy"]], function(i) {
    cv <- obj[["xy"]][[i]]
    n <- length(cv[["x"]])
    data.table::data.table(
      x = cv[["x"]],
      y = cv[["y"]],
      modname = rep(modnames[i], n),
      dsid = rep(dsids[i], n),
      type = rep(.xycurve_label(obj), n)
    )
  })

  df <- .rbind_parts(parts)

  # The curve and point frames carry these three as factors, in the order the
  # object lists them rather than alphabetically, and the plotting code reads
  # that order back out. This frame is no different.
  data.table::set(df, j = "modname", value = factor(
    df[["modname"]],
    levels = attr(obj, "uniq_modnames")
  ))
  data.table::set(df, j = "dsid", value = factor(
    df[["dsid"]],
    levels = attr(obj, "uniq_dsids")
  ))
  data.table::set(df, j = "type", value = factor(df[["type"]]))

  df
}

#
# How a pair names itself in the `type` column of a data frame
#
# The canonical metric names, so that a caller can match on them.
#
.xycurve_label <- function(obj) {
  paste(attr(obj, "y_metric"), "vs", attr(obj, "x_metric"))
}

#
# How a pair names itself in a plot title
#
# The titles the axes are labelled with, so the three read as one sentence
# rather than as two spellings of the same metric.
#
.xycurve_title_label <- function(obj) {
  paste(
    .get_metric_title(attr(obj, "y_metric")), "vs",
    .get_metric_title(attr(obj, "x_metric"))
  )
}

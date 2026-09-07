#
# Create ROC and Precision-Recall curves
#
create_curves <- function(pevals, scores = NULL, labels = NULL,
                          x_bins = 1000, keep_pevals = FALSE, ...) {
  # === Validate input arguments ===
  # Create pevals from scores and labels if pevals is missing
  pevals <- .create_src_obj(
    pevals, "pevals", calc_metrics, scores, labels,
    ...
  )

  if (is.null(x_bins) || any(is.na(x_bins))) {
    x_bins <- 1
  }
  .validate_x_bins(x_bins, allow_zero = TRUE)
  .validate(pevals)

  # === Create ROC and Precision-Recall curves ===
  roc_curve <- create_roc(pevals,
    x_bins = x_bins,
    keep_pevals = keep_pevals, ...
  )
  prc_curve <- create_prc(pevals,
    x_bins = x_bins,
    keep_pevals = keep_pevals, ...
  )

  curves <- list(roc = roc_curve, prc = prc_curve)

  # === Create an S3 object ===
  s3obj <- structure(curves, class = "curves")

  # Set attributes
  attr(s3obj, "modname") <- attr(pevals, "modname")
  attr(s3obj, "dsid") <- attr(pevals, "dsid")
  attr(s3obj, "nn") <- attr(pevals, "nn")
  attr(s3obj, "np") <- attr(pevals, "np")
  attr(s3obj, "args") <- c(list(x_bins = x_bins), list(...))
  if (keep_pevals) {
    attr(s3obj, "src") <- pevals
  } else {
    attr(s3obj, "src") <- NA
  }
  attr(s3obj, "validated") <- FALSE

  # Call .validate.curves()
  .validate(s3obj)
}

#
# Create a ROC curve
#
create_roc <- function(pevals, scores = NULL, labels = NULL, x_bins = 1000,
                       keep_pevals = FALSE, ...) {
  # === Create a ROC curve ===
  .create_curve(
    "specificity", "sensitivity", create_roc_curve,
    "create_roc_curve", "roc_curve", pevals, scores, labels,
    x_bins, keep_pevals, ...
  )
}

#
# Create a Precision-Recall curve
#
create_prc <- function(pevals, scores = NULL, labels = NULL, x_bins = 1000,
                       keep_pevals = FALSE, ...) {
  # === Create a Precision-Recall curve ===
  .create_curve(
    "sensitivity", "precision", create_prc_curve,
    "create_prc_curve", "prc_curve", pevals, scores, labels,
    x_bins, keep_pevals, ...
  )
}

#
# Average precision of a precision-recall curve
#
# AP = sum over i of (r_i - r_{i-1}) * p_i, the step estimator: the precision
# at each cutoff weighted by the recall it gained over the one before. The
# sum starts at the second point, so the precision of the empty prediction
# set - which is 0/0, and which every tool defines differently - never enters
# it. That is the whole reason the metric is defined this way.
#
# It is deliberately not the area under the curve. `create_prc_curve()`
# interpolates between the raw points the way Davis and Goadrich showed is
# correct, and `calc_auc()` integrates that; AP joins the raw points with
# horizontal steps instead and so reads high wherever the two differ. It is
# reported because other packages report it, and because the gap between the
# two numbers is worth being able to see.
#
# Returns NA for a ROC curve: the metric is defined on precision against
# recall, and `.create_curve()` builds both from the same code path.
#
.calc_average_precision <- function(pb, x_name, y_name) {
  if (x_name != "sensitivity" || y_name != "precision") {
    return(NA_real_)
  }

  rec <- pb[[x_name]]
  prec <- pb[[y_name]]
  n <- length(rec)
  if (n < 2L) {
    return(NA_real_)
  }

  sum((rec[2:n] - rec[1:(n - 1)]) * prec[2:n])
}

#
# Create ROC or Precision-Recall curve
#
.create_curve <- function(x_name, y_name, func, func_name, class_name,
                          pevals, scores = NULL, labels = NULL, x_bins = 1000,
                          keep_pevals = FALSE, ...) {
  # === Validate input arguments ===
  # Create pevals from scores and labels if pevals is missing
  pevals <- .create_src_obj(
    pevals, "pevals", calc_metrics, scores, labels,
    ...
  )
  .validate_x_bins(x_bins, allow_zero = TRUE)
  .validate(pevals)

  # === Create a curve ===
  # Calculate a curve
  pb <- pevals[["basic"]]
  crv <- func(
    attr(pevals, "src")[["tp"]], attr(pevals, "src")[["fp"]],
    pb[[x_name]], pb[[y_name]], x_bins
  )
  .check_cpp_func_error(crv, func_name)

  # Average precision, on the raw per-cutoff points rather than on the
  # interpolated curve below - that is what makes it a different estimator
  # from the area under the curve, not just a different way of adding it up.
  ap <- .calc_average_precision(pb, x_name, y_name)

  # Calculate AUC
  auc <- calc_auc(crv[["curve"]][["x"]], crv[["curve"]][["y"]])
  if (auc[["errmsg"]] == "invalid-x-vals") {
    warning(paste0(
      "Invalid ", x_name,
      " values detected. AUC can be inaccurate."
    ))
  } else {
    .check_cpp_func_error(auc, "calc_auc")
  }


  # === Create an S3 object ===
  s3obj <- structure(crv[["curve"]], class = class_name)

  # Set attributes
  attr(s3obj, "modname") <- attr(pevals, "modname")
  attr(s3obj, "dsid") <- attr(pevals, "dsid")
  attr(s3obj, "nn") <- attr(pevals, "nn")
  attr(s3obj, "np") <- attr(pevals, "np")
  attr(s3obj, "auc") <- auc[["auc"]]
  attr(s3obj, "ap") <- ap
  attr(s3obj, "xlim") <- c(0, 1)
  attr(s3obj, "ylim") <- c(0, 1)
  attr(s3obj, "pauc") <- NA
  attr(s3obj, "spauc") <- NA
  attr(s3obj, "args") <- c(list(x_bins = x_bins), list(...))
  attr(s3obj, "cpp_errmsg1") <- crv[["errmsg"]]
  attr(s3obj, "cpp_errmsg2") <- auc[["errmsg"]]
  if (keep_pevals) {
    attr(s3obj, "src") <- pevals
  } else {
    attr(s3obj, "src") <- NA
  }
  attr(s3obj, "validated") <- FALSE

  # Call .validate.roc_curve() or .validate.prc_curve()
  .validate(s3obj)
}

#
# Create placeholder curves for a dataset that holds a single class
#
# ROC and precision-recall curves are undefined when every label is positive
# or every label is negative, so `on_single_class = "na"` asks for a curve of
# NAs instead of an error. That keeps one degenerate fold of an n-fold run
# from aborting the whole evaluation: the fold still occupies its row in
# `auc()` and in the plots, with NA where a number would be.
#
.create_na_curves <- function(fmdat, x_bins = 1000) {
  if (is.null(x_bins) || any(is.na(x_bins)) || x_bins < 2) {
    x_bins <- 2
  }

  # The same supporting points a real curve would be interpolated onto, so
  # that averaging across datasets lines the x values up
  xs <- seq(0, 1, length.out = x_bins + 1)

  cfunc <- function(class_name) {
    s3obj <- structure(
      list(
        x = xs,
        y = rep(NA_real_, length(xs)),
        orig_points = rep(TRUE, length(xs))
      ),
      class = class_name
    )

    attr(s3obj, "modname") <- attr(fmdat, "modname")
    attr(s3obj, "dsid") <- attr(fmdat, "dsid")
    attr(s3obj, "nn") <- attr(fmdat, "nn")
    attr(s3obj, "np") <- attr(fmdat, "np")
    attr(s3obj, "auc") <- NA_real_
    attr(s3obj, "ap") <- NA_real_
    attr(s3obj, "xlim") <- c(0, 1)
    attr(s3obj, "ylim") <- c(0, 1)
    attr(s3obj, "pauc") <- NA
    attr(s3obj, "spauc") <- NA
    attr(s3obj, "args") <- list(x_bins = x_bins)
    attr(s3obj, "cpp_errmsg1") <- ""
    attr(s3obj, "cpp_errmsg2") <- ""
    attr(s3obj, "src") <- NA
    attr(s3obj, "validated") <- FALSE

    .validate(s3obj)
  }

  curves <- list(roc = cfunc("roc_curve"), prc = cfunc("prc_curve"))

  # === Create an S3 object ===
  s3obj <- structure(curves, class = "curves")

  # Set attributes
  attr(s3obj, "modname") <- attr(fmdat, "modname")
  attr(s3obj, "dsid") <- attr(fmdat, "dsid")
  attr(s3obj, "nn") <- attr(fmdat, "nn")
  attr(s3obj, "np") <- attr(fmdat, "np")
  attr(s3obj, "args") <- list(x_bins = x_bins)
  attr(s3obj, "src") <- NA
  attr(s3obj, "validated") <- FALSE

  # Call .validate.curves()
  .validate(s3obj)
}

#
# Validate 'roc_curve' object generated by create_roc()
#
.validate.roc_curve <- function(x) {
  # Need to validate only once
  if (methods::is(x, "roc_curve") && attr(x, "validated")) {
    return(x)
  }

  # Validate class items and attributes
  .validate_curve(x, "roc_curve", "create_roc")

  attr(x, "validated") <- TRUE
  x
}

#
# Validate 'prc_curve' object generated by create_roc()
#
.validate.prc_curve <- function(x) {
  # Need to validate only once
  if (methods::is(x, "prc_curve") && attr(x, "validated")) {
    return(x)
  }

  # Validate class items and attributes
  .validate_curve(x, "prc_curve", "create_prc")

  attr(x, "validated") <- TRUE
  x
}

#
# Validate 'roc_curve' or 'prc_curve'
#
.validate_curve <- function(obj, class_name, func_name) {
  # Validate class items and attributes
  item_names <- c("x", "y", "orig_points")
  attr_names <- c(
    "modname", "dsid", "nn", "np", "auc", "ap", "args",
    "cpp_errmsg1", "cpp_errmsg2", "src", "validated"
  )
  arg_names <- c(
    "x_bins", "na_worst", "na.last", "ties_method", "ties.method",
    "modname", "dsid", "keep_fmdat", "keep_cmats"
  )
  .validate_basic(
    obj, class_name, func_name, item_names, attr_names,
    arg_names
  )

  # Check values of class items
  if ((length(obj[["x"]]) != length(obj[["y"]])) ||
    (length(obj[["x"]]) != length(obj[["orig_points"]]))) {
    stop("x, y, and orig_points must be all the same lengths", call. = FALSE)
  } else if (!(length(obj[["x"]]) > 2)) {
    stop("The minimum length of x, y, and orig_points must be 3",
      call. = FALSE
    )
  }

  # Check values of class attributes
  # AUC. A placeholder curve for a single-class dataset has none.
  auc <- attr(obj, "auc")
  .assert_internal(is.na(auc) || ((auc >= 0) && (auc <= 1)))
}

#
# Validate 'curves' object generated by create_curves()
#
.validate.curves <- function(x) {
  # Need to validate only once
  if (methods::is(x, "curves") && attr(x, "validated")) {
    return(x)
  }

  # Validate class items and attributes
  item_names <- c("roc", "prc")
  attr_names <- c(
    "modname", "dsid", "nn", "np", "args", "src",
    "validated"
  )
  arg_names <- c(
    "x_bins", "na_worst", "na.last", "ties_method", "ties.method",
    "modname", "dsid", "keep_fmdat", "keep_cmats"
  )
  .validate_basic(
    x, "curves", "calc_metrics", item_names, attr_names,
    arg_names
  )

  # Check values of class items
  x[["roc"]] <- .validate(x[["roc"]])
  x[["prc"]] <- .validate(x[["prc"]])

  attr(x, "validated") <- TRUE
  x
}

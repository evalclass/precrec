#
# Create ROC and Precision-Recall curves
#
create_curves <- function(pevals, x_interval = 0.001, scores = NULL,
                          labels = NULL, keep_cmats = FALSE, ...) {

  # === Validate input arguments ===
  # Create pevals from scores and labels if pevals is missing
  pevals <- .create_src_obj(pevals, "pevals", calc_measures, scores, labels,
                            ...)
  if (is.null(x_interval) || is.na(x_interval)) {
    x_interval = 1
  }
  .validate_x_interval(x_interval)
  .validate(pevals)

  # === Create ROC and Precision-Recall curves ===
  roc_curve <- create_roc(pevals, x_interval, keep_cmats = keep_cmats, ...)
  prc_curve <- create_prc(pevals, x_interval, keep_cmats = keep_cmats, ...)
  curves <- list(roc = roc_curve, prc = prc_curve)

  # === Create an S3 object ===
  s3obj <- structure(curves, class = "curves")

  # Set attributes
  attr(s3obj, "modname") <- attr(pevals, "modname")
  attr(s3obj, "setid") <- attr(pevals, "setid")
  attr(s3obj, "nn") <- attr(pevals, "nn")
  attr(s3obj, "np") <- attr(pevals, "np")
  attr(s3obj, "args") <- c(list(x_interval = x_interval), list(...))
  attr(s3obj, "src") <- pevals
  attr(s3obj, "validated") <- FALSE

  # Call .validate.curves()
  .validate(s3obj)
}

#
# Create a ROC curve
#
create_roc <- function(pevals, x_interval = 0.001, scores = NULL, labels = NULL,
                       keep_cmats = FALSE, ...) {
  # === Create a ROC curve ===
  .create_curve("specificity", "sensitivity", create_roc_curve,
                "create_roc_curve", "roc_curve", pevals, x_interval,
                scores, labels, keep_cmats = keep_cmats, ...)
}

#
# Create a Precision-Recall curve
#
create_prc <- function(pevals, x_interval = 0.001, scores = NULL, labels = NULL,
                       keep_cmats = FALSE, ...) {

  # === Create a Precision-Recall curve ===
  .create_curve("sensitivity", "precision", create_prc_curve,
                "create_prc_curve", "prc_curve", pevals, x_interval,
                scores, labels, keep_cmats = keep_cmats, ...)
}

#
# Create ROC or Precision-Recall curve
#
.create_curve <- function(x_name, y_name, func, func_name, class_name,
                          pevals, x_interval = 0.001, scores = NULL,
                          labels = NULL, keep_cmats = FALSE, ...) {

  # === Validate input arguments ===
  # Create pevals from scores and labels if pevals is missing
  pevals <- .create_src_obj(pevals, "pevals", calc_measures, scores, labels,
                            ...)
  .validate_x_interval(x_interval)
  .validate(pevals)

  # === Create a curve ===
  # Calculate a curve
  crv <- func(attr(pevals, "src")[["tp"]], attr(pevals, "src")[["fp"]],
              pevals[[x_name]], pevals[[y_name]], x_interval)
  .check_cpp_func_error(crv, func_name)

  # Calculate AUC
  auc <- calc_auc(crv[["x"]], crv[["y"]])
  if (auc[["errmsg"]] == "invalid-x-vals") {
    warning(paste0("Invalid ", x_name,
                   " values detected. AUC can be inaccurate."))
  } else {
    .check_cpp_func_error(auc, "calc_auc")
  }

  if (!keep_cmats) {
    attr(pevals, "src") = NA
  }

  # === Create an S3 object ===
  cpp_errmsg1 <- crv[["errmsg"]]
  cpp_errmsg2 <- auc[["errmsg"]]
  crv[["errmsg"]] <- NULL
  s3obj <- structure(crv, class = class_name)

  # Set attributes
  attr(s3obj, "modname") <- attr(pevals, "modname")
  attr(s3obj, "setid") <- attr(pevals, "setid")
  attr(s3obj, "nn") <- attr(pevals, "nn")
  attr(s3obj, "np") <- attr(pevals, "np")
  attr(s3obj, "auc") <- auc[["auc"]]
  attr(s3obj, "partial") <- FALSE
  attr(s3obj, "pauc") <- NA
  attr(s3obj, "x_limits") <- c(0, 1)
  attr(s3obj, "y_limits") <- c(0, 1)
  attr(s3obj, "args") <- c(list(x_interval = x_interval), list(...))
  attr(s3obj, "cpp_errmsg1") <- cpp_errmsg1
  attr(s3obj, "cpp_errmsg2") <- cpp_errmsg2
  attr(s3obj, "src") <- pevals
  attr(s3obj, "validated") <- FALSE

  # Call .validate.roc_curve() or .validate.prc_curve()
  .validate(s3obj)
}

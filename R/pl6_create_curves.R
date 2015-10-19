#
# Create ROC and Precision-Recall curves
#
create_curves <- function(pevals, scores = NULL, labels = NULL,
                          x_bins = 1000, keep_pevals = FALSE, ...) {

  # === Validate input arguments ===
  # Create pevals from scores and labels if pevals is missing
  pevals <- .create_src_obj(pevals, "pevals", calc_measures, scores, labels,
                                 ...)

  if (is.null(x_bins) || is.na(x_bins)) {
    x_bins = 1
  }
  .validate_x_bins(x_bins)
  .validate(pevals)

  # === Create ROC and Precision-Recall curves ===
  roc_curve <- create_roc(pevals, x_bins = x_bins,
                          keep_pevals = keep_pevals, ...)
  prc_curve <- create_prc(pevals, x_bins = x_bins,
                          keep_pevals = keep_pevals, ...)

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
  .create_curve("specificity", "sensitivity", create_roc_curve,
                "create_roc_curve", "roc_curve", pevals, scores, labels,
                x_bins, keep_pevals, ...)
}

#
# Create a Precision-Recall curve
#
create_prc <- function(pevals, scores = NULL, labels = NULL, x_bins = 1000,
                       keep_pevals = FALSE, ...) {

  # === Create a Precision-Recall curve ===
  .create_curve("sensitivity", "precision", create_prc_curve,
                "create_prc_curve", "prc_curve", pevals, scores, labels,
                x_bins, keep_pevals, ...)
}

#
# Create ROC or Precision-Recall curve
#
.create_curve <- function(x_name, y_name, func, func_name, class_name,
                          pevals, scores = NULL, labels = NULL, x_bins = 1000,
                          keep_pevals = FALSE, ...) {

  # === Validate input arguments ===
  # Create pevals from scores and labels if pevals is missing
  pevals <- .create_src_obj(pevals, "pevals", calc_measures, scores, labels,
                            ...)
  .validate_x_bins(x_bins)
  .validate(pevals)

  # === Create a curve ===
  # Calculate a curve
  pb <- pevals[["basic"]]
  crv <- func(attr(pevals, "src")[["tp"]], attr(pevals, "src")[["fp"]],
              pb[[x_name]], pb[[y_name]], x_bins)
  .check_cpp_func_error(crv, func_name)

  # Calculate AUC
  auc <- calc_auc(crv[["curve"]][["x"]], crv[["curve"]][["y"]])
  if (auc[["errmsg"]] == "invalid-x-vals") {
    warning(paste0("Invalid ", x_name,
                   " values detected. AUC can be inaccurate."))
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

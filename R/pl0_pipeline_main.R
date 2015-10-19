#
# Control the main pipeline iterations
#
pl_main <- function(mdat, calc_avg = TRUE, ci_alpha = 0.05, all_curves = FALSE,
                    x_bins = 1000, orig_points = TRUE) {

  # === Validation ===
  .validate_pl_main_args(mdat, calc_avg, ci_alpha, all_curves,
                         x_bins, orig_points)

  # Create model_type and dataset_type
  if (length(attr(mdat, "uniq_modnames")) == 1L) {
    model_type <- "single"
  } else {
    model_type <- "multiple"
  }

  if (length(attr(mdat, "uniq_dsids")) == 1L) {
    dataset_type <- "single"
  } else {
    dataset_type <- "multiple"
  }

  # === Create ROC and Precision-Recall curves ===
  # Define a function for each iteration
  plfunc <- function(s) {
    cdat <- create_confmats(mdat[[s]])
    pevals <- calc_measures(cdat)
    curves <- create_curves(pevals, x_bins = x_bins)
  }

  # Create curves
  lcurves <- lapply(seq_along(mdat), plfunc)
  pf <- .make_prefix(model_type, dataset_type)
  rocs <- .group_curves(lcurves, "roc", paste0(pf, "roc"), mdat)
  prcs <- .group_curves(lcurves, "prc", paste0(pf, "prc"), mdat)

  # Calculate the average curves
  if (dataset_type == "multiple" && calc_avg) {
    attr(rocs, "avgcurves") <- calc_avg(rocs, ci_alpha, x_bins)
    attr(prcs, "avgcurves") <- calc_avg(prcs, ci_alpha, x_bins)
  }

  # === Create an S3 object ===
  s3obj <- structure(list(rocs = rocs, prcs = prcs),
                     class = paste0(pf, "curves"))

  # Set attributes
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "dataset_type") <- dataset_type
  attr(s3obj, "args") <- list(x_bins = x_bins,
                              calc_avg = calc_avg,
                              ci_alpha = ci_alpha)
  attr(s3obj, "src") <- mdat
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Make prefix
#
.make_prefix <- function(model_type, dataset_type) {
  mt <- ""
  if (model_type == "single") {
    mt <- "s"
  } else if (model_type == "multiple") {
    mt <- "m"
  }

  dt <- ""
  if (dataset_type == "single") {
    dt <- "s"
  } else if (dataset_type == "multiple") {
    dt <- "m"
  }

  paste0(mt, dt)
}

#
# Get ROC or Precision-Recall curves from mcurves
#
.group_curves <- function(lcurves, curve_type, class_name, mdat) {
  # Group ROC or PRC curves
  mc <- lapply(seq_along(lcurves), function(s) lcurves[[s]][[curve_type]])

  # === Create an S3 object ===
  s3obj <- structure(mc, class = class_name)

  # Set attributes
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "avgcurve") <- NA
  attr(s3obj, "src") <- mdat
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

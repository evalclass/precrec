#
# Control the main pipeline iterations
#
pl_main_rocprc <- function(mdat, calc_avg = TRUE, ci_alpha = 0.05,
                           all_curves = FALSE, x_bins = 1000,
                           orig_points = TRUE) {

  # === Validation ===
  .validate_pl_main_rocprc_args(mdat, calc_avg, ci_alpha, all_curves,
                                x_bins, orig_points)

  # Create model_type and dataset_type
  model_type <- .get_single_or_multiple(mdat, "uniq_modnames")
  dataset_type <- .get_single_or_multiple(mdat, "uniq_dsids")


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
  rocs <- .group_curves(lcurves, "roc", "crvgrp", mdat)
  prcs <- .group_curves(lcurves, "prc", "crvgrp", mdat)

  # Summarize AUC
  aucs <- .group_aucs(lcurves, mdat)

  # Calculate the average curves
  if (dataset_type == "multiple" && calc_avg) {
    attr(rocs, "avgcurves") <- calc_avg(rocs, ci_alpha, x_bins)
    attr(prcs, "avgcurves") <- calc_avg(prcs, ci_alpha, x_bins)
  }

  # === Create an S3 object ===
  s3obj <- structure(list(rocs = rocs, prcs = prcs),
                     class = c(paste0(pf, "curves"), "curve_info"))

  # Set attributes
  attr(s3obj, "aucs") <- aucs
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "dataset_type") <- dataset_type
  attr(s3obj, "args") <- list(calc_avg = calc_avg,
                              ci_alpha = ci_alpha,
                              all_curves = all_curves,
                              x_bins = x_bins,
                              orig_points = orig_points)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Validate arguments of pl_main_rocprc()
#
.validate_pl_main_rocprc_args <- function(mdat, calc_avg, ci_alpha, all_curves,
                                          x_bins, orig_points) {

  # Validate mdat
  .validate(mdat)


  # Validate calc_avg
  .validate_calc_avg(calc_avg)

  # Validate ci_alpha
  .validate_ci_alpha(ci_alpha)

  # Validate all_curves
  .validate_all_curves(all_curves)


  # Check x_bins
  .validate_x_bins(x_bins)

  # Check orig_points
  .validate_orig_points(orig_points)

}

#
# Determin either "single" or "multiple" for model_type and data_type
#
.get_single_or_multiple <- function(mdat, attr_name) {
  if (length(attr(mdat, attr_name)) == 1L) {
    single_or_multiple <- "single"
  } else {
    single_or_multiple <- "multiple"
  }
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
  attr(s3obj, "curve_type") <- attr(mdat, "curve_type")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "avgcurve") <- NA
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Get AUCs
#
.group_aucs <- function(lcurves, mdat) {

  # Group AUC of ROC or PRC curves
  modnames <- attr(mdat, "data_info")[["modnames"]]
  dsids <- attr(mdat, "data_info")[["dsids"]]
  aucs <- data.frame(modnames = rep(modnames, each = 2),
                     dsids = rep(dsids, each = 2),
                     curvetypes = rep(c("ROC", "PRC"), length(modnames)),
                     aucs = rep(NA, length(modnames) * 2),
                     stringsAsFactors = FALSE)

  j <- 1
  for (i in seq_along(lcurves)) {
    aucs[["aucs"]][j] <- attr(lcurves[[i]][["roc"]], "auc")
    j <- j + 1
    aucs[["aucs"]][j] <- attr(lcurves[[i]][["prc"]], "auc")
    j <- j + 1
  }

  aucs
}



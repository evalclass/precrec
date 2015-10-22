#
# Control the main pipeline iterations for basic evaluation messures
#
.pl_main_basic <- function(mdat, model_type, dataset_type, class_name_pf,
                           calc_avg = TRUE, ci_alpha = 0.05,
                           all_curves = FALSE) {

  # === Create ROC and Precision-Recall curves ===
  # Create points
  plfunc <- function(s) {
    cdat <- create_confmats(mdat[[s]])
    pevals <- calc_measures(cdat)
  }
  lpoints <- lapply(seq_along(mdat), plfunc)

  # Group points by evaluation measure
  grpfunc <- function(m) {
    .group_points(lpoints, m, "evalgrp", mdat, dataset_type,
                  calc_avg, ci_alpha)
  }
  grp_points <- lapply(c("error", "accuracy", "specificity", "sensitivity",
                         "precision"), grpfunc)
  names(points_peval)<- c("err", "acc", "sp", "sn", "prec")

  # === Create an S3 object ===
  s3obj <- structure(grp_points, class = c(paste0(class_name_pf, "points"),
                                           "point_info"))

  # Set attributes
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "dataset_type") <- dataset_type
  attr(s3obj, "args") <- list(calc_avg = calc_avg,
                              ci_alpha = ci_alpha,
                              all_curves = all_curves)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Get evaluation measures at all threshold values by models
#
.group_points <- function(lcurves, eval_type, class_name, mdat, dataset_type,
                          calc_avg, ci_alpha) {

  # Group by basic evaluation measure
  grp_func <- function(s) {
    list(x = lcurves[[s]][["threshold"]], y = lcurves[[s]][[eval_type]])
  }
  pevals <- lapply(seq_along(lcurves), grp_func)

  # === Create an S3 object ===
  s3obj <- structure(pevals, class = class_name)

  # Set attributes
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "curve_type") <- attr(mdat, "curve_type")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "avgpoints") <- NA
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  s3obj <- .validate(s3obj)

  # Calculate the average curves
  if (dataset_type == "multiple" && calc_avg) {
    attr(s3obj, "avgcurves") <- calc_avg_basic(s3obj, ci_alpha)
  }

  s3obj

}

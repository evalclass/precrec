#
# Control the main pipeline iterations
#
pl_main <- function(mdat, model_type = "single", data_type = "single",
                    x_bins = 1000, calc_avg = TRUE, ci_level = 0.95) {

  # === Validation ===
  .validate(mdat)
  model_type <- .pmatch_model_data_types(model_type)
  data_type <- .pmatch_model_data_types(data_type)
  .validate_pl_main_args(mdat, model_type, data_type, x_bins, calc_avg,
                         ci_level)

  # === Create ROC and Precision-Recall curves ===
  # Define a function for each iteration
  plfunc <- function(s) {
    cdat <- create_confmats(mdat[[s]])
    pevals <- calc_measures(cdat)
    curves <- create_curves(pevals, x_bins = x_bins)
  }

  # Create curves
  lcurves <- lapply(seq_along(mdat), plfunc)
  pf <- .make_prefix(model_type, data_type)
  rocs <- .group_curves(lcurves, "roc", paste0(pf, "roc"), mdat)
  prcs <- .group_curves(lcurves, "prc", paste0(pf, "prc"), mdat)

  # Calculate the average curves
  if (data_type == "multiple" && calc_avg) {
    modnames <- attr(mdat, "modnames")
    dsids <- attr(mdat, "dsids")

    attr(rocs, "avgcurves") <- calc_avg(rocs, modnames, dsids,
                                        x_bins, ci_level)
    attr(prcs, "avgcurves") <- calc_avg(prcs, modnames, dsids,
                                        x_bins, ci_level)
  }

  # === Create an S3 object ===
  s3obj <- structure(list(rocs = rocs, prcs = prcs),
                     class = paste0(pf, "curves"))

  # Set attributes
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "data_type") <- data_type
  attr(s3obj, "modnames") <- attr(mdat, "modnames")
  attr(s3obj, "dsids") <- attr(mdat, "dsids")
  attr(s3obj, "args") <- list(x_bins = x_bins,
                              calc_avg = calc_avg,
                              ci_level = ci_level)
  attr(s3obj, "src") <- mdat
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Check partial match
#
.pmatch_model_data_types <- function(val) {
  if (assertthat::is.string(val)) {
    if (val == "single" || val == "multiple") {
      return(val)
    }

    if (!is.na(pmatch(val, "single"))) {
      return("single")
    }

    if (!is.na(pmatch(val, "multiple"))) {
      return("multiple")
    }
  }

  val
}

#
# Make prefix
#
.make_prefix <- function(model_type, data_type) {
  mt <- ""
  if (model_type == "single") {
    mt <- "s"
  } else if (model_type == "multiple") {
    mt <- "m"
  }

  dt <- ""
  if (data_type == "single") {
    dt <- "s"
  } else if (data_type == "multiple") {
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
  attr(s3obj, "modnames") <- attr(mdat, "modnames")
  attr(s3obj, "dsids") <- attr(mdat, "dsids")
  attr(s3obj, "avgcurve") <- NA
  attr(s3obj, "src") <- mdat
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

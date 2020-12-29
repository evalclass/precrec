#
# Control the main pipeline iterations for basic evaluation measures
#
.pl_main_basic <- function(mdat, model_type, dataset_type, class_name_pf,
                           calc_avg = TRUE, cb_alpha = 0.05,
                           raw_curves = FALSE) {

  if (dataset_type == "single") {
    calc_avg <- FALSE
    raw_curves <- TRUE
  }

  # === Calculate evaluation measure ===
  # Create points
  plfunc <- function(s) {
    if (attr(mdat[[s]], "nn") == 0 || attr(mdat[[s]], "np") == 0) {
      if (attr(mdat[[s]], "np") > 0) {
        cl <- "positive"
      } else {
        cl <- "negative"
      }
      err_msg <- paste0("Basic measures cannot be calculated. ",
                        "Only a single class (", cl, ") ",
                        "found in dataset (modname: ", attr(mdat[[s]], "modname"),
                        ", dsid: ",attr(mdat[[s]], "dsid"), ").")
      stop(err_msg, call. = FALSE)
    }
    cdat <- create_confmats(mdat[[s]], keep_fmdat = TRUE)
    pevals <- calc_measures(cdat)
  }
  lpoints <- lapply(seq_along(mdat), plfunc)

  # Summarize points by evaluation measure
  grpfunc <- function(m) {
    .summarize_points(lpoints, m, "pointgrp", mdat, dataset_type,
                      calc_avg, cb_alpha)
  }
  eval_names <- c("score", "label", "error", "accuracy", "specificity",
                  "sensitivity", "precision", "mcc", "fscore")
  grp_row_names <- c("score", "label", "err", "acc", "sp", "sn", "prec", "mcc",
                     "fscore")
  grp_points <- lapply(eval_names, grpfunc)
  names(grp_points) <- grp_row_names

  # Summarize basic evaluation measures
  eval_summary <- .summarize_basic(lpoints, mdat)

  # Summarize average
  grpfunc2 <- function(et) {
    attr(grp_points[[et]], "avgcurves")
  }
  grp_avg <- lapply(names(grp_points), grpfunc2)
  names(grp_avg) <- names(grp_points)

  # === Create an S3 object ===
  if (dataset_type == "multiple" && calc_avg && !raw_curves) {
    grpfunc3 <- function(m) {
      .summarize_points(NULL, m, "pointgrp", mdat, NULL, NULL, NULL)
    }
    grp_points <- lapply(eval_names, grpfunc3)
    names(grp_points) <- grp_row_names
  }
  s3obj <- structure(grp_points, class = c(paste0(class_name_pf, "points"),
                                           "beval_info"))

  # Set attributes
  attr(s3obj, "eval_summary") <- eval_summary
  attr(s3obj, "grp_avg") <- grp_avg
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "dataset_type") <- dataset_type
  attr(s3obj, "args") <- list(mode = "basic",
                              calc_avg = calc_avg,
                              cb_alpha = cb_alpha,
                              raw_curves = raw_curves)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Get evaluation measures at all ranks by models
#
.summarize_points <- function(lpoints, eval_type, class_name, mdat,
                              dataset_type, calc_avg, cb_alpha) {

  if (!is.null(lpoints)) {
    # Summarize basic evaluation measures
    grp_func <- function(s) {
      list(x = lpoints[[s]][["basic"]][["rank"]],
           y = lpoints[[s]][["basic"]][[eval_type]])
    }
    pevals <- lapply(seq_along(lpoints), grp_func)

    # Calculate the average curves
    if (dataset_type == "multiple" && calc_avg) {
      modnames <- attr(mdat, "data_info")[["modnames"]]
      uniq_modnames <- attr(mdat, "uniq_modnames")
      avgcurves <- calc_avg_basic(pevals, modnames, uniq_modnames, cb_alpha)
    } else {
      avgcurves <- NA
    }
  } else {
    pevals <- NA
    avgcurves <- NA
  }


  # === Create an S3 object ===
  s3obj <- structure(pevals, class = class_name)

  # Set attributes
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "eval_type") <- eval_type
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "avgcurves") <- avgcurves
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  s3obj <- .validate(s3obj)

  s3obj
}

#
# Summarize basic evaluation measures
#
.summarize_basic <- function(lpoints, mdat) {

  # Summarize AUC of ROC or PRC curves
  modnames <- attr(mdat, "data_info")[["modnames"]]
  dsids <- attr(mdat, "data_info")[["dsids"]]
  evaltypes <- c("rank", "score", "label", "error", "accuracy",
                 "specificity","sensitivity", "precision", "mcc", "fscore")
  elen <- length(evaltypes)

  sbasic <- data.frame(modnames = rep(modnames, each = elen),
                       dsids = rep(dsids, each = elen),
                       evaltypes = rep(evaltypes, length(modnames)),
                       minvals = rep(NA, length(modnames) * elen),
                       q25vals = rep(NA, length(modnames) * elen),
                       medianvals = rep(NA, length(modnames) * elen),
                       meanvals = rep(NA, length(modnames) * elen),
                       q75vals = rep(NA, length(modnames) * elen),
                       maxvals = rep(NA, length(modnames) * elen),
                       stringsAsFactors = FALSE)

  for (i in seq_along(lpoints)) {
    for (j in seq_along(evaltypes)) {
      vals <- lpoints[[i]][["basic"]][[evaltypes[j]]]
      sbasic[(i - 1) * length(evaltypes) + j, 4:9] <- summary(vals)[1:6]
    }
  }

  sbasic
}

#
# Validate point object generated by .pl_main_basic()
#
.validate_points_common <- function(points, class_name) {
  # Need to validate only once
  if (methods::is(points, class_name) && attr(points, "validated")) {
    return(points)
  }

  # Validate class items and attributes
  item_names <- c("score", "label", "err", "acc", "sp", "sn", "prec", "mcc",
                  "fscore")
  attr_names <- c("eval_summary", "grp_avg", "data_info", "uniq_modnames",
                  "uniq_dsids", "model_type", "dataset_type", "args",
                  "validated")
  arg_names <- c("mode", "calc_avg", "cb_alpha", "raw_curves")
  .validate_basic(points, class_name, ".pl_main_basic", item_names, attr_names,
                  arg_names)

  attr(points, "validated") <- TRUE
  points
}

#
# Validate 'sspoints' object generated by .pl_main_basic()
#
.validate.sspoints <- function(sspoints) {
  .validate_points_common(sspoints, "sspoints")
}

#
# Validate 'mspoints' object generated by .pl_main_basic()
#
.validate.mspoints <- function(mspoints) {
  .validate_points_common(mspoints, "mspoints")
}

#
# Validate 'smpoints' object generated by .pl_main_basic()
#
.validate.smpoints <- function(smpoints) {
  .validate_points_common(smpoints, "smpoints")
}

#
# Validate 'mmpoints' object generated by .pl_main_basic()
#
.validate.mmpoints <- function(mmpoints) {
  .validate_points_common(mmpoints, "mmpoints")
}

#
# Validate 'pointgrp' object generated by .summarize_points()
#
.validate.pointgrp <- function(pointgrp) {
  # Need to validate only once
  if (methods::is(pointgrp, "pointgrp") && attr(pointgrp, "validated")) {
    return(pointgrp)
  }

  # Validate class items and attributes
  item_names <- NULL
  attr_names <- c("data_info", "eval_type", "uniq_modnames", "uniq_dsids",
                  "avgcurves", "validated")
  arg_names <- NULL
  .validate_basic(pointgrp, "pointgrp", ".summarize_points", item_names,
                  attr_names, arg_names)

  attr(pointgrp, "validated") <- TRUE
  pointgrp
}

#
# Control the main pipeline iterations for basic evaluation metrics
#
.pl_main_basic <- function(mdat, model_type, dataset_type, class_name_pf,
                           calc_avg = TRUE, cb_alpha = 0.05,
                           raw_curves = FALSE, beta = 1, metrics = NULL,
                           cost_fp = 1, cost_fn = 1, x_bins = 1000,
                           basic_ties = "split") {
  metric_names <- .basic_metric_names(.resolve_metrics(metrics))
  # Only the metrics derived in R reach calc_metrics(); the C++ layer has
  # always produced the rest, and handing it the whole list once per dataset
  # would make it look the table up for nothing
  derived <- setdiff(names(metric_names), .get_metric_names("basic"))
  if (dataset_type == "single") {
    calc_avg <- FALSE
    raw_curves <- TRUE
  }

  # === Calculate evaluation metric ===
  # Create points
  plfunc <- function(s) {
    if (attr(mdat[[s]], "nn") == 0 || attr(mdat[[s]], "np") == 0) {
      # Accuracy and error rate are still defined here, and the metrics that
      # are not - specificity without negatives, sensitivity without
      # positives - come back as NA, so this is a warning rather than the
      # error the curve pipelines raise
      warning(
        .single_class_msg(
          mdat[[s]], "Some basic metrics cannot be calculated."
        ),
        call. = FALSE
      )
    }
    cdat <- create_confmats(mdat[[s]],
      keep_fmdat = TRUE,
      hold_ties = basic_ties == "hold"
    )
    calc_metrics(cdat,
      beta = beta, metrics = derived,
      cost_fp = cost_fp, cost_fn = cost_fn
    )
  }
  lpoints <- .map_idx(mdat, plfunc)

  # Summarize points by evaluation metric
  grpfunc <- function(m) {
    .summarize_points(
      lpoints, m, "pointgrp", mdat, dataset_type,
      calc_avg, cb_alpha
    )
  }
  eval_names <- names(metric_names)
  grp_row_names <- unname(metric_names)
  grp_points <- .map(eval_names, grpfunc)
  names(grp_points) <- grp_row_names

  # Summarize basic evaluation metrics
  eval_summary <- .summarize_basic(lpoints, mdat, eval_names)

  # Summarize average
  grpfunc2 <- function(et) {
    attr(grp_points[[et]], "avgcurves")
  }
  grp_avg <- .map(names(grp_points), grpfunc2)
  names(grp_avg) <- names(grp_points)

  # === Create an S3 object ===
  if (dataset_type == "multiple" && calc_avg && !raw_curves) {
    grpfunc3 <- function(m) {
      .summarize_points(NULL, m, "pointgrp", mdat, NULL, NULL, NULL)
    }
    grp_points <- .map(eval_names, grpfunc3)
    names(grp_points) <- grp_row_names
  }
  s3obj <- structure(grp_points, class = c(
    paste0(class_name_pf, "points"),
    "beval_info"
  ))

  # Set attributes
  attr(s3obj, "metrics") <- eval_names
  attr(s3obj, "eval_summary") <- eval_summary
  attr(s3obj, "grp_avg") <- grp_avg
  attr(s3obj, "data_info") <- attr(mdat, "data_info")
  attr(s3obj, "uniq_modnames") <- attr(mdat, "uniq_modnames")
  attr(s3obj, "uniq_dsids") <- attr(mdat, "uniq_dsids")
  attr(s3obj, "model_type") <- model_type
  attr(s3obj, "dataset_type") <- dataset_type
  attr(s3obj, "args") <- list(
    mode = "basic",
    calc_avg = calc_avg,
    cb_alpha = cb_alpha,
    raw_curves = raw_curves,
    beta = beta,
    metrics = metrics,
    cost_fp = cost_fp,
    cost_fn = cost_fn,
    # Carried for the plots, not for the calculation: the basic metrics have
    # one point per cutoff whatever x_bins says, and it is the number kept
    # when `reduce_points` thins them for drawing.
    x_bins = x_bins,
    basic_ties = basic_ties
  )
  attr(s3obj, "validated") <- FALSE

  # Call .validate.class_name()
  .validate(s3obj)
}

#
# Get evaluation metrics at all ranks by models
#
.summarize_points <- function(lpoints, eval_type, class_name, mdat,
                              dataset_type, calc_avg, cb_alpha) {
  if (!is.null(lpoints)) {
    # Summarize basic evaluation metrics
    grp_func <- function(pt) {
      list(
        x = pt[["basic"]][["rank"]],
        y = pt[["basic"]][[eval_type]]
      )
    }
    pevals <- .map(lpoints, grp_func)

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
# Summarize basic evaluation metrics
#
.summarize_basic <- function(lpoints, mdat, eval_names) {
  # Summarize AUC of ROC or PRC curves
  modnames <- attr(mdat, "data_info")[["modnames"]]
  dsids <- attr(mdat, "data_info")[["dsids"]]
  evaltypes <- c("rank", eval_names)
  elen <- length(evaltypes)

  # Filled as a matrix first: assigning a row into a data frame inside the
  # loop copied the whole frame on every pass
  quantiles <- matrix(NA_real_, nrow = length(modnames) * elen, ncol = 6)
  for (i in seq_along(lpoints)) {
    for (j in seq_along(evaltypes)) {
      vals <- lpoints[[i]][["basic"]][[evaltypes[j]]]
      quantiles[(i - 1) * elen + j, ] <- summary(vals)[1:6]
    }
  }

  data.table::data.table(
    modnames = rep(modnames, each = elen),
    dsids = rep(dsids, each = elen),
    evaltypes = rep(evaltypes, length(modnames)),
    minvals = quantiles[, 1],
    q25vals = quantiles[, 2],
    medianvals = quantiles[, 3],
    meanvals = quantiles[, 4],
    q75vals = quantiles[, 5],
    maxvals = quantiles[, 6]
  )
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
  item_names <- unname(.basic_metric_names(.get_obj_metrics(points)))
  attr_names <- c(
    "metrics", "eval_summary", "grp_avg", "data_info", "uniq_modnames",
    "uniq_dsids", "model_type", "dataset_type", "args",
    "validated"
  )
  arg_names <- c(
    "mode", "calc_avg", "cb_alpha", "raw_curves", "beta", "metrics",
    "cost_fp", "cost_fn", "x_bins", "basic_ties"
  )
  .validate_basic(
    points, class_name, ".pl_main_basic", item_names, attr_names,
    arg_names
  )

  attr(points, "validated") <- TRUE
  points
}

#
# Validate 'sspoints' object generated by .pl_main_basic()
#
.validate.sspoints <- function(x) {
  .validate_points_common(x, "sspoints")
}

#
# Validate 'mspoints' object generated by .pl_main_basic()
#
.validate.mspoints <- function(x) {
  .validate_points_common(x, "mspoints")
}

#
# Validate 'smpoints' object generated by .pl_main_basic()
#
.validate.smpoints <- function(x) {
  .validate_points_common(x, "smpoints")
}

#
# Validate 'mmpoints' object generated by .pl_main_basic()
#
.validate.mmpoints <- function(x) {
  .validate_points_common(x, "mmpoints")
}

#
# Validate 'pointgrp' object generated by .summarize_points()
#
.validate.pointgrp <- function(x) {
  # Need to validate only once
  if (methods::is(x, "pointgrp") && attr(x, "validated")) {
    return(x)
  }

  # Validate class items and attributes
  item_names <- NULL
  attr_names <- c(
    "data_info", "eval_type", "uniq_modnames", "uniq_dsids",
    "avgcurves", "validated"
  )
  arg_names <- NULL
  .validate_basic(
    x, "pointgrp", ".summarize_points", item_names,
    attr_names, arg_names
  )

  attr(x, "validated") <- TRUE
  x
}

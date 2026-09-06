#
# Control the main pipeline iterations
#
pl_main <- function(mdat, mode = "rocprc", calc_avg = TRUE, cb_alpha = 0.05,
                    raw_curves = FALSE, x_bins = 1000, interpolate = TRUE,
                    na_worst = TRUE, ties_method = "equiv", beta = 1,
                    on_single_class = "error", metrics = NULL,
                    cost_fp = 1, cost_fn = 1, validate = TRUE) {
  # === Validation ===
  new_mode <- .pmatch_mode(mode)
  on_single_class <- .pmatch_on_single_class(on_single_class)
  if (validate) {
    .validate_pl_main_args(
      mdat, new_mode, calc_avg, cb_alpha, raw_curves,
      x_bins, interpolate, beta, on_single_class, metrics,
      cost_fp, cost_fn
    )
  }

  # Create model_type and dataset_type
  model_type <- .get_single_or_multiple(mdat, "uniq_modnames")
  dataset_type <- .get_single_or_multiple(mdat, "uniq_dsids")
  class_name_pf <- .make_prefix(model_type, dataset_type)

  if (new_mode == "rocprc") {
    .pl_main_rocprc(mdat, model_type, dataset_type, class_name_pf,
      calc_avg = calc_avg, cb_alpha = cb_alpha,
      raw_curves = raw_curves, x_bins = x_bins,
      interpolate = interpolate, on_single_class = on_single_class
    )
  } else if (new_mode == "basic") {
    .pl_main_basic(mdat, model_type, dataset_type, class_name_pf,
      calc_avg = calc_avg, cb_alpha = cb_alpha,
      raw_curves = raw_curves, beta = beta, metrics = metrics,
      cost_fp = cost_fp, cost_fn = cost_fn
    )
  } else if (new_mode == "aucroc") {
    .pl_main_aucroc(mdat, model_type, dataset_type, class_name_pf,
      calc_avg = calc_avg, cb_alpha = cb_alpha,
      raw_curves = raw_curves, na_worst = na_worst,
      ties_method = ties_method, on_single_class = on_single_class
    )
  }
}

#
# Check partial match - mode
#
.pmatch_mode <- function(val) {
  if (.is_string(val)) {
    if (val == "rocprc" || val == "basic") {
      return(val)
    } else if (val == "prcroc") {
      return("rocprc")
    }

    if (!is.na(pmatch(val, "rocprc"))) {
      return("rocprc")
    }

    if (!is.na(pmatch(val, "prcroc"))) {
      return("rocprc")
    }

    if (!is.na(pmatch(val, "basic"))) {
      return("basic")
    }

    if (!is.na(pmatch(val, "aucroc"))) {
      return("aucroc")
    }
  }

  val
}

#
# Check partial match - on_single_class
#
.pmatch_on_single_class <- function(val) {
  if (.is_string(val)) {
    if (val == "error" || val == "na") {
      return(val)
    }

    if (!is.na(pmatch(val, "error"))) {
      return("error")
    }

    if (!is.na(pmatch(val, "na"))) {
      return("na")
    }
  }

  val
}

#
# Validate arguments of pl_main
#
.validate_pl_main_args <- function(mdat, mode, calc_avg, cb_alpha, raw_curves,
                                   x_bins, interpolate, beta = 1,
                                   on_single_class = "error",
                                   metrics = NULL, cost_fp = 1,
                                   cost_fn = 1) {
  # Validate mdat
  .validate(mdat)
  if (mode != "aucroc" && !is.null(mdat) && length(mdat) > 0 &&
    is(mdat[[1]], "sdat")) {
    stop("Invalid 'mode' value in 'mdata'", call. = FALSE)
  }

  # Check mode
  .validate_mode(mode)

  # Validate calc_avg
  .validate_calc_avg(calc_avg)

  # Validate cb_alpha
  .validate_cb_alpha(cb_alpha, NULL)

  # Validate raw_curves
  .validate_raw_curves(raw_curves, NULL)


  # Check x_bins
  .validate_x_bins(x_bins)


  # Check interpolate
  .validate_interpolate(interpolate)

  # Check beta
  .validate_beta(beta)

  # Check on_single_class
  .validate_on_single_class(on_single_class)

  # Check metrics
  .resolve_metrics(metrics)

  # Check the misclassification costs
  .validate_costs(cost_fp, cost_fn)
}

#
# Determine either "single" or "multiple" for model_type and data_type
#
.get_single_or_multiple <- function(mdat, attr_name) {
  if (length(attr(mdat, attr_name)) == 1L) {
    single_or_multiple <- "single"
  } else {
    single_or_multiple <- "multiple"
  }

  single_or_multiple
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

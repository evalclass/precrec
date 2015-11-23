#
# Control the main pipeline iterations
#
pl_main <- function(mdat, mode = "rocprc", calc_avg = TRUE, cb_alpha = 0.05,
                    raw_curves = FALSE, x_bins = 1000, validate = TRUE) {

  # === Validation ===
  new_mode <- .pmatch_mode(mode)
  if (validate) {
    .validate_pl_main_args(mdat, new_mode, calc_avg, cb_alpha, raw_curves,
                           x_bins)
  }

  # Create model_type and dataset_type
  model_type <- .get_single_or_multiple(mdat, "uniq_modnames")
  dataset_type <- .get_single_or_multiple(mdat, "uniq_dsids")
  class_name_pf <- .make_prefix(model_type, dataset_type)

  if (new_mode == "rocprc") {
    .pl_main_rocprc(mdat, model_type, dataset_type, class_name_pf,
                    calc_avg = calc_avg, cb_alpha = cb_alpha,
                    raw_curves = raw_curves, x_bins = x_bins)
  } else if (new_mode == "basic") {
    .pl_main_basic(mdat, model_type, dataset_type, class_name_pf,
                   calc_avg = calc_avg, cb_alpha = cb_alpha,
                   raw_curves = raw_curves)
  }
}

#
# Check partial match - mode
#
.pmatch_mode <- function(val) {
  if (assertthat::is.string(val)) {
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
  }

  val
}

#
# Validate arguments of pl_main
#
.validate_pl_main_args <- function(mdat, mode, calc_avg, cb_alpha, raw_curves,
                                   x_bins) {

  # Validate mdat
  .validate(mdat)


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

#
# Control the main pipeline iterations
#
pl_main <- function(mdat, mode = "rocprc", calc_avg = TRUE, ci_alpha = 0.05,
                    raw_curves = FALSE, x_bins = 1000) {

  # === Validation ===
  .validate_pl_main_args(mdat, calc_avg, ci_alpha, raw_curves, x_bins)

  # Create model_type and dataset_type
  model_type <- .get_single_or_multiple(mdat, "uniq_modnames")
  dataset_type <- .get_single_or_multiple(mdat, "uniq_dsids")
  class_name_pf <- .make_prefix(model_type, dataset_type)

  if (mode == "rocprc") {
    .pl_main_rocprc(mdat, model_type, dataset_type, class_name_pf,
                    calc_avg = calc_avg, ci_alpha = ci_alpha,
                    raw_curves = raw_curves, x_bins = x_bins)
  } else if (mode == "basic") {
    .pl_main_basic(mdat, model_type, dataset_type, class_name_pf,
                   calc_avg = calc_avg, ci_alpha = ci_alpha,
                   raw_curves = raw_curves)
  }
}

#
# Validate arguments of pl_main
#
.validate_pl_main_args <- function(mdat, calc_avg, ci_alpha, raw_curves,
                                   x_bins) {

  # Validate mdat
  .validate(mdat)


  # Validate calc_avg
  .validate_calc_avg(calc_avg)

  # Validate ci_alpha
  .validate_ci_alpha(ci_alpha)

  # Validate raw_curves
  .validate_raw_curves(raw_curves)


  # Check x_bins
  .validate_x_bins(x_bins)

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

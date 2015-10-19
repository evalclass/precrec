#
# Calculate the average curve for a model
#
calc_avg <- function(curves, ci_alpha, x_bins) {

  # === Validate input arguments ===
  .validate_ci_alpha(ci_alpha)
  .validate_x_bins(x_bins)
  .validate(curves)

  modnames <- attr(curves, "data_info")[["modnames"]]
  uniq_modnames <- attr(curves, "uniq_modnames")

  # === Summarize curves by by models ===
  # Quantile of CI
  ci_q <- qnorm((1.0 - ci_alpha) + (ci_alpha * 0.5))

  # Filter curves by model
  ffunc <- function(mname) {
    curves[modnames == mname]
  }
  curves_by_model <- lapply(uniq_modnames, ffunc)

  # Summarize curves
  vfunc <- function(i) {
    avgs <- calc_avg_curve(curves_by_model[[i]], x_bins, ci_q)
    .check_cpp_func_error(avgs, "calc_avg_curve")
    avgs[["avg"]]
  }
  avgcurves <- lapply(seq_along(curves_by_model), vfunc)

  # === Create an S3 object ===
  s3obj <- structure(avgcurves, class = "avgcurves")

  # Set attributes
  attr(s3obj, "uniq_modnames") <- uniq_modnames
  attr(s3obj, "args") <- list(ci_alpha = ci_alpha,
                              x_bins = x_bins)
  attr(s3obj, "src") <- curves
  attr(s3obj, "validated") <- FALSE

  # Call .validate.sumcrvs()
  .validate(s3obj)
}

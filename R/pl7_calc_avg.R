#
# Calculate the average curve for a model
#
calc_avg <- function(curves, model_names, setids, x_interval, ci_level) {

  # === Validate input arguments ===
  .validate_x_interval(x_interval)
  .validate_ci_level(ci_level)
  .validate(curves)

  umodel_names <- unique(model_names)
  usedids <- unique(setids)


  # === Summarize curves by by models ===
  # Quantile of CI
  ci_q <- qnorm(ci_level + (1.0 - ci_level) * 0.5)

  # Filter curves by model
  ffunc <- function(mname) {
    curves[model_names == mname]
  }
  fcurves <- lapply(umodel_names, ffunc)

  # Summarize curves
  vfunc <- function(i) {
    calc_avg_curve(fcurves[[i]], x_interval, ci_q)
  }
  avgcurves <- lapply(seq_along(fcurves), vfunc)

  # === Create an S3 object ===
  s3obj <- structure(avgcurves, class = "avgcurves")

  # Set attributes
  attr(s3obj, "umodel_names") <- umodel_names
  attr(s3obj, "args") <- list(model_names = model_names, setids = setids,
                              x_interval = x_interval, ci_level = ci_level)
  attr(s3obj, "src") <- curves
  attr(s3obj, "validated") <- FALSE

  # Call .validate.sumcrvs()
  .validate(s3obj)
}

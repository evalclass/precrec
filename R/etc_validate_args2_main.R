# Validate arguments of evalmulti()
.validate_evalmulti_args <- function(x_interval, model_names, na.last,
                                     ties.method, olevs) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model names
  .validate_model_names(model_names)

  # Check na.last
  .validate_na_last(na.last)

  # Check ties.method
  .validate_ties_method(ties.method)

  # Check levels
  .validate_olevs(olevs)
}

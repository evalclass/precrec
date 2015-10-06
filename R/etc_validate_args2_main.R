#
# Validate arguments of evalmod()
#
.validate_evalmod_args <- function(x_interval, model_name, data_no,
                                   na.last, ties.method, levels) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model name
  .validate_model_name(model_name)

  # Check data number
  .validate_data_no(data_no)

  # Check na.last
  .validate_na_last(na.last)

  # Check ties.method
  .validate_ties_method(ties.method)

  # Check levels
  .validate_levels(levels)
}

#
# Validate arguments of evalmulti()
#
.validate_evalmulti_args <- function(x_interval, model_names, data_nos,
                                     na.last, ties.method, levels) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model names
  .validate_model_names(model_names, length(model_names))

  # Check data numbers
  .validate_data_nos(data_nos, length(data_nos))

  # Check na.last
  .validate_na_last(na.last)

  # Check ties.method
  .validate_ties_method(ties.method)

  # Check levels
  .validate_levels(levels)
}

#
# Validate arguments of evalcv()
#
.validate_evalcv_args <- function(x_interval, model_names, data_nos,
                                  na.last, ties.method, levels) {

  .validate_evalmulti_args(x_interval, model_names, data_nos,
                           na.last, ties.method, levels)
}

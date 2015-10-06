#
# Validate arguments of evalmod()
#
.validate_evalmod_args <- function(x_interval, model_name, setid,
                                   na.last, ties.method, levels) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model name
  .validate_model_name(model_name)

  # Check dataset ID
  .validate_setid(setid)

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
.validate_evalmulti_args <- function(x_interval, model_names, setids,
                                     na.last, ties.method, levels) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model names
  .validate_model_names(model_names, length(model_names))

  # Check dataset IDs
  .validate_setids(setids, length(setids))

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
.validate_evalcv_args <- function(x_interval, model_names, setids,
                                  na.last, ties.method, levels) {

  .validate_evalmulti_args(x_interval, model_names, setids,
                           na.last, ties.method, levels)
}

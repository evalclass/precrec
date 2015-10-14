#
# Validate arguments of evalmod()
#
.validate_evalmod_args <- function(x_interval, modname, setid,
                                   na_worst, ties_method, levels) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model name
  .validate_modname(modname)

  # Check dataset ID
  .validate_setid(setid)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)

  # Check levels
  .validate_levels(levels)
}

#
# Validate arguments of evalmods()
#
.validate_evalmods_args <- function(x_interval, modnames, setids,
                                    na_worst, ties_method, levels) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model names
  .validate_modnames(modnames, length(modnames))

  # Check dataset IDs
  .validate_setids(setids, length(setids))

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)

  # Check levels
  .validate_levels(levels)
}

#
# Validate arguments of evalmod_m()
#
.validate_evalmod_m_args <- function(x_interval, calc_avg, ci_level, modnames,
                                     setids, na_worst, ties_method, levels) {

  .validate_evalmods_args(x_interval, modnames, setids,
                          na_worst, ties_method, levels)
}

#
# Validate arguments of evalmods_m()
#
.validate_evalmods_m_args <- function(x_interval, calc_avg, ci_level, modnames,
                                     setids, na_worst, ties_method, levels) {

  .validate_evalmods_args(x_interval, modnames, setids,
                          na_worst, ties_method, levels)
}

#
# Validate arguments of evalmod()
#
.validate_evalmod_args <- function(x_interval, modname, dsid,
                                   na_worst, ties_method) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model name
  .validate_modname(modname)

  # Check dataset ID
  .validate_dsid(dsid)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)
}

#
# Validate arguments of evalmods()
#
.validate_evalmods_args <- function(x_interval, modnames, dsids,
                                    na_worst, ties_method) {

  # Check x_interval
  .validate_x_interval(x_interval)

  # Check model names
  .validate_modnames(modnames, length(modnames))

  # Check dataset IDs
  .validate_dsids(dsids, length(dsids))

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)
}

#
# Validate arguments of evalmod_m()
#
.validate_evalmod_m_args <- function(x_interval, calc_avg, ci_level, modnames,
                                     dsids, na_worst, ties_method) {

  .validate_evalmods_args(x_interval, modnames, dsids,
                          na_worst, ties_method)
}

#
# Validate arguments of evalmods_m()
#
.validate_evalmods_m_args <- function(x_interval, calc_avg, ci_level, modnames,
                                      dsids, na_worst, ties_method) {

  .validate_evalmods_args(x_interval, modnames, dsids,
                          na_worst, ties_method)
}

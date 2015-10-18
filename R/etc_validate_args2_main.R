#
# Validate arguments of evalmod_s()
#
.validate_evalmod_s_args <- function(x_bins, modname, dsid,
                                   na_worst, ties_method) {

  # Check x_bins
  .validate_x_bins(x_bins)

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
# Validate arguments of evalmods_s()
#
.validate_evalmods_s_args <- function(x_bins, modnames, dsids,
                                    na_worst, ties_method) {

  # Check x_bins
  .validate_x_bins(x_bins)

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
.validate_evalmod_m_args <- function(x_bins, calc_avg, ci_level, modnames,
                                     dsids, na_worst, ties_method) {

  .validate_evalmods_s_args(x_bins, modnames, dsids,
                            na_worst, ties_method)
}

#
# Validate arguments of evalmods_m()
#
.validate_evalmods_m_args <- function(x_bins, calc_avg, ci_level, modnames,
                                      dsids, na_worst, ties_method) {

  .validate_evalmods_s_args(x_bins, modnames, dsids,
                            na_worst, ties_method)
}

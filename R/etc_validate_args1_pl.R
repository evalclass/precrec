#
# Validate arguments of pl_main()
#
.validate_pl_main_args <- function(mdat, calc_avg, ci_alpha, all_curves,
                                   x_bins, orig_points) {

  # Validate mdat
  .validate(mdat)


  # Validate calc_avg
  .validate_calc_avg(calc_avg)

  # Validate ci_alpha
  .validate_ci_alpha(ci_alpha)

  # Validate all_curves
  .validate_all_curves(all_curves)


  # Check x_bins
  .validate_x_bins(x_bins)

  # Check orig_points
  .validate_orig_points(orig_points)

}

#
# Validate arguments of .join_datasets()
#
.validate_join_datasets_args <- function(..., efunc_vtype, efunc_nrow, byrow) {

  # Check ...
  arglist <- list(...)
  if (length(arglist) == 0) {
    stop("No datasets specified")
  }

  # Check efunc_vtype
  if (!is.null(efunc_vtype)
      && (!is(efunc_vtype, "function")
          || length(as.list(formals(efunc_vtype))) != 1)) {
    stop("'efunc_vtype' must be a function with 1 argument")
  }

  # Check efunc_nrow
  if (!is.null(efunc_nrow)
      && (!is(efunc_nrow, "function")
          || length(as.list(formals(efunc_nrow))) != 2)) {
    stop("'efunc_nrow' must be a function with 2 arguments")
  }

  # Check byrow
  assertthat::assert_that(assertthat::is.flag(byrow),
                          assertthat::noNA(byrow))

}

#
# Validate arguments of mmdata()
#
.validate_mmdata_args <- function(lscores, llabels, modnames, dsids, posclass,
                                  na_worst, ties_method, expd_first) {

  # Check lscores and llabels
  if (length(llabels) != 1 && length(lscores) != length(llabels)) {
    stop(paste0("The number of scores and label lists should be the same size",
                ", or the number of label list should be 1"))
  }

  # Check model names
  .validate_modnames(modnames, length(lscores))

  # Check dataset IDs
  .validate_dsids(dsids, length(lscores))

  # Check posclass
  .validate_posclass(posclass)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)

  # Check expd_first
  .validate_expd_first(expd_first)
}

#
# Validate arguments of reformat_data()
#
.validate_reformat_data_args <- function(scores, labels, modname, dsid,
                                         posclass, na_worst, ties_method,
                                         ...) {

  # Check '...'
  arglist <- list(...)
  if (!is.null(names(arglist))){
    stop(paste("Invalid arguments:", paste(names(arglist), collapse = ", ")))
  }

  # Check scores and labels
  .validate_scores_and_labels(NULL, NULL, scores, labels)

  # Check model name
  .validate_modname(modname)

  # Check dataset ID
  .validate_dsid(dsid)

  # Check posclass
  .validate_posclass(posclass)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)

}

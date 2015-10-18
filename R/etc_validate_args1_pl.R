#
# Validate arguments of pl_main()
#
.validate_pl_main_args <- function(mdat, model_type, data_type, x_bins,
                                   calc_avg, ci_alpha) {

  # Check model type
  .validate_model_type(model_type)

  modnames <- attr(mdat, "modnames")
  if (model_type == "single") {
    assertthat::assert_that(length(unique(modnames)) == 1L)
  }

  # Check data type
  dsids <- attr(mdat, "dsids")
  if (data_type == "single") {
    assertthat::assert_that(length(unique(dsids)) == 1L)
  }

  # Check x_bins
  .validate_x_bins(x_bins)

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
      && (class(efunc_vtype) != "function"
          || length(as.list(formals(efunc_vtype))) != 1)) {
    stop("'efunc_vtype' must be a function with 1 argument")
  }

  # Check efunc_nrow
  if (!is.null(efunc_nrow)
      && (class(efunc_nrow) != "function"
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
.validate_mmdata_args <- function(lscores, llabels, modnames, dsids,
                                  expd_first, na_worst, ties_method) {

  # Check lscores and llabels
  if (length(llabels) != 1 && length(lscores) != length(llabels)) {
    stop(paste0("'scores' and 'labels' should be of the same size, or ",
                "the size of 'labels' should be 1"))
  }

  # Check model names
  .validate_modnames(modnames, length(lscores))

  # Check dataset IDs
  .validate_dsids(dsids, length(lscores))

  # Check expd_first
  .validate_expd_first(expd_first)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)

}

#
# Validate arguments of reformat_data()
#
.validate_reformat_data_args <- function(obj, obj_name, scores, labels, ...) {

  # Check '...'
  arglist <- list(...)
  if (!is.null(names(arglist))){
    invalid_list <- setdiff(names(arglist), c("na_worst", "ties_method",
                                              "modname", "dsid"))
    if (length(invalid_list) > 0L) {
      stop(paste("Invalid arguments:", paste(invalid_list, collapse = ", ")))
    }

    # Check na_worst
    .validate_na_worst(arglist[["na_worst"]])

    # Check ties_method
    .validate_ties_method(arglist[["ties_method"]])

    # Check modname
    .validate_modname(arglist[["modname"]])

    # Check dsid
    .validate_dsid(arglist[["dsid"]])

  }

  .validate_scores_and_labels(obj, obj_name, scores, labels, ...)
}

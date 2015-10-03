#
# Validate arguments of pl_main()
#
.validate_pl_main_args <- function(mdat, model_type, data_type, x_interval) {

  # Check model type
  .validate_model_type(model_type)

  model_names <- attr(mdat, "model_names")
  if (model_type == "single") {
    assertthat::assert_that(length(unique(model_names)) == 1L)
  }

  # Check data type
  data_nos <- attr(mdat, "data_nos")
  if (model_type == "single") {
    assertthat::assert_that(length(unique(data_nos)) == 1L)
  }

  # Check x_interval
  .validate_x_interval(x_interval)

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
.validate_mmdata_args <- function(lscores, llabels, model_names, data_nos,
                                  exp_priority, na.last, ties.method, levels) {

  # Check lscores and llabels
  if (length(llabels) != 1 && length(lscores) != length(llabels)) {
    stop(paste0("'scores' and 'labels' should be of the same size, or ",
                "the size of 'labels' should be 1"))
  }

  # Check model names
  .validate_model_names(model_names, length(lscores))

  # Check data numbers
  .validate_data_nos(data_nos, length(lscores))

  # Check exp_priority
  .validate_exp_priority(exp_priority)

  # Check na.last
  .validate_na_last(na.last)

  # Check ties.method
  .validate_ties_method(ties.method)

  # Check levels
  .validate_levels(levels)

}

#
# Validate arguments of reformat_data()
#
.validate_reformat_data_args <- function(obj, obj_name, scores, labels, ...) {

  # Check '...'
  arglist <- list(...)
  if (!is.null(names(arglist))){
    invalid_list <- setdiff(names(arglist), c("na.last", "ties.method",
                                              "levels", "model_name",
                                              "data_no"))
    if (length(invalid_list) > 0L) {
      stop(paste("Invalid arguments:", paste(invalid_list, collapse = ", ")))
    }

    # Check na.last
    .validate_na_last(arglist[["na.last"]])

    # Check ties.method
    .validate_ties_method(arglist[["ties.method"]])

    # Check levels
    .validate_levels(arglist[["levels"]])

    # Check model_name
    .validate_model_name(arglist[["model_name"]])

    # Check data_no
    .validate_data_no(arglist[["data_no"]])

  }

  .validate_scores_and_labels(obj, obj_name, scores, labels, ...)
}

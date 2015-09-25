# Validate arguments of pl_main()
.validate_pl_main_args <- function(mdat, model_type, data_type, x_interval) {

  # Check model type
  .validate_model_type(model_type)
  if (model_type == "single"
      && length(unique(attr(mdat, "model_names"))) != 1) {
    stop("'mdat' contains scores and labels for multiple modeles")
  }

  # Check data type
  .validate_data_type(data_type)
  if (data_type == "single" && length(unique(attr(mdat, "data_nos"))) != 1) {
    stop("'mdat' contains scores and labels of multiple test sets")
  }

  # Check x_interval
  .validate_x_interval(x_interval)

}

# Validate arguments of mmdata()
.validate_mmdata_args <- function(lpscores, lolabs, model_names, data_nos,
                                  group_by, ...) {

  # Check lpscores and lolabs
  if (length(lolabs) != 1 && length(lpscores) != length(lolabs)) {
    stop(paste0("'pscores' and 'olabs' should be of the same size, or ",
                "the size of 'olabs' should be 1"))
  }

  # Check model names
  .validate_model_names(model_names, length(lpscores))

  # Check data numbers
  .validate_data_nos(data_nos, length(lpscores))

}

# Validate arguments of reformat_data()
.validate_reformat_data_args <- function(obj, obj_name, scores, olabs, ...) {

  # Check '...'
  arglist <- list(...)
  if (!is.null(names(arglist))){
    invalid_list <- setdiff(names(arglist), c("na.last", "ties.method",
                                              "olevs", "model_name",
                                              "data_no"))
    if (length(invalid_list) > 0L) {
      stop(paste("Invalid arguments:", paste(invalid_list, collapse = ", ")))
    }

    # Check na.last
    .validate_na_last(arglist[["na.last"]])

    # Check ties.method
    .validate_ties_method(arglist[["ties.method"]])

    # Check levels
    .validate_olevs(arglist[["olevs"]])

    # Check model_name
    .validate_model_name(arglist[["model_name"]])

    # Check data_no
    .validate_data_no(arglist[["data_no"]])

  }

  .validate_pscores_and_olabs(obj, obj_name, scores, olabs, ...)
}

# Validate arguments of create_curves()
.validate_create_curves_args <- function(x_interval, ...) {

  # Check x_interval
  .validate_x_interval(x_interval)

}

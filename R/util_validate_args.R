# Used by functions before calling reformat_data()
.validate_reformat_data_args <- function(obj, obj_name, scores, obslabs, ...) {

  # === Validate input arguments ===
  # Check '...'
  arglist <- names(list(...))
  if (!is.null(arglist)){
    invalid_list <- setdiff(arglist, c("na.last", "ties.method", "levels",
                                       "model_name"))
    if (length(invalid_list) > 0) {
      stop(paste("Invalid arguments:", paste(invalid_list, collapse = ", ")))
    }
  }

  # Check if scores and obslabs are specified. Validate fmdat otherwise
  if (missing(obj)) {
    if(is.null(scores) && !is.null(obslabs)) {
      stop("Invalid argument: scores")
    } else if(!is.null(scores) && is.null(obslabs)) {
      stop("Invalid argument: obslabs")
    } else if (is.null(scores) && is.null(obslabs)) {
      stop(paste("'", obj_name, "' must be specified", sep=""))
    }
  } else {
    .validate(obj)
  }

  # Args are ignored when fmdat is specified
  if (!missing(obj) && !is.null(arglist)) {
    if (length(arglist) == 1) {
      warning("Argument (", arglist, ") is ignored", sep="")
    } else {
      warning(paste("Auguments (", paste(arglist, collapse = ", "),
                    ") are ignored"), sep = "")
    }
  }
}

# Used by functions before calling create_curves()
.validate_create_curves_args <- function(obj, obj_name, x_interval,
                                         scores, obslabs, ...) {
  # === Validate input arguments ===
  .validate_reformat_data_args(obj, obj_name, scores, obslabs, ...)

  if (!is.atomic(x_interval) || !is.numeric(x_interval)
      || length(x_interval) != 1){
    stop("x_interval must be a numeric scalar")
  }

  if (x_interval <= 0 || x_interval > 1) {
    stop("x_interval must be (0, 1]")
  }
}

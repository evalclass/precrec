# Check if an internal Rcpp function returns en error
.check_cpp_func_error <- function(obj, func_name) {

  if (obj[["errmsg"]] != "") {
    stop(paste("An internal function (", func_name, "()) failed: ",
               obj[["errmsg"]]), sep = "")
  }
}

# Use scores and obslabs
.create_by_scores_and_labels <- function(obj, obj_name, func,
                                         scores, obslabs, ...) {
  .validate_reformat_data_args(obj, obj_name, scores, obslabs,
                               ...)
  if (missing(obj)) {
    if (!is.null(scores) && !is.null(obslabs)) {
      obj <- func(scores = scores, obslabs = obslabs, ...)
    } else {
      stop("The first argument must be specified.")
    }
  }

  obj
}


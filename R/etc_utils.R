#
# Check if an internal Rcpp function returns en error
#
.check_cpp_func_error <- function(obj, func_name) {

  if (obj[["errmsg"]] != "") {
    stop(paste("An internal function (", func_name, "()) failed: ",
               obj[["errmsg"]]), sep = "")
  }
}

#
# Get a specified object
#
.get_obj <- function(obj, obj_name) {
  if (is.null(obj_name) || is.null(obj) || class(obj) == obj_name) {
    obj
  } else {
    .get_obj(attr(obj, "src"), obj_name)
  }
}

#
# Get an argument of the specified source object
#
.get_obj_arg <- function(obj, obj_name, arg_name) {
  if (!is.null(obj_name) && !is.na(obj_name)) {
    obj <- .get_obj(obj, obj_name)
  }
  obj_args <- attr(obj, "args")
  if (is.null(obj_args)) {
    NULL
  } else {
    obj_args[[arg_name]]
  }
}

#
# Use scores and labels to create obj
#
.create_by_scores_and_labels <- function(obj, obj_name, func, scores, labels,
                                         ...) {
  if (missing(obj)) {
    if (!is.null(scores) && !is.null(labels)) {
      obj <- func(scores = scores, labels = labels, ...)
    } else {
      stop("The first argument must be specified.")
    }
  } else {
    arglist <- list(...)
    if (length(arglist) != 0) {
      warning(gettextf("%s are ignored",
                       paste(dQuote(choices), collapse = ", ")))
    }
  }

  obj
}

#
# Use mscores and mobslabs to create mdat
#
.create_by_mscores_and_mlabels <- function(mdat, mscores, mobslabs) {

  # Check if scores and labels are specified.
  # Validate the 'mdat' argument otherwise
  if (missing(mdat)) {
    if (is.null(mscores) && is.null(mobslabs)) {
      stop("'mdat' must be specified")
    } else if(is.null(mscores)) {
      stop("Invalid argument: mscores")
    } else if(is.null(mobslabs)) {
      stop("Invalid argument: mobslabs")
    } else {
      mdat <- reformat_mdata(mscores = mscores, mobslabs = mobslabs)
    }
  } else {
    mdat <- .validate(mdat)
  }

  mdat
}

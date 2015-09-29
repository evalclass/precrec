# Check if it's a numeric vector
.assert_numeric_vec <- function(v) {
  assertthat::assert_that(is.atomic(v), is.vector(v), is.numeric(v))
}

# Check if it's a numeric or factor vector
.assert_numeric_factor_vec <- function(v) {
  assertthat::assert_that(is.atomic(v),
                          ((is.vector(v) && is.numeric(v)) || is.factor(v)))
}

# Check if it's a number
.assert_number <- function(v) {
  assertthat::assert_that(assertthat::is.number(v))
}

# Check if it's an integer vector
.assert_int_vec <- function(v) {
  assertthat::assert_that(is.atomic(v), is.vector(v), is.integer(v))
}

# Check if it's a character vector
.assert_char_vec <- function(v) {
  assertthat::assert_that(is.atomic(v), is.vector(v), is.character(v))
}

# Check if it's a string
.assert_single_string <- function(v) {
  assertthat::assert_that(assertthat::is.string(v))
}

# Check if it's a logical vector
.assert_logical_vec <- function(v) {
  assertthat::assert_that(is.atomic(v), is.vector(v), is.logical(v))
}

# Check if it's a factor vector
.assert_factor_vec <- function(v) {
  assertthat::assert_that(is.atomic(v), is.factor(v))
}

# Check if an internal Rcpp function returns en error
.check_cpp_func_error <- function(obj, func_name) {

  if (obj[["errmsg"]] != "") {
    stop(paste("An internal function (", func_name, "()) failed: ",
               obj[["errmsg"]]), sep = "")
  }
}

# Get a specified object
.get_obj <- function(obj, obj_name) {
  if (is.null(obj_name) || is.null(obj) || class(obj) == obj_name) {
    obj
  } else {
    .get_obj(attr(obj, "src"), obj_name)
  }
}

# Get an argument of the specified source object
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

# Use scores and labels to create obj
.create_by_scores_and_labels <- function(obj, obj_name, func, pscores, olabs,
                                         ...) {
  if (missing(obj)) {
    if (!is.null(pscores) && !is.null(olabs)) {
      obj <- func(pscores = pscores, olabs = olabs, ...)
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

# Use mscores and mobslabs to create mdat
.create_by_mscores_and_mlabels <- function(mdat, mscores, mobslabs) {

  # Check if scores and olabs are specified.
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

# Check if it's a numeric vector
.is_numeric_vec <- function(v) {
  if (!is.atomic(v) || !is.vector(v) || !is.numeric(v)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Check if it's a number
.is_a_number <- function(v) {
  if (!.is_numeric_vec(v) || length(v) != 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Check if it's an integer vector
.is_int_vec <- function(v) {
  if (!is.atomic(v) || !is.vector(v) || !is.integer(v)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Check if it's a character vector
.is_char_vec <- function(v) {
  if (!is.atomic(v) || !is.vector(v) || !is.character(v)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Check if it's a string
.is_single_string <- function(v) {
  if (!.is_char_vec(v) || length(v) != 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Check if it's a logical vector
.is_logical_vec <- function(v) {
  if (!is.atomic(v) || !is.vector(v) || !is.logical(v)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Check if it's a factor vector
.is_factor_vec <- function(v) {
  if (!is.atomic(v) || !is.factor(v)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
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

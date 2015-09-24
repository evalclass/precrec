# Validate pscores
.validate_pscores <- function(pscores) {
  if (!.is_numeric_vec(pscores)) {
    stop("'pscores' must be a numeric vector")
  } else if (length(pscores) == 0L) {
    stop("'pscores' must be length >= 1")
  }
}

# Validate olabs
.validate_olabs <- function(olabs) {
  if (!.is_numeric_vec(olabs) && !.is_factor_vec(olabs)) {
    stop("'olabs' must be either a numeric vector or a factor")
  } else if (length(unique(olabs)) > 2L) {
    stop("'olabs' cotains the invalid number of unique labels")
  }
}

# Validate pscores and olabs
.validate_pscores_and_olabs <- function(obj, obj_name, pscores, olabs, ...) {

  if (missing(obj) || is.null(obj)) {

    # Check if pscores and olabs are specified
    if(is.null(pscores) && !is.null(olabs)) {
      stop("Invalid 'pscores'")
    } else if(!is.null(pscores) && is.null(olabs)) {
      stop("Invalid 'olabs'")
    } else if (is.null(pscores) && is.null(olabs)) {
      if (is.null(obj)) {
        stop("Invalid 'pscores' & 'olabs'")
      } else {
        stop(paste0("Missing '", obj_name, "'."))
      }
    }

    # Check pscores
    .validate_pscores(pscores)

    # Check olabs
    .validate_olabs(olabs)

    # Check length of pscores and olabs
    if (length(olabs) != length(pscores)) {
      stop("'pscores' and 'olabs' must be of the same length")
    }

  } else if (!is.null(obj)) {
    # Validate the first argument
    obj <- .validate(obj)
  }

  obj
}

# Validate na.last
.validate_na_last <- function(na.last) {
  if (!is.null(na.last)) {
    choices = c(FALSE, TRUE)
    if (length(na.last) != 1L || !(na.last %in% choices)) {
      stop(gettextf("'na.last' should be one of %s",
                    paste(choices, collapse = ", ")))
    }
  }
}

# Validate ties.method
.validate_ties_method <- function(ties.method) {
  if (!is.null(ties.method)) {
    choices = c("average", "random", "first")
    if (length(ties.method) != 1L || !(ties.method %in% choices)) {
      stop(gettextf("'ties.method' should be one of %s",
                    paste(dQuote(choices), collapse = ", ")))
    }
  }
}

# Validate olevs
.validate_olevs <- function(olevs) {
  if (!is.null(olevs)) {
    if (!.is_char_vec(olevs)) {
      stop("'olevs' must be a charactor vector")
    } else if (length(unique(olevs)) > 2L) {
      stop("'olevs' cotains the invalid number of unique labels")
    }
  }
}

# Validate model_name
.validate_model_name <- function(model_name) {
  if (!is.null(model_name)) {
    if (!.is_char_vec(model_name)) {
      stop("'model_name' must be a character vector")
    } else if (length(model_name) != 1L) {
      stop("'model_name' must be a single string")
    }
  }
}

# Validate data_no
.validate_data_no <- function(data_no) {
  if (!is.null(data_no)) {
    if (!.is_numeric_vec(data_no) && !.is_char_vec(data_no)) {
      stop("'data_no' must be either a character or a numeric vector")
    } else if (length(data_no) != 1L) {
      stop("'data_no' must be a number or a single string")
    }
  }
}

# Validate model_names
.validate_model_names <- function(model_names, veclen) {
  if (!is.null(model_names)) {
    if (length(model_names) != veclen) {
      stop("Invalid model names")
    }

    lapply(model_names, .validate_model_name)
  }
}

# Validate data_nos
.validate_data_nos <- function(data_nos, veclen) {
  if (!is.null(data_nos)) {
    if (length(data_nos) != veclen) {
      stop("Invalid data numbers (data_nos)")
    }

    lapply(data_nos, .validate_data_no)
  }
}

# Validate x_interval
.validate_x_interval <- function(x_interval) {
  if (!is.atomic(x_interval) || !is.vector(x_interval)
      || !is.numeric(x_interval) || length(x_interval) != 1L){
    stop("x_interval must be a numeric value")
  }

  if (x_interval <= 0L|| x_interval > 1L) {
    stop("x_interval must be (0, 1]")
  }
}

# Validate model type
.validate_model_type <- function(model_type) {
  if (!is.null(model_type)) {
    if (!.is_char_vec(model_type) || length(model_type) != 1L) {
      stop("'model_type' must be a string")
    } else if (model_type != "single" && model_type != "multiple") {
      stop("'model_type' must be either 'single' or 'multiple'")
    }
  }
}

# Validate data type
.validate_data_type <- function(data_type) {
  if (!is.null(data_type)) {
    if (!.is_char_vec(data_type) || length(data_type) != 1L) {
      stop("'data_type' must be a string")
    } else if (data_type != "single" && data_type != "multiple") {
      stop("'data_type' must be either 'single' or 'multiple'")
    }
  }
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

# Validate arguments of mmdata()
.validate_mmdata_args <- function(lpscores, lolabs, model_names, data_nos,
                                  ...) {

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

  # Check model names
  .validate_x_interval(x_interval)

}

# # Validate arguments of reformat_mdata()
# .validate_reformat_mdata_args <- function(obj, obj_name, lscores, llabels,
#                                           model_names, ...) {
#
#   if (length(llabels) != 1 && length(lscores) != length(llabels)) {
#     stop(paste0("'mscores' and 'mobslabs' should be of the same size, or ",
#                 "the length of 'mobslabs' should be 1"))
#   }
#
#   if (length(model_names) != length(lscores)) {
#     stop("Invalid model names")
#   }
#
#   if (!all(unlist(lapply(model_names, .is_char_vec)))) {
#     stop("Model name must be a character vector")
#   }
#
#   vfunc <- function(i) {
#     .validate_reformat_data_args(obj, obj_name, lscores[[i]], llabels[[i]],
#                                  model_name = model_names[[i]], ...)
#   }
#
#   mfmdat <- lapply(seq_along(lscores), vfunc)
#
# }

# Validate arguments of create_curves()
.validate_create_curves_args <- function(x_interval, ...) {

  if (!is.atomic(x_interval) || !is.vector(x_interval)
      || !is.numeric(x_interval) || length(x_interval) != 1L){
    stop("x_interval must be a numeric scalar")
  }

  if (x_interval <= 0L|| x_interval > 1L) {
    stop("x_interval must be (0, 1]")
  }

}

# Validate arguments of create_mcurves()
.validate_create_mcurves_args <- function(x_interval, model_names, ...) {
  .validate_create_curves_args(x_interval)

  # Check model_names
  if (!is.null(model_names) && (!is.atomic(model_names)
                                || !is.character(model_names))) {
    stop("'model_names' must be a charactor vector")
  }
}

# Validate arguments of evalmulti()
.validate_evalmulti_args <- function(x_interval, model_names, na.last,
                                     ties.method, olevs) {

  .validate_create_mcurves_args(x_interval, model_names)

  # Check na.last
  choices = c(FALSE, TRUE)
  if (length(na.last) != 1L || !(na.last %in% choices)) {
    stop(gettextf("'na.last' should be one of %s",
                  paste(choices, collapse = ", ")))
  }

  # Check ties.method
  choices = c("average", "random", "first")
  if (length(ties.method) != 1L || !(ties.method %in% choices)) {
    stop(gettextf("'ties.method' should be one of %s",
                  paste(dQuote(choices), collapse = ", ")))
  }

  # Check levels
  if (!is.atomic(olevs) || !is.character(olevs)) {
    stop("'olevs' must be a charactor vector")
  } else if (length(unique(olevs)) != 2L) {
    stop("'olevs' cotains the invalid number of unique labels")
  }
}

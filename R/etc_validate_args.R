# Validate scores
.validate_scores <- function(scores) {
  if (!.is_numeric_vec(scores)) {
    stop("'scores' must be a numeric vector")
  } else if (length(scores) == 0L) {
    stop("'scores' must be length >= 1")
  }
}

# Validate obslabs
.validate_obslabs <- function(obslabs) {
  if (!.is_numeric_vec(obslabs) && !.is_factor_vec(obslabs)) {
    stop("'obslabs' must be either a numeric vector or a factor")
  } else if (length(unique(obslabs)) > 2L) {
    stop("'obslabs' cotains the invalid number of unique labels")
  }
}

# Validate scores and obslabs
.validate_scores_and_obslabs <- function(obj, obj_name, scores, obslabs, ...) {

  if (missing(obj) || is.null(obj)) {

    # Check if scores and obslabs are specified
    if(is.null(scores) && !is.null(obslabs)) {
      stop("Invalid 'scores'")
    } else if(!is.null(scores) && is.null(obslabs)) {
      stop("Invalid 'obslabs'")
    } else if (is.null(scores) && is.null(obslabs)) {
      if (is.null(obj)) {
        stop("Invalid 'scores' & 'obslabs'")
      } else {
        stop(paste("'", obj_name, "' must be specified", sep=""))
      }
    }

    # Check scores
    .validate_scores(scores)

    # Check obslabs
    .validate_obslabs(obslabs)

    # Check length of scores and obslabs
    if (length(obslabs) != length(scores)) {
      stop("'scores' and 'obslabs' must be of the same length")
    }

  } else if (!is.null(obj)) {
    # Validate the first argument
    obj <- .validate(obj)
  }

  # Args are ignored when the first argument is specified
  arglist <- list(...)
  if (!missing(obj) && !is.null(obj) && !is.null(arglist)) {
    if (length(arglist) == 1L) {
      warning("Argument (", arglist, ") is ignored", sep="")
    } else {
      warning(paste("Auguments (", paste(arglist, collapse = ", "),
                    ") are ignored"), sep = "")
    }
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

# Validate obslevels
.validate_obslevels <- function(obslevels) {
  if (!is.null(obslevels)) {
    if (!.is_char_vec(obslevels)) {
      stop("'obslevels' must be a charactor vector")
    } else if (length(unique(obslevels)) > 2L) {
      stop("'obslevels' cotains the invalid number of unique labels")
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

# Validate arguments of reformat_data()
.validate_reformat_data_args <- function(obj, obj_name, scores, obslabs, ...) {

  # Check '...'
  arglist <- list(...)
  if (!is.null(names(arglist))){
    invalid_list <- setdiff(names(arglist), c("na.last", "ties.method",
                                              "obslevels", "model_name"))
    if (length(invalid_list) > 0L) {
      stop(paste("Invalid arguments:", paste(invalid_list, collapse = ", ")))
    }

    # Check na.last
    .validate_na_last(arglist[["na.last"]])

    # Check ties.method
    .validate_ties_method(arglist[["ties.method"]])


    # Check levels
    .validate_obslevels(arglist[["obslevels"]])

    # Check model_name
    .validate_model_name(arglist[["model_name"]])

  }

  .validate_scores_and_obslabs(obj, obj_name, scores, obslabs, ...)
}

# Validate arguments of reformat_mdata()
.validate_reformat_mdata_args <- function(obj, obj_name, lscores, llabels,
                                          model_names, ...) {

  if (length(llabels) != 1 || length(lscores) != length(llabels)) {
    stop(paste0("'mscores' and 'mobslabs' should be of the same size, or ",
                "the length of 'mobslabs' should be 1"))
  }

  if (!.is_char_vec(model_names) || length(model_names) != length(lscores)) {
    stop("Invalid model names")
  }

  vfunc <- function(i) {
    .validate_reformat_data_args(obj, obj_name, lscores[[i]], llabels[[i]],
                                 model_name = model_names[[i]]...)
  }

  mfmdat <- lapply(seq_along(mscores), vfunc)

}

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
                                     ties.method, levels) {

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
  if (!is.atomic(levels) || !is.character(levels)) {
    stop("'levels' must be a charactor vector")
  } else if (length(unique(levels)) != 2L) {
    stop("'levels' cotains the invalid number of unique labels")
  }
}

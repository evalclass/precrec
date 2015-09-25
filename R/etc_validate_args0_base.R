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

# Validate group_by
.validate_group_by <- function(group_by) {
  if (!is.null(group_by)) {
    if (!.is_char_vec(group_by) || length(group_by) != 1L) {
      stop("'group_by' must be a string")
    } else if (group_by != "model_name" && group_by != "data_no") {
      stop("'data_type' must be either 'model_name' or 'data_no'")
    }
  }
}


# Validate model_names
.validate_model_names <- function(model_names, veclen = NULL) {
  if (!is.null(model_names)) {
    if (!is.null(veclen) && length(model_names) != veclen) {
      stop("Invalid model names")
    }

    lapply(model_names, .validate_model_name)
  }
}

# Validate data_nos
.validate_data_nos <- function(data_nos, veclen = NULL) {
  if (!is.null(data_nos)) {
    if (!is.null(veclen) && length(data_nos) != veclen) {
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

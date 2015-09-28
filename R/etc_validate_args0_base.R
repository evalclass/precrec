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
  } else if (length(unique(olabs)) != 2L) {
    stop("'olabs' must cotain two unique labels")
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
    if (!.is_logical_vec(na.last) || !(na.last %in% choices)) {
      stop("'na.last' must be either FALSE or TRUE")
    }
  }
}

# Validate ties.method
.validate_ties_method <- function(ties.method) {
  if (!is.null(ties.method)) {
    choices = c("average", "random", "first")
    if (!.is_single_string(ties.method) || !(ties.method %in% choices)) {
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
    } else if (length(unique(olevs)) != 2L) {
      stop("'olevs' must cotain two unique labels")
    }
  }
}

# Validate model_name
.validate_model_name <- function(model_name) {
  if (!is.null(model_name)) {
    if (!.is_single_string(model_name)) {
      stop("'model_name' must be a single string")
    }
  }
}

# Validate data_no
.validate_data_no <- function(data_no) {
  if (!is.null(data_no)) {
    if (!.is_a_number(data_no) && !.is_single_string(data_no)) {
      stop("'data_no' must be a single number or string")
    }
  }
}

# Validate exp_priority
.validate_exp_priority <- function(exp_priority) {
  if (!is.null(exp_priority)) {
    if (!.is_single_string(exp_priority)) {
      stop("'exp_priority' must be a single string")
    } else if (exp_priority != "model_names" && exp_priority != "data_nos") {
      stop("'exp_priority' must be either 'model_names' or 'data_nos'")
    }
  }
}


# Validate model_names
.validate_model_names <- function(model_names, veclen = NULL) {
  if (!is.null(model_names)) {
    if (!.is_char_vec(model_names) ||
          (!is.null(veclen) && length(model_names) != veclen)) {
      stop("Invalid model names")
    }

    for (i in 1:length(model_names)) {
      .validate_model_name(model_names[i])
    }
  }
}

# Validate data_nos
.validate_data_nos <- function(data_nos, veclen = NULL) {
  if (!is.null(data_nos)) {
    if (!(.is_char_vec(data_nos) || .is_a_number(data_nos))
        || (!is.null(veclen) && length(data_nos) != veclen)) {
      stop("Invalid data numbers (data_nos)")
    }

    for (i in 1:length(data_nos)) {
      .validate_data_no(data_nos[i])
    }
  }
}

# Validate x_interval
.validate_x_interval <- function(x_interval) {
  if (!is.atomic(x_interval) || !is.vector(x_interval)
      || !is.numeric(x_interval) || length(x_interval) != 1L){
    stop("x_interval must be a single numeric value")
  }

  if (x_interval <= 0L|| x_interval > 1L) {
    stop("x_interval must be (0, 1]")
  }
}

# Validate model type
.validate_model_type <- function(model_type) {
  if (!is.null(model_type)) {
    if (!.is_char_vec(model_type) || length(model_type) != 1L) {
      stop("'model_type' must be a single string")
    } else if (model_type != "single" && model_type != "multiple") {
      stop("'model_type' must be either 'single' or 'multiple'")
    }
  }
}

# Validate data type
.validate_data_type <- function(data_type) {
  if (!is.null(data_type)) {
    if (!.is_char_vec(data_type) || length(data_type) != 1L) {
      stop("'data_type' must be a single string")
    } else if (data_type != "single" && data_type != "multiple") {
      stop("'data_type' must be either 'single' or 'multiple'")
    }
  }
}

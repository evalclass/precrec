#
# Validate scores
#
.validate_scores <- function(scores) {
  assertthat::assert_that(is.atomic(scores), is.vector(scores),
                          is.numeric(scores), length(scores) > 0L)
}

#
# Validate labels
#
.validate_labels <- function(labels) {
  assertthat::assert_that(is.atomic(labels),
                          ((is.vector(labels) && is.numeric(labels))
                           || is.factor(labels)),
                          length(unique(labels)) == 2L)
}

#
# Validate scores and labels
#
.validate_scores_and_labels <- function(obj, obj_name, scores, labels, ...) {

  if (missing(obj) || is.null(obj)) {

    # Check if scores and labels are specified
    if(is.null(scores) && !is.null(labels)) {
      stop("Invalid scores")
    } else if(!is.null(scores) && is.null(labels)) {
      stop("Invalid labels")
    } else if (is.null(scores) && is.null(labels)) {
      if (is.null(obj)) {
        stop("Invalid scores & labels")
      } else {
        stop(paste0(obj_name, " must be specified"))
      }
    }

    # Check scores
    .validate_scores(scores)

    # Check labels
    .validate_labels(labels)

    # Check length of scores and labels
    if (length(labels) != length(scores)) {
      stop("scores and labels must be of the same length")
    }

  } else if (!is.null(obj)) {
    # Validate the first argument
    obj <- .validate(obj)
  }

  obj
}

#
# Validate na_worst
#
.validate_na_worst <- function(na_worst) {
  if (!is.null(na_worst)) {
    assertthat::assert_that(is.atomic(na_worst),
                            assertthat::is.flag(na_worst),
                            assertthat::noNA(na_worst))
  }
}

#
# Validate ties_method
#
.validate_ties_method <- function(ties_method) {
  if (!is.null(ties_method)) {
    assertthat::assert_that(assertthat::is.string(ties_method))

    choices = c("equiv", "random", "first")
    if (!(ties_method %in% choices)) {
      stop(gettextf("ties_method should be one of %s",
                    paste(dQuote(choices), collapse = ", ")))
    }
  }
}

#
# Validate modname
#
.validate_modname <- function(modname) {
  if (!is.null(modname)) {
    assertthat::assert_that(assertthat::is.string(modname))
  }
}

#
# Validate dsid
#
.validate_dsid <- function(dsid) {
  if (!is.null(dsid)) {
    assertthat::assert_that(assertthat::is.number(dsid))
  }
}

#
# Validate ci_level
#
.validate_ci_level <- function(ci_level) {
  if (!is.null(ci_level)) {
    assertthat::assert_that(assertthat::is.number(ci_level))
  }
}


#
# Validate expd_first
#
.validate_expd_first <- function(expd_first) {
  if (!is.null(expd_first)) {
    assertthat::assert_that(assertthat::is.string(expd_first),
                            (expd_first == "modnames"
                             || expd_first == "dsids"))
  }
}

#
# Validate modnames
#
.validate_modnames <- function(modnames, datalen) {
  if (!is.null(modnames)) {

    assertthat::assert_that(is.atomic(modnames), is.vector(modnames),
                            is.character(modnames),
                            length(modnames) == datalen)

    for (i in 1:length(modnames)) {
      .validate_modname(modnames[i])
    }
  }
}

#
# Validate dsids
#
.validate_dsids <- function(dsids, datalen) {
  if (!is.null(dsids)) {

    assertthat::assert_that(is.atomic(dsids), is.vector(dsids),
                            (is.character(dsids) || is.numeric(dsids)),
                            assertthat::noNA(dsids),
                            length(dsids) == datalen)

    for (i in 1:length(dsids)) {
      .validate_dsid(dsids[i])
    }
  }
}

#
# Validate x_bins
#
.validate_x_bins <- function(x_bins) {
  if (!is.null(x_bins)) {
    assertthat::assert_that(assertthat::is.number(x_bins),
                            x_bins >= 1L)
  }
}

#
# Validate model type
#
.validate_model_type <- function(model_type) {
  if (!is.null(model_type)) {
    assertthat::assert_that(assertthat::is.string(model_type),
                            (model_type == "single"
                             || model_type == "multiple"))
  }
}

#
# Validate data type
#
.validate_data_type <- function(data_type) {
  if (!is.null(data_type)) {
    assertthat::assert_that(assertthat::is.string(data_type),
                            (data_type == "single"
                             || data_type == "multiple"))
  }
}

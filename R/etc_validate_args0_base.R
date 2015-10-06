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
# Validate na.last
#
.validate_na_last <- function(na.last) {
  if (!is.null(na.last)) {
    assertthat::assert_that(is.atomic(na.last),
                            assertthat::is.flag(na.last),
                            assertthat::noNA(na.last))
  }
}

#
# Validate ties.method
#
.validate_ties_method <- function(ties.method) {
  if (!is.null(ties.method)) {
    assertthat::assert_that(assertthat::is.string(ties.method))

    choices = c("average", "random", "first")
    if (!(ties.method %in% choices)) {
      stop(gettextf("ties.method should be one of %s",
                    paste(dQuote(choices), collapse = ", ")))
    }
  }
}

#
# Validate levels
#
.validate_levels <- function(levels) {
  if (!is.null(levels)) {
    assertthat::assert_that(is.atomic(levels),
                            is.character(levels),
                            length(unique(levels)) == 2L)
  }
}

#
# Validate model_name
#
.validate_model_name <- function(model_name) {
  if (!is.null(model_name)) {
    assertthat::assert_that(is.atomic(model_name),
                            is.vector(model_name),
                            assertthat::is.string(model_name))
  }
}

#
# Validate setid
#
.validate_setid <- function(setid) {
  if (!is.null(setid)) {
    assertthat::assert_that(assertthat::is.number(setid)
                            || assertthat::is.string(setid))
  }
}

#
# Validate expd_first
#
.validate_expd_first <- function(expd_first) {
  if (!is.null(expd_first)) {
    assertthat::assert_that(is.atomic(expd_first), is.vector(expd_first),
                            assertthat::is.string(expd_first),
                            (expd_first == "model_names"
                             || expd_first == "setids"))
  }
}

#
# Validate model_names
#
.validate_model_names <- function(model_names, datalen) {
  if (!is.null(model_names)) {

    assertthat::assert_that(is.atomic(model_names), is.vector(model_names),
                            is.character(model_names),
                            length(model_names) == datalen)

    for (i in 1:length(model_names)) {
      .validate_model_name(model_names[i])
    }
  }
}

#
# Validate setids
#
.validate_setids <- function(setids, datalen) {
  if (!is.null(setids)) {

    assertthat::assert_that(is.atomic(setids), is.vector(setids),
                            (is.character(setids) || is.numeric(setids)),
                            assertthat::noNA(setids),
                            length(setids) == datalen)

    for (i in 1:length(setids)) {
      .validate_setid(setids[i])
    }
  }
}

#
# Validate x_interval
#
.validate_x_interval <- function(x_interval) {
  if (!is.null(x_interval)) {
    assertthat::assert_that(assertthat::is.number(x_interval),
                            (x_interval > 0L && x_interval <= 1L))
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

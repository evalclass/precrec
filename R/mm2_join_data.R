#' Join scores of multiple models into a list
#'
#' The \code{join_scores} function takes predicted scores from multiple models
#'   and converts them to a list.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, matrices,
#'   data frames, and lists.
#'
#' @param byrow A Boolean value to specify whether row vectors are used
#'   for matrix, data frame, and array.
#'
#' @param chklen A Boolean value to specify whether all list items must be
#'   the same lengths.
#'
#' @return The \code{join_scores} function returns a list that
#'   contains all combined score data.
#'
#' @seealso \code{\link{evalmod}} for calculation evaluation measures.
#'   \code{\link{mmdata}} for formatting input data.
#'   \code{\link{join_labels}} for formatting labels with multiple datasets.
#'
#' @examples
#'
#' ##################################################
#' ### Add three numeric vectors
#' ###
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' scores1 <- join_scores(s1, s2, s3)
#'
#' ## Show the list structure
#' str(scores1)
#'
#'
#' ##################################################
#' ### Add a matrix and a numeric vector
#' ###
#' a1 <- matrix(seq(8), 4, 2)
#' scores2 <- join_scores(a1, s3)
#'
#' ## Show the list structure
#' str(scores2)
#'
#'
#' ##################################################
#' ### Use byrow
#' ###
#' a2 <- matrix(seq(8), 2, 4, byrow = TRUE)
#' scores3 <- join_scores(a2, s3, byrow = TRUE)
#'
#' ## Show the list structure
#' str(scores3)
#'
#'
#' ##################################################
#' ### Use chklen
#' ###
#' s4 <- c(1, 2, 3)
#' s5 <- c(5, 6, 7, 8)
#' scores4 <- join_scores(s4, s5, chklen = FALSE)
#'
#' ## Show the list structure
#' str(scores4)
#'
#' @export
join_scores <- function(..., byrow = FALSE, chklen = TRUE) {
  # Call join datasets
  .join_datasets(..., efunc_vtype = .validate_scores, efunc_nrow = NULL,
                 byrow = byrow, chklen = chklen)
}

#' Join observed labels of multiple test datasets into a list
#'
#' \code{join_labels} takes observed labels and converts them to a list.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, matrices,
#'   data frames, and lists.
#'
#' @param byrow A Boolean value to specify whether row vectors are used
#'   for matrix, data frame, and array.
#'
#' @param chklen A Boolean value to specify whether all list items must be
#'   the same lengths.
#'
#' @return The \code{join_labels} function returns a list that
#'   contains all combined label data.
#'
#' @seealso \code{\link{evalmod}} for calculation evaluation measures.
#'   \code{\link{mmdata}} for formatting input data.
#'   \code{\link{join_scores}} for formatting scores with multiple datasets.
#'
#' @examples
#'
#' ##################################################
#' ### Add three numeric vectors
#' ###
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' labels1 <- join_labels(l1, l2, l3)
#'
#' ## Show the list structure
#' str(labels1)
#'
#'
#' ##################################################
#' ### Add a matrix and a numeric vector
#' ###
#' a1 <- matrix(rep(c(1, 0), 4), 4, 2)
#' labels2 <- join_labels(a1, l3)
#'
#' ## Show the list structure
#' str(labels2)
#'
#'
#' ##################################################
#' ### Use byrow
#' ###
#' a2 <- matrix(rep(c(1, 0), 4), 2, 4, byrow = TRUE)
#' labels3 <- join_labels(a2, l3, byrow = TRUE)
#'
#' ## Show the list structure
#' str(labels3)
#'
#'
#' ##################################################
#' ### Use chklen
#' ###
#' l4 <- c(-1, 0, -1)
#' l5 <- c(0, -1)
#' labels4 <- join_labels(l4, l5, chklen = FALSE)
#'
#' ## Show the list structure
#' str(labels4)
#'
#' @export
join_labels <- function(..., byrow = FALSE, chklen = TRUE) {
  # Call join datasets
  .join_datasets(..., efunc_vtype = .validate_labels, efunc_nrow = NULL,
                 byrow = byrow, chklen = chklen)
}

#
# Join datasets
#
.join_datasets <- function(..., efunc_vtype = NULL, efunc_nrow = NULL,
                           byrow = FALSE, chklen = TRUE) {

  # Validate arguments
  .validate_join_datasets_args(..., efunc_vtype = efunc_vtype,
                               efunc_nrow = efunc_nrow, byrow = byrow,
                               chklen = chklen)

  # Set a default error function for checking values
  if (is.null(efunc_vtype)) {
    efunc_vtype <- function(efunc_vtype) {
      if (any(is.null(efunc_vtype))) {
        stop("All vectors must contain values", call. = FALSE)
      }
    }
  }

  # Set a default error function for checking the # of rows
  if (is.null(efunc_nrow)) {
    if (chklen) {
      efunc_nrow <- function(m, vlen) {
        if (m != 0 && m != vlen) {
          stop("All vectors must be the same lengths", call. = FALSE)
        }
      }
    } else {
      efunc_nrow <- function(m, vlen) NULL
    }
  }

  # Make a list
  arglist <- list(...)
  cdat <- list()
  for (ds in arglist) {
    if (is.atomic(ds) && (is.vector(ds) || is.factor(ds))) {
      cdat <- c(cdat, list(ds))
    } else if (is.matrix(ds) || is.data.frame(ds)) {
      if (byrow) {
        cdat <- c(cdat, lapply(seq(nrow(ds)), function(i) ds[i, ]))
      } else {
        cdat <- c(cdat, lapply(seq(ncol(ds)), function(j) ds[, j]))
      }
    } else if (is.array(ds)) {
      if (length(dim(ds)) == 1) {
        cdat <- c(cdat, list(as.vector(ds)))
      } else if (length(dim(ds)) == 2) {
        if (byrow) {
          cdat <- c(cdat, lapply(seq(dim(ds)[1]), function(i) ds[i, ]))
        } else {
          cdat <- c(cdat, lapply(seq(dim(ds)[2]), function(j) ds[, j]))
        }
      } else {
        stop("Array must be 1 or 2 dimensions", call. = FALSE)
      }
    } else if (is.list(ds)) {
      if (any(unlist(lapply(ds, is.list)))) {
        f_unlist <- function(ds2) {
          new_list <- list()
          for (i in seq_len(length(ds2))) {
            if (is.list(ds2[[i]])) {
              new_list <- c(new_list, f_unlist(ds2[[i]]))
            } else {
              new_list <- c(new_list, list(ds2[[i]]))
            }
          }
          new_list
        }
        cdat <- c(cdat, f_unlist(ds))
      } else {
        cdat <- c(cdat, ds)
      }
    } else {
      stop("Cannot join this type of data", call. = FALSE)
    }
  }

  # Validate cdat with efunc_vtype and efunc_nrow
  m <- length(cdat[[1]])
  for (i in seq_len(length(cdat))) {
    efunc_vtype(cdat[[i]])
    efunc_nrow(m, length(cdat[[i]]))
  }

  cdat
}

#
# Validate arguments of .join_datasets()
#
.validate_join_datasets_args <- function(..., efunc_vtype, efunc_nrow, byrow,
                                         chklen) {

  # Check ...
  arglist <- list(...)
  if (length(arglist) == 0) {
    stop("No datasets specified", call. = FALSE)
  }

  # Check efunc_vtype
  if (!is.null(efunc_vtype)
      && (!methods::is(efunc_vtype, "function")
          || length(as.list(formals(efunc_vtype))) != 1)) {
    stop("efunc_vtype must be a function with 1 argument", call. = FALSE)
  }

  # Check efunc_nrow
  if (!is.null(efunc_nrow)
      && (!methods::is(efunc_nrow, "function")
          || length(as.list(formals(efunc_nrow))) != 2)) {
    stop("efunc_nrow must be a function with 2 arguments", call. = FALSE)
  }

  # Check byrow
  assertthat::assert_that(assertthat::is.flag(byrow),
                          assertthat::noNA(byrow))

  # Check chklen
  assertthat::assert_that(assertthat::is.flag(chklen),
                          assertthat::noNA(chklen))

}

#' Join scores of multiple models into a list.
#'
#' The \code{join_scores} function takes predicted scores from multiple models
#'   and converst them to a list.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, matrices,
#'   data frames, and lists.
#'
#' @param byrow A boolean value to specify whether row vectors are used
#'   for matrix, data.frame, and array.
#'
#' @param chklen A boolean value to specify whether all list items should be
#'   of the same length.
#'
#' @return The \code{join_scores} function returns a list that
#'   contains all combined datasets.
#'
#' @seealso \code{\link{join_labels}} for joining labels.
#'   \code{\link{mmdata}} for formating input data.
#'
#' @examples
#'
#' ## Add three numeric vectors
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' scores1 <- join_scores(s1, s2, s3)
#'
#' ## Add three numeric vectors
#' a1 <- matrix(seq(8), 4, 2)
#' scores2 <- join_scores(a1, s3)
#'
#' ## Use byrow
#' a2 <- matrix(seq(8), 2, 4, byrow = TRUE)
#' scores3 <- join_scores(a2, s3, byrow = TRUE)
#'
#' @export
join_scores <- function(..., byrow = FALSE, chklen = TRUE) {
  # Set a function to check the vector values
  if (chklen) {
    efunc_nrow <- NULL
  } else {
    efunc_nrow <- function(m, vlen) NULL
  }

  # Call join datasets
  .join_datasets(..., efunc_nrow = efunc_nrow, byrow = byrow)
}

#' Join observed labels of multiple models into a list.
#'
#' \code{join_labels} takes observed labels and converts them to a list.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, matrices,
#'   data frames, and lists.
#'
#' @param byrow A boolean value to specify whether row vectors are used
#'   for matrix, data.frame, and array.
#'
#' @param chklen A boolean value to specify whether all list items should be
#'   of the same length.
#'
#' @return The \code{join_labels} function returns a list that
#'   contains all combined datasets.
#'
#' @seealso \code{\link{join_scores}} for joining scores.
#'   \code{\link{mmdata}} for formating input data.
#'
#' @examples
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' labels <- join_labels(l1, l2, l3)
#'
#' Join observed labels of multiple models into a list.
#'
#' The \code{join_labels} function takes observed labels from multiple datasets
#'   and converst them to a list.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, matrices,
#'   data frames, and lists.
#'
#' @param byrow A boolean value to specify whether row vectors are used
#'   for matrix, data.frame, and array.
#'
#' @param chklen A boolean value to specify whether all list items should be
#'   of the same length.
#'
#' @return \code{join_scores} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' ## Add three numeric vectors
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' labels1 <- join_labels(l1, l2, l3)
#'
#' ## Add three numeric vectors
#' a1 <- matrix(rep(c(1, 0), 4), 4, 2)
#' labels2 <- join_labels(a1, l3)
#'
#' ## Use byrow
#' a2 <- matrix(rep(c(1, 0), 4), 2, 4, byrow = TRUE)
#' labels3 <- join_labels(a2, l3, byrow = TRUE)
#'
#' @export
join_labels <- function(..., byrow = FALSE, chklen = TRUE) {
  # Set a function to check the vector values
  efunc_vtype <- function(v) {
    if (!is.atomic(v) || ((!is.vector(v) || !is.numeric(v))
                          && !is.factor(v))) {
      stop("Invalid type of label data")
    } else if (length(unique(v)) != 2L) {
      stop("The number of unique labels must be 2")
    }
  }

  # Set a function to check the number of vectors
  if (chklen) {
    efunc_nrow <- NULL
  } else {
    efunc_nrow <- function(m, vlen) NULL
  }

  # Call join datasets
  .join_datasets(..., efunc_vtype = efunc_vtype, efunc_nrow = efunc_nrow,
                 byrow = byrow)
}

#
# Join datasets
#
.join_datasets <- function(..., efunc_vtype = NULL, efunc_nrow = NULL,
                           byrow = FALSE) {

  # Validate arguments
  .validate_join_datasets_args(..., efunc_vtype = efunc_vtype,
                               efunc_nrow = efunc_nrow, byrow = byrow)

  # Set a default error function for checking values
  if (is.null(efunc_vtype)) {
    efunc_vtype <- function(v) {
      if (!is.atomic(v) || !is.vector(v) || !is.numeric(v)) {
        stop("All vectors must be numeric")
      }
    }
  }

  # Set a default error function for checking the # of rows
  if (is.null(efunc_nrow)) {
    efunc_nrow <- function(m, vlen) {
      if (m != 0 && m != vlen) {
        stop("All vectors must be of the same size")
      }
    }
  }

  # Make a list
  arglist <- list(...)
  cdat <- list()
  for (ds in arglist) {
    if(is.atomic(ds) && (is.vector(ds) || is.factor(ds))) {
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
        stop("Array must be 1 or 2 dimensions")
      }
    } else if (is.list(ds)) {
      if (any(unlist(lapply(ds, is.list)))) {
        f_unlist <- function(ds2) {
          new_list <- list()
          for (i in 1:length(ds2)) {
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
      stop("Cannot join this type of data")
    }
  }

  # Validate cdat with efunc_vtype and efunc_nrow
  m <- length(cdat[[1]])
  for (i in 1:length(cdat)) {
    efunc_vtype(cdat[[i]])
    efunc_nrow(m, length(cdat[[i]]))
  }

  cdat
}

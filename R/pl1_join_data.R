#' Join scores of multiple models into a list.
#'
#' \code{join_scores} takes predicted scores from multiple models and
#' converst multiple to a list. It takes several types of datasets, such as
#' vectors, arrays, data frames, and lists.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, data frames,
#'   and lists.
#' @param byrow Column vectors are used when matrix, data.frame, or array is used.
#' @return \code{join_scores} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' scores <- join_scores(s1, s2, s3)
#'
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
#' It takes several types of datasets, such as vectors, arrays, data frames,
#' and lists.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, data frames,
#'   and lists.
#' @param byrow Column vectors are used when matrix, data.frame, or array is used.
#' @param prefix Prefix used to name models/classifiers. Serial numbers are
#'   automatically added to prefix to make unique names.
#' @return \code{join_labels} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' labels <- join_labels(l1, l2, l3)
#'
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

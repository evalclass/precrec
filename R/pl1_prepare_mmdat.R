#' Join scores of multiple models into a list.
#'
#' \code{join_pscores} takes predicted scores from multiple models and
#' converst multiple to a list. It takes several types of datasets, such as
#' vectors, arrays, data frames, and lists.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, data frames,
#'   and lists.
#' @param byrow Column vectors are used when matrix, data.frame, or array is used.
#' @return \code{join_pscores} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' mscores <- join_pscores(s1, s2, s3)
#'
#' mscores
join_pscores <- function(..., byrow = FALSE) {
  .join_datasets(..., byrow = byrow)
}

# Join datasets
.join_datasets <- function(..., efunc_vtype = NULL, efunc_nrow = NULL,
                              byrow = FALSE) {
  # Get '...'
  arglist <- list(...)
  if (length(arglist) == 0) {
    stop("No datasets specified")
  }

  # Set a default error function for checking values
  if (is.null(efunc_vtype)) {
    efunc_vtype <- function(v) {
      if (!is.atomic(v) || !is.vector(v) || !is.numeric(v)) {
        stop("All vectors must be numeric")
      }
    }
  } else if (class(efunc_vtype) != "function"
             || length(as.list(formals(efunc_vtype))) != 1) {
    stop("'efunc_vtype' must be a function with 1 argument")
  }

  # Set a default error function for checking the # of rows
  if (is.null(efunc_nrow)) {
    efunc_nrow <- function(m, vlen) {
      if (m != 0 && m != vlen) {
        stop("All vectors must be of the same size")
      }
    }
  } else if (class(efunc_nrow) != "function"
             || length(as.list(formals(efunc_nrow))) != 2) {
    stop("'efunc_nrow' must be a function with 2 arguments")
  }

  # Check byrow
  choices = c(FALSE, TRUE)
  if (length(byrow) != 1L || !(byrow %in% choices)) {
    stop(gettextf("'byrow' should be one of %s",
                  paste(choices, collapse = ", ")))
  }

  # Make a list
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
      cdat <- c(cdat, ds)
    } else {
      stop("Incorrect type of data")
    }
  }

  # Validate cdat with efunc_vtype and efunc_nrow
  m <- length(cdat[[1]])
  efuncs <- function(obj) {
    efunc_vtype(obj)
    efunc_nrow(m, length(obj))
  }
  lapply(cdat, efuncs)

  cdat
}

#' Join observed labels of multiple models into a list.
#'
#' \code{join_olabs} takes observed labels and converts them to a list.
#' It takes several types of datasets, such as vectors, arrays, data frames,
#' and lists.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, data frames,
#'   and lists.
#' @param byrow Column vectors are used when matrix, data.frame, or array is used.
#' @param prefix Prefix used to name models/classifiers. Serial numbers are
#'   automatically added to prefix to make unique names.
#' @return \code{join_olabs} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' molabs <- join_olabs(l1, l2, l3)
#'
#' molabs
join_olabs <- function(..., byrow = FALSE) {
  efunc_vtype <- function(v) {
    if (!is.atomic(v) || ((!is.vector(v) || !is.numeric(v))
                          && !is.factor(v))) {
      stop("Invalid type of label data")
    } else if (length(unique(v)) > 2L) {
      stop("Invalid number of labels")
    }
  }

  .join_datasets(..., efunc_vtype = efunc_vtype, byrow = byrow)
}

#' Reformat input data for Precision-Recall and ROC evaluation
#' with multiple models.
#'
#' \code{reformat_data} takes predicted scores from a model and binary lables
#' from an observed dataset and returns a \code{fmdat} object.
#' \code{fmdat} contains formatted labels and score ranks that are used
#' by a subsequent function, \code{\link{create_confmats}}, in the perforamcne
#' evaluation pipeline.
#'
#' @param mscores A dataset of predicted scores.
#' @param mobslabs A dataset of of observed labels.
#' @param na.last Passed to \code{\link[base]{rank}} for controlling the
#'   treatment of NAs. The value can be TRUE or FALSE. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first.
#' @param ties.method Passed to \code{\link[base]{rank}} for controlling tied
#'   scores. The value can be "average", "random", or "first". The "first"
#'   method results in a permutation with increasing values at each index
#'   set of ties. The "random" method puts these in random order whereas
#'   the default, "average", replaces them by their mean.
#' @param olevs A character vector to overide the levels of the factor for
#'   observed binary labels.
#' @param model_names Names of the models/classifiers to be evaluated.
#' @param ... Other arguments passed to other methods (ignored).
#' @return \code{reformat_data} returns an \code{mfmdat} S3 object that
#'   contains formatted labels and score ranks.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' pscores <- join_pscores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' olabs <- join_olabs(l1, l2, l3)
#'
#' mdat <- mmdata(pscores, olabs)
#' mdat
mmdata <- function(pscores, olabs, model_names = NULL, data_nos = NULL,
                   na.last = FALSE, ties.method = "average",
                   olevs = c("negative", "positive"), ...) {

  # === Join datasets ===
  lpscores <- join_pscores(pscores)
  lolabs <- join_olabs(olabs)

  # === Validate arguments and variables ===
  .validate_mmdata_args(lpscores, lolabs, model_names, data_nos,
                        na.last = na.last, ties.method = ties.method,
                        olevs = olevs, ...)

  # Replicate olabs
  if (length(lpscores) != 1 && length(lolabs) == 1) {
    lolabs <- replicate(length(lpscores), lolabs[[1]], simplify = FALSE)
  }

  # === Model names and data set numbers ===
  if (!is.null(model_names) && !is.null(data_nos)) {
    if ((length(model_names) == length(lpscores)
         &&  length(data_nos)) == length(lpscores)) {
      new_model_names <- model_names
      new_data_nos <- data_nos
    } else if ((length(model_names) * length(data_nos)) == length(lpscores)) {
      new_model_names <- rep(model_names, each = length(data_nos))
      new_data_nos <- rep(data_nos, length(model_names))
    } else {
      stop("Invalid 'model_names' & 'data_nos'")
    }
  } else if (!is.null(model_names)) {
    if (length(model_names) != length(lpscores)) {
      stop("Invalid 'model_names'")
    }
    new_model_names <- model_names
    new_data_nos <- rep(1, length(lpscores))

  } else if (!is.null(data_nos)) {
    if (length(data_nos) != length(lpscores)) {
      stop("Invalid 'data_nos'")
    }
    new_model_names <- rep("m1", length(lpscores))
    new_data_nos <- data_nos

  } else {
    new_model_names <- paste0("m", seq(length(lpscores)))
    new_data_nos <- rep(1, length(lpscores))
  }

  # === Reformat input data ===y
  func_fmdat <- function(i) {
    reformat_data(lpscores[[i]], lolabs[[i]], na.last = na.last,
                  ties.method = ties.method, olevs = olevs,
                  model_name = new_model_names[i], data_no = new_data_nos[i],
                  ...)
  }
  mmdat <- lapply(seq_along(lpscores), func_fmdat)

  # === Create an S3 object ===
  s3obj <- structure(mmdat, class = "mdat")

  # Set attributes
  attr(s3obj, "model_names") <- new_model_names
  attr(s3obj, "data_nos") <- new_data_nos
  attr(s3obj, "args") <- list(na.last = na.last,
                              ties.method = ties.method,
                              olevs = olevs)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.mmdat()
  .validate(s3obj)
}

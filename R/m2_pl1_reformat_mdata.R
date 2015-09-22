.combine_datasets <- function(..., efunc_vtype = NULL, efunc_nrow = NULL,
                              byrow = FALSE, prefix = NULL) {
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
    efunc_nrow <- function(m, tmp_m) {
      if (m != 0 && m != tmp_m) {
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

  # Set a default prefix for model names
  if (is.null(prefix)) {
    prefix <- "m"
  } else if (!is.atomic(prefix) || !is.vector(prefix) || !is.character(prefix)
             || length(prefix) != 1) {
    stop("'prefix' must be a charactor vector with length 1")
  }

  # Make a list
  cdat <- list()
  m <- 1
  for (ds in arglist) {
    if(is.atomic(ds) && (is.vector(ds) || is.factor(ds))) {
      cdat[[paste0(prefix, m)]] <- ds
      m <- m + 1
    } else if (is.matrix(ds) || is.data.frame(ds)) {
      if (byrow) {
        for (j in seq(nrow(ds))) {
          cdat[[paste0(prefix, m)]] <- ds[j, ]
          m <- m + 1
        }
      } else {
        for (j in seq(ncol(ds))) {
          cdat[[paste0(prefix, m)]] <- ds[, j]
          m <- m + 1
        }
      }
    } else if (is.array(ds)) {
      if (length(dim(ds)) == 1) {
        cdat[[paste0(prefix, m)]] <- as.vector(ds)
        m <- m + 1
      } else if (length(dim(ds)) == 2) {
        if (byrow) {
          for (j in seq(dim(ds)[1])) {
            cdat[[paste0(prefix, m)]] <- ds[j, ]
            m <- m + 1
          }
        } else {
          for (j in seq(dim(ds)[2])) {
            cdat[[paste0(prefix, m)]] <- ds[, j]
            m <- m + 1
          }
        }
      } else {
        stop("Array must be 1 or 2 dimensions")
      }
    } else if (is.list(ds)) {
      for (j in seq_along(ds)) {
        cdat[[paste0(prefix, m)]] <- ds[[j]]
        m <- m + 1
      }
    } else {
      stop("Incorrect type of data")
    }
  }

  # Validate cdat with efunc_vtype and efunc_nrow
  m <- 0
  for (i in seq(length(cdat))) {
    tmp_m <- length(cdat[[i]])

    efunc_vtype(cdat[[i]])
    efunc_nrow(m, tmp_m)
    m <- tmp_m
  }

  cdat
}

#' Combined scores of multiple models into a list.
#'
#' \code{combine_scores} takes predicted scores from multiple models and
#' converst multiple to a list. It takes several types of datasets, such as
#' vectors, arrays, data frames, and lists.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, data frames,
#'   and lists.
#' @param byrow Column vectors are used when matrix, data.frame, or array is used.
#' @param prefix Prefix used to name models/classifiers. Serial numbers are
#'   automatically added to prefix to make unique names.
#' @return \code{combine_scores} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' mscores <- combine_scores(s1, s2, s3)
#'
#' mscores
combine_scores <- function(..., byrow = FALSE, prefix = NULL) {
  .combine_datasets(..., byrow = byrow, prefix = prefix)
}

#' Combined observed labels of multiple models into a list.
#'
#' \code{combine_obslbs} takes observed labels and converts them to a list.
#' It takes several types of datasets, such as vectors, arrays, data frames,
#' and lists.
#'
#' @param ... Multiple datasets. They can be vectors, arrays, data frames,
#'   and lists.
#' @param byrow Column vectors are used when matrix, data.frame, or array is used.
#' @param prefix Prefix used to name models/classifiers. Serial numbers are
#'   automatically added to prefix to make unique names.
#' @return \code{combine_scores} returns a list that
#'   contains all combined datasets.
#'
#' @examples
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' mobslbs <- combine_obslbs(l1, l2, l3)
#'
#' mobslbs
combine_obslbs <- function(..., byrow = FALSE, prefix = NULL) {
  efunc_vtype <- function(v) {
    if (!is.atomic(v) || ((!is.vector(v) || !is.numeric(v))
                          && !is.factor(v))) {
      stop("Invalid type of label data")
    } else if (length(unique(v)) != 2L) {
      stop("Invalid number of labels")
    }
  }

  .combine_datasets(..., efunc_vtype = efunc_vtype, byrow = byrow,
                    prefix = prefix)
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
#' mscores <- combine_scores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' mobslabs <- combine_obslbs(l1, l2, l3)
#'
#' mfmdat <- reformat_mdata(mscores, mobslabs)
#' mfmdat
reformat_mdata <- function(mscores, mobslabs, na.last = FALSE,
                           ties.method = "average",
                           olevs = c("negative", "positive"),
                           model_names = list(as.character(NA)), ...) {

  # === Combine datasets ===
  lscores <- combine_scores(mscores)
  llabels <- combine_obslbs(mobslabs)
  if (length(llabels) == 1 && length(lscores) != length(llabels)) {
    llabels <- replicate(length(lscores), llabels[[1]], simplify = FALSE)
    names(llabels) <- names(lscores)
  }

  if (is.null(model_names) || length(model_names) != length(lscores)) {
    model_names <- names(lscores)
  }

  # Validate arguments and variables
  .validate_reformat_mdata_args(NULL, NULL, lscores, llabels, model_names,
                                na.last = na.last, ties.method = ties.method,
                                olevs = olevs, ...)

  # === Reformat input data ===

  # Defined a function for lapply
  func_fmdat <- function(i) {
    reformat_data(lscores[[i]], llabels[[i]], na.last = na.last,
                  ties.method = ties.method, olevs = olevs,
                  model_name = model_names[[i]], ...)
  }

  mfmdat <- lapply(seq_along(lscores), func_fmdat)

  # === Create an S3 object ===
  s3obj <- structure(mfmdat, class = "mfmdat")

  # Set attributes
  attr(s3obj, "model_names") <- model_names
  attr(s3obj, "args") <- list(na.last = na.last,
                              ties.method = ties.method,
                              olevs = olevs,
                              model_names = model_names)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.mfmdat()
  .validate(s3obj)
}


#' Create an \code{mdat} object with scores and labels of multiple models.
#'
#' \code{create_mdat} takes predicted scores from multiple models and
#' corresponding binary lables from an observed dataset. It convert multiple
#' datasets as vectors, arrays, and data frames into a single object. \code{mdat}
#' can be used as input data for \code{\link{evalmulti}}.
#'
#' @param mscores A dataset of predicted scores.
#' @param mobslabs A dataset of of observed labels.
#' @return \code{create_mdat} returns an \code{mdat} S3 object that
#'   contains scores and labels of multiple models.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' mscores <- combine_scores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' mobslabs <- combine_obslbs(l1, l2, l3)
#'
#' mdat <- create_mdat(mscores, mobslabs)
#' mdat
# create_mdat <- function(mscores, mobslabs) {
#   # === Combine datasets ===
#   lscores <- combine_scores(mscores)
#   llabels <- combine_obslbs(mobslabs)
#
#   if (length(mscores) != length(llabels) && length(llabels) != 1) {
#     stop(paste0("'mscores' and 'mobslabs' should be of the same size, or ",
#                 "the length of 'mobslabs' should be 1"))
#   }
#
#   single_mlabels <- FALSE
#   if (length(llabels) == 1) {
#     single_mlabels <- TRUE
#   }
#
#   mdat <- list()
#   for (model_name in names(lscores)) {
#     sdat = list()
#     sdat[["scores"]] = lscores[[model_name]]
#     if (single_mlabels) {
#       sdat[["obslabs"]] = llabels[[1]]
#     } else {
#       sdat[["obslabs"]] = llabels[[model_name]]
#     }
#     mdat[[model_name]] = .validate(structure(list(sdat = sdat,
#                                                   validated = FALSE),
#                                              class = "sdat"))
#   }
#
#   # === Create an S3 object ===
#   s3obj <- structure(mdat, class = "mfmdat")
#
#   # Set attributes
#   attr(s3obj, "model_name") <- model_name
#   attr(s3obj, "nn") <- nn
#   attr(s3obj, "np") <- np
#   attr(s3obj, "args") <- list(na.last = na.last,
#                               ties.method = ties.method,
#                               obslevels = obslevels,
#                               model_names = model_name)
#   attr(s3obj, "validated") <- FALSE
#
#   # Call .validate.mfmdat()
#   .validate(s3obj)
# }


#' Reformat data for Precision-Recall and ROC evaluation
#'
#' The \code{mmdata} function takes predicted scores and lables
#'   and returns an \code{mdat} object.
#'
#' @param scores A numeric data of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list.
#'
#' @param labels A numeric or factor data of observed labels.
#'   It can be a vector, a matrix, an array, a data frame, or a list.
#'
#' @param modnames A character vector as the names
#'   of the models/classifiers.
#'
#' @param setids A numeric vector as dataset IDs.
#'
#' @param expd_first Indicate which of the two vaiables - model names or dataset IDs
#'   should be expanded first when they are automatically generated.
#'
#'   \describe{
#'     \item{"modnames"}{Model names are expanded first. For example,
#'            modnames: c("m1", "m2"), setids: c(1, 1)
#'            when they are automaticlly generated.}
#'     \item{"setids"}{Dataset IDs are expanded first. For example,
#'            modnames: c("m1", "m1"), setids: c(1, 2)
#'            when they are automaticlly generated.}
#'   }
#'
#' @param na_worst A boolean value for controlling the treatment of NAs
#'   in the scores.
#'   \describe{
#'     \item{TRUE}{NAs are treated as the highest score}
#'     \item{FALSE}{NAs are treated as the lowest score}
#'   }
#'
#' @param ties_method A string for controlling tied scores.
#'   Ignored if mdat is set.
#'   \describe{
#'     \item{"equiv"}{Ties are equivalently ranked}
#'     \item{"random"}{Ties are ranked in an incresing order as appeared}
#'     \item{"first"}{ Ties are ranked in random order}
#'   }
#'
#' @param levels A character vector to overide the levels of the factor for
#'   the labels.
#'
#' @param ... Not used by this method.
#'
#' @return The \code{mmdata} function returns an \code{mdat} S3 object
#'   that contains formatted labels and score ranks.
#'
#' @seealso \code{\link{join_scores}} and \code{\link{join_labels}}
#'   for joining socre and labels.
#'
#' @examples
#' ## Generate an mdat object
#' mdat1 <- mmdata(1:8, sample(c(0, 1), 8, replace = TRUE))
#'
#' ## Use join_scores and join_labels
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' scores <- join_scores(s1, s2)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' labels <- join_labels(l1, l2)
#'
#' mdat2 <- mmdata(scores, labels)
#'
#' @export
mmdata <- function(scores, labels, modnames = NULL, setids = NULL,
                   expd_first = "modnames", na_worst = TRUE,
                   ties_method = "equiv",
                   levels = c("negative", "positive"), ...) {

  # === Join datasets ===
  lscores <- join_scores(scores)
  llabels <- join_labels(labels)

  # === Validate arguments and variables ===
  expd_first <- .pmatch_expd_first(expd_first)
  .validate_mmdata_args(lscores, llabels, modnames, setids,
                        expd_first = "modnames", na_worst = na_worst,
                        ties_method = ties_method, levels = levels)

  # Replicate labels
  if (length(lscores) != 1 && length(llabels) == 1) {
    llabels <- replicate(length(lscores), llabels[[1]], simplify = FALSE)
  }

  # === Model names and dataset IDs ===
  mnames <- .create_modnames(length(lscores), modnames, setids,
                             expd_first)
  new_modnames <- mnames[["mn"]]
  new_setids <- mnames[["ds"]]

  # === Reformat input data ===
  func_fmdat <- function(i) {
    reformat_data(lscores[[i]], llabels[[i]], na_worst = na_worst,
                  ties_method = ties_method, levels = levels,
                  modname = new_modnames[i], setid = new_setids[i],
                  ...)
  }
  mmdat <- lapply(seq_along(lscores), func_fmdat)

  # === Create an S3 object ===
  s3obj <- structure(mmdat, class = "mdat")

  # Set attributes
  attr(s3obj, "modnames") <- new_modnames
  attr(s3obj, "setids") <- new_setids
  attr(s3obj, "args") <- list(na_worst = na_worst,
                              ties_method = ties_method,
                              levels = levels)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.mdat()
  .validate(s3obj)
}

#
# Check partial match - expd_first
#
.pmatch_expd_first <- function(val) {
  if (assertthat::is.string(val)) {
    if (val == "setids" || val == "modnames") {
      return(val)
    }

    if (!is.na(pmatch(val, "setids"))) {
      return("setids")
    }

    if (!is.na(pmatch(val, "modnames"))) {
      return("modnames")
    }
  }

  val
}

#
# Check partial match - ties method
#
.pmatch_tiesmethod <- function(val) {
  if (assertthat::is.string(val)) {
    choices = c("equiv", "random", "first")
    if (val %in% choices) {
      return(val)
    }

    if (!is.na(pmatch(val, "equiv"))) {
      return("equiv")
    }

    if (!is.na(pmatch(val, "random"))) {
      return("random")
    }

    if (!is.na(pmatch(val, "first"))) {
      return("first")
    }

  }

  val
}

#
# Get model names and dataset IDs
#
.create_modnames <- function(dlen, modnames, setids,
                             expd_first = "setids") {
  len_mn <- length(modnames)
  len_ds <- length(setids)
  is_null_mn <- is.null(modnames)
  is_null_ds <- is.null(setids)

  ds_mn <- list(mn = modnames, ds = setids)

  # === Reformat model names and dataset IDs ===
  # No reformat
  if (len_mn == dlen && len_ds == dlen) {
    return(ds_mn)
  }

  # Assign a single dataset ID
  if (len_mn == dlen && is_null_ds) {
    ds_mn[["ds"]] <- rep(1, dlen)
    return(ds_mn)
  }

  # Assign a single model name
  if (is_null_mn && len_ds == dlen) {
    ds_mn[["mn"]] <- rep("m1", dlen)
    return(ds_mn)
  }

  # Expand both model names and dataset IDs
  if (len_mn * len_ds == dlen) {
    if (expd_first == "modnames") {
      ds_mn[["mn"]] <- rep(modnames, len_ds)
      ds_mn[["ds"]] <- rep(setids, each = len_mn)
    } else if (expd_first == "setids") {
      ds_mn[["mn"]] <- rep(modnames, each = len_ds)
      ds_mn[["ds"]] <- rep(setids, len_mn)
    }

    return(ds_mn)
  }

  # Expand model names and assign a single dataset ID
  if (is_null_mn && is_null_ds) {
    if (expd_first == "modnames") {
      ds_mn[["mn"]] <- paste0("m", seq(dlen))
      ds_mn[["ds"]] <- rep(1, dlen)
    } else if (expd_first == "setids") {
      ds_mn[["mn"]] <- rep("m1", dlen)
      ds_mn[["ds"]] <- seq(dlen)
    }

    return(ds_mn)
  }

  # === Error handling ===
  stop("Invalid 'modnames' and/or 'setids'")

}

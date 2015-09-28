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
#' pscores <- join_scores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' olabs <- join_labels(l1, l2, l3)
#'
#' model_names <- c("t1", "t2", "t3")
#'
#' mdat <- mmdata(pscores, olabs, model_names = model_names)
#' mdat
mmdata <- function(pscores, olabs, model_names = NULL, data_nos = NULL,
                   exp_priority = "model_names",  na.last = FALSE,
                   ties.method = "average",
                   olevs = c("negative", "positive"), ...) {

  # === Join datasets ===
  lpscores <- join_scores(pscores)
  lolabs <- join_labels(olabs)

  # === Validate arguments and variables ===
  exp_priority <- .pmatch_exp_priority(exp_priority)
  .validate_mmdata_args(lpscores, lolabs, model_names, data_nos,
                        exp_priority = "model_names", na.last = na.last,
                        ties.method = ties.method, olevs = olevs)

  # Replicate olabs
  if (length(lpscores) != 1 && length(lolabs) == 1) {
    lolabs <- replicate(length(lpscores), lolabs[[1]], simplify = FALSE)
  }

  # === Model names and data set numbers ===
  mnames <- .create_modnames(length(lpscores), model_names, data_nos,
                             exp_priority)
  new_model_names <- mnames[["mn"]]
  new_data_nos <- mnames[["dn"]]

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

  # Call .validate.mdat()
  .validate(s3obj)
}

#
# Check partial match - exp_priority
#
.pmatch_exp_priority <- function(val) {
  if (.is_single_string(val)) {
    if (val == "data_nos" || val == "model_names") {
      return(val)
    }

    if (!is.na(pmatch(val, "data_nos"))) {
      return("data_nos")
    }

    if (!is.na(pmatch(val, "model_names"))) {
      return("model_names")
    }
  }

  val
}

#
# Check partial match - ties method
#
.pmatch_tiesmethod <- function(val) {
  if (.is_single_string(val)) {
    choices = c("average", "random", "first")
    if (val %in% choices) {
      return(val)
    }

    if (!is.na(pmatch(val, "average"))) {
      return("average")
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
# Get model names and data numbers
#
.create_modnames <- function(dlen, model_names, data_nos,
                             exp_priority = "data_nos") {
  len_mn <- length(model_names)
  len_dn <- length(data_nos)
  is_null_mn <- is.null(model_names)
  is_null_dn <- is.null(data_nos)

  modnames <- list(mn = model_names, dn = data_nos)

  # === Reformat model names and data numbers ===
  # No reformat
  if (len_mn == dlen && len_dn == dlen) {
    return(modnames)
  }

  # Assign a single data number
  if (len_mn == dlen && is_null_dn) {
    modnames[["dn"]] <- rep(1, dlen)
    return(modnames)
  }

  # Assign a single model name
  if (is_null_mn && len_dn == dlen) {
    modnames[["mn"]] <- rep("m1", dlen)
    return(modnames)
  }

  # Expand both model names and data numbers
  if (len_mn * len_dn == dlen) {
    if (exp_priority == "model_names") {
      modnames[["mn"]] <- rep(model_names, len_dn)
      modnames[["dn"]] <- rep(data_nos, each = len_mn)
    } else if (exp_priority == "data_nos") {
      modnames[["mn"]] <- rep(model_names, each = len_dn)
      modnames[["dn"]] <- rep(data_nos, len_mn)
    }

    return(modnames)
  }

  # Expand model names and assign a single data number
  if (is_null_mn && is_null_dn) {
    if (exp_priority == "model_names") {
      modnames[["mn"]] <- paste0("m", seq(dlen))
      modnames[["dn"]] <- rep(1, dlen)
    } else if (exp_priority == "data_nos") {
      modnames[["mn"]] <- rep("m1", dlen)
      modnames[["dn"]] <- seq(dlen)
    }

    return(modnames)
  }

  # === Error handling ===
  stop("Invalid 'model_names' and/or 'data_nos'")

}

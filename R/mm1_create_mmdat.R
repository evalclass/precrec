#' Reformat input data for performance evaluation calculation
#'
#' The \code{mmdata} function takes predicted scores and labels
#'   and returns an \code{mdat} object. The \code{\link{evalmod}} function
#'   takes an \code{mdat} object as input data to calculate evaluation measures.
#'
#' @param scores A numeric dataset of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list. The \code{\link{join_scores}}
#'   function can be useful to make scores with multiple datasets.
#'
#' @param labels A numeric, character, logical, or factor dataset
#'   of observed labels. It can be a vector, a matrix, an array,
#'   a data frame, or a list. The \code{\link{join_labels}}
#'   function can be useful to make labels with multiple datasets.
#'
#' @param modnames A character vector for the names of the models.
#'   The \code{evalmod} function automatically generates default names
#'   as "m1", "m2", "m3", and so on when it is \code{NULL}.
#'
#' @param dsids A numeric vector for test dataset IDs.
#' The \code{evalmod} function automatically generates the default ID
#' as \code{1} when it is \code{NULL}.
#'
#' @param posclass A scalar value to specify the label of positives
#'   in \code{labels}. It must be the same data type as \code{labels}.
#'   For example, \code{posclass = -1} changes the positive label
#'   from \code{1} to \code{-1} when \code{labels} contains
#'   \code{1} and \code{-1}. The positive label will be automatically
#'   detected when \code{posclass} is \code{NULL}.
#'
#' @param na_worst A Boolean value for controlling the treatment of NAs
#'   in \code{scores}.
#'   \describe{
#'     \item{TRUE}{All NAs are treated as the worst scores}
#'     \item{FALSE}{All NAs are treated as the best scores}
#'   }
#'
#' @param ties_method A string for controlling ties in \code{scores}.
#'   \describe{
#'     \item{"equiv"}{Ties are equivalently ranked}
#'     \item{"first"}{Ties are ranked in an increasing order as appeared}
#'     \item{"random"}{ Ties are ranked in random order}
#'   }
#'
#' @param expd_first A string to indicate which of the two variables
#'   - model names or test dataset IDs
#'   should be expanded first when they are automatically generated.
#'
#'   \describe{
#'     \item{"modnames"}{Model names are expanded first. For example,
#'            The \code{mmdata} function generates \code{modnames} as
#'            \code{c("m1", "m2")} and \code{dsids} as \code{c(1, 1)}
#'            when two vectors are passed as input,
#'            and \code{modnames} and \code{dsids} are unspecified.}
#'     \item{"dsids"}{Test dataset IDs are expanded first. For example,
#'            The \code{mmdata} function generates \code{modnames} as
#'            \code{c("m1", "m1")} and \code{dsids} as \code{c(1, 2)}
#'            when two vectors are passed as input,
#'            and \code{modnames} and \code{dsids} are unspecified.}
#'   }
#'
#' @param mode A string that specifies the types of evaluation measures
#'   that the \code{evalmod} function calculates.
#'   \describe{
#'     \item{"rocprc"}{ROC and Precision-Recall curves}
#'     \item{"prcroc"}{Same as above}
#'     \item{"basic"}{Normalized ranks vs. accuracy, error rate, specificity,
#'                    sensitivity, precision, Matthews correlation coefficient,
#'                    and F-score. }
#'     \item{"aucroc"}{Fast AUC(ROC) calculation with the U statistic}
#'   }
#'
#' @param nfold_df A data frame that contains at least one score column,
#'   label and fold columns.
#'
#' @param score_cols A character/numeric vector that specifies score columns
#'   of \code{nfold_df}.
#'
#' @param lab_col A number/string that specifies the label column
#'   of \code{nfold_df}.
#'
#' @param fold_col A number/string that specifies the fold column
#'   of \code{nfold_df}.
#'
#' @param ... Not used by this method.
#'
#' @return The \code{mmdata} function returns an \code{mdat} object
#'   that contains formatted labels and score ranks. The object can
#'   be used as input data for the \code{\link{evalmod}} function.
#'
#' @seealso \code{\link{evalmod}} for calculation evaluation measures.
#'   \code{\link{join_scores}} and \code{\link{join_labels}} for formatting
#'   scores and labels with multiple datasets.
#'   \code{\link{format_nfold}} for creating n-fold cross validation dataset
#'   from data frame.
#'
#' @examples
#'
#' ##################################################
#' ### Single model & single test dataset
#' ###
#'
#' ## Load a dataset with 10 positives and 10 negatives
#' data(P10N10)
#'
#' ## Generate mdat object
#' ssmdat1 <- mmdata(P10N10$scores, P10N10$labels)
#' ssmdat1
#' ssmdat2 <- mmdata(1:8, sample(c(0, 1), 8, replace = TRUE))
#' ssmdat2
#'
#'
#' ##################################################
#' ### Multiple models & single test dataset
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(1, 100, 100, "all")
#'
#' ## Multiple models & single test dataset
#' msmdat1 <- mmdata(samps[["scores"]], samps[["labels"]],
#'                   modnames = samps[["modnames"]])
#' msmdat1
#'
#' ## Use join_scores and join_labels
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' scores <- join_scores(s1, s2)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 0, 1, 1)
#' labels <- join_labels(l1, l2)
#'
#' msmdat2 <- mmdata(scores, labels, modnames = c("ms1", "ms2"))
#' msmdat2
#'
#'
#' ##################################################
#' ### Single model & multiple test datasets
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(10, 100, 100, "good_er")
#'
#' ## Single model & multiple test datasets
#' smmdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                  modnames = samps[["modnames"]],
#'                  dsids = samps[["dsids"]])
#' smmdat
#'
#'
#' ##################################################
#' ### Multiple models & multiple test datasets
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(10, 100, 100, "all")
#'
#' ## Multiple models & multiple test datasets
#' mmmdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                  modnames = samps[["modnames"]],
#'                  dsids = samps[["dsids"]])
#' mmmdat
#'
#'
#' ##################################################
#' ### N-fold cross validation datasets
#' ###
#'
#' ## Load test data
#' data(M2N50F5)
#' head(M2N50F5)
#'
#' ## Speficy nessesary columns to create mdat
#' cvdat1 <- mmdata(nfold_df = M2N50F5, score_cols = c(1, 2),
#'                  lab_col = 3, fold_col = 4,
#'                  modnames = c("m1", "m2"), dsids = 1:5)
#' cvdat1
#'
#' ## Use column names
#' cvdat2 <- mmdata(nfold_df = M2N50F5, score_cols = c("score1", "score2"),
#'                  lab_col = "label", fold_col = "fold",
#'                  modnames = c("m1", "m2"), dsids = 1:5)
#' cvdat2
#'
#' @export
mmdata <- function(scores, labels, modnames = NULL, dsids = NULL,
                   posclass = NULL, na_worst = TRUE, ties_method = "equiv",
                   expd_first = NULL, mode = "rocprc",
                   nfold_df = NULL, score_cols = NULL, lab_col = NULL,
                   fold_col = NULL, ...) {

  # === Join datasets ===
  if (!is.null(nfold_df) && !is.null(score_cols) && !is.null(lab_col)
      && !is.null(fold_col)) {
    nfold_list <- format_nfold(nfold_df, score_cols, lab_col, fold_col)
    lscores <- nfold_list$scores
    llabels <- nfold_list$labels
    if (is.null(expd_first)) {
      expd_first <- "dsids"
    }
  } else {
    if (missing(scores) || missing(labels)) {
      stop("'scores' and/or 'lables' are missing", call. = FALSE)
    }
    lscores <- join_scores(scores, chklen = FALSE)
    llabels <- join_labels(labels, chklen = FALSE)
    if (is.null(expd_first)) {
      expd_first <- "modnames"
    }
  }

  # === Model names and dataset IDs ===
  new_expd_first <- .pmatch_expd_first(expd_first)
  mnames <- .create_modnames(length(lscores), modnames, dsids, new_expd_first)
  new_modnames <- mnames[["mn"]]
  new_dsids <- mnames[["ds"]]
  data_info <- data.frame(modnames = new_modnames, dsids = new_dsids,
                          nn = rep(NA, length(new_modnames)),
                          np = rep(NA, length(new_modnames)),
                          stringsAsFactors = FALSE)

  # === Validate arguments and variables ===
  new_mode <- .pmatch_mode(mode)
  new_ties_method <- .pmatch_tiesmethod(ties_method, ...)
  new_na_worst <- .get_new_naworst(na_worst, ...)
  .validate_mmdata_args(lscores, llabels, new_modnames, new_dsids,
                        posclass = posclass,
                        na_worst = new_na_worst, ties_method = new_ties_method,
                        expd_first = new_expd_first, mode = new_mode)

  # Replicate labels
  if (length(lscores) != 1 && length(llabels) == 1) {
    llabels <- replicate(length(lscores), llabels[[1]], simplify = FALSE)
  }

  # === Reformat input data ===
  func_fmdat <- function(i) {
    reformat_data(lscores[[i]], llabels[[i]], posclass = posclass,
                  na_worst = new_na_worst, ties_method = new_ties_method,
                  modname = new_modnames[i], dsid = new_dsids[i],
                  mode = new_mode, ...)
  }
  mmdat <- lapply(seq_along(lscores), func_fmdat)

  for (i in seq_along(mmdat)) {
    data_info[["nn"]][i] <- attr(mmdat[[i]], "nn")
    data_info[["np"]][i] <- attr(mmdat[[i]], "np")
  }

  # === Create an S3 object ===
  s3obj <- structure(mmdat, class = "mdat")

  # Set attributes
  attr(s3obj, "data_info") <- data_info
  attr(s3obj, "uniq_modnames") <- unique(new_modnames)
  attr(s3obj, "uniq_dsids") <- unique(new_dsids)
  attr(s3obj, "args") <- list(posclass = posclass,
                              na_worst = new_na_worst,
                              ties_method = new_ties_method,
                              expd_first = new_expd_first,
                              mode = new_mode)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.mdat()
  .validate(s3obj)
}

#
# Check partial match - expd_first
#
.pmatch_expd_first <- function(val) {
  if (assertthat::is.string(val)) {
    if (val == "dsids" || val == "modnames") {
      return(val)
    }

    if (!is.na(pmatch(val, "dsids"))) {
      return("dsids")
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
.pmatch_tiesmethod <- function(val, ...) {

  set_ties_dot_method <- FALSE
  arglist <- list(...)
  if (!is.null(arglist[["ties.method"]])) {
    val = arglist[["ties.method"]]
    set_ties_dot_method <- TRUE
  }

  if (assertthat::is.string(val)) {
    if (!set_ties_dot_method) {
      choices <- c("equiv", "random", "first")
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
    } else {
      choices <- c("average", "random", "last")
      if (val %in% choices) {
        return(val)
      }

      if (!is.na(pmatch(val, "average"))) {
        return("equiv")
      }

      if (!is.na(pmatch(val, "random"))) {
        return("random")
      }

      if (!is.na(pmatch(val, "last"))) {
        return("first")
      }
    }

  }

  val
}

#
# Get na worst value
#
.get_new_naworst <- function(val, ...) {
  set_na_last <- FALSE
  arglist <- list(...)
  if (!is.null(arglist[["na.last"]])) {
    val = arglist[["na.last"]]
    set_na_last <- TRUE
  }

  assertthat::is.flag(val)

  if (set_na_last) {
    val <- !val
  }

  val
}

#
# Get model names and dataset IDs
#
.create_modnames <- function(dlen, modnames, dsids,
                             expd_first = "dsids") {
  len_mn <- length(modnames)
  len_ds <- length(dsids)
  is_null_mn <- is.null(modnames)
  is_null_ds <- is.null(dsids)

  ds_mn <- list(mn = modnames, ds = dsids)

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
      ds_mn[["ds"]] <- rep(dsids, each = len_mn)
    } else if (expd_first == "dsids") {
      ds_mn[["mn"]] <- rep(modnames, each = len_ds)
      ds_mn[["ds"]] <- rep(dsids, len_mn)
    }

    return(ds_mn)
  }

  # Expand model names and assign a single dataset ID
  if (is_null_mn && is_null_ds) {
    if (expd_first == "modnames") {
      ds_mn[["mn"]] <- paste0("m", seq(dlen))
      ds_mn[["ds"]] <- rep(1, dlen)
    } else if (expd_first == "dsids") {
      ds_mn[["mn"]] <- rep("m1", dlen)
      ds_mn[["ds"]] <- seq(dlen)
    }

    return(ds_mn)
  }

  # === Error handling ===
  stop("Invalid modnames and/or dsids", call. = FALSE)

}

#
# Validate arguments of mmdata()
#
.validate_mmdata_args <- function(lscores, llabels, modnames, dsids, posclass,
                                  na_worst, ties_method, expd_first, mode) {

  # Check lscores and llabels
  if (length(llabels) != 1 && length(lscores) != length(llabels)) {
    stop(paste0("scores and labels must be the same lengths",
                ", or the length of labels must be 1"), call. = FALSE)
  }

  # Check model names
  .validate_modnames(modnames, length(lscores))

  # Check dataset IDs
  .validate_dsids(dsids, length(lscores))

  # Check posclass
  .validate_posclass(posclass)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)

  # Check expd_first
  .validate_expd_first(expd_first)

  # Check mode
  .validate_mode(mode)

  # Chekc the length of modnames and dsids
  if (length(modnames) != length(dsids)) {
    stop("modnames and dsids must be the same lengths", call. = FALSE)
  }

}

#
# Validate 'mdat' object generated by mmdata()
#
.validate.mdat <- function(mdat) {
  # Need to validate only once
  if (methods::is(mdat, "mdat") && attr(mdat, "validated")) {
    return(mdat)
  }

  # Check mdat
  if (!methods::is(mdat, "mdat")) {
    stop("mdat created by mmdata() expected", call. = FALSE)
  }

  # Validate class items and attributes
  item_names <- NULL
  attr_names <- c("data_info", "uniq_modnames", "uniq_dsids", "args",
                  "validated")
  arg_names <- c("posclass", "na_worst", "ties_method", "expd_first", "mode")
  .validate_basic(mdat, "mdat", "mmdata", item_names, attr_names,
                  arg_names)

  # Check values of class items
  if (length(mdat) != nrow(attr(mdat, "data_info"))) {
    stop("Invalid modnames and dsids", call. = FALSE)
  }

  # Chekc data consistency among the same dsids
  dsid_nn <- list()
  dsid_np <- list()
  for (i in seq_along(mdat)) {
    # Check nn and np for the same dsids
    nn <- attr(mdat[[i]], "nn")
    np <- attr(mdat[[i]], "np")
    dsid_chr <- as.character(attr(mdat[[i]], "dsid"))
    if (is.null(dsid_nn[[dsid_chr]])) {
      dsid_nn[[dsid_chr]] <- nn
      dsid_np[[dsid_chr]] <- np
    } else if (dsid_nn[[dsid_chr]] != nn || dsid_np[[dsid_chr]] != np) {
      stop(paste0("Inconsistent labels for dsid: ", dsid_chr), call. = FALSE)
    }
  }

  attr(mdat, "validated") <- TRUE
  mdat
}

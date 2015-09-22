# Factor labels
.factor_labels <- function(olabs, olevs = c("negative", "positive"),
                           validate = TRUE) {
  # === Validate input arguments ===
  if (validate) {
    .validate_olabs(olabs)
    .validate_olevs(olevs)
  }

  # === Generate label factors ===
  if (!is.factor(olabs)) {
    flabs <- factor(olabs, ordered = TRUE)
  } else {
    flabs <- rep(olabs)
  }

  if (nlevels(flabs) != length(olevs)) {
    stop("'olevs' cotains the invalid number of unique labels")
  }
  levels(flabs) <- olevs

  flabs
}

# Rank scores
.rank_scores <- function(pscores, na.last = FALSE, ties.method = "average",
                         validate = TRUE) {

  # === Validate input arguments ===
  if (validate) {
    .validate_pscores((pscores))
    .validate_na_last(na.last)
    .validate_ties_method(ties.method)
  }

  # === Create ranks ===
  ranks <- rank(pscores, na.last, ties.method)
}

#' Reformat input data for Precision-Recall and ROC evaluation.
#'
#' \code{reformat_data} takes predicted scores from a model and binary lables
#' from an observed dataset and returns a \code{fmdat} object.
#' \code{fmdat} contains formatted labels and score ranks that are used
#' by a subsequent function, \code{\link{create_confmats}}, in the perforamcne
#' evaluation pipeline.
#'
#' @param pscores A numeric vector of predicted scores.
#' @param olabs A numeric vector or a factor of observed labels.
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
#' @param model_name The name of the model/classifier to be evaluated.
#' @param ... Other arguments passed to other methods (ignored).
#' @return \code{reformat_data} returns an \code{fmdat} S3 object that
#'   contains formatted labels and score ranks.
#'
#' @examples
#' reformat_data(c(0.1, 0.2, 0.3), c(0, 1, 1))
#' reformat_data(c(0.3, 0.1, 0.2), c(-1, -1, 1))
reformat_data <- function(pscores, olabs, na.last = FALSE,
                          ties.method = "average",
                          olevs = c("negative", "positive"),
                          model_name = as.character(NA), data_no = 1L, ...) {

  # === Validate input arguments ===
  .validate_reformat_data_args(NULL, NULL, pscores, olabs, na.last = na.last,
                               ties.method = ties.method, olevs = olevs,
                               model_name = model_name, data_no = data_no, ...)

  # === Reformat input data ===
  # Get score ranks and sorted indices
  ranks <- .rank_scores(pscores, na.last, ties.method, validate = FALSE)
  rank_idx <- order(ranks, decreasing = TRUE)

  # Get a factor with "positive" and "negative"
  fmtlabs <- .factor_labels(olabs, olevs, validate = FALSE)
  num_labs <- table(fmtlabs)
  if (nlevels(fmtlabs) == 2) {
    nn <- num_labs[[olevs[1]]]
    np <- num_labs[[olevs[2]]]
  } else {
    nn <- 0
    np <- num_labs[[olevs[1]]]
  }


  # === Create an S3 object ===
  s3obj <- structure(list(olabs = fmtlabs,
                          ranks = ranks,
                          rank_idx = rank_idx),
                     class = "fmdat")

  # Set attributes
  attr(s3obj, "model_name") <- model_name
  attr(s3obj, "data_no") <- data_no
  attr(s3obj, "nn") <- nn
  attr(s3obj, "np") <- np
  attr(s3obj, "args") <- list(na.last = na.last, ties.method = ties.method,
                              olevs = olevs, model_name = model_name,
                              data_no = data_no)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.fmdat()
  .validate(s3obj)
}

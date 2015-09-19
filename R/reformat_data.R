# Factor labels
.factor_labels <- function(obslabs, levels = c("negative", "positive")) {
  # === Validate input arguments ===
  # Check obslabs
  if (!is.atomic(obslabs) || (!is.numeric(obslabs) && !is.factor(obslabs))) {
      stop("'obslabs' must be either a numeric vector or a factor")
  } else if (length(unique(obslabs)) != 2L) {
    stop("'obslabs' cotains the invalid number of unique labels")
  }

  # Check levels
  if (!is.atomic(levels) || !is.character(levels)) {
    stop("'levels' must be a charactor vector")
  } else if (length(unique(levels)) != 2L) {
    stop("'levels' cotains the invalid number of unique labels")
  }

  # === Generate label factors ===
  if (is.factor(obslabs)) {
    flabs <- rep(obslabs)
  } else {
    flabs <- factor(obslabs, ordered = TRUE)
  }
  levels(flabs) <- levels

  flabs
}

# Rank scores
.rank_scores <- function(scores, na.last = FALSE, ties.method = "average") {
  # === Validate input arguments ===
  # Check scores
  if (!is.atomic(scores) || !is.numeric(scores)) {
    stop("'scores' must be a numeric vector")
  } else if (length(scores) == 0L) {
    stop("'scores' must be length >= 1")
  }

  # Check na.last
  choices = c(FALSE, TRUE)
  if (length(na.last) != 1L || !(na.last %in% choices)) {
    stop(gettextf("'na.last' should be one of %s",
                  paste(choices, collapse = ", ")))
  }

  # Check ties.method
  choices = c("average", "random", "first")
  if (length(ties.method) != 1L || !(ties.method %in% choices)) {
    stop(gettextf("'ties.method' should be one of %s",
                  paste(dQuote(choices), collapse = ", ")))
  }

  # === Create ranks ===
  ranks <- rank(scores, na.last, ties.method)
}

#' Reformat input data for Precision-Recall and ROC evaluation.
#'
#' \code{reformat_data} takes predicted scores from a model and binary lables
#' from an observed dataset and returns a \code{fmdat} object.
#' \code{fmdat} contains formatted labels and score ranks that are used
#' by a subsequent function, \code{\link{create_confmats}}, in the perforamcne
#' evaluation pipeline.
#'
#' @param scores A numeric vector of predicted scores.
#' @param obslabs A numeric vector or a factor of observed labels.
#' @param na.last Passed to \code{\link[base]{rank}} for controlling the
#'   treatment of NAs. The value can be TRUE or FALSE. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first.
#' @param ties.method Passed to \code{\link[base]{rank}} for controlling tied
#'   scores. The value can be "average", "random", or "first". The "first"
#'   method results in a permutation with increasing values at each index
#'   set of ties. The "random" method puts these in random order whereas
#'   the default, "average", replaces them by their mean.
#' @param levels A character vector to overide the levels of the factor for
#'   observed binary labels.
#' @param model_name The name of the model/classifier to be evaluated.
#' @param ... Other arguments passed to other methods (ignored).
#' @return \code{reformat_data} returns an \code{fmdat} S3 object that
#'   contains formatted labels and score ranks.
#'
#' @examples
#' reformat_data(c(0.1, 0.2, 0.3), c(0, 1, 1))
#' reformat_data(c(0.3, 0.1, 0.2), c(-1, -1, 1))
reformat_data <- function(scores, obslabs, na.last = FALSE,
                          ties.method = "average",
                          levels = c("negative", "positive"),
                          model_name = as.character(NA)) {
  # === Validate input arguments ===
  if (missing(scores) || missing(obslabs)
      || is.null(scores) || is.null(obslabs)) {
    stop("'scores' and 'obslabs' must be specified")
  } else if (length(obslabs) != length(scores)) {
    stop("'scores' and 'obslabs' must be the same length")
  }
  # Check model_name
  if (!is.atomic(model_name) || !is.character(model_name)) {
    stop("'model_name' must be a character vector")
  } else if (length(model_name) != 1) {
    stop("'model_name' must be a single string")
  }

  # === Validate and reformat input data ===
  # Get score ranks and sorted indices
  ranks <- .rank_scores(scores, na.last, ties.method)
  rank_idx <- order(ranks, decreasing = TRUE)

  # Get a factor with "positive" and "negative"
  obslabs <- .factor_labels(obslabs, levels)

  # === Create an S3 object ===
  # Call .validate.labs_ranks()
  .validate(structure(list(obslabs = obslabs,
                           ranks = ranks,
                           rank_idx = rank_idx,
                           na.last = c(na.last),
                           ties.method = c(ties.method),
                           levels = c(levels),
                           model_name = c(model_name),
                           validated = FALSE),
                      class = "fmdat"))
}

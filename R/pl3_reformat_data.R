#
# Reformat input data for Precision-Recall and ROC evaluation
#
reformat_data <- function(scores, labels, na.last = FALSE,
                          ties.method = "average",
                          levels = c("negative", "positive"),
                          model_name = as.character(NA), setid = 1L, ...) {

  # === Validate input arguments ===
  .validate_reformat_data_args(NULL, NULL, scores, labels, na.last = na.last,
                               ties.method = ties.method, levels = levels,
                               model_name = model_name, setid = setid, ...)

  # === Reformat input data ===
  # Get score ranks and sorted indices
  ranks <- .rank_scores(scores, na.last, ties.method, validate = FALSE)
  rank_idx <- order(ranks, decreasing = TRUE)

  # Get a factor with "positive" and "negative"
  fmtlabs <- .factor_labels(labels, levels, validate = FALSE)
  num_labs <- table(fmtlabs)
  if (nlevels(fmtlabs) == 2) {
    nn <- num_labs[[levels[1]]]
    np <- num_labs[[levels[2]]]
  } else {
    nn <- 0
    np <- num_labs[[levels[1]]]
  }


  # === Create an S3 object ===
  s3obj <- structure(list(labels = fmtlabs,
                          ranks = ranks,
                          rank_idx = rank_idx),
                     class = "fmdat")

  # Set attributes
  attr(s3obj, "model_name") <- model_name
  attr(s3obj, "setid") <- setid
  attr(s3obj, "nn") <- nn
  attr(s3obj, "np") <- np
  attr(s3obj, "args") <- list(na.last = na.last, ties.method = ties.method,
                              levels = levels, model_name = model_name,
                              setid = setid)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.fmdat()
  .validate(s3obj)
}

#
# Factor labels
#
.factor_labels <- function(labels, levels = c("negative", "positive"),
                           validate = TRUE) {
  # === Validate input arguments ===
  if (validate) {
    .validate_labels(labels)
    .validate_levels(levels)
  }

  # === Generate label factors ===
  if (!is.factor(labels)) {
    flabs <- factor(labels, ordered = TRUE)
  } else {
    flabs <- rep(labels)
  }

  if (nlevels(flabs) != length(levels)) {
    stop("levels must cotain two unique labels")
  }
  levels(flabs) <- levels

  flabs
}

#
# Rank scores
#
.rank_scores <- function(scores, na.last = FALSE, ties.method = "average",
                         validate = TRUE) {

  # === Validate input arguments ===
  if (validate) {
    .validate_scores((scores))
    .validate_na_last(na.last)
    .validate_ties_method(ties.method)
  }

  # === Create ranks ===
  ranks <- rank(scores, na.last, ties.method)
}

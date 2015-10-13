#
# Reformat input data for Precision-Recall and ROC evaluation
#
reformat_data <- function(scores, labels, na_worst = TRUE,
                          ties_method = "equiv",
                          levels = c("negative", "positive"),
                          model_name = as.character(NA), setid = 1L, ...) {

  # === Validate input arguments ===
  .validate_reformat_data_args(NULL, NULL, scores, labels, na_worst = na_worst,
                               ties_method = ties_method, levels = levels,
                               model_name = model_name, setid = setid, ...)

  # === Reformat input data ===
  # Get score ranks and sorted indices
  #   ranks <- .rank_scores(scores, na_worst, ties_method, validate = FALSE)
  #   rank_idx <- order(ranks, decreasing = TRUE)

  sranks <- .rank_scores(scores, na_worst, ties_method, validate = FALSE)
  ranks <- sranks[["ranks"]]
  rank_idx <- sranks[["rank_idx"]]

  # Get a factor with "positive" and "negative"
  fmtlabs <- .factor_labels(labels, levels, validate = FALSE)

  # === Create an S3 object ===
  s3obj <- structure(list(labels = fmtlabs[["labels"]],
                          ranks = ranks,
                          rank_idx = rank_idx),
                     class = "fmdat")

  # Set attributes
  attr(s3obj, "model_name") <- model_name
  attr(s3obj, "setid") <- setid
  attr(s3obj, "nn") <- fmtlabs[["nn"]]
  attr(s3obj, "np") <- fmtlabs[["np"]]
  attr(s3obj, "args") <- list(na_worst = na_worst, ties_method = ties_method,
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
  flabels <- format_labels(labels)
  .check_cpp_func_error(flabels, "format_labels")

  flabels
}

#
# Rank scores
#
.rank_scores <- function(scores, na_worst = TRUE, ties_method = "equiv",
                         validate = TRUE) {

  # === Validate input arguments ===
  if (validate) {
    .validate_scores((scores))
    .validate_na_worst(na_worst)
    .validate_ties_method(ties_method)
  }

  # === Create ranks ===
  #   ranks <- rank(scores, na_worst, ties_method)
  sranks <- get_score_ranks(scores, na_worst, ties_method)
  .check_cpp_func_error(sranks, "get_score_ranks")

  sranks
}

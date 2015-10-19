#
# Reformat input data for Precision-Recall and ROC evaluation
#
reformat_data <- function(scores, labels,
                          modname = as.character(NA), dsid = 1L,
                          posclass = NULL, na_worst = TRUE,
                          ties_method = "equiv", ...) {

  # === Validate input arguments ===
  .validate_reformat_data_args(scores, labels, modname = modname, dsid = dsid,
                               posclass = posclass, na_worst = na_worst,
                               ties_method = ties_method, ...)

  # === Reformat input data ===
  # Get score ranks and sorted indices
  sranks <- .rank_scores(scores, na_worst, ties_method, validate = FALSE)
  ranks <- sranks[["ranks"]]
  rank_idx <- sranks[["rank_idx"]]

  # Get a factor with "positive" and "negative"
  fmtlabs <- .factor_labels(labels, posclass, validate = FALSE)

  # === Create an S3 object ===
  s3obj <- structure(list(labels = fmtlabs[["labels"]],
                          ranks = ranks,
                          rank_idx = rank_idx),
                     class = "fmdat")

  # Set attributes
  attr(s3obj, "modname") <- modname
  attr(s3obj, "dsid") <- dsid
  attr(s3obj, "nn") <- fmtlabs[["nn"]]
  attr(s3obj, "np") <- fmtlabs[["np"]]
  attr(s3obj, "args") <- list(na_worst = na_worst, ties_method = ties_method,
                              modname = modname,
                              dsid = dsid)
  attr(s3obj, "validated") <- FALSE

  # Call .validate.fmdat()
  .validate(s3obj)
}

#
# Factor labels
#
.factor_labels <- function(labels, posclass, validate = TRUE) {
  # === Validate input arguments ===
  if (validate) {
    .validate_labels(labels)
    .validate_posclass(posclass)
  }

  # Update posclass if nesessary
  if (is.null(posclass)) {
    posclass = NA
  } else if (is.factor(labels)) {
    lv <- levels(labels)
    posclass <- which(lv == posclass)
  }

  # === Generate label factors ===
  flabels <- format_labels(labels, posclass)
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

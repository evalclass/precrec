#
# Calculate confusion matrices for all possible threshold values
#
create_confmats <- function(fmdat, scores = NULL, labels = NULL,
                            keep_fmdat = FALSE, ...) {
  # === Validate input arguments ===
  # Create fmdat from scores and labels if fmdat is missing
  fmdat <- .create_src_obj(fmdat, "fmdat", reformat_data, scores, labels, ...)
  .validate(fmdat)

  # === Create confusion matrices for all possible threshold values ===
  # Call a cpp function via Rcpp interface
  cmats <- create_confusion_matrices(fmdat[["labels"]], fmdat[["ranks"]],
                                     fmdat[["rank_idx"]])
  .check_cpp_func_error(cmats, "create_confusion_matrices")

  # === Create an S3 object ===
  cpp_errmsg <- cmats[["errmsg"]]
  cmats[["errmsg"]] <- NULL
  s3obj <- structure(cmats, class = "cmats")

  # Set attributes
  attr(s3obj, "modname") <- attr(fmdat, "modname")
  attr(s3obj, "dsid") <- attr(fmdat, "dsid")
  attr(s3obj, "nn") <- attr(fmdat, "nn")
  attr(s3obj, "np") <- attr(fmdat, "np")
  attr(s3obj, "args") <- list(...)
  attr(s3obj, "cpp_errmsg") <- cpp_errmsg
  if (keep_fmdat) {
    attr(s3obj, "src") <- fmdat
  } else {
    attr(s3obj, "src") <- NA
  }
  attr(s3obj, "validated") <- FALSE

  # Call .validate.cmats()
  .validate(s3obj)
}

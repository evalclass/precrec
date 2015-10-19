#
# Calculate basic evaluation measurs from confusion matrices
#
calc_measures <- function(cmats, scores = NULL, labels = NULL, ...) {

  # === Validate input arguments ===
  # Create cmats from scores and labels if cmats is missing
  cmats <- .create_src_obj(cmats, "cmats", create_confmats, scores, labels,
                           ...)
  .validate(cmats)

  # === Create confusion matrices for all possible threshold values ===
  # Call a cpp function via Rcpp interface
  pevals <- calc_basic_measures(attr(cmats, "np"), attr(cmats, "nn"),
                                cmats[["tp"]], cmats[["fp"]],
                                cmats[["tn"]], cmats[["fn"]])
  .check_cpp_func_error(pevals, "calc_basic_measures")

  # === Create an S3 object ===
  s3obj <- structure(pevals["basic"], class = "pevals")

  # Set attributes
  attr(s3obj, "modname") <- attr(cmats, "modname")
  attr(s3obj, "dsid") <- attr(cmats, "dsid")
  attr(s3obj, "nn") <- attr(cmats, "nn")
  attr(s3obj, "np") <- attr(cmats, "np")
  attr(s3obj, "args") <- list(...)
  attr(s3obj, "cpp_errmsg") <- pevals[["errmsg"]]
  attr(s3obj, "src") <- cmats
  attr(s3obj, "validated") <- FALSE

  # Call .validate.cmats()
  .validate(s3obj)
}

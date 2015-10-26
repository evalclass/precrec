#
# Print mdat
#
#' @export
print.mdat <- function(x, ...) {
  cat("\n")
  cat("     === Input data ===\n\n")

  data_info <- attr(x, "data_info")
  rownames(data_info) <- format(rownames(data_info), width = 4,
                                justify = "right")
  colnames(data_info) <- c("Model name", "Dataset ID", "# of positives",
                           "# of negatives")

  print.data.frame(data_info, print.gap = 2)

  cat("\n")
}

#
# Print the summary of ROC and Precision-Recall curves
#
#' @export
print.curve_info <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  cat("\n")
  cat("    === AUCs ===\n")
  cat("\n")

  aucs <- attr(x, "aucs")
  rownames(aucs) <- format(rownames(aucs), width = 4, justify = "right")
  colnames(aucs) <- c("Model name", "Dataset ID", "Curve type", "AUC")

  print.data.frame(aucs, print.gap = 2)
  cat("\n")

  print.mdat(x)
}

#
# Print the summary of basic performace evaluation measures
#
#' @export
print.beval_info <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  cat("\n")
  cat("    === Basic performance evaluation measures ===\n")
  cat("\n")

  eval_summary <- attr(x, "eval_summary")
  rownames(eval_summary) <- format(rownames(eval_summary), width = 4,
                                   justify = "right")
  colnames(eval_summary) <- c("Model name", "Dataset ID", "Measure", "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

  print.data.frame(eval_summary, print.gap = 2)
  cat("\n")

  print.mdat(x)

}

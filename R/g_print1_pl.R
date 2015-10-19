#
# Print mdat
#
#' @export
print.mdat <- function(x, ...) {

  cat("\n  Input data\n\n")

  data_info <- attr(x, "data_info")
  rownames(data_info) <- format(rownames(data_info), width = 4,
                                justify = "right")
  colnames(data_info) <- c("Model name", "Dataset ID", "# of positives",
                           "# of negatives")

  print(data_info, print.gap = 2)

  cat("\n")
}

#
# Print model name and data size
#
.print_basic_curve_info <- function(x, indent_space = " ",
                                    show_test_data = FALSE) {

  if (show_test_data) {
    cat(indent_space, "Test data\n")
    cat(indent_space, "  # of positives:", attr(x, "np"), "\n")
    cat(indent_space, "  # of negatives:", attr(x, "nn"), "\n")
    cat("\n")
  }

  cat(indent_space, "Model name:", attr(x, "modname"), "\n")
  cat("\n")
}

#
# Print ROC curve summary
#
print.roc_curve <- function(x, show_test_data = FALSE, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  cat("\n  ROC curve\n\n")
  .print_basic_curve_info(x, "   ", show_test_data = show_test_data)
  cat("    AUC:", attr(x, "auc"), "\n\n")
}

#
# Print Precision-Recall curve summary
#
print.prc_curve <- function(x, show_test_data = FALSE, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  cat("\n  Precision-Recall curve\n\n")
  .print_basic_curve_info(x, "   ", show_test_data = show_test_data)
  cat("    AUC:", attr(x, "auc"), "\n\n")
}

#
# Print the summary of ROC and Precision-Recall curves
#
print.curves <- function(x, show_test_data = FALSE, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  cat("\n")
  .print_basic_curve_info(x[["roc"]], show_test_data = show_test_data)

  cat("  ROC curve\n")
  cat("    AUC:", attr(x[["roc"]], "auc"), "\n\n")

  cat("  Precision-Recall curve\n")
  cat("    AUC:", attr(x[["prc"]], "auc"), "\n\n")

}

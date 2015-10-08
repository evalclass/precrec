#
# Print sscurves
#
#' @export
print.sscurves <- function(x, show_test_data = FALSE, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  print(x[["rocs"]])
  print(x[["prcs"]])
  if (show_test_data) {
    .print_testdat_ss(x[["rocs"]])
  }
}

#
# Print ssroc
#
print.ssroc <- function(x, ...) {
  cat("\n  ROC curve\n\n")
  cat("    AUC:", attr(x[[1]], "auc"), "\n\n")
}

#
# Print ssprc
#
print.ssprc <- function(x, ...) {
  # === Print summary ===
  cat("\n  Precision-Recall curve\n\n")
  cat("    AUC:", attr(x[[1]], "auc"), "\n\n")
}

#
# Print test data
#
.print_testdat_ss <- function(x, ...) {
  # === Print summary ===
  cat("\n  Test data\n\n")
  for (nm in names(x)) {
    cat("    Model name:", nm, "\n")
    cat("      # of positives:", attr(x[[nm]], "np"), "\n")
    cat("      # of negatives:", attr(x[[nm]], "nn"), "\n\n")
  }
}

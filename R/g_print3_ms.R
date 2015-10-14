#
# Print the summary of ROC and Precision-Recall curves with multiple models
#
#' @export
print.mscurves <- function(x, show_test_data = FALSE, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  print(x[["rocs"]])
  print(x[["prcs"]])
  if (show_test_data) {
    .print_testdat_ms(x[["rocs"]])
  }
}

#
# Print msroc
#
print.msroc <- function(x, ...) {
  cat("\n  ROC curves\n\n")
  .print_mscurves_base(x, ...)
}

#
# Print msprc
#
print.msprc <- function(x, ...) {
  # === Print summary ===
  cat("\n  Precision-Recall curves\n\n")
  .print_mscurves_base(x, ...)
}

#
# Print test data
#
.print_testdat_ms <- function(x, ...) {
  # === Print summary ===
  cat("\n  Test data\n\n")

  for (i in seq_along(x)) {
    cat("    Model name:", attr(x[[i]], "modname"), "\n")
    cat("      # of positives:", attr(x[[i]], "np"), "\n")
    cat("      # of negatives:", attr(x[[i]], "nn"), "\n\n")
  }
}

#
# Print mscurves
#
.print_mscurves_base <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  for (i in seq_along(x)) {
    cat("    Model name:", attr(x[[i]], "modname"), "\n")
    cat("      AUC:", attr(x[[i]], "auc"), "\n\n")
  }
}

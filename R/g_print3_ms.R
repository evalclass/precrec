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
  pfunc <- function(s) {
    cat("    Model name:", attr(x[[s]], "model_name"), "\n")
    cat("      # of positives:", attr(x[[s]], "np"), "\n")
    cat("      # of negatives:", attr(x[[s]], "nn"), "\n\n")
  }
  lapply(seq_along(x), pfunc)

  invisible(NULL)
}

#
# Print mscurves
#
.print_mscurves_base <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  pfunc <- function(s) {
    cat("    Model name:", attr(x[[s]], "model_name"), "\n")
    cat("      AUC:", attr(x[[s]], "auc"), "\n\n")
  }
  lapply(seq_along(x), pfunc)

  invisible(NULL)
}

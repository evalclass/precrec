#
# Print the summary of ROC and Precision-Recall curves with multiple datasets
#
#' @export
print.smcurves <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  print(x[["rocs"]])
  print(x[["prcs"]])
}

#
# Print msroc
#
print.smroc <- function(x, ...) {
  cat("\n  ROC curves\n\n")
  .print_smcurves_base(x, ...)
}

#
# Print msprc
#
print.smprc <- function(x, ...) {
  # === Print summary ===
  cat("\n  Precision-Recall curves\n\n")
  .print_smcurves_base(x, ...)
}

#
# Print test data
#
.print_testdat_ms <- function(x, ...) {
  # === Print summary ===
  cat("\n  Test data\n\n")
  pfunc <- function(s) {
    cat("    Dataset ID:", attr(x[[s]], "setid"), "\n")
    cat("      # of positives:", attr(x[[s]], "np"), "\n")
    cat("      # of negatives:", attr(x[[s]], "nn"), "\n\n")
  }
  lapply(seq_along(x), pfunc)

  invisible(NULL)
}

#
# Print smcurves
#
.print_smcurves_base <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  pfunc <- function(s) {
    cat("    Dataset ID:", attr(x[[s]], "setid"), "\n")
    cat("           AUC:", attr(x[[s]], "auc"), "\n\n")
  }
  lapply(seq_along(x), pfunc)

  invisible(NULL)
}

#
# Print the summary of ROC and Precision-Recall curves with multiple datasets
#
#' @export
print.mmcurves <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  print(x[["rocs"]])
  print(x[["prcs"]])
}

#
# Print mmroc
#
print.mmroc <- function(x, ...) {
  cat("\n  ROC curves\n\n")
  .print_mmcurves_base(x, ...)
}

#
# Print mmprc
#
print.mmprc <- function(x, ...) {
  # === Print summary ===
  cat("\n  Precision-Recall curves\n\n")
  .print_mmcurves_base(x, ...)
}

#
# Print mmcurves
#
.print_mmcurves_base <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === Print summary ===
  setid <- NA
  for (i in seq_along(x)) {
    if (is.na(setid) || setid != attr(x[[i]], "setid")) {
      cat("    Dataset ID:", attr(x[[i]], "setid"), "\n\n")
      setid <- attr(x[[i]], "setid")
    }
    cat("      Model name:", attr(x[[i]], "model_name"), "\n")
    cat("        AUC:", attr(x[[i]], "auc"), "\n\n")
  }
}

#
# Print mdat
#
#' @export
print.mdat <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === print ===
  cat("\n")
  cat("    === Input data ===\n\n")

  data_info <- attr(x, "data_info")
  rownames(data_info) <- format(rownames(data_info), width = 4,
                                justify = "right")
  colnames(data_info) <- c("Model name", "Dataset ID", "# of negatives",
                           "# of positives")

  print.data.frame(data_info, print.gap = 1)

  cat("\n")
}

#
# Print the summary of ROC and Precision-Recall curves
#
#' @export
print.curve_info <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === print ===
  cat("\n")
  cat("    === AUCs ===\n")
  cat("\n")

  aucs <- attr(x, "aucs")
  rownames(aucs) <- format(rownames(aucs), width = 4, justify = "right")
  colnames(aucs) <- c("Model name", "Dataset ID", "Curve type", "AUC")

  print.data.frame(aucs, print.gap = 1)
  cat("\n")

  if (attr(x, "partial")) {
    cat("\n")

    paucs <- attr(x, "paucs")
    rownames(paucs) <- format(rownames(paucs), width = 4, justify = "right")
    if (ncol(paucs) == 4) {
      cat("    === partial AUCs (average curves only) ===\n")
      colnames(paucs) <- c("Model name", "Curve type", "pAUC", "Standardized")
    } else {
      cat("    === partial AUCs ===\n")
      colnames(paucs) <- c("Model name", "Dataset ID", "Curve type", "pAUC",
                           "Standardized")
    }

    cat("\n")
    print.data.frame(paucs, print.gap = 1)
    cat("\n")
  }

  print.mdat(x)
}

#
# Print the summary of basic performance evaluation measures
#
#' @export
print.beval_info <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === print ===
  cat("\n")
  cat("    === Basic performance evaluation measures ===\n\n")
  cat("     ## Performance measures (Meas.)\n")
  cat("      rank:   normalized rank\n")
  cat("      score:  score\n")
  cat("      label:  label\n")
  cat("      err:    error rate\n")
  cat("      acc:    accuracy\n")
  cat("      sp:     specificity\n")
  cat("      sn:     sensitivity\n")
  cat("      prec:   precision\n")
  cat("      mcc:    Matthews correlation coefficient\n")
  cat("      fscore: F-score\n")
  cat("\n\n")

  eval_summary <- attr(x, "eval_summary")
  rownames(eval_summary) <- format(rownames(eval_summary), width = 4,
                                   justify = "right")
  colnames(eval_summary) <- c("Model", "ID", "Meas.", "Min.",
                              "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  evaltypes <- c("rank", "score", "label", "err", "acc", "sp", "sn", "prec",
                 "mcc", "fscore")
  eval_summary[, "Meas."] <- evaltypes

  print.data.frame(eval_summary, print.gap = 1)
  cat("\n")

  print.mdat(x)
}

#
# Print the summary of AUC(ROC) with U statistic
#
#' @export
print.aucroc <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === print ===
  cat("\n")
  cat("    === Input data ===\n\n")

  data_info <- attr(x, "data_info")
  rownames(data_info) <- format(rownames(data_info), width = 4,
                                justify = "right")
  colnames(data_info) <- c("Model name", "Dataset ID", "# of negatives",
                           "# of positives")

  print.data.frame(data_info, print.gap = 1)
  cat("\n\n")

  cat("    === AUCs ===\n")
  cat("\n")

  aucs <- as.data.frame(x)
  rownames(aucs) <- format(rownames(aucs), width = 4, justify = "right")
  colnames(aucs) <- c("Model name", "Dataset ID", "AUC", "U")

  print.data.frame(aucs, print.gap = 1)
  cat("\n")
}

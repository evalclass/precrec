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

  data_info <- .as_plain_df(attr(x, "data_info"), copy = TRUE)
  rownames(data_info) <- format(rownames(data_info),
    width = 4,
    justify = "right"
  )
  # By lookup rather than by position: a multiclass object carries a class
  # column that binary input does not have
  col_labels <- c(
    modnames = "Model name", dsids = "Dataset ID", classes = "Class",
    nn = "# of negatives", np = "# of positives"
  )
  colnames(data_info) <- unname(col_labels[colnames(data_info)])

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

  aucs <- .as_plain_df(attr(x, "aucs"), copy = TRUE)
  rownames(aucs) <- format(rownames(aucs), width = 4, justify = "right")
  colnames(aucs) <- c("Model name", "Dataset ID", "Curve type", "AUC")

  print.data.frame(aucs, print.gap = 1)
  cat("\n")

  if (attr(x, "partial")) {
    cat("\n")

    paucs <- .as_plain_df(attr(x, "paucs"), copy = TRUE)
    rownames(paucs) <- format(rownames(paucs), width = 4, justify = "right")
    if (ncol(paucs) == 4) {
      cat("    === partial AUCs (average curves only) ===\n")
      colnames(paucs) <- c("Model name", "Curve type", "pAUC", "Standardized")
    } else {
      cat("    === partial AUCs ===\n")
      colnames(paucs) <- c(
        "Model name", "Dataset ID", "Curve type", "pAUC",
        "Standardized"
      )
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
  cat("      bacc:   balanced accuracy\n")
  cat("      npv:    negative predictive value\n")
  cat("      infm:   informedness (Youden's J)\n")
  cat("      mkd:    markedness\n")
  cat("      kappa:  Cohen's kappa\n")
  cat("\n\n")

  eval_summary <- .as_plain_df(attr(x, "eval_summary"), copy = TRUE)
  rownames(eval_summary) <- format(rownames(eval_summary),
    width = 4,
    justify = "right"
  )
  colnames(eval_summary) <- c(
    "Model", "ID", "Meas.", "Min.",
    "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."
  )
  evaltypes <- c("rank", unname(.basic_metric_names()))
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

  data_info <- .as_plain_df(attr(x, "data_info"), copy = TRUE)
  rownames(data_info) <- format(rownames(data_info),
    width = 4,
    justify = "right"
  )
  colnames(data_info) <- c(
    "Model name", "Dataset ID", "# of negatives",
    "# of positives"
  )

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

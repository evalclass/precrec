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
# The short name and the description of each measure, one line each
#
# The normalized rank is not a measure and has no row in the table, but it is
# a row of the summary below, so it heads the list.
#
.metric_legend <- function(metrics) {
  tab <- .basic_metric_table()
  tab <- tab[match(metrics, tab$name), ]
  short <- c("rank", tab$short)
  desc <- c("normalized rank", tab$desc)
  paste0("      ", formatC(paste0(short, ":"), width = -7), " ", desc, "\n")
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
  # Only the measures this object holds - `evalmod(metrics = )` decides that
  cat(.metric_legend(.get_obj_metrics(x)), sep = "")
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
  evaltypes <- c("rank", unname(.basic_metric_names(.get_obj_metrics(x))))
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

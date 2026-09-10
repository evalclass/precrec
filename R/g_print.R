#' Print the summary of a precrec object
#'
#' The `print` function prints a summary of an `S3` object created by
#'   [mmdata()], [evalmod()], [metric_curve()] or
#'   [classification_report()]. It is called for its side effect, and is
#'   what the console shows when one of those objects is evaluated at the
#'   prompt.
#'
#' @param x An `S3` object created by [mmdata()], [evalmod()],
#'   [metric_curve()] or [classification_report()]. The `print` function
#'   takes one of the following `S3` objects.
#'
#'   | **`S3` object** | **Created by** |
#'   |-----------------|----------------|
#'   | `mdat` | [mmdata()] |
#'   | `curve_info` | [evalmod()] |
#'   | `beval_info` | `evalmod(mode = "basic")` |
#'   | `aucroc` | `evalmod(mode = "aucroc")` |
#'   | `xycurve_info` | [metric_curve()] |
#'   | `classification_report` | [classification_report()] |
#'
#'   Every object but a `classification_report` includes a summary of the
#'   input data - the model names, the dataset IDs and the class counts.
#'   Alongside it, a curve object reports its AUCs, and its partial AUCs
#'   as well when it came from [part()]; a basic-metric object reports
#'   what each metric abbreviation means and a five-number summary of
#'   every metric; an `aucroc` object reports the AUCs beside the U
#'   statistics they came from; and a [metric_curve()] object names the
#'   metric pair and counts the points on it. A `classification_report`
#'   prints its own table of per-class precision, recall and F-score.
#'
#'   The curve and point objects carry a second class naming how many
#'   models and test datasets they hold, such as `sscurves` or `mmpoints`,
#'   but all of them print through `curve_info` or `beval_info`. See the
#'   **Value** section of [evalmod()].
#'
#' @param digits The number of digits after the decimal point, between `0`
#'   and `20`. Used by the `classification_report` method only.
#'
#' @param ... Not used by these methods.
#'
#' @return The `print` function returns `x` invisibly.
#'
#' @seealso [evalmod()] and [mmdata()] for creating the objects,
#'   [as.data.frame()] for the same results as a data frame, and
#'   [auc()] for the AUCs alone.
#'
#' @examples
#'
#' ##################################################
#' ### Input data
#' ###
#'
#' ## Load a dataset with 10 positives and 10 negatives
#' data(P10N10)
#'
#' mdat <- mmdata(P10N10$scores, P10N10$labels)
#' mdat
#'
#'
#' ##################################################
#' ### ROC and Precision-Recall curves
#' ###
#'
#' curves <- evalmod(mdat)
#' curves
#'
#' ## Partial curves also report the partial AUCs
#' part(curves, xlim = c(0, 0.25))
#'
#'
#' ##################################################
#' ### Basic evaluation metrics
#' ###
#'
#' points <- evalmod(mdat, mode = "basic")
#' points
#'
#'
#' ##################################################
#' ### AUC with the U statistic
#' ###
#'
#' evalmod(mdat, mode = "aucroc")
#'
#'
#' ##################################################
#' ### One metric against another
#' ###
#'
#' metric_curve(mdat)
#'
#'
#' ##################################################
#' ### Per-class precision, recall and F-score
#' ###
#'
#' classification_report(mdat, at = 12)
#'
#' @name print
NULL

#
# Print mdat
#
#' @rdname print
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

  invisible(x)
}

#
# Print the summary of ROC and Precision-Recall curves
#
#' @rdname print
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

  invisible(x)
}

#
# The short name and the description of each metric, one line each
#
# The normalized rank is not a metric and has no row in the table, but it is
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
# Print the summary of basic performance evaluation metrics
#
#' @rdname print
#' @export
print.beval_info <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === print ===
  cat("\n")
  cat("    === Basic performance evaluation metrics ===\n\n")
  cat("     ## Performance metrics\n")
  # Only the metrics this object holds - `evalmod(metrics = )` decides that
  cat(.metric_legend(.get_obj_metrics(x)), sep = "")
  cat("\n\n")

  eval_summary <- .as_plain_df(attr(x, "eval_summary"), copy = TRUE)
  rownames(eval_summary) <- format(rownames(eval_summary),
    width = 4,
    justify = "right"
  )
  colnames(eval_summary) <- c(
    "Model", "ID", "Metric", "Min.",
    "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."
  )
  evaltypes <- c("rank", unname(.basic_metric_names(.get_obj_metrics(x))))
  eval_summary[, "Metric"] <- evaltypes

  print.data.frame(eval_summary, print.gap = 1)
  cat("\n")

  print.mdat(x)

  invisible(x)
}

#
# Print the summary of AUC(ROC) with U statistic
#
#' @rdname print
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

  invisible(x)
}

#
# Print the summary of an object of metric_curve()
#
#' @rdname print
#' @export
print.xycurve_info <- function(x, ...) {
  # === Validate input arguments ===
  .validate(x)

  # === print ===
  cat("\n")
  cat("    === ", .xycurve_title_label(x), " ===\n\n", sep = "")

  curve <- attr(x, "curve")
  if (is.na(curve)) {
    cat("     The points of this pair are not joined by a line.\n")
    cat("     No interpolation is defined between them; see\n")
    cat("     ?metric_curve for the pairs that have one.\n")
  } else {
    cat("     A registered pair: this is the ", curve, " curve, and is\n",
      sep = ""
    )
    cat("     calculated by the same code as evalmod(mode = \"rocprc\").\n")
  }
  cat("\n")

  npoints <- .map_int(x[["xy"]], function(cv) length(cv[["x"]]))
  summ <- data.frame(
    modnames = attr(x, "data_info")[["modnames"]],
    dsids = attr(x, "data_info")[["dsids"]],
    npoints = npoints
  )
  rownames(summ) <- format(rownames(summ), width = 4, justify = "right")
  colnames(summ) <- c("Model name", "Dataset ID", "# of points")

  print.data.frame(summ, print.gap = 1)
  cat("\n")

  print.mdat(x)

  invisible(x)
}


#' @rdname print
#' @export
print.classification_report <- function(x, digits = 2, ...) {
  # === Validate input arguments ===
  .assert_number(digits, "digits", min = 0, max = 20, whole = TRUE)

  summaries <- c("accuracy", "micro avg", "macro avg", "weighted avg")
  blocks <- unique(as.data.frame(x)[c("modnames", "dsids")])
  labeled <- nrow(blocks) > 1L

  # A blank cell rather than a repeated value: `accuracy` is a single number
  # over the whole block, not something precision and recall each have
  num <- function(v) {
    ifelse(is.na(v), "", formatC(v, format = "f", digits = digits))
  }

  width <- max(nchar(c(x[["class"]], "weighted avg"))) + 1L
  header <- sprintf(
    "%*s %9s %9s %9s %9s", width, "", "precision", "recall", "f1-score",
    "support"
  )

  for (b in seq_len(nrow(blocks))) {
    rows <- as.data.frame(x)[
      x[["modnames"]] == blocks[["modnames"]][b] &
        x[["dsids"]] == blocks[["dsids"]][b], ,
      drop = FALSE
    ]

    cat("\n")
    if (labeled) {
      cat(sprintf(
        "    === %s, dataset %s ===\n\n", blocks[["modnames"]][b],
        blocks[["dsids"]][b]
      ))
    }
    cat(header, "\n\n", sep = "")

    is_summary <- rows[["class"]] %in% summaries
    for (i in seq_len(nrow(rows))) {
      # sklearn sets the summary rows off from the per-class ones
      if (is_summary[i] && (i == 1L || !is_summary[i - 1L])) {
        cat("\n")
      }
      cat(sprintf(
        "%*s %9s %9s %9s %9d\n", width, rows[["class"]][i],
        num(rows[["precision"]][i]), num(rows[["recall"]][i]),
        num(rows[["fscore"]][i]), as.integer(rows[["support"]][i])
      ))
    }
  }

  cat("\n")
  invisible(x)
}

#' Get every evaluation metric at every cutoff
#'
#' The `metric_table` function returns one row per cutoff and one column
#'   per metric, in the manner of `pROC::coords` and the cutoff slots of
#'   a `ROCR::performance` object. It is the table counterpart of
#'   [metric_curve()], which projects one metric against another.
#'
#' @param x An `S3` object created by the [mmdata()] function, or a
#'   basic-metric object created by `evalmod(mode = "basic")`. The
#'   `metric_table` function ignores `scores` and `labels` when `x` is
#'   specified. These arguments are internally passed to the [mmdata()]
#'   function when `x` is unspecified. In that case, both `scores` and
#'   `labels` must be at least specified.
#'
#' @param scores A numeric dataset of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list.
#'
#' @param labels A numeric, character, logical, or factor dataset
#'   of observed labels. It can be a vector, a matrix, an array,
#'   a data frame, or a list.
#'
#' @param metrics A character vector of the metrics to calculate in
#'   addition to the default set, or the string `"all"` for every metric
#'   `precrec` knows, exactly as [evalmod()] takes it. It must be
#'   unspecified when `x` is already a basic-metric object, which carries
#'   the metrics it was built with.
#'
#' @param ... These additional arguments are passed to [evalmod()], and
#'   through it to [mmdata()], when this function builds the basic metrics
#'   itself. `beta`, `cost_fp`, `cost_fn`, `basic_ties`, `modnames`,
#'   `dsids` and `posclass` are the useful ones here.
#'
#' @return The `metric_table` function returns a data frame with one row
#'   per cutoff per model per test dataset, and the following columns.
#'
#'   \tabular{ll}{
#'     `modname` \tab Model name \cr
#'     `dsid` \tab Test dataset ID \cr
#'     `rank` \tab Number of instances called positive, `0` to `n` \cr
#'     `normalized_rank` \tab `rank / n`, the x axis of the basic
#'       metric plots \cr
#'     `score` \tab The cutoff itself, see below \cr
#'     `label` \tab `1` if the instance at this rank is positive,
#'       `-1` if it is negative \cr
#'     ... \tab One column per metric, named as [evalmod()] names them \cr
#'   }
#'
#' @section What a row means:
#'
#' Row `rank = k` is the cutoff that calls the top `k` instances positive,
#' so `score` is the score of the instance at rank `k` and the rule the row
#' stands for is `score >= that value`. The metrics on the row are the
#' metrics of that rule.
#'
#' The first row is `rank = 0` - call nothing positive. There is no
#' instance at that rank, so `score` and `label` are `NA` while the metrics
#' are defined, and it is kept because it is a real operating point.
#'
#' Tied scores share one value of each metric rather than taking a value
#' that depends on the order the ties arrived in. `basic_ties` of
#' [evalmod()] controls that.
#'
#' @section Several test datasets:
#'
#' A cutoff belongs to the dataset it was read off, so the rows are per
#' dataset and nothing is averaged across them - unlike the basic metric
#' plots, which average by default. A basic-metric object passed as `x`
#' therefore has to have been built with `raw_curves = TRUE` when it holds
#' more than one dataset, since an object built without it keeps only the
#' average.
#'
#' @seealso [evalmod()] for calculating the metrics, [metric_curve()] for
#'   projecting one metric against another, and [as.data.frame()] for the
#'   long form the plots use.
#'
#' @examples
#'
#' ##################################################
#' ### Single model & single test dataset
#' ###
#'
#' ## Load a dataset with 10 positives and 10 negatives
#' data(P10N10)
#'
#' ## Get every metric at every cutoff
#' tab <- metric_table(scores = P10N10$scores, labels = P10N10$labels)
#' head(tab)
#'
#' ## The cutoff that maximizes the F-score
#' tab[which.max(tab$fscore), c("rank", "score", "precision", "fscore")]
#'
#'
#' ##################################################
#' ### Add metrics beyond the default set
#' ###
#'
#' lifted <- metric_table(
#'   scores = P10N10$scores, labels = P10N10$labels,
#'   metrics = c("lift", "jaccard")
#' )
#' head(lifted[, c("rank", "score", "precision", "lift", "jaccard")])
#'
#'
#' ##################################################
#' ### Reuse an object that is already calculated
#' ###
#'
#' points <- evalmod(
#'   scores = P10N10$scores, labels = P10N10$labels,
#'   mode = "basic"
#' )
#' head(metric_table(points))
#'
#' @export
metric_table <- function(x, scores = NULL, labels = NULL, metrics = NULL,
                         ...) {
  # === Get a basic-metric object ===
  obj <- .metric_table_obj(
    if (missing(x)) NULL else x, scores, labels, metrics, ...
  )

  # === Pivot the long form the plots use ===
  # The long form is what every other consumer of the basic metrics wants,
  # so it is built by the same C++ converter rather than a second path,
  # and turned on its side here. `reduce_points` stays off: a table is
  # read, not drawn, and thinning it would drop cutoffs the caller asked
  # about.
  long <- .dataframe_common(obj, mode = "basic", raw_curves = TRUE)
  wide <- data.table::dcast(
    long, modname + dsid + x ~ type,
    value.var = "y"
  )
  data.table::setnames(wide, "x", "normalized_rank")

  # === Put the rank back on the count scale ===
  # The x axis is the rank divided by the number of instances, which is
  # the axis that makes several test sets comparable. The count is what a
  # caller acts on, so both are reported.
  ns <- .metric_table_sizes(obj)
  wide <- merge(wide, ns, by = c("modname", "dsid"), sort = FALSE)
  data.table::set(wide,
    j = "rank",
    value = as.integer(round(wide[["normalized_rank"]] * wide[["n"]]))
  )
  data.table::set(wide, j = "n", value = NULL)

  .as_plain_df(.metric_table_order(wide, obj))
}

#
# Resolve the first argument into a basic-metric object
#
# Three doors, the same three `evalmod()` and `auc_boot()` offer, plus one
# this function adds: an object that has already been calculated. Reusing
# it is the point of the function - the metrics are in there, only the
# shape is wrong - so recalculating them would defeat it.
#
.metric_table_obj <- function(x, scores, labels, metrics, ...) {
  if (inherits(x, "beval_info")) {
    if (!is.null(metrics)) {
      .stop_invalid_arg(
        paste(
          "{.arg metrics} cannot be set when {.arg x} is already a",
          "basic-metric object, which carries the metrics it was built",
          "with. Pass {.arg metrics} to {.fn evalmod} instead."
        ),
        arg = "metrics"
      )
    }
    .validate(x)

    # `.check_raw_curves()` would catch this further down, but it talks
    # about a `raw_curves` argument that this function does not have.
    if (attr(x, "dataset_type") == "multiple") {
      args <- attr(x, "args")
      if (!isTRUE(args[["calc_avg"]]) || !isTRUE(args[["raw_curves"]])) {
        .stop_invalid_arg(
          paste(
            "{.arg x} holds several test datasets but keeps only their",
            "average, and a cutoff belongs to the dataset it was read",
            "off. Rebuild it with",
            "{.code evalmod(mode = \"basic\", raw_curves = TRUE)}."
          ),
          arg = "x"
        )
      }
    }

    return(x)
  }

  if (inherits(x, "curve_info") || inherits(x, "aucroc")) {
    .stop_invalid_arg(
      paste(
        "{.arg x} must contain basic metrics. Curves and the fast AUC do",
        "not keep the scores, so the cutoffs cannot be recovered from",
        "them - call {.fn evalmod} with {.code mode = \"basic\"}, or pass",
        "the scores and labels to {.fn metric_table} directly."
      ),
      arg = "x"
    )
  }

  # `raw_curves` is forced on: the rows are per test dataset, so the
  # average alone is not enough to build the table from.
  if (is.null(x)) {
    evalmod(
      scores = scores, labels = labels, mode = "basic",
      metrics = metrics, raw_curves = TRUE, ...
    )
  } else {
    evalmod(x, mode = "basic", metrics = metrics, raw_curves = TRUE, ...)
  }
}

#
# The number of instances behind each model and test dataset
#
.metric_table_sizes <- function(obj) {
  info <- .as_plain_df(attr(obj, "data_info"), copy = TRUE)

  data.table::data.table(
    modname = factor(info[["modnames"]], levels = attr(obj, "uniq_modnames")),
    dsid = factor(info[["dsids"]], levels = attr(obj, "uniq_dsids")),
    n = info[["np"]] + info[["nn"]]
  )
}

#
# Put the columns and the rows in a readable order
#
# The metric columns follow the order of the metric table, which is the
# order the panels and the summary use, so a caller who knows one knows
# the other.
#
.metric_table_order <- function(wide, obj) {
  keys <- c("modname", "dsid", "rank", "normalized_rank")
  ordered <- names(.basic_metric_names(.get_obj_metrics(obj)))
  cols <- c(keys, ordered[ordered %in% names(wide)])
  data.table::setcolorder(wide, cols)
  data.table::setorderv(wide, c("modname", "dsid", "rank"))

  wide
}

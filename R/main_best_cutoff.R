#' Choose an operating point
#'
#' The `best_cutoff` function picks the cutoff that optimizes one
#'   evaluation metric, in the manner of `pROC::coords(x, "best")`. It
#'   returns one row per model per test dataset - a row of
#'   [metric_table()], together with the criterion that chose it.
#'
#' @param x An `S3` object created by the [mmdata()] function, or a
#'   basic-metric object created by `evalmod(mode = "basic")`. The
#'   `best_cutoff` function ignores `scores` and `labels` when `x` is
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
#' @param metric A string naming the metric to optimize. It can be any of
#'   the metrics [evalmod()] calculates, together with two names that come
#'   from the cutpoint literature rather than from the metric table.
#'
#'   \tabular{lll}{
#'     **Name** \tab **Metric** \tab **Optimum** \cr
#'     `"youden"` \tab `informedness` \tab Maximum \cr
#'     `"topleft"` \tab `roc_dist` \tab Minimum \cr
#'     `"fscore"` \tab `fscore` \tab Maximum \cr
#'     `"mcc"` \tab `mcc` \tab Maximum \cr
#'     `"cost"` \tab `cost` \tab Minimum \cr
#'   }
#'
#'   The direction is a property of the metric, so it is not an argument -
#'   `error`, `cost`, `roc_dist`, the two error rates and the negative
#'   likelihood ratio are minimized, and the rest are maximized. `score`,
#'   `label`, `predicted_positive_rate` and `predicted_negative_rate`
#'   describe a cutoff rather than score it, and are refused.
#'
#' @param ... These additional arguments are passed to [evalmod()], and
#'   through it to [mmdata()], when this function builds the metrics
#'   itself. `beta` for `"fscore"`, `cost_fp` and `cost_fn` for `"cost"`,
#'   and `basic_ties`, `modnames`, `dsids` and `posclass` are the useful
#'   ones here.
#'
#' @return The `best_cutoff` function returns a data frame with one row per
#'   model per test dataset, and the following columns.
#'
#'   \tabular{ll}{
#'     `modname` \tab Model name \cr
#'     `dsid` \tab Test dataset ID \cr
#'     `metric` \tab The metric that was optimized \cr
#'     `value` \tab Its value at the chosen cutoff \cr
#'     `rank` \tab Number of instances called positive, `1` to `n` \cr
#'     `normalized_rank` \tab `rank / n` \cr
#'     `score` \tab The cutoff itself, the rule being `score >= ` it \cr
#'     `label` \tab `1` if the instance at this rank is positive,
#'       `-1` if it is negative \cr
#'     ... \tab One column per metric, as [metric_table()] returns them \cr
#'   }
#'
#'   The row is a row of [metric_table()], so every other metric is there
#'   to be read at the same cutoff - which is the point of returning the
#'   whole row rather than the threshold alone.
#'
#' @section The criterion matters more than the threshold:
#'
#' Youden's J and the closest point to the top left corner are the two
#' criteria most often reached for, and both are computed from sensitivity
#' and specificity alone. Both of those are conditioned on the true class,
#' so neither knows the prevalence, and on imbalanced data the cutoff they
#' choose can sit at a precision no one would deploy. That is the argument
#' this package makes about the ROC curve, one step further down the
#' pipeline.
#'
#' \tabular{ll}{
#'   **Prevalence-blind** \tab `"youden"`, `"topleft"` \cr
#'   **Prevalence-aware** \tab `"fscore"`, `"mcc"`, `"cost"` \cr
#' }
#'
#' The default is `"youden"` because that is what a caller arriving from
#' another package expects. On imbalanced data it is the wrong default, and
#' `"mcc"` or a `"cost"` weighted by what the two mistakes actually cost is
#' the better choice. The *Balanced and imbalanced data* article shows the
#' two disagreeing on the same dataset:
#' <https://evalclass.github.io/precrec/articles/howto-imbalanced-data.html>.
#'
#' @section Ties, and what is not here:
#'
#' Several cutoffs can share the optimum. The one with the smallest `rank`
#' is returned - the threshold that calls the fewest instances positive -
#' so the result is one row per model per test dataset whatever the data
#' does.
#'
#' Rows that share a `score` are an exception, because they are one
#' threshold seen several times rather than several cutoffs: `score >= `
#' that value calls all of the tied instances positive, so the `rank`
#' reported is the last of the run and not the first. That is what
#' `evalmod(basic_ties = "hold")` produces throughout, since it gives every
#' cutoff in a run of tied scores the counts of the whole run.
#'
#' The rank `0` row of [metric_table()] - the rule that calls nothing
#' positive - is not a candidate. There is no threshold that predicts
#' nothing, so its `score` is `NA` and it is not an operating point, which
#' is what this function returns. It would otherwise win outright wherever
#' the empty rule is optimal, and win on the tie-break wherever it merely
#' ties: precision at rank `0` is the limit from above, so it is `1`
#' whenever the top-ranked instance is a positive, and any classifier that
#' ranks one first would come back with no threshold at all.
#'
#' A metric that has no interior optimum is still optimized at an end of the
#' range, and no warning says so: `sensitivity` is largest when everything
#' is called positive, and `specificity` when as little as possible is.
#' Those are the correct answers to the question asked, and rarely the
#' question meant. `accuracy` and `error` have an interior optimum on
#' balanced data and lose it as positives get rare, which is the same trap
#' arriving by a different route: at a few percent positives, calling almost
#' nothing positive is close to the most accurate thing a classifier can do.
#'
#' A cutoff chosen on the same data the model is evaluated on is optimistic,
#' by however much the criterion was free to chase. Choosing it on held-out
#' data is the fix, and it is the caller's to make: nothing here resamples
#' or cross-validates the choice.
#'
#' @seealso [metric_table()] for every metric at every cutoff, which this
#'   function takes one row of, [evalmod()] for calculating the metrics,
#'   and [prbe()] for the precision-recall break-even point, which is an
#'   operating point defined by a crossing rather than by an optimum.
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
#' ## The cutoff that maximizes Youden's J
#' best_cutoff(scores = P10N10$scores, labels = P10N10$labels)
#'
#'
#' ##################################################
#' ### A criterion that knows the prevalence
#' ###
#'
#' best_cutoff(scores = P10N10$scores, labels = P10N10$labels, metric = "mcc")
#'
#' ## Weighted by what the two mistakes cost
#' best_cutoff(
#'   scores = P10N10$scores, labels = P10N10$labels,
#'   metric = "cost", cost_fp = 1, cost_fn = 5
#' )
#'
#'
#' ##################################################
#' ### Multiple models & multiple test datasets
#' ###
#'
#' samps <- create_sim_samples(2, 50, 50, c("poor_er", "good_er"))
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'   modnames = samps[["modnames"]], dsids = samps[["dsids"]]
#' )
#' best_cutoff(mdat, metric = "fscore")[, 1:7]
#'
#' @export
best_cutoff <- function(x, scores = NULL, labels = NULL, metric = "youden",
                        ...) {
  # === Validate input arguments ===
  criterion <- .best_cutoff_metric(metric)

  # === Get the table this picks a row out of ===
  tab <- .best_cutoff_table(
    if (missing(x)) NULL else x, scores, labels, criterion, ...
  )

  .best_cutoff_rows(tab, criterion)
}

#
# Resolve the `metric` argument to a metric this can be optimized on
#
# One namespace, the metric names, plus the two names the cutpoint
# literature uses for metrics this package calls something else. `youden`
# is a metric alias proper and lives in the shared table; `topleft` names a
# criterion rather than a metric, so it is resolved here.
#
.best_cutoff_metric <- function(metric) {
  .assert_string(metric, "metric")

  if (metric == "topleft" || metric == "top_left") {
    metric <- "roc_dist"
  }
  name <- .pmatch_metric_names(metric)

  tab <- .basic_metric_table()
  idx <- match(name, tab$name)
  if (!is.na(idx) && is.na(tab$direction[idx])) {
    .stop_invalid_arg(
      paste(
        "{.arg metric} is {.val {name}}, which describes a cutoff rather",
        "than scoring it, so it has no best value. Name a metric that",
        "scores the cutoff instead, such as {.val mcc} or {.val youden}."
      ),
      arg = "metric", .envir = environment()
    )
  }
  .assert_choice(name, "metric", tab$name[!is.na(tab$direction)])

  name
}

#
# Build, or borrow, a metric table that holds the criterion
#
# The criterion is often one of the metrics `evalmod()` does not calculate
# by default - `roc_dist` and `cost` both are - so it is requested by name.
# `metrics` is additive, so the default set comes back with it and the
# returned row carries every metric a caller might want to read at the
# chosen cutoff.
#
.best_cutoff_table <- function(x, scores, labels, criterion, ...) {
  # `at` is a formal of `metric_table()`, so it would bind there and
  # quietly narrow the cutoffs searched rather than being ignored
  if ("at" %in% names(list(...))) {
    .stop_invalid_arg(
      paste(
        "{.arg at} names the cutoffs to report, and this function chooses",
        "one instead. Call {.fn metric_table} with {.arg at} to read the",
        "metrics at given thresholds, or drop it to search every cutoff."
      ),
      arg = "at"
    )
  }

  if (inherits(x, "beval_info")) {
    if (!criterion %in% .get_obj_metrics(x)) {
      .stop_invalid_arg(
        paste(
          "{.arg x} does not hold the {.val {criterion}} metric, so the",
          "cutoff that optimizes it cannot be found. Rebuild it with",
          "{.code evalmod(mode = \"basic\", metrics = \"{criterion}\")}."
        ),
        arg = "metric", .envir = environment()
      )
    }
    return(metric_table(x, ...))
  }

  if (is.null(x)) {
    metric_table(
      scores = scores, labels = labels, metrics = criterion, ...
    )
  } else {
    metric_table(x, metrics = criterion, ...)
  }
}

#
# Take the optimizing row of each model and test dataset
#
# `metric_table()` returns the rows sorted by rank within each pairing, so
# `which.max()` and `which.min()` break a tie toward the smallest rank on
# their own - the threshold that calls the fewest instances positive.
#
.best_cutoff_rows <- function(tab, criterion) {
  direction <- .metric_direction(criterion)
  values <- tab[[criterion]]

  key <- paste(tab[["modname"]], tab[["dsid"]], sep = "\r")
  groups <- unique(key)
  picked <- vapply(groups, function(g) {
    rows <- which(key == g)

    # The rank 0 row is the rule that calls nothing positive, and there is
    # no threshold that predicts nothing - its `score` is NA. This function
    # names an operating point, so that row is not a candidate for one. It
    # would otherwise win outright wherever the empty rule is optimal, which
    # `specificity` always is and `accuracy` becomes as positives get rare,
    # and win on the tie-break wherever it merely ties - `precision` is 1
    # there whenever the top-ranked instance is a positive, so any classifier
    # that ranks one first would come back with no threshold at all.
    rows <- rows[tab[["rank"]][rows] != 0L]

    if (length(rows) == 0L) {
      return(NA_integer_)
    }

    vals <- values[rows]
    if (all(is.na(vals))) {
      return(NA_integer_)
    }
    # An NA is not a candidate, so push it to the losing end rather than
    # letting `which.max()` decide what to do with it
    vals[is.na(vals)] <- if (direction == "max") -Inf else Inf
    at <- if (direction == "max") which.max(vals) else which.min(vals)
    .best_cutoff_run_end(rows, at, vals, tab[["score"]][rows])
  }, integer(1))

  out <- tab[picked, , drop = FALSE]
  # A pairing whose criterion is `NA` throughout keeps its row and its
  # identity, in the manner of `prbe()`, rather than dropping out of the
  # result and leaving the caller to notice
  first <- match(groups, key)
  out[["modname"]] <- tab[["modname"]][first]
  out[["dsid"]] <- tab[["dsid"]][first]

  out <- cbind(
    out[, c("modname", "dsid"), drop = FALSE],
    data.frame(
      metric = rep(criterion, nrow(out)),
      value = out[[criterion]],
      stringsAsFactors = FALSE
    ),
    out[, setdiff(names(out), c("modname", "dsid")), drop = FALSE]
  )
  rownames(out) <- NULL

  out
}

#
# Walk to the end of a run of rows that all name the same threshold
#
# Rows that share a `score` are one cutoff seen several times, and only the
# last of them has the `rank` that cutoff produces: `score >= ` that value
# calls all of the tied instances positive, not the first of them. Under
# `basic_ties = "hold"` a whole run carries one value of every metric, so
# the optimum lands on the first row of the run and the rank has to be
# walked forward to match the score beside it.
#
# The tie-break between cutoffs that genuinely differ is untouched - those
# have different scores, and the loop stops at the first of them.
#
.best_cutoff_run_end <- function(rows, at, vals, scores) {
  while (at < length(rows)) {
    here <- scores[at]
    there <- scores[at + 1L]
    if (is.na(here) || is.na(there) || here != there) {
      break
    }
    if (!identical(vals[at], vals[at + 1L])) {
      break
    }
    at <- at + 1L
  }

  rows[at]
}

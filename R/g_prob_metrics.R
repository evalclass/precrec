#' Calculate the Brier score and the log loss of predicted probabilities
#'
#' The `prob_metrics` function calculates two probability-based evaluation
#'   metrics - the Brier score and the log loss - for prediction scores that
#'   are probabilities. Unlike ROC and Precision-Recall curves, both metrics
#'   depend on the values of the scores rather than on their ranks, so the
#'   scores must lie in the range \[0, 1\].
#'
#' @param mdat An `S3` object created by the [mmdata()] function.
#'   It contains formatted scores and labels. The `prob_metrics`
#'   function ignores `scores` and `labels` when `mdat`
#'   is specified. These arguments are internally passed to the
#'   [mmdata()] function when `mdat` is unspecified. In that case,
#'   both `scores` and `labels` must be at least specified.
#'
#' @param scores A numeric dataset of predicted probabilities. It can be a
#'   vector, a matrix, an array, a data frame, or a list.
#'
#' @param labels A numeric, character, logical, or factor dataset
#'   of observed labels. It can be a vector, a matrix, an array,
#'   a data frame, or a list.
#'
#' @param eps A numeric value used to clamp the scores away from `0`
#'   and `1` before the log loss is calculated. A single confident and
#'   wrong prediction would otherwise make the log loss infinite.
#'
#' @param ... These additional arguments are passed to [mmdata()]
#'   for data preparation.
#'
#' @return The `prob_metrics` function returns a data frame with the
#'   columns `modnames`, `dsids`, `metrics`, and
#'   `values`. `metrics` is either "brier" or "logloss", so
#'   each model and dataset combination takes up two rows.
#'
#' @seealso [prob_metrics_ci()] for the CIs of these metrics over multiple
#'   datasets. [evalmod()] for generating `S3` objects with
#'   performance evaluation measures. [mmdata()] for formatting input data.
#'
#' @examples
#'
#' ##################################################
#' ### Single model & single test dataset
#' ###
#'
#' ## Predicted probabilities of 10 positives and 10 negatives
#' set.seed(1)
#' scores <- c(runif(10, 0.4, 1), runif(10, 0, 0.6))
#' labels <- c(rep(1, 10), rep(0, 10))
#'
#' ## Brier score and log loss
#' prob_metrics(scores = scores, labels = labels)
#'
#'
#' ##################################################
#' ### Multiple models & multiple test datasets
#' ###
#'
#' ## The "poor_er" and "good_er" samples are drawn from beta distributions,
#' ## so their scores can be read as probabilities
#' samps <- create_sim_samples(4, 100, 100, c("poor_er", "good_er"))
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'   modnames = samps[["modnames"]],
#'   dsids = samps[["dsids"]]
#' )
#'
#' pm <- prob_metrics(mdat)
#'
#' ## Show the Brier scores only
#' subset(pm, metrics == "brier")
#'
#' @export
prob_metrics <- function(mdat, scores = NULL, labels = NULL, eps = 1e-15,
                         ...) {
  # === Validate input arguments ===
  mdat <- .create_src_obj(mdat, "mdat", mmdata, scores, labels, ...)
  .validate(mdat)
  .validate_eps(eps)

  # === Calculate the metrics of each model and dataset ===
  modnames <- attr(mdat, "data_info")[["modnames"]]
  dsids <- attr(mdat, "data_info")[["dsids"]]

  pfunc <- function(i) {
    sc <- mdat[[i]][["scores"]]
    .check_prob_scores(sc, modnames[i], dsids[i])

    # Labels are stored as 1 for negatives and 2 for positives
    pm <- calc_prob_metrics(sc, as.integer(mdat[[i]][["labels"]]) - 1L, eps)
    .check_cpp_func_error(pm, "calc_prob_metrics")

    data.table::data.table(
      modnames = rep(modnames[i], 2),
      dsids = rep(dsids[i], 2),
      metrics = c("brier", "logloss"),
      values = c(pm[["brier"]], pm[["logloss"]])
    )
  }

  # Freshly built, so setDF needs no copy
  .as_plain_df(.rbind_parts(.map_idx(mdat, pfunc)))
}

#' Calculate CIs of the Brier score and the log loss
#'
#' The `prob_metrics_ci` function calculates the confidence intervals of
#'   the Brier score and the log loss when multiple test datasets are
#'   specified.
#'
#' @inheritParams prob_metrics
#'
#' @param alpha A numeric value of the significant level (default: 0.05)
#'
#' @param dtype A string to specify the distribution used for CI calculation.
#'
#'   | **dtype**        | **distribution**    |
#'   |------------------|---------------------|
#'   | normal (default) | Normal distribution |
#'   | z                | Normal distribution |
#'   | t                | t-distribution      |
#'
#' @return The `prob_metrics_ci` function returns a data frame with the
#'   columns `modnames`, `metrics`, `mean`, `error`,
#'   `lower_bound`, `upper_bound`, and `n`.
#'
#' @seealso [prob_metrics()] for the per-dataset metrics themselves.
#'   [auc_ci()] for the equivalent calculation on AUC scores.
#'
#' @examples
#'
#' ##################################################
#' ### Single model & multiple test datasets
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(4, 100, 100, "good_er")
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'   modnames = samps[["modnames"]],
#'   dsids = samps[["dsids"]]
#' )
#'
#' ## Calculate the CIs
#' prob_metrics_ci(mdat)
#'
#' @export
prob_metrics_ci <- function(mdat, scores = NULL, labels = NULL, eps = 1e-15,
                            alpha = 0.05, dtype = "normal", ...) {
  # === Validate input arguments ===
  .assert_number(alpha, "alpha", min = 0, max = 1)
  dtype <- .pmatch_dtype(dtype)
  pmetrics <- prob_metrics(mdat, scores, labels, eps = eps, ...)

  if (length(unique(pmetrics[["dsids"]])) < 2) {
    .stop_invalid_arg(
      "{.arg mdat} must contain multiple datasets.",
      arg = "mdat"
    )
  }

  # === Calculate a CI per model and metric ===
  # The Brier score cannot leave [0, 1]; the log loss is unbounded above
  bounds <- list(brier = c(0, 1), logloss = c(0, Inf))

  ci_parts <- list()
  for (modname in unique(pmetrics[["modnames"]])) {
    pm_mod <- pmetrics[pmetrics$modnames == modname, ]
    for (metric in unique(pm_mod[["metrics"]])) {
      vals <- pm_mod[pm_mod$metrics == metric, "values"]
      ci <- .calc_ci_stats(vals, alpha, dtype,
        lower = bounds[[metric]][1], upper = bounds[[metric]][2]
      )

      ci_parts[[length(ci_parts) + 1L]] <- data.table::data.table(
        modnames = modname,
        metrics = metric,
        mean = ci[["mean"]],
        error = ci[["error"]],
        lower_bound = ci[["lower_bound"]],
        upper_bound = ci[["upper_bound"]],
        n = ci[["n"]]
      )
    }
  }

  # Freshly built, so setDF needs no copy
  .as_plain_df(.rbind_parts(ci_parts))
}

#
# Check that scores can be read as probabilities
#
.check_prob_scores <- function(scores, modname, dsid) {
  if (anyNA(scores)) {
    .stop_invalid_arg(
      paste(
        "{.arg scores} must not contain missing values for the Brier score",
        "and the log loss (modname: {modname}, dsid: {dsid})."
      ),
      arg = "scores", .envir = environment()
    )
  }

  if (min(scores) < 0 || max(scores) > 1) {
    .stop_invalid_arg(
      paste(
        "{.arg scores} must be probabilities between {.val {0}} and",
        "{.val {1}} (modname: {modname}, dsid: {dsid})."
      ),
      arg = "scores", .envir = environment()
    )
  }

  invisible(TRUE)
}

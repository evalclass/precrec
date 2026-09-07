#' Calculate the Brier score, the RMSE and the log loss of predicted
#' probabilities
#'
#' The `prob_metrics` function calculates three probability-based
#'   evaluation metrics - the Brier score, its square root the root mean
#'   squared error, and the log loss - for prediction scores that are
#'   probabilities. Unlike ROC and Precision-Recall curves, these metrics
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
#' @param metrics A character vector of additional metrics to calculate.
#'   The three metrics above are always returned; `"d2_brier"` and
#'   `"d2_logloss"` are returned as well when they are named here, and
#'   `"all"` asks for every metric the function knows.
#'
#'   A D2 score rescales a loss against the loss of the null model, the
#'   one that predicts the observed prevalence for every case and ignores
#'   the scores:
#'
#'   `D2 = 1 - loss(model) / loss(null)`
#'
#'   It is `1` for a perfect model and `0` for one that does no
#'   better than the prevalence, and it is negative for a model that does
#'   worse - which is a real result rather than an error, so it is not
#'   clipped. A dataset holding a single class has a null loss of `0`
#'   and so a D2 score of `NA`.
#'
#' @param ... These additional arguments are passed to [mmdata()]
#'   for data preparation.
#'
#' @return The `prob_metrics` function returns a data frame with the
#'   columns `modnames`, `dsids`, `metrics`, and
#'   `values`. `metrics` is one of "brier", "rmse" or
#'   "logloss", so each model and dataset combination takes up three
#'   rows - plus one row for each metric named in `metrics`.
#'
#' @seealso [prob_metrics_ci()] for the CIs of these metrics over multiple
#'   datasets. [evalmod()] for generating `S3` objects with
#'   performance evaluation metrics. [mmdata()] for formatting input data.
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
#' ## The D2 scores, which say how much of each loss the model explains
#' subset(
#'   prob_metrics(mdat, metrics = c("d2_brier", "d2_logloss")),
#'   dsids == 1
#' )
#'
#' @export
prob_metrics <- function(mdat, scores = NULL, labels = NULL, eps = 1e-15,
                         metrics = NULL, ...) {
  # === Validate input arguments ===
  mdat <- .create_src_obj(mdat, "mdat", mmdata, scores, labels, ...)
  .validate(mdat)
  .validate_eps(eps)
  mnames <- .prob_metric_names(metrics)

  # === Calculate the metrics of each model and dataset ===
  modnames <- attr(mdat, "data_info")[["modnames"]]
  dsids <- attr(mdat, "data_info")[["dsids"]]

  pfunc <- function(i) {
    sc <- mdat[[i]][["scores"]]
    .check_prob_scores(sc, modnames[i], dsids[i])

    # Labels are stored as 1 for negatives and 2 for positives
    outcomes <- as.integer(mdat[[i]][["labels"]]) - 1L
    pm <- calc_prob_metrics(sc, outcomes, eps)
    .check_cpp_func_error(pm, "calc_prob_metrics")

    vals <- c(
      brier = pm[["brier"]],
      # The RMSE of a probability against a 0/1 outcome is the square root of
      # the Brier score, so it is read off the value already calculated
      # rather than summed a second time
      rmse = sqrt(pm[["brier"]]),
      logloss = pm[["logloss"]],
      d2_brier = .calc_d2(pm[["brier"]], outcomes, "brier"),
      d2_logloss = .calc_d2(pm[["logloss"]], outcomes, "logloss")
    )

    data.table::data.table(
      modnames = rep(modnames[i], length(mnames)),
      dsids = rep(dsids[i], length(mnames)),
      metrics = mnames,
      values = unname(vals[mnames])
    )
  }

  # Freshly built, so setDF needs no copy
  .as_plain_df(.rbind_parts(.map_idx(mdat, pfunc)))
}

#
# The metrics `prob_metrics()` knows about
#
# The three it has always returned are the default set, and the two D2
# scores are opt-in. This is the same choice `evalmod()` makes for the
# metrics added after the fact: a caller that reshapes the result should
# keep getting three rows per dataset until it asks for more.
#
.prob_metric_names <- function(metrics) {
  default <- c("brier", "rmse", "logloss")
  all_names <- c(default, "d2_brier", "d2_logloss")

  if (is.null(metrics)) {
    return(default)
  }
  if (identical(metrics, "all")) {
    return(all_names)
  }

  .assert_vector(metrics, "metrics", "character")
  for (m in metrics) {
    .assert_choice(m, "metrics", all_names)
  }

  all_names[all_names %in% union(default, metrics)]
}

#
# D2: the fraction of a loss that the model explains
#
# The same shape as R-squared, with a loss in place of the sum of squares:
# one minus the ratio of the model's loss to the loss of the null model.
#
# The null model predicts the observed prevalence for every case and ignores
# the scores entirely. Its Brier score is then p(1 - p) and its log loss is
# the entropy of the labels, both written out here rather than passed back
# through the C++ function, which would want a vector of a constant.
#
# 1 is a perfect model and 0 is one that does no better than the prevalence.
# Negative values are left as they are: a model can be worse than the null,
# and saying so is what the metric is for.
#
# A dataset with only one class has a null loss of 0 - there is nothing to
# explain - so the ratio is undefined and the answer is NA.
#
.calc_d2 <- function(loss, outcomes, type) {
  p <- mean(outcomes)
  if (p <= 0 || p >= 1) {
    return(NA_real_)
  }

  null_loss <- if (type == "brier") {
    p * (1 - p)
  } else {
    -(p * log(p) + (1 - p) * log(1 - p))
  }

  1 - loss / null_loss
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
                            alpha = 0.05, dtype = "normal", metrics = NULL,
                            ...) {
  # === Validate input arguments ===
  .assert_number(alpha, "alpha", min = 0, max = 1)
  dtype <- .pmatch_dtype(dtype)
  pmetrics <- prob_metrics(mdat, scores, labels,
    eps = eps, metrics = metrics, ...
  )

  if (length(unique(pmetrics[["dsids"]])) < 2) {
    .stop_invalid_arg(
      "{.arg mdat} must contain multiple datasets.",
      arg = "mdat"
    )
  }

  # === Calculate a CI per model and metric ===
  # The Brier score and its square root cannot leave [0, 1]; the log loss is
  # unbounded above, and a D2 score is unbounded below
  bounds <- list(
    brier = c(0, 1), rmse = c(0, 1), logloss = c(0, Inf),
    d2_brier = c(-Inf, 1), d2_logloss = c(-Inf, 1)
  )

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

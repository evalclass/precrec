#
# DeLong's analytic variance of the ROC AUC
#
#' DeLong's standard error for the ROC AUC
#'
#' `auc_delong` calculates the ROC AUC of every model on one test set
#' together with the variance and covariance of those AUCs, exactly rather
#' than by resampling. [auc_ci()] turns the result into confidence
#' intervals and [auc_diff()] into comparisons between models.
#'
#' The ROC AUC is a Mann-Whitney U statistic, so its variance follows from
#' the structural components of that statistic and needs no bootstrap. That
#' makes this the exact counterpart of [auc_boot()]: same input, same two
#' functions reading the result, no `boot_n`, no seed, nothing that changes
#' between two runs, and no floor under the p-value.
#'
#' @param mdat An `mdata` object created by [mmdata()] holding exactly one
#'   dataset. It can be omitted when `scores` and `labels` are given.
#'
#' @param scores A numeric vector, matrix, array, data frame, or list of
#'   scores. See [mmdata()] for the accepted shapes.
#'
#' @param labels A numeric, character, logical, or factor vector of observed
#'   labels, or a list of such vectors.
#'
#' @param ... Further arguments passed to [mmdata()] when `scores` and
#'   `labels` are given instead of `mdat`.
#'
#' @return An object of class `aucdelong`: a data frame of one row per
#'   model, with the columns `modnames`, `curvetypes`, `aucs` and `error`,
#'   the last being the standard error of the AUC. The covariance matrix of
#'   the AUCs is attached as the `cov` attribute, and is what makes the
#'   comparison in [auc_diff()] a paired one.
#'
#' @section The ROC AUC only:
#'
#' There is no precision-recall counterpart of this. The ROC AUC is a
#' U statistic - the probability that a random positive outranks a random
#' negative - and everything below rests on that. The precision-recall AUC
#' is not one, and the interpolated area [auc()] reports is further from
#' being one still, so it has no analytic variance to report and does not
#' appear in the result.
#'
#' [auc_boot()] remains the answer for the precision-recall AUC, and the
#' one to reach for whenever the precision-recall curve is the point. This
#' function is here because the ROC AUC is what a great many readers and
#' reviewers ask about by name, and because it is exact where the bootstrap
#' is approximate.
#'
#' @section How the variance is calculated:
#'
#' Write `m` for the positives and `n` for the negatives. Each positive `i`
#' and each negative `j` contributes a structural component - the share of
#' the other class it beats:
#'
#' ```
#' V10[i] = (1 / n) * sum over j of psi(x[i], y[j])
#' V01[j] = (1 / m) * sum over i of psi(x[i], y[j])
#' ```
#'
#' where `psi` is 1 when the positive outranks the negative, 0.5 when they
#' are tied and 0 otherwise - the same half credit for a tie that the AUC
#' itself gives. The variance is then
#'
#' ```
#' var(auc) = var(V10) / m + var(V01) / n
#' ```
#'
#' and for two models on the same test set the covariance is the same
#' expression with the covariance of their components. Both are computed
#' from midranks rather than from all `m * n` comparisons, so the cost is a
#' sort rather than a product.
#'
#' The components are taken from the ranks `precrec` already assigns, so
#' ties and `NA` scores are handled exactly as they are everywhere else in
#' the package and the AUC reported here is the one [auc()] reports.
#'
#' @section What it assumes:
#'
#' The variance is an **asymptotic** one. It describes how the AUC would
#' move over repeated samples of the same size, and the interval built from
#' it is a normal one, so both get better as the test set grows. On a small
#' or badly imbalanced test set the percentile interval of [auc_boot()] is
#' the safer reading, and comparing the two is a cheap way to find out
#' whether the sample is large enough for this one.
#'
#' @seealso [auc_boot()] for the resampling counterpart, which also covers
#'   the precision-recall AUC, [auc_ci()] for the intervals and
#'   [auc_diff()] for comparing models.
#'
#' @examples
#'
#' ## One test set, two models
#' samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'   modnames = samps[["modnames"]]
#' )
#'
#' delong <- auc_delong(mdat)
#' delong
#'
#' auc_ci(delong)
#' auc_diff(delong)
#'
#' @export
auc_delong <- function(mdat, scores = NULL, labels = NULL, ...) {
  # === Validate input arguments ===
  mdat <- .create_src_obj(mdat, "mdat", mmdata, scores, labels, ...)
  .validate(mdat)
  .validate_boot_mdat(mdat)

  # === Structural components, one column per model ===
  # Labels are stored as 1 for negatives and 2 for positives, and
  # `.validate_boot_mdat()` has established that every model shares them
  outcomes <- as.integer(mdat[[1]][["labels"]])
  pos <- outcomes == 2L
  neg <- outcomes == 1L
  np <- sum(pos)
  nn <- sum(neg)

  modnames <- .as_plain_df(attr(mdat, "data_info"), copy = TRUE)[["modnames"]]
  v10 <- matrix(NA_real_, nrow = np, ncol = length(mdat))
  v01 <- matrix(NA_real_, nrow = nn, ncol = length(mdat))
  aucs <- rep(NA_real_, length(mdat))

  for (m in seq_along(mdat)) {
    comp <- .delong_components(.delong_scores(mdat[[m]]), pos, neg)
    v10[, m] <- comp[["v10"]]
    v01[, m] <- comp[["v01"]]
    aucs[m] <- comp[["auc"]]
  }

  # === Assemble ===
  covm <- stats::cov(v10) / np + stats::cov(v01) / nn
  dimnames(covm) <- list(modnames, modnames)

  res <- data.frame(
    modnames = modnames,
    curvetypes = "ROC",
    aucs = aucs,
    error = sqrt(pmax(0, diag(covm))),
    stringsAsFactors = FALSE
  )

  structure(res,
    cov = covm, np = np, nn = nn,
    class = c("aucdelong", class(res))
  )
}


#
# The values DeLong compares, taken from the ranks rather than the scores
#
# `reformat_data()` has already turned the scores into ranks under the
# caller's `na_worst` and `ties_method`, and rank 1 is the highest score.
# Negating them puts them back in score order while keeping precisely the
# ordering, the ties and the placement of `NA`s that the rest of the
# package computes its AUC from - so the AUC reported here is `auc()`'s,
# not a second opinion about the same data.
#
.delong_scores <- function(fmdat) {
  ranks <- fmdat[["ranks"]]
  if (is.null(ranks)) {
    .stop_invalid_arg(
      paste(
        "{.arg mdat} must be built by {.fn mmdata} in its default mode.",
        "{.code mode = \"aucroc\"} keeps no ranks, and the components",
        "DeLong's variance is built from are read off them."
      ),
      arg = "mdat"
    )
  }

  -as.numeric(ranks)
}


#
# The structural components of the U statistic, from midranks
#
# The definition is a comparison of every positive with every negative,
# which is `m * n` work. Reading it off midranks instead costs a sort:
# a positive's midrank among all the values, less its midrank among the
# positives alone, counts the negatives it beats, with a tie splitting
# between the two the same way `psi` does.
#
.delong_components <- function(values, pos, neg) {
  x <- values[pos]
  y <- values[neg]
  m <- length(x)
  n <- length(y)

  tx <- rank(x)
  ty <- rank(y)
  tz <- rank(c(x, y))

  v10 <- (tz[seq_len(m)] - tx) / n
  v01 <- 1 - (tz[m + seq_len(n)] - ty) / m

  list(auc = mean(v10), v10 = v10, v01 = v01)
}


#
# The standard error of a difference between two of the AUCs
#
# Paired through the covariance: two models scored on the same test set
# rise and fall together, and ignoring that would overstate how uncertain
# their difference is.
#
.delong_diff_se <- function(covm, i, j) {
  variance <- covm[i, i] + covm[j, j] - 2 * covm[i, j]

  # Rounding can carry an exactly-zero variance a little below zero
  sqrt(max(0, variance))
}


#
# Compare two ROC AUCs through DeLong's covariance
#
#' @rdname auc_diff
#' @export
auc_diff.aucdelong <- function(x, alpha = 0.05, alternative = "two.sided") {
  # === Validate input arguments ===
  .assert_number(alpha, "alpha", min = 0, max = 1)
  .assert_string(
    alternative, "alternative", c("two.sided", "greater", "less")
  )

  modnames <- x[["modnames"]]
  if (length(modnames) < 2L) {
    .stop_invalid_arg(
      paste(
        "{.arg x} must hold at least two models to compare,",
        "not {length(modnames)}."
      ),
      arg = "x", .envir = environment()
    )
  }

  covm <- attr(x, "cov")
  n <- attr(x, "np") + attr(x, "nn")
  q <- stats::qnorm(1 - alpha / 2)

  parts <- list()
  for (i in seq_len(length(modnames) - 1L)) {
    for (j in seq(i + 1L, length(modnames))) {
      diffs <- x[["aucs"]][i] - x[["aucs"]][j]
      se <- .delong_diff_se(covm, i, j)

      # Two models that rank every instance the same way leave nothing to
      # divide by. Undefined is the honest answer, as it is for the Wald
      # statistic of `auc_diff.aucboot()`.
      if (se == 0) {
        z_value <- NA_real_
        bounds <- c(diffs, diffs)
      } else {
        z_value <- diffs / se
        bounds <- c(max(-1, diffs - q * se), min(1, diffs + q * se))
      }

      parts[[length(parts) + 1L]] <- data.table::data.table(
        curvetypes = "ROC",
        modnames1 = modnames[i],
        modnames2 = modnames[j],
        diffs = diffs,
        lower_bound = bounds[1],
        upper_bound = bounds[2],
        z_values = z_value,
        p_values = .wald_p_value(z_value, alternative),
        n = n
      )
    }
  }

  .as_plain_df(.rbind_parts(parts))
}

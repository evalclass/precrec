#
# Bootstrap the AUC of a single test set
#
#' Bootstrap AUCs from one test set
#'
#' `auc_boot` resamples a single test set and recalculates the areas under
#' the ROC and precision-recall curves on each resample. The result carries
#' the whole bootstrap distribution, which [auc_ci()] turns into confidence
#' intervals and [auc_diff()] into comparisons between models.
#'
#' The rest of the package builds its intervals from the variation between
#' several test sets, so [auc_ci()] on an `evalmod` result needs several and
#' says so. This is the answer for the ordinary case of one test set, where
#' the only sample there is has to stand in for the population.
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
#' @param boot_n The number of bootstrap resamples. The default of 1000 is
#'   the usual compromise; the tails of a percentile interval are estimated
#'   from `alpha / 2` of these, so 25 resamples at each end of a 95%
#'   interval, and 2000 or more is worth the wait when the interval itself
#'   is the result being reported.
#'
#' @param seed A seed for the resampling, so that a reported interval can be
#'   reproduced. The global random number state is restored afterwards, so a
#'   seeded call does not disturb the stream the caller is drawing from.
#'   `NULL`, the default, resamples from the current state.
#'
#' @param ... Further arguments passed to [mmdata()] when `scores` and
#'   `labels` are given instead of `mdat`.
#'
#' @return An object of class `aucboot`: a data frame of one row per model,
#'   curve type and resample, with the columns `modnames`, `curvetypes`,
#'   `boot_id` and `aucs`. The AUCs of the original data are attached as the
#'   `observed` attribute.
#'
#' @section Resampling:
#'
#' The resampling is **stratified**: positives are drawn from the positives
#' and negatives from the negatives, so every resample holds exactly the
#' class balance the original does. An unstratified bootstrap of an
#' imbalanced dataset produces resamples with a different balance, and
#' sometimes with no positives at all, which moves the precision-recall
#' baseline underneath the quantity being estimated.
#'
#' Every model is resampled on the **same** draws. Comparisons between
#' models are then paired, which is what makes [auc_diff()] a statement
#' about the difference rather than about two independent intervals.
#'
#' @seealso [auc_ci()] for the intervals, [auc_diff()] for comparing two
#'   models, and [auc()] for the point estimates.
#'
#' @examples
#'
#' ## One test set, two models
#' samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'   modnames = samps[["modnames"]]
#' )
#'
#' booted <- auc_boot(mdat, boot_n = 200, seed = 42)
#' auc_ci(booted)
#' auc_diff(booted)
#'
#' @export
auc_boot <- function(mdat, scores = NULL, labels = NULL, boot_n = 1000,
                     seed = NULL, ...) {
  # === Validate input arguments ===
  mdat <- .create_src_obj(mdat, "mdat", mmdata, scores, labels, ...)
  .validate(mdat)
  .assert_number(boot_n, "boot_n", min = 2, whole = TRUE)
  if (!is.null(seed)) {
    .assert_number(seed, "seed", whole = TRUE)
  }
  .validate_boot_mdat(mdat)

  # === Resample ===
  # Labels are stored as 1 for negatives and 2 for positives, and
  # `.validate_boot_mdat()` has established that every model shares them
  outcomes <- as.integer(mdat[[1]][["labels"]])
  pos <- which(outcomes == 2L)
  neg <- which(outcomes == 1L)

  idx <- .with_seed(seed, function() {
    lapply(seq_len(boot_n), function(b) {
      c(
        pos[sample.int(length(pos), replace = TRUE)],
        neg[sample.int(length(neg), replace = TRUE)]
      )
    })
  })

  reps <- .boot_aucs(mdat, idx)

  observed <- .as_plain_df(
    attr(evalmod(mdat, calc_avg = FALSE, cb_alpha = NULL), "aucs"),
    copy = TRUE
  )
  observed <- observed[c("modnames", "curvetypes", "aucs")]

  structure(reps,
    observed = observed, boot_n = as.integer(boot_n),
    np = length(pos), nn = length(neg), class = c("aucboot", class(reps))
  )
}


#
# Recalculate the AUCs of every model on every resample
#
# The resamples are handed to `evalmod()` as datasets rather than one call
# per resample: the pipeline is vectorized over datasets and the work is in
# C++, so a thousand resamples cost one call rather than a thousand. They go
# in chunks because the intermediate holds `boot_n * n` scores per model,
# which is what would otherwise decide how large a dataset can be.
#
.boot_aucs <- function(mdat, idx) {
  info <- .as_plain_df(attr(mdat, "data_info"), copy = TRUE)
  modnames <- info[["modnames"]]
  n <- length(mdat[[1]][["scores"]])

  per_chunk <- max(1L, as.integer(2e6 %/% max(n, 1L)))
  chunks <- split(seq_along(idx), ceiling(seq_along(idx) / per_chunk))

  parts <- lapply(chunks, function(rows) {
    scores <- list()
    labels <- list()
    mods <- character()
    dsids <- integer()

    for (m in seq_along(mdat)) {
      sc <- mdat[[m]][["scores"]]
      lb <- as.integer(mdat[[m]][["labels"]])
      for (b in rows) {
        i <- idx[[b]]
        scores[[length(scores) + 1L]] <- sc[i]
        labels[[length(labels) + 1L]] <- lb[i]
        mods <- c(mods, modnames[m])
        dsids <- c(dsids, b)
      }
    }

    booted <- evalmod(
      mmdata(scores, labels, modnames = mods, dsids = dsids),
      calc_avg = FALSE, cb_alpha = NULL
    )
    .as_plain_df(attr(booted, "aucs"), copy = TRUE)
  })

  reps <- .as_plain_df(.rbind_parts(parts))
  names(reps)[names(reps) == "dsids"] <- "boot_id"
  reps[c("modnames", "curvetypes", "boot_id", "aucs")]
}


#
# Run an expression under a seed without disturbing the caller's stream
#
.with_seed <- function(seed, func) {
  if (is.null(seed)) {
    return(func())
  }

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }

  set.seed(seed)
  func()
}


#
# Compare the AUCs of two models on the same resamples
#
#' Compare bootstrapped AUCs between models
#'
#' `auc_diff` takes the resamples of [auc_boot()] and reports, for every
#' pair of models and every curve type, the difference between their AUCs
#' with a confidence interval around it.
#'
#' The comparison is paired: both models are scored on the same resample,
#' so the difference is calculated within a resample and the spread of those
#' differences is what the interval describes. Two separate intervals from
#' [auc_ci()] cannot be read this way - they can overlap while the
#' difference is still clearly on one side of zero, because they say nothing
#' about how the two models move together.
#'
#' @param x An `aucboot` object from [auc_boot()].
#'
#' @param alpha The interval covers `1 - alpha` of the resampled
#'   differences, so the default of 0.05 gives a 95% interval.
#'
#' @param alternative The side of zero the alternative hypothesis is on,
#'   one of `"two.sided"`, `"greater"` and `"less"`. `"greater"` tests
#'   whether the first model of the pair has the larger AUC. It selects the
#'   tail of both p-values; the interval stays two-sided whatever it is set
#'   to.
#'
#' @return A data frame with one row per curve type and pair of models:
#'
#'   \tabular{ll}{
#'     `curvetypes` \tab ROC or PRC \cr
#'     `modnames1`, `modnames2` \tab The pair, in the order compared \cr
#'     `diffs` \tab Observed AUC of the first minus that of the second \cr
#'     `lower_bound`, `upper_bound` \tab Percentile interval of the
#'       resampled differences \cr
#'     `p_values` \tab Percentile p-value, see below \cr
#'     `statistic` \tab `diffs` over the standard deviation of the
#'       resampled differences \cr
#'     `p_values_norm` \tab Normal-approximation p-value, see below \cr
#'     `n` \tab Resamples the row is based on \cr
#'   }
#'
#' @section The interval and the two p-values:
#'
#' The interval is the primary result, and is the `alpha / 2` and
#' `1 - alpha / 2` quantiles of the resampled differences.
#'
#' `p_values` is the proportion of resampled differences falling on the
#' other side of zero from the observed one, doubled for a two-sided test,
#' and calculated as `2 * min(1 + sum(d <= 0), 1 + sum(d >= 0)) / (n + 1)`
#' so that it is never exactly zero - a bootstrap of `n` resamples cannot
#' report a p-value below about `2 / n`, and reporting one would be an
#' artifact of the resample count rather than evidence.
#'
#' `p_values_norm` buys that resolution back with an assumption. It reads
#' `statistic` off the normal distribution, and `statistic` divides the
#' observed difference by the standard deviation of the resampled ones, so
#' the bootstrap spread stands in for a standard error. Nothing is shuffled
#' between the models, so the null is never enforced; the distribution is
#' the sampling one, centered on the observed difference and reflected to
#' sit under zero. It can report a p-value far below `2 / n`, which is what
#' makes it worth having, and it is at its weakest where the resampled
#' differences are skewed rather than normal - few positives, or either
#' model near the ceiling of the precision-recall AUC. The small p-values it
#' makes available are therefore its least trustworthy numbers.
#'
#' Read the interval first. The two p-values are companions to it rather
#' than exact tests, and when they disagree sharply it is the normal
#' approximation to distrust.
#'
#' `statistic` is `NA` when the resamples have no spread to divide by, which
#' happens when two models are given the same scores, and `p_values_norm`
#' is `NA` with it.
#'
#' @seealso [auc_boot()] for the resampling and [auc_ci()] for one model at
#'   a time.
#'
#' @examples
#'
#' samps <- create_sim_samples(1, 100, 100, c("poor_er", "good_er"))
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'   modnames = samps[["modnames"]]
#' )
#'
#' booted <- auc_boot(mdat, boot_n = 200, seed = 42)
#' auc_diff(booted)
#'
#' ## Is the second model the better one?
#' auc_diff(booted, alternative = "less")
#'
#' @export
auc_diff <- function(x, alpha = 0.05, alternative = "two.sided") {
  # === Validate input arguments ===
  if (!inherits(x, "aucboot")) {
    .stop_invalid_arg(
      "{.arg x} must be an {.cls aucboot} object from {.fn auc_boot}.",
      arg = "x"
    )
  }
  .assert_number(alpha, "alpha", min = 0, max = 1)
  .assert_string(
    alternative, "alternative", c("two.sided", "greater", "less")
  )

  observed <- attr(x, "observed")
  modnames <- unique(x[["modnames"]])
  if (length(modnames) < 2L) {
    .stop_invalid_arg(
      paste(
        "{.arg x} must hold at least two models to compare,",
        "not {length(modnames)}."
      ),
      arg = "x", .envir = environment()
    )
  }

  parts <- list()
  for (curvetype in unique(x[["curvetypes"]])) {
    rows <- x[x[["curvetypes"]] == curvetype, , drop = FALSE]
    obs <- observed[observed[["curvetypes"]] == curvetype, , drop = FALSE]

    for (i in seq_len(length(modnames) - 1L)) {
      for (j in seq(i + 1L, length(modnames))) {
        parts[[length(parts) + 1L]] <- .boot_diff_row(
          rows, obs, modnames[i], modnames[j], curvetype, alpha,
          alternative
        )
      }
    }
  }

  .as_plain_df(.rbind_parts(parts))
}


#
# One pair of models, one curve type
#
.boot_diff_row <- function(rows, obs, mod1, mod2, curvetype, alpha,
                           alternative) {
  # Aligned on the resample, so the subtraction is within a resample
  first <- rows[rows[["modnames"]] == mod1, , drop = FALSE]
  second <- rows[rows[["modnames"]] == mod2, , drop = FALSE]
  first <- first[order(first[["boot_id"]]), ]
  second <- second[order(second[["boot_id"]]), ]

  .assert_internal(identical(first[["boot_id"]], second[["boot_id"]]))
  diffs <- first[["aucs"]] - second[["aucs"]]
  diffs <- diffs[!is.na(diffs)]
  n <- length(diffs)
  observed <- .boot_observed(obs, mod1) - .boot_observed(obs, mod2)

  if (n == 0L) {
    bounds <- c(NA_real_, NA_real_)
    p_value <- NA_real_
  } else {
    bounds <- unname(quantile(
      diffs, c(alpha / 2, 1 - alpha / 2),
      names = FALSE
    ))
    p_value <- .boot_p_percentile(diffs, alternative)
  }
  statistic <- .boot_statistic(observed, diffs)

  data.table::data.table(
    curvetypes = curvetype,
    modnames1 = mod1,
    modnames2 = mod2,
    diffs = observed,
    lower_bound = bounds[1],
    upper_bound = bounds[2],
    p_values = p_value,
    statistic = statistic,
    p_values_norm = .boot_p_normal(statistic, alternative),
    n = n
  )
}


#
# The share of resampled differences on the far side of zero
#
# One is added to each count so that the value cannot be exactly zero: `n`
# resamples support nothing below 1 / (n + 1) on one side, and a smaller
# number would describe `boot_n` rather than the models.
#
.boot_p_percentile <- function(diffs, alternative) {
  n <- length(diffs)
  greater <- (1 + sum(diffs <= 0)) / (n + 1)
  less <- (1 + sum(diffs >= 0)) / (n + 1)

  switch(alternative,
    greater = greater,
    less = less,
    two.sided = min(1, 2 * min(greater, less))
  )
}


#
# The observed difference in units of the bootstrap spread
#
# The standard deviation of the resampled differences stands in for the
# standard error. Two models given the same scores produce a run of exact
# zeros, and there is then no scale to divide by - undefined rather than
# infinite is the honest answer, and it carries through to the p-value.
#
.boot_statistic <- function(observed, diffs) {
  if (length(diffs) < 2L) {
    return(NA_real_)
  }

  spread <- stats::sd(diffs)
  if (!is.finite(spread) || spread == 0) {
    return(NA_real_)
  }

  observed / spread
}


#
# The statistic read off the normal distribution
#
.boot_p_normal <- function(statistic, alternative) {
  if (is.na(statistic)) {
    return(NA_real_)
  }

  switch(alternative,
    greater = stats::pnorm(-statistic),
    less = stats::pnorm(statistic),
    two.sided = 2 * stats::pnorm(-abs(statistic))
  )
}


.boot_observed <- function(obs, modname) {
  obs[["aucs"]][obs[["modnames"]] == modname]
}

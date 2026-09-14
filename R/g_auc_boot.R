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
#'   `boot_id`, `aucs` and `baselines`. The AUCs of the original data are
#'   attached as the `observed` attribute, with a `baselines` column of
#'   their own.
#'
#'   `baselines` is the same on every resample, because the resampling is
#'   stratified and so holds the class balance fixed - see below. It is
#'   carried on the resamples anyway so that an area and what it is worth
#'   by chance never have to be joined back together by hand.
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
#'   models, [auc_delong()] for an exact standard error of the ROC AUC,
#'   and [auc()] for the point estimates.
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

  # The resample is stratified, so np and nn are the same on every one of
  # them as on the test set, and so is the chance level of each curve type
  reps[["baselines"]] <- .baselines_at(
    reps[["curvetypes"]], length(pos), length(neg)
  )

  observed <- .as_plain_df(
    attr(evalmod(mdat, calc_avg = FALSE, cb_alpha = NULL), "aucs"),
    copy = TRUE
  )
  observed <- observed[c("modnames", "curvetypes", "aucs")]
  observed[["baselines"]] <- .baselines_at(
    observed[["curvetypes"]], length(pos), length(neg)
  )

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
#' Compare AUCs between models
#'
#' `auc_diff` reports, for every pair of models, the difference between
#' their AUCs with a confidence interval around it. It reads either of the
#' two estimates of uncertainty the package offers for a single test set:
#' the resamples of [auc_boot()], which cover both curve types, or the
#' analytic covariance of [auc_delong()], which covers the ROC AUC exactly.
#'
#' The comparison is paired either way. With a bootstrap both models are
#' scored on the same resample, so the difference is calculated within a
#' resample and the spread of those differences is what the interval
#' describes; with DeLong the pairing is carried by the covariance between
#' the two AUCs. Two separate intervals from [auc_ci()] cannot be read this
#' way - they can overlap while the difference is still clearly on one side
#' of zero, because they say nothing about how the two models move
#' together.
#'
#' @param x An `aucboot` object from [auc_boot()] or an `aucdelong` object
#'   from [auc_delong()].
#'
#' @param alpha The interval covers `1 - alpha` of the resampled
#'   differences, so the default of 0.05 gives a 95% interval.
#'
#' @param alternative The side of zero the alternative hypothesis is on,
#'   one of `"two.sided"`, `"greater"` and `"less"`. `"greater"` tests
#'   whether the first model of the pair has the larger AUC. It selects the
#'   tail of every p-value in the result and leaves `z_values` alone; the
#'   interval stays two-sided whatever it is set to.
#'
#' @return A data frame with one row per curve type and pair of models.
#'
#'   From an `aucboot` object:
#'
#'   \tabular{ll}{
#'     `curvetypes` \tab ROC or PRC \cr
#'     `modnames1`, `modnames2` \tab The pair, in the order compared \cr
#'     `diffs` \tab Observed AUC of the first minus that of the second \cr
#'     `lower_bound`, `upper_bound` \tab Percentile interval of the
#'       resampled differences \cr
#'     `p_values` \tab Percentile p-value, see below \cr
#'     `z_values` \tab Wald statistic, `diffs / sd(d)`, see below \cr
#'     `p_values_wald` \tab Wald p-value, see below \cr
#'     `n` \tab Resamples the row is based on \cr
#'   }
#'
#'   From an `aucdelong` object, where there is one p-value rather than
#'   two and only the ROC AUC to report:
#'
#'   \tabular{ll}{
#'     `curvetypes` \tab ROC \cr
#'     `modnames1`, `modnames2` \tab The pair, in the order compared \cr
#'     `diffs` \tab AUC of the first minus that of the second \cr
#'     `lower_bound`, `upper_bound` \tab Normal interval of the
#'       difference, clipped to `[-1, 1]` \cr
#'     `z_values` \tab `diffs` over its standard error \cr
#'     `p_values` \tab DeLong's p-value, see below \cr
#'     `n` \tab Instances in the test set \cr
#'   }
#'
#' @section DeLong's comparison:
#'
#' Given an `aucdelong` object the standard error of the difference comes
#' out of the covariance matrix,
#'
#' ```
#' se = sqrt(var(auc1) + var(auc2) - 2 * cov(auc1, auc2))
#' z_values = diffs / se
#' p_values = 2 * pnorm(-abs(z_values))
#' ```
#'
#' and the covariance is what makes it a paired test: two models scored on
#' the same test set rise and fall together, and dropping the last term
#' would overstate how uncertain their difference is.
#'
#' This is a Wald test too, so `alternative` selects its tail the same way.
#' What it does not need is `boot_n`: the standard error is exact rather
#' than resampled, so the p-value has no floor and does not move between
#' two runs. What it does assume is that the variance is asymptotic - see
#' [auc_delong()] - and it has nothing to say about the precision-recall
#' AUC.
#'
#' `z_values` is `NA` when two models rank every instance the same way and
#' there is no standard error to divide by, and `p_values` is `NA` with it.
#'
#' @section The interval and the two p-values:
#'
#' This section and the two that follow describe an `aucboot` object.
#'
#'
#' The interval is the primary result, and is the `alpha / 2` and
#' `1 - alpha / 2` quantiles of the resampled differences.
#'
#' The two p-values are the two ways of getting one out of a bootstrap, and
#' each column is named for the method that produced it. Write `d` for the
#' vector of `n` resampled differences and `diffs` for the observed one.
#'
#' `p_values` is the **percentile** p-value - the share of `d` on the other
#' side of zero from `diffs`, doubled for a two-sided test:
#'
#' ```
#' p_values = 2 * min(1 + sum(d <= 0), 1 + sum(d >= 0)) / (n + 1)
#' ```
#'
#' The added ones keep it away from exactly zero. It cannot go below
#' `2 / (n + 1)`, so `boot_n` sets a floor under it.
#'
#' `z_values` and `p_values_wald` are the **Wald** test - an estimate over
#' an estimate of its standard error, referred to a standard normal:
#'
#' ```
#' z_values      = diffs / sd(d)
#' p_values_wald = 2 * pnorm(-abs(z_values))
#' ```
#'
#' `sd(d)` is the standard error, because the spread of the bootstrap
#' distribution is what the bootstrap has to say about how far the
#' difference moves from sample to sample. Having a scale underneath it
#' rather than a count of resamples, the Wald p-value has no floor.
#'
#' `alternative` replaces the doubled minimum with the matching one-sided
#' count, and `-abs()` with `-` or `+`, in the two formulas above.
#'
#' @section Why two p-values:
#'
#' Because they fail in opposite ways, and neither one on its own tells you
#' that it is failing.
#'
#' The percentile p-value assumes nothing about the shape of `d`, and pays
#' for that with the floor. Once it reaches `2 / (n + 1)` it has stopped
#' measuring the models and started reporting `boot_n`: a difference that is
#' merely clear and one that is overwhelming both come out at 0.002 at the
#' default thousand resamples, and the number gives no sign of which it is
#' looking at.
#'
#' The Wald p-value has no floor, and pays for that with an assumption. It
#' takes `d` to be roughly normal, and locates the null by reflecting the
#' sampling distribution rather than by enforcing it. Where `d` is skewed -
#' few positives, or either model near the ceiling of the precision-recall
#' AUC - it is confidently wrong, and again the number carries no warning.
#'
#' Side by side they cover each other. When they agree, the normality the
#' Wald test assumes is doing no harm at this sample size and you can quote
#' its resolution. When they disagree sharply, `d` is not the shape the Wald
#' test needs, and the percentile p-value - floor and all - is the one to
#' trust. The comparison is the diagnostic; neither column is one on its
#' own.
#'
#' There is a second, exact sense in which they are different answers. A
#' Wald test is the counterpart of the bootstrap **normal** interval,
#' `diffs` plus and minus `z` standard errors, while `lower_bound` and
#' `upper_bound` are the **percentile** interval. So `p_values_wald` is not
#' the dual of the bounds reported beside it, and need not agree with them.
#'
#' Read the interval first. Both p-values are companions to it rather than
#' exact tests.
#'
#' @section Not a t statistic:
#'
#' `z_values` divides by a standard error, not by a standard error of a
#' mean, and is read off the normal rather than off a t. `auc_ci(dtype =
#' "t")` is the genuine t in this package, and the contrast is exact: there
#' the spread is taken over a handful of real test sets and divided by the
#' square root of how many there were, so `n - 1` degrees of freedom mean
#' something. Here the spread is taken over resamples and divided by
#' nothing - `boot_n` is a setting rather than a sample size, and a t on
#' `boot_n - 1` degrees of freedom would be a p-value that shrinks when the
#' caller resamples harder.
#'
#' Nor is this the bootstrap-*t*, which forms a statistic of its own inside
#' every resample and takes its reference distribution from those rather
#' than from the normal.
#'
#' `z_values` is `NA` when the resamples have no spread to divide by, which
#' happens when two models are given the same scores, and `p_values_wald`
#' is `NA` with it.
#'
#' @seealso [auc_boot()] for the resampling, [auc_delong()] for the
#'   analytic alternative, and [auc_ci()] for one model at a time.
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
#' ## The same comparison without resampling, for the ROC AUC
#' auc_diff(auc_delong(mdat))
#'
#' @export
auc_diff <- function(x, alpha = NULL, alternative = NULL) {
  UseMethod("auc_diff", x)
}

#' @export
auc_diff.default <- function(x, alpha = NULL, alternative = NULL) {
  .stop_invalid_arg(
    paste(
      "{.arg x} must be an {.cls aucboot} object from {.fn auc_boot} or",
      "an {.cls aucdelong} object from {.fn auc_delong}."
    ),
    arg = "x"
  )
}

#' @rdname auc_diff
#' @export
auc_diff.aucboot <- function(x, alpha = 0.05, alternative = "two.sided") {
  # === Validate input arguments ===
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
  z_value <- .boot_z_value(observed, diffs)

  data.table::data.table(
    curvetypes = curvetype,
    modnames1 = mod1,
    modnames2 = mod2,
    diffs = observed,
    lower_bound = bounds[1],
    upper_bound = bounds[2],
    p_values = p_value,
    z_values = z_value,
    p_values_wald = .wald_p_value(z_value, alternative),
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
# The Wald statistic, `observed / sd(diffs)`
#
# The standard deviation of the resampled differences is the bootstrap's
# estimate of the standard error, and is not divided by `sqrt(boot_n)` the
# way `.calc_ci_stats()` divides by `sqrt(n)`: there `n` counts real test
# sets, here `boot_n` counts resamples of one. That is also why the value
# is read off the normal rather than off a t - `boot_n` degrees of freedom
# would be a p-value that shrinks when the caller resamples harder.
#
# Two models given the same scores produce a run of exact zeros, and there
# is then no scale to divide by - undefined rather than infinite is the
# honest answer, and it carries through to the p-value.
#
.boot_z_value <- function(observed, diffs) {
  if (length(diffs) < 2L) {
    return(NA_real_)
  }

  spread <- stats::sd(diffs)
  if (!is.finite(spread) || spread == 0) {
    return(NA_real_)
  }

  observed / spread
}



.boot_observed <- function(obs, modname) {
  obs[["aucs"]][obs[["modnames"]] == modname]
}

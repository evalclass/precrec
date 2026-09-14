#
# Calculate basic evaluation metrics from confusion matrices
#
calc_metrics <- function(cmats, scores = NULL, labels = NULL, beta = 1,
                         extra_metrics = TRUE, metrics = NULL,
                         cost_fp = 1, cost_fn = 1, ...) {
  # === Validate input arguments ===
  # Create cmats from scores and labels if cmats is missing
  #
  # `sar` reads the values of the scores, which reach this function only
  # through the formatted data the confusion matrices were told to keep. When
  # this call is the one building them it can simply ask, so `keep_fmdat` is
  # taken over rather than left to the caller - a caller who did not ask for
  # `sar` is unaffected.
  make_cmats <- create_confmats
  if ("sar" %in% metrics) {
    make_cmats <- function(scores, labels, keep_fmdat = TRUE, ...) {
      create_confmats(
        scores = scores, labels = labels, keep_fmdat = TRUE, ...
      )
    }
  }
  cmats <- .create_src_obj(
    cmats, "cmats", make_cmats, scores, labels,
    ...
  )
  .validate(cmats)

  # === Create confusion matrices for all ranks ===
  # Call a cpp function via Rcpp interface
  .validate_beta(beta)
  .assert_flag(extra_metrics, "extra_metrics")
  pevals <- calc_basic_metrics(
    attr(cmats, "np"), attr(cmats, "nn"),
    cmats[["tp"]], cmats[["fp"]],
    cmats[["tn"]], cmats[["fn"]], beta, extra_metrics
  )
  .check_cpp_func_error(pevals, "calc_basic_metrics")

  # === Derive the metrics that are algebra on the ones just calculated ===
  pevals[["basic"]] <- .add_derived_metrics(
    pevals[["basic"]], cmats, metrics,
    cost_fp = cost_fp, cost_fn = cost_fn
  )

  # === Create an S3 object ===
  s3obj <- structure(pevals["basic"], class = "pevals")

  # Set attributes
  attr(s3obj, "modname") <- attr(cmats, "modname")
  if (all(is.na(attr(cmats, "src")))) {
    s3obj[["basic"]][["score"]] <- rep(
      NA,
      length(s3obj[["basic"]][["rank"]])
    )
    s3obj[["basic"]][["label"]] <- rep(
      NA,
      length(s3obj[["basic"]][["rank"]])
    )
  } else {
    ridx <- attr(cmats, "src")[["rank_idx"]]
    tscores <- attr(cmats, "src")[["scores"]][ridx]
    tlabels <- as.numeric(attr(cmats, "src")[["labels"]])[ridx]
    tlabels <- tlabels - 1
    tlabels[tlabels == 0] <- -1
    s3obj[["basic"]][["score"]] <- c(NA, tscores)
    s3obj[["basic"]][["label"]] <- c(NA, tlabels)
  }
  attr(s3obj, "dsid") <- attr(cmats, "dsid")
  attr(s3obj, "nn") <- attr(cmats, "nn")
  attr(s3obj, "np") <- attr(cmats, "np")
  attr(s3obj, "args") <- list(...)
  attr(s3obj, "cpp_errmsg") <- pevals[["errmsg"]]
  attr(s3obj, "src") <- cmats
  attr(s3obj, "validated") <- FALSE

  # Call .validate.cmats()
  .validate(s3obj)
}

#
# Add the metrics that are algebra on the confusion matrices
#
# Every one of these is a vectorized transform of columns the C++ layer has
# already produced, or of the counts behind them, so there is nothing to gain
# by computing them next to the loop: doing it here means an unrequested
# metric costs no time and no memory, and `mode = "rocprc"` - which asks for
# three metrics and never for these - is untouched.
#
# They are derived on the same rank grid as the metrics they come from, and
# before any x_bins reduction, so a derived column lines up with its siblings.
# `.validate.pevals()` checks that.
#
.add_derived_metrics <- function(pb, cmats, metrics, cost_fp = 1,
                                 cost_fn = 1) {
  # Checked before the table is read, so that the common call - the curve
  # pipelines and every `evalmod()` that does not pass `metrics` - does no
  # work at all here
  if (length(metrics) == 0L) {
    return(pb)
  }

  tab <- .basic_metric_table()
  derived <- intersect(metrics, tab$name[!tab$default])
  if (length(derived) == 0L) {
    return(pb)
  }

  tp <- cmats[["tp"]]
  fp <- cmats[["fp"]]
  tn <- cmats[["tn"]]
  fn <- cmats[["fn"]]
  n_all <- cmats[["pos_num"]] + cmats[["neg_num"]]

  # The four complements come off the columns rather than off the counts, so
  # that they inherit what those columns already do at the ends: precision
  # and NPV take their undefined value from a neighbor, and a dataset with
  # no negatives has NA specificity rather than 0/0.
  vals <- list(
    fpr = 1 - pb[["specificity"]],
    fnr = 1 - pb[["sensitivity"]],
    false_discovery_rate = 1 - pb[["precision"]],
    false_omission_rate = 1 - pb[["npv"]],
    predicted_positive_rate = (tp + fp) / n_all,
    predicted_negative_rate = (tn + fn) / n_all
  )
  vals[["lift"]] <- pb[["sensitivity"]] / vals[["predicted_positive_rate"]]
  vals[["odds"]] <- (tp * tn) / (fn * fp)
  vals[["mi"]] <- .calc_mutual_information(tp, fp, tn, fn, n_all)

  # Pearson's chi-square of the 2x2 table is n times the square of the
  # Matthews correlation coefficient - the same four margins under the same
  # numerator - so it is read off the column that already exists, and
  # inherits the NA that column carries wherever a margin is empty.
  vals[["chisq"]] <- n_all * pb[["mcc"]]^2

  # Not normalized, following ROCR. With the default weights this is the
  # error rate, which is the check that the two agree.
  vals[["cost"]] <- (fn * cost_fn + fp * cost_fp) / n_all

  # The distance from the point (1 - specificity, sensitivity) to the perfect
  # corner of ROC space. It is the one metric in the table that is better
  # when it is smaller, and the one whose maximum is sqrt(2) rather than 1,
  # which is why its range is "free".
  vals[["roc_dist"]] <- sqrt(
    (1 - pb[["sensitivity"]])^2 + (1 - pb[["specificity"]])^2
  )

  vals[["sedi"]] <- .calc_sedi(pb[["sensitivity"]], vals[["fpr"]])

  # The Jaccard index is the confusion matrix with its true negative corner
  # deleted: TP / (TP + FP + FN). That is the same corner precision and
  # recall leave out, which is why it tracks them on imbalanced data while
  # accuracy, which is mostly TN there, does not.
  vals[["jaccard"]] <- tp / (tp + fp + fn)

  # The two likelihood ratios, of which precrec already carries the ratio:
  # the odds ratio above is LR+ / LR-. Kept apart because they answer
  # different questions - LR+ how much a positive prediction raises the
  # odds, LR- how much a negative one lowers them - and because a
  # classifier can be worth using on one of them alone.
  vals[["positive_likelihood_ratio"]] <-
    pb[["sensitivity"]] / vals[["fpr"]]
  vals[["negative_likelihood_ratio"]] <-
    vals[["fnr"]] / pb[["specificity"]]

  if ("sar" %in% derived) {
    vals[["sar"]] <- .calc_sar(pb, cmats, tp, fp, tn, fn)
  }

  # `fn * fp` is zero at both ends of every dataset, and the numerator with
  # it, so the odds ratio is undefined there by construction rather than by
  # accident; the lift is 0/0 at the top rank for the same reason. ROCR
  # reports Inf and NaN. NA is what `precision` and `npv` already do with
  # their own undefined end, and it keeps an infinity off a shared axis.
  #
  # The likelihood ratios divide by the false positive rate and by the
  # specificity, each of which is 0 at one end, so they need the same
  # treatment. The Jaccard index is 0/0 only for a dataset with no
  # positives, where every other metric is NA already.
  for (m in c(
    "lift", "odds", "jaccard",
    "positive_likelihood_ratio", "negative_likelihood_ratio"
  )) {
    vals[[m]][!is.finite(vals[[m]])] <- NA_real_
  }

  pb[derived] <- vals[derived]
  pb
}

#
# SAR: the mean of accuracy, AUC(ROC) and 1 - RMSE
#
# Three quantities on three different scales, averaged with equal weight -
# a per-cutoff metric, a curve-level scalar and a score-level scalar. Only
# the first varies along the curve; the other two are constants added to
# every point, which is what ROCR does too.
#
# The RMSE reads the values of the scores rather than their ranks, so the
# scores have to be probabilities. That is the same requirement
# `prob_metrics()` has, and it is checked the same way.
#
.calc_sar <- function(pb, cmats, tp, fp, tn, fn) {
  src <- attr(cmats, "src")
  if (all(is.na(src))) {
    stop(paste(
      "The 'sar' metric needs the prediction scores.",
      "Use create_confmats(keep_fmdat = TRUE)."
    ), call. = FALSE)
  }

  scores <- src[["scores"]]
  # Labels are stored as 1 for negatives and 2 for positives
  outcomes <- as.integer(src[["labels"]]) - 1L

  # A warning and NA rather than the error `prob_metrics()` raises for the
  # same input: this is one opt-in column among twenty-nine, and
  # `metrics = "all"` should not stop on a dataset whose scores happen not
  # to be probabilities. Every other metric it asked for is still returned.
  if (anyNA(scores) || min(scores) < 0 || max(scores) > 1) {
    warning(
      paste0(
        "The 'sar' metric needs scores that are probabilities between 0",
        " and 1, and returns NA otherwise (modname: ",
        attr(cmats, "modname"), ", dsid: ", attr(cmats, "dsid"), ")."
      ),
      call. = FALSE
    )
    return(rep(NA_real_, length(pb[["accuracy"]])))
  }

  rmse <- sqrt(mean((scores - outcomes)^2))

  (pb[["accuracy"]] + .roc_auc_from_counts(tp, fp, cmats) + (1 - rmse)) / 3
}

#
# The area under the ROC curve, by the trapezoid rule over the counts
#
# Reading it off the confusion matrices rather than asking the curve pipeline
# for it is safe here in a way it would not be for a precision-recall curve:
# the ROC space *is* linearly interpolated - that is what makes a straight
# line between two ROC points correct and a straight line between two
# precision-recall points wrong - so the trapezoid over every cutoff is the
# curve, not an approximation of it.
#
.roc_auc_from_counts <- function(tp, fp, cmats) {
  np <- cmats[["pos_num"]]
  nn <- cmats[["neg_num"]]
  if (np == 0 || nn == 0) {
    return(NA_real_)
  }

  x <- fp / nn
  y <- tp / np
  n <- length(x)

  sum(diff(x) * (y[-1] + y[-n]) / 2)
}

#
# Symmetric extremal dependence index
#
# A skill score from forecast verification, built to stay informative for
# rare events, where the hit rate and the false alarm rate both go to zero
# and the usual scores degenerate with them:
#
#   SEDI = (log F - log H - log(1 - F) + log(1 - H))
#          / (log F + log H + log(1 - F) + log(1 - H))
#
# with H the hit rate (sensitivity) and F the false alarm rate (FPR). It is
# 0 for a classifier that does no better than chance, and 1 for a perfect
# one. The denominator cannot vanish: each of the four factors is below 1 in
# the open interval, so the sum of their logs is strictly negative.
#
# All four logs are undefined at 0 and at 1, which is exactly where both ends
# of every dataset sit, so the rates are clamped away from the boundary
# first. The clamp is the same 1e-9 yardstick uses, which is what makes the
# two agree on the points either can evaluate.
#
.calc_sedi <- function(sn, fpr) {
  eps <- 1e-9
  clamp <- function(x) pmin(pmax(x, eps), 1 - eps)
  h <- clamp(sn)
  f <- clamp(fpr)

  num <- log(f) - log(h) - log(1 - f) + log(1 - h)
  den <- log(f) + log(h) + log(1 - f) + log(1 - h)
  num / den
}

#
# Mutual information of the predictions and the labels, in bits
#
# I(Y-hat; Y) = sum p_ij log2(p_ij / (p_i. p_.j)) over the four cells of the
# 2x2 table. An empty cell contributes nothing, by the usual convention that
# 0 log 0 is 0, which is also what makes the value correct at the ends: a
# cutoff that predicts one class for everything carries no information about
# the labels, so the answer there is 0 rather than undefined. ROCR reports
# NaN at those two points.
#
.calc_mutual_information <- function(tp, fp, tn, fn, n_all) {
  cell <- function(obs, row_total, col_total) {
    p <- obs / n_all
    ratio <- (obs * n_all) / (row_total * col_total)
    out <- p * log2(ratio)
    out[obs == 0] <- 0
    out
  }

  pos_pred <- tp + fp
  neg_pred <- tn + fn
  pos <- tp + fn
  neg <- fp + tn

  cell(tp, pos_pred, pos) + cell(fp, pos_pred, neg) +
    cell(fn, neg_pred, pos) + cell(tn, neg_pred, neg)
}

#
# Validate 'pevals' object generated by calc_metrics()
#
.validate.pevals <- function(x) {
  # Need to validate only once
  if (methods::is(x, "pevals") && attr(x, "validated")) {
    return(x)
  }

  # Validate class items and attributes
  item_names <- "basic"
  attr_names <- c(
    "modname", "dsid", "nn", "np", "args", "cpp_errmsg",
    "src", "validated"
  )
  arg_names <- c(
    "na_worst", "na.last", "ties.method", "ties_method",
    "modname", "dsid", "keep_fmdat"
  )
  .validate_basic(
    x, "pevals", "calc_metrics", item_names, attr_names,
    arg_names
  )

  pb <- x[["basic"]]

  # Check values of class items. The curve pipeline asks calc_metrics() for
  # the reduced table, so an object either holds every metric or only the
  # ones the curves are drawn from.
  n <- length(pb[["error"]])
  extra <- c(
    "balanced_accuracy", "npv", "informedness", "markedness", "kappa"
  )
  has_extra <- "kappa" %in% names(pb)
  mnames <- intersect(.get_metric_names("basic_all"), names(pb))
  if (any(.map_int(mnames, function(m) length(pb[[m]])) != n)) {
    stop("Evaluation vectors must be all the same lengths", call. = FALSE)
  }

  # Scores
  .assert_internal(
    is.atomic(pb[["score"]]),
    is.vector(pb[["score"]])
  )

  # Labels
  .assert_internal(
    is.atomic(pb[["label"]]),
    is.vector(pb[["label"]])
  )

  # Error rate
  .assert_internal(
    is.atomic(pb[["error"]]),
    is.vector(pb[["error"]]),
    is.numeric(pb[["error"]])
  )

  # Accuracy
  .assert_internal(
    is.atomic(pb[["accuracy"]]),
    is.vector(pb[["accuracy"]]),
    is.numeric(pb[["accuracy"]])
  )

  # Error rate & Arruracy
  .assert_internal(
    pb[["error"]][1] + pb[["accuracy"]][1] == 1,
    pb[["error"]][n] + pb[["accuracy"]][n] == 1
  )

  # SP. A dataset with no negatives has no specificity to report, and
  # calc_basic_metrics() fills the column with NA rather than 0/0.
  .assert_internal(
    is.atomic(pb[["specificity"]]),
    is.vector(pb[["specificity"]]),
    is.numeric(pb[["specificity"]])
  )
  if (attr(x, "nn") == 0) {
    .assert_internal(all(is.na(pb[["specificity"]])))
  } else {
    .assert_internal(
      pb[["specificity"]][1] == 1,
      pb[["specificity"]][n] == 0
    )
  }

  # SN, likewise for a dataset with no positives
  .assert_internal(
    is.atomic(pb[["sensitivity"]]),
    is.vector(pb[["sensitivity"]]),
    is.numeric(pb[["sensitivity"]])
  )
  if (attr(x, "np") == 0) {
    .assert_internal(all(is.na(pb[["sensitivity"]])))
  } else {
    .assert_internal(
      pb[["sensitivity"]][1] == 0,
      pb[["sensitivity"]][n] == 1
    )
  }

  # PREC
  .assert_internal(
    is.atomic(pb[["precision"]]),
    is.vector(pb[["precision"]]),
    is.numeric(pb[["precision"]]),
    pb[["precision"]][1] == pb[["precision"]][2]
  )

  # Matthews correlation coefficient
  .assert_internal(
    is.atomic(pb[["mcc"]]),
    is.vector(pb[["mcc"]]),
    is.numeric(pb[["mcc"]])
  )

  # F-score
  .assert_internal(
    is.atomic(pb[["fscore"]]),
    is.vector(pb[["fscore"]]),
    is.numeric(pb[["fscore"]])
  )

  # Balanced accuracy, NPV, informedness, markedness and Cohen's kappa
  if (has_extra) {
    for (m in extra) {
      .assert_internal(
        is.atomic(pb[[m]]),
        is.vector(pb[[m]]),
        is.numeric(pb[[m]])
      )
    }

    # NPV mirrors precision: the value of the lowest rank is undefined and is
    # taken from its neighbor
    .assert_internal(pb[["npv"]][n] == pb[["npv"]][n - 1])
  }

  # The metrics derived in R, whichever of them were asked for
  for (m in setdiff(mnames, names(.basic_metric_names()))) {
    .assert_internal(
      is.atomic(pb[[m]]),
      is.vector(pb[[m]]),
      is.numeric(pb[[m]])
    )
  }

  attr(x, "validated") <- TRUE
  x
}

#
# Build a classification report at a chosen operating point
#
#' Classification report
#'
#' `classification_report` builds the per-class table of precision, recall
#' and F-score that `scikit-learn`'s `classification_report` prints, together
#' with the summary rows that go under it.
#'
#' @param mdat An `mdata` object created by [mmdata()]. It can be omitted
#'   when `scores` and `labels` are given.
#'
#' @param scores A numeric vector, matrix, array, data frame, or list of
#'   scores. See [mmdata()] for the accepted shapes.
#'
#' @param labels A numeric, character, logical, or factor vector of
#'   observed labels, or a list of such vectors.
#'
#' @param at The operating point, as a score threshold. An observation is
#'   predicted positive for a class when its score for that class is greater
#'   than or equal to the threshold. It has no default and must be given;
#'   see the note below on why. Either
#'
#'   \itemize{
#'     \item{a single number, used for every class, or}
#'     \item{one number per class, named after the classes or given in
#'       their order.}
#'   }
#'
#'   A score of `NA` is never predicted positive, matching the default
#'   `na_worst = TRUE` of the rest of the package.
#'
#' @param zero_division The value reported when a precision, recall or
#'   F-score divides by zero - a class nothing was predicted into, or a
#'   class with no observations. `0` by default, as in `scikit-learn`.
#'   Use `NA` for the convention the per-cutoff metrics of [evalmod()]
#'   follow.
#'
#' @param ... Further arguments passed to [mmdata()] when `scores` and
#'   `labels` are given instead of `mdat`.
#'
#' @return A data frame with one row per class and one row per summary,
#'   and the columns
#'
#'   \tabular{ll}{
#'     `modnames` \tab Model name \cr
#'     `dsids` \tab Dataset ID \cr
#'     `class` \tab Class name, or the name of a summary row \cr
#'     `precision` \tab Predicted positives that are positive \cr
#'     `recall` \tab Positives that are predicted positive \cr
#'     `fscore` \tab Harmonic mean of the two \cr
#'     `support` \tab Observations of the class \cr
#'   }
#'
#'   The object also has the class `classification_report`, which only
#'   affects how it prints; it is a data frame in every other respect.
#'
#' @section The summary rows:
#'
#' `macro avg` is the unweighted mean over the classes and `weighted avg`
#' the mean weighted by support. The third row depends on whether the
#' predictions assign each observation to exactly one class, which is the
#' rule `scikit-learn` documents:
#'
#' \itemize{
#'   \item{A binary problem at a threshold predicts each observation into
#'     exactly one of the two classes, so the row is `accuracy`.}
#'   \item{A multi-class problem is evaluated by one-vs-rest, and each
#'     class is thresholded on its own, so an observation can fall into no
#'     class or into several. There is then no single-label accuracy, and
#'     the row is `micro avg`: precision, recall and F-score recomputed
#'     from the true positives, false positives and false negatives pooled
#'     over the classes.}
#' }
#'
#' @section Why `at` has no default:
#'
#' `scikit-learn` reports on `y_pred`, so the caller has already chosen an
#' operating point before the function is called. `precrec` holds scores and
#' evaluates every cutoff, so the report has to be told which one to use, and
#' the answer changes the table: on the three-class `C3N150` the F-score of
#' one class moves from 0.37 to 0.51 between two reasonable choices. Scores
#' in `precrec` are on whatever scale the classifier produced, so there is no
#' threshold that is meaningful for all of them - `0.5` says nothing about a
#' log-odds or an SVM margin. The threshold is therefore always the caller's.
#'
#' @seealso [evalmod()] for the same metrics at every cutoff,
#'   [auc()] for the threshold-free summaries and their macro averages,
#'   and [prob_metrics()] for the probability-based losses.
#'
#' @examples
#'
#' ## Multi-class: one row per class, then micro, macro and weighted averages
#' data(C3N150)
#' mdat <- mmdata(C3N150$scores, C3N150$labels)
#' classification_report(mdat, at = 0.5)
#'
#' ## A threshold per class, named or in class order
#' classification_report(mdat, at = c(c1 = 0.4, c2 = 0.5, c3 = 0.6))
#'
#' ## Binary: both classes, and an accuracy row rather than a micro average
#' data(P10N10)
#' classification_report(
#'   scores = P10N10$scores, labels = P10N10$labels,
#'   at = 12
#' )
#'
#' ## It is a data frame, so the usual accessors work
#' report <- classification_report(mdat, at = 0.5)
#' report[report$class == "macro avg", ]
#'
#' @export
classification_report <- function(mdat, scores = NULL, labels = NULL,
                                  at = NULL, zero_division = 0, ...) {
  # === Validate input arguments ===
  mdat <- .create_src_obj(mdat, "mdat", mmdata, scores, labels, ...)
  .validate(mdat)
  zero_division <- .validate_zero_division(zero_division)

  multiclass <- .is_multiclass(mdat)
  info <- .report_info(
    .as_plain_df(attr(mdat, "data_info"), copy = TRUE), multiclass
  )
  cuts <- .validate_report_at(at, info, multiclass)

  # === Count each one-vs-rest split at its threshold ===
  counts <- .report_counts(mdat, info, cuts)

  # === One block per model and dataset ===
  keys <- unique(counts[c("group", "dsids")])
  parts <- lapply(seq_len(nrow(keys)), function(k) {
    sel <- counts[["group"]] == keys[["group"]][k] &
      counts[["dsids"]] == keys[["dsids"]][k]
    .report_block(counts[sel, , drop = FALSE], multiclass, zero_division)
  })

  report <- .as_plain_df(.rbind_parts(parts))
  class(report) <- c("classification_report", class(report))
  report
}


#
# Attach the block key and the class label to each one-vs-rest split
#
# A multi-class model name is the class on its own when there is one model,
# and "class:model" when there are several, so the model is what is left
# after the class is taken off the front. A binary dataset has one split per
# model, and its two class rows are built later from the one confusion
# matrix.
#
.report_info <- function(info, multiclass) {
  if (!multiclass) {
    info[["group"]] <- info[["modnames"]]
    info[["class"]] <- NA_character_
    return(info)
  }

  same <- info[["modnames"]] == info[["classes"]]
  info[["group"]] <- ifelse(
    same, "m1", substring(info[["modnames"]], nchar(info[["classes"]]) + 2L)
  )
  info[["class"]] <- info[["classes"]]
  info
}


#
# Count true and false positives and negatives for every split
#
# Each element of `mdat` is one binary problem: a class against the rest for
# a multi-class dataset, or the whole problem for a binary one. Counting
# straight from the scores rather than from the pipeline's cutoff table is
# exact at an arbitrary threshold, and cheaper, since the report needs one
# cutoff rather than all of them.
#
.report_counts <- function(mdat, info, cuts) {
  parts <- lapply(seq_along(mdat), function(i) {
    sc <- mdat[[i]][["scores"]]

    # Labels are stored as 1 for negatives and 2 for positives
    pos <- as.integer(mdat[[i]][["labels"]]) == 2L

    # `NA >= at` is NA, and an observation the model could not score is not
    # a positive prediction
    pred <- !is.na(sc) & sc >= cuts[i]

    data.table::data.table(
      group = info[["group"]][i],
      dsids = info[["dsids"]][i],
      class = info[["class"]][i],
      tp = sum(pred & pos), fp = sum(pred & !pos),
      fn = sum(!pred & pos), tn = sum(!pred & !pos)
    )
  })

  .as_plain_df(.rbind_parts(parts))
}


#
# Turn the counts of one model and dataset into its block of the report
#
.report_block <- function(rows, multiclass, zero_division) {
  if (multiclass) {
    classes <- rows[["class"]]
    tp <- rows[["tp"]]
    fp <- rows[["fp"]]
    fn <- rows[["fn"]]
  } else {
    # A binary problem holds both classes in one confusion matrix. The
    # negative class is that matrix turned around, which is why its
    # precision is the negative predictive value and its recall the
    # specificity. The original label values are not kept on an `mdata`
    # object, so the rows are named for their roles.
    classes <- c("negative", "positive")
    tp <- c(rows[["tn"]], rows[["tp"]])
    fp <- c(rows[["fn"]], rows[["fp"]])
    fn <- c(rows[["fp"]], rows[["fn"]])
  }

  support <- tp + fn
  precision <- .report_ratio(tp, tp + fp, zero_division)
  recall <- .report_ratio(tp, tp + fn, zero_division)
  fscore <- .report_fscore(precision, recall, zero_division)

  per_class <- data.table::data.table(
    modnames = rows[["group"]][1], dsids = rows[["dsids"]][1],
    class = classes, precision = precision, recall = recall,
    fscore = fscore, support = support
  )

  rbind(per_class, .report_summaries(
    tp, fp, fn, precision, recall, fscore, support, multiclass, rows,
    zero_division
  ))
}


#
# The rows that go under the per-class ones
#
# `scikit-learn` prints a micro average only when the predictions are not a
# single label per observation, "because it corresponds to accuracy
# otherwise and would be the same for all metrics". A binary problem cut at
# a threshold is single-label, so it gets accuracy; a one-vs-rest
# multi-class problem thresholds every class on its own, so an observation
# can land in none of them or in several, and it gets the micro average.
#
.report_summaries <- function(tp, fp, fn, precision, recall, fscore, support,
                              multiclass, rows, zero_division) {
  total <- sum(support)
  wts <- if (total > 0) support / total else rep(0, length(support))

  make <- function(class, p, r, f) {
    data.table::data.table(
      modnames = rows[["group"]][1], dsids = rows[["dsids"]][1],
      class = class, precision = p, recall = r, fscore = f, support = total
    )
  }

  if (multiclass) {
    # Pooled counts, not an average of the per-class rates
    mp <- .report_ratio(sum(tp), sum(tp) + sum(fp), zero_division)
    mr <- .report_ratio(sum(tp), sum(tp) + sum(fn), zero_division)
    first <- make("micro avg", mp, mr, .report_fscore(mp, mr, zero_division))
  } else {
    # The two class rows put each observation on exactly one diagonal, so
    # the correct predictions are `tp` summed over them
    acc <- .report_ratio(sum(tp), total, zero_division)
    first <- make("accuracy", NA_real_, NA_real_, acc)
  }

  rbind(
    first,
    make("macro avg", mean(precision), mean(recall), mean(fscore)),
    make(
      "weighted avg", sum(precision * wts), sum(recall * wts),
      sum(fscore * wts)
    )
  )
}


#
# Ratios that report `zero_division` rather than NaN when the denominator
# counted nothing
#
.report_ratio <- function(num, den, zero_division) {
  ifelse(den > 0, num / den, zero_division)
}


.report_fscore <- function(precision, recall, zero_division) {
  denom <- precision + recall
  ifelse(
    is.na(denom) | denom <= 0, zero_division,
    2 * precision * recall / denom
  )
}

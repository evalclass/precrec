# G: classification_report()

## Build scores whose 0.5 threshold reproduces a set of hard predictions
## exactly, so the report can be held against `scikit-learn`'s own output.
onehot_mdat <- function(y_true, y_pred, cls) {
  scores <- outer(y_pred, seq_along(cls) - 1L, "==") * 1
  colnames(scores) <- cls
  mmdata(scores, cls[y_true + 1L])
}

report_row <- function(report, class) {
  row <- report[report[["class"]] == class, ]
  round(c(row[["precision"]], row[["recall"]], row[["fscore"]]), 2)
}

test_that("classification_report() reproduces the scikit-learn example", {
  # sklearn.metrics.classification_report reference output for
  # y_true = [0, 1, 2, 2, 2], y_pred = [0, 0, 2, 2, 1]
  cls <- c("class 0", "class 1", "class 2")
  report <- classification_report(
    onehot_mdat(c(0, 1, 2, 2, 2), c(0, 0, 2, 2, 1), cls),
    at = 0.5
  )

  expect_equal(report_row(report, "class 0"), c(0.50, 1.00, 0.67))
  expect_equal(report_row(report, "class 1"), c(0.00, 0.00, 0.00))
  expect_equal(report_row(report, "class 2"), c(1.00, 0.67, 0.80))
  expect_equal(report_row(report, "macro avg"), c(0.50, 0.56, 0.49))
  expect_equal(report_row(report, "weighted avg"), c(0.70, 0.60, 0.61))
  expect_equal(report[["support"]][report[["class"]] == "class 2"], 3)
})

test_that("classification_report() reproduces the user-guide example", {
  # y_true = [0, 1, 2, 2, 0], y_pred = [0, 0, 2, 1, 0]
  cls <- c("class 0", "class 1", "class 2")
  report <- classification_report(
    onehot_mdat(c(0, 1, 2, 2, 0), c(0, 0, 2, 1, 0), cls),
    at = 0.5
  )

  expect_equal(report_row(report, "class 0"), c(0.67, 1.00, 0.80))
  expect_equal(report_row(report, "class 1"), c(0.00, 0.00, 0.00))
  expect_equal(report_row(report, "class 2"), c(1.00, 0.50, 0.67))
  expect_equal(report_row(report, "macro avg"), c(0.56, 0.50, 0.49))
  expect_equal(report_row(report, "weighted avg"), c(0.67, 0.60, 0.59))
})

test_that("the micro average is the accuracy when predictions are single-label", {
  # The identity scikit-learn cites for printing one row and not the other:
  # with each observation in exactly one class the pooled counts give
  # precision == recall == F-score == accuracy.
  cls <- c("class 0", "class 1", "class 2")
  report <- classification_report(
    onehot_mdat(c(0, 1, 2, 2, 2), c(0, 0, 2, 2, 1), cls),
    at = 0.5
  )
  micro <- report[report[["class"]] == "micro avg", ]

  expect_equal(micro[["precision"]], micro[["recall"]])
  expect_equal(micro[["precision"]], micro[["fscore"]])
  expect_equal(round(micro[["fscore"]], 2), 0.60)
})

test_that("multi-class reports a micro average and no accuracy", {
  data(C3N150, envir = environment())
  report <- classification_report(
    mmdata(C3N150$scores, C3N150$labels),
    at = 0.5
  )

  expect_true("micro avg" %in% report[["class"]])
  expect_false("accuracy" %in% report[["class"]])
  expect_equal(
    report[["class"]],
    c("c1", "c2", "c3", "micro avg", "macro avg", "weighted avg")
  )
})

test_that("multi-class one-vs-rest is not single-label, which is why", {
  # The reason the accuracy row is absent: thresholding each class on its
  # own leaves observations in no class and in several.
  data(C3N150, envir = environment())
  npred <- rowSums(C3N150$scores >= 0.5)

  expect_true(any(npred == 0))
  expect_true(any(npred > 1))
})

test_that("binary reports an accuracy and no micro average", {
  data(P10N10, envir = environment())
  report <- classification_report(
    scores = P10N10$scores, labels = P10N10$labels, at = 12
  )

  expect_false("micro avg" %in% report[["class"]])
  expect_equal(
    report[["class"]],
    c("negative", "positive", "accuracy", "macro avg", "weighted avg")
  )

  # The accuracy is a single number over the block, so precision and recall
  # are not filled in with it
  acc <- report[report[["class"]] == "accuracy", ]
  expect_true(is.na(acc[["precision"]]))
  expect_true(is.na(acc[["recall"]]))
})

test_that("the binary negative row is the confusion matrix turned around", {
  data(P10N10, envir = environment())
  scores <- P10N10$scores
  labels <- P10N10$labels
  at <- 12
  report <- classification_report(scores = scores, labels = labels, at = at)

  pos <- labels == 1
  pred <- scores >= at
  tp <- sum(pred & pos)
  fp <- sum(pred & !pos)
  tn <- sum(!pred & !pos)
  fn <- sum(!pred & pos)

  neg <- report[report[["class"]] == "negative", ]
  expect_equal(neg[["precision"]], tn / (tn + fn)) # negative predictive value
  expect_equal(neg[["recall"]], tn / (tn + fp)) # specificity

  posrow <- report[report[["class"]] == "positive", ]
  expect_equal(posrow[["precision"]], tp / (tp + fp))
  expect_equal(posrow[["recall"]], tp / (tp + fn))
  expect_equal(
    report[["fscore"]][report[["class"]] == "accuracy"],
    (tp + tn) / length(labels)
  )
})

test_that("support is the number of observations of the class", {
  data(C3N150, envir = environment())
  report <- classification_report(
    mmdata(C3N150$scores, C3N150$labels),
    at = 0.5
  )
  counts <- table(C3N150$labels)

  for (cls in names(counts)) {
    expect_equal(
      report[["support"]][report[["class"]] == cls], as.numeric(counts[[cls]])
    )
  }
  expect_equal(
    report[["support"]][report[["class"]] == "macro avg"], sum(counts)
  )
})

test_that("`at` takes one threshold per class, named or in class order", {
  data(C3N150, envir = environment())
  mdat <- mmdata(C3N150$scores, C3N150$labels)
  thresholds <- c(0.4, 0.5, 0.6)

  ordered <- classification_report(mdat, at = thresholds)
  named <- classification_report(
    mdat,
    at = c(c3 = 0.6, c1 = 0.4, c2 = 0.5)
  )
  expect_equal(ordered, named)

  # Each class really is cut at its own threshold
  for (i in seq_along(thresholds)) {
    one <- classification_report(mdat, at = thresholds[i])
    cls <- c("c1", "c2", "c3")[i]
    expect_equal(report_row(ordered, cls), report_row(one, cls))
  }
})

test_that("`at` is required and validated", {
  data(C3N150, envir = environment())
  mdat <- mmdata(C3N150$scores, C3N150$labels)

  expect_error(classification_report(mdat), class = "precrec_error_invalid_at")
  expect_error(
    classification_report(mdat, at = c(0.1, 0.2)),
    class = "precrec_error_invalid_at"
  )
  expect_error(
    classification_report(mdat, at = c(a = 0.1, b = 0.2, c = 0.3)),
    class = "precrec_error_invalid_at"
  )
  expect_error(
    classification_report(mdat, at = "0.5"),
    class = "precrec_error_invalid_at"
  )
  expect_error(
    classification_report(mdat, at = NA_real_),
    class = "precrec_error_invalid_at"
  )
  expect_error(
    classification_report(mdat, at = Inf),
    class = "precrec_error_invalid_at"
  )
})

test_that("zero_division sets what an empty denominator reports", {
  # Nothing clears the threshold, so no observation is predicted positive
  # and the positive class divides by zero for its precision. The negative
  # class does not: everything landed there.
  mdat <- mmdata(c(0, 0, 0, 0), c(1, 1, 0, 0))

  zeroed <- classification_report(mdat, at = 0.5)
  expect_equal(zeroed[["precision"]][zeroed[["class"]] == "positive"], 0)
  expect_equal(zeroed[["precision"]][zeroed[["class"]] == "negative"], 0.5)

  missing <- classification_report(mdat, at = 0.5, zero_division = NA)
  expect_true(is.na(missing[["precision"]][missing[["class"]] == "positive"]))
  expect_equal(missing[["precision"]][missing[["class"]] == "negative"], 0.5)

  ones <- classification_report(mdat, at = 0.5, zero_division = 1)
  expect_equal(ones[["precision"]][ones[["class"]] == "positive"], 1)

  expect_error(
    classification_report(mdat, at = 0.5, zero_division = 0.5),
    class = "precrec_error_invalid_zero_division"
  )
})

test_that("a score of NA is never a positive prediction", {
  scores <- c(0.9, NA, 0.8, 0.1)
  labels <- c(1, 1, 0, 0)
  report <- classification_report(scores = scores, labels = labels, at = 0.5)

  # The NA belongs to a positive, so it becomes a false negative: recall of
  # the positive class is 1/2 rather than 1/1
  expect_equal(report[["recall"]][report[["class"]] == "positive"], 0.5)
})

test_that("each model and dataset gets its own block", {
  data(C3N150, envir = environment())
  mdat <- mmdata(
    list(C3N150$scores, C3N150$scores * 0.9),
    list(C3N150$labels, C3N150$labels),
    modnames = c("A", "B"), multiclass = "ovr"
  )
  report <- classification_report(mdat, at = 0.5)

  expect_equal(sort(unique(report[["modnames"]])), c("A", "B"))
  expect_equal(nrow(report), 12)
})

test_that("the report is a data frame that prints like scikit-learn's", {
  data(C3N150, envir = environment())
  report <- classification_report(
    mmdata(C3N150$scores, C3N150$labels),
    at = 0.5
  )

  expect_s3_class(report, "data.frame")
  expect_equal(
    names(report),
    c(
      "modnames", "dsids", "class", "precision", "recall", "fscore",
      "support"
    )
  )
  expect_output(print(report), "precision *recall *f1-score *support")
  expect_output(print(report), "micro avg")
  expect_output(print(report, digits = 4), "0\\.6533")
})

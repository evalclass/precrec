# MM 5: Expand a multiclass dataset into one-vs-rest binary datasets

mm5_test_scores <- function(classes = c("c1", "c2", "c3"), n = 4) {
  m <- matrix(seq_len(n * length(classes)), nrow = n)
  colnames(m) <- classes
  m
}

mm5_test_labels <- function() {
  c("c1", "c2", "c3", "c1")
}

# Test .get_new_multiclass(multiclass, scores, labels)

test_that(".get_new_multiclass() detects a one-vs-rest dataset", {
  expect_equal(
    .get_new_multiclass(NULL, mm5_test_scores(), mm5_test_labels()),
    "ovr"
  )
})

test_that(".get_new_multiclass() leaves binary input alone", {
  # Two classes
  expect_equal(
    .get_new_multiclass(NULL, matrix(1:8, 4, 2), c(1, 0, 1, 0)),
    "none"
  )

  # Three classes, but the scores are not one column per class
  expect_equal(
    .get_new_multiclass(NULL, 1:4, mm5_test_labels()),
    "none"
  )
  expect_equal(
    .get_new_multiclass(NULL, matrix(1:8, 4, 2), mm5_test_labels()),
    "none"
  )
})

test_that(".get_new_multiclass() honours an explicit choice", {
  expect_equal(
    .get_new_multiclass("none", mm5_test_scores(), mm5_test_labels()),
    "none"
  )
  expect_equal(
    .get_new_multiclass("o", mm5_test_scores(), mm5_test_labels()),
    "ovr"
  )
  expect_error(
    .get_new_multiclass("pairwise", mm5_test_scores(), mm5_test_labels()),
    class = "precrec_error_invalid_multiclass"
  )
})

# Test .expand_multiclass(scores, labels, modnames, dsids)

test_that(".expand_multiclass() makes one binary dataset per class", {
  mc <- .expand_multiclass(
    mm5_test_scores(), mm5_test_labels(), NULL, NULL
  )

  expect_equal(mc[["classnames"]], c("c1", "c2", "c3"))
  expect_equal(length(mc[["scores"]]), 3)
  expect_equal(mc[["modnames"]], c("c1", "c2", "c3"))
  expect_equal(mc[["dsids"]], c(1, 1, 1))
  expect_equal(mc[["classes"]], c("c1", "c2", "c3"))

  # Column j of the matrix becomes the scores of class j
  expect_equal(mc[["scores"]][[2]], c(5, 6, 7, 8))

  # 1 for the class, 0 for the rest
  expect_equal(mc[["labels"]][[1]], c(1, 0, 0, 1))
  expect_equal(mc[["labels"]][[2]], c(0, 1, 0, 0))
})

test_that(".expand_multiclass() matches columns by name when it can", {
  m <- mm5_test_scores(c("c3", "c1", "c2"))
  mc <- .expand_multiclass(m, mm5_test_labels(), NULL, NULL)

  # The class order comes from the labels, not from the columns
  expect_equal(mc[["classnames"]], c("c1", "c2", "c3"))
  expect_equal(mc[["scores"]][[1]], m[, "c1"])
  expect_equal(mc[["scores"]][[3]], m[, "c3"])
})

test_that(".expand_multiclass() falls back to the column order", {
  m <- mm5_test_scores()
  colnames(m) <- NULL
  mc <- .expand_multiclass(m, mm5_test_labels(), NULL, NULL)

  expect_equal(mc[["scores"]][[1]], c(1, 2, 3, 4))
  expect_equal(mc[["scores"]][[3]], c(9, 10, 11, 12))
})

test_that(".expand_multiclass() keeps the levels of a factor", {
  # "c4" has no rows here, but it is still a class of the problem
  labels <- factor(mm5_test_labels(), levels = c("c1", "c2", "c3", "c4"))
  mc <- .expand_multiclass(
    mm5_test_scores(c("c1", "c2", "c3", "c4")), labels, NULL, NULL
  )

  expect_equal(mc[["classnames"]], c("c1", "c2", "c3", "c4"))
  expect_equal(mc[["labels"]][[4]], c(0, 0, 0, 0))
})

test_that(".expand_multiclass() takes several matrices as several datasets", {
  scores <- list(mm5_test_scores(), mm5_test_scores() + 100)
  mc <- .expand_multiclass(scores, mm5_test_labels(), NULL, NULL)

  expect_equal(length(mc[["scores"]]), 6)
  expect_equal(mc[["modnames"]], rep(c("c1", "c2", "c3"), 2))
  expect_equal(mc[["dsids"]], rep(c(1, 2), each = 3))
})

test_that(".expand_multiclass() combines class and model names", {
  scores <- list(mm5_test_scores(), mm5_test_scores() + 100)
  mc <- .expand_multiclass(
    scores, mm5_test_labels(), c("svm", "rf"), c(1, 1)
  )

  expect_equal(
    mc[["modnames"]],
    c("c1:svm", "c2:svm", "c3:svm", "c1:rf", "c2:rf", "c3:rf")
  )
  expect_equal(mc[["classes"]], rep(c("c1", "c2", "c3"), 2))
  expect_equal(mc[["dsids"]], rep(1, 6))
})

test_that(".expand_multiclass() rejects input it cannot decompose", {
  expect_error(
    .expand_multiclass(1:4, mm5_test_labels(), NULL, NULL),
    class = "precrec_error_invalid_scores"
  )
  expect_error(
    .expand_multiclass(
      mm5_test_scores(c("c1", "c2")), mm5_test_labels(), NULL, NULL
    ),
    class = "precrec_error_invalid_scores"
  )
  expect_error(
    .expand_multiclass(
      matrix(1:8, 4, 2), c(1, 0, 1, 0), NULL, NULL
    ),
    class = "precrec_error_invalid_labels"
  )
  expect_error(
    .expand_multiclass(
      list(mm5_test_scores(), mm5_test_scores()),
      list(mm5_test_labels(), mm5_test_labels(), mm5_test_labels()),
      NULL, NULL
    ),
    class = "precrec_error_invalid_labels"
  )
})

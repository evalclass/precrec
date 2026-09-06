# UC 3: Use cases 3 - multiclass evaluation

uc3_mdat <- function(...) {
  data(C3N150)
  mmdata(C3N150$scores, C3N150$labels, ...)
}

test_that("mmdata() detects a multiclass dataset", {
  mdat <- uc3_mdat()

  expect_equal(attr(mdat, "args")[["multiclass"]], "ovr")
  expect_equal(attr(mdat, "classnames"), c("c1", "c2", "c3"))
  expect_equal(length(mdat), 3)

  # One binary problem per class, all on one dataset
  info <- as.data.frame(attr(mdat, "data_info"))
  expect_equal(info[["modnames"]], c("c1", "c2", "c3"))
  expect_equal(info[["classes"]], c("c1", "c2", "c3"))
  expect_equal(info[["dsids"]], c(1, 1, 1))
  expect_equal(info[["np"]], rep(50, 3))
  expect_equal(info[["nn"]], rep(100, 3))
})

test_that("mmdata() leaves binary input alone", {
  data(P10N10)
  mdat <- mmdata(P10N10$scores, P10N10$labels)

  expect_equal(attr(mdat, "args")[["multiclass"]], "none")
  expect_true(is.na(attr(mdat, "classnames")))
  expect_false("classes" %in% names(attr(mdat, "data_info")))
})

test_that("evalmod() evaluates each class against the rest", {
  curves <- evalmod(uc3_mdat())

  expect_true(is(curves, "mscurves"))
  aucs <- auc(curves, macro = FALSE)
  expect_equal(nrow(aucs), 6)
  expect_equal(as.character(unique(aucs[["modnames"]])), c("c1", "c2", "c3"))

  # The sample is built so that c1 separates best and c3 worst
  rocs <- aucs[aucs$curvetypes == "ROC", "aucs"]
  expect_true(rocs[1] > rocs[2])
  expect_true(rocs[2] > rocs[3])
})

test_that("auc() adds the macro-average of the per-class AUCs", {
  curves <- evalmod(uc3_mdat())
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 8)
  macro <- aucs[aucs$modnames == "macro-average", ]
  per_class <- aucs[aucs$modnames != "macro-average", ]

  expect_equal(
    macro[macro$curvetypes == "ROC", "aucs"],
    mean(per_class[per_class$curvetypes == "ROC", "aucs"])
  )
  expect_equal(
    macro[macro$curvetypes == "PRC", "aucs"],
    mean(per_class[per_class$curvetypes == "PRC", "aucs"])
  )
})

test_that("auc() leaves the macro-average out for binary input", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  expect_equal(nrow(auc(curves)), 2)
  expect_equal(auc(curves), auc(curves, macro = FALSE))
})

test_that("multiclass evaluation works in the other modes", {
  points <- evalmod(uc3_mdat(mode = "basic"), mode = "basic")
  expect_true(is(points, "mspoints"))
  expect_equal(
    as.character(unique(as.data.frame(points)[["modname"]])),
    c("c1", "c2", "c3")
  )

  uaucs <- evalmod(uc3_mdat(mode = "aucroc"), mode = "aucroc")
  df <- as.data.frame(uaucs)
  expect_equal(nrow(df), 3)
  expect_false(any(is.na(df[["aucs"]])))
})

test_that("multiclass evaluation works across several datasets", {
  data(C3N150)
  scores <- list(C3N150$scores, C3N150$scores[, c(2, 3, 1)])
  mdat <- mmdata(scores, C3N150$labels)

  # Two datasets of the same three classes
  expect_equal(length(mdat), 6)
  expect_equal(attr(mdat, "uniq_modnames"), c("c1", "c2", "c3"))
  expect_equal(attr(mdat, "uniq_dsids"), c(1, 2))

  # Three classes on the model axis and two datasets
  curves <- evalmod(mdat)
  expect_true(is(curves, "mmcurves"))

  # One macro-average per dataset
  aucs <- auc(curves)
  macro <- aucs[aucs$modnames == "macro-average", ]
  expect_equal(nrow(macro), 4)
  expect_equal(sort(unique(macro[["dsids"]])), c(1, 2))
})

test_that("multiclass evaluation keeps model names when there are models", {
  data(C3N150)
  scores <- list(C3N150$scores, C3N150$scores[, c(2, 3, 1)])
  mdat <- mmdata(scores, C3N150$labels,
    modnames = c("m1", "m2"), dsids = c(1, 1)
  )

  expect_equal(
    attr(mdat, "uniq_modnames"),
    c("c1:m1", "c2:m1", "c3:m1", "c1:m2", "c2:m2", "c3:m2")
  )

  # One macro-average per model
  aucs <- auc(evalmod(mdat))
  macro <- aucs[grepl("^macro-average", aucs$modnames), ]
  expect_equal(
    as.character(unique(macro[["modnames"]])),
    c("macro-average:m1", "macro-average:m2")
  )
  expect_equal(nrow(macro), 4)
})

test_that("multiclass plots do not draw a single precision-recall baseline", {
  # Every one-vs-rest decomposition has its own class balance
  pn_info <- .get_pn_info(evalmod(uc3_mdat()))
  expect_true(pn_info$is_consistant)

  data(C3N150)
  uneven <- C3N150$labels
  uneven[1:20] <- "c2"
  info <- .get_pn_info(evalmod(scores = C3N150$scores, labels = uneven))
  expect_false(info$is_consistant)
})

test_that("posclass is ignored for a multiclass dataset", {
  data(C3N150)
  expect_warning(
    mdat <- mmdata(C3N150$scores, C3N150$labels, posclass = "c1"),
    "posclass is ignored"
  )
  expect_true(is.null(attr(mdat, "args")[["posclass"]]))
})

test_that("multiclass cannot be combined with nfold_df", {
  data(M2N50F5)
  expect_error(
    mmdata(
      nfold_df = M2N50F5, score_cols = c(1, 2), lab_col = 3, fold_col = 4,
      modnames = c("m1", "m2"), dsids = 1:5, multiclass = "ovr"
    ),
    class = "precrec_error_invalid_multiclass"
  )
})

test_that("three classes without a score matrix are still an error", {
  expect_error(
    mmdata(1:6, c("c1", "c2", "c3", "c1", "c2", "c3")),
    "invalid-labels"
  )
})

# An unbalanced three-class dataset, so that the uniform and the
# prevalence-weighted macro-averages are actually different numbers
uc3_unbalanced <- function() {
  set.seed(42)
  labels <- c(rep("a", 10), rep("b", 30), rep("c", 60))
  scores <- cbind(a = runif(100), b = runif(100), c = runif(100))
  mmdata(scores, labels, multiclass = "ovr")
}

test_that("macro_weight = 'prevalence' weights the classes by their size", {
  curves <- evalmod(uc3_unbalanced())

  aucs <- auc(curves, macro_weight = "prevalence")
  per_class <- aucs[aucs[["modnames"]] %in% c("a", "b", "c"), ]
  weighted <- aucs[aucs[["modnames"]] == "macro-average-weighted", ]

  for (ct in c("ROC", "PRC")) {
    vals <- per_class[per_class[["curvetypes"]] == ct, "aucs"]
    expect_equal(
      weighted[weighted[["curvetypes"]] == ct, "aucs"],
      sum(vals * c(10, 30, 60)) / 100
    )
  }
})

test_that("the two macro-averages differ when the classes are unbalanced", {
  curves <- evalmod(uc3_unbalanced())

  uniform <- auc(curves)
  weighted <- auc(curves, macro_weight = "prevalence")

  u <- uniform[uniform[["modnames"]] == "macro-average", "aucs"]
  w <- weighted[weighted[["modnames"]] == "macro-average-weighted", "aucs"]

  expect_equal(length(u), 2)
  expect_equal(length(w), 2)
  expect_false(isTRUE(all.equal(u, w)))
})

test_that("the two macro-averages agree when the classes are balanced", {
  curves <- evalmod(uc3_mdat())

  uniform <- auc(curves)
  weighted <- auc(curves, macro_weight = "prevalence")

  expect_equal(
    uniform[uniform[["modnames"]] == "macro-average", "aucs"],
    weighted[weighted[["modnames"]] == "macro-average-weighted", "aucs"]
  )

  # Only the label of the added rows changes
  expect_equal(
    uniform[uniform[["modnames"]] != "macro-average", ],
    weighted[weighted[["modnames"]] != "macro-average-weighted", ]
  )
})

test_that("macro_weight is left alone when macro = FALSE", {
  curves <- evalmod(uc3_unbalanced())

  expect_equal(
    auc(curves, macro = FALSE),
    auc(curves, macro = FALSE, macro_weight = "prevalence")
  )
})

test_that("auc() rejects an unknown macro_weight", {
  curves <- evalmod(uc3_mdat())

  expect_error(auc(curves, macro_weight = "median"))
})

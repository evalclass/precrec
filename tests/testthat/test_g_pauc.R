# PC 1: Retrieve pAUCs
# Test auc(curves)

pauc_create_mscurves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat)
}

pauc_create_smcurves <- function(raw_curves = TRUE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, raw_curves = raw_curves)
}

pauc_create_mmcurves <- function(raw_curves = TRUE) {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  s4 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3, s4)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  l4 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3, l4)

  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
  evalmod(mdat, raw_curves = raw_curves)
}

test_that("pauc for invalid object", {
  expect_error(pauc(""), "unknown class")
})

test_that("paucs for sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_error(pauc(curves), "should be used first")

  curves_part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves_part)

  expect_equal(nrow(paucs), 2)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 1)
})

test_that("paucs for mscurves", {
  curves <- pauc_create_mscurves()
  expect_error(pauc(curves), "should be used first")

  curves_part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves_part)

  expect_equal(nrow(paucs), 6)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 3)
})

test_that("paucs for smcurves", {
  curves <- pauc_create_smcurves()
  expect_error(pauc(curves), "should be used first")

  curves_part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves_part)

  expect_equal(nrow(paucs), 6)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 3)
})

test_that("paucs for avg smcurves", {
  curves <- pauc_create_smcurves(FALSE)
  expect_error(pauc(curves), "should be used first")

  curves_part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves_part)

  expect_equal(nrow(paucs), 2)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 1)
})

test_that("paucs for mmcurves", {
  curves <- pauc_create_mmcurves()
  expect_error(pauc(curves), "should be used first")

  curves_part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves_part)

  expect_equal(nrow(paucs), 8)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 4)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 4)
})

test_that("paucs for avg mmcurves", {
  curves <- pauc_create_mmcurves(FALSE)
  expect_error(pauc(curves), "should be used first")

  curves_part <- part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1))
  paucs <- pauc(curves_part)

  expect_equal(nrow(paucs), 4)
  expect_equal(nrow(subset(paucs, curvetypes == "PRC")), 2)
  expect_equal(nrow(subset(paucs, curvetypes == "ROC")), 2)
})

test_that("pauc() returns a plain data frame that does not alias the object", {
  data(P10N10)
  curves <- part(evalmod(scores = P10N10$scores, labels = P10N10$labels),
    xlim = c(0, 0.25)
  )

  paucs <- pauc(curves)
  expect_identical(class(paucs), "data.frame")

  before <- pauc(curves)[["paucs"]]
  paucs[["paucs"]] <- -1
  expect_equal(pauc(curves)[["paucs"]], before)
})

test_that("pauc() adds the McClish correction only when it is asked for", {
  data(P10N10)
  curves <- part(evalmod(scores = P10N10$scores, labels = P10N10$labels),
    xlim = c(0, 0.25)
  )

  expect_equal(
    names(pauc(curves)),
    c("modnames", "dsids", "curvetypes", "paucs", "spaucs")
  )
  expect_equal(
    names(pauc(curves, corrected = TRUE)),
    c("modnames", "dsids", "curvetypes", "paucs", "spaucs", "cpaucs")
  )

  expect_error(
    pauc(curves, corrected = "yes"),
    class = "precrec_error_invalid_corrected"
  )
})

test_that("pauc(corrected = TRUE) matches the values pROC reports", {
  # pROC 1.19.1, auc(partial.auc = , partial.auc.correct = TRUE)
  set.seed(7)
  scores <- c(rnorm(60, 1.1), rnorm(140, 0))
  labels <- rep(c(1, 0), c(60, 140))
  curves <- evalmod(mmdata(scores, labels))

  roc_row <- function(xlim) {
    paucs <- pauc(part(curves, xlim = xlim), corrected = TRUE)
    paucs[paucs[["curvetypes"]] == "ROC", ]
  }

  expect_equal(roc_row(c(0, 0.2))[["cpaucs"]], 0.7159392, tolerance = 1e-6)
  # A region that does not start at zero, where the formula quoted for the
  # zero case gives the wrong answer
  expect_equal(roc_row(c(0.1, 0.3))[["cpaucs"]], 0.7752976, tolerance = 1e-6)
  expect_equal(roc_row(c(0.5, 1))[["cpaucs"]], 0.9352381, tolerance = 1e-6)

  # Over the whole curve the correction is the identity
  whole <- roc_row(c(0, 1))
  expect_equal(whole[["cpaucs"]], whole[["paucs"]])
  expect_equal(
    whole[["cpaucs"]],
    subset(auc(curves), curvetypes == "ROC")[["aucs"]]
  )
})

test_that("pauc(corrected = TRUE) reports nothing where it is undefined", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  # A precision-recall curve is measured against the class balance, not the
  # diagonal, so the correction has no counterpart there
  roc_only <- pauc(part(curves, xlim = c(0, 0.5)), corrected = TRUE)
  expect_true(is.na(subset(roc_only, curvetypes == "PRC")[["cpaucs"]]))
  expect_false(is.na(subset(roc_only, curvetypes == "ROC")[["cpaucs"]]))

  # A restricted y range has no settled definition
  boxed <- pauc(
    part(curves, xlim = c(0, 0.5), ylim = c(0.5, 1)),
    corrected = TRUE
  )
  expect_true(all(is.na(boxed[["cpaucs"]])))
})

test_that("pauc(corrected = TRUE) works on averaged curves", {
  curves <- part(pauc_create_smcurves(FALSE), xlim = c(0, 0.5))
  paucs <- pauc(curves, corrected = TRUE)

  expect_equal(
    names(paucs),
    c("modnames", "curvetypes", "paucs", "spaucs", "cpaucs")
  )

  roc <- subset(paucs, curvetypes == "ROC")
  chance <- 0.5^2 / 2
  expect_equal(
    roc[["cpaucs"]],
    0.5 * (1 + (roc[["paucs"]] - chance) / (0.5 - chance))
  )
})

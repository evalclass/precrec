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
    c(
      "modnames", "dsids", "curvetypes", "paucs", "baselines", "spaucs",
      "sbaselines"
    )
  )
  expect_equal(
    names(pauc(curves, corrected = TRUE)),
    c(
      "modnames", "dsids", "curvetypes", "paucs", "baselines", "spaucs",
      "sbaselines", "cpaucs"
    )
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
    c(
      "modnames", "curvetypes", "paucs", "baselines", "spaucs",
      "sbaselines", "cpaucs"
    )
  )

  roc <- subset(paucs, curvetypes == "ROC")
  chance <- 0.5^2 / 2
  expect_equal(
    roc[["cpaucs"]],
    0.5 * (1 + (roc[["paucs"]] - chance) / (0.5 - chance))
  )
})

test_that("pauc() reports what a partial area is worth by chance", {
  set.seed(1)
  scores <- c(rnorm(20, 1.2), rnorm(80))
  labels <- rep(c(1, 0), c(20, 80))
  curves <- evalmod(scores = scores, labels = labels)

  x1 <- 0.1
  x2 <- 0.3
  paucs <- pauc(part(curves, xlim = c(x1, x2)))
  roc <- paucs[paucs$curvetypes == "ROC", ]
  prc <- paucs[paucs$curvetypes == "PRC", ]

  # A coin flip covers the area under the diagonal across the region, which
  # is region^2 / 2 only when the region starts at 0
  expect_equal(roc$baselines, (x2^2 - x1^2) / 2)
  expect_equal(roc$sbaselines, (x1 + x2) / 2)
  expect_false(isTRUE(all.equal(roc$sbaselines, 0.5)))

  # A precision-recall curve is flat at the proportion of positives
  expect_equal(prc$baselines, 0.2 * (x2 - x1))
  expect_equal(prc$sbaselines, 0.2)

  # spaucs divides the area by the area of the region, and the baselines
  # divide the same way
  expect_equal(paucs$baselines / (x2 - x1), paucs$sbaselines)
})

test_that("pauc() over the whole curve agrees with auc()", {
  set.seed(2)
  scores <- c(rnorm(20, 1.2), rnorm(80))
  labels <- rep(c(1, 0), c(20, 80))
  curves <- evalmod(scores = scores, labels = labels)

  paucs <- pauc(part(curves, xlim = c(0, 1)))
  aucs <- auc(curves)

  expect_equal(paucs$paucs, aucs$aucs)
  expect_equal(paucs$baselines, aucs$baselines)
  expect_equal(paucs$sbaselines, aucs$baselines)
})

test_that("pauc() gives no baseline for a restricted ylim", {
  set.seed(3)
  scores <- c(rnorm(20, 1.2), rnorm(80))
  labels <- rep(c(1, 0), c(20, 80))
  curves <- evalmod(scores = scores, labels = labels)

  # The region is then a box the chance curve may cross, touch or miss, and
  # the area of chance inside it has no settled definition - the case
  # cpaucs declines for the same reason
  paucs <- pauc(part(curves, xlim = c(0, 0.5), ylim = c(0.1, 1)))

  expect_true(all(is.na(paucs$baselines)))
  expect_true(all(is.na(paucs$sbaselines)))
  expect_false(anyNA(paucs$paucs))
})

test_that("pauc() on averaged curves averages the baseline over datasets", {
  scores <- list(c(rnorm(10, 1), rnorm(90)), c(rnorm(30, 1), rnorm(70)))
  labels <- list(rep(c(1, 0), c(10, 90)), rep(c(1, 0), c(30, 70)))
  curves <- evalmod(
    mmdata(scores, labels, modnames = c("m1", "m1"), dsids = c(1, 2))
  )

  paucs <- pauc(part(curves, xlim = c(0, 0.5)))
  prc <- paucs[paucs$curvetypes == "PRC", ]

  # An averaged curve has no single test dataset behind it, so its chance
  # level is the mean of the prevalences that went into it
  expect_false("dsids" %in% names(paucs))
  expect_equal(prc$sbaselines, mean(c(0.1, 0.3)))
})

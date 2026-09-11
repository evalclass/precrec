# AC 1: Retrieve AUCs
# Test auc(curves)

auc_create_mscurves <- function() {
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

auc_create_smcurves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat)
}

auc_create_mmcurves <- function() {
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
  evalmod(mdat)
}

test_that("aucs for invalid object", {
  expect_error(auc(""), "unknown class")
})

test_that("aucs for sscurves", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 2)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 1)
})

test_that("aucs for mscurves", {
  curves <- auc_create_mscurves()
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 6)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 3)
})

test_that("aucs for smcurves", {
  curves <- auc_create_smcurves()
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 6)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 3)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 3)
})

test_that("aucs for mmcurves", {
  curves <- auc_create_mmcurves()
  aucs <- auc(curves)

  expect_equal(nrow(aucs), 8)
  expect_equal(nrow(subset(aucs, curvetypes == "PRC")), 4)
  expect_equal(nrow(subset(aucs, curvetypes == "ROC")), 4)
})

test_that("auc() returns a plain data frame that does not alias the object", {
  data(P10N10)
  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)

  aucs <- auc(curves)
  expect_identical(class(aucs), "data.frame")

  before <- auc(curves)[["aucs"]]
  aucs[["aucs"]] <- -1
  expect_equal(auc(curves)[["aucs"]], before)
})

test_that("auc() reports the chance level of each curve", {
  set.seed(1)
  scores <- c(rnorm(20, 1.2), rnorm(980, 0))
  labels <- rep(c(1, 0), c(20, 980))
  aucs <- auc(evalmod(scores = scores, labels = labels))

  expect_equal(
    names(aucs),
    c("modnames", "dsids", "curvetypes", "aucs", "baselines")
  )

  # 0.5 for a ROC curve whatever the data, the prevalence for a PRC curve
  expect_equal(aucs[["baselines"]][aucs[["curvetypes"]] == "ROC"], 0.5)
  expect_equal(aucs[["baselines"]][aucs[["curvetypes"]] == "PRC"], 0.02)

  # The area this is here to qualify: below its ROC AUC and above its
  # baseline at the same time
  prc <- aucs[["aucs"]][aucs[["curvetypes"]] == "PRC"]
  expect_lt(prc, aucs[["aucs"]][aucs[["curvetypes"]] == "ROC"])
  expect_gt(prc, 0.02)
})

test_that("auc() takes the baseline of each test dataset separately", {
  scores <- join_scores(c(4, 3, 2, 1), c(4, 3, 2, 1))
  labels <- join_labels(c(1, 1, 1, 0), c(1, 0, 0, 0))
  aucs <- auc(evalmod(mmdata(scores, labels, expd_first = "dsids")))

  prc <- subset(aucs, curvetypes == "PRC")
  expect_equal(prc[["baselines"]], c(0.75, 0.25))
  expect_equal(subset(aucs, curvetypes == "ROC")[["baselines"]], c(0.5, 0.5))
})

test_that("auc() averages the baseline over the classes it averaged", {
  set.seed(5)
  labels <- rep(c("c1", "c2", "c3"), c(10, 40, 150))
  scores <- cbind(rnorm(200), rnorm(200), rnorm(200))
  curves <- evalmod(mmdata(scores, labels, multiclass = "ovr"))

  per_class <- c(10, 40, 150) / 200

  uniform <- auc(curves)
  base <- subset(uniform, modnames == "macro-average" & curvetypes == "PRC")
  expect_equal(base[["baselines"]], mean(per_class))

  weighted <- auc(curves, macro_weight = "prevalence")
  wbase <- subset(
    weighted, modnames == "macro-average-weighted" & curvetypes == "PRC"
  )
  expect_equal(
    wbase[["baselines"]],
    sum(per_class * c(10, 40, 150)) / sum(c(10, 40, 150))
  )

  # A ROC row stays at chance however the classes are mixed
  expect_equal(
    subset(uniform, curvetypes == "ROC")[["baselines"]], rep(0.5, 4)
  )
})

test_that("auc() leaves the baseline out of nothing when macro is off", {
  data(C3N150)
  aucs <- auc(evalmod(mmdata(C3N150$scores, C3N150$labels)), macro = FALSE)

  expect_true(all(!is.na(aucs[["baselines"]])))
  expect_equal(subset(aucs, curvetypes == "PRC")[["baselines"]], rep(1 / 3, 3))
})

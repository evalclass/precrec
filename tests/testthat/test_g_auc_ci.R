#' @importFrom precrec

context("CI 1: AUC CIs")
# Test auc_ci(curves)

auc_ci_create_mscurves <- function() {
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

auc_ci_create_smcurves <- function() {
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

auc_ci_create_smcurves_n2 <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  scores <- join_scores(s1, s2)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  labels <- join_labels(l1, l2)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat)
}

auc_ci_create_mmcurves <- function() {
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

  mdat <- mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
                 expd_first = "modnames")
  evalmod(mdat)
}

auc_ci_create_mmcurves_n1 <- function() {
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

  mdat <- mmdata(scores, labels, modnames = c("m1", "m1", "m1" ,"m2"),
                 dsids = c(1, 2, 3, 1))
  evalmod(mdat)
}

test_that("auc_ci for sscurves", {
  data(P10N10)

  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_error(auc_ci(curves), "'curves' must contain multiple datasets.")
})

test_that("auc_ci for mscurves", {
  curves <- auc_ci_create_mscurves()

  curves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
  expect_error(auc_ci(curves), "'curves' must contain multiple datasets.")
})

test_that("auc_ci for smcurves", {
  curves <- auc_ci_create_smcurves()
  cis <- auc_ci(curves)

  expect_equal(nrow(cis), 2)
  expect_equal(ncol(cis), 7)
  expect_equal(nrow(subset(cis, curvetypes == "PRC")), 1)
  expect_equal(nrow(subset(cis, curvetypes == "ROC")), 1)
})

test_that("auc_ci for mmcurves", {
  curves <- auc_ci_create_mmcurves()
  cis <- auc_ci(curves)

  expect_equal(nrow(cis), 4)
  expect_equal(ncol(cis), 7)
  expect_equal(nrow(subset(cis, curvetypes == "PRC")), 2)
  expect_equal(nrow(subset(cis, curvetypes == "ROC")), 2)
})

test_that("auc_ci alpha", {
  curves <- auc_ci_create_smcurves()

  # Check function signature
  expect_error(auc_ci(curves, 0.5), NA)
  expect_error(auc_ci(curves, alpha = 0.5), NA)

  # Check varialbe type
  err_msg <- "alpha is not a number"
  expect_error(auc_ci(curves, alpha = "0.5"), err_msg)
  expect_error(auc_ci(curves, alpha = NA), err_msg)
  expect_error(auc_ci(curves, alpha = NA), err_msg)

  # Check lower limit
  err_msg <- "alpha not greater than or equal to 0"
  expect_error(auc_ci(curves, -0.1), err_msg)
  expect_error(auc_ci(curves, alpha = -0.1), err_msg)
  expect_error(auc_ci(curves, alpha = 0), NA)

  # Check upper limit
  err_msg <- "alpha not less than or equal to 1"
  expect_error(auc_ci(curves, 1.1), err_msg)
  expect_error(auc_ci(curves, alpha = 1.1), err_msg)
  expect_error(auc_ci(curves, alpha = 1), NA)

})

test_that("auc_ci dtype", {
  curves <- auc_ci_create_smcurves()

  # Check function signature
  expect_error(auc_ci(curves, 0.5, "normal"), NA)
  expect_error(auc_ci(curves, alpha = 0.5, "normal"), NA)
  expect_error(auc_ci(curves, dtype = "normal", alpha = 0.5), NA)
  expect_error(auc_ci(curves, dtype = "normal"), NA)

  # Check varialbe type
  err_msg <- "dtype is not a string"
  expect_error(auc_ci(curves, dtype = 0), err_msg)
  expect_error(auc_ci(curves, dtype = NA), err_msg)
  expect_error(auc_ci(curves, dtype = FALSE), err_msg)

  # Check valid input
  err_msg <- "'dtype' must be one of "
  expect_error(auc_ci(curves, dtype = "normal"), NA)
  expect_error(auc_ci(curves, dtype = "n"), NA)
  expect_error(auc_ci(curves, dtype = "Normal"), NA)
  expect_error(auc_ci(curves, dtype = "t"), NA)
  expect_error(auc_ci(curves, dtype = "T"), NA)
  expect_error(auc_ci(curves, dtype = "z"), NA)
  expect_error(auc_ci(curves, dtype = "d"), err_msg)
  expect_error(auc_ci(curves, dtype = "ormal"), err_msg)

})

test_that("auc_ci normal distribution for ROC", {
  curves <- auc_ci_create_smcurves()

  # AUC
  roc_auc <- subset(auc(curves), curvetypes == "ROC")

  # CI of AUC
  roc_ci <- subset(auc_ci(curves), curvetypes == "ROC")

  # Prepare for CI calculation
  roc_mean <- mean(roc_auc$aucs)
  roc_n <- length(roc_auc$aucs)
  roc_sd <- sd(roc_auc$aucs)

  # Check mean
  expect_equal(roc_ci$n, roc_n)
  expect_equal(roc_ci$mean, roc_mean)

  # Calculate CI
  alpha = 0.05
  norm_z <- qnorm(1 - (alpha / 2))

  roc_error <- norm_z * roc_sd / sqrt(roc_n)
  roc_lower <- max(roc_mean - roc_error, 0.0)
  roc_upper <- min(roc_mean + roc_error, 1.0)

  # Check CI
  expect_equal(roc_ci$error, roc_error)
  expect_equal(roc_ci$lower_bound, roc_lower)
  expect_equal(roc_ci$upper_bound, roc_upper)
})

test_that("auc_ci normal distribution (z) for ROC", {
  curves <- auc_ci_create_smcurves()

  # AUC
  roc_auc <- subset(auc(curves), curvetypes == "ROC")

  # CI of AUC
  roc_ci <- subset(auc_ci(curves, dtype = "z"), curvetypes == "ROC")

  # Prepare for CI calculation
  roc_mean <- mean(roc_auc$aucs)
  roc_n <- length(roc_auc$aucs)
  roc_sd <- sd(roc_auc$aucs)

  # Check mean
  expect_equal(roc_ci$n, roc_n)
  expect_equal(roc_ci$mean, roc_mean)

  # Calculate CI
  alpha = 0.05
  norm_z <- qnorm(1 - (alpha / 2))

  roc_error <- norm_z * roc_sd / sqrt(roc_n)
  roc_lower <- max(roc_mean - roc_error, 0.0)
  roc_upper <- min(roc_mean + roc_error, 1.0)

  # Check CI
  expect_equal(roc_ci$error, roc_error)
  expect_equal(roc_ci$lower_bound, roc_lower)
  expect_equal(roc_ci$upper_bound, roc_upper)
})

test_that("auc_ci normal distribution for PRC", {
  curves <- auc_ci_create_smcurves()

  # AUC
  prc_auc <- subset(auc(curves), curvetypes == "PRC")

  # CI of AUC
  prc_ci <- subset(auc_ci(curves), curvetypes == "PRC")

  # Prepare for CI calculation
  prc_mean <- mean(prc_auc$aucs)
  prc_n <- length(prc_auc$aucs)
  prc_sd <- sd(prc_auc$aucs)

  # Check mean
  expect_equal(prc_ci$n, prc_n)
  expect_equal(prc_ci$mean, prc_mean)

  # Calculate CI
  alpha = 0.05
  norm_z <- qnorm(1 - (alpha / 2))

  prc_error <- norm_z * prc_sd / sqrt(prc_n)
  prc_lower <- max(prc_mean - prc_error, 0.0)
  prc_upper <- min(prc_mean + prc_error, 1.0)

  # Check CI
  expect_equal(prc_ci$error, prc_error)
  expect_equal(prc_ci$lower_bound, prc_lower)
  expect_equal(prc_ci$upper_bound, prc_upper)
})

test_that("auc_ci t-distribution for ROC", {
  curves <- auc_ci_create_smcurves()

  # AUC
  roc_auc <- subset(auc(curves), curvetypes == "ROC")

  # CI of AUC
  roc_ci <- subset(auc_ci(curves, dtype = "t"), curvetypes == "ROC")

  # Prepare for CI calculation
  roc_mean <- mean(roc_auc$aucs)
  roc_n <- length(roc_auc$aucs)
  roc_sd <- sd(roc_auc$aucs)

  # Check mean
  expect_equal(roc_ci$n, roc_n)
  expect_equal(roc_ci$mean, roc_mean)

  # Calculate CI
  alpha = 0.05
  q_t <- qt(1 - (alpha / 2), df = roc_n - 1)

  roc_error <- q_t * roc_sd / sqrt(roc_n)
  roc_lower <- max(roc_mean - roc_error, 0.0)
  roc_upper <- min(roc_mean + roc_error, 1.0)

  # Check CI
  expect_equal(roc_ci$error, roc_error)
  expect_equal(roc_ci$lower_bound, roc_lower)
  expect_equal(roc_ci$upper_bound, roc_upper)
})

test_that("auc_ci t-distribution for PRC", {
  curves <- auc_ci_create_smcurves()

  # AUC
  prc_auc <- subset(auc(curves), curvetypes == "PRC")

  # CI of AUC
  prc_ci <- subset(auc_ci(curves, dtype = "t"), curvetypes == "PRC")

  # Prepare for CI calculation
  prc_mean <- mean(prc_auc$aucs)
  prc_n <- length(prc_auc$aucs)
  prc_sd <- sd(prc_auc$aucs)

  # Check mean
  expect_equal(prc_ci$n, prc_n)
  expect_equal(prc_ci$mean, prc_mean)

  # Calculate CI
  alpha = 0.05
  q_t <- qt(1 - (alpha / 2), df = prc_n - 1)

  prc_error <- q_t * prc_sd / sqrt(prc_n)
  prc_lower <- max(prc_mean - prc_error, 0.0)
  prc_upper <- min(prc_mean + prc_error, 1.0)

  # Check CI
  expect_equal(prc_ci$error, prc_error)
  expect_equal(prc_ci$lower_bound, prc_lower)
  expect_equal(prc_ci$upper_bound, prc_upper)
})

test_that("auc_ci n = 2 ROC", {
  curves <- auc_ci_create_smcurves_n2()

  # AUC
  roc_auc <- subset(auc(curves), curvetypes == "ROC")

  # CI of AUC
  roc_ci <- subset(auc_ci(curves), curvetypes == "ROC")

  # Prepare for CI calculation
  roc_mean <- mean(roc_auc$aucs)
  roc_n <- length(roc_auc$aucs)
  roc_sd <- sd(roc_auc$aucs)

  # Check mean
  expect_equal(roc_ci$n, roc_n)
  expect_equal(roc_ci$mean, roc_mean)

  # Calculate CI
  alpha = 0.05
  norm_z <- qnorm(1 - (alpha / 2))

  roc_error <- norm_z * roc_sd / sqrt(roc_n)
  roc_lower <- max(roc_mean - roc_error, 0.0)
  roc_upper <- min(roc_mean + roc_error, 1.0)

  # Check CI
  expect_equal(roc_ci$error, roc_error)
  expect_equal(roc_ci$lower_bound, roc_lower)
  expect_equal(roc_ci$upper_bound, roc_upper)
})

test_that("auc_ci n = 2 PRC", {
  curves <- auc_ci_create_smcurves_n2()

  # AUC
  prc_auc <- subset(auc(curves), curvetypes == "PRC")

  # CI of AUC
  prc_ci <- subset(auc_ci(curves), curvetypes == "PRC")

  # Prepare for CI calculation
  prc_mean <- mean(prc_auc$aucs)
  prc_n <- length(prc_auc$aucs)
  prc_sd <- sd(prc_auc$aucs)

  # Check mean
  expect_equal(prc_ci$n, prc_n)
  expect_equal(prc_ci$mean, prc_mean)

  # Calculate CI
  alpha = 0.05
  norm_z <- qnorm(1 - (alpha / 2))

  prc_error <- norm_z * prc_sd / sqrt(prc_n)
  prc_lower <- max(prc_mean - prc_error, 0.0)
  prc_upper <- min(prc_mean + prc_error, 1.0)

  # Check CI
  expect_equal(prc_ci$error, prc_error)
  expect_equal(prc_ci$lower_bound, prc_lower)
  expect_equal(prc_ci$upper_bound, prc_upper)
})

test_that("auc_ci n = 1", {
  curves <- auc_ci_create_mmcurves_n1()

  # CI of AUC
  ci_n1 <- subset(auc_ci(curves), modnames == "m2")
  ci_n1_roc <- subset(ci_n1, curvetypes == "ROC")
  ci_n1_prc <- subset(ci_n1, curvetypes == "PRC")

  # Check CI
  expect_equal(ci_n1_roc$error, 0)
  expect_equal(ci_n1_roc$lower_bound, ci_n1_roc$mean)
  expect_equal(ci_n1_roc$upper_bound, ci_n1_roc$mean)

  expect_equal(ci_n1_prc$error, 0)
  expect_equal(ci_n1_prc$lower_bound, ci_n1_prc$mean)
  expect_equal(ci_n1_prc$upper_bound, ci_n1_prc$mean)
})


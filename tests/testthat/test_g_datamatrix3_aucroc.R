#' @importFrom precrec

context("DF 3: as.data.frame aucroc")
# Test as.data.frame(x, ...)

df3_create_msaucs <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
  evalmod(mdat, mode = "aucroc")
}

df3_create_smaucs <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
  evalmod(mdat, mode = "aucroc")
}

df3_create_mmaucs <- function() {
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
  evalmod(mdat, mode = "aucroc")
}

test_that("as.data.frame aucroc ss", {
  data(P10N10)
  aucs <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
                  mode = "aucroc")

  aucs_df <- as.data.frame(aucs)
  expect_true(is.data.frame(aucs_df))
  expect_equal(nrow(aucs_df), 1)
  expect_equal(ncol(aucs_df), 4)
})

test_that("as.data.frame aucroc ms", {
  aucs <- df3_create_msaucs()

  aucs_df <- as.data.frame(aucs)
  expect_true(is.data.frame(aucs_df))
  expect_equal(nrow(aucs_df), 3)
  expect_equal(ncol(aucs_df), 4)
})

test_that("as.data.frame aucroc sm", {
  aucs <- df3_create_smaucs()

  aucs_df <- as.data.frame(aucs)
  expect_true(is.data.frame(aucs_df))
  expect_equal(nrow(aucs_df), 3)
  expect_equal(ncol(aucs_df), 4)
})

test_that("as.data.frame aucroc mm", {
  aucs <- df3_create_mmaucs()

  aucs_df <- as.data.frame(aucs)
  expect_true(is.data.frame(aucs_df))
  expect_equal(nrow(aucs_df), 4)
  expect_equal(ncol(aucs_df), 4)
})


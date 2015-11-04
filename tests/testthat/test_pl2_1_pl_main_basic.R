library(precrec)

context("PL 2: Pipeline main for basic evaluation values")
# Test .pl_main_basic(mdat, x_bins)

pl1_create_mdat_ms <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels)
}

pl1_create_mdat_sm <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mdat <- mmdata(scores, labels, expd_first = "dsids")
}

pl1_create_mdat_mm <- function() {
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
}

pl1_create_mdat_mm <- function() {
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
}

test_that(".make_prefix() takes 'model_type' and 'data_type'", {
  expect_equal(.make_prefix("single", "single"), "ss")
  expect_equal(.make_prefix("multiple", "single"), "ms")
  expect_equal(.make_prefix("single", "multiple"), "sm")
  expect_equal(.make_prefix("multiple", "multiple"), "mm")

  expect_equal(.make_prefix("sing", "multi"), "")
  expect_equal(.make_prefix("s", "m"), "")
})

test_that(".pl_main_basic() returns 'sspoints'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1)
  pl <- .pl_main_basic(mdat, "single", "single", "ss")

  expect_true(is(pl, "sspoints"))
})

test_that(".pl_main_basic() returns 'mspoints'", {
  mdat <- pl1_create_mdat_ms()
  pl <- .pl_main_basic(mdat, "multiple", "single", "ms")

  expect_true(is(pl, "mspoints"))
})

test_that(".pl_main_basic() returns 'smpoints'", {
  mdat <- pl1_create_mdat_sm()
  pl <- .pl_main_basic(mdat, "single", "multiple", "sm")

  expect_true(is(pl, "smpoints"))
})

test_that(".pl_main_basic() returns 'mmpoints'", {
  mdat <- pl1_create_mdat_mm()
  pl <- .pl_main_basic(mdat, "multiple", "multiple", "mm")

  expect_true(is(pl, "mmpoints"))
})

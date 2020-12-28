#' @importFrom precrec

context("PL 2: Pipeline main for AUC (ROC)")
# Test .pl_main_aucroc(mdat, model_type, dataset_type, class_name_pf,
#                      cald_avg, cb_alpha, raw_curves, na_worst, ties_method)

pl2_create_mdat_ms <- function(mode = "aucroc") {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels, mode = mode)
}

pl2_create_mdat_sm <- function(mode = "aucroc") {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  mmdata(scores, labels, expd_first = "dsids", mode = mode)
}

pl2_create_mdat_mm <- function(mode = "aucroc") {
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

  mmdata(scores, labels, modnames = c("m1", "m2"), dsids = c(1, 2),
         expd_first = "modnames", mode = mode)
}

test_that(".pl_main_aucroc() returns 'aucroc'", {
  s1 <- c(1, 2, 3, 4)
  l1 <- c(1, 0, 1, 0)

  mdat <- mmdata(s1, l1, mode = "aucroc")
  pl <- .pl_main_aucroc(mdat, "single", "single", "ss")

  expect_true(is(pl, "aucroc"))
})

test_that(".pl_main_aucroc() returns 'aucroc'", {
  mdat <- pl2_create_mdat_ms()
  pl <- .pl_main_aucroc(mdat, "multiple", "single", "ms")

  expect_true(is(pl, "aucroc"))
})

test_that(".pl_main_aucroc() returns 'aucroc'", {
  mdat <- pl2_create_mdat_sm()
  pl <- .pl_main_aucroc(mdat, "single", "multiple", "sm")

  expect_true(is(pl, "aucroc"))
})

test_that(".pl_main_aucroc() returns 'aucroc'", {
  mdat <- pl2_create_mdat_mm()
  pl <- .pl_main_aucroc(mdat, "multiple", "multiple", "mm")

  expect_true(is(pl, "aucroc"))
})

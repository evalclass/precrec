#' @importFrom precrec

context("PL 3: Calculate AUC (ROC) with the U statitics")
# Test calc_auc_with_u(sdat, scores, labels, na_worst, ties_method, keep_sdat)

test_that("calc_auc_with_u() reterns a 'uauc' object", {
  auc1 <- calc_auc_with_u(scores = c(0.1, 0.2, 0), labels = c(1, 0, 1))

  data(P10N10)
  sdat <- reformat_data(P10N10$scores, P10N10$labels, mode = "aucroc")
  auc2 <- calc_auc_with_u(sdat)
  auc3 <- calc_auc_with_u(scores = P10N10$scores, labels = P10N10$labels)

  expect_true(is(auc1, "uauc"))
  expect_true(is(auc2, "uauc"))
  expect_true(is(auc3, "uauc"))
})

test_that("'sdat' must be a 'sdat' object", {
  expect_err_msg <- function(sdat) {
    err_msg <- "Unrecognized class for .validate()"
    eval(bquote(expect_error(calc_auc_with_u(sdat), err_msg)))
  }

  expect_err_msg(list())
  expect_err_msg(data.frame())
})

test_that("calc_auc_with_u() can directly take scores and labels", {
  sdat <- reformat_data(c(0.1, 0.2, 0.2, 0), c(1, 0, 1, 1), mode = "aucroc")
  auc1 <- calc_auc_with_u(sdat)
  auc2 <- calc_auc_with_u(scores = c(0.1, 0.2, 0.2, 0),
                          labels = c(1, 0, 1, 1))

  expect_equal(auc1, auc2)
})

test_that("calc_auc_with_u() accepts arguments for reformat_data()", {
  err_msg <- "Invalid arguments: na.rm"
  expect_error(calc_auc_with_u(scores = c(0.1, 0.2, 0.2, 0),
                               labels = c(1, 0, 1, 1), na.rm = TRUE),
               err_msg)

  aucs <- calc_auc_with_u(scores = c(0.1, 0.2, 0),
                          labels = c(1, 0, 1),
                          na_worst = TRUE,
                          ties_method = "first",
                          keep_sdat = TRUE)

  expect_equal(.get_obj_arg(aucs, "sdat", "na_worst"), TRUE)
})

pl3_create_ms_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl3_create_sm_dat <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  list(scores = scores, labels = labels)
}

pl3_create_mm_dat <- function() {
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

  list(scores = scores, labels = labels)
}

test_that("ss test data", {
  scores <- c(1, 2, 3, 4)
  labels <- c(1, 0, 1, 0)

  curves <- evalmod(scores = scores, labels = labels)
  aucs <- auc(curves)
  aucs <- subset(aucs, curvetypes == "ROC")$aucs

  uaucs1 <- calc_auc_with_u(scores = scores, labels = labels)
  expect_equal(aucs, uaucs1$auc, tolerance = 1e-4)

  uaucs2 <- calc_auc_with_u(scores = scores, labels = labels,
                            ustat_method = "sort")
  expect_equal(aucs, uaucs2$auc, tolerance = 1e-4)

})

test_that("ms test data", {
  msdat <- pl3_create_ms_dat()
  scores <- msdat[["scores"]]
  labels <- msdat[["labels"]]

  curves <- evalmod(scores = scores, labels = labels)
  aucs <- auc(curves)
  aucs <- subset(aucs, curvetypes == "ROC")$aucs


  for (i in seq_along(aucs)) {
    s <- msdat[["scores"]][[i]]
    l <- msdat[["labels"]][[i]]

    uaucs1 <- calc_auc_with_u(scores = s, labels = l)
    expect_equal(aucs[i], uaucs1$auc, tolerance = 1e-4)

    uaucs2 <- calc_auc_with_u(scores = s, labels = l,
                              ustat_method = "sort")
    expect_equal(aucs[i], uaucs2$auc, tolerance = 1e-4)
  }
})

test_that("sm test data", {
  smdat <- pl3_create_sm_dat()
  scores <- smdat[["scores"]]
  labels <- smdat[["labels"]]

  curves <- evalmod(scores = scores, labels = labels)
  aucs <- auc(curves)
  aucs <- subset(aucs, curvetypes == "ROC")$aucs

  for (i in seq_along(aucs)) {
    s <- smdat[["scores"]][[i]]
    l <- smdat[["labels"]][[i]]

    uaucs1 <- calc_auc_with_u(scores = s, labels = l)
    expect_equal(aucs[i], uaucs1$auc, tolerance = 1e-4)

    uaucs2 <- calc_auc_with_u(scores = s, labels = l,
                              ustat_method = "sort")
    expect_equal(aucs[i], uaucs2$auc, tolerance = 1e-4)
  }


})

test_that("mm test data", {
  mmdat <- pl3_create_mm_dat()
  scores <- mmdat[["scores"]]
  labels <- mmdat[["labels"]]

  curves <- evalmod(scores = scores, labels = labels)
  aucs <- auc(curves)
  aucs <- subset(aucs, curvetypes == "ROC")$aucs


  for (i in seq_along(aucs)) {
    s <- mmdat[["scores"]][[i]]
    l <- mmdat[["labels"]][[i]]

    uaucs1 <- calc_auc_with_u(scores = s, labels = l)
    expect_equal(aucs[i], uaucs1$auc, tolerance = 1e-4)

    uaucs2 <- calc_auc_with_u(scores = s, labels = l,
                              ustat_method = "sort")
    expect_equal(aucs[i], uaucs2$auc, tolerance = 1e-4)
  }

})


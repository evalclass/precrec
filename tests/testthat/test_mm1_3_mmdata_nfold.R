#' @importFrom precrec

context("MM 1: Create mmdata for n-fold cross validation")
# Test mmdata(nfold_df = NULL, score_cols = NULL, lab_col = NULL,
#             fold_col = NULL, ...)

test_that("mmdata() accepts arguments for 'format_nfold'", {
  data(M2N50F5)
  dat1 = M2N50F5[M2N50F5$fold == 1, ]
  dat2 = M2N50F5[M2N50F5$fold == 2, ]
  dat3 = M2N50F5[M2N50F5$fold == 3, ]
  dat4 = M2N50F5[M2N50F5$fold == 4, ]
  dat5 = M2N50F5[M2N50F5$fold == 5, ]

  mdat <- mmdata(nfold_df = M2N50F5, score_cols = c(1, 2), lab_col = 3,
                 fold_col = 4, modnames = c("score1", "score2"),
                 dsids = 1:5)

  expect_equal(mdat[[1]]$scores, dat1$score1)
  expect_equal(mdat[[2]]$scores, dat2$score1)
  expect_equal(mdat[[3]]$scores, dat3$score1)
  expect_equal(mdat[[4]]$scores, dat4$score1)
  expect_equal(mdat[[5]]$scores, dat5$score1)

  expect_equal(mdat[[6]]$scores, dat1$score2)
  expect_equal(mdat[[7]]$scores, dat2$score2)
  expect_equal(mdat[[8]]$scores, dat3$score2)
  expect_equal(mdat[[9]]$scores, dat4$score2)
  expect_equal(mdat[[10]]$scores, dat5$score2)

  expect_equal(mdat[[1]]$labels, as.numeric(dat1$label))
  expect_equal(mdat[[2]]$labels, as.numeric(dat2$label))
  expect_equal(mdat[[3]]$labels, as.numeric(dat3$label))
  expect_equal(mdat[[4]]$labels, as.numeric(dat4$label))
  expect_equal(mdat[[5]]$labels, as.numeric(dat5$label))

  expect_equal(mdat[[6]]$labels, as.numeric(dat1$label))
  expect_equal(mdat[[7]]$labels, as.numeric(dat2$label))
  expect_equal(mdat[[8]]$labels, as.numeric(dat3$label))
  expect_equal(mdat[[9]]$labels, as.numeric(dat4$label))
  expect_equal(mdat[[10]]$labels, as.numeric(dat5$label))

})

test_that("mmdata() requires scores and labels if n-fold args are incomplete", {
  data(M2N50F5)

  expect_error(mmdata(nfold_df = M2N50F5,
                      score_cols = c(1, 2),
                      lab_col = 3,
                      fold_col = 4,
                      modnames = c("score1", "score2"),
                      dsids = 1:5), NA)

  err_msg <- "'scores' and/or 'lables' are missing"
  expect_error(mmdata(score_cols = c(1, 2),
                      lab_col = 3,
                      fold_col = 4,
                      modnames = c("score1", "score2"),
                      dsids = 1:5), err_msg)

  expect_error(mmdata(nfold_df = M2N50F5,
                      lab_col = 3,
                      fold_col = 4,
                      modnames = c("score1", "score2"),
                      dsids = 1:5), err_msg)
  expect_error(mmdata(nfold_df = M2N50F5,
                      score_cols = c(1, 2),
                      fold_col = 4,
                      modnames = c("score1", "score2"),
                      dsids = 1:5), err_msg)
  expect_error(mmdata(nfold_df = M2N50F5,
                      score_cols = c(1, 2),
                      lab_col = 3,
                      modnames = c("score1", "score2"),
                      dsids = 1:5), err_msg)

  err_msg <- "Invalid modnames and/or dsids"
  expect_error(mmdata(nfold_df = M2N50F5,
                      score_cols = c(1, 2),
                      lab_col = 3,
                      fold_col = 4,
                      dsids = 1:5), err_msg)
  expect_error(mmdata(nfold_df = M2N50F5,
                      score_cols = c(1, 2),
                      lab_col = 3,
                      fold_col = 4,
                      modnames = c("score1", "score2"), err_msg))


})


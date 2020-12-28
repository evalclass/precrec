#' @importFrom precrec

context("MM 4: Format n-fold cross validation data frame")
# Test format_nfold(nfold_df, score_cols, lab_col, fold_col)

test_that("format_nfold() converts a data frame to a list", {
  data(M2N50F5)

  lfold1 = format_nfold(M2N50F5, 1, 3, 4)
  expect_true(is.list(lfold1))
  expect_equal(length(lfold1), 2)

  expect_true(is.list(lfold1$scores))
  expect_equal(length(lfold1$scores), 5)

  expect_true(is.list(lfold1$labels))
  expect_equal(length(lfold1$labels), 5)

  lfold2 = format_nfold(M2N50F5, c(1, 2), 3, 4)
  expect_true(is.list(lfold2))
  expect_equal(length(lfold2), 2)

  expect_true(is.list(lfold2$scores))
  expect_equal(length(lfold2$scores), 10)

  expect_true(is.list(lfold2$labels))
  expect_equal(length(lfold2$labels), 10)

})

test_that("'nfold_df' should be a data frame", {
  expect_err_msg <- function(err_msg, dat) {
    eval(bquote(expect_error(format_nfold(dat, 1, 2, 3), err_msg)))
  }

  err_msg <- "nfold_df must be a data frame"
  expect_err_msg(err_msg, "")
  expect_err_msg(err_msg, c("", ""))
  expect_err_msg(err_msg, list())
  expect_err_msg(err_msg, array())
  expect_err_msg(err_msg, matrix())
})

test_that("'score_cols' should be valid column names", {
  data(M2N50F5)
  expect_err_msg <- function(err_msg, score_cols) {
    eval(bquote(expect_error(format_nfold(M2N50F5, score_cols, 3, 4), err_msg)))
  }

  err_msg <- "score_cols is not a numeric or integer vector"
  expect_err_msg(err_msg, NA)
  expect_err_msg(err_msg, TRUE)

  err_msg <- "Invalid score_cols"
  expect_err_msg(err_msg, "c")
  expect_err_msg(err_msg, 5)

  expect_err_msg(err_msg, c("score1", "score3"))
  expect_err_msg(err_msg, c(1, 5))

  expect_err_msg(err_msg, c("score1", "score2", "score3"))
  expect_err_msg(err_msg, c(1, 2, 5))

})

test_that("'lab_col' should be a valid column name", {
  data(M2N50F5)
  expect_err_msg <- function(err_msg, lab_col) {
    eval(bquote(expect_error(format_nfold(M2N50F5, 1, lab_col, 4), err_msg)))
  }

  err_msg <- "lab_col is not a number"
  expect_err_msg(err_msg, NA)
  expect_err_msg(err_msg, TRUE)
  expect_err_msg(err_msg, c("label", "label2"))
  expect_err_msg(err_msg, c(1, 5))
  expect_err_msg(err_msg, c("label", "label", "label2"))
  expect_err_msg(err_msg, c(1, 2, 5))

  err_msg <- "Invalid lab_col"
  expect_err_msg(err_msg, "c")
  expect_err_msg(err_msg, 5)

})

test_that("'fold_col' should be a valid column name", {
  data(M2N50F5)
  expect_err_msg <- function(err_msg, fold_col) {
    eval(bquote(expect_error(format_nfold(M2N50F5, 1, 3, fold_col), err_msg)))
  }

  err_msg <- "fold_col is not a number"
  expect_err_msg(err_msg, NA)
  expect_err_msg(err_msg, TRUE)
  expect_err_msg(err_msg, c("fold", "fold2"))
  expect_err_msg(err_msg, c(1, 5))
  expect_err_msg(err_msg, c("fold", "fold", "fold2"))
  expect_err_msg(err_msg, c(1, 2, 5))

  err_msg <- "Invalid fold_col"
  expect_err_msg(err_msg, "c")
  expect_err_msg(err_msg, 5)
})

test_that("format_nfold() correctly converts M2N50F5", {
  data(M2N50F5)
  dat1 = M2N50F5[M2N50F5$fold == 1, ]
  dat2 = M2N50F5[M2N50F5$fold == 2, ]
  dat3 = M2N50F5[M2N50F5$fold == 3, ]
  dat4 = M2N50F5[M2N50F5$fold == 4, ]
  dat5 = M2N50F5[M2N50F5$fold == 5, ]

  check_scores <- function(lscore) {
    expect_equal(lscore[[1]], dat1$score1)
    expect_equal(lscore[[2]], dat2$score1)
    expect_equal(lscore[[3]], dat3$score1)
    expect_equal(lscore[[4]], dat4$score1)
    expect_equal(lscore[[5]], dat5$score1)

    expect_equal(lscore[[6]], dat1$score2)
    expect_equal(lscore[[7]], dat2$score2)
    expect_equal(lscore[[8]], dat3$score2)
    expect_equal(lscore[[9]], dat4$score2)
    expect_equal(lscore[[10]], dat5$score2)
  }

  check_labels <- function(llabel) {
    expect_equal(llabel[[1]], as.integer(dat1$label))
    expect_equal(llabel[[2]], as.integer(dat2$label))
    expect_equal(llabel[[3]], as.integer(dat3$label))
    expect_equal(llabel[[4]], as.integer(dat4$label))
    expect_equal(llabel[[5]], as.integer(dat5$label))

    expect_equal(llabel[[6]], as.integer(dat1$label))
    expect_equal(llabel[[7]], as.integer(dat2$label))
    expect_equal(llabel[[8]], as.integer(dat3$label))
    expect_equal(llabel[[9]], as.integer(dat4$label))
    expect_equal(llabel[[10]], as.integer(dat5$label))
  }

  lfold1 = format_nfold(M2N50F5, c(1, 2), 3, 4)
  scores1 = lfold1$scores
  labels1 = lfold1$labels
  check_scores(scores1)
  check_labels(labels1)

  lfold2 = format_nfold(M2N50F5, c("score1", "score2"), "label", "fold")
  scores2 = lfold2$scores
  labels2 = lfold2$labels
  check_scores(scores2)
  check_labels(labels2)

})



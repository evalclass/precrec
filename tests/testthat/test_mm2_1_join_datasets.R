#' @importFrom precrec

context("MM 2: Join datasets")
# Test .join_datasets(..., efunc_vtype, efunc_nrow, byrow, chklen)

test_that(".join_datasets() returns a list", {
  cdat <- .join_datasets(0)

  expect_true(is.list(cdat))
})

test_that("'...' must be specified", {
  err_msg <- "No datasets specified"
  expect_error(.join_datasets(), err_msg)
})

test_that("'efunc_vtype' must be a function with 1 argument", {
  expect_error(.join_datasets(0, efunc_vtype = function(a1) TRUE), NA)

  expect_err_msg <- function(efunc_vtype) {
    err_msg <- "efunc_vtype must be a function with 1 argument"
    eval(bquote(expect_error(.join_datasets(0, efunc_vtype = efunc_vtype),
                             err_msg)))
  }

  expect_err_msg(function(a1, a2) print())
  expect_err_msg(function() print())
})

test_that("'efunc_nrow' must be a function with 2 arguments", {
  expect_error(.join_datasets(0, efunc_nrow = function(a1, a2) TRUE), NA)

  expect_err_msg <- function(efunc_nrow) {
    err_msg <- "efunc_nrow must be a function with 2 arguments"
    eval(bquote(expect_error(.join_datasets(0, efunc_nrow = efunc_nrow),
                             err_msg)))
  }

  expect_err_msg(function(a1) print())
  expect_err_msg(function() print())
})

test_that("'byrow' should be TRUE or FALSE", {
  expect_error(.join_datasets(0, byrow = TRUE), NA)
  expect_error(.join_datasets(0, byrow = FALSE), NA)

  expect_err_msg <- function(err_msg, byrow) {

    eval(bquote(expect_error(.join_datasets(0, byrow = byrow), err_msg)))
  }

  err_msg <- "byrow contains 1 missing values"
  expect_err_msg(err_msg, NA)

  err_msg <- "byrow is not a flag"
  expect_err_msg(err_msg, "T")
  expect_err_msg(err_msg, list(c(TRUE, FALSE)))
  expect_err_msg(err_msg, data.frame(c(TRUE, FALSE)))
  expect_err_msg(err_msg, array(c(TRUE, FALSE)))
  expect_err_msg(err_msg, matrix(c(TRUE, FALSE)))
})

test_that("'chklen' should be TRUE or FALSE", {
  expect_error(.join_datasets(0, chklen = TRUE), NA)
  expect_error(.join_datasets(0, chklen = FALSE), NA)

  expect_err_msg <- function(err_msg, chklen) {

    eval(bquote(expect_error(.join_datasets(0, chklen = chklen), err_msg)))
  }

  err_msg <- "chklen contains 1 missing values"
  expect_err_msg(err_msg, NA)

  err_msg <- "chklen is not a flag"
  expect_err_msg(err_msg, "T")
  expect_err_msg(err_msg, list(c(TRUE, FALSE)))
  expect_err_msg(err_msg, data.frame(c(TRUE, FALSE)))
  expect_err_msg(err_msg, array(c(TRUE, FALSE)))
  expect_err_msg(err_msg, matrix(c(TRUE, FALSE)))
})

test_that(".join_datasets() only accepts basic data types", {
  expect_err_msg <- function(dat) {
    err_msg <- "Cannot join this type of data"
    eval(bquote(expect_error(.join_datasets(dat), err_msg)))
  }

  expect_err_msg(NULL)

  cdat_vec <- .join_datasets(0)
  expect_true(is.list(cdat_vec))

  cdat_mat <- .join_datasets(matrix(0))
  expect_true(is.list(cdat_mat))

  cdat_arr <- .join_datasets(array(0))
  expect_true(is.list(cdat_arr))

  cdat_df <- .join_datasets(data.frame(0))
  expect_true(is.list(cdat_df))

  cdat_list <- .join_datasets(list(0))
  expect_true(is.list(cdat_list))

  cdat_factor <- .join_datasets(factor(0), efunc_vtype = function(v) NULL)
  expect_true(is.list(cdat_factor))
})

test_that(".join_datasets() accepts vectors", {
  vec1 <- c(0, 1)
  vec2 <- c(1, 2)
  vec3 <- c(1, 2)
  cdat_vec <- .join_datasets(vec1, vec2, vec3)

  expect_equal(length(cdat_vec), 3)
  expect_equal(cdat_vec[[1]], vec1)
  expect_equal(cdat_vec[[2]], vec2)
  expect_equal(cdat_vec[[3]], vec3)
})

test_that(".join_datasets() accepts matrices", {
  mat1 <- matrix(1:6, 3, 2)
  mat2 <- matrix(7:9, 3, 1)
  cdat_mat <- .join_datasets(mat1, mat2)

  expect_equal(length(cdat_mat), 3)
  expect_equal(cdat_mat[[1]], c(1, 2, 3))
  expect_equal(cdat_mat[[2]], c(4, 5, 6))
  expect_equal(cdat_mat[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts matrices as row vectors", {
  mat1 <- matrix(1:6, 2, 3, byrow = TRUE)
  mat2 <- matrix(7:9, 1, 3, byrow = TRUE)
  cdat_mat <- .join_datasets(mat1, mat2, byrow = TRUE)

  expect_equal(length(cdat_mat), 3)
  expect_equal(cdat_mat[[1]], c(1, 2, 3))
  expect_equal(cdat_mat[[2]], c(4, 5, 6))
  expect_equal(cdat_mat[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts 1d arrays", {
  arr1 <- array(1:3, c(3, 1))
  arr2 <- array(4:6, c(3, 1))
  arr3 <- array(7:9, c(3, 1))
  cdat_arr <- .join_datasets(arr1, arr2, arr3)

  expect_equal(length(cdat_arr), 3)
  expect_equal(cdat_arr[[1]], c(1, 2, 3))
  expect_equal(cdat_arr[[2]], c(4, 5, 6))
  expect_equal(cdat_arr[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts 2d arrays", {
  arr1 <- array(1:6, c(3, 2))
  arr2 <- array(7:9, c(3, 1))
  cdat_arr <- .join_datasets(arr1, arr2)

  expect_equal(length(cdat_arr), 3)
  expect_equal(cdat_arr[[1]], c(1, 2, 3))
  expect_equal(cdat_arr[[2]], c(4, 5, 6))
  expect_equal(cdat_arr[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts 1d or 2d arrays", {
  arr <- array(1:8, c(2, 2, 2))
  expect_error(.join_datasets(arr), "Array must be 1 or 2 dimensions")
})

test_that(".join_datasets() accepts 2d arrays as row vectors", {
  arr1 <- t(array(1:6, c(3, 2)))
  arr2 <- t(array(7:9, c(3, 1)))
  cdat_arr <- .join_datasets(arr1, arr2, byrow = TRUE)

  expect_equal(length(cdat_arr), 3)
  expect_equal(cdat_arr[[1]], c(1, 2, 3))
  expect_equal(cdat_arr[[2]], c(4, 5, 6))
  expect_equal(cdat_arr[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts data frames", {
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(c = 7:9)
  cdat_df <- .join_datasets(df1, df2)

  expect_equal(length(cdat_df), 3)
  expect_equal(cdat_df[[1]], c(1, 2, 3))
  expect_equal(cdat_df[[2]], c(4, 5, 6))
  expect_equal(cdat_df[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts lists", {
  list1 <- list(a = 1:3, b = 4:6)
  list2 <- list(c = 7:9)
  cdat_list <- .join_datasets(list1, list2)

  expect_equal(length(cdat_list), 3)
  expect_equal(cdat_list[[1]], c(1, 2, 3))
  expect_equal(cdat_list[[2]], c(4, 5, 6))
  expect_equal(cdat_list[[3]], c(7, 8, 9))
})

test_that(".join_datasets() accepts nested lists with two levels", {
  list1 <- list(a = 1:3)
  list2 <- list(b = 4:6, c = list(d = 7:9, e = 10:12))
  cdat_list <- .join_datasets(list1, list2)

  expect_equal(length(cdat_list), 4)
  expect_equal(cdat_list[[1]], c(1, 2, 3))
  expect_equal(cdat_list[[2]], c(4, 5, 6))
  expect_equal(cdat_list[[3]], c(7, 8, 9))
  expect_equal(cdat_list[[4]], c(10, 11, 12))
})

test_that(".join_datasets() accepts nested lists with multiple levels", {
  list1 <- list(a = 1:3)
  list2 <- list(b = 4:6, c = list(d = list(e = 7:9, f = 10:12), g = 13:15))
  cdat_list <- .join_datasets(list1, list2)

  expect_equal(length(cdat_list), 5)
  expect_equal(cdat_list[[1]], c(1, 2, 3))
  expect_equal(cdat_list[[2]], c(4, 5, 6))
  expect_equal(cdat_list[[3]], c(7, 8, 9))
  expect_equal(cdat_list[[4]], c(10, 11, 12))
  expect_equal(cdat_list[[5]], c(13, 14, 15))
})


test_that(".join_datasets() accepts various data types", {
  vec1 <- 1:3
  mat1 <- matrix(4:9, 3, 2)
  arr1 <- array(10:15, c(3, 2))
  df1 <- data.frame(a = 16:18, b = 19:21)
  list1 <- list(a = 22:24, b = 25:27)
  cdat_list <- .join_datasets(vec1, mat1, arr1, df1, list1)

  expect_equal(cdat_list[[1]], 1:3)
  expect_equal(cdat_list[[2]], 4:6)
  expect_equal(cdat_list[[3]], 7:9)
  expect_equal(cdat_list[[4]], 10:12)
  expect_equal(cdat_list[[5]], 13:15)
  expect_equal(cdat_list[[6]], 16:18)
  expect_equal(cdat_list[[7]], 19:21)
  expect_equal(cdat_list[[8]], 22:24)
  expect_equal(cdat_list[[9]], 25:27)
})

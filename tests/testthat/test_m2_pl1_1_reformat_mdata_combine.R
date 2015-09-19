context("M2 PL1: Combine datasets")
# Test .combine_datasets(arg:..., arg:efunc_vtype, arg:efunc_nrow,
#                        arg:byrow, arg:prefix)

test_that("arg:... must be specified", {
  err_msg <- "No datasets specified"
  expect_error(.combine_datasets(), err_msg)
})

test_that("arg:efunc_vtype must be a function with 1 argument", {
  expect_err_msg <- function(efunc_vtype) {
    err_msg <- "'efunc_vtype' must be a function with 1 argument"
    eval(bquote(expect_error(.combine_datasets(c(0),
                                               efunc_vtype = efunc_vtype),
                             err_msg)))
  }

  expect_err_msg(function(a1, a2) print())
  expect_err_msg(function() print())
})

test_that("arg:efunc_nrow must be a function with 2 arguments", {
  expect_err_msg <- function(efunc_nrow) {
    err_msg <- "'efunc_nrow' must be a function with 2 arguments"
    eval(bquote(expect_error(.combine_datasets(c(0),
                                               efunc_nrow = efunc_nrow),
                             err_msg)))
  }

  expect_err_msg(function(a1) print())
  expect_err_msg(function() print())
})

test_that("arg:byrow should be TRUE or FALSE", {
  expect_err_msg <- function(byrow) {
    err_msg <- "'byrow' should be one of FALSE, TRUE"
    eval(bquote(expect_error(.combine_datasets(c(0), byrow = byrow), err_msg)))
  }

  expect_err_msg("T")
  expect_err_msg(NA)
  expect_err_msg(list(c(TRUE, FALSE)))
  expect_err_msg(data.frame(c(TRUE, FALSE)))
  expect_err_msg(array(c(TRUE, FALSE)))
  expect_err_msg(matrix(c(TRUE, FALSE)))
})

test_that("arg:prefix must be a charactor vector", {
  expect_err_msg <- function(prefix) {
    err_msg <- "'prefix' must be a charactor vector with length 1"
    eval(bquote(expect_error(.combine_datasets(c(0), prefix = prefix),
                             err_msg)))
  }

  expect_err_msg(c("1", "2"))
  expect_err_msg(as.character())
  expect_err_msg(list("1"))
  expect_err_msg(data.frame("1"))
  expect_err_msg(matrix("1"))
})

test_that(".combine_datasets() returns a list", {
  cdat <- .combine_datasets(c(0))

  expect_true(is.list(cdat))
})

test_that(".combine_datasets() only accepts basic data types", {
  expect_err_msg <- function(dat) {
    err_msg <- "Incorrect type of data"
    eval(bquote(expect_error(.combine_datasets(dat), err_msg)))
  }

  expect_err_msg(NULL)

  cdat_vec <- .combine_datasets(c(0))
  expect_true(is.list(cdat_vec))

  cdat_mat <- .combine_datasets(matrix(0))
  expect_true(is.list(cdat_mat))

  cdat_arr <- .combine_datasets(array(0))
  expect_true(is.list(cdat_arr))

  cdat_df <- .combine_datasets(data.frame(0))
  expect_true(is.list(cdat_df))

  cdat_list <- .combine_datasets(list(0))
  expect_true(is.list(cdat_list))

  cdat_factor <- .combine_datasets(factor(0), efunc_vtype = function(v) NULL)
  expect_true(is.list(cdat_factor))
})

test_that(".combine_datasets() accepts vectors", {
  vec1 <- c(0, 1)
  vec2 <- c(1, 2)
  vec3 <- c(1, 2)
  cdat_vec <- .combine_datasets(vec1, vec2, vec3)

  expect_equal(length(cdat_vec), 3)
  expect_equal(cdat_vec[["m1"]], vec1)
  expect_equal(cdat_vec[["m2"]], vec2)
  expect_equal(cdat_vec[["m3"]], vec3)
})

test_that(".combine_datasets() accepts matrices", {
  mat1 = matrix(1:6, 3, 2)
  mat2 = matrix(7:9, 3, 1)
  cdat_mat <- .combine_datasets(mat1, mat2)

  expect_equal(length(cdat_mat), 3)
  expect_equal(cdat_mat[["m1"]], c(1, 2, 3))
  expect_equal(cdat_mat[["m2"]], c(4, 5, 6))
  expect_equal(cdat_mat[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts matrices as row vectors", {
  mat1 = matrix(1:6, 2, 3, byrow = TRUE)
  mat2 = matrix(7:9, 1, 3, byrow = TRUE)
  cdat_mat <- .combine_datasets(mat1, mat2, byrow = TRUE)

  expect_equal(length(cdat_mat), 3)
  expect_equal(cdat_mat[["m1"]], c(1, 2, 3))
  expect_equal(cdat_mat[["m2"]], c(4, 5, 6))
  expect_equal(cdat_mat[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts 1d arrays", {
  arr1 = array(1:3, c(3, 1))
  arr2 = array(4:6, c(3, 1))
  arr3 = array(7:9, c(3, 1))
  cdat_arr <- .combine_datasets(arr1, arr2, arr3)

  expect_equal(length(cdat_arr), 3)
  expect_equal(cdat_arr[["m1"]], c(1, 2, 3))
  expect_equal(cdat_arr[["m2"]], c(4, 5, 6))
  expect_equal(cdat_arr[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts 2d arrays", {
  arr1 = array(1:6, c(3, 2))
  arr2 = array(7:9, c(3, 1))
  cdat_arr <- .combine_datasets(arr1, arr2)

  expect_equal(length(cdat_arr), 3)
  expect_equal(cdat_arr[["m1"]], c(1, 2, 3))
  expect_equal(cdat_arr[["m2"]], c(4, 5, 6))
  expect_equal(cdat_arr[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts 1d or 2d arrays", {
  arr = array(1:8, c(2, 2, 2))
  expect_error(.combine_datasets(arr), "Array must be 1 or 2 dimensions")
})

test_that(".combine_datasets() accepts 2d arrays as row vectors", {
  arr1 = t(array(1:6, c(3, 2)))
  arr2 = t(array(7:9, c(3, 1)))
  cdat_arr <- .combine_datasets(arr1, arr2, byrow = TRUE)

  expect_equal(length(cdat_arr), 3)
  expect_equal(cdat_arr[["m1"]], c(1, 2, 3))
  expect_equal(cdat_arr[["m2"]], c(4, 5, 6))
  expect_equal(cdat_arr[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts data frames", {
  df1 = data.frame(a = 1:3, b = 4:6)
  df2 = data.frame(c = 7:9)
  cdat_df <- .combine_datasets(df1, df2)

  expect_equal(length(cdat_df), 3)
  expect_equal(cdat_df[["m1"]], c(1, 2, 3))
  expect_equal(cdat_df[["m2"]], c(4, 5, 6))
  expect_equal(cdat_df[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts lists", {
  list1 = list(a = 1:3, b = 4:6)
  list2 = list(c = 7:9)
  cdat_list <- .combine_datasets(list1, list2)

  expect_equal(length(cdat_list), 3)
  expect_equal(cdat_list[["m1"]], c(1, 2, 3))
  expect_equal(cdat_list[["m2"]], c(4, 5, 6))
  expect_equal(cdat_list[["m3"]], c(7, 8, 9))
})

test_that(".combine_datasets() accepts various data types", {
  vec1 <- 1:3
  mat1 = matrix(4:9, 3, 2)
  arr1 = array(10:15, c(3, 2))
  df1 = data.frame(a = 16:18, b = 19:21)
  list1 = list(a = 22:24, b = 25:27)
  cdat_list <- .combine_datasets(vec1, mat1, arr1, df1, list1)

  expect_equal(cdat_list[["m1"]], 1:3)
  expect_equal(cdat_list[["m2"]], 4:6)
  expect_equal(cdat_list[["m3"]], 7:9)
  expect_equal(cdat_list[["m4"]], 10:12)
  expect_equal(cdat_list[["m5"]], 13:15)
  expect_equal(cdat_list[["m6"]], 16:18)
  expect_equal(cdat_list[["m7"]], 19:21)
  expect_equal(cdat_list[["m8"]], 22:24)
  expect_equal(cdat_list[["m9"]], 25:27)
})

# ETC: Shared helpers
# Test .map(x, f), .map_dbl(x, f), .map_int(x, f), .map_chr(x, f),
#      .map_lgl(x, f), .map_idx(x, f), .keep(x, p), .flatten(x)

test_that(".map() returns a list of the same length", {
  expect_equal(.map(1:3, function(i) i * 2), list(2, 4, 6))
  expect_equal(.map(list(), identity), list())
  expect_type(.map(1:2, identity), "list")
})

test_that(".map() passes extra arguments through", {
  expect_equal(.map(list(c(1, NA)), sum, na.rm = TRUE), list(1))
})

test_that("the typed helpers return an atomic vector of that type", {
  expect_type(.map_dbl(1:3, function(i) i + 0.5), "double")
  expect_type(.map_int(list(1:2, 1:5), length), "integer")
  expect_type(.map_chr(1:2, function(i) "a"), "character")
  expect_type(.map_lgl(1:3, function(i) i > 1), "logical")
  expect_equal(.map_int(list(1:2, 1:5), length), c(2L, 5L))
})

test_that("the typed helpers reject a result of the wrong type or length", {
  expect_error(.map_dbl(1:2, function(i) "a"))
  expect_error(.map_int(1:2, function(i) c(1L, 2L)))
})

test_that(".map_idx() maps over positions, not elements", {
  x <- list("a", "b", "c")
  expect_equal(.map_idx(x, function(i) i), list(1L, 2L, 3L))
  expect_equal(.map_idx(list(), function(i) i), list())
})

test_that(".map_idx() drops the names its input carries", {
  # The callers store the result as an S3 object whose class items are
  # positional, so a stray name from the input would leak into it.
  expect_null(names(.map_idx(list(a = 1, b = 2), function(i) i)))
})

test_that(".keep() selects the elements the predicate accepts", {
  expect_equal(.keep(1:5, function(i) i %% 2 == 0), c(2L, 4L))
  expect_equal(.keep(list(), function(i) TRUE), list())
  expect_equal(.keep(1:3, function(i) FALSE), integer(0))
})

test_that(".flatten() removes exactly one level of nesting", {
  expect_equal(
    .flatten(list(list(1, 2), list(3))),
    list(1, 2, 3)
  )
  expect_equal(
    .flatten(list(list(list(1)))),
    list(list(1))
  )
  expect_null(.flatten(list()))
})

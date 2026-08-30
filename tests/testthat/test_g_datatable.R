# DT: as.data.table for curves, points and aucroc objects
# Test as.data.table(x, ...)

dt_create_mscurves <- function() {
  s1 <- c(1, 2, 3, 4)
  s2 <- c(5, 6, 7, 8)
  s3 <- c(2, 4, 6, 8)
  scores <- join_scores(s1, s2, s3)

  l1 <- c(1, 0, 1, 1)
  l2 <- c(0, 1, 1, 1)
  l3 <- c(1, 1, 0, 1)
  labels <- join_labels(l1, l2, l3)

  evalmod(mmdata(scores, labels))
}

dt_create_mmcurves <- function(raw_curves = FALSE) {
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

  mdat <- mmdata(scores, labels,
    modnames = c("m1", "m2"), dsids = c(1, 2),
    expd_first = "modnames"
  )
  evalmod(mdat, raw_curves = raw_curves)
}

test_that("as.data.table returns a data.table", {
  data(P10N10)
  objs <- list(
    sscurves = evalmod(scores = P10N10$scores, labels = P10N10$labels),
    sspoints = evalmod(
      scores = P10N10$scores, labels = P10N10$labels,
      mode = "basic"
    ),
    aucroc = evalmod(
      scores = P10N10$scores, labels = P10N10$labels,
      mode = "aucroc"
    ),
    mscurves = dt_create_mscurves(),
    mmcurves = dt_create_mmcurves()
  )

  for (nm in names(objs)) {
    dt <- data.table::as.data.table(objs[[nm]])
    expect_true(data.table::is.data.table(dt), info = nm)
    expect_true(is.data.frame(dt), info = nm)
  }
})

test_that("as.data.table holds the same content as as.data.frame", {
  data(P10N10)
  objs <- list(
    sscurves = evalmod(scores = P10N10$scores, labels = P10N10$labels),
    sspoints = evalmod(
      scores = P10N10$scores, labels = P10N10$labels,
      mode = "basic"
    ),
    aucroc = evalmod(
      scores = P10N10$scores, labels = P10N10$labels,
      mode = "aucroc"
    ),
    mscurves = dt_create_mscurves(),
    mmcurves = dt_create_mmcurves()
  )

  for (nm in names(objs)) {
    dt <- data.table::as.data.table(objs[[nm]])
    df <- as.data.frame(objs[[nm]])
    expect_equal(as.data.frame(dt), df, info = nm)
  }
})

test_that("as.data.table follows raw_curves", {
  curves <- dt_create_mmcurves(raw_curves = TRUE)

  dt_raw <- data.table::as.data.table(curves, raw_curves = TRUE)
  dt_avg <- data.table::as.data.table(curves, raw_curves = FALSE)

  expect_equal(as.data.frame(dt_raw), as.data.frame(curves,
    raw_curves = TRUE
  ))
  expect_equal(as.data.frame(dt_avg), as.data.frame(curves,
    raw_curves = FALSE
  ))
  expect_true(nrow(dt_raw) != nrow(dt_avg))
})

test_that("the returned data.table does not alias the stored table", {
  data(P10N10)
  aucroc <- evalmod(
    scores = P10N10$scores, labels = P10N10$labels,
    mode = "aucroc"
  )

  before <- as.data.frame(aucroc)[["aucs"]]
  dt <- data.table::as.data.table(aucroc)
  data.table::set(dt, j = "aucs", value = -1)

  expect_equal(as.data.frame(aucroc)[["aucs"]], before)
})

library(precrec)

context("PL 6: Calculate average curves")
# Test calc_avg_rocprc(epoints, modnames, uniq_modnames, ci_alpha)

pl6_create_mdat_sm <- function() {
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

pl6_create_mdat_mm <- function() {
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

pl6_calc_avg_rocprc <- function(mdat, curvetype = "roc", ci_alpha = 0.05,
                                x_bins = 10) {
  plfunc <- function(s) {
    cdat <- create_confmats(mdat[[s]])
    pevals <- calc_measures(cdat)
    curves <- create_curves(pevals, x_bins = x_bins)
  }
  lcurves <- lapply(seq_along(mdat), plfunc)

  mc <- lapply(seq_along(lcurves), function(s) lcurves[[s]][[curvetype]])

  modnames <- attr(mdat, "data_info")[["modnames"]]
  uniq_modnames <- attr(mdat, "uniq_modnames")
  avgcurves <- calc_avg_rocprc(mc, modnames, uniq_modnames, ci_alpha, x_bins)
}

test_that("calc_avg_rocprc() returns 'avgcurves'", {

  for (ct in c("roc", "prc")) {
    mdat1 <- pl6_create_mdat_sm()
    avg1 <- pl6_calc_avg_rocprc(mdat1, ct)
    expect_true(is(avg1, "avgcurves"))

    mdat2 <- pl6_create_mdat_mm()
    avg2 <- pl6_calc_avg_rocprc(mdat2, ct)
    expect_true(is(avg2, "avgcurves"))
  }

})


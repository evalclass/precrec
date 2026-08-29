# vdiffr comparisons are skipped on CI: rendered SVG output varies across
# platforms and ggplot2 versions, and the baseline .svg files are not
# committed (see .gitignore). Locally, run the visual comparison.
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}

check_ggplot_fig <- function(ptitle, p) {
  if (on_ci()) {
    testthat::expect_true(is(p, "ggplot"))
  } else {
    vdiffr::expect_doppelganger(ptitle, p)
  }
}

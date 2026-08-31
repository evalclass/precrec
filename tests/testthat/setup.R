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

# Structural assertions for ggplot output.
#
# vdiffr compares rendered SVG byte for byte, so a baseline belongs to the
# machine that produced it - the font "sans" resolves differently per platform
# and svglite bakes the measured text widths into the file. That is why the
# baselines are gitignored and the comparison is skipped on CI, and it leaves
# the plot code with no assertion that survives crossing machines. These
# helpers read structure off the plot object instead of rendering it, so they
# behave identically everywhere.

# ggplot2 4.0 moved the labels behind an accessor; 3.x keeps them in the list.
# A patchwork rejects a non-numeric [[, so the fallback reads the field with $.
gg_all_labs <- function(p) {
  if (utils::packageVersion("ggplot2") >= "4.0.0") {
    getExportedValue("ggplot2", "get_labs")(p)
  } else {
    p$labels
  }
}

# A multi-panel autoplot is a patchwork, which holds every panel but the last
# in $patches$plots and is itself the last one.
gg_panels <- function(p) {
  patches <- p$patches$plots
  if (is.null(patches)) {
    list(p)
  } else {
    c(patches, list(p))
  }
}

gg_labs <- function(p, which = "title") {
  vapply(gg_panels(p), function(panel) {
    lab <- gg_all_labs(panel)[[which]]
    if (is.null(lab)) NA_character_ else as.character(lab)
  }, character(1))
}

# Regression snapshots of plot output.
#
# What these tests need to know is whether a plot still draws the same thing
# after a change, and that question is about the data ggplot2 computes, not
# about pixels. Rendering the plot to compare it - which is what vdiffr did
# here until 0.15.0 - answered it only after dragging in the local font
# metrics and the ggplot2 layout engine, so a baseline belonged to one
# machine, could not be committed, and reported a stale baseline and a real
# regression in the same words.
#
# So record the plot instead: its panels, their titles and axis labels, and
# the data behind every layer. That is text, it is identical on every
# machine, and it lives in tests/testthat/_snaps/ under version control, so
# the comparison runs for everyone and on CI.

# Aesthetics ggplot2 computes from its own defaults (colour, size, shape,
# alpha) are the theme's business and move when ggplot2 does. The data
# precrec puts on the plot is x, y and the grouping.
plot_layer_digest <- function(panel) {
  n_layers <- length(panel$layers)
  if (n_layers == 0) {
    return("    (no layers)")
  }

  vapply(seq_len(n_layers), function(i) {
    dat <- ggplot2::layer_data(panel, i)
    geom <- class(panel$layers[[i]]$geom)[1]
    if (nrow(dat) == 0) {
      return(sprintf("    layer %d: %s  n=0", i, geom))
    }

    # Long curves are sampled at fixed positions so the snapshot stays
    # readable; the summary lines below still cover every row.
    idx <- unique(round(seq(1, nrow(dat), length.out = min(nrow(dat), 9))))
    axis <- function(nm) {
      v <- dat[[nm]]
      sprintf(
        "      %s: min=%s max=%s mean=%s | %s", nm,
        fmt_num(min(v, na.rm = TRUE)), fmt_num(max(v, na.rm = TRUE)),
        fmt_num(mean(v, na.rm = TRUE)),
        paste(fmt_num(v[idx]), collapse = " ")
      )
    }
    # A reference line carries its position in slope/intercept rather than
    # in x/y, and where those lines sit is exactly what changed when the
    # precision-recall baseline became conditional on the prevalence.
    aes_cols <- intersect(
      c("x", "y", "slope", "intercept", "yintercept", "xintercept"),
      names(dat)
    )
    n_na <- if ("y" %in% names(dat)) sum(is.na(dat$y)) else 0L
    paste(
      c(
        sprintf(
          "    layer %d: %s  n=%d  groups=%d  n_na_y=%d",
          i, geom, nrow(dat), length(unique(dat$group)), n_na
        ),
        vapply(aes_cols, axis, character(1), USE.NAMES = FALSE)
      ),
      collapse = "\n"
    )
  }, character(1))
}

# Eight significant digits is far more than any of these measures resolve,
# and it keeps the text clear of last-bit noise.
fmt_num <- function(x) {
  ifelse(is.na(x), "NA", formatC(signif(x, 8), format = "g", width = 1))
}

plot_digest <- function(p) {
  panels <- gg_panels(p)
  out <- sprintf("plot: %d panel(s)", length(panels))

  for (i in seq_along(panels)) {
    labs <- gg_all_labs(panels[[i]])
    lab <- function(nm) {
      if (is.null(labs[[nm]])) "-" else as.character(labs[[nm]])
    }
    out <- c(
      out,
      sprintf("  panel %d", i),
      sprintf("    title: %s", lab("title")),
      sprintf("    x: %s", lab("x")),
      sprintf("    y: %s", lab("y")),
      plot_layer_digest(panels[[i]])
    )
  }

  out
}

# Named text snapshots, one file per plot, the way vdiffr named its SVGs -
# so a failure points at the plot rather than at an anonymous index.
check_ggplot_fig <- function(ptitle, p) {
  testthat::expect_true(inherits(p, "ggplot"))

  path <- file.path(tempdir(), paste0(ptitle, ".txt"))
  writeLines(plot_digest(p), path)
  testthat::expect_snapshot_file(
    path,
    name = paste0(ptitle, ".txt"),
    compare = testthat::compare_file_text
  )
}

# Reading structure off a plot object.
#
# Used both by the snapshot digest above and by the assertions in
# test_etc_utils_autoplot.R, neither of which renders anything.

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

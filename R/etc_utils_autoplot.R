#
# .grid_arrange_shared_legend
#
#   Modified version of grid_arrange_shared_legend from RPubs
#   URL of the original version:
#     http://rpubs.com/sjackman/grid_arrange_shared_legend
#
.grid_arrange_shared_legend <- function(..., main_ncol = 2) {
  plots <- list(...)

  g <- ggplot2::ggplotGrob(plots[[1]]
                           + ggplot2::theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)

  fncol <- function (...) gridExtra::arrangeGrob(..., ncol = main_ncol)
  fnolegend <- function(x) x + ggplot2::theme(legend.position = "none")

  gridExtra::arrangeGrob(
    do.call(fncol, lapply(plots, fnolegend)),
    legend,
    heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight),
    ncol = 1)
}

#
# Load ggplot2
#
.load_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste("This function should not be called directly,",
               "and ggplot2 is needed to work.",
               "Please install it."),
         call. = FALSE)
  }
}

#
# Load grid
#
.load_grid <- function() {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("grid needed for this function to work. Please install it.",
         call. = FALSE)
  }
}

#
# Load gridExtra
#
.load_gridExtra <- function() {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra needed for this function to work. Please install it.",
         call. = FALSE)
  }
}

#
# Prepare autoplot and return a data frame
#
.prepare_autoplot <- function(object, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(object)

  # === Prepare a data frame for ggplot2 ===
  df <- ggplot2::fortify(object, ...)
}

#
# Combine ROC and Precision-Recall plots
#
.combine_roc_prc <- function(p_roc, p_prc, show_legend, ret_grob) {
  .load_grid()
  .load_gridExtra()

  if (show_legend) {
    grobframe <- .grid_arrange_shared_legend(p_roc, p_prc)
  } else {
    grobframe <- gridExtra::arrangeGrob(p_roc, p_prc, ncol = 2)
  }

  if (ret_grob) {
    grobframe
  } else {
    plot.new()
    grid::grid.draw(grobframe)
  }
}

#
# Geom basic
#
.geom_basic <- function(p, main, xlab, ylab, show_legend) {
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::ggtitle(main)
  p <- p + ggplot2::xlab(xlab)
  p <- p + ggplot2::ylab(ylab)

  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

#
# Make main title
#
.make_rocprc_title <- function(object, pt) {
  np <- attr(object, "np")
  nn <- attr(object, "nn")

  main <- paste0(pt, " - P: ", np, ", N: ", nn)
}

#
# Geom basic for ROC
#
.geom_basic_roc <- function(p, object, show_legend = TRUE) {
  main <- .make_rocprc_title(object, "ROC")

  p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey",
                                linetype = 3)
  p <- p + ggplot2::coord_fixed(ratio = 1)

  p <- .geom_basic(p, main, "1 - Specificity", "Sensitivity", show_legend)

  p
}

#
# Geom_line for Precision-Recall
#
.geom_basic_prc <- function(p, object, show_legend = TRUE) {
  main <- .make_rocprc_title(object, "Precision-Recall")
  np <- attr(object, "np")
  nn <- attr(object, "nn")

  p <- p + ggplot2::geom_hline(yintercept = np / (np + nn), colour = "grey",
                               linetype = 3)
  p <- p + ggplot2::scale_y_continuous(limits = c(0.0, 1.0))
  p <- p + ggplot2::coord_fixed(ratio = 1)

  p <- .geom_basic(p, main, "Recall", "Precision", show_legend)

  p
}

#
# Check curve types
#
.check_curvetype <- function(curvetype) {
  if (!is.atomic(curvetype) || !is.character(curvetype)
      || length(curvetype) > 2
      || length(setdiff(curvetype, c("ROC", "PRC"))) != 0) {
    stop("Invalid 'curvetype' value")
  }
}

#
# Check ret_grob
#
.check_ret_grob <- function(ret_grob) {
  assertthat::assert_that(is.atomic(ret_grob),
                          assertthat::is.flag(ret_grob),
                          assertthat::noNA(ret_grob))
}

#
# Check show_legend
#
.check_show_legend <- function(show_legend) {
  assertthat::assert_that(is.atomic(show_legend),
                          assertthat::is.flag(show_legend),
                          assertthat::noNA(show_legend))
}

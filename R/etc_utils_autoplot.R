#
# Modified version of grid_arrange_shared_legend from RPubs
# URL of the orignal version:
#   http://rpubs.com/sjackman/grid_arrange_shared_legend
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
# Load pryr
#
.load_pryr <- function() {
  if (!requireNamespace("pryr", quietly = TRUE)) {
    stop("pryr needed for this function to work. Please install it.",
         call. = FALSE)
  }
}

#
# Prepare autoplot and return a data frame
#
.prepare_autoplot <- function(object) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(object)

  # === Prepare a data frame for ggplot2 ===
  df <- ggplot2::fortify(object)
}

#
# Geom basic
#
.geom_basic <- function(p, main, xlab, ylab) {
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::ggtitle(main)
  p <- p + ggplot2::xlab(xlab)
  p <- p + ggplot2::ylab(ylab)

  p
}

#
# Geom_line wrapper
#
.geom_line_wrapper <- function(p, main, xlab, ylab) {
  p <- p + ggplot2::geom_line()
  p <- .geom_basic(p, main, xlab, ylab)

  p
}

#
# Geom_line wrapper for ROC and Precision-Recall
#
.geom_rocprc_line_wrapper <- function(p, main, xlab, ylab) {
  p <- .geom_line_wrapper(p, main, xlab, ylab)
  p <- p + ggplot2::coord_fixed(ratio = 1)
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())

  p
}

#
# Make main titile
#
.make_rocprc_title <- function(object, pt) {
  np <- attr(object, "np")
  nn <- attr(object, "nn")

  main <- paste0(pt, " - P: ", np, ", N: ", nn)
}

#
# Geom_line for ROC
#
.geom_roc_line_wrapper <- function(p, object, show_legend = TRUE) {
  main <- .make_rocprc_title(object, "ROC")

  p <- .geom_rocprc_line_wrapper(p, main, "1 - Specificity", "Sensitivity")
  p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey",
                                linetype = 3)
  p <- p + ggplot2::coord_fixed(ratio = 1)
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

#
# Geom_line for Precision-Recall
#
.geom_prc_line_wrapper <- function(p, object, show_legend = TRUE) {
  main <- .make_rocprc_title(object, "Precision-Recall")
  np <- attr(object, "np")
  nn <- attr(object, "nn")

  p <- .geom_rocprc_line_wrapper(p, main, "Recall", "Precision")
  p <- p + ggplot2::geom_hline(yintercept = np / (np + nn), colour = "grey",
                               linetype = 3)
  p <- p + ggplot2::scale_y_continuous(limits = c(0.0, 1.0))
  p <- p + ggplot2::coord_fixed(ratio = 1)
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

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

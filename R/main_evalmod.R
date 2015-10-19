#' Evaluate a single model with multiple datasets
#'
#' The \code{evalmod} function takes predicted scores and binary lables
#'   and calculates ROC and Precision-Recall curves.
#'
#' @param mdat An \code{mdat} object created by \code{\link{mmdata}}.
#'   The following arguments are ignored when \code{mdat} is specified.
#'   \itemize{
#'     \item \code{scores}
#'     \item \code{labels}
#'     \item \code{modnames}
#'     \item \code{dsids}
#'     \item \code{na_worst}
#'     \item \code{ties_method}
#'   }
#'
#' @param x_bins A numeric value with the range (0, 1] to specifiy
#'   an interval of the evaluation values on the x-axis.
#'   No interpolation between two points is performed when it is set to 1.
#'
#' @param scores A numeric data of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list.
#'
#' @param labels A numeric or factor data of observed labels.
#'   It can be a vector, a matrix, an array, a data frame, or a list.
#'
#' @param modnames A character vector as the names
#'   of the models/classifiers.
#'
#' @param dsids A numeric vector as dataset IDs.
#'
#' @param na_worst A boolean value for controlling the treatment of NAs
#'   in the scores.
#'   \describe{
#'     \item{TRUE}{NAs are treated as the highest score}
#'     \item{FALSE}{NAs are treated as the lowest score}
#'   }
#'
#' @param ties_method A string for controlling tied scores.
#'   Ignored if mdat is set.
#'   \describe{
#'     \item{"equiv"}{Ties are equivalently ranked}
#'     \item{"random"}{Ties are ranked in an incresing order as appeared}
#'     \item{"first"}{ Ties are ranked in random order}
#'   }
#'
#' @return The \code{evalmods_m} function returns an \code{smcurves} S3 object
#'   that contains ROC and Precision-Recall curves.
#'
#' @seealso \code{\link{plot.smcurves}}, \code{\link{autoplot.smcurves}},
#'   and \code{\link{fortify.smcurves}} for plotting curves.
#'   \code{\link{join_scores}}, \code{\link{join_scores}},
#'   and \code{\link{join_labels}} for formatting input data.
#'
#' @examples
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(10, 100, 100, "all")
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                modnames = samps[["modnames"]],
#'                dsids = samps[["dsids"]])
#'
#' ## Generate an mscurve object
#' curves1 <- evalmods_m(mdat)
#'
#' ## Directly specifiy scores and labels
#' curves2 <- evalmod(scores = samps[["scores"]], labels = samps[["labels"]],
#'                    modnames = samps[["modnames"]],
#'                    dsids = samps[["dsids"]])
#'
#' ## Print the summary
#' curves2
#'
#' ## Plot Precision-Recall
#' plot(curves2, "PRC")
#'
#' ## Set x_bins = 10
#' curves3 <- evalmods_m(mdat, x_bins = 10)
#' plot(curves3, "PRC")
#'
#' ## No interpolation of Precsion-Recall curve
#' curves4 <- evalmods_m(mdat, x_bins = NULL)
#' plot(curves4, "PRC")
#'
#' @export
evalmod <- function(mdat, scores = NULL, labels = NULL,
                    modnames = NULL, dsids = NULL, posclass = NULL,
                    na_worst = TRUE, ties_method = "equiv",
                    calc_avg = TRUE, ci_alpha = 0.05, all_curves = FALSE,
                    x_bins = 1000, orig_points = TRUE) {

  .validate_evalmod_args(modnames, dsids, posclass, na_worst, ties_method,
                         calc_avg, ci_alpha, all_curves, x_bins, orig_points)

  if (!missing(mdat)) {
    .validate(mdat)
  } else {
    mdat <- mmdata(scores, labels, modnames = modnames,
                   dsids = dsids, posclass = posclass, na_worst = na_worst,
                   ties_method = ties_method)
  }

  pl_main(mdat, model_type = "multiple", data_type = "multiple",
          x_bins = x_bins, calc_avg = calc_avg, ci_alpha = ci_alpha)

}

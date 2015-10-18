#' Evaluate multiple models
#'
#' The \code{evalmods} function takes predicted scores and binary lables
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
#' @param x_interval A numeric value with the range (0, 1] to specifiy
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
#' @return The \code{evalmods} function returns an \code{mscurves} S3 object
#'   that contains ROC and Precision-Recall curves.
#'
#' @seealso \code{\link{plot.mscurves}}, \code{\link{autoplot.mscurves}},
#'   and \code{\link{fortify.mscurves}} for plotting curves.
#'   \code{\link{join_scores}}, \code{\link{join_scores}},
#'   and \code{\link{join_labels}} for formatting input data.
#'
#' @examples
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(1, 100, 100, "all")
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                modnames = samps[["modnames"]],
#'                dsids = samps[["dsids"]])
#'
#' ## Generate an mscurve object
#' curves1 <- evalmods(mdat)
#'
#' ## Directly specifiy scores and labels
#' curves2 <- evalmods(scores = samps[["scores"]], labels = samps[["labels"]],
#'                      modnames = samps[["modnames"]])
#'
#' ## Print the summary
#' curves2
#'
#' ## Plot Precision-Recall
#' plot(curves2, "PRC")
#'
#' ## Set x_interval = 0.1
#' curves3 <- evalmods(mdat, x_interval = 0.1)
#' plot(curves3, "PRC")
#'
#' ## No interpolation of Precsion-Recall curve
#' curves4 <- evalmods(mdat, x_interval = NULL)
#' plot(curves4, "PRC")
#'
#' @export
evalmods_s <- function(mdat, x_interval = 0.001, scores = NULL, labels = NULL,
                       modnames = NULL, dsids = NULL, na_worst = TRUE,
                       ties_method = "equiv") {

  .validate_evalmods_s_args(x_interval, modnames, dsids, na_worst,
                            ties_method)

  if (!missing(mdat)) {
    .validate(mdat)
  } else {
    mdat <- mmdata(scores, labels, modnames = modnames,
                   dsids = dsids, na_worst = na_worst,
                   ties_method = ties_method)
  }

  pl_main(mdat, model_type = "multiple", data_type = "single",
          x_interval = x_interval)

}

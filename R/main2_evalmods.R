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
#'     \item \code{model_names}
#'     \item \code{setids}
#'     \item \code{na.last}
#'     \item \code{ties.method}
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
#' @param model_names A character vector as the names
#'   of the models/classifiers.
#'
#' @param setids A numeric vector as dataset IDs.
#'
#' @param na.last A boolean value for controlling the treatment of NAs
#'   in the scores.
#'   \describe{
#'     \item{TRUE}{NAs are treated as the highest score}
#'     \item{FALSE}{NAs are treated as the lowest score}
#'   }
#'
#' @param ties.method A string for controlling tied scores.
#'   Ignored if mdat is set.
#'   \describe{
#'     \item{"equiv"}{Ties are equivalently ranked}
#'     \item{"random"}{Ties are ranked in an incresing order as appeared}
#'     \item{"first"}{ Ties are ranked in random order}
#'   }
#'
#' @param levels A character vector to overide the levels of the factor for
#'   the labels.
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
#'                model_names = samps[["model_names"]],
#'                setids = samps[["setids"]])
#'
#' ## Generate an mscurve object
#' curves1 <- evalmods(mdat)
#'
#' ## Directly specifiy scores and labels
#' curves2 <- evalmods(scores = samps[["scores"]], labels = samps[["labels"]],
#'                      model_names = samps[["model_names"]])
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
evalmods <- function(mdat, x_interval = 0.001, scores = NULL, labels = NULL,
                     model_names = NULL, setids = NULL, na.last = TRUE,
                     ties.method = "equiv",
                     levels = c("negative", "positive")) {

  .validate_evalmods_args(x_interval, model_names, setids, na.last,
                          ties.method, levels)

  if (!missing(mdat)) {
    .validate(mdat)
  } else {
    mdat <- mmdata(scores, labels, model_names = model_names,
                   setids = setids, na.last = na.last,
                   ties.method = ties.method, levels = levels)
  }

  pl_main(mdat, model_type = "multiple", data_type = "single",
          x_interval = x_interval)

}

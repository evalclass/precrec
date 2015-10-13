#' Evaluate a single model
#'
#' The \code{evalmod} function takes predicted scores and binary lables
#'   and calculates ROC and Precision-Recall curves.
#'
#' @param mdat An \code{mdat} object created by \code{\link{mmdata}}.
#'   The following arguments are ignored when \code{mdat} is specified.
#'   \itemize{
#'     \item \code{scores}
#'     \item \code{labels}
#'     \item \code{model_name}
#'     \item \code{setid}
#'     \item \code{na.last}
#'     \item \code{ties.method}
#'   }
#'
#' @param x_interval A numeric value with the range (0, 1] to specifiy
#'   an interval of the evaluation values on the x-axis.
#'   No interpolation between two points is performed when it is set to 1.
#'
#' @param scores A numeric vector of predicted scores.
#'
#' @param labels A numeric vector or a factor of observed labels.
#'
#' @param model_name A string as the name of the model/classifier.
#'
#' @param setid A numeric value as a dataset ID.
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
#' @seealso \code{\link{plot.sscurves}}, \code{\link{autoplot.sscurves}},
#'   and \code{\link{fortify.sscurves}} for plotting curves.
#'   \code{\link{join_scores}}, \code{\link{join_scores}},
#'   and \code{\link{join_labels}} for formatting input data.
#'
#' @return The \code{evalmod} function returns an \code{sscurves} S3 object
#'   that contains ROC and Precision-Recall curves.
#'
#' @examples
#'
#' ## Load a dataset with 10 positives and 10 negatives
#' data(P10N10)
#'
#' ## Generate an sscurve object
#' mdat <- mmdata(P10N10$scores, P10N10$labels)
#' curves1 <- evalmod(mdat)
#'
#' ## Directly specifiy scores and labels
#' curves2 <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
#'
#' ## Print the summary
#' curves2
#'
#' ## Plot Precision-Recall
#' plot(curves2, "PRC")
#'
#' ## Set x_interval = 0.1
#' curves3 <- evalmod(mdat, x_interval = 0.1)
#' plot(curves3, "PRC")
#'
#' ## No interpolation of Precsion-Recall curve
#' curves4 <- evalmod(mdat, x_interval = NULL)
#' plot(curves4, "PRC")
#'
#' @export
evalmod <- function(mdat, x_interval = 0.001, scores = NULL, labels = NULL,
                    model_name = as.character(NA), setid = 1L,
                    na.last = TRUE, ties.method = "equiv",
                    levels = c("negative", "positive")) {

  .validate_evalmod_args(x_interval, model_name, setid, na.last,
                         ties.method, levels)

  if (!missing(mdat)) {
    .validate(mdat)
  } else {
    mdat <- mmdata(scores, labels, model_names = model_name, setids = setid,
                   na.last = na.last, ties.method = ties.method, levels = levels)
  }

  pl_main(mdat, model_type = "single", data_type = "single",
          x_interval =x_interval)
}

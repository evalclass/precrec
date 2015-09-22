#' Evaluate a single model
#'
#' \code{evalmod} takes predicted scores from a model and binary lables
#' from an observed dataset and calculates ROC and Precision-Recall curves.
#'
#' @param pscores A numeric vector of predicted scores.
#' @param olabs A numeric vector or a factor of observed labels.
#' @param x_interval A numeric value to specifiy an interval of the
#'   x-axis (TPRs for ROC and recall for Precision-Recall).
#' @param na.last Passed to \code{\link[base]{rank}} for controlling the
#'   treatment of NAs. The value can be TRUE or FALSE. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first.
#' @param ties.method Passed to \code{\link[base]{rank}} for controlling tied
#'   scores. The value can be "average", "random", or "first". The "first"
#'   method results in a permutation with increasing values at each index
#'   set of ties. The "random" method puts these in random order whereas
#'   the default, "average", replaces them by their mean.
#' @param olevs A character vector to overide the levels of the factor for
#'   observed binary labels.
#' @param model_name The name of the model/classifier to be evaluated.
#' @return \code{evalmod} returns a \code{curves} S3 object that
#'   contains ROC and Precision-Recall curves.
#'
#' @examples
#' data(P10N10)
#' curves <- evalmod(P10N10$scores, P10N10$labels)
#' curves
#'
#' plot(curves)
evalmod <- function(pscores, olabs, x_interval = 0.001, na.last = FALSE,
                    ties.method = "average",
                    olevs = c("negative", "positive"),
                    model_name = as.character(NA), data_no = 1L) {

  # Format input data
  fmdat <- reformat_data(pscores, olabs, na.last = na.last,
                         ties.method = ties.method, olevs = olevs,
                         model_name = model_name, data_no = data_no)

  # Create confusion matrices for all threshold values
  cdat <- create_confmats(fmdat)

  # Calculate evaluation measures
  pevals <- calc_measures(cdat)

  # Create ROC and Precisio-Recall curves
  curves <- create_curves(pevals, x_interval = x_interval)
}

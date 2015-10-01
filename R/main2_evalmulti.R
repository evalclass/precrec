#' Evaluate multiple models
#'
#' \code{evalmod} takes predicted scores from a model and binary lables
#' from an observed dataset and calculates ROC and Precision-Recall curves
#' for multiple models.
#'
#' @param mdat An \code{mdat} object created by \code{\link{mmdata}}.
#' @param scores A numeric vector of predicted scores.
#' @param labels A numeric vector or a factor of observed labels.
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
#' @param levels A character vector to overide the levels of the factor for
#'   observed binary labels.
#' @param model_name The name of the model/classifier to be evaluated.
#' @return \code{evalmod} returns an \code{mscurves} S3 object that
#'   contains ROC and Precision-Recall curves.
#'
#' @examples
#' data(IB500)
#'
#' s1 <- IB500$random_scores
#' s2 <- IB500$poor_er_scores
#' s3 <- IB500$good_er_scores
#' s4 <- IB500$excel_scores
#' s5 <- IB500$perf_scores
#' l1 <- IB500$labels
#'
#' model_names <- c("Random", "Poor ER", "Good ER", "Excellent", "Perfect")
#'
#' scores <- join_scores(s1, s2, s3, s4, s5)
#' labels <- l1
#' mdat <- mmdata(scores, labels, model_names = model_names)
#'
#' curves <- evalmulti(mdat)
evalmulti <- function(mdat, scores = NULL, labels = NULL,
                      model_names = NULL, data_nos = NULL, x_interval = 0.001,
                      na.last = FALSE, ties.method = "average",
                      levels = c("negative", "positive")) {

  .validate_evalmulti_args(x_interval, model_names, data_nos, na.last,
                           ties.method, levels)

  if (!missing(mdat)) {
    .validate(mdat)
  } else {
    mdat <- mmdata(mscores, mlabels,
                   model_names = model_names, data_nos = data_nos,
                   na.last = na.last, ties.method = ties.method, levels = levels)
  }

  pl_main(mdat, model_type = "multiple", data_type = "single",
          x_interval = x_interval)

}

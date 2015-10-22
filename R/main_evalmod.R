#' Evaluate models and generate performance evaluation plots
#'
#' The \code{evalmod} function takes predicted scores and binary lables
#'   and calculates ROC and Precision-Recall curves. It can aslo generate
#'   several other performace evaluation plots, alternatively.
#'
#' @param mdat An \code{mdat} object created by the \code{\link{mmdata}}
#'   function. The following arguments are ignored when \code{mdat} is
#'   specified.
#'   \itemize{
#'     \item \code{scores}
#'     \item \code{labels}
#'     \item \code{modnames}
#'     \item \code{dsids}
#'     \item \code{posclass}
#'     \item \code{na_worst}
#'     \item \code{ties_method}
#'   }
#'   Both \code{scores} and \code{labels} must be at least specified
#'   when \code{mdat} is unspecified.
#'
#' @param mode A string that specifies what evaluation plots
#'   \code{evalmod} generates.
#'   \describe{
#'     \item{"rocprc"}{ROC and Precision-Recall curves}
#'     \item{"basic"}{Threshold values vs. accuracy, error rate, specificity,
#'                    sensitivity, or precision}
#'   }
#'
#' @param scores A numeric dataset of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list.
#'
#' @param labels A numeric, character, logical, or factor dataset
#'   of observed labels. It can be a vector, a matrix, an array,
#'   a data frame, or a list.
#'
#' @param na_worst A boolean value for controlling the treatment of NAs
#'   in \code{scores}.
#'   \describe{
#'     \item{TRUE}{NAs are treated as the highest score}
#'     \item{FALSE}{NAs are treated as the lowest score}
#'   }
#'
#' @param ties_method A string for controlling ties in \code{scores}.
#'   Ignored if mdat is set.
#'   \describe{
#'     \item{"equiv"}{Ties are equivalently ranked}
#'     \item{"random"}{Ties are ranked in an incresing order as appeared}
#'     \item{"first"}{ Ties are ranked in random order}
#'   }
#'
#' @param posclass A scaler value to specify positives in \code{labels}.
#'
#' @param modnames A character vector for the names of the models.
#'
#' @param dsids A numeric vector for dataset IDs.
#'
#' @param calc_avg A logical value to specifiy whether average curves should
#'   be calculated.
#'
#' @param ci_alpha A numeric value with range [0, 1] to specifiy the alpha
#'   value of the confidence interval calculation. It is effective only
#'   when \code{calc_avg} is set to \code{TRUE}
#'
#' @param all_curves A logical value to specifiy whether all raw curves
#'   are stored or deleted when the average curves are calculated.
#'
#' @param x_bins A numeric value to specifiy the number of minimum bins
#'   on the x-axis.
#'
#' @return The \code{evalmod} function returns an S3 object
#'   that contains peformace evaluation measures, such as ROC
#'   and Precision-Recall curves. The returned S3 object
#'   can be \code{sscurves}, \code{mscurves}, \code{scurves}
#'
#' @seealso \code{\link{plot}}, \code{\link{autoplot}},
#'   and \code{\link{fortify}} for plotting curves.
#'   \code{\link{join_scores}}, \code{\link{join_scores}},
#'   and \code{\link{mmdata}} for formatting input data.
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
#' curves2 <- evalmod(scores = samps[["scores"]],
#'                    labels = samps[["labels"]],
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
evalmod <- function(mdat, mode = "rocprc", scores = NULL, labels = NULL,
                    modnames = NULL, dsids = NULL,
                    posclass = NULL, na_worst = TRUE, ties_method = "equiv",
                    calc_avg = TRUE, ci_alpha = 0.05, all_curves = FALSE,
                    x_bins = 1000, orig_points = TRUE) {

  .validate_evalmod_args(mode, modnames, dsids, posclass, na_worst, ties_method,
                         calc_avg, ci_alpha, all_curves, x_bins, orig_points)

  if (!missing(mdat)) {
    .validate(mdat)
  } else {
    mdat <- mmdata(scores, labels,
                   modnames = modnames, dsids = dsids, posclass = posclass,
                   na_worst = na_worst, ties_method = ties_method)
  }

  if (mode == "rocprc") {
    pl_main_rocprc(mdat, calc_avg = calc_avg, ci_alpha = ci_alpha,
                   all_curves = all_curves, x_bins = x_bins,
                   orig_points = orig_points)
  } else if (mode == "basic") {
    pl_main_basic(mdat, calc_avg = calc_avg, ci_alpha = ci_alpha,
                  all_curves = all_curves)
  }

}

#
# Validate arguments of evalmod()
#
.validate_evalmod_args <- function(mode, modnames, dsids,
                                   posclass, na_worst, ties_method,
                                   calc_avg, ci_alpha, all_curves,
                                   x_bins, orig_points) {

  # Check mode
  .validate_mode(mode)

  # Check model names
  .validate_modnames(modnames, length(modnames))

  # Check dataset IDs
  .validate_dsids(dsids, length(dsids))

  # Check posclass
  .validate_posclass(posclass)

  # Check na_worst
  .validate_na_worst(na_worst)

  # Check ties_method
  .validate_ties_method(ties_method)


  # Validate calc_avg
  .validate_calc_avg(calc_avg)

  # Validate ci_alpha
  .validate_ci_alpha(ci_alpha)

  # Validate all_curves
  .validate_all_curves(all_curves)


  # Check x_bins
  .validate_x_bins(x_bins)

  # Check orig_points
  .validate_orig_points(orig_points)

}

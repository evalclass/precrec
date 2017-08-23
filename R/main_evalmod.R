#' Evaluate models and calculate performance evaluation measures
#'
#' The \code{evalmod} function calculates ROC and Precision-Recall curves for
#'   specified prediction scores and binary labels. It also calculate several
#'   basic performance evaluation measures, such as accuracy, error rate, and
#'   precision, by specifying \code{mode} as "basic".
#'
#' @param mdat An \code{S3} object created by the \code{\link{mmdata}}
#'   function. It contains formatted scores and labels.
#'   The \code{evalmod} function ignores the following arguments
#'   when \code{mdat} is specified.
#'   \itemize{
#'     \item \code{scores}
#'     \item \code{labels}
#'     \item \code{modnames}
#'     \item \code{dsids}
#'     \item \code{posclass}
#'     \item \code{na_worst}
#'     \item \code{ties_method}
#'   }
#'   These arguments are internally passed to the \code{\link{mmdata}} function
#'   when \code{mdat} is unspecified.
#'   In that case, both \code{scores} and \code{labels} must be
#'   at least specified.
#'
#' @param mode A string that specifies the types of evaluation measures
#'   that the \code{evalmod} function calculates.
#'   \describe{
#'     \item{"rocprc"}{ROC and Precision-Recall curves}
#'     \item{"prcroc"}{Same as above}
#'     \item{"basic"}{Normalized ranks vs. accuracy, error rate, specificity,
#'                    sensitivity, precision, Matthews correlation coefficient,
#'                    and F-score. }
#'     \item{"aucroc"}{Fast AUC(ROC) calculation with the U statistic}
#'   }
#'
#' @param scores A numeric dataset of predicted scores. It can be a vector,
#'   a matrix, an array, a data frame, or a list. The \code{\link{join_scores}}
#'   function can be useful to make scores with multiple datasets.
#'
#' @param labels A numeric, character, logical, or factor dataset
#'   of observed labels. It can be a vector, a matrix, an array,
#'   a data frame, or a list. The \code{\link{join_labels}}
#'   function can be useful to make labels with multiple datasets.
#'
#' @param modnames A character vector for the names of the models.
#'   The \code{evalmod} function automatically generates default names
#'   as "m1", "m2", "m3", and so on when it is \code{NULL}.
#'
#' @param dsids A numeric vector for test dataset IDs.
#' The \code{evalmod} function automatically generates the default ID
#' as \code{1} when it is \code{NULL}.
#'
#' @param posclass A scalar value to specify the label of positives
#'   in \code{labels}. It must be the same data type as \code{labels}.
#'   For example, \code{posclass = -1} changes the positive label
#'   from \code{1} to \code{-1} when \code{labels} contains
#'   \code{1} and \code{-1}. The positive label will be automatically
#'   detected when \code{posclass} is \code{NULL}.
#
#' @param na_worst A Boolean value for controlling the treatment of NAs
#'   in \code{scores}.
#'   \describe{
#'     \item{TRUE}{All NAs are treated as the worst scores}
#'     \item{FALSE}{All NAs are treated as the best scores}
#'   }
#'
#' @param ties_method A string for controlling ties in \code{scores}.
#'   \describe{
#'     \item{"equiv"}{Ties are equivalently ranked}
#'     \item{"first"}{Ties are ranked in an increasing order as appeared}
#'     \item{"random"}{ Ties are ranked in random order}
#'   }
#'
#' @param calc_avg A logical value to specify whether average curves should
#'   be calculated. It is effective only when \code{dsids} contains multiple
#'   dataset IDs. For instance, the function calculates the average for the
#'   model "m1" when \code{modnames} is \code{c("m1", "m1", "m1")} and
#'   \code{dsids} is \code{c(1, 2, 3)}. The calculation points are defined by
#'   \code{x_bins}.
#'
#' @param cb_alpha A numeric value with range [0, 1] to specify the alpha
#'   value of the point-wise confidence bounds calculation. It is effective only
#'   when \code{calc_avg} is set to \code{TRUE}. For example, it should be
#'   \code{0.05} for the 95\% confidence level. The calculation points are
#'   defined by \code{x_bins}.
#'
#' @param raw_curves A logical value to specify whether all raw curves
#'   should be discarded after the average curves are calculated.
#'   It is effective only when \code{calc_avg} is set to \code{TRUE}.
#'
#' @param x_bins An integer value to specify the number of minimum bins
#'   on the x-axis. It is then used to define supporting points For instance,
#'   the x-values of the supporting points will be \code{c(0, 0.5, 1)} and
#'   \code{c(0, 0.25, 0.5, 0.75, 1)} when \code{x_bins = 2}
#'   and \code{x_bins = 4}, respectively. All corresponding y-values of
#'   the supporting points are calculated.
#'
#' @param ... These additional arguments are passed to \code{\link{mmdata}}
#'   for data preparation.
#'
#' @return The \code{evalmod} function returns an \code{S3} object
#'   that contains performance evaluation measures. The number of models and
#'   the number of datasets can be controlled by \code{modnames} and
#'   \code{dsids}. For example, the number of models is "single" and the number
#'   of test datasets is "multiple" when \code{modnames = c("m1", "m1", "m1")}
#'   and \code{dsids = c(1, 2, 3)} are specified.
#'
#' Different \code{S3} objects have different default behaviors of \code{S3}
#'   generics, such as \code{\link{plot}}, \code{\link{autoplot}}, and
#'   \code{\link{fortify}}.
#'
#' \enumerate{
#'
#'   \item  The \code{evalmod} function returns one of the following \code{S3}
#'   objects when \code{mode} is "prcroc".
#'   The objects contain ROC and Precision-Recall curves.
#'
#'   \tabular{lll}{
#'     \strong{\code{S3} object}
#'     \tab \strong{# of models}
#'     \tab \strong{# of test datasets} \cr
#'
#'     sscurves \tab single   \tab single   \cr
#'     mscurves \tab multiple \tab single   \cr
#'     smcurves \tab single   \tab multiple \cr
#'     mmcurves \tab multiple \tab multiple
#'   }
#'
#'   \item The \code{evalmod} function returns one of the following \code{S3}
#'   objects when \code{mode} is "basic".
#'   They contain five different basic evaluation measures; error rate,
#'   accuracy, specificity, sensitivity, and precision.
#'
#'   \tabular{lll}{
#'     \strong{\code{S3} object}
#'     \tab \strong{# of models}
#'     \tab \strong{# of test datasets} \cr
#'
#'     sspoints \tab single   \tab single   \cr
#'     mspoints \tab multiple \tab single   \cr
#'     smpoints \tab single   \tab multiple \cr
#'     mmpoints \tab multiple \tab multiple
#'   }
#'
#'   \item The \code{evalmod} function returns the \code{aucroc} S3 object
#'   when \code{mode} is "aucroc", which can be used with 'print' and 'as.data.frame'.
#'
#' }
#'
#' @seealso \code{\link{plot}} for plotting curves with the general R plot.
#'   \code{\link{autoplot}} and \code{\link{fortify}} for plotting curves
#'   with \pkg{ggplot2}. \code{\link{mmdata}} for formatting input data.
#'   \code{\link{join_scores}} and \code{\link{join_labels}} for formatting
#'   scores and labels with multiple datasets.
#'   \code{\link{format_nfold}} for creating n-fold cross validation dataset
#'   from data frame.
#'   \code{\link{create_sim_samples}} for generating random samples
#'   for simulations.
#'
#' @examples
#'
#' ##################################################
#' ### Single model & single test dataset
#' ###
#'
#' ## Load a dataset with 10 positives and 10 negatives
#' data(P10N10)
#'
#' ## Generate an sscurve object that contains ROC and Precision-Recall curves
#' sscurves <- evalmod(scores = P10N10$scores, labels = P10N10$labels)
#' sscurves
#'
#' ## Generate an sspoints object that contains basic evaluation measures
#' sspoints <- evalmod(mode = "basic", scores = P10N10$scores,
#'                     labels = P10N10$labels)
#' sspoints
#'
#'
#' ##################################################
#' ### Multiple models & single test dataset
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(1, 100, 100, "all")
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                modnames = samps[["modnames"]])
#'
#' ## Generate an mscurve object that contains ROC and Precision-Recall curves
#' mscurves <- evalmod(mdat)
#' mscurves
#'
#' ## Generate an mspoints object that contains basic evaluation measures
#' mspoints <- evalmod(mdat, mode = "basic")
#' mspoints
#'
#'
#' ##################################################
#' ### Single model & multiple test datasets
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(4, 100, 100, "good_er")
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                modnames = samps[["modnames"]],
#'                dsids = samps[["dsids"]])
#'
#' ## Generate an smcurve object that contains ROC and Precision-Recall curves
#' smcurves <- evalmod(mdat)
#' smcurves
#'
#' ## Generate an smpoints object that contains basic evaluation measures
#' smpoints <- evalmod(mdat, mode = "basic")
#' smpoints
#'
#'
#' ##################################################
#' ### Multiple models & multiple test datasets
#' ###
#'
#' ## Create sample datasets with 100 positives and 100 negatives
#' samps <- create_sim_samples(4, 100, 100, "all")
#' mdat <- mmdata(samps[["scores"]], samps[["labels"]],
#'                modnames = samps[["modnames"]],
#'                dsids = samps[["dsids"]])
#'
#' ## Generate an mmcurve object that contains ROC and Precision-Recall curves
#' mmcurves <- evalmod(mdat)
#' mmcurves
#'
#' ## Generate an mmpoints object that contains basic evaluation measures
#' mmpoints <- evalmod(mdat, mode = "basic")
#' mmpoints
#'
#'
#' ##################################################
#' ### N-fold cross validation datasets
#' ###
#'
#' ## Load test data
#' data(M2N50F5)
#'
#' ## Speficy nessesary columns to create mdat
#' cvdat <- mmdata(nfold_df = M2N50F5, score_cols = c(1, 2),
#'                 lab_col = 3, fold_col = 4,
#'                 modnames = c("m1", "m2"), dsids = 1:5)
#'
#' ## Generate an mmcurve object that contains ROC and Precision-Recall curves
#' cvcurves <- evalmod(cvdat)
#' cvcurves
#'
#' ## Generate an mmpoints object that contains basic evaluation measures
#' cvpoints <- evalmod(cvdat, mode = "basic")
#' cvpoints
#'
#' ## Specify mmdata arguments from evalmod
#' cvcurves2 <- evalmod(nfold_df = M2N50F5, score_cols = c(1, 2),
#'                      lab_col = 3, fold_col = 4,
#'                      modnames = c("m1", "m2"), dsids = 1:5)
#' cvcurves2
#'
#'
#' ##################################################
#' ### AUC with the U statistic
#' ###
#'
#' ## mode = "aucroc" returns 'aucroc' S3 object
#' data(P10N10)
#'
#' # 'aucroc' S3 object
#' uauc1 <- evalmod(scores = P10N10$scores, labels = P10N10$labels,
#'                  mode="aucroc")
#'
#' # print 'aucroc'
#' uauc1
#'
#' # as.data.frame 'aucroc'
#' as.data.frame(uauc1)
#'
#' ## It is 2-3 times faster than mode = "rocprc"
#' # A sample of 100,000
#' samp1 <- create_sim_samples(1, 50000, 50000)
#'
#' # a function to test mode = "rocprc"
#' func_evalmod_rocprc <- function(samp) {
#'    curves <- evalmod(scores = samp$scores, labels = samp$labels)
#'    aucs <- auc(curves)
#' }
#'
#' # a function to test mode = "aucroc"
#' func_evalmod_aucroc <- function(samp) {
#'   uaucs <- evalmod(scores = samp$scores, labels = samp$labels, mode="aucroc")
#'   as.data.frame(uaucs)
#' }
#'
#' # Process time
#' system.time(res1 <- func_evalmod_rocprc(samp1))
#' system.time(res2 <- func_evalmod_aucroc(samp1))
#'
#' # AUCs
#' res1
#' res2
#'
#' @export
evalmod <- function(mdat, mode = NULL, scores = NULL, labels = NULL,
                    modnames = NULL, dsids = NULL,
                    posclass = NULL, na_worst = TRUE, ties_method = "equiv",
                    calc_avg = TRUE, cb_alpha = 0.05, raw_curves = FALSE,
                    x_bins = 1000, ...) {

  # Validation
  new_mode <- .get_new_mode(mode, mdat, "rocprc")
  new_ties_method <- .pmatch_tiesmethod(ties_method, ...)
  new_na_worst <- .get_new_naworst(na_worst, ...)
  if (x_bins == 0) {
    x_bins <- 1
  }
  .validate_evalmod_args(new_mode, modnames, dsids, posclass, new_na_worst,
                         new_ties_method, calc_avg, cb_alpha, raw_curves,
                         x_bins)

  # Create mdat if not provided
  if (missing(mdat)) {
    mdat <- mmdata(scores, labels,
                   modnames = modnames, dsids = dsids, posclass = posclass,
                   na_worst = new_na_worst, ties_method = new_ties_method,
                   mode = new_mode, ...)
  }
  .validate(mdat)

  # Call pipeline controller
  pl_main(mdat, mode = new_mode, calc_avg = calc_avg, cb_alpha = cb_alpha,
          raw_curves = raw_curves, x_bins = x_bins, na_worst = new_na_worst,
          ties_method = new_ties_method, validate = FALSE)

}

#
# Get a mode
#
.get_new_mode <- function(mode, mdat, def_mode = "rocprc") {
  mdat_mode <- NA
  if (!missing(mdat)) {
    .validate(mdat)
    mdat_mode <- attr(mdat, "args")[["mode"]]
  }

  new_mode <- NA
  if (!is.null(mode)) {
    new_mode <- .pmatch_mode(mode)
    if (new_mode != "aucroc" && !is.na(mdat_mode) &&  mdat_mode == "aucroc") {
      stop(paste0("Invalid 'mode': evalmod <- '", new_mode,
                  "'', mmdata <- '", mdat_mode, "'"),
           call. = FALSE)
    }
  } else if (!is.na(mdat_mode)) {
    new_mode <- mdat_mode
  }

  if (is.null(new_mode) || is.na(new_mode)) {
    new_mode <- def_mode
  }

  new_mode
}

#
# Validate arguments of evalmod()
#
.validate_evalmod_args <- function(mode, modnames, dsids,
                                   posclass, na_worst, ties_method,
                                   calc_avg, cb_alpha, raw_curves,
                                   x_bins) {

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

  # Validate cb_alpha
  .validate_cb_alpha(cb_alpha, calc_avg)

  # Validate raw_curves
  .validate_raw_curves(raw_curves, calc_avg)


  # Check x_bins
  .validate_x_bins(x_bins)

}

# # Get ROC or Precision-Recall curves from mcurves
# .get_mrocprc_curves <- function(mcurves, curve_type, class_name) {
#   # Group ROC or PRC curves
#   mc <- list()
#   for (i in seq_along(mcurves)) {
#     curves <- mcurves[[i]][[curve_type]]
#     mc[[attr(curves, "model_name")]] <- curves
#   }
#
#   # === Create an S3 object ===
#   s3obj <- structure(mc, class = class_name)
#
#   # Set attributes
#   attr(s3obj, "validated") <- FALSE
#
#   # Call .validate.class_name()
#   .validate(s3obj)
# }

# Get model names from mdat
.get_mdat_model_names <- function(mdat, model_names) {
  # Set model names
  if (missing(model_names) || is.null(model_names)) {
    model_names <- names(mdat[["mdat"]])
  }
  if (length(model_names) != length(mdat[["mdat"]])) {
    stop("Incorrect model names")
  }

  model_names
}

# Validate list curves
.validate_list_curves <- function(list_curves) {

  vfunc <- function(s) {
    .validate(list_curves[[s]])
  }

  list_curves <- mapply(vfunc, seq(length(list_curves)), SIMPLIFY = FALSE)

  list_curves
}

# Create a list of ROC and Precision-Recall curves for multiple models.
.create_list_curves <- function(mfmdat, mscores, mobslabs, x_interval = 0.001,
                                model_names = NULL, ...) {

  # Create mdat from mscores and mobslabs if it's missing
  mfmdat <- .create_by_mscores_and_mlabels(mfmdat, mscores, mobslabs)

  # Set model names
  model_names <- .get_mdat_model_names(mfmdat, model_names)

  # Validation
  .validate_create_mcurves_args(x_interval, model_names, ...)

  # Define a function for a single model
  mkfunc <- function(s) {
    cdat <- create_confmats(mfmdat[[s]])
    pevals <- calc_measures(cdat)
    curves <- create_curves(pevals, x_interval = x_interval,
                            model_name = model_names[s])
    curves
  }

  list_curves <- lapply(seq_along(mfmdat), mkfunc)
  .validate_list_curves(list_curves)
}

#' Create ROC curves for multiple models.
#'
#' \code{create_mroc_curves} takes input data as three different options
#' as below.
#' \enumerate{
#'   \item \code{list_curves} only
#'   \item \code{mdat} only
#'   \item Both \code{mscores} and \code{mobslabs}
#' }
#' \code{list_curves} must be a list that contains ROC and Precision-Recall
#' curves for multiple models. \code{mdat} should be an S3 object that contains
#' predicted scores from multiple models and corresponding binary lables
#' from an observed dataset. \code{create_mroc_curves} can also take
#' datasets of scores and labels as \code{mscores} and \code{mobslabs}.
#' It returns a \code{mroc_curves} that contains ROC curves for multiple models.
#'
#' @param list_curves A list of  \code{curves} objects created by
#'   \code{\link{create_curves_for_multi}}.
#' @param mdat An \code{mdat} object created by \code{\link{create_mdat}}.
#' @param mscores A dataset of predicted scores.
#' @param mobslabs A dataset of of observed labels.
#' @param x_interval A numeric value to specifiy an interval of the
#'   x-axis (TPRs for ROC and recall for Precision-Recall).
#' @param model_names Names of the models/classifiers to be evaluated.
#' @param ... Other arguments passed to \code{\link{reformat_data}}
#' @return \code{create_mroc_curves} returns a \code{mroc_curves}
#' S3 objects that contains ROC curves for multiple models.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' mscores <- combine_scores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' mobslabs <- combine_obslbs(l1, l2, l3)
#'
#' mdat <- create_mdat(mscores, mobslabs)
#' list_curves <- create_curves_for_multi(mdat)
#'
#' mroc_curves <- create_mroc_curves(list_curves)
#' mroc_curves
create_mroc_curves <- function(mdat, mscores = NULL, mobslabs = NULL,
                               x_interval = 0.001, model_names = NULL, ...) {

  list_curves <- .create_list_curves(mdat, mscores = mscores,
                                     mobslabs = mobslabs,
                                     x_interval = x_interval,
                                     model_names = model_names,  ...)

  mroc_curves <- .get_mrocprc_curves(list_curves, "roc", "mroc_curves")
  mroc_curves
}

#' Create Precision-Recall curves for multiple models.
#'
#' \code{create_mprc_curves} takes input data as three different options
#' as below.
#' \enumerate{
#'   \item \code{list_curves} only
#'   \item \code{mdat} only
#'   \item Both \code{mscores} and \code{mobslabs}
#' }
#' \code{list_curves} must be a list that contains ROC and Precision-Recall
#' curves for multiple models. \code{mdat} should be an S3 object that contains
#' predicted scores from multiple models and corresponding binary lables
#' from an observed dataset. \code{create_mprc_curves} can also take
#' datasets of scores and labels as \code{mscores} and \code{mobslabs}.
#' It returns a \code{mprc_curves} that contains Precision-Recall curves
#' for multiple models.
#'
#' @param list_curves A list of  \code{curves} objects created by
#'   \code{\link{create_curves_for_multi}}.
#' @param mdat An \code{mdat} object created by \code{\link{create_mdat}}.
#' @param mscores A dataset of predicted scores.
#' @param mobslabs A dataset of of observed labels.
#' @param x_interval A numeric value to specifiy an interval of the
#'   x-axis (TPRs for ROC and recall for Precision-Recall).
#' @param model_names Names of the models/classifiers to be evaluated.
#' @param ... Other arguments passed to \code{\link{reformat_data}}
#' @return \code{create_mprc_curves} returns a \code{mprc_curves}
#' S3 objects that contains Precision-Recall curves for multiple models.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' mscores <- combine_scores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' mobslabs <- combine_obslbs(l1, l2, l3)
#'
#' mdat <- create_mdat(mscores, mobslabs)
#' list_curves <- create_curves_for_multi(mdat)
#'
#' mprc_curves <- create_mprc_curves(list_curves)
#' mprc_curves
create_mprc_curves <- function(mdat, mscores = NULL, mobslabs = NULL,
                               x_interval = 0.001, model_names = NA, ...) {

  list_curves <- .create_list_curves(mdat, mscores = mscores,
                                     mobslabs = mobslabs,
                                     x_interval = x_interval,
                                     model_names = model_names,  ...)

  mprc_curves <- .get_mrocprc_curves(list_curves, "prc", "mprc_curves")
  mprc_curves
}

#' Create ROC and Precision-Recall curves for multiple models.
#'
#' \code{create_mcurves} takes input data as three different options
#' as below.
#' \enumerate{
#'   \item \code{list_curves} only
#'   \item \code{mdat} only
#'   \item Both \code{mscores} and \code{mobslabs}
#' }
#' \code{list_curves} must be a list that contains ROC and Precision-Recall
#' curves for multiple models. \code{mdat} should be an S3 object that contains
#' predicted scores from multiple models and corresponding binary lables
#' from an observed dataset. \code{create_mcurves} can also take
#' datasets of scores and labels as \code{mscores} and \code{mobslabs}.
#' It returns a \code{mcurves} that contains ROC and Precision-Recall curves
#' for multiple models.
#'
#' @param list_curves A list of  \code{curves} objects created by
#'   \code{\link{create_curves_for_multi}}.
#' @param mfmdat An \code{mfmdat} object created by \code{\link{create_mdat}}.
#' @param mscores A dataset of predicted scores.
#' @param mobslabs A dataset of of observed labels.
#' @param x_interval A numeric value to specifiy an interval of the
#'   x-axis (TPRs for ROC and recall for Precision-Recall).
#' @param model_names Names of the models/classifiers to be evaluated.
#' @param ... Other arguments passed to \code{\link{reformat_data}}
#' @return \code{create_mprc_curves} returns a \code{mprc_curves}
#' S3 objects that contains Precision-Recall curves for multiple models.
#'
#' @examples
#' s1 <- c(1, 2, 3, 4)
#' s2 <- c(5, 6, 7, 8)
#' s3 <- c(2, 4, 6, 8)
#' mscores <- combine_scores(s1, s2, s3)
#'
#' l1 <- c(1, 0, 1, 1)
#' l2 <- c(1, 1, 0, 0)
#' l3 <- c(0, 1, 0, 1)
#' mobslabs <- combine_obslbs(l1, l2, l3)
#'
#' mfmdat <- reformat_mdata(mscores, mobslabs)
#' mcurves <- create_mcurves(mfmdat)
#' mcurves
create_mcurves <- function(mfmdat, mscores = NULL, mobslabs = NULL,
                           x_interval = 0.001, model_names = NULL, ...) {

  # Make list_curves
  list_curves <- .create_list_curves(mfmdat, mscores = mscores,
                                     mobslabs = mobslabs,
                                     x_interval = x_interval,
                                     model_names = model_names,  ...)

  mroc_curves <- .get_mrocprc_curves(list_curves, "roc", "mroc_curves")
  mprc_curves <- .get_mrocprc_curves(list_curves, "prc", "mprc_curves")

  # === Create an S3 object ===
  s3obj <- structure(list(mroc_curves = mroc_curves,
                          mprc_curves = mprc_curves), class = "mcurves")

  # Set attributes
  attr(s3obj, "validated") <- FALSE

  # Call .validate.mfmdat()
  .validate(s3obj)
}

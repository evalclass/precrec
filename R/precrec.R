#' precrec: A package for computating accurate ROC and Precision-Recall curves
#'
#' The prec package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section precrec functions:
#' The precrec functions ...
#'
#' @docType package
#' @name precrec
#'
#' @useDynLib precrec
#' @importFrom Rcpp sourceCpp
#' @importFrom ggplot2 autoplot
#' @importFrom ggplot2 fortify
#'
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("precrec", libpath)
}

#' Balacned data with 500 positives and 500 negatives.
#'
#' A list contains labels and scores of five different levels.
#' All scores were randomly generated.
#'
#' @format A list with 8 items:
#' \describe{
#'   \item{np}{number of positives: 500}
#'   \item{nn}{number of negatives: 500}
#'   \item{labels}{labeles of observed data}
#'   \item{random_scores}{scores of random perforamnce level}
#'   \item{poor_er_scores}{scores of poor early retrieval level}
#'   \item{good_er_scores}{scores of good early retrieval level}
#'   \item{excel_scores}{scores of excellent level}
#'   \item{perf_scores}{scores of perfect level}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name B500
#' @usage data(B500)
NULL

#' Balacned data with 1000 positives and 1000 negatives.
#'
#' A list contains labels and scores of five different levels.
#' All scores were randomly generated.
#'
#' @format A list with 8 items:
#' \describe{
#'   \item{np}{number of positives: 1000}
#'   \item{nn}{number of negatives: 1000}
#'   \item{labels}{labeles of observed data}
#'   \item{random_scores}{scores of random perforamnce level}
#'   \item{poor_er_scores}{scores of poor early retrieval level}
#'   \item{good_er_scores}{scores of good early retrieval level}
#'   \item{excel_scores}{scores of excellent level}
#'   \item{perf_scores}{scores of perfect level}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name B1000
#' @usage data(B1000)
NULL

#' Imbalacned data with 500 positives and 5000 negatives.
#'
#' A list contains labels and scores of five different levels.
#' All scores were randomly generated.
#'
#' @format A list with 8 items:
#' \describe{
#'   \item{np}{number of positives: 500}
#'   \item{nn}{number of negatives: 5000}
#'   \item{labels}{labeles of observed data}
#'   \item{random_scores}{scores of random perforamnce level}
#'   \item{poor_er_scores}{scores of poor early retrieval level}
#'   \item{good_er_scores}{scores of good early retrieval level}
#'   \item{excel_scores}{scores of excellent level}
#'   \item{perf_scores}{scores of perfect level}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name IB500
#' @usage data(IB500)
NULL

#' Imbalacned data with 1000 positives and 10000 negatives.
#'
#' A list contains labels and scores of five different levels.
#' All scores were randomly generated.
#'
#' @format A list with 8 items:
#' \describe{
#'   \item{np}{number of positives: 1000}
#'   \item{nn}{number of negatives: 10000}
#'   \item{labels}{labeles of observed data}
#'   \item{random_scores}{scores of random perforamnce level}
#'   \item{poor_er_scores}{scores of poor early retrieval level}
#'   \item{good_er_scores}{scores of good early retrieval level}
#'   \item{excel_scores}{scores of excellent level}
#'   \item{perf_scores}{scores of perfect level}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name IB1000
#' @usage data(IB1000)
NULL

#' A small example dataset with tied scores.
#'
#' A list contains labels and scores with tied scores
#'
#' @format A list with 8 items:
#' \describe{
#'   \item{np}{number of positives: 10}
#'   \item{nn}{number of negatives: 10}
#'   \item{labels}{20 labeles of observed data}
#'   \item{scores}{20 scores with ties}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name P10N10
#' @usage data(P10N10)
NULL

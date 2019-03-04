#' Create random samples for simulations
#'
#' The \code{create_sim_samples} function generates random samples
#'   with different performance levels.
#'
#' @param n_repeat The number of iterations to make samples.
#'
#' @param np The number of positives in a sample.
#'
#' @param nn The number of negatives in a sample.
#'
#' @param score_names A character vector for the names of
#'   the following performance levels.
#'   \describe{
#'     \item{"random"}{Random}
#'     \item{"poor_er"}{Poor early retrieval}
#'     \item{"good_er"}{Good early retrieval}
#'     \item{"excel"}{Excellent}
#'     \item{"perf"}{Perfect}
#'     \item{"all"}{All of the above}
#'   }
#'
#' @return The \code{create_sim_samples} function returns a list
#'   with the following items.
#'   \itemize{
#'     \item scores: a list of numeric vectors
#'     \item labels: an integer vector
#'     \item modnames: a character vector of the model names
#'     \item dsids: a character vector of the dataset IDs
#'   }
#'
#' @seealso \code{\link{mmdata}} for formatting input data.
#'   \code{\link{evalmod}} for calculation evaluation measures.
#'
#' @examples
#'
#' ##################################################
#' ### Create a set of samples with 10 positives and 10 negatives
#' ### for the random performance level
#' ###
#' samps1 <- create_sim_samples(1, 10, 10, "random")
#'
#' ## Show the list structure
#' str(samps1)
#'
#'
#' ##################################################
#' ### Create two sets of samples with 10 positives and 20 negatives
#' ### for the random and the poor early retrieval performance levels
#' ###
#' samps2 <- create_sim_samples(2, 10, 20, c("random", "poor_er"))
#'
#' ## Show the list structure
#' str(samps2)
#'
#'
#' ##################################################
#' ### Create 3 sets of samples with 5 positives and 5 negatives
#' ### for all 5 levels
#' ###
#' samps3 <- create_sim_samples(3, 5, 5, "all")
#'
#' ## Show the list structure
#' str(samps3)
#'
#' @export
create_sim_samples <- function(n_repeat, np, nn, score_names = "random") {

  # === Validate input arguments ===
  choices <- c("random", "poor_er", "good_er", "excel", "perf")
  if (assertthat::see_if(assertthat::is.string(score_names))
      && any(score_names == "all")) {
    score_names <- choices
  } else if (!is.atomic(score_names) || !is.character(score_names)
             || !(all(score_names %in% choices))) {
    stop(gettextf("'score_names' must be one of %s",
                  paste(dQuote(choices), collapse = ", ")), call. = FALSE)
  }
  snames <- paste0(score_names, "_scores")

  # === Sample random variables ===
  afunc <- function() {
    simdat <- .sample_rnd5levs(np, nn)
    ffunc <- function(i) {
      if (names(simdat[i]) %in% snames) {
        TRUE
      } else {
        FALSE
      }
    }
    list_ids <- Filter(ffunc, seq_along(simdat))
    sd <- simdat[list_ids]
    names(sd) <- NULL
    sd
  }
  scores <- replicate(n_repeat, afunc(), simplify = FALSE)
  labels <- c(rep(1, np), rep(0, nn))

  # === Make a list ===
  list(scores = scores,
       labels = labels,
       modnames = rep(score_names, n_repeat),
       dsids = rep(seq(n_repeat), each = length(score_names)))
}

#
# Sample random data for five different levels
#
.sample_rnd5levs <- function(np, nn) {
  labels <- c(rep(1, np), rep(0, nn))

  random_scores <- c(stats::rnorm(np, 0, 1), stats::rnorm(nn, 0, 1))
  poor_er_scores <- c(stats::rbeta(np, 4, 1), stats::rbeta(nn, 1, 1))
  good_er_scores <- c(stats::rbeta(np, 1, 1), stats::rbeta(nn, 1, 4))
  excel_scores <- c(stats::rnorm(np, 3, 1), stats::rnorm(nn, 0, 1))
  perf_scores <- c(rep(1, np), rep(0, nn))

  list(np = np,
       nn = nn,
       labels = labels,
       random_scores = random_scores,
       poor_er_scores = poor_er_scores,
       good_er_scores = good_er_scores,
       excel_scores = excel_scores,
       perf_scores = perf_scores
  )
}

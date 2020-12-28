#' Create n-fold cross validation dataset from data frame
#'
#' The \code{format_nfold} function takes a data frame with scores, label,
#'   and n-fold columns and convert it to a list for \code{\link{evalmod}}
#'   and \code{\link{mmdata}}.
#'
#' @param nfold_df A data frame that contains at least one score column,
#'   label and fold columns.
#'
#' @param score_cols A character/numeric vector that specifies score columns
#'   of \code{nfold_df}.
#'
#' @param lab_col A number/string that specifies the label column
#'   of \code{nfold_df}.
#'
#' @param fold_col A number/string that specifies the fold column
#'   of \code{nfold_df}.
#'
#' @return The \code{format_nfold} function returns a list that
#'   contains multiple scores and labels.
#'
#' @seealso \code{\link{evalmod}} for calculation evaluation measures.
#'   \code{\link{mmdata}} for formatting input data.
#'   \code{\link{join_scores}} and \code{\link{join_labels}} for formatting
#'   scores and labels with multiple datasets.
#'
#' @examples
#'
#' ##################################################
#' ### Convert dataframe with 2 models and 5-fold datasets
#' ###
#'
#' ## Load test data
#' data(M2N50F5)
#' head(M2N50F5)
#'
#' ## Convert with format_nfold
#' nfold_list1 = format_nfold(nfold_df = M2N50F5, score_cols = c(1, 2),
#'                           lab_col = 3, fold_col = 4)
#'
#' ## Show the list structure
#' str(nfold_list1)
#' str(nfold_list1$scores)
#' str(nfold_list1$labels)
#'
#'
#' ##################################################
#' ### Speficy a single score column
#' ###
#'
#' ## Convert with format_nfold
#' nfold_list2 = format_nfold(nfold_df = M2N50F5, score_cols = 1,
#'                            lab_col = 3, fold_col = 4)
#'
#' ## Show the list structure
#' str(nfold_list2)
#' str(nfold_list2$scores)
#' str(nfold_list2$labels)
#'
#'
#' ##################################################
#' ### Use column names
#' ###
#'
#' ## Convert with format_nfold
#' nfold_list3 = format_nfold(nfold_df = M2N50F5,
#'                            score_cols = c("score1", "score2"),
#'                            lab_col = "label", fold_col = "fold")
#'
#' ## Show the list structure
#' str(nfold_list3)
#' str(nfold_list3$scores)
#' str(nfold_list3$labels)
#'
#' @export
format_nfold <- function(nfold_df, score_cols, lab_col, fold_col) {

  # Validate arguments
  .validate_format_nfold_args(nfold_df, score_cols, lab_col, fold_col)

  # Get fold ids
  fold_vec <- nfold_df[fold_col][[1]]
  fids <- sort(unique(fold_vec))

  # Split data frame by dataset IDs
  slcols = c(score_cols, lab_col)
  split_df <- lapply(fids, function(fid) nfold_df[fold_vec == fid, slcols])

  # Combine scores
  f_comb_s = function(col_idx) {
    lapply(seq_along(split_df), function(i) c(split_df[[i]][[col_idx]]))
  }
  scores <- unlist(lapply(seq_along(score_cols), f_comb_s), recursive = FALSE)

  # Combine labels
  lab_col_idx <- length(slcols)
  cfunc <- function(i) {
    if (is.factor(split_df[[i]][[lab_col_idx]])) {
      as.numeric(split_df[[i]][[lab_col_idx]])
    } else {
      c(split_df[[i]][[lab_col_idx]])
    }
  }
  f_comb_l <- function(s_col) {
    lapply(seq_along(split_df), cfunc)
  }
  labels <- unlist(lapply(score_cols, f_comb_l), recursive = FALSE)

  list(scores = scores, labels = labels)
}


#
# Validate arguments of format_nfold()
#
.validate_format_nfold_args <- function(nfold_df, score_cols, lab_col, fold_col) {

  if (!is.data.frame(nfold_df)) {
    stop("nfold_df must be a data frame.", call. = FALSE)
  }

  # Check score column names
  .validate_score_cols(score_cols, nfold_df)

  # Check label column name
  .validate_lab_col(lab_col, nfold_df)

  # Check fold column name
  .validate_fold_col(fold_col, nfold_df)

}


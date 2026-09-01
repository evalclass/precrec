#' Create n-fold cross validation dataset from data frame
#'
#' The `format_nfold` function takes a data frame with scores, label,
#'   and n-fold columns and convert it to a list for [evalmod()]
#'   and [mmdata()].
#'
#' @param nfold_df A data frame that contains at least one score column,
#'   label and fold columns.
#'
#' @param score_cols A character/numeric vector that specifies score columns
#'   of `nfold_df`.
#'
#' @param lab_col A number/string that specifies the label column
#'   of `nfold_df`.
#'
#' @param fold_col A number/string that specifies the fold column
#'   of `nfold_df`.
#'
#' @return The `format_nfold` function returns a list that
#'   contains multiple scores and labels.
#'
#' @seealso [evalmod()] for calculation evaluation measures.
#'   [mmdata()] for formatting input data.
#'   [join_scores()] and [join_labels()] for formatting
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
#' nfold_list1 <- format_nfold(
#'   nfold_df = M2N50F5, score_cols = c(1, 2),
#'   lab_col = 3, fold_col = 4
#' )
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
#' nfold_list2 <- format_nfold(
#'   nfold_df = M2N50F5, score_cols = 1,
#'   lab_col = 3, fold_col = 4
#' )
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
#' nfold_list3 <- format_nfold(
#'   nfold_df = M2N50F5,
#'   score_cols = c("score1", "score2"),
#'   lab_col = "label", fold_col = "fold"
#' )
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
  slcols <- c(score_cols, lab_col)
  split_df <- .map(fids, function(fid) nfold_df[fold_vec == fid, slcols])

  # Combine scores
  f_comb_s <- function(col_idx) {
    .map(split_df, function(fold) c(fold[[col_idx]]))
  }
  scores <- .flatten(.map_idx(score_cols, f_comb_s))

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
    .map_idx(split_df, cfunc)
  }
  labels <- .flatten(.map(score_cols, f_comb_l))

  list(scores = scores, labels = labels)
}


#
# Validate arguments of format_nfold()
#
.validate_format_nfold_args <- function(nfold_df, score_cols,
                                        lab_col, fold_col) {
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

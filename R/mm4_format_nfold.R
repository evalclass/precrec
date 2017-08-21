#
# Create n-fold cross validation dataset from data frame
#
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
  lab_col_idx = length(slcols)
  f_comb_l = function(s_col) {
    lapply(seq_along(split_df), function(i) c(split_df[[i]][[lab_col_idx]]))
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


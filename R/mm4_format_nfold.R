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
#' @seealso [evalmod()] for calculation evaluation metrics.
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


#' Reconstruct per-instance data from a table of curve points
#'
#' The `format_points` function takes a data frame of performance
#'   values calculated at a set of thresholds - true and false positive
#'   rates, or recall and precision - and reconstructs per-instance scores
#'   and labels that reproduce those points exactly. The result is a list
#'   for [evalmod()] and [mmdata()].
#'
#' @param points_df A data frame with one row per threshold, holding a
#'   threshold column, two rate columns, and optionally model and dataset
#'   columns.
#'
#' @param threshold_col A number/string that specifies the threshold column
#'   of `points_df`.
#'
#' @param tpr_col A number/string that specifies the true positive rate
#'   column of `points_df`.
#'
#' @param fpr_col A number/string that specifies the false positive rate
#'   column of `points_df`.
#'
#' @param rec_col A number/string that specifies the recall column of
#'   `points_df`. Recall and the true positive rate are the same quantity;
#'   supply one pair of columns, either `tpr_col`/`fpr_col` or
#'   `rec_col`/`prec_col`.
#'
#' @param prec_col A number/string that specifies the precision column of
#'   `points_df`.
#'
#' @param np The number of positives. A single number, a numeric vector with
#'   one element per group, or a number/string that specifies a column of
#'   `points_df`.
#'
#' @param nn The number of negatives, in the same three forms as `np`.
#'
#' @param mod_col A number/string that specifies the model column of
#'   `points_df`. `NULL` treats the whole table as one model.
#'
#' @param dsid_col A number/string that specifies the dataset column of
#'   `points_df`. `NULL` treats the whole table as one dataset.
#'
#' @details
#' A table of rates is a set of supporting points on a curve, and the
#'   instances between two adjacent points are missing. `format_points`
#'   puts them back: the rows between two thresholds become instances that
#'   share a score, and precrec spreads the true and false positives of a
#'   tied run evenly over the cutoffs inside it. That even spread is the
#'   non-linear interpolation of Davis and Goadrich (2006), so the
#'   reconstructed curve is the one that interpolation prescribes.
#'
#' The interpolation is exact only where a gap holds a single instance. Over
#'   the wider gaps a threshold table leaves, it assumes the positives and
#'   negatives inside a gap alternate at a constant rate, and the area under
#'   the resulting curve is an estimate whose error the table gives no way to
#'   bound. Per-instance scores and labels remain the input to prefer
#'   wherever they exist.
#'
#' `np` and `nn` are required. Recall and precision determine the class skew
#'   but not the totals, and true and false positive rates determine neither,
#'   so the counts cannot be recovered without them. They also place the
#'   chance-level baseline of the precision-recall curve, and supply the
#'   instances below the lowest threshold when the table stops short of
#'   predicting everything positive.
#'
#' Reconstructed scores are threshold values, not per-instance predictions.
#'   Everything that reads the *ranking* is exact - the curves, the areas, the
#'   basic metrics - but [prob_metrics()] reads the values themselves, and a
#'   Brier score or log loss taken from reconstructed data means nothing.
#'
#' The direction of the threshold column is inferred from the rates. Both
#'   conventions work - a threshold that keeps fewer instances as it rises,
#'   and one that keeps more - and the reconstructed scores are negated for
#'   the second so that a larger score always means a more likely positive.
#'
#' @return The `format_points` function returns a list that contains
#'   scores, labels, model names and dataset IDs.
#'
#' @references
#' Davis J, Goadrich M (2006) The relationship between precision-recall and
#'   ROC curves. \emph{Proceedings of the 23rd International Conference on
#'   Machine Learning}, 233-240. \doi{10.1145/1143844.1143874}
#'
#' @seealso [evalmod()] for calculation evaluation metrics.
#'   [mmdata()] for formatting input data.
#'   [format_nfold()] for cross validation data frames.
#'
#' @examples
#'
#' ##################################################
#' ### A single model, true and false positive rates
#' ###
#'
#' roc_df <- data.frame(
#'   threshold = c(0.9, 0.7, 0.5, 0.3, 0.1),
#'   tpr = c(0.2, 0.5, 0.7, 0.9, 1.0),
#'   fpr = c(0.02, 0.10, 0.25, 0.55, 1.0)
#' )
#'
#' pts1 <- format_points(roc_df,
#'   threshold_col = "threshold",
#'   tpr_col = "tpr", fpr_col = "fpr",
#'   np = 50, nn = 100
#' )
#'
#' evalmod(mmdata(pts1$scores, pts1$labels))
#'
#'
#' ##################################################
#' ### Two models, recall and precision
#' ###
#'
#' prc_df <- data.frame(
#'   model = rep(c("m1", "m2"), each = 4),
#'   threshold = rep(c(0.8, 0.6, 0.4, 0.2), 2),
#'   recall = c(0.3, 0.6, 0.8, 1.0, 0.2, 0.4, 0.7, 1.0),
#'   precision = c(0.9, 0.8, 0.6, 0.4, 0.7, 0.6, 0.5, 0.4)
#' )
#'
#' pts2 <- format_points(prc_df,
#'   threshold_col = "threshold",
#'   rec_col = "recall", prec_col = "precision",
#'   mod_col = "model", np = 40, nn = 60
#' )
#'
#' evalmod(mmdata(pts2$scores, pts2$labels, modnames = pts2$modnames))
#'
#' @export
format_points <- function(points_df, threshold_col,
                          tpr_col = NULL, fpr_col = NULL,
                          rec_col = NULL, prec_col = NULL,
                          np = NULL, nn = NULL,
                          mod_col = NULL, dsid_col = NULL) {
  # Validate arguments and settle which pair of rate columns was given
  input_type <- .validate_format_points_args(
    points_df, threshold_col, tpr_col, fpr_col, rec_col, prec_col,
    np, nn, mod_col, dsid_col
  )

  # Split the table into (model, dataset) groups
  groups <- .split_points_df(points_df, mod_col, dsid_col)

  # Resolve the class totals against those groups
  nps <- .resolve_points_total(np, "np", points_df, groups)
  nns <- .resolve_points_total(nn, "nn", points_df, groups)

  # Reconstruct one group at a time
  expanded <- .map_idx(groups$rows, function(i) {
    .expand_points_group(
      points_df[groups$rows[[i]], , drop = FALSE], threshold_col,
      tpr_col, fpr_col, rec_col, prec_col, input_type,
      nps[[i]], nns[[i]], groups$labels[[i]]
    )
  })

  .inform_points_reconstruction(
    n_inst = sum(.map_dbl(expanded, function(x) length(x[["scores"]]))),
    n_pts = sum(.map_dbl(expanded, function(x) x[["npoints"]])),
    n_grp = length(groups$rows),
    rounding = max(.map_dbl(expanded, function(x) x[["rounding"]]))
  )

  list(
    scores = .map(expanded, function(x) x[["scores"]]),
    labels = .map(expanded, function(x) x[["labels"]]),
    modnames = groups$modnames,
    dsids = groups$dsids
  )
}


#
# Split a points data frame into (model, dataset) groups
#
# Returns the row indices of each group together with the model names and
# dataset IDs that `mmdata()` needs alongside them. Dataset IDs are numbered
# in sorted order of the column values, so a character column works as well
# as a numeric one.
#
.split_points_df <- function(points_df, mod_col, dsid_col) {
  n <- nrow(points_df)

  mod_vec <- if (is.null(mod_col)) {
    rep("m1", n)
  } else {
    as.character(points_df[[.points_col_idx(mod_col, points_df)]])
  }

  dsid_vec <- if (is.null(dsid_col)) {
    rep(1L, n)
  } else {
    raw <- points_df[[.points_col_idx(dsid_col, points_df)]]
    match(as.character(raw), sort(unique(as.character(raw))))
  }

  # Model first, then dataset, so several datasets of one model stay adjacent
  keys <- paste(mod_vec, dsid_vec, sep = "\r")
  ukeys <- unique(keys[order(mod_vec, dsid_vec)])
  rows <- .map(ukeys, function(k) which(keys == k))

  list(
    rows = rows,
    modnames = .map_chr(rows, function(r) mod_vec[[r[[1]]]]),
    dsids = .map_dbl(rows, function(r) as.double(dsid_vec[[r[[1]]]])),
    labels = .map_chr(rows, function(r) {
      paste0(mod_vec[[r[[1]]]], " / ", dsid_vec[[r[[1]]]])
    })
  )
}


#
# Resolve np or nn against the groups of a points data frame
#
# Accepts a single number, one number per group, or a column reference. A
# column must be constant within each group - the totals belong to a test
# set, not to a threshold.
#
.resolve_points_total <- function(total, arg, points_df, groups) {
  ngroups <- length(groups$rows)

  # A string names a column; a number is always a count, so that a small
  # total is never mistaken for a column position.
  if (.is_string(total)) {
    vals <- points_df[[.points_col_idx(total, points_df)]]
    out <- .map_dbl(groups$rows, function(r) as.double(vals[[r[[1]]]]))
    consistent <- .map_lgl(groups$rows, function(r) {
      length(unique(vals[r])) == 1L
    })
    if (!all(consistent)) {
      .stop_invalid_arg(
        paste(
          "Column {.arg {arg}} must hold one value per group,",
          "but it varies within {sum(!consistent)} of them."
        ),
        arg = arg, .envir = environment()
      )
    }
  } else {
    if (length(total) != 1L && length(total) != ngroups) {
      .stop_invalid_arg(
        paste(
          "{.arg {arg}} must be length 1 or length {ngroups},",
          "one per group, not length {length(total)}."
        ),
        arg = arg, .envir = environment()
      )
    }
    out <- rep_len(as.double(total), ngroups)
  }

  for (val in out) {
    .assert_number(val, arg, min = 1, whole = TRUE)
  }

  out
}


#
# Reconstruct the instances of one group
#
.expand_points_group <- function(grp_df, threshold_col,
                                 tpr_col, fpr_col, rec_col, prec_col,
                                 input_type, np, nn, glabel) {
  thresholds <- as.double(grp_df[[.points_col_idx(threshold_col, grp_df)]])

  if (anyDuplicated(thresholds) != 0L) {
    .stop_invalid_arg(
      "Group {glabel} repeats a threshold. Each row must be a distinct one.",
      arg = "threshold_col", .envir = environment()
    )
  }

  # Cumulative counts implied by the rates
  counts <- .points_counts(
    grp_df, tpr_col, fpr_col, rec_col, prec_col, input_type, np, nn, glabel
  )

  # A threshold that keeps fewer instances as it rises is the common
  # convention, but the reverse is what a negated score produces. Take
  # whichever ordering makes the counts run from strictest to loosest.
  ord <- order(thresholds, decreasing = TRUE)
  flip <- !.is_monotone_points(counts, ord)
  if (flip) {
    ord <- rev(ord)
    if (!.is_monotone_points(counts, ord)) {
      .stop_invalid_arg(
        paste(
          "Group {glabel} is not monotone in either threshold direction.",
          "True and false positive counts must both grow as the threshold",
          "admits more instances."
        ),
        arg = "points_df", .envir = environment()
      )
    }
  }

  tp <- counts$tp[ord]
  fp <- counts$fp[ord]
  scores <- if (flip) -thresholds[ord] else thresholds[ord]

  if (tp[[length(tp)]] > np || fp[[length(fp)]] > nn) {
    .stop_invalid_arg(
      paste(
        "Group {glabel} implies more instances than {.arg np} and",
        "{.arg nn} allow."
      ),
      arg = "points_df", .envir = environment()
    )
  }

  # Each gap becomes a run of tied scores; whatever is left over sits below
  # the lowest threshold, so the curve is anchored even when the table stops
  # short of predicting everything positive.
  d_tp <- diff(c(0, tp, np))
  d_fp <- diff(c(0, fp, nn))
  bucket_scores <- c(scores, scores[[length(scores)]] - 1)

  keep <- (d_tp + d_fp) > 0
  list(
    scores = rep(bucket_scores[keep], (d_tp + d_fp)[keep]),
    labels = .flatten(.map(which(keep), function(j) {
      rep(c(1, 0), times = c(d_tp[[j]], d_fp[[j]]))
    })),
    npoints = nrow(grp_df),
    rounding = counts$rounding
  )
}


#
# Turn the rate columns of one group into cumulative counts
#
.points_counts <- function(grp_df, tpr_col, fpr_col, rec_col, prec_col,
                           input_type, np, nn, glabel) {
  if (input_type == "tprfpr") {
    tpr <- .points_rate(grp_df, tpr_col, "tpr_col", glabel)
    fpr <- .points_rate(grp_df, fpr_col, "fpr_col", glabel)
    raw_tp <- tpr * np
    raw_fp <- fpr * nn
  } else {
    rec <- .points_rate(grp_df, rec_col, "rec_col", glabel)
    prec <- .points_rate(grp_df, prec_col, "prec_col", glabel)
    if (any(prec == 0 & rec > 0)) {
      .stop_invalid_arg(
        "Group {glabel} has zero precision at a non-zero recall.",
        arg = "prec_col", .envir = environment()
      )
    }
    raw_tp <- rec * np
    # Precision fixes how many negatives came with those positives
    raw_fp <- ifelse(prec == 0, 0, raw_tp * (1 - prec) / prec)
  }

  tp <- round(raw_tp)
  fp <- round(raw_fp)

  list(
    tp = tp, fp = fp,
    rounding = max(c(0, abs(raw_tp - tp) / np, abs(raw_fp - fp) / nn))
  )
}


#
# Read one rate column of a group and check its range
#
.points_rate <- function(grp_df, col, arg, glabel) {
  vals <- as.double(grp_df[[.points_col_idx(col, grp_df)]])

  if (anyNA(vals) || any(vals < 0) || any(vals > 1)) {
    .stop_invalid_arg(
      "Column {.arg {arg}} of group {glabel} must hold rates from 0 to 1.",
      arg = arg, .envir = environment()
    )
  }

  vals
}


#
# Check that both counts grow along a candidate ordering
#
.is_monotone_points <- function(counts, ord) {
  all(diff(counts$tp[ord]) >= 0) && all(diff(counts$fp[ord]) >= 0)
}


#
# Resolve a column reference to a position
#
.points_col_idx <- function(col, df) {
  if (.is_string(col)) match(col, colnames(df)) else as.integer(col)
}


#
# Report what the reconstruction did
#
# The returned scores and labels are indistinguishable from measured ones
# downstream, so this is where the provenance is stated.
#
.inform_points_reconstruction <- function(n_inst, n_pts, n_grp, rounding) {
  msg <- c(
    paste(
      "Reconstructed {n_inst} instance{?s} from {n_pts} point{?s}",
      "in {n_grp} group{?s}."
    ),
    "i" = paste(
      "Between two points the curve assumes a constant class skew.",
      "Areas read off it are estimates."
    )
  )

  if (rounding > 1e-8) {
    msg <- c(msg, "i" = paste0(
      "Rates were rounded to whole counts, moving one by at most ",
      signif(rounding, 2), "."
    ))
  }

  cli::cli_inform(msg, .envir = environment())
}


#
# Validate arguments of format_points()
#
# Returns which pair of rate columns was given, since every later step
# branches on it.
#
.validate_format_points_args <- function(points_df, threshold_col,
                                         tpr_col, fpr_col, rec_col, prec_col,
                                         np, nn, mod_col, dsid_col) {
  if (!is.data.frame(points_df)) {
    .stop_invalid_arg(
      paste(
        "{.arg points_df} must be a data frame,",
        "not {.obj_type_friendly {points_df}}."
      ),
      arg = "points_df", .envir = environment()
    )
  }

  if (nrow(points_df) == 0L) {
    .stop_invalid_arg(
      "{.arg points_df} must have at least one row.",
      arg = "points_df", .envir = environment()
    )
  }

  has_roc <- !is.null(tpr_col) && !is.null(fpr_col)
  has_prc <- !is.null(rec_col) && !is.null(prec_col)

  if (has_roc == has_prc) {
    .stop_invalid_arg(
      paste(
        "Supply one pair of rate columns: {.arg tpr_col} with",
        "{.arg fpr_col}, or {.arg rec_col} with {.arg prec_col}."
      ),
      arg = "points_df", .envir = environment()
    )
  }

  .validate_col(threshold_col, points_df, "threshold_col", "points_df")
  if (has_roc) {
    .validate_col(tpr_col, points_df, "tpr_col", "points_df")
    .validate_col(fpr_col, points_df, "fpr_col", "points_df")
  } else {
    .validate_col(rec_col, points_df, "rec_col", "points_df")
    .validate_col(prec_col, points_df, "prec_col", "points_df")
  }
  if (!is.null(mod_col)) {
    .validate_col(mod_col, points_df, "mod_col", "points_df")
  }
  if (!is.null(dsid_col)) {
    .validate_col(dsid_col, points_df, "dsid_col", "points_df")
  }

  for (arg in c("np", "nn")) {
    total <- if (arg == "np") np else nn
    if (is.null(total)) {
      .stop_invalid_arg(
        paste(
          "{.arg {arg}} is required. Rates alone do not carry the class",
          "totals, so the counts behind them cannot be recovered."
        ),
        arg = arg, .envir = environment()
      )
    }
    if (.is_string(total)) {
      .validate_col(total, points_df, arg, "points_df")
    } else if (!is.numeric(total)) {
      .stop_invalid_arg(
        paste(
          "{.arg {arg}} must be a number, a number per group, or a column",
          "name, not {.obj_type_friendly {total}}."
        ),
        arg = arg, .envir = environment()
      )
    }
  }

  if (has_roc) "tprfpr" else "recprec"
}

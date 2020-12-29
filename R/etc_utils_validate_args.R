#
# Validate scores and labels
#
.validate_scores_and_labels <- function(obj, obj_name, scores, labels, ...) {

  if (missing(obj) || is.null(obj)) {

    # Check if scores and labels are specified
    if (is.null(scores) && !is.null(labels)) {
      stop("Invalid scores", call. = FALSE)
    } else if (!is.null(scores) && is.null(labels)) {
      stop("Invalid labels", call. = FALSE)
    } else if (is.null(scores) && is.null(labels)) {
      if (is.null(obj)) {
        stop("Invalid scores & labels", call. = FALSE)
      } else {
        stop(paste0(obj_name, " must be specified"), call. = FALSE)
      }
    }

    # Check scores
    .validate_scores(scores)

    # Check labels
    .validate_labels(labels)

    # Check length of scores and labels
    if (length(labels) != length(scores)) {
      stop("scores and labels must be the same lengths", call. = FALSE)
    }

  } else if (!is.null(obj)) {
    # Validate the first argument
    obj <- .validate(obj)
  }

  obj
}

# Check mode
.validate_mode <- function(mode) {
  assertthat::assert_that(assertthat::is.string(mode),
                          (mode == "rocprc"
                           || mode == "basic"
                           || mode == "aucroc"))
}

#
# Validate scores
#
.validate_scores <- function(scores) {
  assertthat::assert_that(is.atomic(scores),
                          is.vector(scores),
                          is.numeric(scores),
                          length(scores) > 0L)
}

#
# Validate labels
#
.validate_labels <- function(labels) {
  assertthat::assert_that(is.atomic(labels),
                          (is.vector(labels) || is.factor(labels)),
                          length(labels) > 0L)
}

#
# Validate modname
#
.validate_modname <- function(modname) {
  if (!is.null(modname)) {
    assertthat::assert_that(assertthat::is.string(modname))
  }
}

#
# Validate modnames
#
.validate_modnames <- function(modnames, datalen) {
  if (!is.null(modnames)) {

    assertthat::assert_that(is.vector(modnames),
                            is.character(modnames),
                            length(modnames) == datalen)

    for (i in seq_len(length(modnames))) {
      .validate_modname(modnames[i])
    }
  }
}

#
# Validate dsid
#
.validate_dsid <- function(dsid) {
  if (!is.null(dsid)) {
    assertthat::assert_that(assertthat::is.number(dsid))
  }
}

#
# Validate dsids
#
.validate_dsids <- function(dsids, datalen) {
  if (!is.null(dsids)) {

    assertthat::assert_that(is.vector(dsids),
                            is.numeric(dsids),
                            length(dsids) == datalen)

    for (i in seq_len(length(dsids))) {
      .validate_dsid(dsids[i])
    }
  }
}

#
# Validate posclass
#
.validate_posclass <- function(posclass) {
  if (!is.null(posclass)) {
    assertthat::assert_that(is.atomic(posclass),
                            (is.vector(posclass) || is.factor(posclass)),
                            length(posclass) == 1L)
  }
}


#
# Validate na_worst
#
.validate_na_worst <- function(na_worst) {
  if (!is.null(na_worst)) {
    assertthat::assert_that(assertthat::is.flag(na_worst),
                            assertthat::noNA(na_worst))
  }
}

#
# Validate ties_method
#
.validate_ties_method <- function(ties_method) {
  if (!is.null(ties_method)) {
    assertthat::assert_that(assertthat::is.string(ties_method))

    choices = c("equiv", "random", "first")
    if (!(ties_method %in% choices)) {
      stop(gettextf("ties_method must be one of %s",
                    paste(dQuote(choices), collapse = ", ")), call. = FALSE)
    }
  }
}

#
# Validate expd_first
#
.validate_expd_first <- function(expd_first) {
  if (!is.null(expd_first)) {
    assertthat::assert_that(assertthat::is.string(expd_first),
                            (expd_first == "modnames"
                             || expd_first == "dsids"))
  }
}

#
# Validate model type
#
.validate_model_type <- function(model_type) {
  if (!is.null(model_type)) {
    assertthat::assert_that(assertthat::is.string(model_type),
                            (model_type == "single"
                             || model_type == "multiple"))
  }
}

#
# Validate data type
#
.validate_data_type <- function(data_type) {
  if (!is.null(data_type)) {
    assertthat::assert_that(assertthat::is.string(data_type),
                            (data_type == "single"
                             || data_type == "multiple"))
  }
}

#
# Validate calc_avg
#
.validate_calc_avg <- function(calc_avg) {
  if (!is.null(calc_avg)) {
    assertthat::assert_that(assertthat::is.flag(calc_avg),
                            assertthat::noNA(calc_avg))
  }
}

#
# Validate cb_alpha
#
.validate_cb_alpha <- function(cb_alpha, calc_avg = NULL) {
  if (!is.null(cb_alpha)) {
    assertthat::assert_that(assertthat::is.number(cb_alpha),
                            cb_alpha >= 0 && cb_alpha <= 1)
    if (!is.null(calc_avg)) {
      if (!calc_avg && cb_alpha) {
        warning("cb_alpha is ignored when calc_avg = FALSE", call. = FALSE)
      }
    }
  }
}

#
# Validate raw_curves
#
.validate_raw_curves <- function(raw_curves, calc_avg = NULL) {
  if (!is.null(raw_curves)) {
    assertthat::assert_that(assertthat::is.flag(raw_curves),
                            assertthat::noNA(raw_curves))
    if (!is.null(calc_avg)) {
      if (!calc_avg && raw_curves) {
        warning("raw_curves is ignored when calc_avg = FALSE", call. = FALSE)
      }
    }
  }
}

#
# Validate x_bins
#
.validate_x_bins <- function(x_bins) {
  if (!is.null(x_bins) && all(!is.na(x_bins))) {
    assertthat::assert_that(assertthat::is.number(x_bins),
                            x_bins %% 1 == 0,
                            x_bins >= 1L)
  }
}

# Check score column names
.validate_score_cols <- function(score_cols, nfold_df) {
  assertthat::assert_that(is.vector(score_cols))
  assertthat::assert_that(is.numeric(score_cols)
                          || is.character(score_cols))

  if (is.numeric(score_cols)) {
    assertthat::assert_that(all(score_cols <= ncol(nfold_df)),
                            msg = "Invalid score_cols")
  } else if (is.character(score_cols)) {
    assertthat::assert_that(all(score_cols %in% colnames(nfold_df)),
                            msg = "Invalid score_cols")
  }

}

# Check label column name
.validate_lab_col <- function(lab_col, nfold_df) {
  assertthat::assert_that(assertthat::see_if(assertthat::is.number(lab_col))
                          || assertthat::see_if(assertthat::is.string(lab_col)))

  if (assertthat::see_if(assertthat::is.number(lab_col))) {
    assertthat::assert_that(lab_col <= ncol(nfold_df),
                            msg = "Invalid lab_col")
  } else if (assertthat::see_if(assertthat::is.string(lab_col))) {
    assertthat::assert_that(lab_col %in% colnames(nfold_df),
                            msg = "Invalid lab_col")
  }
}

# Check fold column name
.validate_fold_col <- function(fold_col, nfold_df) {
  assertthat::assert_that(assertthat::see_if(assertthat::is.number(fold_col))
                          || assertthat::see_if(assertthat::is.string(fold_col)))

  if (assertthat::see_if(assertthat::is.number(fold_col))) {
    assertthat::assert_that(fold_col <= ncol(nfold_df),
                            msg = "Invalid fold_col")
  } else if (assertthat::see_if(assertthat::is.string(fold_col))) {
    assertthat::assert_that(fold_col %in% colnames(nfold_df),
                            msg = "Invalid fold_col")
  }

}

# Check mode
.check_mode <- function(mode, obj = NULL) {
  .validate_mode(mode)
  obj_mode <- attr(obj, "args")[["mode"]]
  if (mode != obj_mode) {
    stop("Invalid mode", call. = FALSE)
  }
}

#
# Check curve types
#
.check_curvetype <- function(curvetype, obj = NULL) {
  roc_prc <- TRUE
  basic_eval <- TRUE

  cfunc <- function(curvetype, all_types, all_len) {
    if (!is.atomic(curvetype) || !is.character(curvetype)
        || length(curvetype) > all_len
        || length(setdiff(curvetype, all_types)) != 0) {
      FALSE
    } else {
      TRUE
    }
  }
  roc_prc <- cfunc(curvetype, c("ROC", "PRC"), 2)
  basic_eval <- cfunc(curvetype, c("score", "label", "error", "accuracy",
                                   "specificity", "sensitivity", "precision",
                                   "mcc", "fscore"),
                      9)

  if (!roc_prc && !basic_eval) {
    stop("Invalid curvetype", call. = FALSE)
  }

  if (!is.null(obj)) {
    obj_mode <- attr(obj, "args")[["mode"]]
    if (((obj_mode == "rocprc") && !roc_prc)
        || ((obj_mode == "basic") && !basic_eval)) {
      stop("Invalid curvetype", call. = FALSE)
    }
  }

}

#
# Check type
#
.check_type <- function(type) {
  if (!is.null(type)) {
    assertthat::assert_that(assertthat::is.string(type),
                            (type == "l" || type == "p" || type == "b"))
  }

}

#
# Check show_cb
#
.check_show_cb <- function(show_cb, obj = NULL) {
  assertthat::assert_that(is.atomic(show_cb),
                          assertthat::is.flag(show_cb),
                          assertthat::noNA(show_cb))

  if (!is.null(obj) && (attr(obj, "dataset_type") == "multiple")) {
    obj_calc_avg <- attr(obj, "args")[["calc_avg"]]
    if (show_cb && !obj_calc_avg ) {
      stop(paste0("calc_avg of the evalmod function",
                  " must be set as TRUE before using show_cb",
                  " of this function"),
           call. = FALSE)
    }
  }
}

#
# Check raw_curves
#
.check_raw_curves <- function(raw_curves, obj = NULL) {
  assertthat::assert_that(is.atomic(raw_curves),
                          assertthat::is.flag(raw_curves),
                          assertthat::noNA(raw_curves))

  if (!is.null(obj) && (attr(obj, "dataset_type") == "multiple")) {
    obj_calc_avg <- attr(obj, "args")[["calc_avg"]]
    obj_raw_curves <- attr(obj, "args")[["raw_curves"]]
    if (raw_curves && (!obj_calc_avg || !obj_raw_curves)) {
      stop(paste0("Both calc_avg and raw_curves of the evalmod function",
                  " must be set as TRUE before using raw_curves",
                  " of this function"),
           call. = FALSE)
    }
  }
}

#
# Check show_legend
#
.check_show_legend <- function(show_legend) {
  assertthat::assert_that(is.atomic(show_legend),
                          assertthat::is.flag(show_legend),
                          assertthat::noNA(show_legend))
}

#
# Check add_np_nn
#
.check_add_np_nn <- function(add_np_nn) {
  assertthat::assert_that(is.atomic(add_np_nn),
                          assertthat::is.flag(add_np_nn),
                          assertthat::noNA(add_np_nn))
}

#
# Check ret_grob
#
.check_ret_grob <- function(ret_grob) {
  assertthat::assert_that(is.atomic(ret_grob),
                          assertthat::is.flag(ret_grob),
                          assertthat::noNA(ret_grob))
}

#
# Check xlim and ylim
#
.check_limits <- function(xlim, ylim) {
  assertthat::assert_that(is.vector(xlim) && is.numeric(xlim),
                          length(xlim) == 2L,
                          xlim[1] >= 0 && xlim[1] <= 1,
                          xlim[2] >= 0 && xlim[2] <= 1,
                          xlim[1] < xlim[2])

  assertthat::assert_that(is.vector(ylim) && is.numeric(ylim),
                          length(ylim) == 2L,
                          ylim[1] >= 0 && ylim[1] <= 1,
                          ylim[2] >= 0 && ylim[2] <= 1,
                          ylim[1] < ylim[2])
}

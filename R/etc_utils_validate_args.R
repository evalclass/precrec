#
# Abort with a precrec condition class
#
# `arg` names the offending argument. The condition carries the classes
# "precrec_error_invalid_<arg>", "precrec_error_invalid_arg" and
# "precrec_error", so callers and tests can match on the cause instead of on
# the message text. `call = NULL` keeps the internal validator out of the
# message, matching the `stop(call. = FALSE)` style used elsewhere.
#
.stop_invalid_arg <- function(msg, arg, .envir = rlang::caller_env()) {
  cli::cli_abort(
    msg,
    class = c(
      paste0("precrec_error_invalid_", arg),
      "precrec_error_invalid_arg",
      "precrec_error"
    ),
    arg = arg,
    call = NULL,
    .envir = .envir
  )
}

#
# Assert an internal invariant
#
# These guard objects that precrec itself builds, so a failure is a bug in the
# package rather than bad user input. Each argument is a condition that must
# evaluate to TRUE; the first one that does not is reported by expression.
#
.assert_internal <- function(..., call = rlang::caller_env()) {
  conds <- rlang::enquos(...)

  for (cond in conds) {
    if (!isTRUE(rlang::eval_tidy(cond))) {
      cli::cli_abort(
        c(
          "Internal check failed: {.code {rlang::as_label(cond)}}",
          "i" = paste(
            "This is a bug in {.pkg precrec}. Please report it at",
            "{.url https://github.com/evalclass/precrec/issues}."
          )
        ),
        class = c("precrec_error_internal", "precrec_error"),
        call = call
      )
    }
  }

  invisible(TRUE)
}

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
      stop("Invalid scores & labels", call. = FALSE)
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


#
# Assert that an argument is a single TRUE or FALSE
#
.assert_flag <- function(x, arg) {
  if (!checkmate::test_flag(x)) {
    .stop_invalid_arg(
      paste(
        "{.arg {arg}} must be {.code TRUE} or {.code FALSE},",
        "not {.obj_type_friendly {x}}."
      ),
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Assert that an argument is a single string, optionally one of `values`
#
# `allow_na` keeps NA acceptable for arguments that use NA as a sentinel,
# such as an unnamed model.
#
.assert_string <- function(x, arg, values = NULL, allow_na = FALSE) {
  # `na.ok` in checkmate accepts an NA of any type, but this package has always
  # required a character one - a logical NA reaching a name argument means the
  # caller passed the wrong thing. `is.character()` keeps that.
  if (!checkmate::test_string(x, na.ok = allow_na) || !is.character(x)) {
    .stop_invalid_arg(
      "{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}.",
      arg = arg, .envir = environment()
    )
  }

  if (is.na(x)) {
    return(invisible(TRUE))
  }

  if (!is.null(values) && !checkmate::test_choice(x, values)) {
    near <- .nearest_value(x, values)
    .stop_invalid_arg(
      .msg_bad_choice(near),
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Build the message for a value outside an allowed set
#
# Returns a cli template rather than a finished string, so `arg`, `x`, `values`
# and `near` are interpolated from the calling assert function's environment -
# which is the one `.stop_invalid_arg()` is handed. Both callers therefore have
# to compute `near` themselves before raising.
#
.msg_bad_choice <- function(near) {
  msg <- "{.arg {arg}} must be one of {.or {.val {values}}}, not {.val {x}}."
  if (!is.null(near)) {
    msg <- c(msg, "i" = "Did you mean {.val {near}}?")
  }

  msg
}

#
# Find the allowed value closest to a rejected one
#
# A typo in a measure name should not send the reader back to the help page to
# scan two dozen alternatives. The threshold scales with the length of what was
# typed, so a short wrong word offers nothing rather than a coincidence: "xx"
# is within 3 edits of plenty of names without resembling any of them.
#
.nearest_value <- function(x, values, max_dist = 3L) {
  if (length(values) == 0L || !.is_string(x) || is.na(x)) {
    return(NULL)
  }

  dists <- utils::adist(x, values, ignore.case = TRUE)[1, ]
  limit <- min(max_dist, max(1L, floor(nchar(x) / 2)))
  if (min(dists) > limit) {
    return(NULL)
  }

  values[[which.min(dists)]]
}

#
# Assert that an argument is one of a set of allowed values
#
# Unlike `.assert_string(values = )` this accepts any atomic type, for
# arguments whose allowed set is not made of strings.
#
.assert_choice <- function(x, arg, values) {
  if (!checkmate::test_choice(x, values)) {
    near <- .nearest_value(x, values)
    .stop_invalid_arg(
      .msg_bad_choice(near),
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Assert that an argument is a vector of one type, optionally of a given length
#
# `is.vector()` rather than a checkmate predicate on purpose: it rejects
# anything carrying an attribute other than names, which is the behaviour the
# callers below have always had, and which `test_atomic_vector()` would relax.
#
.assert_vector <- function(x, arg, type = c("character", "numeric"),
                           len = NULL) {
  type <- match.arg(type)
  is_type <- switch(type,
    character = is.character,
    numeric = is.numeric
  )

  if (!is.vector(x) || !is_type(x)) {
    .stop_invalid_arg(
      paste(
        "{.arg {arg}} must be a {type} vector,",
        "not {.obj_type_friendly {x}}."
      ),
      arg = arg, .envir = environment()
    )
  }

  if (!is.null(len) && length(x) != len) {
    .stop_invalid_arg(
      "{.arg {arg}} must be length {len}, not length {length(x)}.",
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Assert that an argument is a single number, optionally whole and in range
#
# `allow_na` keeps NA acceptable for arguments that use NA as a sentinel.
#
.assert_number <- function(x, arg, min = NULL, max = NULL, whole = FALSE,
                           allow_na = FALSE) {
  # See `.assert_string()` for why the type is checked alongside checkmate:
  # `na.ok` would otherwise let a character NA through as a number.
  if (!checkmate::test_number(x, na.ok = allow_na) || !is.numeric(x)) {
    .stop_invalid_arg(
      "{.arg {arg}} must be a single number, not {.obj_type_friendly {x}}.",
      arg = arg, .envir = environment()
    )
  }

  if (is.na(x)) {
    return(invisible(TRUE))
  }

  # `Inf %% 1` is `NaN`, so the modulo alone would fail with R's own "missing
  # value where TRUE/FALSE needed" instead of a precrec condition.
  if (whole && (!is.finite(x) || x %% 1 != 0)) {
    .stop_invalid_arg(
      "{.arg {arg}} must be a whole number, not {.val {x}}.",
      arg = arg, .envir = environment()
    )
  }

  if ((!is.null(min) && x < min) || (!is.null(max) && x > max)) {
    range_msg <- if (is.null(max)) {
      "{min} or larger"
    } else if (is.null(min)) {
      "{max} or smaller"
    } else {
      "between {min} and {max}"
    }
    .stop_invalid_arg(
      paste0("{.arg {arg}} must be ", range_msg, ", not {.val {x}}."),
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Test whether a value is a single string
#
.is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

#
# Test whether a value is a single number
#
.is_number <- function(x) {
  is.numeric(x) && length(x) == 1L
}

#
# Validate mode
#
.validate_mode <- function(mode) {
  .assert_string(mode, "mode", c("rocprc", "basic", "aucroc"))
}

#
# Validate scores
#
.validate_scores <- function(scores) {
  if (!is.atomic(scores) || !is.vector(scores) || !is.numeric(scores)) {
    .stop_invalid_arg(
      paste(
        "{.arg scores} must be a numeric vector,",
        "not {.obj_type_friendly {scores}}."
      ),
      arg = "scores", .envir = environment()
    )
  }

  if (length(scores) == 0L) {
    .stop_invalid_arg("{.arg scores} must not be empty.", arg = "scores")
  }

  invisible(TRUE)
}

#
# Validate labels
#
.validate_labels <- function(labels) {
  if (!is.atomic(labels) || !(is.vector(labels) || is.factor(labels))) {
    .stop_invalid_arg(
      paste(
        "{.arg labels} must be an atomic vector or a factor,",
        "not {.obj_type_friendly {labels}}."
      ),
      arg = "labels", .envir = environment()
    )
  }

  if (length(labels) == 0L) {
    .stop_invalid_arg("{.arg labels} must not be empty.", arg = "labels")
  }

  invisible(TRUE)
}

#
# Validate modname
#
.validate_modname <- function(modname) {
  if (!is.null(modname)) {
    .assert_string(modname, "modname", allow_na = TRUE)
  }
}

#
# Validate modnames
#
.validate_modnames <- function(modnames, datalen) {
  if (!is.null(modnames)) {
    .assert_vector(modnames, "modnames", "character", len = datalen)
  }

  invisible(TRUE)
}

#
# Validate dsid
#
.validate_dsid <- function(dsid) {
  if (!is.null(dsid)) {
    .assert_number(dsid, "dsid", allow_na = TRUE)
  }
}

#
# Validate dsids
#
.validate_dsids <- function(dsids, datalen) {
  if (!is.null(dsids)) {
    .assert_vector(dsids, "dsids", "numeric", len = datalen)
  }

  invisible(TRUE)
}

#
# Validate posclass
#
.validate_posclass <- function(posclass) {
  if (!is.null(posclass)) {
    if (!is.atomic(posclass) ||
      !(is.vector(posclass) || is.factor(posclass)) ||
      length(posclass) != 1L) {
      .stop_invalid_arg(
        paste(
          "{.arg posclass} must be a single atomic value,",
          "not {.obj_type_friendly {posclass}}."
        ),
        arg = "posclass", .envir = environment()
      )
    }
  }

  invisible(TRUE)
}

#
# Validate na_worst
#
.validate_na_worst <- function(na_worst) {
  if (!is.null(na_worst)) {
    .assert_flag(na_worst, "na_worst")
  }
}

#
# Validate ties_method
#
.validate_ties_method <- function(ties_method) {
  if (!is.null(ties_method)) {
    .assert_string(
      ties_method, "ties_method", c("equiv", "random", "first")
    )
  }
}

#
# Validate expd_first
#
.validate_expd_first <- function(expd_first) {
  if (!is.null(expd_first)) {
    .assert_string(expd_first, "expd_first", c("modnames", "dsids"))
  }
}

#
# Validate calc_avg
#
.validate_calc_avg <- function(calc_avg) {
  if (!is.null(calc_avg)) {
    .assert_flag(calc_avg, "calc_avg")
  }
}

#
# Validate cb_alpha
#
.validate_cb_alpha <- function(cb_alpha, calc_avg = NULL) {
  if (!is.null(cb_alpha)) {
    .assert_number(cb_alpha, "cb_alpha", min = 0, max = 1)

    if (!is.null(calc_avg)) {
      if (!calc_avg && cb_alpha) {
        warning("cb_alpha is ignored when calc_avg = FALSE", call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

#
# Validate raw_curves
#
.validate_raw_curves <- function(raw_curves, calc_avg = NULL) {
  if (!is.null(raw_curves)) {
    .assert_flag(raw_curves, "raw_curves")

    if (!is.null(calc_avg)) {
      if (!calc_avg && raw_curves) {
        warning("raw_curves is ignored when calc_avg = FALSE", call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

#
# Validate x_bins
#
.validate_x_bins <- function(x_bins, allow_zero = FALSE) {
  if (allow_zero) {
    min_x_bin <- 0
  } else {
    min_x_bin <- 1
  }

  if (!is.null(x_bins) && all(!is.na(x_bins))) {
    .assert_number(x_bins, "x_bins", min = min_x_bin, whole = TRUE)
  }

  invisible(TRUE)
}

#
# Validate interpolate
#
.validate_interpolate <- function(interpolate) {
  if (!is.null(interpolate) && all(!is.na(interpolate))) {
    .assert_flag(interpolate, "interpolate")
  }
}

#
# Validate beta of the F-beta score
#
.validate_beta <- function(beta) {
  if (!is.null(beta) && all(!is.na(beta))) {
    .assert_number(beta, "beta", min = 0)
    if (is.infinite(beta)) {
      .stop_invalid_arg("{.arg beta} must be finite.", arg = "beta")
    }
  }

  invisible(TRUE)
}

#
# Validate on_single_class
#
.validate_on_single_class <- function(on_single_class) {
  .assert_string(on_single_class, "on_single_class", c("error", "na"))

  invisible(TRUE)
}

#
# Validate eps of the log loss
#
.validate_eps <- function(eps) {
  .assert_number(eps, "eps", min = 0, max = 0.5)

  invisible(TRUE)
}

#
# Validate score column names
#
.validate_score_cols <- function(score_cols, nfold_df) {
  if (!is.vector(score_cols) ||
    !(is.numeric(score_cols) || is.character(score_cols))) {
    .stop_invalid_arg(
      paste(
        "{.arg score_cols} must be a numeric or character vector,",
        "not {.obj_type_friendly {score_cols}}."
      ),
      arg = "score_cols", .envir = environment()
    )
  }

  if (is.numeric(score_cols)) {
    if (!all(score_cols <= ncol(nfold_df))) {
      .stop_invalid_arg(
        paste(
          "{.arg score_cols} must index columns of {.arg nfold_df},",
          "which has {ncol(nfold_df)} column{?s}."
        ),
        arg = "score_cols", .envir = environment()
      )
    }
  } else if (!all(score_cols %in% colnames(nfold_df))) {
    .stop_invalid_arg(
      "{.arg score_cols} must name columns of {.arg nfold_df}.",
      arg = "score_cols", .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Validate a single column reference, either a position or a name
#
.validate_col <- function(col, nfold_df, arg) {
  if (!.is_number(col) && !.is_string(col)) {
    .stop_invalid_arg(
      paste(
        "{.arg {arg}} must be a single column position or name,",
        "not {.obj_type_friendly {col}}."
      ),
      arg = arg, .envir = environment()
    )
  }

  if (.is_number(col)) {
    if (col > ncol(nfold_df)) {
      .stop_invalid_arg(
        paste(
          "{.arg {arg}} must index a column of {.arg nfold_df},",
          "which has {ncol(nfold_df)} column{?s}."
        ),
        arg = arg, .envir = environment()
      )
    }
  } else if (!(col %in% colnames(nfold_df))) {
    .stop_invalid_arg(
      "{.arg {arg}} must name a column of {.arg nfold_df}.",
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

#
# Validate label column name
#
.validate_lab_col <- function(lab_col, nfold_df) {
  .validate_col(lab_col, nfold_df, "lab_col")
}

#
# Validate fold column name
#
.validate_fold_col <- function(fold_col, nfold_df) {
  .validate_col(fold_col, nfold_df, "fold_col")
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
# Check the misclassification costs
#
# A cost is a weight on an error count, so it has to be a single finite
# number and cannot be negative - a negative cost would reward the mistake it
# is weighting. The two are checked together because they are only ever
# passed together.
#
.validate_costs <- function(cost_fp, cost_fn) {
  .assert_number(cost_fp, "cost_fp", min = 0)
  .assert_number(cost_fn, "cost_fn", min = 0)

  invisible(TRUE)
}

#
# Validate the arguments that describe the input data
#
# `evalmod()` and `metric_curve()` both hand these five to `mmdata()`, and
# both check them before they get there. One helper rather than two identical
# blocks: an argument added to `mmdata()`'s front door is then checked the
# same way by both.
#
.validate_data_args <- function(modnames, dsids, posclass, na_worst,
                                ties_method) {
  .validate_modnames(modnames, length(modnames))
  .validate_dsids(dsids, length(dsids))
  .validate_posclass(posclass)
  .validate_na_worst(na_worst)
  .validate_ties_method(ties_method)

  invisible(TRUE)
}

#
# Check curve types
#
.check_curvetype <- function(curvetype, obj = NULL) {
  roc_prc <- TRUE
  basic_eval <- TRUE

  cfunc <- function(curvetype, all_types, all_len) {
    if (!is.atomic(curvetype) || !is.character(curvetype) ||
      length(curvetype) > all_len ||
      length(setdiff(curvetype, all_types)) != 0) {
      FALSE
    } else {
      TRUE
    }
  }
  roc_prc <- cfunc(curvetype, c("ROC", "PRC"), 2)
  basic_names <- .get_metric_names("basic_all")
  basic_eval <- cfunc(curvetype, basic_names, length(basic_names))

  if (!roc_prc && !basic_eval) {
    stop("Invalid curvetype", call. = FALSE)
  }

  if (!is.null(obj)) {
    obj_mode <- attr(obj, "args")[["mode"]]
    if (((obj_mode == "rocprc") && !roc_prc) ||
      ((obj_mode == "basic") && !basic_eval)) {
      stop("Invalid curvetype", call. = FALSE)
    }
    if (obj_mode == "basic") {
      .check_curvetype_held(curvetype, obj)
    }
  }
}

#
# Check that the object holds the measures being asked for
#
# A measure this package knows but that was not calculated is a different
# mistake from a measure that does not exist, and it has a different fix:
# `evalmod(metrics = )` decides which measures an object carries, so the
# error says so rather than repeating the list of valid names.
#
.check_curvetype_held <- function(curvetype, obj) {
  held <- .get_obj_metrics(obj)
  missing_types <- setdiff(curvetype, held)
  if (length(missing_types) == 0L) {
    return(invisible(TRUE))
  }

  .stop_invalid_arg(
    c(
      paste(
        "{.arg curvetype} names {.val {missing_types}}, which",
        "{?is/are} not among the measures this object holds."
      ),
      "i" = paste(
        "Ask for {?it/them} with",
        "{.code evalmod(metrics = {.val {missing_types}})}."
      )
    ),
    arg = "curvetype", .envir = environment()
  )
}

#
# Check type
#
.check_type <- function(type) {
  if (!is.null(type)) {
    .assert_string(type, "type", c("l", "p", "b"))
  }
}

#
# Check show_cb
#
.check_show_cb <- function(show_cb, obj = NULL) {
  .assert_flag(show_cb, "show_cb")

  if (!is.null(obj) && (attr(obj, "dataset_type") == "multiple")) {
    obj_calc_avg <- attr(obj, "args")[["calc_avg"]]
    if (show_cb && !obj_calc_avg) {
      .stop_invalid_arg(
        paste(
          "calc_avg of the evalmod function must be set as TRUE",
          "before using show_cb of this function"
        ),
        arg = "show_cb"
      )
    }
  }

  invisible(TRUE)
}

#
# Check raw_curves
#
.check_raw_curves <- function(raw_curves, obj = NULL) {
  .assert_flag(raw_curves, "raw_curves")

  if (!is.null(obj) && (attr(obj, "dataset_type") == "multiple")) {
    obj_calc_avg <- attr(obj, "args")[["calc_avg"]]
    obj_raw_curves <- attr(obj, "args")[["raw_curves"]]
    if (raw_curves && (!obj_calc_avg || !obj_raw_curves)) {
      .stop_invalid_arg(
        paste(
          "Both calc_avg and raw_curves of the evalmod function must be set",
          "as TRUE before using raw_curves of this function"
        ),
        arg = "raw_curves"
      )
    }
  }

  invisible(TRUE)
}

#
# Check show_legend
#
.check_show_legend <- function(show_legend) {
  .assert_flag(show_legend, "show_legend")
}

#
# Check add_np_nn
#
.check_add_np_nn <- function(add_np_nn) {
  .assert_flag(add_np_nn, "add_np_nn")
}

#
# Check ret_grob
#
.check_ret_grob <- function(ret_grob) {
  .assert_flag(ret_grob, "ret_grob")
}

#
# Check multiplot_lib
#
.check_multiplot_lib <- function(multiplot_lib) {
  .assert_string(multiplot_lib, "multiplot_lib", c("patchwork", "grid"))
}

#
# Check xlim and ylim
#
.check_limits <- function(xlim, ylim) {
  .check_limit(xlim, "xlim")
  .check_limit(ylim, "ylim")
}

#
# Check a single pair of axis limits
#
.check_limit <- function(lim, arg) {
  if (!is.vector(lim) || !is.numeric(lim)) {
    .stop_invalid_arg(
      "{.arg {arg}} must be a numeric vector, not {.obj_type_friendly {lim}}.",
      arg = arg, .envir = environment()
    )
  }

  if (length(lim) != 2L) {
    .stop_invalid_arg(
      "{.arg {arg}} must be length 2, not length {length(lim)}.",
      arg = arg, .envir = environment()
    )
  }

  if (anyNA(lim) || any(lim < 0) || any(lim > 1)) {
    .stop_invalid_arg(
      "{.arg {arg}} must be between 0 and 1.",
      arg = arg, .envir = environment()
    )
  }

  if (lim[1] >= lim[2]) {
    .stop_invalid_arg(
      paste(
        "{.arg {arg}} must be increasing,",
        "but {.val {lim[1]}} is not less than {.val {lim[2]}}."
      ),
      arg = arg, .envir = environment()
    )
  }

  invisible(TRUE)
}

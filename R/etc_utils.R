#
# Check if an internal Rcpp function returns en error
#
.check_cpp_func_error <- function(obj, func_name) {
  if (obj[["errmsg"]] != "") {
    stop(paste0(
      "Internal cpp function (", func_name, "()) failed: ",
      obj[["errmsg"]]
    ), call. = FALSE)
  }
}

#
# Get a specified object
#
.get_obj <- function(obj, obj_name) {
  if (is.null(obj_name) || is.null(obj) || methods::is(obj, obj_name)) {
    obj
  } else {
    .get_obj(attr(obj, "src"), obj_name)
  }
}

#
# Get an argument of the specified source object
#
.get_obj_arg <- function(obj, obj_name, arg_name) {
  if (!is.null(obj_name) && !is.na(obj_name)) {
    obj <- .get_obj(obj, obj_name)
  }
  obj_args <- attr(obj, "args")
  if (is.null(obj_args)) {
    NULL
  } else {
    obj_args[[arg_name]]
  }
}

#
# Use scores and labels to create obj
#
.create_src_obj <- function(obj, obj_name, func, scores, labels,
                            ...) {
  if (missing(obj)) {
    if (!is.null(scores) && !is.null(labels)) {
      obj <- func(scores = scores, labels = labels, ...)
    } else {
      stop("The first argument must be specified.", call. = FALSE)
    }
  }

  obj
}

#
# Map the basic evaluation measures to their internal short names
#
# One ordered list, used everywhere a measure is named: the columns the C++
# layer returns, the slots of the points object, the rows of the summary
# `print` shows, and the default panels of `plot()` and `autoplot()`. Keeping
# it in one place is what stops those from drifting apart when a measure is
# added.
#
.basic_metric_names <- function() {
  c(
    score = "score", label = "label", error = "err",
    accuracy = "acc", specificity = "sp", sensitivity = "sn",
    precision = "prec", mcc = "mcc", fscore = "fscore",
    balanced_accuracy = "bacc", npv = "npv",
    informedness = "infm", markedness = "mkd", kappa = "kappa"
  )
}

#
# Measures that run from -1 to 1 rather than from 0 to 1
#
# They share an axis range and an aspect ratio in the plots, so the plotting
# helpers ask this rather than each carrying its own list.
#
.is_signed_metric <- function(curvetype) {
  curvetype %in% c("label", "mcc", "informedness", "markedness", "kappa")
}

#
# Get the plot title of a basic evaluation measure
#
# Capitalising the name covers most of them; the rest are acronyms or need
# the units spelled out.
#
.get_metric_title <- function(curvetype) {
  # The plot code reads the curve type out of a data frame column, where it is
  # a factor. Indexing a named vector by a factor picks the integer level code
  # instead of the name, so coerce before the lookup below.
  curvetype <- as.character(curvetype)

  titles <- c(
    label = "Label (1:pos, -1:neg)", mcc = "MCC", npv = "NPV",
    balanced_accuracy = "Balanced accuracy"
  )
  if (curvetype %in% names(titles)) {
    return(unname(titles[curvetype]))
  }

  paste0(toupper(substring(curvetype, 1, 1)), substring(curvetype, 2))
}

#
# Get names of evaluation metrics
#
.get_metric_names <- function(mode) {
  if (mode == "rocprc" || mode == "prcroc") {
    mnames <- c("ROC", "PRC")
  } else if (mode == "basic") {
    mnames <- names(.basic_metric_names())
  }

  mnames
}

#
# Hand a plain data frame back at the public boundary
#
# The tables are data.tables internally, but `as.data.frame` and the
# accessors keep their base contract: printing, `[` semantics and
# copy-on-modify stay what callers have always seen.
#
# setDF() converts by reference, so a table that is stored somewhere - an
# attribute the caller could mutate through - has to be copied first. A
# table that was just built for this call does not.
#
.as_plain_df <- function(x, copy = FALSE) {
  if (!data.table::is.data.table(x)) {
    return(x)
  }
  if (copy) {
    x <- data.table::copy(x)
  }
  data.table::setDF(x)
}

#
# Bind tables collected inside a loop into one data.table
#
# The callers used to grow a data frame with rbind() on every pass, which
# copies everything built so far each time. rbindlist() binds once.
#
.rbind_parts <- function(parts) {
  if (length(parts) == 0L) {
    return(NULL)
  }
  data.table::rbindlist(parts)
}

#
# Check partial match - distribution used for a CI calculation
#
.pmatch_dtype <- function(dtype) {
  .assert_string(dtype, "dtype")

  dtype_tab <- c("normal", "z", "t")
  dtype_match <- pmatch(tolower(dtype), dtype_tab)
  if (!is.na(dtype_match)) {
    dtype <- dtype_tab[dtype_match]
  }
  if (!(dtype %in% dtype_tab)) {
    .stop_invalid_arg(
      paste(
        "{.arg dtype} must be one of {.or {.val {dtype_tab}}},",
        "not {.val {dtype}}."
      ),
      arg = "dtype", .envir = environment()
    )
  }

  dtype
}

#
# Calculate a confidence interval of per-dataset values
#
# Shared by `auc_ci()` and `prob_metrics_ci()`: same normal or t interval,
# differing only in where the measure is allowed to sit. A single dataset has
# no spread to estimate, so the interval collapses onto the value itself.
# `n` is the number of datasets the interval was built from, which is not the
# number supplied when some of them could not be evaluated.
#
.calc_ci_stats <- function(values, alpha, dtype, lower = -Inf, upper = Inf) {
  # A dataset that could not be evaluated - a fold holding a single class,
  # say - carries NA. Leaving it out is what makes the interval a statement
  # about the datasets that were evaluated; `n` counts those.
  values <- values[!is.na(values)]
  val_mean <- mean(values)
  val_n <- length(values)
  if (val_n == 0L) {
    return(list(
      mean = NA_real_, error = NA_real_,
      lower_bound = NA_real_, upper_bound = NA_real_, n = 0L
    ))
  }
  if (val_n < 2) {
    return(list(
      mean = val_mean, error = 0,
      lower_bound = val_mean, upper_bound = val_mean, n = val_n
    ))
  }

  if (dtype == "t") {
    val_q <- qt(1 - (alpha / 2), df = val_n - 1)
  } else {
    val_q <- qnorm(1 - (alpha / 2))
  }
  val_error <- val_q * sd(values) / sqrt(val_n)

  list(
    mean = val_mean,
    error = val_error,
    lower_bound = max(val_mean - val_error, lower),
    upper_bound = min(val_mean + val_error, upper),
    n = val_n
  )
}

#
# Is this a one-vs-rest decomposition of a multiclass dataset?
#
# The class column of `data_info` is what says so. It travels with the object
# from `mmdata()` through the pipeline, which saves every result class an
# attribute of its own.
#
.is_multiclass <- function(obj) {
  "classes" %in% names(attr(obj, "data_info"))
}

#
# Consume arguments a method accepts but cannot act on
#
# Some `fortify` methods take an argument that does not apply to the object
# they are given - point reduction is only defined for `mode = "rocprc"`, and
# a single dataset has no average to contrast a raw curve with. The arguments
# stay in the signature so every method shares one interface. The ggplot2
# `fortify` generic runs `rlang::check_dots_used()` though, which reports an
# argument whose promise is never forced as a possible misspelling, so the
# methods hand the ones they ignore to this helper to force them.
#
.ignore_unused_args <- function(...) {
  invisible(list(...))
}

#
# Describe a dataset that holds only one class
#
# The three pipelines word the first sentence differently - what cannot be
# calculated is not the same in each - but the rest of the message names the
# dataset the same way.
#
.single_class_msg <- function(fmdat, what) {
  if (attr(fmdat, "np") > 0) {
    cl <- "positive"
  } else {
    cl <- "negative"
  }

  paste0(
    what, " Only a single class (", cl, ") found in dataset (modname: ",
    attr(fmdat, "modname"), ", dsid: ", attr(fmdat, "dsid"), ")."
  )
}

#
# Number of columns of a multi-panel figure
#
# Shared by the base-R and the ggplot2 sides so that a set of measures is
# laid out the same way whichever one draws it.
#
.get_plot_ncol <- function(nplots) {
  if (nplots <= 3) {
    nplots
  } else if (nplots == 4) {
    2
  } else if (nplots <= 9) {
    3
  } else {
    4
  }
}

#
# Load data.table
#
.load_data_table <- function() {
  loaded <- TRUE
  if (!requireNamespace("data.table", quietly = TRUE)) {
    loaded <- FALSE
  }
  loaded
}

#
# Get negative and positive numbers
#
.get_pn_info <- function(object) {
  nps <- attr(object, "data_info")[["np"]]
  nns <- attr(object, "data_info")[["nn"]]

  is_consistant <- TRUE
  prev_np <- NA
  prev_nn <- NA
  np_tot <- 0
  nn_tot <- 0
  n <- 0
  for (i in seq_along(nps)) {
    np <- nps[i]
    nn <- nns[i]

    if ((!is.na(prev_np) && np != prev_np) ||
      (!is.na(prev_nn) && nn != prev_nn)) {
      is_consistant <- FALSE
    }

    np_tot <- np_tot + np
    nn_tot <- nn_tot + nn
    prev_np <- np
    prev_nn <- nn
    n <- n + 1
  }

  avg_np <- np_tot / n
  avg_nn <- nn_tot / n

  prc_base <- avg_np / (avg_np + avg_nn)

  list(
    avg_np = avg_np, avg_nn = avg_nn, is_consistant = is_consistant,
    prc_base = prc_base
  )
}

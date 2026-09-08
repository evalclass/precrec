#
# Map a function over a list or a vector
#
# The `.map_*` family replaces the `apply` calls this package used to make. The
# names follow purrr's, so the call sites read the same way, but nothing is
# imported: purrr would pull in vctrs, lifecycle and magrittr, and the reason
# to reach for it here is readability rather than anything base R cannot do.
#
# The typed variants are the point of the family. `lapply()` says nothing about
# what comes back, so a helper that quietly returns a list of length two for one
# input and a list of length one for another is only caught downstream, if at
# all. `.map_dbl()` and its siblings fail at the call that broke.
#
.map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

.map_dbl <- function(.x, .f, ...) {
  vapply(.x, .f, double(1), ...)
}

.map_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}

.map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

.map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}

#
# Map over the indices of a list
#
# Several call sites need the position rather than the element - they index a
# second list with it, or build an attribute out of it. Written out, that is
# `lapply(seq_along(x), ...)`, which says "index" only after the reader has
# checked what `seq_along()` was given.
#
.map_idx <- function(.x, .f, ...) {
  lapply(seq_along(.x), .f, ...)
}

#
# Keep the elements of a list that satisfy a predicate
#
.keep <- function(.x, .p, ...) {
  .x[.map_lgl(.x, .p, ...)]
}

#
# Flatten one level of nesting
#
# `unlist(x, recursive = FALSE)` reads as "make this a vector" until the second
# argument is noticed, and it is the second argument that carries the meaning.
#
.flatten <- function(.x) {
  unlist(.x, recursive = FALSE)
}

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
# The basic evaluation metrics, with everything that is known about each
#
# One ordered table, used everywhere a metric is named: the columns the C++
# layer returns and the ones derived from them in R, the slots of the points
# object, the rows of the summary `print` shows, the panel titles, the axis
# range each metric needs, and the default panels of `plot()` and
# `autoplot()`. Keeping it in one place is what stops those from drifting
# apart when a metric is added.
#
# `default` is what an object holds when `evalmod()` is not told otherwise.
# The metrics added for ROCR, yardstick and scikit-learn parity are FALSE:
# turning them on by default would take an existing caller from twelve panels
# to twenty-nine, and make every `evalmod(mode = "basic")` call carry
# seventeen more vectors it was not asked for. `evalmod(metrics = )` is
# how they are turned on.
#
# `range` is the y axis a metric needs: "unit" for [0, 1], "signed" for
# [-1, 1], and "free" for one that is unbounded above and has to be read off
# the data.
#
.basic_metric_table <- function() {
  data.frame(
    name = c(
      "score", "label", "error", "accuracy", "specificity", "sensitivity",
      "precision", "mcc", "fscore", "balanced_accuracy", "npv",
      "informedness", "markedness", "kappa", "fpr", "fnr",
      "false_discovery_rate", "false_omission_rate",
      "predicted_positive_rate", "predicted_negative_rate", "lift", "odds",
      "mi", "chisq", "cost", "sar", "roc_dist", "sedi", "jaccard",
      "positive_likelihood_ratio", "negative_likelihood_ratio"
    ),
    short = c(
      "score", "label", "err", "acc", "sp", "sn", "prec", "mcc", "fscore",
      "bacc", "npv", "infm", "mkd", "kappa", "fpr", "fnr", "fdr", "for",
      "ppr", "pnr", "lift", "odds", "mi", "chisq", "cost", "sar",
      "rocdist", "sedi", "jacc", "lrp", "lrn"
    ),
    desc = c(
      "score", "label", "error rate", "accuracy", "specificity",
      "sensitivity", "precision", "Matthews correlation coefficient",
      "F-score", "balanced accuracy", "negative predictive value",
      "informedness (Youden's J)", "markedness", "Cohen's kappa",
      "false positive rate", "false negative rate", "false discovery rate",
      "false omission rate", "rate of positive predictions",
      "rate of negative predictions", "lift", "odds ratio",
      "mutual information (bits)", "chi-square statistic",
      "misclassification cost",
      "mean of accuracy, AUC(ROC) and 1 - RMSE",
      "distance to the perfect point in ROC space",
      "symmetric extremal dependence index",
      "Jaccard index (critical success index)",
      "positive likelihood ratio", "negative likelihood ratio"
    ),
    default = c(rep(TRUE, 14), rep(FALSE, 17)),
    range = c(
      "free", "signed", "unit", "unit", "unit", "unit", "unit", "signed",
      "unit", "unit", "unit", "signed", "signed", "signed", "unit", "unit",
      "unit", "unit", "unit", "unit", "free", "free", "unit", "free",
      "free", "unit", "free", "signed", "unit", "free", "free"
    ),
    stringsAsFactors = FALSE
  )
}

#
# Other names a metric answers to
#
# ROCR ships its own identifier for most of these, and several have a standard
# abbreviation that is shorter than the name this package settled on. Both are
# accepted wherever a metric is named, so a call written against ROCR keeps
# working and nobody has to spell out `false_discovery_rate` to plot it.
#
.basic_metric_aliases <- function() {
  c(
    fall = "fpr",
    miss = "fnr",
    fdr = "false_discovery_rate",
    pcfall = "false_discovery_rate",
    "for" = "false_omission_rate",
    pcmiss = "false_omission_rate",
    ppr = "predicted_positive_rate",
    rpp = "predicted_positive_rate",
    pnr = "predicted_negative_rate",
    rnp = "predicted_negative_rate",
    odds_ratio = "odds",
    mutual_information = "mi"
  )
}

#
# Map the basic evaluation metrics to their internal short names
#
# With no argument this is the default set, which is what every caller that
# does not know about `evalmod(metrics = )` wants.
#
.basic_metric_names <- function(metrics = NULL) {
  tab <- .basic_metric_table()
  if (is.null(metrics)) {
    tab <- tab[tab$default, ]
  } else {
    tab <- tab[tab$name %in% metrics, ]
  }
  stats::setNames(tab$short, tab$name)
}

#
# Resolve a `metrics` argument to the metrics an object should hold
#
# The default metrics are always included: an object that dropped them would
# break every plot, summary and data frame that names one, and the point of
# the argument is to add the metrics ROCR has, not to take the existing ones
# away. `"all"` is the whole table.
#
.resolve_metrics <- function(metrics) {
  tab <- .basic_metric_table()
  if (is.null(metrics)) {
    return(tab$name[tab$default])
  }

  if (identical(metrics, "all")) {
    return(tab$name)
  }

  .assert_vector(metrics, "metrics", "character")
  metrics <- .pmatch_metric_names(metrics)
  for (m in metrics) {
    .assert_choice(m, "metrics", tab$name)
  }

  tab$name[tab$default | tab$name %in% metrics]
}

#
# Resolve metric aliases to the name this package uses
#
.pmatch_metric_names <- function(metrics) {
  aliases <- .basic_metric_aliases()
  hit <- match(metrics, names(aliases))
  metrics[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  metrics
}

#
# The basic metrics an object actually holds
#
# `evalmod(metrics = )` decides this per object, so the plot and summary code
# has to ask the object rather than the table. An object built before the
# argument existed carries no attribute and holds the default set.
#
.get_obj_metrics <- function(obj) {
  metrics <- attr(obj, "metrics")
  if (is.null(metrics)) {
    metrics <- .get_metric_names("basic")
  }
  metrics
}

#
# The axis range a metric needs
#
# "unit" runs from 0 to 1, "signed" from -1 to 1, and "free" is unbounded -
# the score, the lift and the odds ratio, which have to be read off the data.
#
# Both the name and the internal short name are accepted, because the base-R
# plotting code indexes the points object and so holds the short one while
# the ggplot2 code holds the name.
#
.metric_range <- function(curvetype) {
  tab <- .basic_metric_table()
  curvetype <- as.character(curvetype)
  idx <- match(curvetype, tab$name)
  short <- match(curvetype, tab$short)
  idx[is.na(idx)] <- short[is.na(idx)]
  ifelse(is.na(idx), "unit", tab$range[idx])
}

#
# Get the plot title of a basic evaluation metric
#
# Capitalizing the name covers most of them; the rest are acronyms.
#
.get_metric_title <- function(curvetype) {
  # The plot code reads the curve type out of a data frame column, where it is
  # a factor. Indexing a named vector by a factor picks the integer level code
  # instead of the name, so coerce before the lookup below.
  curvetype <- as.character(curvetype)

  titles <- c(
    label = "Label (1:pos, -1:neg)", mcc = "MCC", npv = "NPV",
    fpr = "FPR", fnr = "FNR", odds = "Odds ratio",
    mi = "Mutual information", chisq = "Chi-square", sar = "SAR",
    roc_dist = "ROC distance", rocdist = "ROC distance", sedi = "SEDI",
    jaccard = "Jaccard index", jacc = "Jaccard index",
    lrp = "Positive likelihood ratio", lrn = "Negative likelihood ratio"
  )
  if (curvetype %in% names(titles)) {
    return(unname(titles[curvetype]))
  }

  curvetype <- gsub("_", " ", curvetype)
  paste0(toupper(substring(curvetype, 1, 1)), substring(curvetype, 2))
}

#
# Get names of evaluation metrics
#
# "basic" is the default set, which is what the plot and validation code has
# always meant by it; "basic_all" is every metric the table knows.
#
.get_metric_names <- function(mode) {
  if (mode == "rocprc" || mode == "prcroc") {
    mnames <- c("ROC", "PRC")
  } else if (mode == "basic") {
    mnames <- names(.basic_metric_names())
  } else if (mode == "basic_all") {
    mnames <- .basic_metric_table()$name
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
# differing only in where the metric is allowed to sit. A single dataset has
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
# Shared by the base-R and the ggplot2 sides so that a set of metrics is
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

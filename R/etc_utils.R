#
# Check if an internal Rcpp function returns en error
#
.check_cpp_func_error <- function(obj, func_name) {

  if (obj[["errmsg"]] != "") {
    stop(paste0("Internal cpp function (", func_name, "()) failed: ",
                obj[["errmsg"]]), call. = FALSE)
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
# Get names of evaluation metrics
#
.get_metric_names <- function(mode) {
  if (mode == "rocprc" || mode == "prcroc") {
    mnames <- c("ROC", "PRC")
  } else if (mode == "basic") {
    mnames <-  c("score", "label", "error", "accuracy", "specificity",
                 "sensitivity", "precision", "mcc", "fscore")
  }

  mnames
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

    if ((!is.na(prev_np) && np != prev_np)
        ||  (!is.na(prev_nn) && nn != prev_nn)) {
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

  list(avg_np = avg_np, avg_nn = avg_nn, is_consistant = is_consistant,
       prc_base = prc_base)

}


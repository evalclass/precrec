#
# Convert a fmdat object to a data frame for ggplot2
#
fortify.fmdat <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  data.frame(x = model[["labels"]], y = model[["ranks"]])
}

#
# Convert a cmats object to a data frame for ggplot2
#
fortify.cmats <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  n <- length(model[["ranks"]])
  data.frame(x = rep(seq_len(length(model[["ranks"]])), 4),
             y = c(model[["tp"]], model[["fn"]],
                   model[["fp"]], model[["tn"]]),
             group = factor(c(rep("TPs", n), rep("FNs", n),
                              rep("FPs", n), rep("TNs", n)),
                            levels = c("TPs", "FNs",
                                       "FPs", "TNs")))
}

#
# Convert a pevals object to a data frame for ggplot2
#
fortify.pevals <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  pb <- model[["basic"]]
  n <- length(pb[["error"]])
  data.frame(x = rep(1:n, 10),
             y = c(pb[["score"]], pb[["label"]],
                   pb[["error"]], pb[["accuracy"]],
                   pb[["specificity"]], pb[["sensitivity"]],
                   1 - pb[["specificity"]], pb[["precision"]],
                   pb[["mcc"]], pb[["fscore"]]),
             group = factor(c(rep("score", n),
                              rep("label", n),
                              rep("error", n),
                              rep("accuracy", n),
                              rep("specificity", n),
                              rep("sensitivity", n),
                              rep("1 - specificity", n),
                              rep("precision", n),
                              rep("mcc", n),
                              rep("fscore", n)),
                            levels = c("score", "label",
                                       "error", "accuracy",
                                       "specificity",
                                       "sensitivity",
                                       "1 - specificity",
                                       "precision",
                                       "mcc",
                                       "fscore")))
}

#' @rdname fortify
#' @export
fortify.sscurves <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {
  .dataframe_common(model, raw_curves = TRUE, reduce_points = reduce_points,
                    check_ggplot = TRUE, ...)
}

#' @rdname fortify
#' @export
fortify.mscurves <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {
  .dataframe_common(model, raw_curves = TRUE, reduce_points = reduce_points,
                    check_ggplot = TRUE, ...)
}

#' @rdname fortify
#' @export
fortify.smcurves <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {

  arglist <- .get_fortify_arglist(attr(model, "args"),
                                  def_raw_curves = raw_curves, ...)

  .dataframe_common(model, raw_curves = arglist[["raw_curves"]],
                    reduce_points = reduce_points, check_ggplot = TRUE, ...)
}

#' @rdname fortify
#' @export
fortify.mmcurves <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {

  arglist <- .get_fortify_arglist(attr(model, "args"),
                                  def_raw_curves = raw_curves, ...)

  .dataframe_common(model, raw_curves = arglist[["raw_curves"]],
                    reduce_points = reduce_points, check_ggplot = TRUE, ...)
}

#' @rdname fortify
#' @export
fortify.sspoints <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {
  .dataframe_common(model, mode = "basic", raw_curves = TRUE,
                    check_ggplot = TRUE, reduce_points = FALSE, ...)
}

#' @rdname fortify
#' @export
fortify.mspoints <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {
  .dataframe_common(model, mode = "basic", raw_curves = TRUE,
                    check_ggplot = TRUE, reduce_points = FALSE, ...)
}

#' @rdname fortify
#' @export
fortify.smpoints <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {

  arglist <- .get_fortify_arglist(attr(model, "args"),
                                  def_raw_curves = raw_curves, ...)

  .dataframe_common(model, mode = "basic", raw_curves = arglist[["raw_curves"]],
                    check_ggplot = TRUE, reduce_points = FALSE, ...)
}

#' @rdname fortify
#' @export
fortify.mmpoints <- function(model, raw_curves = NULL, reduce_points = FALSE,
                             ...) {

  arglist <- .get_fortify_arglist(attr(model, "args"),
                                  def_raw_curves = raw_curves, ...)

  .dataframe_common(model, mode = "basic", raw_curves = arglist[["raw_curves"]],
                    check_ggplot = TRUE, reduce_points = FALSE, ...)
}

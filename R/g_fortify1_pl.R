#
# Convert a fmdat object to a data frame for ggplot2
#
fortify.fmdat <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  df <- data.frame(x = model[["labels"]],
                   y = model[["ranks"]])
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
  df <- data.frame(x = rep(1:length(model[["ranks"]]), 4),
                   y = c(model[["tp"]],
                         model[["fn"]],
                         model[["fp"]],
                         model[["tn"]]),
                   group = factor(c(rep("TPs", n), rep("FNs", n),
                                    rep("FPs", n), rep("TNs", n)),
                                  levels = c("TPs", "FNs", "FPs", "TNs")))
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
  n <- length(model[["error"]])
  df <- data.frame(x = rep(1:n, 6),
                   y = c(model[["error"]],
                         model[["accuracy"]],
                         model[["specificity"]],
                         model[["sensitivity"]],
                         1 - model[["specificity"]],
                         model[["precision"]]),
                   group = factor(c(rep("error", n),
                                    rep("accuracy", n),
                                    rep("specificity", n),
                                    rep("sensitivity", n),
                                    rep("1 - specificity", n),
                                    rep("precision", n)),
                                  levels = c("error", "accuracy",
                                             "specificity", "sensitivity",
                                             "1 - specificity",
                                             "precision")))
}

#
# Convert a roc_curve object to a data frame for ggplot2
#
fortify.roc_curve <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Prepare a data frame for ggplot2 ===
  df <- data.frame(x = model[["x"]], y = model[["y"]])

}

#
# Convert a prc_curve object to a data frame for ggplot2
#
fortify.prc_curve <- function(model, ...) {
  fortify.roc_curve(model, data, ...)
}

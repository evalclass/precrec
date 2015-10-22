#
# Common interface for .fortify_curves
#
.fortify_common <- function(model, ...) {
  # === Check package availability  ===
  .load_ggplot2()

  # === Validate input arguments ===
  .validate(model)

  # === Create dataframe ===
  .fortify_curves(model, ...)
}

#
# Make a dataframe for plotting
#
.fortify_curves <- function(obj, all_curves = TRUE, ...) {

  # Prepare variables
  uniq_modnames <- attr(obj, "uniq_modnames")
  uniq_dsids <- attr(obj, "uniq_dsids")
  modnames <- attr(obj, "data_info")[["modnames"]]
  dsids <- attr(obj, "data_info")[["dsids"]]
  curvetype_names <- list(ROC = "rocs", PRC = "prcs")

  # Make dsis-modname pairs
  i <- 1
  dsid_modnames <- vector(mode = "character",
                          length = length(uniq_modnames) * length(uniq_dsids))
  for (modname in uniq_modnames) {
    for (dsid in uniq_dsids) {
      dsid_modnames[i] <- paste(modname, dsid, sep = ":")
      i <- i + 1
    }
  }

  # Create curve_df
  if (all_curves) {
    curve_df <- .fortify_regular(obj, uniq_modnames, uniq_dsids, modnames,
                                 dsids, dsid_modnames, curvetype_names)
  } else {
    curve_df <- .fortify_average(obj, uniq_modnames, uniq_dsids, modnames,
                                 dsids, dsid_modnames, curvetype_names)
  }

  curve_df
}

#
# Make a dataframe for plotting with regular curves
#
.fortify_regular <- function(obj, uniq_modnames, uniq_dsids, modnames, dsids,
                             dsid_modnames, curvetype_names) {
  curve_df <- NULL
  for (curvetype in names(curvetype_names)) {
    curves <- obj[[curvetype_names[[curvetype]]]]
    for (i in seq_along(curves)) {
      x <- curves[[i]][["x"]]
      y <- curves[[i]][["y"]]

      modname <- factor(rep(modnames[i], length(x)), levels = uniq_modnames)
      dsid <- factor(rep(dsids[i], length(x)), levels = uniq_dsids)
      dsid_modname <- factor(rep(paste(modnames[i], dsids[i], sep = ":"),
                                 length(x)),
                             levels = dsid_modnames)
      curvename <- factor(rep(curvetype, length(x)),
                          levels = names(curvetype_names))
      curve_df <- rbind(curve_df, data.frame(x = x, y = y, modname = modname,
                                             dsid = dsid,
                                             dsid_modname = dsid_modname,
                                             curvetype = curvename))
    }
  }

  curve_df
}

#
# Make a dataframe for plotting with average curves
#
.fortify_average <- function(obj, uniq_modnames, uniq_dsids, modnames, dsids,
                             dsid_modnames, curvetype_names) {
  curve_df <- NULL
  for (curvetype in names(curvetype_names)) {
    curves <- obj[[curvetype_names[[curvetype]]]]
    avgcurves <- attr(curves, "avgcurves")

    for (i in seq_along(avgcurves)) {
      x <- avgcurves[[i]][["x"]]
      y <- avgcurves[[i]][["y_avg"]]
      ymin <- avgcurves[[i]][["y_ci_l"]]
      ymax <- avgcurves[[i]][["y_ci_h"]]

      modname <- factor(rep(uniq_modnames[i], length(x)),
                        levels = uniq_modnames)
      curvename <- factor(rep(curvetype, length(x)),
                          levels = names(curvetype_names))
      curve_df <- rbind(curve_df, data.frame(x = x, y = y,
                                             ymin = ymin, ymax = ymax,
                                             modname = modname,
                                             curvetype = curvename))
    }
  }

  curve_df
}

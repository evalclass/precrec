# The benchmark cases.
#
# Each case names the C++ entry point it is meant to exercise, so that the
# E4 optimisation work has an obvious before/after target. See
# .claude/plans/enhancements-2026.md.

# Cases that run over every dataset in the catalogue.
#
# Each returns a zero-argument function, already closed over its inputs, so
# that setup work is not timed.
bench_cases <- function(d) {
  mdat <- mmdata(d[["scores"]], d[["labels"]])
  curves <- evalmod(mdat)
  points <- evalmod(mdat, mode = "basic")

  list(
    # get_score_ranks / order_scores_desc
    mmdata = function() mmdata(d[["scores"]], d[["labels"]]),

    # create_roc_curve, create_prc_curve, interpolate_prc
    evalmod_rocprc = function() evalmod(mdat),

    # calc_basic_metrics
    evalmod_basic = function() evalmod(mdat, mode = "basic"),

    # calc_uauc_frank
    evalmod_aucroc = function() evalmod(mdat, mode = "aucroc"),

    # convert_curve_df
    as_data_frame_rocprc = function() as.data.frame(curves),

    # convert_curve_df, basic curvetypes
    as_data_frame_basic = function() as.data.frame(points),

    # calc_auc over the stored curves
    auc = function() auc(curves)
  )
}

# Cases that need several datasets per model, for the averaging paths.
bench_avg_cases <- function(m) {
  mdat <- mmdata(
    join_scores(m[["scores"]]), join_labels(m[["labels"]]),
    expd_first = "dsids"
  )
  avg <- evalmod(mdat, calc_avg = TRUE, raw_curves = FALSE)

  list(
    # calc_avg_curve
    evalmod_avg_rocprc = function() {
      evalmod(mdat, calc_avg = TRUE, raw_curves = FALSE)
    },

    # calc_avg_points
    evalmod_avg_basic = function() {
      evalmod(mdat, mode = "basic", calc_avg = TRUE, raw_curves = FALSE)
    },

    # convert_curve_avg_df
    as_data_frame_avg = function() as.data.frame(avg, raw_curves = FALSE)
  )
}

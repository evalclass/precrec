// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP precrec_calc_auc(SEXP, SEXP);
extern SEXP precrec_calc_avg_curve(SEXP, SEXP, SEXP);
extern SEXP precrec_calc_avg_points(SEXP, SEXP);
extern SEXP precrec_calc_basic_measures(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_calc_uauc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_calc_uauc_frank(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_convert_curve_avg_df(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_convert_curve_df(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_create_confusion_matrices(SEXP, SEXP, SEXP);
extern SEXP precrec_create_prc_curve(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_create_roc_curve(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP precrec_format_labels(SEXP, SEXP);
extern SEXP precrec_get_score_ranks(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"precrec_calc_auc",                  (DL_FUNC) &precrec_calc_auc,                  2},
  {"precrec_calc_avg_curve",            (DL_FUNC) &precrec_calc_avg_curve,            3},
  {"precrec_calc_avg_points",           (DL_FUNC) &precrec_calc_avg_points,           2},
  {"precrec_calc_basic_measures",       (DL_FUNC) &precrec_calc_basic_measures,       6},
  {"precrec_calc_uauc",                 (DL_FUNC) &precrec_calc_uauc,                 6},
  {"precrec_calc_uauc_frank",           (DL_FUNC) &precrec_calc_uauc_frank,           7},
  {"precrec_convert_curve_avg_df",      (DL_FUNC) &precrec_convert_curve_avg_df,      5},
  {"precrec_convert_curve_df",          (DL_FUNC) &precrec_convert_curve_df,          8},
  {"precrec_create_confusion_matrices", (DL_FUNC) &precrec_create_confusion_matrices, 3},
  {"precrec_create_prc_curve",          (DL_FUNC) &precrec_create_prc_curve,          5},
  {"precrec_create_roc_curve",          (DL_FUNC) &precrec_create_roc_curve,          5},
  {"precrec_format_labels",             (DL_FUNC) &precrec_format_labels,             2},
  {"precrec_get_score_ranks",           (DL_FUNC) &precrec_get_score_ranks,           3},
  {NULL, NULL, 0}
};

void R_init_precrec(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

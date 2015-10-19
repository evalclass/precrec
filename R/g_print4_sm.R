# #
# # Print the summary of ROC and Precision-Recall curves with multiple datasets
# #
# #' @export
# print.smcurves <- function(x, ...) {
#   # === Validate input arguments ===
#   .validate(x)
#
#   # === Print summary ===
#   print(x[["rocs"]])
#   print(x[["prcs"]])
# }
#
# #
# # Print smroc
# #
# print.smroc <- function(x, ...) {
#   cat("\n  ROC curves\n\n")
#   .print_smcurves_base(x, ...)
# }
#
# #
# # Print smprc
# #
# print.smprc <- function(x, ...) {
#   # === Print summary ===
#   cat("\n  Precision-Recall curves\n\n")
#   .print_smcurves_base(x, ...)
# }
#
# #
# # Print smcurves
# #
# .print_smcurves_base <- function(x, ...) {
#   # === Validate input arguments ===
#   .validate(x)
#
#   # === Print summary ===
#   for (i in seq_along(x)) {
#     cat("    Dataset ID:", attr(x[[i]], "dsid"), "\n")
#     cat("           AUC:", attr(x[[i]], "auc"), "\n\n")
#   }
# }

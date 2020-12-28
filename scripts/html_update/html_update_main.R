# # https://stackoverflow.com/questions/1815606
# # @return full path to this script
# csf <- function() {
#   # http://stackoverflow.com/a/32016824/2292993
#   cmdArgs = commandArgs(trailingOnly = FALSE)
#   needle = "--file="
#   match = grep(needle, cmdArgs)
#   if (length(match) > 0) {
#     # Rscript via command line
#     return(normalizePath(sub(needle, "", cmdArgs[match])))
#   } else {
#     ls_vars = ls(sys.frames()[[1]])
#     if ("fileName" %in% ls_vars) {
#       # Source'd via RStudio
#       return(normalizePath(sys.frames()[[1]]$fileName))
#     } else {
#       if (!is.null(sys.frames()[[1]]$ofile)) {
#         # Source'd via R console
#         return(normalizePath(sys.frames()[[1]]$ofile))
#       } else {
#         # RStudio Run Selection
#         # http://stackoverflow.com/a/35842176/2292993
#         return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
#       }
#     }
#   }
# }
#
# src_path <- dirname(csf())
# source(paste(sep = "/", src_path, "html_tables.R"))
# source(paste(sep = "/", src_path, "html_update_funcs.R"))
#
# get_site_path <- function(subdir = "reference") {
#   normalizePath(paste(sep = "/", src_path, "../../docs", subdir))
# }
#
# get_file_path <- function(file_name, subdir = "reference") {
#   normalizePath(paste(sep = "/", get_site_path(subdir), file_name))
# }
#
# update_main <- function(call_pkgdown = FALSE) {
#   if (call_pkgdown) {
#     library(pkgdown)
#     pkgdown::build_site(run_dont_run = TRUE)
#   }
#
#   # update_html(get_file_path("precrec.html"),
#   #             c("six functions",
#   #               "eight different"),
#   #             c("precrec_funcs", "precrec_s3"))
#
#   # update_html(get_file_path("evalmod.html"),
#   #             c("one of the following",
#   #               "one of the following"),
#   #             c("mode_rocprcr", "mode_basic"))
#
#   # update_html(get_file_path("plot.html"),
#   #             c("ROC and Precision-Recall ",
#   #               "Basic evaluation",
#   #               "ROC and Precision-Recall ",
#   #               "Basic evaluation"),
#   #             c("mode_rocprcr", "mode_basic", "curve_type", "curve_type_basic"))
#
#   # update_html(get_file_path("autoplot.html"),
#   #             c("ROC and Precision-Recall ",
#   #               "Basic evaluation",
#   #               "ROC and Precision-Recall ",
#   #               "Basic evaluation"),
#   #             c("mode_rocprcr", "mode_basic", "curve_type", "curve_type_basic"))
#
#   # update_html(get_file_path("fortify.html"),
#   #             c("ROC and Precision-Recall ",
#   #               "Basic evaluation"),
#   #             c("mode_rocprcr", "mode_basic"))
#
#   # update_html(get_file_path("as.data.frame.html"),
#   #             c("ROC and Precision-Recall ",
#   #               "Basic evaluation",
#   #               "Fast AUC"),
#   #             c("mode_rocprcr", "mode_basic", "mode_aucroc"))
#
#   # update_html(get_file_path("auc.html"),
#   #             "accepts the following",
#   #             "mode_rocprcr")
#
#   # update_html(get_file_path("pauc.html"),
#   #             "accepts the following",
#   #             "mode_rocprcr")
#
#   # update_html(get_file_path("part.html"),
#   #             c("accepts the following",
#   #               "following curve"),
#   #             c("mode_rocprcr", "curve_type"))
# }
#
# update_main(call_pkgdown = TRUE)

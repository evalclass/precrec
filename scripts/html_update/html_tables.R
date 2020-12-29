# get_html_table <- function(tab_name) {
#   func_name <- get(paste0("get_tab_", tab_name))
#   func_name()
# }
#
# get_tab_precrec_funcs <- function() {
#   c(
#     "<table class=\"table\">",
#     "  <tr>",
#     "    <td><strong>Function</strong> </td>",
#     "    <td> <strong>Description</strong></td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='evalmod.html'>evalmod</a></code> </td>",
#     "    <td> Main function to calculate evaluation measures </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='mmdata.html'>mmdata</a></code></td>",
#     "    <td> Reformat input data for performance evaluation calculation </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='join_scores.html'>join_scores</a></code> </td>",
#     "    <td> Join scores of multiple models into a list </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='join_labels.html'>join_labels</a></code> </td>",
#     "    <td> Join observed labels of multiple test datasets into a list </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='create_sim_samples.html'>create_sim_samples</a></code> </td>",
#     "    <td> Randomly creates test datasets with different performance levels </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='format_nfold.html'>format_nfold</a></code> </td>",
#     "    <td> Create n-fold cross validation dataset from data frame </td>",
#     "  </tr>",
#     "</table>"
#   )
# }
#
# get_tab_precrec_s3 <- function() {
#   c(
#     "<table class=\"table\">",
#     "  <tr>",
#     "    <td> <strong>S3 generic</strong> </td>",
#     "    <td> <strong>Library</strong> </td>",
#     "    <td> <strong>Description</strong> </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code>print</code> </td>",
#     "    <td> base </td>",
#     "    <td> Print the calculation results and the summary of the test data </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='as.data.frame.html'>as.data.frame</a></code> </td>",
#     "    <td> base </td>",
#     "    <td> Convert a precrec object to a data frame </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='plot.html'>plot</a></code> </td>",
#     "    <td> graphics  </td>",
#     "    <td> Plot performance evaluation measures </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='autoplot.html'>autoplot</a></code> </td>",
#     "    <td> ggplot2 </td>",
#     "    <td> Plot performance evaluation measures with ggplot2  </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='fortify.html'>fortify</a></code>  </td>",
#     "    <td> ggplot2 </td>",
#     "    <td> Prepare a data frame for ggplot2 </td>",
#     "  </tr><tr>",
#     "    <td><code><a href='auc.html'>auc</a></code> </td>",
#     "    <td> precrec   </td>",
#     "    <td> Make a data frame with AUC scores </td>",
#     "  </tr>",
#     "  <tr>",
#     "    <td><code><a href='part.html'>part</a></code> </td>",
#     "    <td> precrec   </td>",
#     "    <td> Calculate partial curves and partial AUC scores </td>",
#     "  </tr>",
#     "</table>"
#   )
# }
#
# get_tab_mode_rocprcr <- function() {
#   c(
#     "<table class=\"table\">",
#     "   <tr>",
#     "      <td><strong>S3 object</strong> </td>",
#     "      <td><strong># of models</strong> </td>",
#     "      <td><strong># of test datasets</strong> </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> sscurves </td>",
#     "      <td> single   </td>",
#     "      <td> single   </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> mscurves </td>",
#     "      <td> multiple </td>",
#     "      <td> single   </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> smcurves </td>",
#     "      <td> single   </td>",
#     "      <td> multiple </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> mmcurves </td>",
#     "      <td> multiple </td>",
#     "      <td> multiple </td>",
#     "   </tr>",
#     "</table>"
#   )
# }
#
# get_tab_mode_basic <- function() {
#   c(
#     "<table class=\"table\">",
#     "   <tr>",
#     "      <td><strong>S3 object</strong> </td>",
#     "      <td><strong># of models</strong> </td>",
#     "      <td><strong># of test datasets</strong> </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> sspoints </td>",
#     "      <td> single   </td>",
#     "      <td> single   </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> mspoints </td>",
#     "      <td> multiple </td>",
#     "      <td> single   </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> smpoints </td>",
#     "      <td> single   </td>",
#     "      <td> multiple </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> mmpoints </td>",
#     "      <td> multiple </td>",
#     "      <td> multiple </td>",
#     "   </tr>",
#     "</table>"
#   )
# }
#
# get_tab_mode_aucroc <- function() {
#   c(
#     "<table class=\"table\">",
#     "   <tr>",
#     "      <td><strong><code>S3</code> object</strong></td>",
#     "      <td> <strong># of models</strong></td>",
#     "      <td> <strong># of test datasets</strong> </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td>aucroc</td>",
#     "      <td> - </td>",
#     "      <td> - </td>",
#     "   </tr>",
#     "</table>"
#   )
# }
#
# get_tab_curve_type <- function() {
#   c(
#     "<table class=\"table\">",
#     "   <tr>",
#     "      <td><strong>curvetype</strong> </td>",
#     "      <td><strong>description</strong> </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> ROC </td>",
#     "      <td> ROC curve </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> PRC </td>",
#     "      <td> Precision-Recall curve </td>",
#     "   </tr>",
#     "</table>"
#   )
# }
#
# get_tab_curve_type_basic <- function() {
#   c(
#     "<table class=\"table\">",
#     "   <tr>",
#     "      <td><strong>curvetype</strong> </td>",
#     "      <td><strong>description</strong> </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> error </td>",
#     "      <td> Normalized ranks vs. error rate </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> accuracy </td>",
#     "      <td> Normalized ranks vs. accuracy </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> specificity </td>",
#     "      <td> Normalized ranks vs. specificity </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> sensitivity </td>",
#     "      <td> Normalized ranks vs. sensitivity </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> precision </td>",
#     "      <td> Normalized ranks vs. precision </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> mcc </td>",
#     "      <td> Normalized ranks vs. Matthews correlation coefficient </td>",
#     "   </tr>",
#     "   <tr>",
#     "      <td> fscore </td>",
#     "      <td> Normalized ranks vs. F-Score </td>",
#     "   </tr>",
#     "</table>"
#   )
# }

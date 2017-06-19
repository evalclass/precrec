#include <Rcpp.h>
#include <string>
#include "sort_indices_by_scores.h"

//
// Calculate AUC (ROC) with the U statistic
//
// [[Rcpp::export]]
Rcpp::List calc_uauc_frank(unsigned np, unsigned nn,
                           const Rcpp::NumericVector& scores,
                           const Rcpp::IntegerVector& olabs,
                           const bool& na_last,
                           const std::string& ties_method,
                           Rcpp::Function frank) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;
  double ranksum = 0;
  double ustat = 0;
  double np_dbl = (double)np;
  double nn_dbl = (double)nn;

  // Rank scores
  Rcpp::NumericVector ranks = frank(scores);

  // Calculate U statistic
  for (unsigned i = 0; i < olabs.size(); i++) {
    if (olabs[i] == 2) {
      ranksum += ranks[i];
    }
  }
  ustat = ranksum - np_dbl * (np_dbl + 1) / 2;

  // Calculate AUC
  auc = ustat / (np_dbl * nn_dbl);
  if (auc < 0) {
    auc = 0;
  } else if (auc > 1) {
    auc = 1;
  }

  // Return a list
  ret_val["auc"] = auc;
  ret_val["ustat"] = ustat;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

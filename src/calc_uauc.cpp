#include <Rcpp.h>
#include <string>
#include "sort_indices_by_scores.h"

//
// Calculate AUC (ROC) with the U statistic
//
// [[Rcpp::export]]
Rcpp::List calc_uauc(unsigned np, unsigned nn,
                     const Rcpp::NumericVector& scores,
                     const Rcpp::IntegerVector& olabs,
                     const bool& na_worst,
                     const std::string& ties_method) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;
  double ranksum = 0;
  double ustat = 0;
  double np_dbl = (double)np;
  double nn_dbl = (double)nn;

  int idx = 0;

  // Variables
  std::vector<int> ranks(scores.size());
  std::vector<int> rank_idx(scores.size());

  // Sort neg scores
  std::vector<int> sorted_idx(scores.size());
  sort_indices(sorted_idx, scores, na_worst, ties_method, false);

  // Calculate U statistic
  for (unsigned i = 0; i < sorted_idx.size(); ++i) {
    if (olabs[sorted_idx[i]] == 2) {
      ranksum += (i + 1);
    }
  }
  ustat = ranksum - np_dbl * (np_dbl + 1) / 2;

  // Calculate AUC
  auc = ustat / (np_dbl * nn_dbl);

  // Return a list
  ret_val["auc"] = auc;
  ret_val["ranksum"] = ranksum;
  ret_val["ustat"] = ustat;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

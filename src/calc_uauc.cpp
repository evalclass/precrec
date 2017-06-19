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
  double ustat = 0;
  double np_dbl = (double)np;
  double nn_dbl = (double)nn;

  // Determin NA values
  double na_val;
  if (na_worst) {
    na_val = DBL_MIN;
  } else {
    na_val = DBL_MAX;
  }

  // Create pos and neg vectors
  std::vector<double> pos_vec(np);
  std::vector<double> neg_vec(nn);
  unsigned pos_idx = 0;
  unsigned neg_idx = 0;
  double s;
  for (unsigned i = 0; i < olabs.size(); ++i) {
    if (Rcpp::NumericVector::is_na(scores[i])) {
      s = na_val;
    } else {
      s = scores[i];
    }

    if (olabs[i] == 2) {
      pos_vec[pos_idx] = s;
      pos_idx++;
    } else {
      neg_vec[neg_idx] = s;
      neg_idx++;
    }

  }

  // Sort neg scores
  std::sort(neg_vec.begin(), neg_vec.end());
  std::sort(pos_vec.begin(), pos_vec.end());

  // Calculate U statistic
  pos_idx = 0;
  neg_idx = 0;
  while (pos_idx < pos_vec.size()) {
    if (neg_idx < neg_vec.size() && pos_vec[pos_idx] >= neg_vec[neg_idx]) {
      neg_idx++;
    } else {
      ustat += (double)neg_idx;
      pos_idx++;
    }
  }

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

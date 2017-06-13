#include <Rcpp.h>
#include <string>

//
// Calculate AUC (ROC) with the U statistic
//
// [[Rcpp::export]]
Rcpp::List calc_uauc(int np, int nn,
                     const Rcpp::NumericVector& scores,
                     const Rcpp::IntegerVector& olabs,
                     const bool& na_worst,
                     const std::string& ties_method) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;
  int idx = 0;

  // Calculate AUC
  for (int i = 0; i < olabs.size(); ++i) {
    double score = scores[i];
    if (olabs[i] == 1) {
      ++auc;
    }
  }

  auc = auc / (np + nn);

  // Return a list
  ret_val["auc"] = auc;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

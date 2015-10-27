#include <Rcpp.h>
#include <string>

//
// Calculate area under the curve
//
// [[Rcpp::export]]
Rcpp::List calc_auc(const Rcpp::NumericVector& xs,
                    const Rcpp::NumericVector& ys) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;

  // Calculate AUC
  for (int i = 1; i < xs.size(); ++i) {
    if (xs[i] > xs[i-1]) {
      auc += 0.5 * (ys[i] + ys[i-1]) * (xs[i] - xs[i-1]);
    } else if (xs[i] < xs[i-1]) {
      errmsg = "invalid-x-vals";
    }
  }

  // Return a list
  ret_val["auc"] = auc;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}


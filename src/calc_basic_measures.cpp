#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

//
// Calculate confusion matrices for all possible threshold values
//
// [[Rcpp::export]]
Rcpp::List calc_basic_measures(int np,
                               int nn,
                               const Rcpp::NumericVector& tps,
                               const Rcpp::NumericVector& fps,
                               const Rcpp::NumericVector& tns,
                               const Rcpp::NumericVector& fns) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  int n = tps.size();               // Input data size
  std::vector<double> threshold(n); // Error-rate
  std::vector<double> errrate(n);   // Error-rate
  std::vector<double> acc(n);       // Accuracy
  std::vector<double> sp(n);        // Specificity
  std::vector<double> sn(n);        // Sensitivity
  std::vector<double> prec(n);      // Precision
  std::vector<double> mcc(n);       // Matthews correlation coefficient
  double tpfp, tpfn, tnfp, tnfn;    // For mcc calculation

  // Vector size must be >1
  if (n < 2) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate evaluation measures for all thresholds
  // n should be >1
  for (int i = 0; i < n; ++i) {
    threshold[i] = double(i) / double(n - 1);
    errrate[i] = (fps[i] + fns[i]) / (np + nn);
    acc[i] = 1 - errrate[i];
    sp[i] = tns[i] / nn;
    sn[i] = tps[i] / np;
    if (i > 0) {
      prec[i] = tps[i] / (tps[i] + fps[i]);
    }

    tpfp = tps[i] + fps[i];
    tpfn = tps[i] + fns[i];
    tnfp = tns[i] + fps[i];
    tnfn = tns[i] + fns[i];

    if (tpfp == 0 || tpfn == 0 || tnfp == 0 || tnfn == 0) {
      mcc[i] = ::NA_REAL;
    } else {
      mcc[i] = ((tps[i] * tns[i]) - (fps[i] * fns[i]))
               / ::sqrt(tpfp * tpfn * tnfp * tnfn);
    }
  }

  // Update the precision value of the highest rank
  prec[0] = prec[1];

  // Return a list with P, N, and basic evaluation measures
  df["threshold"] = threshold;
  df["error"] = errrate;
  df["accuracy"] = acc;
  df["specificity"] = sp;
  df["sensitivity"] = sn;
  df["precision"] = prec;
  df["mcc"] = mcc;

  ret_val["basic"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

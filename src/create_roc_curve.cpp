#include <Rcpp.h>
#include <vector>
#include <string>

// Prototype
int interpolate_roc(const Rcpp::NumericVector& sp,
                    const Rcpp::NumericVector& sn,
                    int idx,
                    double x_interval,
                    std::vector<double>& fpr,
                    std::vector<double>& tpr,
                    int n);

//
// Calculate confusion matrices for all possible threshold values
//
// [[Rcpp::export]]
Rcpp::List create_roc_curve(const Rcpp::NumericVector& tps,
                            const Rcpp::NumericVector& fps,
                            const Rcpp::NumericVector& sp,
                            const Rcpp::NumericVector& sn,
                            double x_bins) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  double x_interval = 1.0 / x_bins;
  int max_n = sp.size() + (1.0 / x_interval);
  std::vector<double> fpr(max_n);           // False positive rate
  std::vector<double> tpr(max_n);           // True positive rate
  std::vector<bool> roc_orig(max_n, false); // true: original point

  int n = 0;

  // Interval must be >0
  if (x_interval <= 0) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate ROC points
  for (int i = 0; i < sp.size(); ++i) {
    if ((i != 0) && (sp[i] == sp[i-1])  && (sn[i] == sn[i-1])) {
      continue;
    }

    // Interpolate two points
    if ((i != 0) && ((sp[i-1] - sp[i]) > x_interval)) {
      n = interpolate_roc(sp, sn, i, x_interval, fpr, tpr, n);
    }

    fpr[n] = 1 - sp[i];
    tpr[n] = sn[i];
    roc_orig[n] = true;
    ++n;
  }

  fpr.resize(n);
  tpr.resize(n);
  roc_orig.resize(n);

  // Return a list
  df["x"] = fpr;
  df["y"] = tpr;
  df["orig_points"] = roc_orig;

  ret_val["curve"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

// Linearly interpolate two ROC points
int interpolate_roc(const Rcpp::NumericVector& sp,
                    const Rcpp::NumericVector& sn,
                    int idx,
                    double x_interval,
                    std::vector<double>& fpr,
                    std::vector<double>& tpr,
                    int n) {

  double cur_fpr = 1 - sp[idx];
  double prev_fpr = 1 - sp[idx-1];
  double slope = (sn[idx] - sn[idx-1]) / (cur_fpr - prev_fpr);
  double y_interval = slope * x_interval;
  double tmp_fpr = x_interval * int(prev_fpr / x_interval);
  double tmp_tpr = sn[idx-1] + (tmp_fpr - prev_fpr) * slope;

  while (tmp_fpr < 1) {
    tmp_fpr += x_interval;
    if (tmp_fpr >= cur_fpr){
      break;
    }
    tmp_tpr += y_interval;
    if ((fpr[n-1] == tmp_fpr)  && (tpr[n-1] == tmp_tpr)) {
      continue;
    }
    fpr[n] = tmp_fpr;
    tpr[n] = tmp_tpr;
    ++n;
  }

  return n;

}

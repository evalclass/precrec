#include <Rcpp.h>
#include <vector>
#include <string>

// Prototype
int interpolate_prc(const Rcpp::NumericVector& tps,
                    const Rcpp::NumericVector& fps,
                    const Rcpp::NumericVector& sn,
                    const Rcpp::NumericVector& pr,
                    int idx,
                    double x_interval,
                    std::vector<double>& rec,
                    std::vector<double>& prec,
                    int n);

//
// Calculate precision-recall curve
//
// [[Rcpp::export]]
Rcpp::List create_prc_curve(const Rcpp::NumericVector& tps,
                            const Rcpp::NumericVector& fps,
                            const Rcpp::NumericVector& sn,
                            const Rcpp::NumericVector& pr,
                            double x_bins) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  double x_interval = 1.0 / x_bins;
  int max_n = sn.size() + (1.0 / x_interval);
  std::vector<double> rec(max_n);           // Recall
  std::vector<double> prec(max_n);          // Precision
  std::vector<bool> prc_orig(max_n, false); // true: original point

  int n = 0;

  // Interval must be >0
  if (x_interval <= 0) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate Precision-Recall points
  for (int i = 0; i < sn.size(); ++i) {
    if ((i != 0) && (sn[i] == sn[i-1])  && (pr[i] == pr[i-1])) {
      continue;
    }

    // Interpolate two points
    if (i != 0) {
      n = interpolate_prc(tps, fps, sn, pr, i, x_interval, rec, prec, n);
    }

    rec[n] = sn[i];
    prec[n] = pr[i];
    prc_orig[n] = true;

    ++n;
  }

  rec.resize(n);
  prec.resize(n);
  prc_orig.resize(n);

  // Return a list
  df["x"] = rec;
  df["y"] = prec;
  df["orig_points"] = prc_orig;

  ret_val["curve"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

// Non-linearly interpolate two Precision-Recall points
int interpolate_prc(const Rcpp::NumericVector& tps,
                    const Rcpp::NumericVector& fps,
                    const Rcpp::NumericVector& sn,
                    const Rcpp::NumericVector& pr,
                    int idx,
                    double x_interval,
                    std::vector<double>& rec,
                    std::vector<double>& prec,
                    int n) {

  double tmp_rec = x_interval * int(sn[idx-1] / x_interval);
  double tmp_prec;
  double x;

  while (tmp_rec < 1) {
    tmp_rec += x_interval;
    if (tmp_rec >= sn[idx]){
      break;
    }

    if (pr[idx] == pr[idx-1]) {
      tmp_prec = pr[idx];
    } else {
      x = (tmp_rec - sn[idx-1]) * tps[idx] / sn[idx];
      tmp_prec = (tps[idx-1] + x) / (tps[idx-1] + x
                                     + fps[idx-1]
                                     + (((fps[idx] - fps[idx-1]) * x)
                                        / (tps[idx] - tps[idx-1])));
    }

    if ((rec[n-1] == tmp_rec)  && (prec[n-1] == tmp_prec)) {
      continue;
    }
    rec[n] = tmp_rec;
    prec[n] = tmp_prec;

    ++n;
  }

  return n;
}

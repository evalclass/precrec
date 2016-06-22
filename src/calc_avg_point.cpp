#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <set>
#include <map>
#include <string>

//
// Calculate average points
//
// [[Rcpp::export]]
Rcpp::List calc_avg_points(const Rcpp::List& points,
                           double ci_q, double minval, double maxval) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";

  std::set<double> all_x_vals;       // all x values
  std::map<double, int> x_vals_idx;  // map x values to indices

  std::vector<double> x_val;         // x values
  std::vector<double> avg_y;         // Average
  std::vector<double> se_y;          // SE
  std::vector<double> ci_h_y;        // CI upper bound
  std::vector<double> ci_l_y;        // CI lower bound

  std::vector<double> tot_y;         // Total of ys
  std::vector<int> count_y;          // Count of ys
  std::vector<double> stot_y;        // Total of squared ys

  // Create all unique x values
  for (int i = 0; i < points.size(); ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];

    for (int j = 0; j < xs.size(); ++j) {
      all_x_vals.insert(xs[j]);
    }
  }

  // Resize vectors
  int vec_size = all_x_vals.size();
  x_val.resize(vec_size, 0.0);
  avg_y.resize(vec_size, 0.0);
  se_y.resize(vec_size, 0.0);
  ci_h_y.resize(vec_size, 0.0);
  ci_l_y.resize(vec_size, 0.0);
  tot_y.resize(vec_size, 0.0);
  count_y.resize(vec_size, 0);
  stot_y.resize(vec_size, 0.0);

  // Make maps x vals to indices
  std::set<double>::iterator set_it;
  int idx = 0;
  for (set_it = all_x_vals.begin(); set_it != all_x_vals.end(); ++set_it) {
    x_val[idx] = *set_it;
    x_vals_idx.insert(std::pair<double, int>(*set_it, idx));
    ++idx;
  }

  // Calculate total
  idx = 0;
  for (int i = 0; i < points.size(); ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];
    Rcpp::NumericVector ys = c["y"];

    for (int j = 0; j < ys.size(); ++j) {
      idx = x_vals_idx[xs[j]];

      tot_y[idx] += ys[j];
      stot_y[idx] += (ys[j] * ys[j]);
      ++count_y[idx];
    }
  }

  // Calculate average & CI
  double exp2;
  double sd;
  double n;
  for (int i = 0; i < vec_size; ++i) {
    n = double(count_y[i]);

    // y
    avg_y[i] = tot_y[i] / n;

    // se
    exp2 = (stot_y[i] / n) - (avg_y[i] * avg_y[i]);
    if (exp2 < 0){
      exp2 = 0;
    }
    sd =  ::sqrt(n / (n - 1)) * ::sqrt(exp2);
    se_y[i] = sd / ::sqrt(n);

    // ci upper bound
    ci_h_y[i] = avg_y[i] + ci_q * se_y[i];
    if (maxval != ::NA_REAL && ci_h_y[i] > maxval) {
      ci_h_y[i] = maxval;
    }

    // ci lower bound
    ci_l_y[i] = avg_y[i] - ci_q * se_y[i];
    if (minval != ::NA_REAL && ci_l_y[i] < minval) {
      ci_l_y[i] = minval;
    }
  }

  // Return a list
  df["x"] = x_val;
  df["y_avg"] = avg_y;
  df["y_se"] = se_y;
  df["y_ci_h"] = ci_h_y;
  df["y_ci_l"] = ci_l_y;
  ret_val["avg"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

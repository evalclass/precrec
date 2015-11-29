#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>
#define EPSILON_D (1.0E-8)

// Prototype
void get_yval_single(const Rcpp::NumericVector& xs,
                     const Rcpp::NumericVector& ys,
                     double x_interval,
                     double x_bins,
                     int vec_size,
                     std::vector<double>& s_y_val);

//
// Calculate average curves
//
// [[Rcpp::export]]
Rcpp::List calc_avg_curve(const Rcpp::List& curves,
                          double x_bins,
                          double ci_q) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  double x_interval = 1.0 / x_bins;


  int vec_size = 3 + (1.0 / x_interval);

  std::vector<double> x_val(vec_size);         // x values
  std::vector<double> avg_y(vec_size);         // Average
  std::vector<double> se_y(vec_size);          // SE
  std::vector<double> ci_h_y(vec_size);        // CI upper bound
  std::vector<double> ci_l_y(vec_size);        // CI lower bound

  std::vector<double> tot_y(vec_size, 0.0);    // Total of ys
  std::vector<double> stot_y(vec_size, 0.0);   // Total of squared ys
  std::vector<double> s_y_val(vec_size, 0.0);  // x values of a single curve
  int n = curves.size();

  // Calculate total
  for (int i = 0; i < n; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(curves[i]);

    get_yval_single(c["x"], c["y"], x_interval, x_bins, vec_size, s_y_val);

    for (int j = 0; j < vec_size; ++j) {
      tot_y[j] += s_y_val[j];
      stot_y[j] += (s_y_val[j] * s_y_val[j]);
    }
    s_y_val.clear();
    s_y_val.resize(vec_size, 0.0);
  }

  // Calculate average & CI
  double exp2;
  double sd;
  for (int i = 0; i < vec_size; ++i) {
    // x
    if (i == 0) {
      x_val[i] = 0;
    } else if (i == vec_size - 1) {
      x_val[i] = 1;
    } else {
      x_val[i] = (i - 1) * x_interval;
    }

    // y
    avg_y[i] = tot_y[i] / double(n);

    // se
    exp2 = (stot_y[i] / double(n)) - (avg_y[i] * avg_y[i]);
    if (exp2 < 0){
      exp2 = 0;
    }
    sd =  ::sqrt(double(n) / double(n - 1)) * ::sqrt(exp2);
    se_y[i] = sd / ::sqrt(double(n));

    // ci upper bound
    ci_h_y[i] = avg_y[i] + ci_q * se_y[i];
    if (ci_h_y[i] > 1.0) {
      ci_h_y[i] = 1.0;
    }

    // ci lower bound
    ci_l_y[i] = avg_y[i] - ci_q * se_y[i];
    if (ci_l_y[i] < 0.0) {
      ci_l_y[i] = 0.0;
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

void get_yval_single(const Rcpp::NumericVector& xs,
                     const Rcpp::NumericVector& ys,
                     double x_interval,
                     double x_bins,
                     int vec_size,
                     std::vector<double>& s_y_val) {

  std::vector<double> y_tot(vec_size, 0.0); // Total of ys
  std::vector<int> n_y(vec_size, 0);        // Number of each point
  std::set<double> x_set;
  int idx;
  double rounded_xval;

  // x = 0
  s_y_val[0] = ys[0];
  n_y[0]  = 1;
  idx = 1;
  while (xs[idx] == 0.0) {
    s_y_val[1] = ys[idx];
    n_y[1]  = 1;
    ++idx;
  }
  if (n_y[1] == 0) {
    s_y_val[1] = s_y_val[0];
  }
  n_y[1]  = 1;

  // 0 < x < 1
  for (int j = 1; j < xs.size() - 1; ++j) {
    if (xs[j] == 0.0 || xs[j] == 1.0) {
      continue;
    }
    rounded_xval = ::roundf(xs[j] * x_bins) / x_bins;
    if (::fabs(rounded_xval - xs[j]) > EPSILON_D) {
      continue;
    }
    idx = int(::roundf(rounded_xval * x_bins) + 1.0);
    s_y_val[idx] = ys[j];
    ++n_y[idx];
  }

  // x = 1
  s_y_val[vec_size - 1] = ys[ys.size() - 1];
  n_y[vec_size - 1]  = 1;
  idx = ys.size() - 2;
  while (xs[idx] == 1.0) {
    s_y_val[vec_size - 2] = ys[idx];
    n_y[vec_size - 2] = 1;
    --idx;
  }
  if (n_y[vec_size - 2] == 0) {
    s_y_val[vec_size - 2] = s_y_val[vec_size - 1];
  }
  n_y[vec_size - 2]  = 1;

  //Treat missing values
  for (int i = 0; i < vec_size; ++i) {
    if (n_y[i] == 0) {
      if (n_y[i-1] != 0 && n_y[i+1] != 0) {
        s_y_val[i] = (s_y_val[i-1] + s_y_val[i+1]) / 2.0;
      } else if (n_y[i-1] != 0) {
        s_y_val[i] = s_y_val[i-1];
      } else if (n_y[i+1] != 0) {
        s_y_val[i] = s_y_val[i+1];
      }
    }
  }
}

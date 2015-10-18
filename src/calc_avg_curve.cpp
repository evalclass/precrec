#include <Rcpp.h>
#include <cmath>

// Prototypes
void calc_avg_single(const Rcpp::NumericVector& xs,
                     const Rcpp::NumericVector& ys,
                     double x_interval,
                     int vec_size,
                     std::vector<double>& wk_avg_y);

// Calculate average curves
// [[Rcpp::export]]
Rcpp::List calc_avg_curve(const Rcpp::List& curves,
                          double x_interval,
                          double ci_q) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";


  int vec_size = 3 + (1.0 / x_interval);

  std::vector<double> x_val(vec_size);         // x values
  std::vector<double> avg_y(vec_size);         // Average
  std::vector<double> se_y(vec_size);          // SE
  std::vector<double> ci_h_y(vec_size);        // CI upper bound
  std::vector<double> ci_l_y(vec_size);        // CI lower bound

  std::vector<double> tot_y(vec_size, 0.0);    // Total of ys
  std::vector<double> stot_y(vec_size, 0.0);   // Total of squared ys
  std::vector<double> wk_avg_y(vec_size);      // Average
  int n = curves.size();



  // Calculate total
  for (int i = 0; i < n; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(curves[i]);

    calc_avg_single(c["x"], c["y"], x_interval, vec_size, wk_avg_y);

    for (int i = 0; i < vec_size; ++i) {
      tot_y[i] += wk_avg_y[i];
      stot_y[i] += (wk_avg_y[i] * wk_avg_y[i]);
    }
  }

  // Calculate average & CI
  for (int i = 0; i < vec_size; ++i) {
    // x
    if (i == 0) {
      x_val[i] = 0;
    } else {
      x_val[i] = (i - 1) * x_interval;
    }

    // y
    avg_y[i] = tot_y[i] / n;

    // se
    se_y[i] = (::sqrt(double(n) / double(n - 1))
               * ::sqrt((stot_y[i] / n) - (avg_y[i] * avg_y[i])))
              / ::sqrt(double(n));

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

void calc_avg_single(const Rcpp::NumericVector& xs,
                     const Rcpp::NumericVector& ys,
                     double x_interval,
                     int vec_size,
                     std::vector<double>& wk_avg_y) {

  std::vector<double> y_tot(vec_size, 0.0); // Total of ys
  std::vector<int> n_y(vec_size, 0);        // Number of each point
  int idx;

  // x = 0
  y_tot[0] = ys[0];
  n_y[0]  = 1;

  // 0 < x < 1
  for (int j = 1; j < xs.size() - 1; ++j) {
    idx = int((xs[j] / x_interval) + 1.0);
    y_tot[idx] += ys[j];
    ++n_y[idx];
  }

  // x = 1
  y_tot[vec_size - 1] = ys[ys.size() - 1];
  n_y[vec_size - 1]  = 1;

  // Calculate average
  for (int i = 1; i < y_tot.size(); ++i) {
    if (n_y[i] == 0) {
      y_tot[i] = y_tot[i-1];
      n_y[i] = n_y[i-1];
    }

    wk_avg_y[i] = y_tot[i] / n_y[i];
  }

}

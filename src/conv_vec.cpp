#include "conv_vec.h"

//
// Calculate vector size
//
int calc_vec_size(const Rcpp::List& obj,
                  const Rcpp::List& curvetype_names) {

  int vec_size = 0;

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    std::string lblctype = Rcpp::as<std::string>(curvetypes[i]);
    std::string idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    Rcpp::List curves = Rcpp::as<Rcpp::List>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      Rcpp::List xys = Rcpp::as<Rcpp::List>(curves[j]);
      Rcpp::NumericVector xs = Rcpp::as<Rcpp::NumericVector>(xys["x"]);
      vec_size += xs.size();
    }
  }

  return(vec_size);
}

//
// Copy vector
//
void copy_xy_vec(const Rcpp::NumericVector& from_vec,
                 std::vector<double>& to_vec,
                 int start_idx) {

  for (unsigned i = 0; i < from_vec.size(); i++) {
    to_vec[start_idx+i] = from_vec[i];
  }
}

//
// Add to vector
//
void add_to_vec(std::vector<int>& vec, int size, int value, int start_idx) {

  for (unsigned i = 0; i < size; i++) {
    vec[start_idx+i] = value;
  }
}

//
// Set reduced points
//
int set_reduced_points(const Rcpp::NumericVector& from_vec,
                       std::vector<bool>& points,
                       const int x_bins) {

  double x_pos = 0.0;
  double step = 1.0 / x_bins;
  double eps = std::numeric_limits<double>::epsilon() * x_bins;
  int n = 0;

  for (unsigned i = 0; i < from_vec.size(); i++) {
    int count = (int)(from_vec[i] / step);
    x_pos = (double)count * step;
    if (fabs(x_pos - from_vec[i]) <= eps) {
      points[i] = true;
      n++;
    } else {
      points[i] = false;
    }
  }

  return n;
}


//
// Copy reduced points
//
void copy_reduced_xy_vec(const Rcpp::NumericVector& from_vec,
                         std::vector<double>& to_vec,
                         int start_idx,
                         std::vector<bool>& points) {

  int idx = 0;

  for (unsigned i = 0; i < from_vec.size(); i++) {
    if (points[i]) {
      to_vec[start_idx+idx] = from_vec[i];
      idx++;
    }
  }

}


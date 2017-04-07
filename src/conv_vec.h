#ifndef CONV_VEC_H_
#define CONV_VEC_H_

#include <Rcpp.h>
#include <vector>
#include <string>

int calc_vec_size(const Rcpp::List& obj,
                  const Rcpp::List& curvetype_names);

void copy_xy_vec(const Rcpp::NumericVector& from_vec,
                 std::vector<double>& to_vec,
                 int start_idx);

void add_to_vec(std::vector<int>& vec, int size, int value, int start_idx);

int set_reduced_points(const Rcpp::NumericVector& from_vec,
                       std::vector<bool>& points, const int x_bins);

void copy_reduced_xy_vec(const Rcpp::NumericVector& from_vec,
                         std::vector<double>& to_vec,
                         int start_idx,
                         std::vector<bool>& points);

#endif /* CONV_VEC_H_ */

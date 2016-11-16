#ifndef CONV_VEC_H_
#define CONV_VEC_H_

#include <Rcpp.h>
#include <string>

int calc_vec_size(const Rcpp::List& obj,
                  const Rcpp::List& curvetype_names);

int copy_xy_vec(const Rcpp::NumericVector& from_vec,
                Rcpp::NumericVector& to_vec,
                int start_idx);

void create_vec(Rcpp::IntegerVector& vec, int size, int value, int start_idx);

#endif /* CONV_VEC_H_ */

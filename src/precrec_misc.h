#ifndef PRECREC_MISC_H_
#define PRECREC_MISC_H_

#include <Rcpp.h>
#include <cfloat>       // DBL_MIN, DBL_MAX
#include <vector>       // std::vector
#include <string>       // std::string
#include <algorithm>    // std::sort, std::stable_sort

//
// Make pairs for sorting
//
void make_index_pairs(std::vector<std::pair<unsigned, double > >& indices,
                      const Rcpp::NumericVector& scores,
                      const bool& na_worst);

//
// Sort indices by scores
//
void sort_indices(std::vector<std::pair<unsigned, double > >& indices,
                  const std::string& ties_method,
                  bool desc);

#endif /* PRECREC_MISC_H_ */


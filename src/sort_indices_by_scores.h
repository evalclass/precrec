#ifndef SORT_INDICES_BY_SCORES_H_
#define SORT_INDICES_BY_SCORES_H_

#include <Rcpp.h>
#include <vector>       // std::vector
#include <string>       // std::string
#include <algorithm>    // std::sort, std::stable_sort

// Classes for sorting
class CompDVecDesc {
    const std::vector<double>& _vals;

  public:
    CompDVecDesc(const std::vector<double>& vals): _vals(vals) {}

    bool operator() (const int& a, const int& b) const {
      return _vals[a] > _vals[b];
    }
};

class CompDVecAsc {
    const std::vector<double>& _vals;

  public:
    CompDVecAsc(const std::vector<double>& vals): _vals(vals) {}

    bool operator() (const int& a, const int& b) const {
      return _vals[a] < _vals[b];
    }

};

//
// Sort indices by scores
//
void sort_indices(std::vector<int>& indices,
                  const Rcpp::NumericVector& scores,
                  const bool& na_worst,
                  const std::string& ties_method,
                  bool desc);

#endif /* SORT_INDICES_BY_SCORES_H_ */


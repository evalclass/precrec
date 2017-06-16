#ifndef SORT_INDICES_BY_SCORES_H_
#define SORT_INDICES_BY_SCORES_H_

#include <vector>       // std::vector
#include <string>       // std::string
#include <algorithm>    // std::sort, std::stable_sort

// Class for sorting
class CompDVec {
    const std::vector<double>& _vals;

  public:
    CompDVec(const std::vector<double>& vals): _vals(vals) {}

    bool operator() (const int& a, const int& b) const {
      return _vals[a] > _vals[b];
    }
};

//
// Sort indices by scores
//
void sort_indices(std::vector<int>& indices,
                  const std::vector<double>& scores,
                  const std::string& ties_method);

#endif /* SORT_INDICES_BY_SCORES_H_ */


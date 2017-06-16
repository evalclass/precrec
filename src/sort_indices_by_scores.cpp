#include "sort_indices_by_scores.h"

//
// Sort indices by scores
//
void sort_indices(std::vector<int>& indices,
                  const std::vector<double>& scores,
                  const std::string& ties_method) {

  // Sort scores
  CompDVec fcomp(scores);
  if (ties_method == "first") {
    std::stable_sort(indices.begin(), indices.end(), fcomp);
  } else {
    std::sort(indices.begin(), indices.end(), fcomp);
  }
}

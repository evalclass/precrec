#include "sort_indices_by_scores.h"

//
// Sort indices by scores
//
void sort_indices(std::vector<int>& indices,
                  const Rcpp::NumericVector& scores,
                  const bool& na_worst,
                  const std::string& ties_method,
                  bool desc) {

  // Determin NA values
  double na_val;
  if (na_worst) {
    na_val = DBL_MIN;
  } else {
    na_val = DBL_MAX;
  }

  // Update NAs
  std::vector<double> svals(scores.size());
  for (int i = 0; i < scores.size(); ++i) {
    if (Rcpp::NumericVector::is_na(scores[i])) {
      svals[i] = na_val;
    } else {
      svals[i] = scores[i];
    }
    indices[i] = i;
  }

  // Sort scores
  if (desc) {
    CompDVecDesc fcompd(svals);
    if (ties_method == "first") {
      std::stable_sort(indices.begin(), indices.end(), fcompd);
    } else {
      std::sort(indices.begin(), indices.end(), fcompd);
    }
  } else {
    CompDVecAsc fcompa(svals);
    if (ties_method == "first") {
      std::stable_sort(indices.begin(), indices.end(), fcompa);
    } else {
      std::sort(indices.begin(), indices.end(), fcompa);
    }
  }
}

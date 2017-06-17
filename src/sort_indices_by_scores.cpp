#include "sort_indices_by_scores.h"
#include <ctime>

//
// Comp functions
//

bool comp_asc(const std::pair<unsigned, double > &a,
              const std::pair<unsigned, double > &b)
{
       return a.second < b.second;
}

bool comp_desc(const std::pair<unsigned, double > &a,
               const std::pair<unsigned, double > &b)
{
       return a.second > b.second;
}

//
// Make pairs
//
void make_index_pairs(std::vector<std::pair<unsigned, double > >& indices,
                      const Rcpp::NumericVector& scores,
                      const bool& na_worst) {

  // Determin NA values
  double na_val;
  if (na_worst) {
    na_val = DBL_MIN;
  } else {
    na_val = DBL_MAX;
  }

  // Update NAs
  for (unsigned i = 0; i < scores.size(); ++i) {
    if (Rcpp::NumericVector::is_na(scores[i])) {
      indices[i] = std::make_pair(i, na_val);
    } else {
      indices[i] = std::make_pair(i, scores[i]);
    }
  }
}

//
// Sort indices by scores
//
void sort_indices(std::vector<std::pair<unsigned, double > >& indices,
                  const std::string& ties_method,
                  bool desc) {

  bool (*comp_func)(const std::pair<unsigned, double > &,
                    const std::pair<unsigned, double > &);
  if (desc) {
    comp_func = &comp_desc;
  } else {
    comp_func = &comp_asc;
  }

  // Sort scores
  if (ties_method == "first") {
    std::stable_sort(indices.begin(), indices.end(), comp_func);
  } else {
    std::sort(indices.begin(), indices.end(), comp_func);
  }
}


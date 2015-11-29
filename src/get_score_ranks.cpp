#include <Rcpp.h>
#include <vector>       // std::vector
#include <string>       // std::string
#include <cfloat>       // DBL_MIN, DBL_MAX
#include <algorithm>    // std::random_shuffle, std::sort, std::stable_sort

// Prototype
void update_ties(std::vector<int>& ranks,
                 std::vector<int>& rank_idx,
                 std::vector<int>& tied_idx,
                 const std::string& ties_method);

// Class for sorting
class CompDVec {
    std::vector<double>& _vals;

  public:
    CompDVec(std::vector<double>& vals): _vals(vals) {}

    bool operator() (const int& a, const int& b) const {
      return _vals[a] > _vals[b];
    }
};

//
// Get ranks and index of scores
//
// [[Rcpp::export]]
Rcpp::List get_score_ranks(const Rcpp::NumericVector& scores,
                           const bool& na_last,
                           const std::string& ties_method) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  std::vector<int> ranks(scores.size());
  std::vector<int> rank_idx(scores.size());

  // Update NAs
  std::vector<double> svals(scores.size());
  std::vector<int> sorted_idx(scores.size());
  for (int i = 0; i < scores.size(); ++i) {
    if (Rcpp::NumericVector::is_na(scores[i])) {
      if (na_last) {
        svals[i] = DBL_MIN;
      } else {
        svals[i] = DBL_MAX;
      }
    } else {
      svals[i] = scores[i];
    }
    sorted_idx[i] = i;
  }

  // Sort scores
  CompDVec fcomp(svals);
  if (ties_method == "first") {
    std::stable_sort(sorted_idx.begin(), sorted_idx.end(), fcomp);
  } else {
    std::sort(sorted_idx.begin(), sorted_idx.end(), fcomp);
  }

  // Set ranks
  for (unsigned i = 0; i < sorted_idx.size(); ++i) {
     ranks[sorted_idx[i]] = i + 1;
     rank_idx[i] = sorted_idx[i];
  }

  // Update ties
  if (ties_method == "equiv" || ties_method == "random") {
    std::vector<int> tied_idx;
    double prev_val = svals[rank_idx[0]];
    bool tied = false;
    for (unsigned i = 1; i < rank_idx.size(); ++i) {
      if (tied) {
        if (prev_val != svals[rank_idx[i]]) {
          update_ties(ranks, rank_idx, tied_idx, ties_method);
          tied_idx.clear();
          tied = false;
        } else {
          tied_idx.push_back(rank_idx[i]);
        }
      } else if (prev_val == svals[rank_idx[i]]) {
        tied_idx.push_back(rank_idx[i - 1]);
        tied_idx.push_back(rank_idx[i]);
        tied = true;
      }

      prev_val = svals[rank_idx[i]];
    }

    if (tied) {
      update_ties(ranks, rank_idx, tied_idx, ties_method);
    }
  }

  // Add 1 to rank_idx
  for (unsigned i = 0; i < rank_idx.size(); ++i) {
    rank_idx[i] = rank_idx[i] + 1;
  }

  // Return result
  ret_val["ranks"] = ranks;
  ret_val["rank_idx"] = rank_idx;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

// Copied from http://gallery.rcpp.org/articles/stl-random-shuffle/
// wrapper around R's RNG such that we get a uniform distribution over
// [0,n) as required by the STL algorithm
inline int randWrapper(const int n) { return floor(unif_rand() * n); }

// Update ranks and rank_idx for ties
void update_ties(std::vector<int>& ranks,
                 std::vector<int>& rank_idx,
                 std::vector<int>& tied_idx,
                 const std::string& ties_method) {

  typedef std::vector<int>::iterator TIntIt;

  int base_rank = ranks[tied_idx[0]];
  int base_rank_idx = rank_idx[tied_idx[0]];

  if (ties_method == "equiv") {
    for (TIntIt it = tied_idx.begin(); it != tied_idx.end(); ++it) {
      ranks[*it] = base_rank;
    }
  } else if (ties_method == "random") {
    std::random_shuffle(tied_idx.begin(), tied_idx.end(), randWrapper);
    for (unsigned i = 0; i < tied_idx.size(); ++i) {
      ranks[rank_idx[tied_idx[i]]] = base_rank + i;
      rank_idx[tied_idx[i]] = base_rank_idx + i;
    }
  }
}

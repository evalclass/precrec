#include <Rcpp.h>
#include <vector>       // std::vector
#include <string>       // std::string
#include <cfloat>       // DBL_MIN, DBL_MAX
#include "sort_indices_by_scores.h"

// Prototype
void update_ties(std::vector<int>& ranks,
                 std::vector<int>& rank_idx,
                 std::vector<int>& tied_idx,
                 const std::string& ties_method);

//
// Get ranks and index of scores
//
// [[Rcpp::export]]
Rcpp::List get_score_ranks(const Rcpp::NumericVector& scores,
                           const bool& na_worst,
                           const std::string& ties_method) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  std::vector<int> ranks(scores.size());
  std::vector<int> rank_idx(scores.size());

  // Sort scores
  std::vector<std::pair<unsigned, double > > sorted_idx(scores.size());
  make_index_pairs(sorted_idx, scores, na_worst);
  sort_indices(sorted_idx, ties_method, true);

  // Set ranks
  for (unsigned i = 0; i < sorted_idx.size(); ++i) {
     ranks[sorted_idx[i].first] = i + 1;
     rank_idx[i] = sorted_idx[i].first + 1;
  }

  // Update ties
  if (ties_method == "equiv" || ties_method == "random") {
    std::vector<int> tied_idx;
    double prev_val = sorted_idx[0].second;
    bool tied = false;
    for (unsigned i = 1; i < sorted_idx.size(); ++i) {
      if (tied) {
        if (prev_val != sorted_idx[i].second) {
          update_ties(ranks, rank_idx, tied_idx, ties_method);
          tied_idx.clear();
          tied = false;
        } else {
          tied_idx.push_back(sorted_idx[i].first);
        }
      } else if (prev_val == sorted_idx[i].second) {
        tied_idx.push_back(sorted_idx[i-1].first);
        tied_idx.push_back(sorted_idx[i].first);
        tied = true;
      }

      prev_val = sorted_idx[i].second;
    }

    if (tied) {
      update_ties(ranks, rank_idx, tied_idx, ties_method);
    }
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

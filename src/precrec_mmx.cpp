#include <Rcpp.h>
#include <vector>       // std::vector
#include <string>       // std::string
#include <cfloat>       // DBL_MIN, DBL_MAX
#include "precrec_misc.h"

/*
##############################################
 Name: format_labels
 R file: mm3_reformat_data.R
 R func: reformat_data
##############################################
*/

// Make new labels - negative: 1, positive: 2
template<typename T, typename S>
Rcpp::List make_new_labels(T labels,
                           S posclass,
                           bool is_pc_na,
                           S def_posclass,
                           S def_negclass) {
  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  std::vector<double> new_labels(labels.size());
  int nn = 0;
  int np = 0;

  // Get two labels
  S lab_p = labels[0];
  S lab_n;
  bool is_single_class = true;
  for (unsigned i = 0; i < labels.size(); ++i) {
    if (lab_p != labels[i]) {
      lab_n = labels[i];
      is_single_class = false;
      break;
    }
  }

  // Find positive label
  if (is_pc_na) {
    if (is_single_class) {
      if (lab_p == def_posclass) {
        lab_n = def_negclass;
      } else {
        S lab_tmp = lab_p;
        lab_p = def_posclass;
        lab_n = lab_tmp;
      }
    } else if (lab_p < lab_n) {
      S lab_tmp = lab_p;
      lab_p = lab_n;
      lab_n = lab_tmp;
    }
  } else {
    if (is_single_class) {
      if (lab_p == posclass) {
        lab_n = def_negclass;
      } else {
        S lab_tmp = lab_p;
        lab_p = posclass;
        lab_n = lab_tmp;
      }
    } else if (lab_n == posclass) {
      S lab_tmp = lab_p;
      lab_p = lab_n;
      lab_n = lab_tmp;
    } else if (lab_p != posclass) {
      ret_val["errmsg"] = "invalid-posclass";
      return ret_val;
    }
  }

  // Make new labels - negative 1 & positive 2
  for (unsigned i = 0; i < labels.size(); ++i) {
    if (labels[i] == lab_p) {
      ++np;
      new_labels[i] = 2;
    } else if (labels[i] == lab_n) {
      ++nn;
      new_labels[i] = 1;
    } else {
      ret_val["errmsg"] = "invalid-labels";
      return ret_val;
    }
  }

  ret_val["nn"] = nn;
  ret_val["np"] = np;
  ret_val["labels"] = new_labels;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

//
// Format labels
//
// [[Rcpp::export]]
Rcpp::List format_labels(SEXP labels,
                         SEXP posclass) {
  bool is_pc_na;

  switch (TYPEOF(labels)) {
  case INTSXP: {
    Rcpp::IntegerVector pos_class_i = Rcpp::as<Rcpp::IntegerVector>(posclass);
    is_pc_na = Rcpp::IntegerVector::is_na(pos_class_i[0]);
    int def_posclass = 2;
    int def_negclass = 1;
    return make_new_labels<Rcpp::IntegerVector, int>
      (labels, pos_class_i[0], is_pc_na, def_posclass, def_negclass);
  }
  case REALSXP: {
    Rcpp::NumericVector pos_class_d = Rcpp::as<Rcpp::NumericVector>(posclass);
    is_pc_na = Rcpp::NumericVector::is_na(pos_class_d[0]);
    double def_posclass = 1.0;
    double def_negclass = -1.0;
    return make_new_labels<Rcpp::NumericVector, double>
      (labels, pos_class_d[0], is_pc_na, def_posclass, def_negclass);
  }
  case LGLSXP: {
    Rcpp::LogicalVector pos_class_b = Rcpp::as<Rcpp::LogicalVector>(posclass);
    is_pc_na = Rcpp::LogicalVector::is_na(pos_class_b[0]);
    bool def_posclass = true;
    bool def_negclass = false;
    return make_new_labels<Rcpp::LogicalVector, bool>
      (labels, pos_class_b[0], is_pc_na, def_posclass, def_negclass);
  }
  case STRSXP: {
    Rcpp::CharacterVector pos_class_c = Rcpp::as<Rcpp::CharacterVector>(posclass);
    std::string pos_class_c2(pos_class_c[0]);
    is_pc_na = Rcpp::CharacterVector::is_na(pos_class_c[0]);
    std::vector<std::string> labels_s = Rcpp::as<std::vector<std::string> >(labels);
    std::string def_posclass = "P";
    std::string def_negclass = "N";
    return make_new_labels<std::vector<std::string>, std::string>
      (labels_s, pos_class_c2, is_pc_na, def_posclass, def_negclass);
  }
  default:
    Rcpp::List ret_val;
    ret_val["errmsg"] = "incompatible-SEXP";
    return ret_val;
  }
}

/*
##############################################
 Name: get_score_ranks
 R file: mm3_reformat_data.R
 R func: .rank_scores
##############################################
*/

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



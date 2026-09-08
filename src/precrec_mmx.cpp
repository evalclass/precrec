#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <vector>       // std::vector
#include <string>       // std::string
#include <cfloat>       // DBL_MIN, DBL_MAX
#include <cstdint>      // uint64_t, uint32_t
#include <cstring>      // std::memcpy
#include <limits>       // std::numeric_limits
#include <utility>      // std::swap
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
  unsigned nn = 0;
  unsigned np = 0;

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
    const Rcpp::IntegerVector& pos_class_i = static_cast<const Rcpp::IntegerVector&>(posclass);
    is_pc_na = Rcpp::IntegerVector::is_na(pos_class_i[0]);
    int def_posclass = 2;
    int def_negclass = 1;
    return make_new_labels<const Rcpp::IntegerVector&, int>
      (labels, pos_class_i[0], is_pc_na, def_posclass, def_negclass);
  }
  case REALSXP: {
    const Rcpp::NumericVector& pos_class_d = static_cast<const Rcpp::NumericVector&>(posclass);
    is_pc_na = Rcpp::NumericVector::is_na(pos_class_d[0]);
    double def_posclass = 1.0;
    double def_negclass = -1.0;
    return make_new_labels<const Rcpp::NumericVector&, double>
      (labels, pos_class_d[0], is_pc_na, def_posclass, def_negclass);
  }
  case LGLSXP: {
    const Rcpp::LogicalVector& pos_class_b = static_cast<const Rcpp::LogicalVector&>(posclass);
    is_pc_na = Rcpp::LogicalVector::is_na(pos_class_b[0]);
    bool def_posclass = true;
    bool def_negclass = false;
    return make_new_labels<const Rcpp::LogicalVector&, bool>
      (labels, pos_class_b[0], is_pc_na, def_posclass, def_negclass);
  }
  case STRSXP: {
    Rcpp::CharacterVector pos_class_c = Rcpp::as<Rcpp::CharacterVector>(posclass);
    is_pc_na = Rcpp::CharacterVector::is_na(pos_class_c[0]);
    std::vector<std::string> labels_s = Rcpp::as<std::vector<std::string> >(labels);
    std::string def_posclass = "P";
    std::string def_negclass = "N";
    return make_new_labels<const std::vector<std::string>&, std::string>
      (labels_s, static_cast<std::string>(pos_class_c[0]), is_pc_na, def_posclass, def_negclass);
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

// Order-preserving map from a double to a uint64_t whose unsigned order is
// the double's ascending order, then complemented so that an ascending sort
// of the keys is a descending sort of the scores. The map is injective, so
// two keys are equal exactly when the two scores are, and the tie scan can
// read keys instead of scores.
static inline uint64_t desc_key(double x) {
  uint64_t k;
  // -0.0 and 0.0 are equal as numbers but differ in the sign bit, so keying
  // on the bits alone would rank them apart and split a tie the comparison
  // sort kept together. They are the only pair `==` treats as equal while
  // the bits differ - NaN is the other bit-level oddity, and the caller has
  // already replaced it with the NA sentinel.
  if (x == 0.0) {
    x = 0.0;
  }
  std::memcpy(&k, &x, sizeof(k));
  k = (k & 0x8000000000000000ULL) ? ~k : (k | 0x8000000000000000ULL);
  return ~k;
}

// Order the scores, descending, ties by ascending input index
//
// Fills `sorted_v` with input indices in that order and `sorted_k` with the
// matching keys.
//
// This replaced a std::sort over (index, score) pairs whose comparator broke
// ties on the input index. That tie-break was there to pin the permutation:
// std::sort may reorder equal elements however its introsort happens to fall
// out, and libstdc++ and libc++ fall out differently, so without it the ranks
// were library-dependent. An LSD radix sort is stable pass by pass, so it
// gives that same permutation from the algorithm rather than from the
// comparator, and the ranks stay fixed by the data alone.
//
// Eight passes on byte digits. Radix runs about 2x faster than the comparison
// sort on unsorted scores and better than that when they are heavily tied,
// but it loses on scores that arrive already ordered. The monotonicity scan
// in front hands those to a direct fill instead; it stops as soon as the
// scores are neither ascending nor descending, which on unsorted input is
// within the first few elements.
static void order_scores_desc(const Rcpp::NumericVector& scores,
                              const double na_val,
                              std::vector<unsigned>& sorted_v,
                              std::vector<uint64_t>& sorted_k) {
  const size_t n = scores.size();
  const double* sp = scores.begin();

  // is_na() is checked here and again below rather than materializing a
  // cleaned copy of the scores, which would cost a pass of its own.
  bool desc = true;
  bool asc = true;
  for (size_t i = 1; i < n && (desc || asc); ++i) {
    const double a = sp[i-1];
    const double b = sp[i];
    const double x = Rcpp::NumericVector::is_na(a) ? na_val : a;
    const double y = Rcpp::NumericVector::is_na(b) ? na_val : b;
    if (y > x) {
      desc = false;
    }
    // Ascending must be strict: with ties, reading the input backwards would
    // put the tied indices in descending order where the tie-break asks for
    // ascending.
    if (y <= x) {
      asc = false;
    }
  }

  if (desc || asc) {
    for (size_t i = 0; i < n; ++i) {
      const size_t j = desc ? i : (n - 1 - i);
      const double x = sp[j];
      sorted_v[i] = static_cast<unsigned>(j);
      sorted_k[i] = desc_key(Rcpp::NumericVector::is_na(x) ? na_val : x);
    }
    return;
  }

  const unsigned RADIX = 256;
  const unsigned NPASS = 8;
  std::vector<uint64_t> kb(n);
  std::vector<unsigned> vb(n);
  std::vector<uint32_t> cnt(static_cast<size_t>(NPASS) * RADIX, 0);

  for (size_t i = 0; i < n; ++i) {
    const double x = sp[i];
    const uint64_t k = desc_key(Rcpp::NumericVector::is_na(x) ? na_val : x);
    sorted_k[i] = k;
    sorted_v[i] = static_cast<unsigned>(i);
    for (unsigned p = 0; p < NPASS; ++p) {
      ++cnt[static_cast<size_t>(p) * RADIX + ((k >> (p * 8)) & 0xFF)];
    }
  }

  uint64_t* src_k = &sorted_k[0];
  uint64_t* dst_k = &kb[0];
  unsigned* src_v = &sorted_v[0];
  unsigned* dst_v = &vb[0];
  for (unsigned p = 0; p < NPASS; ++p) {
    uint32_t* c = &cnt[static_cast<size_t>(p) * RADIX];
    const unsigned shift = p * 8;
    // Every key shares this digit, so the pass would only copy
    if (c[(src_k[0] >> shift) & 0xFF] == n) {
      continue;
    }
    uint32_t sum = 0;
    for (unsigned d = 0; d < RADIX; ++d) {
      const uint32_t t = c[d];
      c[d] = sum;
      sum += t;
    }
    for (size_t i = 0; i < n; ++i) {
      const uint64_t k = src_k[i];
      const uint32_t pos = c[(k >> shift) & 0xFF]++;
      dst_k[pos] = k;
      dst_v[pos] = src_v[i];
    }
    std::swap(src_k, dst_k);
    std::swap(src_v, dst_v);
  }

  // An odd number of executed passes leaves the result in the scratch pair
  if (src_k != &sorted_k[0]) {
    std::memcpy(&sorted_k[0], src_k, n * sizeof(uint64_t));
    std::memcpy(&sorted_v[0], src_v, n * sizeof(unsigned));
  }
}

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
  const size_t n = scores.size();
  std::vector<int> ranks(n);
  std::vector<int> rank_idx(n);

  // Sort scores.
  // The sentinel must sort below (na_worst) or above (!na_worst) every real
  // score. DBL_MIN is the smallest *positive* double, so it would rank NAs
  // above every negative score; lowest() is the most negative one.
  const double na_val = na_worst ? std::numeric_limits<double>::lowest()
                                 : std::numeric_limits<double>::max();
  std::vector<unsigned> sorted_v(n);
  std::vector<uint64_t> sorted_k(n);
  order_scores_desc(scores, na_val, sorted_v, sorted_k);

  // Set ranks
  for (size_t i = 0; i < n; ++i) {
    ranks[sorted_v[i]] = static_cast<int>(i) + 1;
    rank_idx[i] = static_cast<int>(sorted_v[i]) + 1;
  }

  // Update ties
  if (n > 1 && (ties_method == "equiv" || ties_method == "random")) {
    std::vector<int> tied_idx;
    uint64_t prev_val = sorted_k[0];
    bool tied = false;
    for (size_t i = 1; i < n; ++i) {
      if (tied) {
        if (prev_val != sorted_k[i]) {
          update_ties(ranks, rank_idx, tied_idx, ties_method);
          tied_idx.clear();
          tied = false;
        } else {
          tied_idx.push_back(sorted_v[i]);
        }
      } else if (prev_val == sorted_k[i]) {
        tied_idx.push_back(sorted_v[i-1]);
        tied_idx.push_back(sorted_v[i]);
        tied = true;
      }

      prev_val = sorted_k[i];
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
    shuffle_intvec(tied_idx.begin(), tied_idx.end(), randWrapper);
    for (unsigned i = 0; i < tied_idx.size(); ++i) {
      ranks[rank_idx[tied_idx[i]]] = base_rank + i;
      rank_idx[tied_idx[i]] = base_rank_idx + i;
    }
  }
}



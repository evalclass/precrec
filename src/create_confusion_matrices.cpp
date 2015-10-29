#include <Rcpp.h>
#include <vector>
#include <string>

// Prototypes
void calc_tp_fp(const Rcpp::IntegerVector& olabs,
                const Rcpp::NumericVector& ranks,
                const Rcpp::IntegerVector& rank_idx,
                int n, int& np, int& nn,
                std::vector<double>& tp, std::vector<double>& fp,
                std::vector<double>& sorted_ranks);

void solve_ties(std::vector<double>& tp, std::vector<double>& fp,
                int curpos, int ties);

//
// Calculate confusion matrices for all possible threshold values
//
// [[Rcpp::export]]
Rcpp::List create_confusion_matrices(const Rcpp::IntegerVector& olabs,
                                     const Rcpp::NumericVector& ranks,
                                     const Rcpp::IntegerVector& rank_idx) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  int n = olabs.size();                   // Input data size
  int np;                                 // # of positive
  int nn;                                 // # of negatives
  std::vector<double> tp(n+1);            // TPs
  std::vector<double> fp(n+1);            // FPs
  std::vector<double> tn(n+1);            // TNs
  std::vector<double> fn(n+1);            // FNs
  std::vector<double> sorted_ranks(n+1);  // Ranks

  // Calculate TPs and FPs
  calc_tp_fp(olabs, ranks, rank_idx, n, np, nn, tp, fp, sorted_ranks);

  // Calculate TNs and FNs
  for (int i = 0; i < n+1; ++i) {
    tn[i] = nn - fp[i];
    fn[i] = np - tp[i];
  }

  // Return a list with P, N, TPs, FP, TNs, FN, and ranks
  ret_val["pos_num"] = np;
  ret_val["neg_num"] = nn;
  ret_val["tp"] = tp;
  ret_val["fp"] = fp;
  ret_val["tn"] = tn;
  ret_val["fn"] = fn;
  ret_val["ranks"] = sorted_ranks;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

// Calculate TPs and FPs
void calc_tp_fp(const Rcpp::IntegerVector& olabs,
                const Rcpp::NumericVector& ranks,
                const Rcpp::IntegerVector& rank_idx,
                int n, int& np, int& nn,
                std::vector<double>& tp, std::vector<double>& fp,
                std::vector<double>& sorted_ranks) {
  int ties = 0;
  double prev_rank = 0;

  // Initialize
  np = 0;
  nn = 0;
  tp[0] = 0;
  fp[0] = 0;
  sorted_ranks[0] = ranks[rank_idx[0] - 1] + 1;

  // Iterate all thresholds
  for (int i = 0; i < n; ++i) {
    int idx = rank_idx[i] - 1;

    // olabs is an ordered factor - positive: 2, negative: 1
    if (olabs[idx] == 2) {
      ++np;
    } else {
      ++nn;
    }
    tp[i+1] = np;
    fp[i+1] = nn;
    sorted_ranks[i+1] = ranks[idx];

    // Solve ties
    if (ranks[idx] == prev_rank) {
      ++ties;
    } else if (ties != 0) {
      solve_ties(tp, fp, i, ties);
      ties = 0;
    }
    prev_rank = ranks[idx];
  }

  // Solve ties when the lowest scores are tied
  if (ties != 0) {
    solve_ties(tp, fp, n, ties);
  }
}

// Solve tied scores
void solve_ties(std::vector<double>& tp, std::vector<double>& fp,
                int curpos, int ties) {
  double tied_tp;
  double tied_fp;

  tied_tp = (tp[curpos] - tp[curpos-ties-1]) / (ties + 1);
  tied_fp = (fp[curpos] - fp[curpos-ties-1]) / (ties + 1);
  for (int i = 0; i < ties; ++i) {
    tp[curpos-ties+i] = tp[curpos-ties+i-1] + tied_tp;
    fp[curpos-ties+i] = fp[curpos-ties+i-1] + tied_fp;
  }

}

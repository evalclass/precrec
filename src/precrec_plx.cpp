#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <string>
#include "precrec_misc.h"

/*
##############################################
 Name: create_confusion_matrices
 R file: pl3_create_confmats.R
 R func: create_confmats
##############################################
*/

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
// Calculate confusion matrices for ranks
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
  sorted_ranks[0] = ranks[rank_idx[0] - 1] - 1;

  // Iterate all ranks
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

/*
##############################################
 Name: calc_uauc
 R file: pl3_calc_auc_with_u.R
 R func: calc_auc_with_u
##############################################
*/

//
// Calculate AUC (ROC) with the U statistic
//
// [[Rcpp::export]]
Rcpp::List calc_uauc(unsigned np, unsigned nn,
                     const Rcpp::NumericVector& scores,
                     const Rcpp::IntegerVector& olabs,
                     const bool& na_worst,
                     const std::string& ties_method) {
  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;
  double ustat = 0;
  double np_dbl = (double)np;
  double nn_dbl = (double)nn;

  // Determin NA values
  double na_val;
  if (na_worst) {
    na_val = DBL_MIN;
  } else {
    na_val = DBL_MAX;
  }

  // Create pos and neg vectors
  std::vector<double> pos_vec(np);
  std::vector<double> neg_vec(nn);
  unsigned pos_idx = 0;
  unsigned neg_idx = 0;
  double s;
  for (unsigned i = 0; i < olabs.size(); ++i) {
    if (Rcpp::NumericVector::is_na(scores[i])) {
      s = na_val;
    } else {
      s = scores[i];
    }

    if (olabs[i] == 2) {
      pos_vec[pos_idx] = s;
      pos_idx++;
    } else {
      neg_vec[neg_idx] = s;
      neg_idx++;
    }
  }

  // Sort neg scores
  std::sort(neg_vec.begin(), neg_vec.end());
  std::sort(pos_vec.begin(), pos_vec.end());

  // Calculate U statistic
  pos_idx = 0;
  neg_idx = 0;
  while (pos_idx < pos_vec.size()) {
    if (neg_idx < neg_vec.size() && pos_vec[pos_idx] >= neg_vec[neg_idx]) {
      neg_idx++;
    } else {
      ustat += (double)neg_idx;
      pos_idx++;
    }
  }

  // Calculate AUC
  auc = ustat / (np_dbl * nn_dbl);
  if (auc < 0) {
    auc = 0;
  } else if (auc > 1) {
    auc = 1;
  }

  // Return a list
  ret_val["auc"] = auc;
  ret_val["ustat"] = ustat;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

/*
##############################################
 Name: calc_uauc_frank
 R file: pl3_calc_auc_with_u.R
 R func: calc_auc_with_u
##############################################
*/

//
// Calculate AUC (ROC) with the U statistic
//
// [[Rcpp::export]]
Rcpp::List calc_uauc_frank(unsigned np, unsigned nn,
                           const Rcpp::NumericVector& scores,
                           const Rcpp::IntegerVector& olabs,
                           const bool& na_last,
                           const std::string& ties_method,
                           Rcpp::Function frank) {
  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;
  double ranksum = 0;
  double ustat = 0;
  double np_dbl = (double)np;
  double nn_dbl = (double)nn;

  // Rank scores
  Rcpp::NumericVector ranks = frank(scores);

  // Calculate U statistic
  for (unsigned i = 0; i < olabs.size(); i++) {
    if (olabs[i] == 2) {
      ranksum += ranks[i];
    }
  }
  ustat = ranksum - np_dbl * (np_dbl + 1) / 2;

  // Calculate AUC
  auc = ustat / (np_dbl * nn_dbl);
  if (auc < 0) {
    auc = 0;
  } else if (auc > 1) {
    auc = 1;
  }

  // Return a list
  ret_val["auc"] = auc;
  ret_val["ustat"] = ustat;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

/*
##############################################
 Name: calc_basic_measures
 R file: pl4_calc_measures.R
 R func: calc_measures
##############################################
*/

//
// Calculate confusion matrices for all ranks
//
// [[Rcpp::export]]
Rcpp::List calc_basic_measures(int np,
                               int nn,
                               const Rcpp::NumericVector& tps,
                               const Rcpp::NumericVector& fps,
                               const Rcpp::NumericVector& tns,
                               const Rcpp::NumericVector& fns) {
  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  int n = tps.size();               // Input data size
  std::vector<double> rank(n);      // Normalized rank
  std::vector<double> errrate(n);   // Error-rate
  std::vector<double> acc(n);       // Accuracy
  std::vector<double> sp(n);        // Specificity
  std::vector<double> sn(n);        // Sensitivity
  std::vector<double> prec(n);      // Precision
  std::vector<double> mcc(n);       // Matthews correlation coefficient
  double tpfp, tpfn, tnfp, tnfn;    // For mcc calculation
  std::vector<double> fscore(n);    // F1Score

  // Vector size must be >1
  if (n < 2) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate evaluation measures for ranks
  // n should be >1
  for (int i = 0; i < n; ++i) {
    rank[i] = double(i) / double(n - 1);
    errrate[i] = (fps[i] + fns[i]) / (np + nn);
    acc[i] = 1 - errrate[i];
    if (nn == 0) {
      sp[i] = ::NA_REAL;
    } else {
      sp[i] = tns[i] / nn;
    }
    if (np == 0) {
      sn[i] = ::NA_REAL;
    } else {
      sn[i] = tps[i] / np;
    }
    if (i > 0) {
      prec[i] = tps[i] / (tps[i] + fps[i]);
    }

    tpfp = tps[i] + fps[i];
    tpfn = tps[i] + fns[i];
    tnfp = tns[i] + fps[i];
    tnfn = tns[i] + fns[i];

    if (tpfp == 0 || tpfn == 0 || tnfp == 0 || tnfn == 0) {
      mcc[i] = ::NA_REAL;
    } else {
      mcc[i] = ((tps[i] * tns[i]) - (fps[i] * fns[i]))
      / ::sqrt(tpfp * tpfn * tnfp * tnfn);
    }
    fscore[i] = (2 * tps[i]) / (2 * tps[i] + fps[i] + fns[i]);
  }

  // Update the precision value of the highest rank
  prec[0] = prec[1];

  // Return a list with P, N, and basic evaluation measures
  df["rank"] = rank;
  df["error"] = errrate;
  df["accuracy"] = acc;
  df["specificity"] = sp;
  df["sensitivity"] = sn;
  df["precision"] = prec;
  df["mcc"] = mcc;
  df["fscore"] = fscore;

  ret_val["basic"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

/*
##############################################
 Name: create_roc_curve
 R file: pl5_create_curves.R
 R func: create_roc
##############################################
*/

// Prototype
int interpolate_roc(const Rcpp::NumericVector& sp,
                    const Rcpp::NumericVector& sn,
                    int idx,
                    double x_interval,
                    std::vector<double>& fpr,
                    std::vector<double>& tpr,
                    int n);

//
// Calculate ROC curve
//
// [[Rcpp::export]]
Rcpp::List create_roc_curve(const Rcpp::NumericVector& tps,
                            const Rcpp::NumericVector& fps,
                            const Rcpp::NumericVector& sp,
                            const Rcpp::NumericVector& sn,
                            double x_bins) {
  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  double x_interval = 1.0 / x_bins;
  int max_n = sp.size() + (1.0 / x_interval);
  std::vector<double> fpr(max_n);           // False positive rate
  std::vector<double> tpr(max_n);           // True positive rate
  std::vector<bool> roc_orig(max_n, false); // true: original point

  int n = 0;

  // Interval must be >0
  if (x_interval <= 0) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate ROC points
  for (int i = 0; i < sp.size(); ++i) {
    if ((i != 0) && (sp[i] == sp[i-1])  && (sn[i] == sn[i-1])) {
      continue;
    }

    // Interpolate two points
    if (i != 0) {
      n = interpolate_roc(sp, sn, i, x_interval, fpr, tpr, n);
    }

    fpr[n] = 1 - sp[i];
    tpr[n] = sn[i];
    roc_orig[n] = true;
    ++n;
  }

  fpr.resize(n);
  tpr.resize(n);
  roc_orig.resize(n);

  // Return a list
  df["x"] = fpr;
  df["y"] = tpr;
  df["orig_points"] = roc_orig;

  ret_val["curve"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

// Linearly interpolate two ROC points
int interpolate_roc(const Rcpp::NumericVector& sp,
                    const Rcpp::NumericVector& sn,
                    int idx,
                    double x_interval,
                    std::vector<double>& fpr,
                    std::vector<double>& tpr,
                    int n) {
  double cur_fpr = 1 - sp[idx];
  double prev_fpr = 1 - sp[idx-1];
  double slope = (sn[idx] - sn[idx-1]) / (cur_fpr - prev_fpr);
  double y_interval = slope * x_interval;
  double tmp_fpr = x_interval * int(prev_fpr / x_interval);
  double tmp_tpr = sn[idx-1] + (tmp_fpr - prev_fpr) * slope;

  while (tmp_fpr < 1) {
    tmp_fpr += x_interval;
    if (tmp_fpr >= cur_fpr){
      break;
    }
    tmp_tpr += y_interval;
    if ((fpr[n-1] == tmp_fpr)  && (tpr[n-1] == tmp_tpr)) {
      continue;
    }
    fpr[n] = tmp_fpr;
    tpr[n] = tmp_tpr;
    ++n;
  }

  return n;
}

/*
##############################################
 Name: create_prc_curve
 R file: pl5_create_curves.R
 R func: create_prc
##############################################
 */

// Prototype
int interpolate_prc(const Rcpp::NumericVector& tps,
                    const Rcpp::NumericVector& fps,
                    const Rcpp::NumericVector& sn,
                    const Rcpp::NumericVector& pr,
                    int idx,
                    double x_interval,
                    std::vector<double>& rec,
                    std::vector<double>& prec,
                    int n);

//
// Calculate precision-recall curve
//
// [[Rcpp::export]]
Rcpp::List create_prc_curve(const Rcpp::NumericVector& tps,
                            const Rcpp::NumericVector& fps,
                            const Rcpp::NumericVector& sn,
                            const Rcpp::NumericVector& pr,
                            double x_bins) {
  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  double x_interval = 1.0 / x_bins;
  int max_n = sn.size() + (1.0 / x_interval);
  std::vector<double> rec(max_n);           // Recall
  std::vector<double> prec(max_n);          // Precision
  std::vector<bool> prc_orig(max_n, false); // true: original point

  int n = 0;

  // Interval must be >0
  if (x_interval <= 0) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate Precision-Recall points
  for (int i = 0; i < sn.size(); ++i) {
    if ((i != 0) && (sn[i] == sn[i-1])  && (pr[i] == pr[i-1])) {
      continue;
    }

    // Interpolate two points
    if (i != 0) {
      n = interpolate_prc(tps, fps, sn, pr, i, x_interval, rec, prec, n);
    }

    rec[n] = sn[i];
    prec[n] = pr[i];
    prc_orig[n] = true;

    ++n;
  }

  rec.resize(n);
  prec.resize(n);
  prc_orig.resize(n);

  // Return a list
  df["x"] = rec;
  df["y"] = prec;
  df["orig_points"] = prc_orig;

  ret_val["curve"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

// Non-linearly interpolate two Precision-Recall points
int interpolate_prc(const Rcpp::NumericVector& tps,
                    const Rcpp::NumericVector& fps,
                    const Rcpp::NumericVector& sn,
                    const Rcpp::NumericVector& pr,
                    int idx,
                    double x_interval,
                    std::vector<double>& rec,
                    std::vector<double>& prec,
                    int n) {
  double tmp_rec = x_interval * int(sn[idx-1] / x_interval);
  double tmp_prec;
  double x;

  while (tmp_rec < 1) {
    tmp_rec += x_interval;
    if (tmp_rec >= sn[idx]){
      break;
    }

    if (pr[idx] == pr[idx-1]) {
      tmp_prec = pr[idx];
    } else {
      x = (tmp_rec - sn[idx-1]) * tps[idx] / sn[idx];
      tmp_prec = (tps[idx-1] + x) / (tps[idx-1] + x
                                       + fps[idx-1]
                                       + (((fps[idx] - fps[idx-1]) * x)
                                       / (tps[idx] - tps[idx-1])));
    }

    if ((rec[n-1] == tmp_rec)  && (prec[n-1] == tmp_prec)) {
      continue;
    }
    rec[n] = tmp_rec;
    prec[n] = tmp_prec;

    ++n;
  }

  return n;
}


/*
##############################################
 Name: calc_auc
 R file: pl5_create_curves.R
 R func: .create_curve
##############################################
*/

//
// Calculate area under the curve
//
// [[Rcpp::export]]
Rcpp::List calc_auc(const Rcpp::NumericVector& xs,
                    const Rcpp::NumericVector& ys) {
  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  double auc = 0;

  // Calculate AUC
  for (int i = 1; i < xs.size(); ++i) {
    if (xs[i] > xs[i-1]) {
      auc += 0.5 * (ys[i] + ys[i-1]) * (xs[i] - xs[i-1]);
    } else if (xs[i] < xs[i-1]) {
      errmsg = "invalid-x-vals";
    }
  }

  // Return a list
  ret_val["auc"] = auc;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

/*
##############################################
 Name: calc_avg_curve
 R file: pl6_calc_average.R
 R func: .calc_avg_common
##############################################
*/

#define EPSILON_D (1.0E-8)

// Prototype
void get_yval_single(const Rcpp::NumericVector& xs,
                     const Rcpp::NumericVector& ys,
                     double x_interval,
                     double x_bins,
                     int vec_size,
                     std::vector<double>& s_y_val);

//
// Calculate average curves
//
// [[Rcpp::export]]
Rcpp::List calc_avg_curve(const Rcpp::List& curves,
                          double x_bins,
                          double ci_q) {
  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  double x_interval = 1.0 / x_bins;


  int vec_size = 3 + (1.0 / x_interval);

  std::vector<double> x_val(vec_size);         // x values
  std::vector<double> avg_y(vec_size);         // Average
  std::vector<double> se_y(vec_size);          // SE
  std::vector<double> ci_h_y(vec_size);        // CI upper bound
  std::vector<double> ci_l_y(vec_size);        // CI lower bound

  std::vector<double> tot_y(vec_size, 0.0);    // Total of ys
  std::vector<double> stot_y(vec_size, 0.0);   // Total of squared ys
  std::vector<double> s_y_val(vec_size, 0.0);  // x values of a single curve
  int n = curves.size();

  // Calculate total
  for (int i = 0; i < n; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(curves[i]);

    get_yval_single(c["x"], c["y"], x_interval, x_bins, vec_size, s_y_val);

    for (int j = 0; j < vec_size; ++j) {
      tot_y[j] += s_y_val[j];
      stot_y[j] += (s_y_val[j] * s_y_val[j]);
    }
    s_y_val.clear();
    s_y_val.resize(vec_size, 0.0);
  }

  // Calculate average & CI
  double exp2;
  double sd;
  for (int i = 0; i < vec_size; ++i) {
    // x
    if (i == 0) {
      x_val[i] = 0;
    } else if (i == vec_size - 1) {
      x_val[i] = 1;
    } else {
      x_val[i] = (i - 1) * x_interval;
    }

    // y
    avg_y[i] = tot_y[i] / double(n);

    // se
    exp2 = (stot_y[i] / double(n)) - (avg_y[i] * avg_y[i]);
    if (exp2 < 0){
      exp2 = 0;
    }
    sd =  ::sqrt(double(n) / double(n - 1)) * ::sqrt(exp2);
    se_y[i] = sd / ::sqrt(double(n));

    // ci upper bound
    ci_h_y[i] = avg_y[i] + ci_q * se_y[i];

    // ci lower bound
    ci_l_y[i] = avg_y[i] - ci_q * se_y[i];
  }

  // Return a list
  df["x"] = x_val;
  df["y_avg"] = avg_y;
  df["y_se"] = se_y;
  df["y_ci_h"] = ci_h_y;
  df["y_ci_l"] = ci_l_y;
  ret_val["avg"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

void get_yval_single(const Rcpp::NumericVector& xs,
                     const Rcpp::NumericVector& ys,
                     double x_interval,
                     double x_bins,
                     int vec_size,
                     std::vector<double>& s_y_val) {
  std::vector<double> y_tot(vec_size, 0.0); // Total of ys
  std::vector<int> n_y(vec_size, 0);        // Number of each point
  std::set<double> x_set;
  int idx;
  double rounded_xval;

  // x = 0
  s_y_val[0] = ys[0];
  n_y[0]  = 1;
  idx = 1;
  while (xs[idx] == 0.0) {
    s_y_val[1] = ys[idx];
    n_y[1]  = 1;
    ++idx;
  }
  if (n_y[1] == 0) {
    s_y_val[1] = s_y_val[0];
  }
  n_y[1]  = 1;

  // 0 < x < 1
  for (int j = 1; j < xs.size() - 1; ++j) {
    if (xs[j] == 0.0 || xs[j] == 1.0) {
      continue;
    }
    rounded_xval = ::roundf(xs[j] * x_bins) / x_bins;
    if (::fabs(rounded_xval - xs[j]) > EPSILON_D) {
      continue;
    }
    idx = int(::roundf(rounded_xval * x_bins) + 1.0);
    s_y_val[idx] = ys[j];
    ++n_y[idx];
  }

  // x = 1
  s_y_val[vec_size - 1] = ys[ys.size() - 1];
  n_y[vec_size - 1]  = 1;
  idx = ys.size() - 2;
  while (xs[idx] == 1.0) {
    s_y_val[vec_size - 2] = ys[idx];
    n_y[vec_size - 2] = 1;
    --idx;
  }
  if (n_y[vec_size - 2] == 0) {
    s_y_val[vec_size - 2] = s_y_val[vec_size - 1];
  }
  n_y[vec_size - 2]  = 1;

  //Treat missing values
  for (int i = 0; i < vec_size; ++i) {
    if (n_y[i] == 0) {
      if (n_y[i-1] != 0 && n_y[i+1] != 0) {
        s_y_val[i] = (s_y_val[i-1] + s_y_val[i+1]) / 2.0;
      } else if (n_y[i-1] != 0) {
        s_y_val[i] = s_y_val[i-1];
      } else if (n_y[i+1] != 0) {
        s_y_val[i] = s_y_val[i+1];
      }
    }
  }
}

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <set>
#include <map>
#include <string>

/*
##############################################
 Name: calc_avg_points
 R file: pl6_calc_average.R
 R func: .calc_avg_common
##############################################
*/

//
// Calculate average points
//
// [[Rcpp::export]]
Rcpp::List calc_avg_points(const Rcpp::List& points, double ci_q) {
  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";

  std::set<double> all_x_vals;       // all x values
  std::map<double, int> x_vals_idx;  // map x values to indices

  std::vector<double> x_val;         // x values
  std::vector<double> avg_y;         // Average
  std::vector<double> se_y;          // SE
  std::vector<double> ci_h_y;        // CI upper bound
  std::vector<double> ci_l_y;        // CI lower bound

  std::vector<double> tot_y;         // Total of ys
  std::vector<int> count_y;          // Count of ys
  std::vector<double> stot_y;        // Total of squared ys

  // Create all unique x values
  for (int i = 0; i < points.size(); ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];

    for (int j = 0; j < xs.size(); ++j) {
      all_x_vals.insert(xs[j]);
    }
  }

  // Resize vectors
  int vec_size = all_x_vals.size();
  x_val.resize(vec_size, 0.0);
  avg_y.resize(vec_size, 0.0);
  se_y.resize(vec_size, 0.0);
  ci_h_y.resize(vec_size, 0.0);
  ci_l_y.resize(vec_size, 0.0);
  tot_y.resize(vec_size, 0.0);
  count_y.resize(vec_size, 0);
  stot_y.resize(vec_size, 0.0);

  // Make maps x vals to indices
  std::set<double>::iterator set_it;
  int idx = 0;
  for (set_it = all_x_vals.begin(); set_it != all_x_vals.end(); ++set_it) {
    x_val[idx] = *set_it;
    x_vals_idx.insert(std::pair<double, int>(*set_it, idx));
    ++idx;
  }

  // Calculate total
  idx = 0;
  for (int i = 0; i < points.size(); ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];
    Rcpp::NumericVector ys = c["y"];

    for (int j = 0; j < ys.size(); ++j) {
      idx = x_vals_idx[xs[j]];

      tot_y[idx] += ys[j];
      stot_y[idx] += (ys[j] * ys[j]);
      ++count_y[idx];
    }
  }

  // Calculate average & CI
  double exp2;
  double sd;
  double n;
  for (int i = 0; i < vec_size; ++i) {
    n = double(count_y[i]);

    // y
    avg_y[i] = tot_y[i] / n;

    // se
    exp2 = (stot_y[i] / n) - (avg_y[i] * avg_y[i]);
    if (exp2 < 0){
      exp2 = 0;
    }
    sd =  ::sqrt(n / (n - 1)) * ::sqrt(exp2);
    se_y[i] = sd / ::sqrt(n);

    // ci upper bound
    ci_h_y[i] = avg_y[i] + ci_q * se_y[i];

    // ci lower bound
    ci_l_y[i] = avg_y[i] - ci_q * se_y[i];
  }

  // Return a list
  df["x"] = x_val;
  df["y_avg"] = avg_y;
  df["y_se"] = se_y;
  df["y_ci_h"] = ci_h_y;
  df["y_ci_l"] = ci_l_y;
  ret_val["avg"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}



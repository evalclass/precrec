#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <string>
#include <limits>
#include <algorithm>
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
                const unsigned n, unsigned& np, unsigned& nn,
                std::vector<double>& tp, std::vector<double>& fp,
                std::vector<double>& sorted_ranks);

void solve_ties(std::vector<double>& tp, std::vector<double>& fp,
                unsigned curpos, unsigned ties);

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
  const unsigned n = olabs.size();    // Input data size
  const unsigned nvec = n + 1;        // Vector size

  unsigned np;                                 // # of positive
  unsigned nn;                                 // # of negatives
  std::vector<double> tp(nvec);           // TPs
  std::vector<double> fp(nvec);           // FPs
  std::vector<double> tn(nvec);           // TNs
  std::vector<double> fn(nvec);           // FNs
  std::vector<double> sorted_ranks(nvec); // Ranks

  // Calculate TPs and FPs
  calc_tp_fp(olabs, ranks, rank_idx, n, np, nn, tp, fp, sorted_ranks);

  // Calculate TNs and FNs
  for (unsigned i = 0; i < nvec; ++i) {
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
                const unsigned n, unsigned& np, unsigned& nn,
                std::vector<double>& tp, std::vector<double>& fp,
                std::vector<double>& sorted_ranks) {
  unsigned ties = 0;
  double prev_rank = 0;

  // Initialize
  np = 0;
  nn = 0;
  tp[0] = 0;
  fp[0] = 0;
  sorted_ranks[0] = ranks[rank_idx[0] - 1] - 1;

  // Iterate all ranks
  for (unsigned i = 0; i < n; ++i) {
    const unsigned idx = rank_idx[i] - 1;

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
                unsigned curpos, unsigned ties) {
  const double tied_tp = (tp[curpos] - tp[curpos-ties-1]) / (ties + 1);
  const double tied_fp = (fp[curpos] - fp[curpos-ties-1]) / (ties + 1);
  for (unsigned i = 0; i < ties; ++i) {
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
  const double np_dbl = static_cast<const double>(np);
  const double nn_dbl = static_cast<const double>(nn);

  // Determin NA values
  // The sentinel must sort below (na_worst) or above (!na_worst) every real
  // score. DBL_MIN is the smallest *positive* double, so it would rank NAs
  // above every negative score; lowest() is the most negative one.
  double na_val;
  if (na_worst) {
    na_val = std::numeric_limits<double>::lowest();
  } else {
    na_val = std::numeric_limits<double>::max();
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
      ustat += static_cast<double>(neg_idx);
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
  const double np_dbl = static_cast<const double>(np);
  const double nn_dbl =  static_cast<const double>(nn);

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
                               const Rcpp::NumericVector& fns,
                               double beta = 1.0,
                               bool extra_measures = true) {
  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  const unsigned n = tps.size();               // Input data size
  std::vector<double> rank(n);      // Normalized rank
  std::vector<double> errrate(n);   // Error-rate
  std::vector<double> acc(n);       // Accuracy
  std::vector<double> sp(n);        // Specificity
  std::vector<double> sn(n);        // Sensitivity
  std::vector<double> prec(n);      // Precision
  std::vector<double> mcc(n);       // Matthews correlation coefficient
  double tpfp, tpfn, tnfp, tnfn;    // For mcc calculation
  std::vector<double> fscore(n);    // F-score

  // The curve pipeline reads specificity, sensitivity and precision and
  // nothing else, so it asks for the measures below to be left out rather
  // than filling five more vectors the length of the input for nobody
  const unsigned n_extra = extra_measures ? n : 0;
  std::vector<double> bacc(n_extra);   // Balanced accuracy
  std::vector<double> npv(n_extra);    // Negative predictive value
  std::vector<double> infm(n_extra);   // Informedness (Youden's J)
  std::vector<double> mkd(n_extra);    // Markedness
  std::vector<double> kappa(n_extra);  // Cohen's kappa

  // Vector size must be >1
  if (n < 2) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Loop-invariant, so lifted out. These stay divisions on purpose:
  // multiplying by a precomputed reciprocal would move every published
  // measure by an ulp, and the loop is dominated by a sqrt and eight
  // stores rather than by three divides.
  const double d_ranks = static_cast<double>(n - 1);
  const double d_all = static_cast<double>(np) + static_cast<double>(nn);
  const double d_nn = static_cast<double>(nn);
  const double d_np = static_cast<double>(np);
  const bool no_nn = (nn == 0);
  const bool no_np = (np == 0);

  // F-beta weights recall beta^2 times as heavily as precision; beta == 1
  // gives back the F1 score this used to compute unconditionally.
  const double beta2 = beta * beta;
  const double beta2_1 = 1.0 + beta2;

  // Calculate evaluation measures for ranks
  // n should be >1
  for (unsigned i = 0; i < n; ++i) {
    rank[i] = i / d_ranks;
    errrate[i] = (fps[i] + fns[i]) / d_all;
    acc[i] = 1 - errrate[i];
    if (no_nn) {
      sp[i] = ::NA_REAL;
    } else {
      sp[i] = tns[i] / d_nn;
    }
    if (no_np) {
      sn[i] = ::NA_REAL;
    } else {
      sn[i] = tps[i] / d_np;
    }
    if (extra_measures) {
      if (no_nn || no_np) {
        bacc[i] = ::NA_REAL;
        infm[i] = ::NA_REAL;
      } else {
        bacc[i] = (sn[i] + sp[i]) / 2;
        infm[i] = sn[i] + sp[i] - 1;
      }
    }

    tpfp = tps[i] + fps[i];
    tpfn = tps[i] + fns[i];
    tnfp = tns[i] + fps[i];
    tnfn = tns[i] + fns[i];

    // Nothing is predicted positive at the first rank and nothing is
    // predicted negative at the last one, so precision and NPV each have one
    // undefined end. Both are filled in from their neighbour below.
    if (i > 0) {
      prec[i] = tps[i] / tpfp;
    }
    if (extra_measures && i + 1 < n) {
      npv[i] = tns[i] / tnfn;
    }

    if (tpfp == 0 || tpfn == 0 || tnfp == 0 || tnfn == 0) {
      mcc[i] = ::NA_REAL;
    } else {
      mcc[i] = ((tps[i] * tns[i]) - (fps[i] * fns[i]))
      / ::sqrt(tpfp * tpfn * tnfp * tnfn);
    }
    fscore[i] = (beta2_1 * tps[i])
      / (beta2_1 * tps[i] + beta2 * fns[i] + fps[i]);

    if (extra_measures) {
      // Cohen's kappa: observed agreement against the agreement two raters
      // with these margins would reach by chance
      const double pe = ((tpfp * tpfn) + (tnfn * tnfp)) / (d_all * d_all);
      if (pe == 1) {
        kappa[i] = ::NA_REAL;
      } else {
        kappa[i] = (acc[i] - pe) / (1 - pe);
      }

      mkd[i] = prec[i] + npv[i] - 1;
    }
  }

  // Update the precision value of the highest rank
  prec[0] = prec[1];
  if (extra_measures) {
    // The NPV of the lowest rank is undefined in the same way, and the two
    // markedness values built from the two patched cells follow
    npv[n - 1] = npv[n - 2];
    mkd[0] = prec[0] + npv[0] - 1;
    mkd[n - 1] = prec[n - 1] + npv[n - 1] - 1;
  }

  // Return a list with P, N, and basic evaluation measures
  df["rank"] = rank;
  df["error"] = errrate;
  df["accuracy"] = acc;
  df["specificity"] = sp;
  df["sensitivity"] = sn;
  df["precision"] = prec;
  df["mcc"] = mcc;
  df["fscore"] = fscore;
  if (extra_measures) {
    df["balanced_accuracy"] = bacc;
    df["npv"] = npv;
    df["informedness"] = infm;
    df["markedness"] = mkd;
    df["kappa"] = kappa;
  }

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
                    const unsigned idx,
                    const double x_interval,
                    std::vector<double>& fpr,
                    std::vector<double>& tpr,
                    unsigned n);

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
  double x_interval;
  unsigned max_n;
  if (x_bins > 0) {
    x_interval = 1.0 / x_bins;
    max_n = sp.size() + (1.0 / x_interval);
  } else {
    x_interval = 0;
    max_n = sp.size() ;
  }
  std::vector<double> fpr(max_n);           // False positive rate
  std::vector<double> tpr(max_n);           // True positive rate
  std::vector<bool> roc_orig(max_n, false); // true: original point

  unsigned n = 0;

  // Interval must be >0
  if (x_interval < 0) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate ROC points
  for (unsigned i = 0; i < sp.size(); ++i) {
    if ((i != 0) && (sp[i] == sp[i-1])  && (sn[i] == sn[i-1])) {
      continue;
    }

    // Interpolate two points
    if ((x_interval > 0) && (i > 0)) {
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
                    const unsigned idx,
                    const double x_interval,
                    std::vector<double>& fpr,
                    std::vector<double>& tpr,
                    unsigned n) {
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
                    const unsigned idx,
                    const double x_interval,
                    std::vector<double>& rec,
                    std::vector<double>& prec,
                    unsigned n);

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
  double x_interval;
  unsigned max_n;
  if (x_bins > 0) {
    x_interval = 1.0 / x_bins;
    max_n = sn.size() + (1.0 / x_interval);
  } else {
    x_interval = 0;
    max_n = sn.size() ;
  }
  std::vector<double> rec(max_n);           // Recall
  std::vector<double> prec(max_n);          // Precision
  std::vector<bool> prc_orig(max_n, false); // true: original point

  int n = 0;

  // Interval must be >=0
  if (x_interval < 0) {
    errmsg = "invalid-vecsize-1";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // Calculate Precision-Recall points
  for (unsigned i = 0; i < sn.size(); ++i) {
    if ((i != 0) && (sn[i] == sn[i-1])  && (pr[i] == pr[i-1])) {
      continue;
    }

    // Interpolate two points
    if ((x_interval > 0) && (i > 0)) {
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
                    const unsigned idx,
                    const double x_interval,
                    std::vector<double>& rec,
                    std::vector<double>& prec,
                    unsigned n) {
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
  for (unsigned i = 1; i < xs.size(); ++i) {
    if (xs[i] >= xs[i-1]) {
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
                     const unsigned vec_size,
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
  const unsigned  vec_size = 3 + (1.0 / x_interval);
  const unsigned n = curves.size();

  std::vector<double> x_val(vec_size);         // x values
  std::vector<double> avg_y(vec_size);         // Average
  std::vector<double> se_y(vec_size);          // SE
  std::vector<double> ci_h_y(vec_size);        // CI upper bound
  std::vector<double> ci_l_y(vec_size);        // CI lower bound
  std::vector<double> mean_y(vec_size, 0.0);   // Running mean of ys
  std::vector<double> m2_y(vec_size, 0.0);     // Running sum of squared devs
  std::vector<double> s_y_val(vec_size, 0.0);  // x values of a single curve

  // Accumulate mean and variance with Welford's online algorithm
  for (unsigned i = 0; i < n; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(curves[i]);

    get_yval_single(c["x"], c["y"], x_interval, x_bins, vec_size, s_y_val);

    for (unsigned j = 0; j < vec_size; ++j) {
      const double delta = s_y_val[j] - mean_y[j];
      mean_y[j] += delta / static_cast<double>(i + 1);
      m2_y[j] += delta * (s_y_val[j] - mean_y[j]);
    }
    s_y_val.clear();
    s_y_val.resize(vec_size, 0.0);
  }

  // Calculate average & CI
  double sd;
  for (unsigned i = 0; i < vec_size; ++i) {
    // x
    if (i == 0) {
      x_val[i] = 0;
    } else if (i == vec_size - 1) {
      x_val[i] = 1;
    } else {
      x_val[i] = (i - 1) * x_interval;
    }

    // y
    avg_y[i] = mean_y[i];

    // se
    sd = ::sqrt(m2_y[i] / (double(n) - 1.0));
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
                     const unsigned vec_size,
                     std::vector<double>& s_y_val) {
  std::vector<double> y_tot(vec_size, 0.0); // Total of ys
  std::vector<int> n_y(vec_size, 0);        // Number of each point
  unsigned idx;
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
  for (unsigned j = 1; j < static_cast<unsigned>(xs.size() - 1); ++j) {
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

  // Treat missing values
  for (unsigned i = 0; i < vec_size; ++i) {
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

  const unsigned n_curves = static_cast<unsigned>(points.size());

  // Collect every x value of every dataset
  unsigned n_all_x = 0;
  for (unsigned i = 0; i < n_curves; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];
    n_all_x += static_cast<unsigned>(xs.size());
  }

  std::vector<double> x_val;
  x_val.reserve(n_all_x);
  for (unsigned i = 0; i < n_curves; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];
    x_val.insert(x_val.end(), xs.begin(), xs.end());
  }

  // Reduce to the unique values, in ascending order. A sorted vector plus a
  // binary search replaces the std::set and std::map this used to build:
  // both allocated a node per value and chased a pointer per comparison,
  // and the lookup below runs once for every point of every dataset.
  std::sort(x_val.begin(), x_val.end());
  x_val.erase(std::unique(x_val.begin(), x_val.end()), x_val.end());
  std::vector<double>(x_val).swap(x_val);  // Release the collection slack

  const unsigned vec_size = static_cast<const unsigned>(x_val.size());
  const std::vector<double>::const_iterator x_beg = x_val.begin();
  const std::vector<double>::const_iterator x_end = x_val.end();

  std::vector<double> mean_y(vec_size, 0.0);  // Running mean of ys
  std::vector<int> count_y(vec_size, 0);      // Count of ys
  std::vector<double> m2_y(vec_size, 0.0);    // Running sum of squared devs

  // Accumulate mean and variance with Welford's online algorithm
  for (unsigned i = 0; i < n_curves; ++i) {
    Rcpp::List c = Rcpp::as<Rcpp::List>(points[i]);
    Rcpp::NumericVector xs = c["x"];
    Rcpp::NumericVector ys = c["y"];

    for (unsigned j = 0; j < static_cast<unsigned>(ys.size()); ++j) {
      const unsigned idx = static_cast<unsigned>(
        std::lower_bound(x_beg, x_end, xs[j]) - x_beg);

      ++count_y[idx];
      const double delta = ys[j] - mean_y[idx];
      mean_y[idx] += delta / static_cast<double>(count_y[idx]);
      m2_y[idx] += delta * (ys[j] - mean_y[idx]);
    }
  }

  // Calculate average & CI. Filled in place so that the results are never
  // held twice, once in a std::vector and once in the wrapped copy.
  Rcpp::NumericVector out_x(Rcpp::no_init(vec_size));
  Rcpp::NumericVector avg_y(Rcpp::no_init(vec_size));
  Rcpp::NumericVector se_y(Rcpp::no_init(vec_size));
  Rcpp::NumericVector ci_h_y(Rcpp::no_init(vec_size));
  Rcpp::NumericVector ci_l_y(Rcpp::no_init(vec_size));

  for (unsigned i = 0; i < vec_size; ++i) {
    const double n = static_cast<double>(count_y[i]);

    // x
    out_x[i] = x_val[i];

    // y
    avg_y[i] = mean_y[i];

    // se
    const double sd = ::sqrt(m2_y[i] / (n - 1.0));
    se_y[i] = sd / ::sqrt(n);

    // ci upper bound
    ci_h_y[i] = avg_y[i] + ci_q * se_y[i];

    // ci lower bound
    ci_l_y[i] = avg_y[i] - ci_q * se_y[i];
  }

  // Return a list
  df["x"] = out_x;
  df["y_avg"] = avg_y;
  df["y_se"] = se_y;
  df["y_ci_h"] = ci_h_y;
  df["y_ci_l"] = ci_l_y;
  ret_val["avg"] = df;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}


/*
##############################################
 Name: calc_prob_metrics
 R file: g_prob_metrics.R
 R func: prob_metrics
##############################################
*/

//
// Calculate the Brier score and the log loss of predicted probabilities
//
// [[Rcpp::export]]
Rcpp::List calc_prob_metrics(const Rcpp::NumericVector& scores,
                             const Rcpp::IntegerVector& labels,
                             double eps) {
  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  const unsigned n = static_cast<unsigned>(scores.size());

  // Vector size must be >0 and the two vectors must agree
  if (n == 0 || static_cast<unsigned>(labels.size()) != n) {
    errmsg = "invalid-vecsize";
    ret_val["errmsg"] = errmsg;
    return ret_val;
  }

  // One pass, and no intermediate vector: both metrics are a mean over the
  // same two values, so there is nothing to hold on to between elements.
  double sum_sq = 0.0;
  double sum_ll = 0.0;
  const double hi = 1.0 - eps;

  for (unsigned i = 0; i < n; ++i) {
    const double p = scores[i];
    const double y = static_cast<double>(labels[i]);
    const double d = p - y;
    sum_sq += d * d;

    // A probability of exactly 0 or 1 that turns out to be wrong makes the
    // log loss infinite. Clamping keeps a single such prediction from
    // swallowing the whole sample, which is what every other implementation
    // of this measure does too.
    double q = p;
    if (q < eps) {
      q = eps;
    } else if (q > hi) {
      q = hi;
    }
    sum_ll += y * ::log(q) + (1.0 - y) * ::log(1.0 - q);
  }

  const double d_n = static_cast<double>(n);
  ret_val["brier"] = sum_sq / d_n;
  ret_val["logloss"] = -sum_ll / d_n;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

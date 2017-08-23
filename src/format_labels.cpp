#include <Rcpp.h>
#include <vector>
#include <string>

// Make new labels - negative: 1, positive: 2
template<typename T, typename S>
Rcpp::List make_new_labels(T labels,
                           S posclass,
                           bool is_pc_na) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  std::vector<double> new_labels(labels.size());
  int nn = 0;
  int np = 0;

  // Get two labels
  S lab_p = labels[0];
  S lab_n;
  for (unsigned i = 0; i < labels.size(); ++i) {
    if (lab_p != labels[i]) {
      lab_n = labels[i];
      break;
    }
  }

  // Find positive label
  if (is_pc_na) {
    if (lab_p < lab_n) {
      S lab_tmp = lab_p;
      lab_p = lab_n;
      lab_n = lab_tmp;
    }
  } else {
    if (lab_n == posclass) {
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
      return make_new_labels<Rcpp::IntegerVector, int>
             (labels, pos_class_i[0], is_pc_na);
    }
    case REALSXP: {
      Rcpp::NumericVector pos_class_d = Rcpp::as<Rcpp::NumericVector>(posclass);
      is_pc_na = Rcpp::NumericVector::is_na(pos_class_d[0]);
      return make_new_labels<Rcpp::NumericVector, double>
             (labels, pos_class_d[0], is_pc_na);
    }
    case LGLSXP: {
      Rcpp::LogicalVector pos_class_b = Rcpp::as<Rcpp::LogicalVector>(posclass);
      is_pc_na = Rcpp::LogicalVector::is_na(pos_class_b[0]);
      return make_new_labels<Rcpp::LogicalVector, bool>
             (labels, pos_class_b[0], is_pc_na);
    }
    case STRSXP: {
      Rcpp::CharacterVector pos_class_c = Rcpp::as<Rcpp::CharacterVector>(posclass);
      std::string pos_class_c2(pos_class_c[0]);
      is_pc_na = Rcpp::CharacterVector::is_na(pos_class_c[0]);
      std::vector<std::string> labels_s = Rcpp::as<std::vector<std::string> >(labels);
      return make_new_labels<std::vector<std::string>, std::string>
             (labels_s, pos_class_c2, is_pc_na);
    }
    default:
      Rcpp::List ret_val;
      ret_val["errmsg"] = "incompatible-SEXP";
      return ret_val;
  }
}

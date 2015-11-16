#include <Rcpp.h>
#include <vector>
#include <set>
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

  std::set<S> label_set;
  typename std::set<S>::iterator it;
  S lab2 = labels[0];

  // Check two labels
  for (unsigned i = 0; i < labels.size(); ++i) {
    label_set.insert(labels[i]);
  }

  // Find positive label
  if (is_pc_na) {
    if (label_set.size() != 2) {
      ret_val["errmsg"] = "invalid-labels";
      return ret_val;
    }
    for (it = label_set.begin(); it != label_set.end(); ++it) {
      if (lab2 < *it) {
        lab2 = *it;
      }
    }
  } else {
    it = label_set.find(posclass);
    if (it == label_set.end()) {
      ret_val["errmsg"] = "invalid-posclass";
      return ret_val;
    }
    lab2 = posclass;
  }

  // Make new labels - negative 1 & positive 2
  for (unsigned i = 0; i < labels.size(); ++i) {
    if (labels[i] == lab2) {
      ++np;
      new_labels[i] = 2;
    } else {
      ++nn;
      new_labels[i] = 1;
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

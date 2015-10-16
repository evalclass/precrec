#include <Rcpp.h>
#include <string>

// Format labels - 0: nagative, 1: positive: 2
// [[Rcpp::export]]
Rcpp::List format_labels(Rcpp::NumericVector& labels) {

  // Variables
  Rcpp::List ret_val;
  std::string errmsg = "";
  Rcpp::NumericVector new_labels(labels.size());
  double lab1 = labels[0];
  double lab2 = labels[0];
  bool found = false;
  int nn = 0;
  int np = 0;

  // Find two labels
  for (int i = 0; i < labels.size(); ++i) {
    if (labels[i] > lab2) {
      if (found) {
         errmsg = "invalid-labels";
         ret_val["errmsg"] = errmsg;
         return ret_val;
      }
      lab2 = labels[i];
      found = true;
    } else if (labels[i] < lab1) {
      if (found) {
        errmsg = "invalid-labels";
        ret_val["errmsg"] = errmsg;
        return ret_val;
       }
       lab1 = labels[i];
       found = true;
    }
  }

  // Make new labels - negative 1 & positive 2
  for (int i = 0; i < labels.size(); ++i) {
    if (labels[i] == lab1) {
      ++nn;
      new_labels[i] = 1;
    } else {
      ++np;
      new_labels[i] = 2;
    }
  }
  ret_val["nn"] = nn;
  ret_val["np"] = np;
  ret_val["labels"] = new_labels;
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

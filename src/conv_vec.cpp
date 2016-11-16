#include <Rcpp.h>
#include <string>

//
// Calculate vector size
//
int calc_vec_size(const Rcpp::List& obj,
                  const Rcpp::List& curvetype_names) {

  int vec_size = 0;

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    std::string lblctype = Rcpp::as<std::string>(curvetypes[i]);
    std::string idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    Rcpp::List curves = Rcpp::as<Rcpp::List>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      Rcpp::List xys = Rcpp::as<Rcpp::List>(curves[j]);
      Rcpp::NumericVector xs = Rcpp::as<Rcpp::NumericVector>(xys["x"]);
      vec_size += xs.size();
    }
  }

  return(vec_size);
}

//
// Copy vector
//
int copy_xy_vec(const Rcpp::NumericVector& from_vec,
                Rcpp::NumericVector& to_vec,
                int start_idx) {

  for (unsigned i = 0; i < from_vec.size(); i++) {
    to_vec[start_idx+i] = from_vec[i];
  }

  return (int)from_vec.size();
}

//
// Create vector
//
void create_vec(Rcpp::IntegerVector& vec, int size, int value, int start_idx) {

  for (unsigned i = 0; i < size; i++) {
    vec[start_idx+i] = value;
  }
}

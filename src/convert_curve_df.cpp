#include <Rcpp.h>
#include <string>
#include "conv_vec.h"

//
// Convert curve dataframe
//
// [[Rcpp::export]]
Rcpp::List convert_curve_df(const Rcpp::List& obj,
                            const Rcpp::CharacterVector& uniq_modnames,
                            const Rcpp::CharacterVector& uniq_dsids,
                            const Rcpp::IntegerVector& modnames,
                            const Rcpp::IntegerVector& dsids,
                            const Rcpp::CharacterVector& dsid_modnames,
                            const Rcpp::List& curvetype_names) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  std::string lblctype;
  std::string idxctype;
  int vec_size = calc_vec_size(obj, curvetype_names);

  Rcpp::NumericVector xs(vec_size);
  Rcpp::NumericVector ys(vec_size);
  Rcpp::IntegerVector modname(vec_size);
  Rcpp::IntegerVector dsid(vec_size);
  Rcpp::IntegerVector dsid_modname(vec_size);
  Rcpp::IntegerVector curvetype(vec_size);

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  int start_idx = 0;
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    lblctype = Rcpp::as<std::string>(curvetypes[i]);
    idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    Rcpp::List curves = Rcpp::as<Rcpp::List>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      Rcpp::List xys = Rcpp::as<Rcpp::List>(curves[j]);
      Rcpp::NumericVector x = Rcpp::as<Rcpp::NumericVector>(xys["x"]);
      Rcpp::NumericVector y = Rcpp::as<Rcpp::NumericVector>(xys["y"]);

      int n = copy_xy_vec(x, xs, start_idx);
      (void)copy_xy_vec(y, ys, start_idx);

      create_vec(modname, n, modnames[j], start_idx);
      create_vec(dsid, n, dsids[j], start_idx);
      create_vec(dsid_modname, n, j+1, start_idx);
      create_vec(curvetype, n, i+1, start_idx);

      start_idx += n;
    }
  }

  modname.attr("levels") = uniq_modnames;
  modname.attr("class") = "factor";
  dsid.attr("levels") = uniq_dsids;
  dsid.attr("class") = "factor";
  dsid_modname.attr("levels") = dsid_modnames;
  dsid_modname.attr("class") = "factor";
  curvetype.attr("levels") = curvetypes;
  curvetype.attr("class") = "factor";

  // Return a list
  ret_val["df"] = Rcpp::DataFrame::create(Rcpp::_["x"]= xs,
    Rcpp::_["y"]= ys,
    Rcpp::_["modname"] = modname,
    Rcpp::_["dsid"] = dsid,
    Rcpp::_["dsid_modname"] = dsid_modname,
    Rcpp::_["curvetype"] = curvetype);
  ret_val["errmsg"] = errmsg;

  return ret_val;
}

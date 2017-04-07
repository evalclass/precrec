#include <Rcpp.h>
#include <vector>
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
                            const Rcpp::List& curvetype_names,
                            const int x_bins) {

  // Variables
  Rcpp::List ret_val;
  Rcpp::DataFrame df;
  std::string errmsg = "";
  std::string lblctype;
  std::string idxctype;
  bool reduce_points = false;
  if (x_bins > 1){
    reduce_points = true;
  }
  int vec_size = calc_vec_size(obj, curvetype_names);

  std::vector<double> vec_xs(vec_size);
  std::vector<double> vec_ys(vec_size);
  std::vector<int> vec_modname(vec_size);
  std::vector<int> vec_dsid(vec_size);
  std::vector<int> vec_dsid_modname(vec_size);
  std::vector<int> vec_curvetype(vec_size);
  std::vector<bool> vec_points(vec_size);

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  int start_idx = 0;
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    lblctype = Rcpp::as<std::string>(curvetypes[i]);
    idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    Rcpp::List curves = Rcpp::as<Rcpp::List>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      int n = 0;
      Rcpp::List xys = Rcpp::as<Rcpp::List>(curves[j]);
      Rcpp::NumericVector x = Rcpp::as<Rcpp::NumericVector>(xys["x"]);
      Rcpp::NumericVector y = Rcpp::as<Rcpp::NumericVector>(xys["y"]);

      if (reduce_points){
        vec_points.resize(x.size(), false);
        n = set_reduced_points(x, vec_points, x_bins);

        copy_reduced_xy_vec(x, vec_xs, start_idx, vec_points);
        copy_reduced_xy_vec(y, vec_ys, start_idx, vec_points);
      } else {
        n = x.size();
        copy_xy_vec(x, vec_xs, start_idx);
        copy_xy_vec(y, vec_ys, start_idx);
      }

      add_to_vec(vec_modname, n, modnames[j], start_idx);
      add_to_vec(vec_dsid, n, dsids[j], start_idx);
      add_to_vec(vec_dsid_modname, n, j+1, start_idx);
      add_to_vec(vec_curvetype, n, i+1, start_idx);

      start_idx += n;
    }
  }

  if (reduce_points){
    vec_xs.resize(start_idx);
    vec_ys.resize(start_idx);
    vec_modname.resize(start_idx);
    vec_dsid.resize(start_idx);
    vec_dsid_modname.resize(start_idx);
    vec_curvetype.resize(start_idx);
  }

  Rcpp::NumericVector xs = Rcpp::wrap(vec_xs);
  Rcpp::NumericVector ys = Rcpp::wrap(vec_ys);
  Rcpp::IntegerVector modname = Rcpp::wrap(vec_modname);
  Rcpp::IntegerVector dsid = Rcpp::wrap(vec_dsid);
  Rcpp::IntegerVector dsid_modname = Rcpp::wrap(vec_dsid_modname);
  Rcpp::IntegerVector curvetype = Rcpp::wrap(vec_curvetype);

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

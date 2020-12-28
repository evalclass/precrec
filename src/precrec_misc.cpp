#include <Rcpp.h>
#include <vector>
#include <string>
#include <ctime>

/*
##############################################
 Common functions
##############################################
*/

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
void copy_xy_vec(const Rcpp::NumericVector& from_vec,
                 std::vector<double>& to_vec,
                 int start_idx) {
  for (unsigned i = 0; i < from_vec.size(); i++) {
    to_vec[start_idx+i] = from_vec[i];
  }
}

//
// Add to vector
//
void add_to_vec(std::vector<int>& vec, int size, int value, int start_idx) {
  for (unsigned i = 0; i < size; i++) {
    vec[start_idx+i] = value;
  }
}

//
// Set reduced points
//
int set_reduced_points(const Rcpp::NumericVector& from_vec,
                       std::vector<bool>& points,
                       const int x_bins) {
  double x_pos = 0.0;
  double step = 1.0 / x_bins;
  double eps = std::numeric_limits<double>::epsilon() * x_bins;
  int n = 0;

  for (unsigned i = 0; i < from_vec.size(); i++) {
    int count = (int)(from_vec[i] / step);
    x_pos = (double)count * step;
    if (fabs(x_pos - from_vec[i]) <= eps) {
      points[i] = true;
      n++;
    } else {
      points[i] = false;
    }
  }

  return n;
}


//
// Copy reduced points
//
void copy_reduced_xy_vec(const Rcpp::NumericVector& from_vec,
                         std::vector<double>& to_vec,
                         int start_idx,
                         std::vector<bool>& points) {
  int idx = 0;

  for (unsigned i = 0; i < from_vec.size(); i++) {
    if (points[i]) {
      to_vec[start_idx+idx] = from_vec[i];
      idx++;
    }
  }
}

//
// Comp functions
//

bool comp_asc(const std::pair<unsigned, double > &a,
              const std::pair<unsigned, double > &b) {
  return a.second < b.second;
}

bool comp_desc(const std::pair<unsigned, double > &a,
               const std::pair<unsigned, double > &b) {
  return a.second > b.second;
}

//
// Make pairs
//
void make_index_pairs(std::vector<std::pair<unsigned, double > >& indices,
                      const Rcpp::NumericVector& scores,
                      const bool& na_worst) {
  // Determin NA values
  double na_val;
  if (na_worst) {
    na_val = DBL_MIN;
  } else {
    na_val = DBL_MAX;
  }

  // Update NAs
  for (unsigned i = 0; i < scores.size(); ++i) {
    if (Rcpp::NumericVector::is_na(scores[i])) {
      indices[i] = std::make_pair(i, na_val);
    } else {
      indices[i] = std::make_pair(i, scores[i]);
    }
  }
}

//
// Sort indices by scores
//
void sort_indices(std::vector<std::pair<unsigned, double > >& indices,
                  const std::string& ties_method,
                  bool desc) {
  bool (*comp_func)(const std::pair<unsigned, double > &,
        const std::pair<unsigned, double > &);
  if (desc) {
    comp_func = &comp_desc;
  } else {
    comp_func = &comp_asc;
  }

  // Sort scores
  if (ties_method == "first") {
    std::stable_sort(indices.begin(), indices.end(), comp_func);
  } else {
    std::sort(indices.begin(), indices.end(), comp_func);
  }
}


/*
##############################################
 Name: convert_curve_df
 R file: etc_utils_dataframe.R
 R func: .dataframe_common
##############################################
*/

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

/*
##############################################
 Name: convert_curve_avg_df
 R file: etc_utils_dataframe.R
 R func: .dataframe_common
##############################################
*/

//
// Convert curve dataframe
//
// [[Rcpp::export]]
Rcpp::List convert_curve_avg_df(const Rcpp::List& obj,
                                const Rcpp::CharacterVector& uniq_modnames,
                                const Rcpp::IntegerVector& modnames,
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
  std::vector<double> vec_ymin(vec_size);
  std::vector<double> vec_ymax(vec_size);
  std::vector<int> vec_modname(vec_size);
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
      Rcpp::NumericVector y = Rcpp::as<Rcpp::NumericVector>(xys["y_avg"]);
      Rcpp::NumericVector ymi = Rcpp::as<Rcpp::NumericVector>(xys["y_ci_l"]);
      Rcpp::NumericVector yma = Rcpp::as<Rcpp::NumericVector>(xys["y_ci_h"]);

      if (reduce_points){
        vec_points.resize(x.size(), false);
        n = set_reduced_points(x, vec_points, x_bins);

        copy_reduced_xy_vec(x, vec_xs, start_idx, vec_points);
        copy_reduced_xy_vec(y, vec_ys, start_idx, vec_points);
        copy_reduced_xy_vec(ymi, vec_ymin, start_idx, vec_points);
        copy_reduced_xy_vec(yma, vec_ymax, start_idx, vec_points);
      } else {
        n = x.size();

        copy_xy_vec(x, vec_xs, start_idx);
        copy_xy_vec(y, vec_ys, start_idx);
        copy_xy_vec(ymi, vec_ymin, start_idx);
        copy_xy_vec(yma, vec_ymax, start_idx);
      }

      add_to_vec(vec_modname, n, j+1, start_idx);
      add_to_vec(vec_curvetype, n, i+1, start_idx);

      start_idx += n;
    }
  }

  if (reduce_points){
    vec_xs.resize(start_idx);
    vec_ys.resize(start_idx);
    vec_ymin.resize(start_idx);
    vec_ymax.resize(start_idx);
    vec_modname.resize(start_idx);
    vec_curvetype.resize(start_idx);
  }

  Rcpp::NumericVector xs = Rcpp::wrap(vec_xs);
  Rcpp::NumericVector ys = Rcpp::wrap(vec_ys);
  Rcpp::NumericVector ymin = Rcpp::wrap(vec_ymin);
  Rcpp::NumericVector ymax = Rcpp::wrap(vec_ymax);
  Rcpp::IntegerVector modname = Rcpp::wrap(vec_modname);
  Rcpp::IntegerVector curvetype = Rcpp::wrap(vec_curvetype);

  modname.attr("levels") = uniq_modnames;
  modname.attr("class") = "factor";
  curvetype.attr("levels") = curvetypes;
  curvetype.attr("class") = "factor";

  // Return a list
  ret_val["df"] = Rcpp::DataFrame::create(Rcpp::_["x"]= xs,
                                        Rcpp::_["y"]= ys,
                                        Rcpp::_["ymin"] = ymin,
                                        Rcpp::_["ymax"] = ymax,
                                        Rcpp::_["modname"] = modname,
                                        Rcpp::_["curvetype"] = curvetype);
  ret_val["errmsg"] = errmsg;

  return ret_val;
}




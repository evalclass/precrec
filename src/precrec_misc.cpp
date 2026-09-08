#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <vector>
#include <string>
#include <ctime>
#include <limits>       // std::numeric_limits

/*
##############################################
 Common functions
##############################################
*/

//
// Calculate vector size
//
unsigned calc_vec_size(const Rcpp::List& obj,
                       const Rcpp::List& curvetype_names) {
  unsigned vec_size = 0;

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    std::string lblctype = Rcpp::as<std::string>(curvetypes[i]);
    std::string idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    const Rcpp::List& curves = static_cast<const Rcpp::List&>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      const Rcpp::List& xys = static_cast<const Rcpp::List&>(curves[j]);
      const Rcpp::NumericVector& xs = static_cast<const Rcpp::NumericVector&>(xys["x"]);
      vec_size += xs.size();
    }
  }

  return(vec_size);
}

//
// Copy vector
//
// Templated on the destination so that the converters below can fill an
// Rcpp vector in place instead of a std::vector that is then wrapped into
// a second copy of the same data.
//
template <typename T>
void copy_xy_vec(const Rcpp::NumericVector& from_vec,
                 T& to_vec,
                 const unsigned start_idx) {
  for (unsigned i = 0; i < from_vec.size(); i++) {
    to_vec[start_idx+i] = from_vec[i];
  }
}

//
// Add to vector
//
template <typename T>
void add_to_vec(T& vec,
                const unsigned size,
                int value,
                const unsigned start_idx) {
  for (unsigned i = 0; i < size; i++) {
    vec[start_idx+i] = value;
  }
}

//
// Set reduced points
//
unsigned set_reduced_points(const Rcpp::NumericVector& from_vec,
                            std::vector<bool>& points,
                            const int x_bins) {
  double x_pos = 0.0;
  const double step = 1.0 / x_bins;
  const double eps = std::numeric_limits<double>::epsilon() * x_bins;
  unsigned n = 0;

  for (unsigned i = 0; i < from_vec.size(); i++) {
    unsigned count = (unsigned)(from_vec[i] / step);
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
// Mark evenly spaced positions, keeping at most `max_points` of them
//
// set_reduced_points() above keeps a point when its x lands on a multiple of
// 1 / x_bins. That works for the curves, whose x values come from that same
// grid, but the basic metrics are sampled at k / n for n instances, and
// unless x_bins divides n those values miss the grid lines and almost
// nothing is kept. Choosing the positions by index instead is exact for any
// n, and both ends are always kept so the axis still runs 0 to 1.
//
unsigned set_thinned_points(const unsigned size,
                            std::vector<bool>& points,
                            const int max_points) {
  points.assign(size, false);
  if (size == 0) {
    return 0;
  }

  const unsigned m = static_cast<unsigned>(max_points);
  if (max_points <= 1 || m >= size) {
    points.assign(size, true);
    return size;
  }

  const double span = static_cast<double>(size - 1);
  const double steps = static_cast<double>(m - 1);
  unsigned n = 0;
  for (unsigned i = 0; i < m; ++i) {
    // Rounded rather than truncated so the positions stay centered on the
    // ideal spacing; i == m - 1 lands exactly on size - 1.
    const unsigned pos =
      static_cast<unsigned>((static_cast<double>(i) * span) / steps + 0.5);
    if (!points[pos]) {
      points[pos] = true;
      ++n;
    }
  }

  return n;
}


//
// Copy reduced points
//
template <typename T>
void copy_reduced_xy_vec(const Rcpp::NumericVector& from_vec,
                         T& to_vec,
                         const unsigned start_idx,
                         const std::vector<bool>& points) {
  unsigned idx = 0;

  for (unsigned i = 0; i < from_vec.size(); i++) {
    if (points[i]) {
      to_vec[start_idx+idx] = from_vec[i];
      idx++;
    }
  }
}

//
// Trim a vector to its first `n` elements
//
// Returns the vector itself when it is already the right length, which is
// the case whenever the points are not reduced.
//
template <typename T>
T trim_vec(const T& vec, const unsigned n) {
  if (static_cast<unsigned>(vec.size()) == n) {
    return vec;
  }
  return T(vec.begin(), vec.begin() + n);
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
                            const int x_bins,
                            const bool thin_by_index = false) {
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
  const unsigned vec_size = calc_vec_size(obj, curvetype_names);

  // Filled in place. `vec_size` is exact unless the points are reduced, in
  // which case the tail is trimmed off once the real length is known - so
  // the common path never holds the columns twice, once in a std::vector
  // and once in the wrapped copy.
  Rcpp::NumericVector vec_xs(Rcpp::no_init(vec_size));
  Rcpp::NumericVector vec_ys(Rcpp::no_init(vec_size));
  Rcpp::IntegerVector vec_modname(Rcpp::no_init(vec_size));
  Rcpp::IntegerVector vec_dsid(Rcpp::no_init(vec_size));
  Rcpp::IntegerVector vec_dsid_modname(Rcpp::no_init(vec_size));
  Rcpp::IntegerVector vec_curvetype(Rcpp::no_init(vec_size));
  std::vector<bool> vec_points(vec_size);

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  unsigned start_idx = 0;
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    lblctype = Rcpp::as<std::string>(curvetypes[i]);
    idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    const Rcpp::List& curves = static_cast<const Rcpp::List&>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      unsigned n = 0;
      const Rcpp::List& xys = static_cast<const Rcpp::List&>(curves[j]);
      const Rcpp::NumericVector& x = static_cast<const Rcpp::NumericVector&>(xys["x"]);
      const Rcpp::NumericVector& y = static_cast<const Rcpp::NumericVector&>(xys["y"]);

      if (reduce_points){
        if (thin_by_index) {
          n = set_thinned_points(x.size(), vec_points, x_bins);
        } else {
          vec_points.resize(x.size(), false);
          n = set_reduced_points(x, vec_points, x_bins);
        }

        copy_reduced_xy_vec(x, vec_xs, start_idx, vec_points);
        copy_reduced_xy_vec(y, vec_ys, start_idx, vec_points);
      } else {
        n = x.size();
        copy_xy_vec(x, vec_xs, start_idx);
        copy_xy_vec(y, vec_ys, start_idx);
      }

      add_to_vec(vec_modname, n, modnames[j], start_idx);
      add_to_vec(vec_dsid, n, dsids[j], start_idx);
      add_to_vec(vec_dsid_modname, n, static_cast<int>(j+1), start_idx);
      add_to_vec(vec_curvetype, n, static_cast<int>(i+1), start_idx);

      start_idx += n;
    }
  }

  Rcpp::NumericVector xs = trim_vec(vec_xs, start_idx);
  Rcpp::NumericVector ys = trim_vec(vec_ys, start_idx);
  Rcpp::IntegerVector modname = trim_vec(vec_modname, start_idx);
  Rcpp::IntegerVector dsid = trim_vec(vec_dsid, start_idx);
  Rcpp::IntegerVector dsid_modname = trim_vec(vec_dsid_modname, start_idx);
  Rcpp::IntegerVector curvetype = trim_vec(vec_curvetype, start_idx);

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
                                const int x_bins,
                                const bool thin_by_index = false) {
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
  const unsigned vec_size = calc_vec_size(obj, curvetype_names);

  // Filled in place - see convert_curve_df above
  Rcpp::NumericVector vec_xs(Rcpp::no_init(vec_size));
  Rcpp::NumericVector vec_ys(Rcpp::no_init(vec_size));
  Rcpp::NumericVector vec_ymin(Rcpp::no_init(vec_size));
  Rcpp::NumericVector vec_ymax(Rcpp::no_init(vec_size));
  Rcpp::IntegerVector vec_modname(Rcpp::no_init(vec_size));
  Rcpp::IntegerVector vec_curvetype(Rcpp::no_init(vec_size));
  std::vector<bool> vec_points(vec_size);

  Rcpp::CharacterVector curvetypes = curvetype_names.names();
  unsigned start_idx = 0;
  for (unsigned i = 0; i < curvetypes.size(); i++) {
    lblctype = Rcpp::as<std::string>(curvetypes[i]);
    idxctype = Rcpp::as<std::string>(curvetype_names[lblctype]);
    const Rcpp::List& curves = static_cast<const Rcpp::List&>(obj[idxctype]);

    for (unsigned j = 0; j < curves.size(); j++) {
      unsigned n = 0;
      const Rcpp::List& xys = static_cast<const Rcpp::List&>(curves[j]);
      const Rcpp::NumericVector& x = static_cast<const Rcpp::NumericVector&>(xys["x"]);
      const Rcpp::NumericVector& y = static_cast<const Rcpp::NumericVector&>(xys["y_avg"]);
      const Rcpp::NumericVector& ymi = static_cast<const Rcpp::NumericVector&>(xys["y_ci_l"]);
      const Rcpp::NumericVector& yma = static_cast<const Rcpp::NumericVector&>(xys["y_ci_h"]);

      if (reduce_points){
        if (thin_by_index) {
          n = set_thinned_points(x.size(), vec_points, x_bins);
        } else {
          vec_points.resize(x.size(), false);
          n = set_reduced_points(x, vec_points, x_bins);
        }

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

      add_to_vec(vec_modname, n, static_cast<int>(j+1), start_idx);
      add_to_vec(vec_curvetype, n, static_cast<int>(i+1), start_idx);

      start_idx += n;
    }
  }

  Rcpp::NumericVector xs = trim_vec(vec_xs, start_idx);
  Rcpp::NumericVector ys = trim_vec(vec_ys, start_idx);
  Rcpp::NumericVector ymin = trim_vec(vec_ymin, start_idx);
  Rcpp::NumericVector ymax = trim_vec(vec_ymax, start_idx);
  Rcpp::IntegerVector modname = trim_vec(vec_modname, start_idx);
  Rcpp::IntegerVector curvetype = trim_vec(vec_curvetype, start_idx);

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

//
// Shuffle int vector
//
void shuffle_intvec(std::vector<int>::iterator first,
                    std::vector<int>::iterator last,
                    int (*gen)(const int)) {
  std::iterator_traits<std::vector<int>::iterator >::difference_type i, n;
  n = (last - first);
  for (i = n - 1; i > 0; --i) {
    std::swap(first[i], first[gen(i + 1)]);
  }
}

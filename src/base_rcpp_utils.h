// base_rcpp_utils.h

#include <Rcpp.h>

inline void stop_if(int err, const char* msg) {
  if (err != 0) Rcpp::stop(msg);
}

inline void copy_posixct_attrs(Rcpp::NumericVector &target, const Rcpp::NumericVector &ref) {
  SEXP cls = ref.attr("class");
  if (!Rf_isNull(cls)) target.attr("class") = cls;
  SEXP tz = ref.attr("tzone");
  if (!Rf_isNull(tz)) target.attr("tzone") = tz;
}
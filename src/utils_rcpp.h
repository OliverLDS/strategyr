#ifndef UTILSRCPP_H
#define UTILSRCPP_H

// carry POSIXct class/tzone from a reference vector
inline void copy_posixct_attrs(Rcpp::NumericVector &target, const Rcpp::NumericVector &ref) {
  SEXP cls = ref.attr("class");
  if (!Rf_isNull(cls)) target.attr("class") = cls;
  SEXP tz = ref.attr("tzone");
  if (!Rf_isNull(tz)) target.attr("tzone") = tz;
}

#endif
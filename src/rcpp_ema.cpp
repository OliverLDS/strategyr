#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// headers
#include "base_utils.h"
#include "base_rcpp_utils.h"
#include "eng_ema.h"
#include "ker_ema.h"


// [[Rcpp::export]]
Rcpp::NumericVector ema_ttr_fixed_step(const Rcpp::NumericVector& x,
                                          int n,
                                          bool wilder = false) {
  if (n < 0) Rcpp::stop("n must be >= 0");
  const size_t len = (size_t)x.size();
  Rcpp::NumericVector out(len);

  const int err = ema_ttr_fixed_step_kernel(REAL(out), REAL(x), len, (size_t)n, wilder);
  stop_if(err, "ema_ttr_fixed_step_kernel failed (check n and len).");

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector ema_fixed_step_tau(const Rcpp::NumericVector& x, double tau) {
  const size_t len = (size_t)x.size();
  Rcpp::NumericVector out(len);

  const int err = ema_fixed_step_kernel(REAL(out), REAL(x), len, tau);
  stop_if(err, "ema_fixed_step_kernel failed (check tau > 0 and len > 0).");

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector ema_tau_irregular(const Rcpp::NumericVector& x,
                                         const Rcpp::IntegerVector& t,
                                         double tau) {
  const size_t len = (size_t)x.size();
  if ((size_t)t.size() != len) Rcpp::stop("t must have the same length as x.");
  if (len == 0) return Rcpp::NumericVector(0);

  // Convert int time to size_t time (safe for non-negative)
  std::vector<size_t> tt(len);
  for (size_t i = 0; i < len; ++i) {
    int ti = t[(int)i];
    if (ti < 0) Rcpp::stop("t must be non-negative.");
    tt[i] = (size_t)ti;
  }

  Rcpp::NumericVector out(len);
  const int err = ema_tau_irregular_kernel(REAL(out), REAL(x), tt.data(), len, tau);
  stop_if(err, "ema_tau_irregular_kernel failed (check tau>0 and strictly increasing t).");

  return out;
}

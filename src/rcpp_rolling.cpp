#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// headers
#include "base_utils.h"
#include "base_rcpp_utils.h"
#include "eng_rolling.h"
#include "ker_rolling.h"


// [[Rcpp::export]]
Rcpp::NumericVector rolling_mean(const Rcpp::NumericVector& x, int n) {
  if (n < 0) Rcpp::stop("n must be >= 0");
  const size_t len = (size_t)x.size();
  Rcpp::NumericVector out(len);

  const int err = rolling_1st_moment_kernel(REAL(out), REAL(x), len, (size_t)n);
  stop_if(err, "rolling_1st_moment_kernel failed (check n and len).");

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector rolling_sd(const Rcpp::NumericVector& x, int n, bool sample = false) {
  if (n < 0) Rcpp::stop("n must be >= 0");
  const size_t len = (size_t)x.size();
  Rcpp::NumericVector out(len);

  const int err = rolling_2nd_moment_kernel(REAL(out), REAL(x), len, (size_t)n, sample);
  stop_if(err, "rolling_2nd_moment_kernel failed (check n/len; if sample=TRUE then n>=2).");

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix rolling_quantiles(const Rcpp::NumericVector& x,
                                         int n,
                                         const Rcpp::NumericVector& probs) {
  if (n < 0) Rcpp::stop("n must be >= 0");
  const size_t len = (size_t)x.size();
  const size_t P   = (size_t)probs.size();

  Rcpp::NumericMatrix out((int)len, (int)P); // column-major, matches your kernel layout

  rolling_quantiles_kernel(REAL(out), REAL(x), len, (size_t)n, REAL(probs), P);

  // kernel "returns void" and simply leaves NA if invalid; we keep that behavior
  return out;
}
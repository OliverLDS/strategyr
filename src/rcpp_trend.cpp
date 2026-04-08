#include <Rcpp.h>
#include <algorithm>
#include <cmath>

#include "base_utils.h"

template <bool is_max>
Rcpp::NumericVector aroon_pct_impl(const Rcpp::NumericVector& x, int n) {
  if (n < 0) Rcpp::stop("n must be >= 0");
  const int len = x.size();
  Rcpp::NumericVector out(len, STRATEGYR::kNaReal);
  if (len == 0) return out;

  double extreme = x[0];
  int last_ext_idx = 0;

  for (int i = 0; i < len; ++i) {
    const double xi = x[i];
    if (i == 0) {
      extreme = xi;
      last_ext_idx = 0;
    } else if (!is_na(xi)) {
      const bool is_new_extreme = is_max ? (xi > extreme) : (xi < extreme);
      if (is_new_extreme) {
        extreme = xi;
        last_ext_idx = i;
      }
    }

    if (i >= n - 1) {
      const int age = i - last_ext_idx;
      out[i] = (age < n) ? (100.0 * static_cast<double>(n - age) / static_cast<double>(n)) : 0.0;
    }
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector aroon_up_pct_cpp(const Rcpp::NumericVector& high, int n) {
  return aroon_pct_impl<true>(high, n);
}

// [[Rcpp::export]]
Rcpp::NumericVector aroon_dn_pct_cpp(const Rcpp::NumericVector& low, int n) {
  return aroon_pct_impl<false>(low, n);
}

// [[Rcpp::export]]
Rcpp::NumericVector sar_cpp(const Rcpp::NumericVector& high,
                            const Rcpp::NumericVector& low,
                            const Rcpp::NumericVector& accel) {
  const int len = high.size();
  if (len != low.size()) Rcpp::stop("high and low must have the same length");
  if (accel.size() != 2) Rcpp::stop("accel must have length 2: c(step, max)");
  if (len == 0) return Rcpp::NumericVector();

  const double step = accel[0];
  const double max_accel = accel[1];
  if (step <= 0.0 || max_accel <= 0.0) Rcpp::stop("accel values must be > 0");

  Rcpp::NumericVector out(len, STRATEGYR::kNaReal);
  out[0] = low[0];
  if (len == 1) return out;

  bool is_long = true;
  if (!is_na(high[1]) && !is_na(low[1]) && !is_na(high[0]) && !is_na(low[0])) {
    const double dH = high[1] - high[0];
    const double dL = low[0] - low[1];
    if (!(dH >= dL)) is_long = false;
  }

  double af = is_long ? (2.0 * step) : step;
  double ep = is_long ? high[1] : low[1];
  out[1] = is_long ? low[0] : high[0];

  for (int i = 2; i < len; ++i) {
    double sar = out[i - 1] + af * (ep - out[i - 1]);

    if (is_long) {
      sar = std::min(sar, low[i - 1]);
      if (i > 2) {
        sar = std::min(sar, low[i - 2]);
      }

      if (low[i] < sar) {
        is_long = false;
        sar = ep;
        ep = low[i];
        af = step;
      } else {
        if (high[i] > ep) {
          ep = high[i];
          af = std::min(af + step, max_accel);
        }
      }
    } else {
      sar = std::max(sar, high[i - 1]);
      sar = std::max(sar, high[i - 2]);

      if (high[i] > sar) {
        is_long = true;
        sar = ep;
        ep = high[i];
        af = step;
      } else {
        if (low[i] < ep) {
          ep = low[i];
          af = std::min(af + step, max_accel);
        }
      }
    }

    out[i] = sar;
  }

  return out;
}

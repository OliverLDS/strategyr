#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector rolling_sum(const Rcpp::NumericVector &x, int n) {
  int len = x.size();
  Rcpp::NumericVector out(len, NA_REAL);

  if (n > len || n <= 0) return out;

  double sum = 0.0;

  for (int i = 0; i < len; ++i) {
		double xi = x[i];
    sum += xi;
    if (i >= n) {
      sum -= x[i - n];
    }
    if (i >= n - 1) {
      out[i] = sum;
    }
  }

  return out;
}

/**
 * @title Rolling mean (simple moving average)
 * @description
 * Compute a rolling arithmetic mean over a fixed-size window.
 *
 * @param x Numeric vector of input values.
 * @param n Window length (number of observations per average). Must be > 0
 *   and not larger than the length of `x`.
 *
 * @return A numeric vector of the same length as `x`. Positions with an
 *   incomplete window (the first `n - 1` elements, and all positions if
 *   `n > length(x)` or `n <= 0`) are filled with `NA_real_`.
 *
 * This function is intended as a low-level utility for higher-level R
 * wrappers such as `calc_ind_SMA()`.
 */
// [[Rcpp::export]]
Rcpp::NumericVector rolling_mean(const Rcpp::NumericVector &x, int n) {
  int len = x.size();
  Rcpp::NumericVector out(len, NA_REAL);

  if (n > len || n <= 0) return out;

  double sum = 0.0;
	double inv_n = 1.0 / n; // avoid division

  for (int i = 0; i < len; ++i) {
		double xi = x[i];
    sum += xi;
    if (i >= n) {
      sum -= x[i - n];
    }
    if (i >= n - 1) {
      out[i] = sum * inv_n;
    }
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector rolling_sd(const Rcpp::NumericVector &x, int n, bool sample = true) {
  int len = x.size();
  Rcpp::NumericVector out(len, NA_REAL);

  if (n > len || n < 2) return out;

  double sum = 0.0;  // Σ x
	double sumsq = 0.0;  // Σ x^2
	double inv_n = 1.0 / n;
	double inv_denom = sample ? 1.0 / (n - 1) : inv_n;

  for (int i = 0; i < len; ++i) {
		double xi = x[i];
    sum += xi;
		sumsq += xi * xi;
    if (i >= n) {
			double xo = x[i - n];
      sum -= xo;
      sumsq -= xo * xo;
    }
    if (i >= n - 1) {
			double mean = sum * inv_n;
      double var  = (sumsq - sum * mean) * inv_denom;
      if (var < 0.0) var = 0.0;  // guard tiny negative due to rounding
      out[i] = std::sqrt(var);
    }
  }

  return out;
}
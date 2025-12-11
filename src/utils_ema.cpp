#include <Rcpp.h>
#include <cmath>
#include "utils_ema.h"

/**
 * @title EMA with TTR-style smoothing (fixed step)
 * @description
 * Compute an exponential moving average (EMA) using the TTR convention
 * for the smoothing parameter \code{alpha}, with an option to use
 * Wilder smoothing.
 *
 * @param x Numeric vector of input values.
 * @param n Window length (number of periods). Must be > 0 and
 *   \code{n <= length(x)}.
 * @param wilder Logical flag. If \code{TRUE}, use Wilder-style smoothing
 *   \code{alpha = 1 / n}. If \code{FALSE}, use standard EMA smoothing
 *   \code{alpha = 2 / (n + 1)} (TTR convention).
 *
 * @return A numeric vector of the same length as \code{x}. Elements before
 *   the first complete EMA (positions \code{0:(n-2)}) are \code{NA_real_}.
 *
 * The initial EMA at index \code{n - 1} is set to the simple average of
 * the first \code{n} values of \code{x}, and subsequent values follow the
 * usual recursive EMA update.
 */
// [[Rcpp::export]]
Rcpp::NumericVector ema_ttr_fixed_step(const Rcpp::NumericVector &x, int n, bool wilder) { 
  const int len = x.size();
  Rcpp::NumericVector ema(len, NA_REAL);
  if (len == 0 || n <= 0 || len < n) return ema;
	
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    sum += x[i];
  }

	const double alpha = wilder ? 1.0 / n : 2.0 / (n+1); // Wilder smoothing or Standard EMA
	
	double prev = sum / n;
	ema[n - 1] = sum / n;

  for (int i = n; i < len; ++i) {
		const double xi = x[i];
		prev = prev + alpha * (xi - prev);
		ema[i] = prev;
  }

	return ema;
}

/**
 * @title EMA with time constant tau (fixed step)
 * @description
 * Compute an exponential moving average (EMA) given a continuous-time
 * time constant \code{tau}, assuming a fixed time step of 1 between
 * observations.
 *
 * @details
 * The smoothing parameter is
 * \deqn{ \alpha = 1 - \exp(-1 / \tau). }
 * If the user wants to parameterise via half-life \code{h}, they may set
 * \code{tau = h / log(2)} so that the weight decays by 50\% over \code{h}
 * steps.
 *
 * @param x Numeric vector of input values.
 * @param tau Positive time constant controlling the rate of decay.
 *
 * @return A numeric vector of the same length as \code{x}. The first
 *   value \code{ema[0]} is set to \code{x[0]}. If \code{len == 0} or
 *   \code{tau <= 0}, a vector of \code{NA_real_} is returned.
 */
// [[Rcpp::export]]
Rcpp::NumericVector ema_tau_fixed_step(const Rcpp::NumericVector &x, double tau) { 
  const int len = x.size();
  Rcpp::NumericVector ema(len, NA_REAL);
	
	if (len == 0 || tau <= 0.0) return ema;
	
	const double alpha = 1.0 - std::exp(-1.0 / tau); // For half-life h and step size dt=1, user can pass tau = h / std::log(2.0)
	
	double prev = x[0];
  ema[0] = x[0];

  for (int i = 1; i < len; ++i) {
		const double xi = x[i];
		prev = prev + alpha * (xi - prev);
		ema[i] = prev;
  }

	return ema;
}

/**
 * @title EMA with time constant tau (irregular time grid)
 * @description
 * Compute an exponential moving average (EMA) when observations arrive
 * at irregular times. The recursion uses a time-varying smoothing
 * parameter based on the elapsed time between samples.
 *
 * @details
 * Let \code{t[i]} be the time of observation \code{x[i]}, with
 * \code{dt = t[i] - t[i - 1]} and \code{tau > 0}. Then
 * \deqn{ \alpha_i = 1 - \exp(-dt / \tau), }
 * and the EMA is updated as
 * \deqn{ ema[i] = ema[i - 1] + \alpha_i (x[i] - ema[i - 1]). }
 *
 * This requires that \code{t} is strictly increasing; otherwise an
 * error is thrown.
 *
 * @param t Numeric vector of time stamps (strictly increasing).
 * @param x Numeric vector of values, same length as \code{t}.
 * @param tau Positive time constant controlling decay.
 *
 * @return A numeric vector of the same length as \code{x}. The first
 *   element \code{ema[0]} is set to \code{x[0]}. If \code{len == 0}
 *   or \code{tau <= 0}, a vector of \code{NA_real_} is returned.
 *
 * @throws std::runtime_error if \code{t} and \code{x} have different
 *   lengths, or if any \code{t[i] - t[i - 1] <= 0}.
 */
// [[Rcpp::export]]
Rcpp::NumericVector ema_tau_irregular(const Rcpp::NumericVector &t, const Rcpp::NumericVector &x, double tau) { 
  const int len = x.size();
  Rcpp::NumericVector ema(len, NA_REAL);
	
	if (len == 0 || tau <= 0) return ema;
	if (t.size() != len) Rcpp::stop("t and x must have the same length");
	
  ema[0] = x[0];

  for (int i = 1; i < len; ++i) {
		const double dt = t[i] - t[i - 1];
		if (dt <= 0) Rcpp::stop("d_t should be positive.");
		const double alpha = 1.0 - std::exp(-dt/tau);
    ema[i] = ema[i - 1] + alpha * (x[i] - ema[i - 1]);
  }

	return ema;
}


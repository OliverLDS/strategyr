#' Validate Option Type
#'
#' Internal helper that normalizes option type.
#'
#' @param type Character option type.
#'
#' @return Normalized option type.
#' @noRd
.validate_option_type <- function(type) {
  match.arg(type, c("call", "put"))
}

#' Validate Option Inputs
#'
#' Internal helper that validates Black-Scholes inputs.
#'
#' @param S Numeric spot price.
#' @param K Numeric strike price.
#' @param T Numeric time to expiry in years.
#' @param r Numeric annualized risk-free rate.
#' @param sigma Numeric annualized volatility.
#' @param q Numeric annualized dividend yield.
#'
#' @return List of normalized numeric inputs.
#' @noRd
.validate_option_inputs <- function(S, K, T, r, sigma, q = 0) {
  stopifnot(is.numeric(S), length(S) == 1, is.finite(S), S > 0)
  stopifnot(is.numeric(K), length(K) == 1, is.finite(K), K > 0)
  stopifnot(is.numeric(T), length(T) == 1, is.finite(T), T > 0)
  stopifnot(is.numeric(r), length(r) == 1, is.finite(r))
  stopifnot(is.numeric(sigma), length(sigma) == 1, is.finite(sigma), sigma > 0)
  stopifnot(is.numeric(q), length(q) == 1, is.finite(q))

  list(
    S = as.numeric(S),
    K = as.numeric(K),
    T = as.numeric(T),
    r = as.numeric(r),
    sigma = as.numeric(sigma),
    q = as.numeric(q)
  )
}

#' Black-Scholes d1
#'
#' Internal Black-Scholes helper.
#'
#' @inheritParams calc_option_delta
#'
#' @return Numeric scalar `d1`.
#' @noRd
.bs_d1 <- function(S, K, T, r, sigma, q = 0) {
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  (log(x$S / x$K) + (x$r - x$q + 0.5 * x$sigma^2) * x$T) / (x$sigma * sqrt(x$T))
}

#' Black-Scholes d2
#'
#' Internal Black-Scholes helper.
#'
#' @inheritParams calc_option_delta
#'
#' @return Numeric scalar `d2`.
#' @noRd
.bs_d2 <- function(S, K, T, r, sigma, q = 0) {
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  .bs_d1(x$S, x$K, x$T, x$r, x$sigma, q = x$q) - x$sigma * sqrt(x$T)
}

#' Compute Black-Scholes Option Price
#'
#' Internal helper that computes Black-Scholes price for European calls and
#' puts with continuous dividend yield.
#'
#' @inheritParams calc_option_delta
#'
#' @return Numeric scalar price.
#' @noRd
.calc_option_price_bs <- function(S, K, T, r, sigma, type, q = 0) {
  type <- .validate_option_type(type)
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  d1 <- .bs_d1(x$S, x$K, x$T, x$r, x$sigma, q = x$q)
  d2 <- d1 - x$sigma * sqrt(x$T)
  df_q <- exp(-x$q * x$T)
  df_r <- exp(-x$r * x$T)

  if (type == "call") {
    x$S * df_q * stats::pnorm(d1) - x$K * df_r * stats::pnorm(d2)
  } else {
    x$K * df_r * stats::pnorm(-d2) - x$S * df_q * stats::pnorm(-d1)
  }
}

#' Compute Option Delta
#'
#' Computes Black-Scholes delta for a European call or put with continuous
#' dividend yield.
#'
#' @param S Numeric spot price.
#' @param K Numeric strike price.
#' @param T Numeric time to expiry in years.
#' @param r Numeric annualized risk-free rate.
#' @param sigma Numeric annualized volatility.
#' @param type Character scalar, either `"call"` or `"put"`.
#' @param q Numeric annualized continuous dividend yield.
#'
#' @return Numeric scalar delta.
#' @export
calc_option_delta <- function(S, K, T, r, sigma, type = c("call", "put"), q = 0) {
  type <- .validate_option_type(type)
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  d1 <- .bs_d1(x$S, x$K, x$T, x$r, x$sigma, q = x$q)
  df_q <- exp(-x$q * x$T)

  if (type == "call") {
    df_q * stats::pnorm(d1)
  } else {
    df_q * (stats::pnorm(d1) - 1)
  }
}

#' Compute Option Gamma
#'
#' Computes Black-Scholes gamma for a European option with continuous dividend
#' yield.
#'
#' @inheritParams calc_option_delta
#'
#' @return Numeric scalar gamma.
#' @export
calc_option_gamma <- function(S, K, T, r, sigma, q = 0) {
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  d1 <- .bs_d1(x$S, x$K, x$T, x$r, x$sigma, q = x$q)
  exp(-x$q * x$T) * stats::dnorm(d1) / (x$S * x$sigma * sqrt(x$T))
}

#' Compute Option Vega
#'
#' Computes Black-Scholes vega for a European option with continuous dividend
#' yield.
#'
#' @inheritParams calc_option_delta
#'
#' @return Numeric scalar vega per 1.00 volatility change.
#' @export
calc_option_vega <- function(S, K, T, r, sigma, q = 0) {
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  d1 <- .bs_d1(x$S, x$K, x$T, x$r, x$sigma, q = x$q)
  x$S * exp(-x$q * x$T) * stats::dnorm(d1) * sqrt(x$T)
}

#' Compute Option Theta
#'
#' Computes Black-Scholes theta for a European call or put with continuous
#' dividend yield.
#'
#' @inheritParams calc_option_delta
#' @param scale Character scalar, either `"annual"` or `"daily"`.
#'
#' @return Numeric scalar theta.
#' @export
calc_option_theta <- function(S, K, T, r, sigma, type = c("call", "put"), q = 0, scale = c("annual", "daily")) {
  type <- .validate_option_type(type)
  scale <- match.arg(scale)
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  d1 <- .bs_d1(x$S, x$K, x$T, x$r, x$sigma, q = x$q)
  d2 <- d1 - x$sigma * sqrt(x$T)
  df_q <- exp(-x$q * x$T)
  df_r <- exp(-x$r * x$T)

  common <- -(x$S * df_q * stats::dnorm(d1) * x$sigma) / (2 * sqrt(x$T))
  theta <- if (type == "call") {
    common - x$r * x$K * df_r * stats::pnorm(d2) + x$q * x$S * df_q * stats::pnorm(d1)
  } else {
    common + x$r * x$K * df_r * stats::pnorm(-d2) - x$q * x$S * df_q * stats::pnorm(-d1)
  }

  if (scale == "daily") theta / 365 else theta
}

#' Compute Option Rho
#'
#' Computes Black-Scholes rho for a European call or put with continuous
#' dividend yield.
#'
#' @inheritParams calc_option_delta
#'
#' @return Numeric scalar rho per 1.00 rate change.
#' @export
calc_option_rho <- function(S, K, T, r, sigma, type = c("call", "put"), q = 0) {
  type <- .validate_option_type(type)
  x <- .validate_option_inputs(S, K, T, r, sigma, q = q)
  d2 <- .bs_d2(x$S, x$K, x$T, x$r, x$sigma, q = x$q)
  df_r <- exp(-x$r * x$T)

  if (type == "call") {
    x$K * x$T * df_r * stats::pnorm(d2)
  } else {
    -x$K * x$T * df_r * stats::pnorm(-d2)
  }
}

#' Compute Option Greeks
#'
#' Computes Black-Scholes price and Greeks for a European call or put with
#' continuous dividend yield.
#'
#' @inheritParams calc_option_theta
#'
#' @return A one-row `data.table` with `price`, `delta`, `gamma`, `vega`,
#'   `theta`, and `rho`.
#' @export
calc_option_greeks <- function(S, K, T, r, sigma, type = c("call", "put"), q = 0, theta_scale = c("annual", "daily")) {
  type <- .validate_option_type(type)
  theta_scale <- match.arg(theta_scale)

  data.table::data.table(
    price = .calc_option_price_bs(S, K, T, r, sigma, type = type, q = q),
    delta = calc_option_delta(S, K, T, r, sigma, type = type, q = q),
    gamma = calc_option_gamma(S, K, T, r, sigma, q = q),
    vega = calc_option_vega(S, K, T, r, sigma, q = q),
    theta = calc_option_theta(S, K, T, r, sigma, type = type, q = q, scale = theta_scale),
    rho = calc_option_rho(S, K, T, r, sigma, type = type, q = q)
  )
}

#' Compute Option Implied Volatility
#'
#' Solves Black-Scholes implied volatility from an observed European option
#' price with continuous dividend yield.
#'
#' @param price Numeric observed option price.
#' @param S Numeric spot price.
#' @param K Numeric strike price.
#' @param T Numeric time to expiry in years.
#' @param r Numeric annualized risk-free rate.
#' @param type Character scalar, either `"call"` or `"put"`.
#' @param q Numeric annualized continuous dividend yield.
#' @param interval Numeric vector of length two giving the volatility search
#'   bracket.
#'
#' @return Numeric scalar implied volatility.
#' @export
calc_option_iv <- function(price, S, K, T, r, type = c("call", "put"), q = 0, interval = c(1e-6, 5)) {
  type <- .validate_option_type(type)
  stopifnot(is.numeric(price), length(price) == 1, is.finite(price), price > 0)
  stopifnot(is.numeric(interval), length(interval) == 2, interval[1] > 0, interval[1] < interval[2])

  f <- function(sigma) {
    .calc_option_price_bs(S, K, T, r, sigma, type = type, q = q) - price
  }

  stats::uniroot(f, interval = interval, tol = .Machine$double.eps^0.5)$root
}

#' Validate Fixed-Income Cashflow Inputs
#'
#' Internal helper that normalizes cash flow and rate vectors.
#'
#' @param cashflows Numeric vector of period cash flows.
#' @param rates Numeric scalar or vector of per-period discount rates.
#'
#' @return A list containing normalized `cashflows`, `rates`, and `period`.
#' @noRd
.validate_fi_cashflows <- function(cashflows, rates) {
  stopifnot(is.numeric(cashflows), length(cashflows) >= 1)
  stopifnot(is.numeric(rates), length(rates) >= 1)

  n <- length(cashflows)
  if (length(rates) == 1) {
    rates <- rep(rates, n)
  }
  stopifnot(length(rates) == n)

  list(
    cashflows = as.numeric(cashflows),
    rates = as.numeric(rates),
    period = seq_len(n)
  )
}

.validate_accrual_frac <- function(accrual_frac) {
  stopifnot(is.numeric(accrual_frac), length(accrual_frac) == 1)
  stopifnot(accrual_frac >= 0, accrual_frac < 1)
  as.numeric(accrual_frac)
}

.validate_curve_inputs <- function(tenor, zero_rate) {
  stopifnot(is.numeric(tenor), length(tenor) >= 2)
  stopifnot(is.numeric(zero_rate), length(zero_rate) == length(tenor))
  stopifnot(all(is.finite(tenor)), all(is.finite(zero_rate)))
  stopifnot(all(diff(tenor) > 0))

  list(
    tenor = as.numeric(tenor),
    zero_rate = as.numeric(zero_rate)
  )
}

.interp_curve_rate <- function(target_tenor, tenor, zero_rate, method = "linear") {
  curve <- .validate_curve_inputs(tenor, zero_rate)
  stopifnot(is.numeric(target_tenor), length(target_tenor) >= 1)
  method <- match.arg(method, c("linear"))
  stopifnot(all(target_tenor >= min(curve$tenor)), all(target_tenor <= max(curve$tenor)))

  stats::approx(
    x = curve$tenor,
    y = curve$zero_rate,
    xout = target_tenor,
    method = method,
    ties = "ordered"
  )$y
}

.shock_curve_tenor <- function(tenor, zero_rate, shock_tenor = NULL, shock_bp) {
  curve <- .validate_curve_inputs(tenor, zero_rate)
  stopifnot(is.numeric(shock_bp), length(shock_bp) == 1)

  zero_rate_shocked <- curve$zero_rate
  bump <- shock_bp / 10000

  if (is.null(shock_tenor)) {
    zero_rate_shocked <- zero_rate_shocked + bump
  } else {
    stopifnot(is.numeric(shock_tenor), length(shock_tenor) == 1)
    idx <- which(abs(curve$tenor - shock_tenor) < .Machine$double.eps^0.5)
    stopifnot(length(idx) == 1)
    zero_rate_shocked[idx] <- zero_rate_shocked[idx] + bump
  }

  data.table::data.table(
    tenor = curve$tenor,
    zero_rate = curve$zero_rate,
    zero_rate_shocked = zero_rate_shocked
  )
}

.bond_discount_periods <- function(n_periods, accrual_frac = 0) {
  seq_len(n_periods) - accrual_frac
}

#' Compute Per-Period Yield Rates
#'
#' Converts annualized yield-to-maturity values into per-period rates.
#'
#' @param ytm Numeric scalar or vector of annualized yields.
#' @param freq Integer compounding frequency per year.
#'
#' @return Numeric vector of per-period rates.
#' @export
calc_period_rate <- function(ytm, freq = 2) {
  stopifnot(is.numeric(ytm), is.numeric(freq), length(freq) == 1, freq > 0)
  as.numeric(ytm) / freq
}

#' Compute Bond Cash Flows
#'
#' Generates level-coupon bond cash flows by period.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric maturity in years.
#' @param freq Integer coupon frequency per year.
#'
#' @return Numeric vector of period cash flows excluding time-zero.
#' @export
calc_bond_cashflows <- function(par = 1, c_rate, T, freq = 2) {
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(T), length(T) == 1, T > 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)

  n_periods <- as.integer(round(T * freq))
  stopifnot(n_periods >= 1)

  coupon <- par * c_rate / freq
  cashflows <- rep(coupon, n_periods)
  cashflows[n_periods] <- cashflows[n_periods] + par
  cashflows
}

.bond_dirty_price_from_ytm <- function(par, c_rate, T, freq, ytm, accrual_frac = 0) {
  accrual_frac <- .validate_accrual_frac(accrual_frac)
  cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
  rates <- calc_period_rate(ytm, freq = freq)
  periods <- .bond_discount_periods(length(cashflows), accrual_frac = accrual_frac)
  sum(cashflows / (1 + rates)^periods)
}

.bond_dirty_price_from_curve <- function(cashflows, rates, accrual_frac = 0) {
  accrual_frac <- .validate_accrual_frac(accrual_frac)
  x <- .validate_fi_cashflows(cashflows, rates)
  periods <- .bond_discount_periods(length(x$cashflows), accrual_frac = accrual_frac)
  sum(x$cashflows / (1 + x$rates)^periods)
}

.bond_dirty_price_from_curve_spread <- function(cashflows, rates, spread, freq = 2, accrual_frac = 0) {
  stopifnot(is.numeric(spread), length(spread) == 1)
  .bond_dirty_price_from_curve(
    cashflows = cashflows,
    rates = rates + spread / freq,
    accrual_frac = accrual_frac
  )
}

#' Compute Bond Net Present Value
#'
#' Computes bond present value from explicit cash flows and per-period rates, or
#' directly from bond terms.
#'
#' @param cashflows Optional numeric vector of period cash flows.
#' @param rates Optional numeric scalar or vector of per-period discount rates.
#' @param par Optional numeric face value for term-based input.
#' @param c_rate Optional annual coupon rate for term-based input.
#' @param T Optional maturity in years for term-based input.
#' @param freq Integer compounding frequency per year for term-based input.
#' @param ytm Optional annualized yield-to-maturity for term-based input.
#'
#' @return Numeric scalar present value.
#' @export
calc_bond_npv <- function(cashflows = NULL, rates = NULL, par = NULL, c_rate = NULL, T = NULL, freq = 2, ytm = NULL) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- calc_period_rate(ytm, freq = freq)
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  sum(x$cashflows / (1 + x$rates)^x$period)
}

#' Compute Bond Macaulay Duration
#'
#' Computes Macaulay duration in years from explicit cash flows and rates, or
#' directly from bond terms.
#'
#' @inheritParams calc_bond_npv
#'
#' @return Numeric scalar duration in years.
#' @export
calc_bond_duration <- function(cashflows = NULL, rates = NULL, par = NULL, c_rate = NULL, T = NULL, freq = 2, ytm = NULL) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- calc_period_rate(ytm, freq = freq)
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  pv <- x$cashflows / (1 + x$rates)^x$period
  sum(x$period * pv) / sum(pv) / freq
}

#' Compute Bond Modified Duration
#'
#' Computes modified duration in years from explicit cash flows and rates, or
#' directly from bond terms.
#'
#' @inheritParams calc_bond_npv
#'
#' @return Numeric scalar modified duration in years.
#' @export
calc_bond_mduration <- function(cashflows = NULL, rates = NULL, par = NULL, c_rate = NULL, T = NULL, freq = 2, ytm = NULL) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- calc_period_rate(ytm, freq = freq)
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  pv <- x$cashflows / (1 + x$rates)^x$period
  sum(x$period * pv / (1 + x$rates)) / sum(pv) / freq
}

#' Compute Bond Convexity
#'
#' Computes bond convexity in year-squared units from explicit cash flows and
#' rates, or directly from bond terms.
#'
#' @inheritParams calc_bond_npv
#'
#' @return Numeric scalar convexity.
#' @export
calc_bond_convexity <- function(cashflows = NULL, rates = NULL, par = NULL, c_rate = NULL, T = NULL, freq = 2, ytm = NULL) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- calc_period_rate(ytm, freq = freq)
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  pv <- x$cashflows / (1 + x$rates)^x$period
  sum((x$period + 1) * x$period * pv / (1 + x$rates)^2) / sum(pv) / (freq^2)
}

#' Compute Effective Bond Duration
#'
#' Computes effective duration by bumping discount rates up and down by a basis
#' point shock.
#'
#' @inheritParams calc_bond_npv
#' @param bases Numeric shock size in basis points.
#'
#' @return Numeric scalar effective duration.
#' @export
calc_bond_eduration <- function(cashflows = NULL, rates = NULL, par = NULL, c_rate = NULL, T = NULL, freq = 2, ytm = NULL, bases = 100) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- calc_period_rate(ytm, freq = freq)
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  bump <- bases / 10000 / freq
  pv_0 <- sum(x$cashflows / (1 + x$rates)^x$period)
  pv_up <- sum(x$cashflows / (1 + x$rates + bump)^x$period)
  pv_dn <- sum(x$cashflows / (1 + x$rates - bump)^x$period)
  (pv_dn - pv_up) / ((bases / 10000) * 2 * pv_0)
}

#' Compute Effective Bond Convexity
#'
#' Computes effective convexity by bumping discount rates up and down by a
#' basis point shock.
#'
#' @inheritParams calc_bond_eduration
#'
#' @return Numeric scalar effective convexity.
#' @export
calc_bond_econvexity <- function(cashflows = NULL, rates = NULL, par = NULL, c_rate = NULL, T = NULL, freq = 2, ytm = NULL, bases = 100) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- calc_period_rate(ytm, freq = freq)
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  bump <- bases / 10000 / freq
  pv_0 <- sum(x$cashflows / (1 + x$rates)^x$period)
  pv_up <- sum(x$cashflows / (1 + x$rates + bump)^x$period)
  pv_dn <- sum(x$cashflows / (1 + x$rates - bump)^x$period)
  (pv_dn + pv_up - 2 * pv_0) / (((bases / 10000)^2) * pv_0)
}

#' Compute Bond Accrued Interest
#'
#' Computes accrued coupon interest for a plain-vanilla fixed-coupon bond using
#' the fraction of the current coupon period that has elapsed.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param freq Integer coupon frequency per year.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#'
#' @return Numeric scalar accrued interest.
#' @export
calc_bond_accrued_interest <- function(par = 1, c_rate, freq = 2, accrual_frac = 0) {
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  accrual_frac <- .validate_accrual_frac(accrual_frac)

  par * c_rate / freq * accrual_frac
}

#' Compute Bond Dirty Price
#'
#' Computes the dirty price of a plain-vanilla fixed-coupon bond from yield and
#' bond terms. Settlement within the current coupon period is represented by
#' `accrual_frac`.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param ytm Numeric annualized yield-to-maturity.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#'
#' @return Numeric scalar dirty price.
#' @export
calc_bond_dirty_price <- function(par = 1, c_rate, T, freq = 2, ytm, accrual_frac = 0) {
  stopifnot(is.numeric(ytm), length(ytm) == 1)
  .bond_dirty_price_from_ytm(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm, accrual_frac = accrual_frac)
}

#' Compute Bond Clean Price
#'
#' Computes the clean price of a plain-vanilla fixed-coupon bond from yield and
#' bond terms.
#'
#' @inheritParams calc_bond_dirty_price
#'
#' @return Numeric scalar clean price.
#' @export
calc_bond_clean_price <- function(par = 1, c_rate, T, freq = 2, ytm, accrual_frac = 0) {
  calc_bond_dirty_price(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm, accrual_frac = accrual_frac) -
    calc_bond_accrued_interest(par = par, c_rate = c_rate, freq = freq, accrual_frac = accrual_frac)
}

#' Compute Bond Yield To Maturity
#'
#' Solves annualized yield-to-maturity from a clean or dirty bond price under a
#' plain-vanilla fixed-coupon schedule.
#'
#' @param price Numeric observed bond price.
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param price_type Character scalar, either `"dirty"` or `"clean"`.
#' @param interval Optional numeric vector of length two giving the search
#'   bracket for annualized yield.
#'
#' @return Numeric scalar annualized yield-to-maturity.
#' @export
calc_bond_yield <- function(price, par = 1, c_rate, T, freq = 2, accrual_frac = 0, price_type = c("dirty", "clean"), interval = NULL) {
  stopifnot(is.numeric(price), length(price) == 1, price > 0)
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(T), length(T) == 1, T > 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  accrual_frac <- .validate_accrual_frac(accrual_frac)
  price_type <- match.arg(price_type)

  if (is.null(interval)) {
    interval <- c(-0.99 * freq, 10)
  }
  stopifnot(is.numeric(interval), length(interval) == 2, interval[1] < interval[2])
  stopifnot(interval[1] > -freq)

  target_dirty <- if (price_type == "dirty") {
    price
  } else {
    price + calc_bond_accrued_interest(par = par, c_rate = c_rate, freq = freq, accrual_frac = accrual_frac)
  }

  f <- function(ytm) {
    .bond_dirty_price_from_ytm(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm, accrual_frac = accrual_frac) - target_dirty
  }

  stats::uniroot(f, interval = interval, tol = .Machine$double.eps^0.5)$root
}

#' Compute Bond DV01
#'
#' Computes DV01 as the average absolute price change for a one-basis-point
#' annualized yield shock.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param ytm Numeric annualized yield-to-maturity.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param bases Numeric yield shock in basis points. Defaults to `1`.
#'
#' @return Numeric scalar DV01.
#' @export
calc_bond_dv01 <- function(par = 1, c_rate, T, freq = 2, ytm, accrual_frac = 0, bases = 1) {
  stopifnot(is.numeric(ytm), length(ytm) == 1)
  stopifnot(is.numeric(bases), length(bases) == 1, bases > 0)

  bump <- bases / 10000
  p_up <- calc_bond_dirty_price(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm + bump, accrual_frac = accrual_frac)
  p_dn <- calc_bond_dirty_price(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm - bump, accrual_frac = accrual_frac)
  (p_dn - p_up) / 2
}

#' Compute Bond PV01
#'
#' Computes PV01 as the average absolute dirty-price change for a one-basis-
#' point annualized yield shock.
#'
#' @inheritParams calc_bond_dv01
#'
#' @return Numeric scalar PV01.
#' @export
calc_bond_pv01 <- function(par = 1, c_rate, T, freq = 2, ytm, accrual_frac = 0, bases = 1) {
  calc_bond_dv01(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac,
    bases = bases
  )
}

#' Approximate Bond Price Change From Duration And Convexity
#'
#' Approximates the absolute bond price change for an annualized yield shock
#' using modified duration and convexity.
#'
#' @param ytm Numeric annualized yield-to-maturity.
#' @param yield_shock Numeric annualized yield shock in decimal units.
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param price_type Character scalar, either `"dirty"` or `"clean"`.
#'
#' @return Numeric scalar approximate absolute price change.
#' @export
calc_bond_price_change_approx <- function(ytm, yield_shock, par = 1, c_rate, T, freq = 2, accrual_frac = 0, price_type = c("dirty", "clean")) {
  stopifnot(is.numeric(ytm), length(ytm) == 1)
  stopifnot(is.numeric(yield_shock), length(yield_shock) == 1)
  price_type <- match.arg(price_type)

  base_price <- if (price_type == "dirty") {
    calc_bond_dirty_price(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm, accrual_frac = accrual_frac)
  } else {
    calc_bond_clean_price(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm, accrual_frac = accrual_frac)
  }

  mduration <- calc_bond_mduration(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm)
  convexity <- calc_bond_convexity(par = par, c_rate = c_rate, T = T, freq = freq, ytm = ytm)
  base_price * (-mduration * yield_shock + 0.5 * convexity * yield_shock^2)
}

#' Compute Bond Current Yield
#'
#' Computes current yield as annual coupon income divided by clean price.
#'
#' @param price Numeric observed bond price.
#' @param c_rate Numeric annual coupon rate.
#' @param par Numeric face value.
#' @param freq Integer coupon frequency per year.
#' @param price_type Character scalar, either `"clean"` or `"dirty"`.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#'
#' @return Numeric scalar current yield.
#' @export
calc_bond_current_yield <- function(price, c_rate, par = 1, freq = 2, price_type = c("clean", "dirty"), accrual_frac = 0) {
  stopifnot(is.numeric(price), length(price) == 1, price > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  price_type <- match.arg(price_type)

  clean_price <- if (price_type == "clean") {
    price
  } else {
    price - calc_bond_accrued_interest(par = par, c_rate = c_rate, freq = freq, accrual_frac = accrual_frac)
  }
  stopifnot(clean_price > 0)

  par * c_rate / clean_price
}

#' Compute Bond Holding-Period Return
#'
#' Computes holding-period return from beginning and ending bond prices plus
#' coupon income received during the holding period.
#'
#' @param begin_price Numeric beginning bond price.
#' @param end_price Numeric ending bond price.
#' @param coupon_income Numeric coupon cash received during the holding period.
#' @param price_type Character scalar, either `"dirty"` or `"clean"`.
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param freq Integer coupon frequency per year.
#' @param begin_accrual_frac Numeric fraction of the opening coupon period
#'   already accrued, on `[0, 1)`.
#' @param end_accrual_frac Numeric fraction of the closing coupon period already
#'   accrued, on `[0, 1)`.
#'
#' @return Numeric scalar holding-period return.
#' @export
calc_bond_holding_period_return <- function(
  begin_price,
  end_price,
  coupon_income = 0,
  price_type = c("dirty", "clean"),
  par = 1,
  c_rate = 0,
  freq = 2,
  begin_accrual_frac = 0,
  end_accrual_frac = 0
) {
  stopifnot(is.numeric(begin_price), length(begin_price) == 1, begin_price > 0)
  stopifnot(is.numeric(end_price), length(end_price) == 1, end_price >= 0)
  stopifnot(is.numeric(coupon_income), length(coupon_income) == 1)
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  price_type <- match.arg(price_type)

  begin_dirty <- if (price_type == "dirty") {
    begin_price
  } else {
    begin_price + calc_bond_accrued_interest(par = par, c_rate = c_rate, freq = freq, accrual_frac = begin_accrual_frac)
  }

  end_dirty <- if (price_type == "dirty") {
    end_price
  } else {
    end_price + calc_bond_accrued_interest(par = par, c_rate = c_rate, freq = freq, accrual_frac = end_accrual_frac)
  }

  (end_dirty + coupon_income - begin_dirty) / begin_dirty
}

#' Compute Bond Key Rate Duration
#'
#' Computes key-rate duration by bumping one point on an explicit per-period
#' discount-rate curve at a time.
#'
#' @param cashflows Optional numeric vector of period cash flows.
#' @param rates Optional numeric scalar or vector of per-period discount rates.
#' @param par Optional numeric face value for term-based input.
#' @param c_rate Optional annual coupon rate for term-based input.
#' @param T Optional maturity in years for term-based input.
#' @param freq Integer compounding frequency per year for term-based input and
#'   annualized basis-point conversion.
#' @param ytm Optional annualized yield-to-maturity for term-based flat-curve
#'   input.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param key_periods Optional integer vector of period indices to shock.
#'   Defaults to all periods.
#' @param bases Numeric annualized key-rate shock size in basis points.
#'
#' @return A `data.table` with `key_period` and `krd`.
#' @export
calc_bond_key_rate_duration <- function(
  cashflows = NULL,
  rates = NULL,
  par = NULL,
  c_rate = NULL,
  T = NULL,
  freq = 2,
  ytm = NULL,
  accrual_frac = 0,
  key_periods = NULL,
  bases = 1
) {
  if (is.null(cashflows) || is.null(rates)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T), !is.null(ytm))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
    rates <- rep(calc_period_rate(ytm, freq = freq), length(cashflows))
  }

  x <- .validate_fi_cashflows(cashflows, rates)
  accrual_frac <- .validate_accrual_frac(accrual_frac)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  stopifnot(is.numeric(bases), length(bases) == 1, bases > 0)

  if (is.null(key_periods)) {
    key_periods <- seq_along(x$cashflows)
  }
  key_periods <- as.integer(key_periods)
  stopifnot(all(key_periods >= 1), all(key_periods <= length(x$cashflows)))

  pv_0 <- .bond_dirty_price_from_curve(x$cashflows, x$rates, accrual_frac = accrual_frac)
  bump <- bases / 10000 / freq

  krd <- vapply(key_periods, function(k) {
    rates_up <- x$rates
    rates_dn <- x$rates
    rates_up[k] <- rates_up[k] + bump
    rates_dn[k] <- rates_dn[k] - bump
    pv_up <- .bond_dirty_price_from_curve(x$cashflows, rates_up, accrual_frac = accrual_frac)
    pv_dn <- .bond_dirty_price_from_curve(x$cashflows, rates_dn, accrual_frac = accrual_frac)
    (pv_dn - pv_up) / ((bases / 10000) * 2 * pv_0)
  }, numeric(1))

  data.table::data.table(
    key_period = key_periods,
    krd = krd
  )
}

#' Compute Bond Carry Return
#'
#' Computes a simple carry return estimate as coupon accrual earned over the
#' holding horizon minus financing cost, scaled by the opening dirty price.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param ytm Numeric annualized yield-to-maturity.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param holding_years Numeric holding horizon in years.
#' @param funding_rate Numeric annualized financing rate applied to the opening
#'   dirty price.
#'
#' @return Numeric scalar carry return.
#' @export
calc_bond_carry <- function(par = 1, c_rate, T, freq = 2, ytm, accrual_frac = 0, holding_years = 1 / freq, funding_rate = 0) {
  stopifnot(is.numeric(holding_years), length(holding_years) == 1, holding_years >= 0, holding_years <= T)
  stopifnot(is.numeric(funding_rate), length(funding_rate) == 1)

  begin_dirty <- calc_bond_dirty_price(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )
  coupon_accrual <- par * c_rate * holding_years
  financing_cost <- begin_dirty * funding_rate * holding_years
  (coupon_accrual - financing_cost) / begin_dirty
}

#' Compute Bond Roll-Down Return
#'
#' Computes the price return from rolling a bond down the curve over a holding
#' horizon while leaving the annualized yield assumption unchanged.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param ytm Numeric annualized yield-to-maturity.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param holding_years Numeric holding horizon in years.
#'
#' @return Numeric scalar roll-down return.
#' @export
calc_bond_roll_down_return <- function(par = 1, c_rate, T, freq = 2, ytm, accrual_frac = 0, holding_years = 1 / freq) {
  stopifnot(is.numeric(holding_years), length(holding_years) == 1, holding_years >= 0, holding_years < T)

  begin_dirty <- calc_bond_dirty_price(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )
  end_dirty <- calc_bond_dirty_price(
    par = par,
    c_rate = c_rate,
    T = T - holding_years,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )
  (end_dirty - begin_dirty) / begin_dirty
}

#' Compute Bond Z-Spread
#'
#' Solves the constant annualized spread that must be added to a per-period spot
#' rate curve so discounted cash flows match an observed clean or dirty price.
#'
#' @param price Numeric observed bond price.
#' @param spot_rates Numeric scalar or vector of per-period spot rates.
#' @param cashflows Optional numeric vector of period cash flows.
#' @param par Optional numeric face value for term-based input.
#' @param c_rate Optional annual coupon rate for term-based input.
#' @param T Optional maturity in years for term-based input.
#' @param freq Integer compounding frequency per year.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param price_type Character scalar, either `"dirty"` or `"clean"`.
#' @param interval Optional numeric vector of length two giving the annualized
#'   z-spread search bracket.
#'
#' @return Numeric scalar annualized z-spread.
#' @export
calc_bond_zspread <- function(
  price,
  spot_rates,
  cashflows = NULL,
  par = NULL,
  c_rate = NULL,
  T = NULL,
  freq = 2,
  accrual_frac = 0,
  price_type = c("dirty", "clean"),
  interval = NULL
) {
  stopifnot(is.numeric(price), length(price) == 1, price > 0)
  stopifnot(is.numeric(spot_rates), length(spot_rates) >= 1)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  accrual_frac <- .validate_accrual_frac(accrual_frac)
  price_type <- match.arg(price_type)

  if (is.null(cashflows)) {
    stopifnot(!is.null(par), !is.null(c_rate), !is.null(T))
    cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
  }

  if (length(spot_rates) == 1) {
    spot_rates <- rep(spot_rates, length(cashflows))
  }
  stopifnot(length(spot_rates) == length(cashflows))

  target_dirty <- if (price_type == "dirty") {
    price
  } else {
    stopifnot(!is.null(par), !is.null(c_rate))
    price + calc_bond_accrued_interest(par = par, c_rate = c_rate, freq = freq, accrual_frac = accrual_frac)
  }

  if (is.null(interval)) {
    interval <- c(-0.99 * freq, 10)
  }
  stopifnot(is.numeric(interval), length(interval) == 2, interval[1] < interval[2])
  stopifnot(interval[1] > -freq)

  f <- function(spread) {
    .bond_dirty_price_from_curve_spread(
      cashflows = cashflows,
      rates = spot_rates,
      spread = spread,
      freq = freq,
      accrual_frac = accrual_frac
    ) - target_dirty
  }

  stats::uniroot(f, interval = interval, tol = .Machine$double.eps^0.5)$root
}

#' Compute Bond Spread Duration
#'
#' Computes spread duration by bumping an annualized z-spread up and down while
#' holding the zero-rate curve fixed.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param tenor Numeric vector of curve tenors in years.
#' @param zero_rate Numeric vector of annualized zero rates aligned with
#'   `tenor`.
#' @param zspread Numeric annualized z-spread in decimal units.
#' @param shock_bp Numeric spread shock size in basis points.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param method Character interpolation method. Currently only `"linear"` is
#'   supported.
#'
#' @return Numeric scalar spread duration.
#' @export
calc_bond_spread_duration <- function(
  par = 1,
  c_rate,
  T,
  freq = 2,
  tenor,
  zero_rate,
  zspread,
  shock_bp = 1,
  accrual_frac = 0,
  method = c("linear")
) {
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(T), length(T) == 1, T > 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  stopifnot(is.numeric(zspread), length(zspread) == 1)
  stopifnot(is.numeric(shock_bp), length(shock_bp) == 1, shock_bp > 0)
  accrual_frac <- .validate_accrual_frac(accrual_frac)
  method <- match.arg(method)

  curve <- .validate_curve_inputs(tenor, zero_rate)
  cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
  pay_tenor <- .bond_discount_periods(length(cashflows), accrual_frac = accrual_frac) / freq
  base_rates <- calc_curve_zero_rate(
    tenor_out = pay_tenor,
    tenor = curve$tenor,
    zero_rate = curve$zero_rate,
    method = method
  ) / freq

  pv_0 <- .bond_dirty_price_from_curve_spread(
    cashflows = cashflows,
    rates = base_rates,
    spread = zspread,
    freq = freq,
    accrual_frac = accrual_frac
  )
  pv_up <- .bond_dirty_price_from_curve_spread(
    cashflows = cashflows,
    rates = base_rates,
    spread = zspread + shock_bp / 10000,
    freq = freq,
    accrual_frac = accrual_frac
  )
  pv_dn <- .bond_dirty_price_from_curve_spread(
    cashflows = cashflows,
    rates = base_rates,
    spread = zspread - shock_bp / 10000,
    freq = freq,
    accrual_frac = accrual_frac
  )

  (pv_dn - pv_up) / ((shock_bp / 10000) * 2 * pv_0)
}

#' Compute Bond Nominal Spread
#'
#' Computes the simple nominal spread between a bond yield and a benchmark
#' yield, in annualized decimal units.
#'
#' @param ytm Numeric annualized yield-to-maturity.
#' @param benchmark_ytm Numeric annualized benchmark yield.
#'
#' @return Numeric scalar nominal spread.
#' @export
calc_bond_nominal_spread <- function(ytm, benchmark_ytm) {
  stopifnot(is.numeric(ytm), length(ytm) == 1)
  stopifnot(is.numeric(benchmark_ytm), length(benchmark_ytm) == 1)
  ytm - benchmark_ytm
}

#' Compute Yield Curve Slope
#'
#' Computes the annualized slope between a short and a long point on the yield
#' curve.
#'
#' @param short_rate Numeric annualized short-end yield.
#' @param long_rate Numeric annualized long-end yield.
#'
#' @return Numeric scalar curve slope.
#' @export
calc_curve_slope <- function(short_rate, long_rate) {
  stopifnot(is.numeric(short_rate), length(short_rate) == 1)
  stopifnot(is.numeric(long_rate), length(long_rate) == 1)
  long_rate - short_rate
}

#' Compute Yield Curve Butterfly
#'
#' Computes a simple annualized butterfly measure as the belly yield minus the
#' average of wing yields.
#'
#' @param short_rate Numeric annualized short-end yield.
#' @param mid_rate Numeric annualized belly yield.
#' @param long_rate Numeric annualized long-end yield.
#'
#' @return Numeric scalar butterfly measure.
#' @export
calc_curve_butterfly <- function(short_rate, mid_rate, long_rate) {
  stopifnot(is.numeric(short_rate), length(short_rate) == 1)
  stopifnot(is.numeric(mid_rate), length(mid_rate) == 1)
  stopifnot(is.numeric(long_rate), length(long_rate) == 1)
  mid_rate - (short_rate + long_rate) / 2
}

#' Interpolate Zero Rates On A Curve
#'
#' Interpolates annualized zero rates from an input tenor grid onto requested
#' output tenors.
#'
#' @param tenor_out Numeric vector of requested output tenors in years.
#' @param tenor Numeric vector of input curve tenors in years.
#' @param zero_rate Numeric vector of annualized zero rates aligned with
#'   `tenor`.
#' @param method Character interpolation method. Currently only `"linear"` is
#'   supported.
#'
#' @return Numeric vector of interpolated annualized zero rates.
#' @export
calc_curve_zero_rate <- function(tenor_out, tenor, zero_rate, method = c("linear")) {
  method <- match.arg(method)
  .interp_curve_rate(target_tenor = tenor_out, tenor = tenor, zero_rate = zero_rate, method = method)
}

#' Shock A Zero-Rate Curve
#'
#' Applies either a parallel or tenor-specific basis-point shock to an
#' annualized zero-rate curve.
#'
#' @param tenor Numeric vector of curve tenors in years.
#' @param zero_rate Numeric vector of annualized zero rates aligned with
#'   `tenor`.
#' @param shock_bp Numeric shock size in basis points.
#' @param shock_tenor Optional scalar tenor to shock. If `NULL`, applies a
#'   parallel shift to the full curve.
#'
#' @return A `data.table` with `tenor`, `zero_rate`, and `zero_rate_shocked`.
#' @export
calc_curve_shock <- function(tenor, zero_rate, shock_bp, shock_tenor = NULL) {
  .shock_curve_tenor(
    tenor = tenor,
    zero_rate = zero_rate,
    shock_tenor = shock_tenor,
    shock_bp = shock_bp
  )
}

#' Compute Bond Key Rate Duration By Curve Tenor
#'
#' Computes key-rate duration by bumping one annualized zero-rate tenor on an
#' interpolated curve while holding the rest of the curve fixed.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param tenor Numeric vector of curve tenors in years.
#' @param zero_rate Numeric vector of annualized zero rates aligned with
#'   `tenor`.
#' @param key_tenor Numeric scalar tenor to shock.
#' @param shock_bp Numeric key-rate shock size in basis points.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param method Character interpolation method. Currently only `"linear"` is
#'   supported.
#'
#' @return Numeric scalar tenor-based key-rate duration.
#' @export
calc_bond_key_rate_duration_tenor <- function(
  par = 1,
  c_rate,
  T,
  freq = 2,
  tenor,
  zero_rate,
  key_tenor,
  shock_bp = 1,
  accrual_frac = 0,
  method = c("linear")
) {
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(T), length(T) == 1, T > 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  stopifnot(is.numeric(key_tenor), length(key_tenor) == 1)
  stopifnot(is.numeric(shock_bp), length(shock_bp) == 1, shock_bp > 0)
  method <- match.arg(method)
  accrual_frac <- .validate_accrual_frac(accrual_frac)

  curve <- .validate_curve_inputs(tenor, zero_rate)
  stopifnot(any(abs(curve$tenor - key_tenor) < .Machine$double.eps^0.5))

  cashflows <- calc_bond_cashflows(par = par, c_rate = c_rate, T = T, freq = freq)
  pay_tenor <- .bond_discount_periods(length(cashflows), accrual_frac = accrual_frac) / freq
  base_zero <- calc_curve_zero_rate(
    tenor_out = pay_tenor,
    tenor = curve$tenor,
    zero_rate = curve$zero_rate,
    method = method
  )
  base_rates <- base_zero / freq
  pv_0 <- .bond_dirty_price_from_curve(cashflows = cashflows, rates = base_rates, accrual_frac = accrual_frac)

  shock_up <- calc_curve_shock(curve$tenor, curve$zero_rate, shock_bp = shock_bp, shock_tenor = key_tenor)
  shock_dn <- calc_curve_shock(curve$tenor, curve$zero_rate, shock_bp = -shock_bp, shock_tenor = key_tenor)

  rates_up <- calc_curve_zero_rate(
    tenor_out = pay_tenor,
    tenor = shock_up$tenor,
    zero_rate = shock_up$zero_rate_shocked,
    method = method
  ) / freq
  rates_dn <- calc_curve_zero_rate(
    tenor_out = pay_tenor,
    tenor = shock_dn$tenor,
    zero_rate = shock_dn$zero_rate_shocked,
    method = method
  ) / freq

  pv_up <- .bond_dirty_price_from_curve(cashflows = cashflows, rates = rates_up, accrual_frac = accrual_frac)
  pv_dn <- .bond_dirty_price_from_curve(cashflows = cashflows, rates = rates_dn, accrual_frac = accrual_frac)

  (pv_dn - pv_up) / ((shock_bp / 10000) * 2 * pv_0)
}

#' Approximate Bond Rate Return
#'
#' Computes the duration-convexity approximation to bond return from an
#' annualized yield shock.
#'
#' @param duration Numeric modified duration.
#' @param convexity Numeric convexity. Defaults to `0`.
#' @param delta_y Numeric annualized yield shock in decimal units.
#'
#' @return Numeric scalar approximate rate return.
#' @export
calc_bond_rate_return_approx <- function(duration, delta_y, convexity = 0) {
  stopifnot(is.numeric(duration), length(duration) == 1)
  stopifnot(is.numeric(delta_y), length(delta_y) == 1)
  stopifnot(is.numeric(convexity), length(convexity) == 1)

  -duration * delta_y + 0.5 * convexity * delta_y^2
}

#' Approximate Bond Spread Return
#'
#' Computes the first-order approximation to bond return from a spread move.
#'
#' @param spread_duration Numeric spread duration.
#' @param delta_s Numeric annualized spread shock in decimal units.
#'
#' @return Numeric scalar approximate spread return.
#' @export
calc_bond_spread_return_approx <- function(spread_duration, delta_s) {
  stopifnot(is.numeric(spread_duration), length(spread_duration) == 1)
  stopifnot(is.numeric(delta_s), length(delta_s) == 1)

  -spread_duration * delta_s
}

#' Approximate Bond Z-Spread Return
#'
#' Computes the first-order approximation to bond return from a z-spread move.
#'
#' @param spread_duration Numeric spread duration.
#' @param delta_z Numeric annualized z-spread shock in decimal units.
#'
#' @return Numeric scalar approximate z-spread return.
#' @export
calc_bond_zspread_return_approx <- function(spread_duration, delta_z) {
  calc_bond_spread_return_approx(
    spread_duration = spread_duration,
    delta_s = delta_z
  )
}

#' Decompose Bond Carry And Roll Return
#'
#' Returns carry, roll-down, and their sum as a compact one-row `data.table`.
#'
#' @param carry Numeric carry return.
#' @param roll_down_return Numeric roll-down return.
#'
#' @return One-row `data.table` with `carry`, `roll_down`, and `carry_roll`.
#' @export
calc_bond_carry_roll_decomp <- function(carry, roll_down_return) {
  stopifnot(is.numeric(carry), length(carry) == 1)
  stopifnot(is.numeric(roll_down_return), length(roll_down_return) == 1)

  data.table::data.table(
    carry = carry,
    roll_down = roll_down_return,
    carry_roll = carry + roll_down_return
  )
}

#' Decompose Bond Total Return
#'
#' Returns the main building blocks of bond total return as a compact one-row
#' `data.table`.
#'
#' @param carry Numeric carry return.
#' @param roll_down_return Numeric roll-down return.
#' @param rate_return Numeric rate-driven return.
#' @param spread_return Numeric spread-driven return.
#' @param residual Numeric residual return. Defaults to `0`.
#'
#' @return One-row `data.table` with component returns and `total_return`.
#' @export
calc_bond_total_return_decomp <- function(
  carry,
  roll_down_return = 0,
  rate_return = 0,
  spread_return = 0,
  residual = 0
) {
  stopifnot(is.numeric(carry), length(carry) == 1)
  stopifnot(is.numeric(roll_down_return), length(roll_down_return) == 1)
  stopifnot(is.numeric(rate_return), length(rate_return) == 1)
  stopifnot(is.numeric(spread_return), length(spread_return) == 1)
  stopifnot(is.numeric(residual), length(residual) == 1)

  data.table::data.table(
    carry = carry,
    roll_down = roll_down_return,
    rate_return = rate_return,
    spread_return = spread_return,
    residual = residual,
    total_return = carry + roll_down_return + rate_return + spread_return + residual
  )
}

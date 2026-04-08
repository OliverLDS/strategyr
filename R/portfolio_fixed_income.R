#' Compute Bond Risk State
#'
#' Builds a compact fixed-income risk-state snapshot that can be used by
#' portfolio-adjustment or hedge-planning logic.
#'
#' @param par Numeric face value.
#' @param c_rate Numeric annual coupon rate.
#' @param T Numeric scheduled maturity in years.
#' @param freq Integer coupon frequency per year.
#' @param ytm Numeric annualized yield-to-maturity.
#' @param accrual_frac Numeric fraction of the current coupon period already
#'   accrued, on `[0, 1)`.
#' @param tenor Optional numeric vector of curve tenors in years.
#' @param zero_rate Optional numeric vector of annualized zero rates aligned
#'   with `tenor`.
#' @param zspread Optional annualized z-spread in decimal units.
#'
#' @return A one-row `data.table` containing bond valuation and risk measures.
#' @export
calc_bond_risk_state <- function(
  par = 1,
  c_rate,
  T,
  freq = 2,
  ytm,
  accrual_frac = 0,
  tenor = NULL,
  zero_rate = NULL,
  zspread = NULL
) {
  stopifnot(is.numeric(par), length(par) == 1, par > 0)
  stopifnot(is.numeric(c_rate), length(c_rate) == 1, c_rate >= 0)
  stopifnot(is.numeric(T), length(T) == 1, T > 0)
  stopifnot(is.numeric(freq), length(freq) == 1, freq > 0)
  stopifnot(is.numeric(ytm), length(ytm) == 1)

  clean_price <- calc_bond_clean_price(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )
  dirty_price <- calc_bond_dirty_price(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )
  duration <- calc_bond_duration(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm
  )
  mduration <- calc_bond_mduration(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm
  )
  convexity <- calc_bond_convexity(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm
  )
  dv01 <- calc_bond_dv01(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )
  pv01 <- calc_bond_pv01(
    par = par,
    c_rate = c_rate,
    T = T,
    freq = freq,
    ytm = ytm,
    accrual_frac = accrual_frac
  )

  out <- data.table::data.table(
    clean_price = clean_price,
    dirty_price = dirty_price,
    ytm = ytm,
    duration = duration,
    mduration = mduration,
    convexity = convexity,
    dv01 = dv01,
    pv01 = pv01
  )

  if (!is.null(tenor) && !is.null(zero_rate)) {
    stopifnot(is.numeric(tenor), is.numeric(zero_rate), length(tenor) == length(zero_rate))
    if (is.null(zspread)) {
      zspread <- calc_bond_zspread(
        price = dirty_price,
        spot_rates = calc_curve_zero_rate(
          tenor_out = .bond_discount_periods(as.integer(round(T * freq)), accrual_frac = accrual_frac) / freq,
          tenor = tenor,
          zero_rate = zero_rate
        ) / freq,
        par = par,
        c_rate = c_rate,
        T = T,
        freq = freq,
        accrual_frac = accrual_frac,
        price_type = "dirty"
      )
    }
    out[, zspread := zspread]
  }

  out[]
}

#' Plan Duration-Neutral Adjustment
#'
#' Computes the hedge units needed to move current DV01 toward a target DV01.
#'
#' @param current_dv01 Numeric current portfolio DV01.
#' @param target_dv01 Numeric target portfolio DV01. Defaults to `0`.
#' @param hedge_dv01 Numeric DV01 contribution per hedge unit.
#'
#' @return A one-row `data.table` with current, target, gap, and hedge units.
#' @export
plan_duration_neutral_adjustment <- function(current_dv01, target_dv01 = 0, hedge_dv01) {
  stopifnot(is.numeric(current_dv01), length(current_dv01) == 1)
  stopifnot(is.numeric(target_dv01), length(target_dv01) == 1)
  stopifnot(is.numeric(hedge_dv01), length(hedge_dv01) == 1, hedge_dv01 != 0)

  dv01_gap <- target_dv01 - current_dv01
  hedge_units <- dv01_gap / hedge_dv01

  data.table::data.table(
    current_dv01 = current_dv01,
    target_dv01 = target_dv01,
    dv01_gap = dv01_gap,
    hedge_dv01 = hedge_dv01,
    hedge_units = hedge_units
  )
}

#' Plan Curve Trade Adjustment
#'
#' Computes hedge units that move current key-rate exposure toward target
#' key-rate exposure.
#'
#' @param current_krd Named numeric vector of current key-rate exposures.
#' @param target_krd Named numeric vector of target key-rate exposures.
#' @param hedge_krd Named numeric vector of hedge-instrument key-rate exposures
#'   per unit.
#'
#' @return A `data.table` with tenor, current, target, gap, hedge exposure, and
#'   hedge units.
#' @export
plan_curve_trade_adjustment <- function(current_krd, target_krd, hedge_krd) {
  stopifnot(is.numeric(current_krd), is.numeric(target_krd), is.numeric(hedge_krd))
  stopifnot(!is.null(names(current_krd)), !is.null(names(target_krd)), !is.null(names(hedge_krd)))

  tenor <- union(names(current_krd), union(names(target_krd), names(hedge_krd)))
  current <- setNames(rep(0, length(tenor)), tenor)
  target <- current
  hedge <- current

  current[names(current_krd)] <- current_krd
  target[names(target_krd)] <- target_krd
  hedge[names(hedge_krd)] <- hedge_krd

  stopifnot(all(hedge != 0))

  gap <- target - current
  hedge_units <- gap / hedge

  data.table::data.table(
    tenor = tenor,
    current_krd = as.numeric(current),
    target_krd = as.numeric(target),
    krd_gap = as.numeric(gap),
    hedge_krd = as.numeric(hedge),
    hedge_units = as.numeric(hedge_units)
  )
}

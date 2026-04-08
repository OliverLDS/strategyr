#' Compute Position Greeks
#'
#' Computes Black-Scholes Greeks per row and scales them by position size and
#' contract size so the result is ready for portfolio aggregation.
#'
#' @param option_state A `data.table` containing at least `asset`, `S`, `K`,
#'   `T`, `r`, `sigma`, `type`, and `units`.
#' @param contract_size_col Column name containing contract-size multipliers.
#' @param units_col Column name containing signed position units.
#'
#' @return A `data.table` with per-contract and position-scaled Greek columns.
#' @export
calc_position_greeks <- function(option_state, contract_size_col = "contract_size", units_col = "units") {
  stopifnot(data.table::is.data.table(option_state))

  required_cols <- c("asset", "S", "K", "T", "r", "sigma", "type", units_col)
  stopifnot(all(required_cols %in% names(option_state)))

  DT <- data.table::copy(option_state)
  if (!contract_size_col %in% names(DT)) {
    DT[, (contract_size_col) := 1.0]
  }
  if (!"q" %in% names(DT)) {
    DT[, q := 0.0]
  }

  DT[, price := mapply(.calc_option_price_bs, S, K, T, r, sigma, type, q)]
  DT[, delta := mapply(calc_option_delta, S, K, T, r, sigma, type, q)]
  DT[, gamma := mapply(calc_option_gamma, S, K, T, r, sigma, q)]
  DT[, vega := mapply(calc_option_vega, S, K, T, r, sigma, q)]
  DT[, theta := mapply(calc_option_theta, S, K, T, r, sigma, type, q, MoreArgs = list(scale = "daily"))]
  DT[, rho := mapply(calc_option_rho, S, K, T, r, sigma, type, q)]

  DT[, delta_pos := get(units_col) * get(contract_size_col) * delta]
  DT[, gamma_pos := get(units_col) * get(contract_size_col) * gamma]
  DT[, vega_pos := get(units_col) * get(contract_size_col) * vega]
  DT[, theta_pos := get(units_col) * get(contract_size_col) * theta]
  DT[, rho_pos := get(units_col) * get(contract_size_col) * rho]
  DT[]
}

#' Compute Option Risk State
#'
#' Builds a compact option risk-state snapshot from Black-Scholes price,
#' implied volatility, and Greeks.
#'
#' @param S Numeric spot price.
#' @param K Numeric strike price.
#' @param T Numeric time to expiry in years.
#' @param r Numeric annualized risk-free rate.
#' @param sigma Optional numeric annualized volatility.
#' @param type Character scalar, either `"call"` or `"put"`.
#' @param q Numeric annualized continuous dividend yield.
#' @param price Optional observed option price. Required when `sigma` is
#'   missing.
#'
#' @return A one-row `data.table` with observed/model price, implied
#'   volatility, IV source, and Greeks.
#' @export
calc_option_risk_state <- function(S, K, T, r, sigma = NULL, type = c("call", "put"), q = 0, price = NULL) {
  type <- .validate_option_type(type)

  iv_source <- "input_sigma"
  if (is.null(sigma)) {
    stopifnot(!is.null(price))
    sigma <- calc_option_iv(price = price, S = S, K = K, T = T, r = r, type = type, q = q)
    iv_source <- "inferred_from_price"
  }

  greeks <- calc_option_greeks(S = S, K = K, T = T, r = r, sigma = sigma, type = type, q = q, theta_scale = "daily")
  model_price <- greeks$price
  market_price <- if (is.null(price)) model_price else price

  greeks[, `:=`(
    market_price = market_price,
    model_price = model_price,
    iv = sigma,
    iv_source = iv_source,
    type = type
  )]
  greeks[, price := market_price]
  data.table::setcolorder(
    greeks,
    c("price", "market_price", "model_price", "iv", "iv_source", "type", "delta", "gamma", "vega", "theta", "rho")
  )
  greeks[]
}

.plan_scalar_greek_adjustment <- function(current_exposure, target_exposure, hedge_exposure, greek_label) {
  stopifnot(is.numeric(current_exposure), length(current_exposure) == 1)
  stopifnot(is.numeric(target_exposure), length(target_exposure) == 1)
  stopifnot(is.numeric(hedge_exposure), length(hedge_exposure) == 1, hedge_exposure != 0)
  stopifnot(is.character(greek_label), length(greek_label) == 1)

  exposure_gap <- target_exposure - current_exposure
  hedge_units <- exposure_gap / hedge_exposure
  hedge_action <- if (abs(hedge_units) < .Machine$double.eps^0.5) {
    "flat"
  } else if (hedge_units > 0) {
    "buy"
  } else {
    "sell"
  }

  out <- data.table::data.table(
    current_exposure = current_exposure,
    target_exposure = target_exposure,
    exposure_gap = exposure_gap,
    hedge_exposure = hedge_exposure,
    hedge_units = hedge_units,
    hedge_units_abs = abs(hedge_units),
    hedge_action = hedge_action
  )

  data.table::setnames(
    out,
    old = c("current_exposure", "target_exposure", "exposure_gap", "hedge_exposure"),
    new = c(
      paste0("current_", greek_label),
      paste0("target_", greek_label),
      paste0(greek_label, "_gap"),
      paste0("hedge_", greek_label)
    )
  )
  out[]
}

#' Plan Delta-Neutral Adjustment
#'
#' Computes hedge units needed to move current delta toward a target delta.
#'
#' @param current_delta Numeric current portfolio delta.
#' @param target_delta Numeric target portfolio delta. Defaults to `0`.
#' @param hedge_delta Numeric delta contribution per hedge unit.
#'
#' @return A one-row `data.table` with current, target, gap, hedge exposure,
#'   hedge units, absolute hedge units, and hedge action.
#' @export
plan_delta_neutral_adjustment <- function(current_delta, target_delta = 0, hedge_delta) {
  .plan_scalar_greek_adjustment(
    current_exposure = current_delta,
    target_exposure = target_delta,
    hedge_exposure = hedge_delta,
    greek_label = "delta"
  )
}

#' Plan Vega Target Adjustment
#'
#' Computes hedge units needed to move current vega toward a target vega.
#'
#' @param current_vega Numeric current portfolio vega.
#' @param target_vega Numeric target portfolio vega. Defaults to `0`.
#' @param hedge_vega Numeric vega contribution per hedge unit.
#'
#' @return A one-row `data.table` with current, target, gap, hedge exposure,
#'   hedge units, absolute hedge units, and hedge action.
#' @export
plan_vega_target_adjustment <- function(current_vega, target_vega = 0, hedge_vega) {
  .plan_scalar_greek_adjustment(
    current_exposure = current_vega,
    target_exposure = target_vega,
    hedge_exposure = hedge_vega,
    greek_label = "vega"
  )
}

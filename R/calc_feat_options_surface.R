.validate_option_chain <- function(DT, required_cols) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(required_cols %in% names(DT)))
}

.pick_nearest_row <- function(DT, target_col, target_value, side = c("any", "left", "right")) {
  side <- match.arg(side)
  if (!nrow(DT)) {
    return(DT[0])
  }
  x <- DT[[target_col]]
  keep <- rep(TRUE, length(x))
  if (side == "left") {
    keep <- x <= target_value
  } else if (side == "right") {
    keep <- x >= target_value
  }
  cand <- DT[keep]
  if (!nrow(cand)) {
    return(DT[0])
  }
  cand[which.min(abs(cand[[target_col]] - target_value))]
}

#' Add Option Moneyness Features
#'
#' Computes spot-based option moneyness columns in place from underlying spot
#' and strike.
#'
#' @param DT A `data.table` option chain containing spot and strike columns.
#' @param spot_col Underlying spot-price column name.
#' @param strike_col Strike column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_option_moneyness <- function(DT, spot_col = "S", strike_col = "K") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(spot_col, strike_col) %in% names(DT)))

  ratio <- DT[[spot_col]] / DT[[strike_col]]
  data.table::set(DT, j = "option_moneyness", value = ratio)
  data.table::set(DT, j = "option_log_moneyness", value = log(ratio))
  invisible(DT)
}

#' Add Option Forward-Moneyness Features
#'
#' Computes forward-based option moneyness columns in place from spot, strike,
#' time to expiry, rates, and dividend yield.
#'
#' @param DT A `data.table` option chain containing spot, strike, expiry, and
#'   rate columns.
#' @param spot_col Underlying spot-price column name.
#' @param strike_col Strike column name.
#' @param expiry_col Time-to-expiry column name in years.
#' @param rate_col Risk-free rate column name.
#' @param dividend_col Dividend-yield column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_option_forward_moneyness <- function(DT, spot_col = "S", strike_col = "K", expiry_col = "T", rate_col = "r", dividend_col = "q") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(spot_col, strike_col, expiry_col, rate_col) %in% names(DT)))
  if (!dividend_col %in% names(DT)) {
    DT[, (dividend_col) := 0]
  }

  forward <- DT[[spot_col]] * exp((DT[[rate_col]] - DT[[dividend_col]]) * DT[[expiry_col]])
  ratio <- forward / DT[[strike_col]]
  data.table::set(DT, j = "option_forward", value = forward)
  data.table::set(DT, j = "option_forward_moneyness", value = ratio)
  data.table::set(DT, j = "option_log_forward_moneyness", value = log(ratio))
  invisible(DT)
}

#' Compute Option Implied-Volatility Skew
#'
#' Computes per-date and per-expiry put-minus-call IV skew using strikes nearest
#' to a target absolute forward-moneyness level.
#'
#' @param DT A `data.table` option chain containing date, expiry, type,
#'   moneyness, and IV columns.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name containing `call`/`put`.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param target_abs_moneyness Numeric target absolute log-moneyness.
#'
#' @return A `data.table` with IV skew per date and expiry.
#' @export
calc_option_iv_skew <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1) {
  .validate_option_chain(DT, c(date_col, expiry_col, type_col, moneyness_col, iv_col))

  out <- DT[, {
    put_row <- .pick_nearest_row(.SD[get(type_col) == "put"], moneyness_col, -abs(target_abs_moneyness), side = "left")
    call_row <- .pick_nearest_row(.SD[get(type_col) == "call"], moneyness_col, abs(target_abs_moneyness), side = "right")
    put_iv <- if (nrow(put_row)) put_row[[iv_col]][1] else NA_real_
    call_iv <- if (nrow(call_row)) call_row[[iv_col]][1] else NA_real_
    .(
      iv_put_otm = put_iv,
      iv_call_otm = call_iv,
      iv_skew = put_iv - call_iv
    )
  }, by = c(date_col, expiry_col)]
  data.table::setorderv(out, c(date_col, expiry_col))
  out[]
}

#' Compute Option Implied-Volatility Term Structure
#'
#' Computes per-date ATM IV term-structure slope using the nearest-to-ATM option
#' at each expiry.
#'
#' @param DT A `data.table` option chain containing date, expiry, moneyness, and
#'   IV columns.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#'
#' @return A `data.table` with ATM-front, ATM-back, and ATM-term-structure
#'   slope per date.
#' @export
calc_option_iv_term_structure <- function(DT, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv") {
  .validate_option_chain(DT, c(date_col, expiry_col, moneyness_col, iv_col))

  atm <- DT[, .SD[which.min(abs(get(moneyness_col)))], by = c(date_col, expiry_col)]
  out <- atm[, {
    x <- get(expiry_col)
    y <- get(iv_col)
    slope <- if (length(x) >= 2) stats::coef(stats::lm(y ~ x))[[2]] else NA_real_
    idx_front <- which.min(x)
    idx_back <- which.max(x)
    .(
      iv_atm_front = y[idx_front],
      iv_atm_back = y[idx_back],
      iv_term_structure = slope
    )
  }, by = date_col]
  data.table::setorderv(out, date_col)
  out[]
}

#' Compute Option Smile Slope
#'
#' Computes per-date and per-expiry smile slope from a linear fit of implied
#' volatility on absolute forward log-moneyness.
#'
#' @param DT A `data.table` option chain containing date, expiry, moneyness, and
#'   IV columns.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#'
#' @return A `data.table` with smile slope per date and expiry.
#' @export
calc_option_smile_slope <- function(DT, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv") {
  .validate_option_chain(DT, c(date_col, expiry_col, moneyness_col, iv_col))

  out <- DT[, {
    x <- abs(get(moneyness_col))
    y <- get(iv_col)
    valid <- is.finite(x) & is.finite(y)
    slope <- if (sum(valid) >= 2) stats::coef(stats::lm(y[valid] ~ x[valid]))[[2]] else NA_real_
    .(iv_smile_slope = slope)
  }, by = c(date_col, expiry_col)]
  data.table::setorderv(out, c(date_col, expiry_col))
  out[]
}

#' Compute Option Put-Call IV Spread
#'
#' Computes per-date and per-expiry put-minus-call ATM IV spread using the
#' nearest-to-ATM option of each type.
#'
#' @param DT A `data.table` option chain containing date, expiry, type,
#'   moneyness, and IV columns.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name containing `call`/`put`.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#'
#' @return A `data.table` with ATM put-call IV spread per date and expiry.
#' @export
calc_option_put_call_iv_spread <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv") {
  .validate_option_chain(DT, c(date_col, expiry_col, type_col, moneyness_col, iv_col))

  out <- DT[, {
    put_row <- .pick_nearest_row(.SD[get(type_col) == "put"], moneyness_col, 0, side = "left")
    call_row <- .pick_nearest_row(.SD[get(type_col) == "call"], moneyness_col, 0, side = "right")
    put_iv <- if (nrow(put_row)) put_row[[iv_col]][1] else NA_real_
    call_iv <- if (nrow(call_row)) call_row[[iv_col]][1] else NA_real_
    .(
      iv_put_atm = put_iv,
      iv_call_atm = call_iv,
      iv_put_call_spread = put_iv - call_iv
    )
  }, by = c(date_col, expiry_col)]
  data.table::setorderv(out, c(date_col, expiry_col))
  out[]
}

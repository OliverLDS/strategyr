#' Add On-Balance Volume Features
#'
#' Computes the cumulative on-balance volume series from candle closes and
#' traded volume in place.
#'
#' @param DT A `data.table` containing `close` and `volume`.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_OBV <- function(DT) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("close", "volume") %in% names(DT)))

  close <- DT[["close"]]
  volume <- DT[["volume"]]
  len <- length(close)
  obv_step <- rep(NA_real_, len)

  if (len > 0) {
    obv_step[1] <- volume[1]
  }
  if (len > 1) {
    pr_chg <- close[-1] - close[-len]
    obv_step[-1] <- ifelse(pr_chg > 0, volume[-1], -volume[-1])
    obv_step[-1][abs(pr_chg) < sqrt(.Machine$double.eps)] <- 0
  }

  data.table::set(DT, j = "obv", value = cumsum(obv_step))
  invisible(DT)
}

#' Add Money Flow Index Features
#'
#' Computes Money Flow Index columns from candle highs, lows, closes, and
#' traded volume in place.
#'
#' @param DT A `data.table` containing `high`, `low`, `close`, and `volume`.
#' @param ns Integer vector of MFI windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_MFI <- function(DT, ns = c(14)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close", "volume") %in% names(DT)))

  price <- (DT[["high"]] + DT[["low"]] + DT[["close"]]) / 3
  price_lag <- .lag_num(price, 1)
  money_flow <- price * DT[["volume"]]
  pos_flow <- ifelse(price > price_lag, money_flow, 0)
  neg_flow <- ifelse(price < price_lag, money_flow, 0)

  for (n in ns) {
    pos_sum <- rolling_sum(pos_flow, n)
    neg_sum <- rolling_sum(neg_flow, n)
    money_ratio <- pos_sum / neg_sum
    mfi <- 100 - 100 / (1 + money_ratio)
    mfi[is.nan(mfi)] <- NA_real_
    mfi[neg_sum == 0] <- 100
    mfi[neg_sum == 0 & pos_sum == 0] <- 50

    data.table::set(DT, j = paste0("mfi_", n), value = mfi)
  }

  invisible(DT)
}

#' Add Volume-Weighted Average Price Features
#'
#' Computes rolling VWAP columns from candle closes and traded volume in place.
#'
#' @param DT A `data.table` containing `close` and `volume`.
#' @param ns Integer vector of VWAP windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_VWAP <- function(DT, ns = c(10)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("close", "volume") %in% names(DT)))

  pxv <- DT[["close"]] * DT[["volume"]]
  volume <- DT[["volume"]]

  for (n in ns) {
    denom <- rolling_sum(volume, n)
    vwap <- rolling_sum(pxv, n) / denom
    vwap[is.nan(vwap)] <- NA_real_
    vwap[denom == 0] <- NA_real_

    data.table::set(DT, j = paste0("vwap_", n), value = vwap)
  }

  invisible(DT)
}

#' Add Volume-Weighted Moving Average Features
#'
#' Computes rolling VWMA columns from candle closes and traded volume in place.
#'
#' @param DT A `data.table` containing `close` and `volume`.
#' @param ns Integer vector of VWMA windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_VWMA <- function(DT, ns = c(10)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("close", "volume") %in% names(DT)))

  pxv <- DT[["close"]] * DT[["volume"]]
  volume <- DT[["volume"]]

  for (n in ns) {
    denom <- rolling_sum(volume, n)
    vwma <- rolling_sum(pxv, n) / denom
    vwma[is.nan(vwma)] <- NA_real_
    vwma[denom == 0] <- NA_real_

    data.table::set(DT, j = paste0("vwma_", n), value = vwma)
  }

  invisible(DT)
}

#' Add Chaikin Money Flow Features
#'
#' Computes Chaikin Money Flow columns from candle highs, lows, closes, and
#' traded volume in place.
#'
#' @param DT A `data.table` containing `high`, `low`, `close`, and `volume`.
#' @param ns Integer vector of CMF windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_CMF <- function(DT, ns = c(20)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close", "volume") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]
  close <- DT[["close"]]
  volume <- DT[["volume"]]
  denom_clv <- high - low
  clv <- ((close - low) - (high - close)) / denom_clv
  clv[is.nan(clv)] <- NA_real_

  for (n in ns) {
    denom <- rolling_sum(volume, n)
    cmf <- rolling_sum(clv * volume, n) / denom
    cmf[is.nan(cmf)] <- NA_real_
    cmf[denom == 0] <- NA_real_

    data.table::set(DT, j = paste0("cmf_", n), value = cmf)
  }

  invisible(DT)
}

#' Add Chaikin Accumulation Distribution Features
#'
#' Computes the cumulative Chaikin Accumulation Distribution line from candle
#' highs, lows, closes, and traded volume in place.
#'
#' @param DT A `data.table` containing `high`, `low`, `close`, and `volume`.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_chaikinAD <- function(DT) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "close", "volume") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]
  close <- DT[["close"]]
  volume <- DT[["volume"]]
  denom <- high - low
  clv <- ((close - low) - (high - close)) / denom
  clv[is.nan(clv)] <- NA_real_

  ad_step <- clv * volume
  idx <- which(!is.na(ad_step))
  out <- rep(NA_real_, length(ad_step))
  if (length(idx)) {
    out[idx] <- cumsum(ad_step[idx])
  }

  data.table::set(DT, j = "chaikin_ad", value = out)
  invisible(DT)
}

#' Add Chaikin Volatility Features
#'
#' Computes Chaikin Volatility columns from candle highs and lows in place
#' using an EMA of the range followed by a discrete rate of change.
#'
#' @param DT A `data.table` containing `high` and `low`.
#' @param ns Integer vector of EMA and ROC windows.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_chaikinVolatility <- function(DT, ns = c(10)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low") %in% names(DT)))

  hl_range <- DT[["high"]] - DT[["low"]]
  for (n in ns) {
    mavg <- ema_ttr_fixed_step(hl_range, n, FALSE)
    volatility <- mavg / .lag_num(mavg, n) - 1
    volatility[is.na(.lag_num(mavg, n))] <- NA_real_
    volatility[is.nan(volatility)] <- NA_real_
    data.table::set(DT, j = paste0("chaikin_volatility_", n), value = volatility)
  }

  invisible(DT)
}

#' Add Ease of Movement Features
#'
#' Computes the raw Ease of Movement series and smoothed moving-average columns
#' from candle highs, lows, and traded volume in place.
#'
#' @param DT A `data.table` containing `high`, `low`, and `volume`.
#' @param ns Integer vector of smoothing windows for the moving-average EMV.
#' @param vol_divisor Numeric divisor applied to `volume` before the box-ratio
#'   calculation.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_EMV <- function(DT, ns = c(9), vol_divisor = 10000) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c("high", "low", "volume") %in% names(DT)))

  high <- DT[["high"]]
  low <- DT[["low"]]
  volume <- DT[["volume"]] / vol_divisor
  mid <- (high + low) / 2
  distance <- mid - .lag_num(mid, 1)
  box_ratio <- volume / (high - low)
  emv <- distance / box_ratio
  emv[is.nan(emv)] <- NA_real_

  data.table::set(DT, j = "emv", value = emv)
  for (n in ns) {
    data.table::set(DT, j = paste0("emv_ma_", n), value = .apply_after_first_non_na(emv, rolling_mean, n = n))
  }

  invisible(DT)
}

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

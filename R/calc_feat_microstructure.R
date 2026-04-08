#' Add Bid-Ask Spread Features
#'
#' Computes absolute and relative bid-ask spread columns in place.
#'
#' @param DT A `data.table` containing bid and ask columns.
#' @param bid_col Bid-price column name.
#' @param ask_col Ask-price column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_bid_ask_spread <- function(DT, bid_col = "bid", ask_col = "ask") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(bid_col, ask_col) %in% names(DT)))

  bid <- DT[[bid_col]]
  ask <- DT[[ask_col]]
  spread <- ask - bid
  rel_spread <- spread / ((ask + bid) / 2)
  rel_spread[is.nan(rel_spread) | is.infinite(rel_spread)] <- NA_real_

  data.table::set(DT, j = "bid_ask_spread", value = spread)
  data.table::set(DT, j = "bid_ask_spread_rel", value = rel_spread)
  invisible(DT)
}

#' Add Mid-Price Features
#'
#' Computes mid-price columns from bid and ask quotes in place.
#'
#' @param DT A `data.table` containing bid and ask columns.
#' @param bid_col Bid-price column name.
#' @param ask_col Ask-price column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_mid_price <- function(DT, bid_col = "bid", ask_col = "ask") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(bid_col, ask_col) %in% names(DT)))

  data.table::set(DT, j = "mid_price", value = (DT[[bid_col]] + DT[[ask_col]]) / 2)
  invisible(DT)
}

#' Add Microprice Features
#'
#' Computes microprice columns from bid/ask quotes and queue sizes in place.
#'
#' @param DT A `data.table` containing bid, ask, bid-size, and ask-size
#'   columns.
#' @param bid_col Bid-price column name.
#' @param ask_col Ask-price column name.
#' @param bid_size_col Bid-size column name.
#' @param ask_size_col Ask-size column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_microprice <- function(DT, bid_col = "bid", ask_col = "ask", bid_size_col = "bid_size", ask_size_col = "ask_size") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(bid_col, ask_col, bid_size_col, ask_size_col) %in% names(DT)))

  denom <- DT[[bid_size_col]] + DT[[ask_size_col]]
  out <- (DT[[ask_col]] * DT[[bid_size_col]] + DT[[bid_col]] * DT[[ask_size_col]]) / denom
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  data.table::set(DT, j = "microprice", value = out)
  invisible(DT)
}

#' Add Order-Imbalance Features
#'
#' Computes normalized order-imbalance columns from bid and ask queue sizes in
#' place.
#'
#' @param DT A `data.table` containing bid-size and ask-size columns.
#' @param bid_size_col Bid-size column name.
#' @param ask_size_col Ask-size column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_order_imbalance <- function(DT, bid_size_col = "bid_size", ask_size_col = "ask_size") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(bid_size_col, ask_size_col) %in% names(DT)))

  denom <- DT[[bid_size_col]] + DT[[ask_size_col]]
  out <- (DT[[bid_size_col]] - DT[[ask_size_col]]) / denom
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  data.table::set(DT, j = "order_imbalance", value = out)
  invisible(DT)
}

#' Add Turnover Features
#'
#' Computes traded-value turnover and optional float-normalized turnover ratio
#' in place.
#'
#' @param DT A `data.table` containing price and volume columns.
#' @param price_col Price column name.
#' @param volume_col Traded-volume column name.
#' @param float_col Optional float or shares-outstanding column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_turnover <- function(DT, price_col = "close", volume_col = "volume", float_col = NULL) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(price_col, volume_col) %in% names(DT)))

  traded_value <- DT[[price_col]] * DT[[volume_col]]
  data.table::set(DT, j = "turnover_value", value = traded_value)

  if (!is.null(float_col)) {
    stopifnot(float_col %in% names(DT))
    out <- DT[[volume_col]] / DT[[float_col]]
    out[is.nan(out) | is.infinite(out)] <- NA_real_
    data.table::set(DT, j = "turnover_ratio", value = out)
  }

  invisible(DT)
}

#' Add Slippage-Proxy Features
#'
#' Computes a simple execution slippage proxy from intrabar range and close
#' price in place.
#'
#' @param DT A `data.table` containing high, low, and close columns.
#' @param high_col High-price column name.
#' @param low_col Low-price column name.
#' @param close_col Close-price column name.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_slippage_proxy <- function(DT, high_col = "high", low_col = "low", close_col = "close") {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(high_col, low_col, close_col) %in% names(DT)))

  out <- (DT[[high_col]] - DT[[low_col]]) / DT[[close_col]]
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  data.table::set(DT, j = "slippage_proxy", value = out)
  invisible(DT)
}

#' Add Price-Impact Proxy Features
#'
#' Computes an Amihud-style price-impact proxy from absolute returns and dollar
#' volume in place.
#'
#' @param DT A `data.table` containing close and volume columns.
#' @param close_col Close-price column name.
#' @param volume_col Traded-volume column name.
#' @param use_log Logical; if `TRUE`, uses close-to-close log returns.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_price_impact_proxy <- function(DT, close_col = "close", volume_col = "volume", use_log = TRUE) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(close_col, volume_col) %in% names(DT)))

  close <- DT[[close_col]]
  ret <- rep(NA_real_, length(close))
  if (length(close) > 1) {
    if (use_log) {
      ret[2:length(close)] <- diff(log(close))
    } else {
      ret[2:length(close)] <- close[2:length(close)] / close[1:(length(close) - 1)] - 1
    }
  }

  dollar_volume <- close * DT[[volume_col]]
  out <- abs(ret) / dollar_volume
  out[is.nan(out) | is.infinite(out)] <- NA_real_
  data.table::set(DT, j = "price_impact_proxy", value = out)
  invisible(DT)
}

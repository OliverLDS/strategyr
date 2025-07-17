.new_order <- function() {
  data.table::data.table(
    type           = character(),
    pos            = character(),
    size           = numeric(),
    price          = numeric(),
    pricing_method = character(),
    trade_reason   = character()
  )  
}

#' Generate Take-Profit and Stop-Loss Orders
#'
#' Based on current unrealized PnL and ATR-based thresholds, this function decides
#' whether the current long or short positions should be closed due to TP or SL conditions.
#'
#' @param trade_state List containing current position sizes and PnL info.
#' @param public_info List of market data including latest close and ATR.
#' @param trade_pars List of SL/TP settings.
#'
#' @return A `data.table` of trade exit orders, or empty if no condition is met.
#' @export
generate_tp_sl_orders <- function(trade_state, public_info, trade_pars) {
  orders <- .new_order()
  
  wallet_balance <- trade_state$wallet_balance
  long_size <- trade_state$long_size
  avg_long_price <- trade_state$avg_long_price
  short_size <- trade_state$short_size
  avg_short_price <- trade_state$avg_short_price
  unrealized_pnl <- trade_state$unrealized_pnl
  pnl_ratio <- unrealized_pnl / wallet_balance # suppose only one net position at a time

  latest_close  <- public_info$latest_close
  atr_now <- if (!is.na(public_info$atr_14)) public_info$atr_14 else Inf
  
  risk_TP <- trade_pars$risk_TP
  risk_SL <- trade_pars$risk_SL
  pos_TP_atr_mult <- trade_pars$pos_TP_atr_mult
  pos_SL_atr_mult <- trade_pars$pos_SL_atr_mult
  
  if (long_size > 0) {
    price_diff_long <- latest_close - avg_long_price
    if (pnl_ratio > risk_TP || price_diff_long > pos_TP_atr_mult * atr_now) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "long", long_size, 0, "MARKET", "TP"
      )))
    } else if (pnl_ratio < risk_SL || price_diff_long < - pos_SL_atr_mult * atr_now) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "long", long_size, 0, "MARKET", "SL"
      )))
    }
  }
  
  if (short_size > 0) {
    price_diff_short <- avg_short_price - latest_close
    if (pnl_ratio > risk_TP || price_diff_short > pos_TP_atr_mult * atr_now) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "short", short_size, 0, "MARKET", "TP"
      )))
    } else if (pnl_ratio < risk_SL || price_diff_short < - pos_SL_atr_mult * atr_now) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "short", short_size, 0, "MARKET", "SL"
      )))
    }
  }

  return(orders)
}


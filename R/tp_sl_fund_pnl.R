.new_order <- function(type = character(), pos = character(), size = numeric(), price = numeric(), pricing_method = character(), trade_reason = character()) {
  data.table::data.table(
    type           = type,
    pos            = pos,
    size           = size,
    price          = price,
    pricing_method = pricing_method,
    trade_reason   = trade_reason
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
generate_tp_sl_orders <- function(trade_state, public_info, trade_pars, unit_per_contract, min_digit) {
  orders <- .new_order()
  
  eq_budget <- trade_state$eq_budget
  long_contracts <- trade_state$long_contracts
  long_avg_entry_price <- trade_state$long_avg_entry_price
  short_contracts <- trade_state$short_contracts
  short_avg_entry_price <- trade_state$short_avg_entry_price
  unrealized_pnl <- trade_state$unrealized_pnl
  pnl_ratio <- unrealized_pnl / eq_budget # suppose only one net position at a time

  latest_close  <- public_info$latest_close
  atr_pct <- public_info$atr_pct
  
  risk_TP <- trade_pars$risk_TP
  risk_SL <- trade_pars$risk_SL
  pos_TP_atr_mult <- trade_pars$pos_TP_atr_mult
  pos_SL_atr_mult <- trade_pars$pos_SL_atr_mult
  
  if (is.na(atr_pct)) {
    pct_TP <- 0.05
    pct_SL <- -0.02
  } else {
    pct_TP <- max(pos_TP_atr_mult*atr_pct, 0.05)
    pct_SL <- min(pos_SL_atr_mult*atr_pct, -0.02)
  }
  
  if (long_contracts > 0) {
    price_diff_long <- (latest_close - long_avg_entry_price)/long_avg_entry_price
    if (pnl_ratio > risk_TP || price_diff_long > pct_TP) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "long", long_contracts, 0, "MARKET", "TP"
      )))
    } else if (pnl_ratio < risk_SL || price_diff_long < pct_SL) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "long", long_contracts, 0, "MARKET", "SL"
      )))
    }
  }
  
  if (short_contracts > 0) {
    price_diff_short <- (short_avg_entry_price - latest_close)/short_avg_entry_price
    if (pnl_ratio > risk_TP || price_diff_short > pct_TP) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "short", short_contracts, 0, "MARKET", "TP"
      )))
    } else if (pnl_ratio < risk_SL || price_diff_short < pct_SL) {
      orders <- data.table::rbindlist(list(orders, list(
        "CLOSE", "short", short_contracts, 0, "MARKET", "SL"
      )))
    }
  }

  return(orders)
}


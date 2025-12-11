.gen_ord_tp_sl_fund_pnl_core <- function(inst_id, unit_per_contract, min_digit,
  eq_budget, long_contracts, long_avg_entry_price, short_contracts, short_avg_entry_price, unrealized_pnl,
  latest_close, atr_pct, 
  risk_TP, risk_SL, pos_TP_atr_mult, pos_SL_atr_mult,
  min_tp = 0.05, min_sl = 0.02) {
  
  orders <- .new_order()
  
  pnl_ratio <- unrealized_pnl / eq_budget # suppose only one net position at a time
  
  if (is.na(atr_pct)) {
    pct_TP <- min_tp
    pct_SL <- -min_sl
  } else {
    pct_TP <- max(pos_TP_atr_mult*atr_pct, min_tp)
    pct_SL <- min(pos_SL_atr_mult*atr_pct, -min_sl)
  }
  
  if (long_contracts > 0) {
    price_diff_long <- (latest_close - long_avg_entry_price)/long_avg_entry_price
    if (pnl_ratio > risk_TP || price_diff_long > pct_TP) {
      orders <- data.table::rbindlist(list(orders, .new_order(
        inst_id, "CLOSE", "long", long_contracts, 0, "MARKET", "TP"
      )))
    } else if (pnl_ratio < risk_SL || price_diff_long < pct_SL) {
      orders <- data.table::rbindlist(list(orders, .new_order(
        inst_id, "CLOSE", "long", long_contracts, 0, "MARKET", "SL"
      )))
    }
  }
  
  if (short_contracts > 0) {
    price_diff_short <- (short_avg_entry_price - latest_close)/short_avg_entry_price
    if (pnl_ratio > risk_TP || price_diff_short > pct_TP) {
      orders <- data.table::rbindlist(list(orders, .new_order(
        inst_id, "CLOSE", "short", short_contracts, 0, "MARKET", "TP"
      )))
    } else if (pnl_ratio < risk_SL || price_diff_short < pct_SL) {
      orders <- data.table::rbindlist(list(orders, .new_order(
        inst_id, "CLOSE", "short", short_contracts, 0, "MARKET", "SL"
      )))
    }
  }

  return(invisible(orders))
}

#' @export
gen_ord_tp_sl_fund_pnl <- function(trade_state, public_info, trade_pars, min_tp = 0.05, min_sl = 0.02) {
  
  .gen_ord_tp_sl_fund_pnl_core(
    inst_id = trade_state$inst_id, 
    unit_per_contract = trade_state$unit_per_contract, 
    min_digit = trade_state$min_digit,
    eq_budget = trade_state$eq_budget,
    long_contracts = trade_state$long_contracts,
    long_avg_entry = trade_state$long_avg_entry_price,
    short_contracts = trade_state$short_contracts,
    short_avg_entry = trade_state$short_avg_entry_price,
    unrealized_pnl = trade_state$unrealized_pnl,
    latest_close = public_info$latest_close,
    atr_pct = public_info$atr_pct,
    risk_TP = trade_pars$risk_TP,
    risk_SL = trade_pars$risk_SL,
    pos_TP_atr_mult = trade_pars$pos_TP_atr_mult,
    pos_SL_atr_mult = trade_pars$pos_SL_atr_mult,
    min_tp = min_tp,
    min_sl = min_sl
  )

}


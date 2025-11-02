.gen_ord_breakout_layers_core <- function(inst_id, unit_per_contract, min_digit,
  eq_budget, long_contracts, long_avg_entry_price, short_contracts, short_avg_entry_price,
  fast_ma, slow_ma, latest_close, high_prev, low_prev, atr_rising,
  lever, pos_pct, layer_mult
  ) {
  
  orders <- .new_order()
  
  long_notional <- long_contracts * unit_per_contract
  short_notional <- short_contracts * unit_per_contract
  
  if (anyNA(c(fast_ma, slow_ma, latest_close, high_prev, low_prev, atr_rising))) return(orders)
  
  side <- NA_character_
  layer_id <- NA_integer_
  
  if (atr_rising && fast_ma > slow_ma && latest_close > high_prev) {
    side <- "long"; layer_id <- 1
  } else if (fast_ma > slow_ma && latest_close > high_prev) {
    side <- "long"; layer_id <- 2
  } else if (atr_rising && fast_ma > slow_ma) {
    side <- "long"; layer_id <- 3
  } else if (fast_ma > slow_ma) {
    side <- "long"; layer_id <- 4
  } else if (atr_rising && fast_ma < slow_ma && latest_close < low_prev) {
    side <- "short"; layer_id <- 1
  } else if (fast_ma < slow_ma && latest_close < low_prev) {
    side <- "short"; layer_id <- 2
  } else if (atr_rising && fast_ma < slow_ma) {
    side <- "short"; layer_id <- 3
  } else if (fast_ma < slow_ma) {
    side <- "short"; layer_id <- 4
  } else {
    return(orders) # So side and layer_id can't be NA in remaining codes
  }
  
  # Exit logic
  if (short_contracts > 0 && side == 'long') {
    if (layer_id <= 2) {
      orders <- data.table::rbindlist(list(orders, .new_order(inst_id, "CLOSE", "short", short_contracts, 0, "MARKET", "exit_short_zbreak")))
    } else {
      return(orders)
    }
  }
  
  if (long_contracts > 0 && side == 'short') {
    if (layer_id <= 2) {
      orders <- data.table::rbindlist(list(orders, .new_order(inst_id, "CLOSE", "long", long_contracts, 0, "MARKET", "exit_long_zbreak")))
    } else {
      return(orders)
    }
  }

  # Adjusted position for new base size
  adj_long <- if (side == "short") 0 else long_notional
  adj_short <- if (side == "long") 0 else short_notional

  pos_size_notional <- (eq_budget * lever * pos_pct * layer_mult[layer_id] -
                adj_long * long_avg_entry_price -
                adj_short * short_avg_entry_price) / latest_close
  pos_size <- as.numeric(round(pos_size_notional / unit_per_contract, min_digit))
  if (pos_size <= 0) return(orders)
  label <- sprintf("breakout_layer%s_%s", layer_id, side)
  orders <- data.table::rbindlist(list(orders, .new_order(inst_id, "OPEN", side, pos_size, 0, "MARKET", label)))
  
  return(invisible(orders))
}


#' @export
gen_ord_breakout_layers <- function(trade_state, public_info, trade_pars) { 
  
  .gen_ord_breakout_layers_core(
    inst_id = trade_state$inst_id, 
    unit_per_contract = trade_state$unit_per_contract, 
    min_digit = trade_state$min_digit,
    eq_budget = trade_state$eq_budget,
    long_contracts = trade_state$long_contracts,
    long_avg_entry_price = trade_state$long_avg_entry_price,
    short_contracts = trade_state$short_contracts,
    short_avg_entry_price = trade_state$short_avg_entry_price,
    fast_ma = public_info$ema_20,
    slow_ma = public_info$ema_50,
    latest_close = public_info$latest_close,
    high_prev = public_info$high_max_25,
    low_prev = public_info$low_min_25,
    atr_rising = public_info$atr_rising,
    lever = trade_pars$leverage,
    pos_pct = trade_pars$breakout_pos_pct,
    layer_mult = c(trade_pars$breakout_layer1_mult, 
      trade_pars$breakout_layer2_mult, 
      trade_pars$breakout_layer3_mult, 
      trade_pars$breakout_layer4_mult)
  )
  
}

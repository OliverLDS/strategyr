

.gen_ord_dca_ladder_core <- function(inst_id, unit_per_contract, min_digit,
  eq_budget, long_contracts, short_contracts, 
  price_now, candle_DT, lev, 
  direction = NULL) {
  
  long_notional <- long_contracts * unit_per_contract
  short_notional <- short_contracts * unit_per_contract
  
  DT <- gen_ind_EMA(candle_DT)
  pivots <- get_now_pivots(DT, min_swing = 0.05)
  cycles <- get_now_cycles(pivots)
  fibs <- get_all_cycle_fibs(cycles, price_now, tail(DT$ema_20, 1), tail(DT$ema_50, 1), tail(DT$ema_100, 1))
  rs_points <- get_sr_from_fibs(fibs)
  position_ladder <- get_position_ladder_from_fibs(fibs, up_break_offset = 1)
  
  p <- position_ladder$entry_levels
  n <- length(p)
  if (price_now <= p[n] | price_now >= p[1]) return('breakout')
  idx_b_near <- n - findInterval(price_now, rev(p))
  idx_s_near <- idx_b_near + 1L 
  idx_b_all <- which(p > price_now)
  idx_s_all <- which(p < price_now)
  # idx_b_other  <- setdiff(idx_b_all, idx_b_near)
  # idx_s_other  <- setdiff(idx_s_all, idx_s_near)
  # all the adjusted orders will be placed in market mode; so we don't need to differentiate near and other levels
  
  orders <- .new_order()
  
  long_pos_sz <- long_contracts*unit_per_contract*price_now/lev/eq_budget
  short_pos_sz <- short_contracts*unit_per_contract*price_now/lev/eq_budget
  
  diff_long_size <- position_ladder$cum_long_size[idx_b_near] - long_pos_sz
  diff_short_size <- position_ladder$cum_short_size[idx_s_near] - short_pos_sz
  
  if (diff_long_size > 0) {
    orders <- rbind(orders,
      data.table::data.table(
        inst_id = inst_id, type = 'OPEN', pos = 'long',
        size = round(diff_long_size*lev*eq_budget/price_now/unit_per_contract, min_digit),
        price = NA_real_, pricing_method = 'market', trade_reason = 'main_cycle_adjust'
      )
    )
  } else if (diff_long_size < 0) {
    orders <- rbind(orders,
      data.table::data.table(
        inst_id = inst_id, type = 'CLOSE', pos = 'long',
        size = round(-diff_long_size*lev*eq_budget/price_now/unit_per_contract, min_digit),
        price = NA_real_, pricing_method = 'market', trade_reason = 'main_cycle_adjust'
      )
    )
  }
  
  if (diff_short_size > 0) {
    orders <- rbind(orders,
      data.table::data.table(
        inst_id = inst_id, type = 'OPEN', pos = 'short',
        size = round(diff_short_size*lev*eq_budget/price_now/unit_per_contract, min_digit),
        price = NA_real_, pricing_method = 'market', trade_reason = 'main_cycle_adjust'
      )
    )
  } else if (diff_short_size < 0) {
    orders <- rbind(orders,
      data.table::data.table(
        inst_id = inst_id, type = 'CLOSE', pos = 'short',
        size = round(-diff_short_size*lev*eq_budget/price_now/unit_per_contract, min_digit),
        price = NA_real_, pricing_method = 'market', trade_reason = 'main_cycle_adjust'
      )
    )
  }
  
  if (length(idx_s_all)>0) {
    orders <- rbind(orders,
      data.table::data.table(
        inst_id = inst_id, type = 'OPEN', pos = 'long',
        size = round(position_ladder$long_size[idx_s_all]*lev*eq_budget/p[idx_s_all]/unit_per_contract, min_digit),
        price = p[idx_s_all], pricing_method = 'limit', trade_reason = 'main_cycle_place'
      )
    )
  }
  
  if (length(idx_b_all)>0) {
    orders <- rbind(orders,
      data.table::data.table(
        inst_id = inst_id, type = 'OPEN', pos = 'short',
        size = round(position_ladder$short_size[idx_b_all]*lev*eq_budget/p[idx_b_all]/unit_per_contract, min_digit),
        price = p[idx_b_all], pricing_method = 'limit', trade_reason = 'main_cycle_place'
      )
    )
  }
  
  list(
    position_ladder = position_ladder,
    orders = orders
  )
  
  # res <- get_now_fib_trade_points(candle_DT, latest_close, latest_n = 500L)
  # return(invisible(orders))
  
}

# the step_in and step_out weights should be independent from current mark price
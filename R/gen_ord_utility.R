.new_order <- function(inst_id = character(), type = character(), pos = character(), size = numeric(), price = numeric(), pricing_method = character(), trade_reason = character()) {
  data.table::data.table(
    inst_id = inst_id,
    type = type, # OPEN or CLOSE
    pos = pos, # long or short
    size = size,
    price = price,
    pricing_method = pricing_method, # market or limit
    trade_reason = trade_reason
  )  
}

#' @export
get_now_pivots <- function(DT, span = 3, latest_n = NULL, refined = TRUE, min_swing = 0.05) {
  pivots <- data.table::as.data.table(get_now_pivots_cpp(DT$high, DT$low, DT$datetime, span, latest_n, refined, min_swing))
  pivots[, datetime:=as.POSIXct(datetime, origin='1970-01-01')]
  return(invisible(pivots))
}

#' @export
get_now_cycles <- function(pivots, min_backward_bars = 360, min_stable_pivots = 7) {
  invisible(get_now_cycles_cpp(pivots, min_backward_bars, min_stable_pivots))
}

.label_cycles <- function(cycles, price_now) {
  # expects columns: bg_price, ed_price
  if (!data.table::is.data.table(cycles)) data.table::setDT(cycles)
  stopifnot(all(c("bg_price","ed_price") %in% names(cycles)))
  
  cycles[, contains_now := fifelse((price_now >= bg_price & price_now <= ed_price) | (price_now <= bg_price & price_now >= ed_price), TRUE, FALSE)]
  if (any(cycles$contains_now)) {cycles <- cycles[contains_now ==TRUE,]}
  cycles[, cycle_id := .I]
  cycles[, fresh := fifelse(.I == .N, TRUE, FALSE)]
  cycles[, main := fifelse(.I == 1, TRUE, FALSE)]

  invisible(cycles)
}

.check_proximity <- function(base, value, threshold = 0.002) {
  invisible(abs(value/base-1) < threshold)
}

#' @export
get_all_cycle_fibs <- function(cycles, price_now, ema20, ema50, ema100) {

  cycles <- .label_cycles(cycles, price_now)

  fib_list <- lapply(seq_len(nrow(cycles)), function(i) {
    pts <- fib_all_cpp(cycles$bg_price[i], cycles$ed_price[i])$points
    data.table::setDT(pts)[
      , `:=`(cycle_id = cycles$cycle_id[i], cycle_fresh = cycles$fresh[i], cycle_main = cycles$main[i], cycle_dir = cycles$direction[i])]
  })

  fibs <- data.table::rbindlist(fib_list, use.names = TRUE, fill = TRUE)

  fibs[, sr := data.table::fifelse(level < price_now, "support", "resistance")]
  fibs[, ema20 := data.table::fifelse(.check_proximity(ema20, level), TRUE, FALSE)]
  fibs[, ema50 := data.table::fifelse(.check_proximity(ema50, level), TRUE, FALSE)]
  fibs[, ema100 := data.table::fifelse(.check_proximity(ema100, level), TRUE, FALSE)]
  fibs[, flipped := data.table::fifelse(sr_hint == sr, TRUE, FALSE)]

  fibs[, score := 0]
  fibs[, score := score + data.table::fifelse(ema20 | ema50 | ema100, 3^7, 0)]
  fibs[, score := score + data.table::fifelse(cycle_fresh & group == 'retr', 3^6, 0)]
  fibs[, score := score + data.table::fifelse(cycle_main & group == 'retr', 3^5, 0)]
  fibs[, score := score + data.table::fifelse(cycle_fresh & group != 'retr', 3^4, 0)]
  fibs[, score := score + data.table::fifelse(cycle_main & group != 'retr', 3^3, 0)]
  fibs[, score := score + data.table::fifelse(label %in% c('61.8%', '38.2%'), 3^2, 0)]
  fibs[, score := score + data.table::fifelse(flipped, 3^1, 0)]
  
  data.table::setorder(fibs, -level)
  invisible(fibs)
}

#' @export
get_sr_from_fibs <- function(fibs) {
  resistance_pts <- fibs[sr == 'resistance',][order(-score),][1:3][order(level),]$level
  support_pts <- fibs[sr == 'support',][order(-score),][1:3][order(-level),]$level
  
  invisible(
    data.table::data.table(
      r1 = resistance_pts[1],
      r2 = resistance_pts[2],
      r3 = resistance_pts[3],
      s1 = support_pts[1],
      s2 = support_pts[2],
      s3 = support_pts[3]
    )
  )
}

# we can generate fib_dt and top 3 support and resistance zones here, for plotting purpose
# more important is to infer swing direction from main cycle (basically, 0.618 of the main cycle is a good place to bet reversal)
# 6.18/ema conguent is for point score to place event order

#' @export
get_position_ladder_from_fibs <- function(core_levels, lev = 10, up_break_offset = 0, down_break_offset = 0) {
  
  entry_levels <- core_levels[7:12]
  
  w_long  <- c(0, 0, 0.1, 0.2, 0.3, 0.4)
  w_short <- c(0.4, 0.3, 0.2, 0.1, 0, 0)
  
  long_weighted_price_tier  <- sum(entry_levels * w_long)
  short_weighted_price_tier <- sum(entry_levels * w_short)
  
  long_liq_ref <- core_levels[14 + down_break_offset]
  short_liq_ref <- core_levels[5 - up_break_offset]
  
  long_tier_max_size  <- 1 / (lev * (1 - long_liq_ref / long_weighted_price_tier))
  short_tier_max_size <- 1 / (lev * (1 - short_weighted_price_tier / short_liq_ref))
  
  long_size  <- long_tier_max_size  * w_long
  short_size <- short_tier_max_size * w_short
  
  cum_long_size  <- cumsum(long_size)
  cum_short_size <- rev(cumsum(rev(short_size)))
  
  avg_long_price <- cumsum(entry_levels * long_size) / cum_long_size
  avg_short_price <- rev(cumsum(rev(entry_levels * short_size))) / cum_short_size
  
  long_liq_price <- (1 - 1/lev/cum_long_size) * avg_long_price
  short_liq_price <- avg_short_price / (1 - 1/lev/cum_short_size)
  
  data.table::data.table(
    entry_levels = entry_levels,
    long_size = long_size,
    short_size = short_size,
    cum_long_size = cum_long_size,
    cum_short_size = cum_short_size,
    avg_long_price = avg_long_price,
    long_liq_price = long_liq_price,
    avg_short_price = avg_short_price,
    short_liq_price = short_liq_price
  )
}

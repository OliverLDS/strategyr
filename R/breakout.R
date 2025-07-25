#' Breakout Strategy Signal Generator (4-Layer)
#'
#' This function implements a 4-layer breakout strategy that evaluates market conditions
#' using moving averages, volatility (ATR trend), and previous highs/lows. It determines
#' if a new position should be opened or an existing one reversed or closed, based on
#' signal strength. Order size is scaled according to breakout layer confidence.
#'
#' @param trade_state A named list containing the current trading state:
#'   \describe{
#'     \item{wallet_balance}{Total wallet value in quote currency (e.g., USDT).}
#'     \item{long_size}{Current size of long position.}
#'     \item{avg_long_price}{Average entry price of the long position.}
#'     \item{short_size}{Current size of short position.}
#'     \item{avg_short_price}{Average entry price of the short position.}
#'   }
#' 
#' @param public_info A named list with current market indicators:
#'   \describe{
#'     \item{ema_20}{Fast EMA (e.g., 20-period).}
#'     \item{ema_50}{Slow EMA (e.g., 50-period).}
#'     \item{latest_close}{Most recent candle close price.}
#'     \item{high_max_25}{25-period rolling high.}
#'     \item{low_min_25}{25-period rolling low.}
#'     \item{atr_rising}{Logical; TRUE if ATR is rising (indicates volatility).}
#'   }
#' 
#' @param trade_pars A named list of strategy parameters:
#'   \describe{
#'     \item{leverage}{Leverage multiplier for position sizing.}
#'     \item{breakout_pos_pct}{Base position percentage (of wallet balance) for sizing.}
#'     \item{breakout_layer1_mult}{Multiplier for Layer 1 (strongest signal).}
#'     \item{breakout_layer2_mult}{Multiplier for Layer 2.}
#'     \item{breakout_layer3_mult}{Multiplier for Layer 3.}
#'     \item{breakout_layer4_mult}{Multiplier for Layer 4 (weakest signal).}
#'   }
#'
#' @return A `data.table` of one or more trade orders. Each row contains:
#'   \describe{
#'     \item{action}{"OPEN" or "CLOSE"}
#'     \item{side}{"long" or "short"}
#'     \item{size}{Order size (positive numeric).}
#'     \item{price}{Set to 0 for market orders.}
#'     \item{type}{"MARKET"}
#'     \item{label}{String identifying the order source, e.g., "breakout_layer2_long"}
#'   }
#'
#' @examples
#' trade_state <- list(wallet_balance = 1000, long_size = 0, avg_long_price = 0,
#'                     short_size = 0, avg_short_price = 0)
#' public_info <- list(ema_20 = 101, ema_50 = 100, latest_close = 102,
#'                     high_max_25 = 101.5, low_min_25 = 98, atr_rising = TRUE)
#' trade_pars <- list(leverage = 5, breakout_pos_pct = 0.2,
#'                    breakout_layer1_mult = 1.0, breakout_layer2_mult = 0.7,
#'                    breakout_layer3_mult = 0.5, breakout_layer4_mult = 0.3)
#' breakout_v1(trade_state, public_info, trade_pars)
#'
#' @export
breakout_v1 <- function(trade_state, public_info, trade_pars, unit_per_contract, min_digit) { 
  
  orders <- .new_order()
  
  eq_budget <- trade_state$eq_budget
  long_contracts <- trade_state$long_contracts
  long_avg_entry_price <- trade_state$long_avg_entry_price
  short_contracts <- trade_state$short_contracts
  short_avg_entry_price <- trade_state$short_avg_entry_price
  unrealized_pnl <- trade_state$unrealized_pnl
  
  long_notional <- long_contracts * unit_per_contract
  short_notional <- short_contracts * unit_per_contract
  
  fast_ma <- public_info$ema_20
  slow_ma <- public_info$ema_50
  latest_close  <- public_info$latest_close
  high_prev <- public_info$high_max_25
  low_prev  <- public_info$low_min_25
  atr_rising <- public_info$atr_rising
  
  lever <- trade_pars$leverage
  pos_pct <- trade_pars$breakout_pos_pct
  layer_mult <- c(trade_pars$breakout_layer1_mult,
                  trade_pars$breakout_layer2_mult,
                  trade_pars$breakout_layer3_mult,
                  trade_pars$breakout_layer4_mult)
  
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
      orders <- data.table::rbindlist(list(orders, .new_order("CLOSE", "short", short_contracts, 0, "MARKET", "exit_short_zbreak")))
    } else {
      return(orders)
    }
  }
  
  if (long_contracts > 0 && side == 'short') {
    if (layer_id <= 2) {
      orders <- data.table::rbindlist(list(orders, .new_order("CLOSE", "short", short_contracts, 0, "MARKET", "exit_short_zbreak")))
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
  pos_size <- round(pos_size_notional / unit_per_contract, min_digit)
  if (pos_size <= 0) return(orders)
  label <- sprintf("breakout_layer%s_%s", layer_id, side)
  orders <- data.table::rbindlist(list(orders, .new_order("OPEN", side, pos_size, 0, "MARKET", label)))
  
  return(orders)
}

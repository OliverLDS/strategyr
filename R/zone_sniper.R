#' Zone Sniper Entry Strategy (ATR+RSI Near Zones)
#'
#' Places long or short entries when price nears key support/resistance levels
#' (either quantile-based or pivot-based) with confirmation from RSI. Tolerance
#' is scaled by ATR. This strategy aims to fade price near strong zones.
#'
#' @param trade_state A named list containing wallet balance and position info:
#'   \describe{
#'     \item{wallet_balance}{Total account balance.}
#'     \item{long_size}{Current long position size.}
#'     \item{avg_long_price}{Average price of long entry.}
#'     \item{short_size}{Current short position size.}
#'     \item{avg_short_price}{Average price of short entry.}
#'   }
#'
#' @param public_info A named list of market context data:
#'   \describe{
#'     \item{support_strong_qt_d}{Quantile-based support level.}
#'     \item{support_1}{Pivot-based support level.}
#'     \item{resistance_strong_qt_d}{Quantile-based resistance level.}
#'     \item{resistance_1}{Pivot-based resistance level.}
#'     \item{latest_close}{Current close price.}
#'     \item{rsi_14}{14-period RSI value.}
#'     \item{atr_14}{14-period ATR value.}
#'   }
#'
#' @param trade_pars A named list of strategy parameters:
#'   \describe{
#'     \item{leverage}{Leverage used to calculate position size.}
#'     \item{zsniper_pos_pct}{Position size as percentage of wallet.}
#'     \item{zsniper_tol_atr}{Tolerance as multiple of ATR to qualify proximity.}
#'   }
#'
#' @return A `data.table` of entry orders near zone boundaries if conditions are met.
#' @export
zone_sniper_v1 <- function(trade_state, public_info, trade_pars) {
  
  orders <- .new_order()
  
  wallet_balance <- trade_state$wallet_balance
  long_size <- trade_state$long_size
  avg_long_price <- trade_state$avg_long_price
  short_size <- trade_state$short_size
  avg_short_price <- trade_state$avg_short_price
  
  support_strong_qt_d <- public_info$support_strong_qt_d
  support_1 <- public_info$support_1
  resistance_strong_qt_d <- public_info$resistance_strong_qt_d
  resistance_1 <- public_info$resistance_1
  latest_close <- public_info$latest_close
  rsi_14 <- public_info$rsi_14
  atr_14 <- public_info$atr_14
  
  lever <- trade_pars$leverage
  tol <- trade_pars$zsniper_tol_atr * atr_14
  pos_pct <- trade_pars$zsniper_pos_pct
  
  is_overbought <- function(rsi, thr = 70) !is.na(rsi) && rsi > thr
  is_oversold   <- function(rsi, thr = 30) !is.na(rsi) && rsi < thr
  
  if (is.na(latest_close) || is.na(rsi_14) || is.na(atr_14)) {return(orders)}
  
  pos_size <- (wallet_balance * lever * pos_pct - long_size * avg_long_price - short_size * avg_short_price) / latest_close
  if (pos_size <= 0) {return(orders)}

  if (!is.na(support_strong_qt_d)) {
    # LONG off dynamic quantile *or* pivot support
    if (abs(latest_close - support_strong_qt_d) < tol &&
        is_oversold(rsi_14)) {
      orders <- rbindlist(list(orders, list(
        "OPEN", "long", pos_size, 0, "MARKET",
        "zone_sniper_long"
      )))
    }
  }

  if (!is.na(support_1)) {
    if (abs(latest_close - support_1) < tol &&
        is_oversold(rsi_14)) {
      orders <- rbindlist(list(orders, list(
        "OPEN", "long", pos_size, 0, "MARKET",
        "zone_sniper_long_pivot"
      )))
    }
  }

  if (!is.na(resistance_strong_qt_d)) {
    # SHORT off resistance
    if (abs(latest_close - resistance_strong_qt_d) < tol &&
        is_overbought(rsi_14)) {
  
      orders <- rbindlist(list(orders, list(
        "OPEN", "short", pos_size, 0, "MARKET",
        "zone_sniper_short"
      )))
    }
  }

  if (!is.na(resistance_1)) {
    if (abs(latest_close - resistance_1) < tol &&
        is_overbought(rsi_14)) {
  
      orders <- rbindlist(list(orders, list(
        "OPEN", "short", pos_size, 0, "MARKET",
        "zone_sniper_short_pivot"
      )))
    }
  }

  return(orders)
}
.ensure_rsi_trend_aware_revert_features <- function(DT, rsi_n, trend_n) {
  .validate_market_dt(DT, "close")

  rsi_col <- paste0("rsi_", rsi_n)
  ema_col <- paste0("ema_", trend_n)
  if (!rsi_col %in% names(DT)) {
    calc_RSI(DT, ns = rsi_n, hs = NULL)
  }
  if (!ema_col %in% names(DT)) {
    calc_EMA(DT, ns = trend_n)
  }

  invisible(c("close", rsi_col, ema_col))
}

.rsi_trend_aware_revert_signal <- function(close, ema_value, rsi, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0) {
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(ema_value[i]) || is.na(rsi[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && (rsi[i] >= exit_level || close[i] < ema_value[i])) {
      pos_now <- 0.0
    } else if (pos_now < 0 && (rsi[i] <= exit_level || close[i] > ema_value[i])) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (close[i] > ema_value[i] && rsi[i] <= oversold) {
        pos_now <- target_size
      } else if (close[i] < ema_value[i] && rsi[i] >= overbought) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' RSI-Trend-Aware-Reversion Target Positions
#'
#' Generates a target-position path from RSI mean-reversion signals that are
#' conditioned on trend direction. Oversold longs are only allowed in uptrends,
#' while overbought shorts are only allowed in downtrends.
#'
#' @param DT A candle `data.table`.
#' @param rsi_n Integer RSI window.
#' @param trend_n Integer EMA window used as the trend filter.
#' @param oversold Numeric RSI threshold used for long entries.
#' @param overbought Numeric RSI threshold used for short entries.
#' @param exit_level Numeric RSI level used to exit open positions.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing RSI and EMA features
#'   are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_rsi_trend_aware_revert_tgt_pos <- function(DT, rsi_n = 14L, trend_n = 50L, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_rsi_trend_aware_revert_features(DT, rsi_n = rsi_n, trend_n = trend_n)
  } else {
    cols_needed <- c("close", paste0("rsi_", rsi_n), paste0("ema_", trend_n))
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .rsi_trend_aware_revert_signal(
    close = DT[["close"]],
    ema_value = DT[[paste0("ema_", trend_n)]],
    rsi = DT[[paste0("rsi_", rsi_n)]],
    oversold = oversold,
    overbought = overbought,
    exit_level = exit_level,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' RSI-Trend-Aware-Reversion Action Plan
#'
#' Applies the trend-aware RSI-reversion rule to the latest bar and translates
#' the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_rsi_trend_aware_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_rsi_trend_aware_revert_action_plan <- function(DT, state, rsi_n = 14L, trend_n = 50L, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0, compute_features = TRUE, strat_id = 311L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_rsi_trend_aware_revert_tgt_pos(
    DT,
    rsi_n = rsi_n,
    trend_n = trend_n,
    oversold = oversold,
    overbought = overbought,
    exit_level = exit_level,
    target_size = target_size,
    compute_features = compute_features,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

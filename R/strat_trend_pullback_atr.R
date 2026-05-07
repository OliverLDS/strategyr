.ensure_trend_pullback_atr_features <- function(DT, trend_n, rsi_n, atr_n) {
  .validate_market_dt(DT, c("high", "low", "close"))

  ema_col <- paste0("ema_", trend_n)
  rsi_col <- paste0("rsi_", rsi_n)
  atr_col <- paste0("atr_", atr_n)

  if (!ema_col %in% names(DT)) {
    calc_EMA(DT, ns = trend_n)
  }
  if (!rsi_col %in% names(DT)) {
    calc_RSI(DT, ns = rsi_n, hs = NULL)
  }
  if (!atr_col %in% names(DT)) {
    calc_ATR(DT, ns = atr_n, hs = NULL)
  }

  invisible(c(ema_col, rsi_col, atr_col))
}

.trend_pullback_atr_signal <- function(close, ema_value, rsi, atr_value, pullback_long = 40, pullback_short = 60, exit_rsi = 50, min_atr_pullback = 0.5, max_atr_pullback = 3.0, target_size = 1.0) {
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(ema_value[i]) || is.na(rsi[i]) || is.na(atr_value[i]) || atr_value[i] <= 0) {
      out[i] <- pos_now
      next
    }

    trend_dir <- sign(close[i] - ema_value[i])
    pullback_dist <- abs(close[i] - ema_value[i]) / atr_value[i]
    in_zone <- pullback_dist >= min_atr_pullback && pullback_dist <= max_atr_pullback

    if (pos_now > 0 && (trend_dir <= 0 || rsi[i] >= exit_rsi)) {
      pos_now <- 0.0
    } else if (pos_now < 0 && (trend_dir >= 0 || rsi[i] <= exit_rsi)) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0 && in_zone) {
      if (trend_dir > 0 && rsi[i] <= pullback_long) {
        pos_now <- target_size
      } else if (trend_dir < 0 && rsi[i] >= pullback_short) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Trend-Pullback-ATR Target Positions
#'
#' Generates a trend-following pullback target-position path that requires the
#' pullback to reach a minimum ATR distance from the EMA trend anchor.
#'
#' @param DT A candle `data.table`.
#' @param trend_n Integer EMA window used for the trend filter.
#' @param rsi_n Integer RSI window.
#' @param atr_n Integer ATR window.
#' @param pullback_long Numeric RSI threshold used to enter long pullbacks in
#'   an uptrend.
#' @param pullback_short Numeric RSI threshold used to enter short pullbacks in
#'   a downtrend.
#' @param exit_rsi Numeric RSI level used to exit pullback trades.
#' @param min_atr_pullback Numeric minimum ATR pullback distance.
#' @param max_atr_pullback Numeric maximum ATR pullback distance.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing EMA, RSI, and ATR
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_trend_pullback_atr_tgt_pos <- function(DT, trend_n = 20L, rsi_n = 14L, atr_n = 14L, pullback_long = 40, pullback_short = 60, exit_rsi = 50, min_atr_pullback = 0.5, max_atr_pullback = 3.0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_trend_pullback_atr_features(DT, trend_n = trend_n, rsi_n = rsi_n, atr_n = atr_n)
  } else {
    cols_needed <- c(
      "close",
      paste0("ema_", trend_n),
      paste0("rsi_", rsi_n),
      paste0("atr_", atr_n)
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .trend_pullback_atr_signal(
    close = DT[["close"]],
    ema_value = DT[[paste0("ema_", trend_n)]],
    rsi = DT[[paste0("rsi_", rsi_n)]],
    atr_value = DT[[paste0("atr_", atr_n)]],
    pullback_long = pullback_long,
    pullback_short = pullback_short,
    exit_rsi = exit_rsi,
    min_atr_pullback = min_atr_pullback,
    max_atr_pullback = max_atr_pullback,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Trend-Pullback-ATR Action Plan
#'
#' Applies the trend-pullback-plus-ATR-zone rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_trend_pullback_atr_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_trend_pullback_atr_action_plan <- function(DT, state, trend_n = 20L, rsi_n = 14L, atr_n = 14L, pullback_long = 40, pullback_short = 60, exit_rsi = 50, min_atr_pullback = 0.5, max_atr_pullback = 3.0, target_size = 1.0, compute_features = TRUE, strat_id = 404L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_trend_pullback_atr_tgt_pos(
    DT,
    trend_n = trend_n,
    rsi_n = rsi_n,
    atr_n = atr_n,
    pullback_long = pullback_long,
    pullback_short = pullback_short,
    exit_rsi = exit_rsi,
    min_atr_pullback = min_atr_pullback,
    max_atr_pullback = max_atr_pullback,
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

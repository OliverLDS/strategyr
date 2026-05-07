.ensure_bollinger_revert_rsi_features <- function(DT, bb_n, bb_k, rsi_n) {
  .validate_market_dt(DT, "close")

  k_tag <- .suffix_num(bb_k)
  cols_needed <- c(
    paste0("bb_mid_", bb_n),
    paste0("bb_high_", bb_n, "_", k_tag),
    paste0("bb_low_", bb_n, "_", k_tag),
    paste0("rsi_", rsi_n)
  )

  if (!all(cols_needed[1:3] %in% names(DT))) {
    calc_BollingerBands(DT, ns = bb_n, ks = bb_k)
  }
  if (!cols_needed[4] %in% names(DT)) {
    calc_RSI(DT, ns = rsi_n, hs = NULL)
  }

  invisible(cols_needed)
}

.bollinger_revert_rsi_signal <- function(close, bb_mid, bb_high, bb_low, rsi, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0) {
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(bb_mid[i]) || is.na(bb_high[i]) || is.na(bb_low[i]) || is.na(rsi[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && (close[i] >= bb_mid[i] || rsi[i] >= exit_level)) {
      pos_now <- 0.0
    } else if (pos_now < 0 && (close[i] <= bb_mid[i] || rsi[i] <= exit_level)) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (close[i] <= bb_low[i] && rsi[i] <= oversold) {
        pos_now <- target_size
      } else if (close[i] >= bb_high[i] && rsi[i] >= overbought) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Bollinger-Reversion-RSI Target Positions
#'
#' Generates a Bollinger-band mean-reversion target-position path that requires
#' RSI confirmation before opening long or short reversion trades.
#'
#' @param DT A candle `data.table`.
#' @param bb_n Integer Bollinger window.
#' @param bb_k Numeric Bollinger width multiplier.
#' @param rsi_n Integer RSI window.
#' @param oversold Numeric RSI threshold used to confirm long entries.
#' @param overbought Numeric RSI threshold used to confirm short entries.
#' @param exit_level Numeric RSI level used as a secondary exit trigger.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing Bollinger and RSI
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_bollinger_revert_rsi_tgt_pos <- function(DT, bb_n = 20L, bb_k = 2, rsi_n = 14L, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  k_tag <- .suffix_num(bb_k)
  if (compute_features) {
    cols_needed <- .ensure_bollinger_revert_rsi_features(DT, bb_n = bb_n, bb_k = bb_k, rsi_n = rsi_n)
  } else {
    cols_needed <- c(
      "close",
      paste0("bb_mid_", bb_n),
      paste0("bb_high_", bb_n, "_", k_tag),
      paste0("bb_low_", bb_n, "_", k_tag),
      paste0("rsi_", rsi_n)
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .bollinger_revert_rsi_signal(
    close = DT[["close"]],
    bb_mid = DT[[paste0("bb_mid_", bb_n)]],
    bb_high = DT[[paste0("bb_high_", bb_n, "_", k_tag)]],
    bb_low = DT[[paste0("bb_low_", bb_n, "_", k_tag)]],
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

#' Bollinger-Reversion-RSI Action Plan
#'
#' Applies the Bollinger-band-plus-RSI mean-reversion rule to the latest bar
#' and translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_bollinger_revert_rsi_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_bollinger_revert_rsi_action_plan <- function(DT, state, bb_n = 20L, bb_k = 2, rsi_n = 14L, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0, compute_features = TRUE, strat_id = 308L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_bollinger_revert_rsi_tgt_pos(
    DT,
    bb_n = bb_n,
    bb_k = bb_k,
    rsi_n = rsi_n,
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

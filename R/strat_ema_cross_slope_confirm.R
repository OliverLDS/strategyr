.ensure_ema_cross_slope_confirm_features <- function(DT, fast, slow) {
  .validate_market_dt(DT, "close")

  cols_needed <- paste0("ema_", c(fast, slow))
  if (!all(cols_needed %in% names(DT))) {
    calc_EMA(DT, ns = sort(unique(c(fast, slow))))
  }

  invisible(cols_needed)
}

.ema_cross_slope_confirm_signal <- function(ema_fast, ema_slow, slope_lag = 1L, target_size = 1.0) {
  fast_slope <- ema_fast - .lag_num(ema_fast, slope_lag)
  slow_slope <- ema_slow - .lag_num(ema_slow, slope_lag)
  out <- rep(0.0, length(ema_fast))
  valid <- !is.na(ema_fast) & !is.na(ema_slow) & !is.na(fast_slope) & !is.na(slow_slope)
  out[valid & ema_fast > ema_slow & fast_slope > 0 & slow_slope > 0] <- target_size
  out[valid & ema_fast < ema_slow & fast_slope < 0 & slow_slope < 0] <- -target_size
  out
}

#' EMA-Cross-Slope-Confirm Target Positions
#'
#' Generates a target-position path from EMA alignment and EMA slope
#' confirmation. A long target requires the fast EMA above the slow EMA and both
#' EMAs sloping upward, while a short target requires the reverse.
#'
#' @param DT A candle `data.table`.
#' @param fast Integer fast EMA length.
#' @param slow Integer slow EMA length.
#' @param slope_lag Integer lag used to estimate EMA slope.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing EMA features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_ema_cross_slope_confirm_tgt_pos <- function(DT, fast = 20L, slow = 50L, slope_lag = 1L, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_ema_cross_slope_confirm_features(DT, fast = fast, slow = slow)
  } else {
    cols_needed <- paste0("ema_", c(fast, slow))
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .ema_cross_slope_confirm_signal(
    ema_fast = DT[[paste0("ema_", fast)]],
    ema_slow = DT[[paste0("ema_", slow)]],
    slope_lag = slope_lag,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' EMA-Cross-Slope-Confirm Action Plan
#'
#' Applies the EMA-cross-plus-slope-confirmation rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_ema_cross_slope_confirm_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ema_cross_slope_confirm_action_plan <- function(DT, state, fast = 20L, slow = 50L, slope_lag = 1L, target_size = 1.0, compute_features = TRUE, strat_id = 104L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ema_cross_slope_confirm_tgt_pos(
    DT,
    fast = fast,
    slow = slow,
    slope_lag = slope_lag,
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

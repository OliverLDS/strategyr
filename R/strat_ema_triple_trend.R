.ensure_ema_triple_trend_features <- function(DT, fast, mid, slow) {
  .validate_market_dt(DT, "close")

  cols_needed <- paste0("ema_", c(fast, mid, slow))
  if (!all(cols_needed %in% names(DT))) {
    calc_EMA(DT, ns = sort(unique(c(fast, mid, slow))))
  }

  invisible(cols_needed)
}

.ema_triple_trend_signal <- function(ema_fast, ema_mid, ema_slow, target_size = 1.0) {
  out <- rep(0.0, length(ema_fast))
  valid <- !is.na(ema_fast) & !is.na(ema_mid) & !is.na(ema_slow)
  out[valid & ema_fast > ema_mid & ema_mid > ema_slow] <- target_size
  out[valid & ema_fast < ema_mid & ema_mid < ema_slow] <- -target_size
  out
}

#' EMA-Triple-Trend Target Positions
#'
#' Generates a simple target-position path from triple-EMA alignment. Long
#' exposure is targeted when the fast EMA is above the medium EMA and the medium
#' EMA is above the slow EMA. Short exposure is targeted for the reverse
#' ordering.
#'
#' @param DT A candle `data.table`.
#' @param fast Integer fast EMA window.
#' @param mid Integer medium EMA window.
#' @param slow Integer slow EMA window.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing EMA features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_ema_triple_trend_tgt_pos <- function(DT, fast = 20L, mid = 50L, slow = 100L, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_ema_triple_trend_features(DT, fast = fast, mid = mid, slow = slow)
  } else {
    cols_needed <- paste0("ema_", c(fast, mid, slow))
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .ema_triple_trend_signal(
    ema_fast = DT[[paste0("ema_", fast)]],
    ema_mid = DT[[paste0("ema_", mid)]],
    ema_slow = DT[[paste0("ema_", slow)]],
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' EMA-Triple-Trend Action Plan
#'
#' Applies the triple-EMA trend rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_ema_triple_trend_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ema_triple_trend_action_plan <- function(DT, state, fast = 20L, mid = 50L, slow = 100L, target_size = 1.0, compute_features = TRUE, strat_id = 106L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ema_triple_trend_tgt_pos(
    DT,
    fast = fast,
    mid = mid,
    slow = slow,
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

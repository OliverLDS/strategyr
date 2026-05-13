.ensure_atr_breakout_trailing_stop_features <- function(DT, n) {
  .ensure_atr_breakout_features(DT, n = n)
}

.atr_breakout_trailing_stop_signal <- function(close, atr, atr_mult = 1, trail_mult = 2, target_size = 1.0) {
  strat_atr_breakout_trailing_stop_signal_cpp(
    close = close,
    atr = atr,
    atr_mult = atr_mult,
    trail_mult = trail_mult,
    target_size = target_size
  )
}

#' ATR-Breakout-Trailing-Stop Target Positions
#'
#' Generates a breakout target-position path from ATR moves with an ATR-based
#' trailing stop. Entries use the current ATR breakout rule, and exits trail the
#' most favorable close by a multiple of current ATR.
#'
#' @param DT A candle `data.table`.
#' @param n Integer ATR window.
#' @param atr_mult Numeric ATR multiple used for the breakout threshold.
#' @param trail_mult Numeric ATR multiple used for the trailing stop.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing ATR features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_atr_breakout_trailing_stop_tgt_pos <- function(DT, n = 14L, atr_mult = 1, trail_mult = 2, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    atr_col <- .ensure_atr_breakout_trailing_stop_features(DT, n = n)
  } else {
    atr_col <- paste0("atr_", n)
    .validate_market_dt(DT, c("close", atr_col))
  }

  tgt_pos <- .atr_breakout_trailing_stop_signal(
    close = DT[["close"]],
    atr = DT[[atr_col]],
    atr_mult = atr_mult,
    trail_mult = trail_mult,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = c("close", atr_col)))
  }
  tgt_pos
}

#' ATR-Breakout-Trailing-Stop Action Plan
#'
#' Applies the ATR breakout with trailing-stop rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_atr_breakout_trailing_stop_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_atr_breakout_trailing_stop_action_plan <- function(DT, state, n = 14L, atr_mult = 1, trail_mult = 2, target_size = 1.0, compute_features = TRUE, strat_id = 405L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_atr_breakout_trailing_stop_tgt_pos(
    DT,
    n = n,
    atr_mult = atr_mult,
    trail_mult = trail_mult,
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

.ensure_vol_target_regime_floor_features <- function(DT, trend_n, rv_n, annualization) {
  .ensure_vol_target_features(DT, trend_n = trend_n, rv_n = rv_n, annualization = annualization)
}

.vol_target_regime_floor_signal <- function(close, ema_value, rv_value, vol_target = 0.2, max_leverage = 1.0, rv_ceiling = 0.4) {
  out <- rep(0.0, length(close))
  valid <- !is.na(close) & !is.na(ema_value) & !is.na(rv_value) & rv_value > 0 & rv_value <= rv_ceiling
  dir <- sign(close - ema_value)
  scale <- pmin(max_leverage, vol_target / rv_value)
  out[valid] <- dir[valid] * scale[valid]
  out
}

#' Vol-Target-Regime-Floor Target Positions
#'
#' Generates a volatility-targeted target-position path that goes flat when
#' realized volatility exceeds a ceiling. Direction comes from price relative to
#' a trend EMA and size is otherwise scaled by target volatility over realized
#' volatility.
#'
#' @inheritParams strat_vol_target_tgt_pos
#' @param rv_ceiling Numeric maximum realized volatility allowed for active
#'   positions.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_vol_target_regime_floor_tgt_pos <- function(DT, trend_n = 20L, rv_n = 20L, vol_target = 0.2, max_leverage = 1.0, rv_ceiling = 0.4, annualization = 252, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_vol_target_regime_floor_features(DT, trend_n = trend_n, rv_n = rv_n, annualization = annualization)
  } else {
    cols_needed <- c("close", paste0("ema_", trend_n), paste0("rv_", rv_n))
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .vol_target_regime_floor_signal(
    close = DT[["close"]],
    ema_value = DT[[paste0("ema_", trend_n)]],
    rv_value = DT[[paste0("rv_", rv_n)]],
    vol_target = vol_target,
    max_leverage = max_leverage,
    rv_ceiling = rv_ceiling
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Vol-Target-Regime-Floor Action Plan
#'
#' Applies the volatility-targeted rule with a volatility ceiling to the latest
#' bar and translates the resulting target exposure into an executable action
#' plan.
#'
#' @inheritParams strat_vol_target_regime_floor_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_vol_target_regime_floor_action_plan <- function(DT, state, trend_n = 20L, rv_n = 20L, vol_target = 0.2, max_leverage = 1.0, rv_ceiling = 0.4, annualization = 252, compute_features = TRUE, strat_id = 406L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_vol_target_regime_floor_tgt_pos(
    DT,
    trend_n = trend_n,
    rv_n = rv_n,
    vol_target = vol_target,
    max_leverage = max_leverage,
    rv_ceiling = rv_ceiling,
    annualization = annualization,
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

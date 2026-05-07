.ensure_ema_cross_adx_features <- function(DT, fast, slow, adx_n) {
  .validate_market_dt(DT, c("high", "low", "close"))

  ema_cols_needed <- sprintf("ema_%s", c(fast, slow))
  if (!all(ema_cols_needed %in% names(DT))) {
    calc_EMA(DT, ns = sort(unique(c(fast, slow))))
  }

  adx_col <- paste0("adx_", adx_n)
  if (!adx_col %in% names(DT)) {
    calc_ADX(DT, ns = adx_n)
  }

  invisible(c(ema_cols_needed, adx_col))
}

.ema_cross_adx_signal <- function(fast_value, slow_value, adx_value, adx_threshold = 20, target_size = 1.0) {
  out <- rep(0.0, length(fast_value))
  valid <- !is.na(fast_value) & !is.na(slow_value) & !is.na(adx_value)
  out[valid & adx_value >= adx_threshold & fast_value > slow_value] <- target_size
  out[valid & adx_value >= adx_threshold & fast_value < slow_value] <- -target_size
  out
}

#' EMA-Cross-ADX Target Positions
#'
#' Generates a simple EMA-cross target-position path filtered by ADX trend
#' strength. EMA direction drives the side, and low-ADX regimes stay flat.
#'
#' @param DT A candle `data.table`.
#' @param fast Integer fast EMA length.
#' @param slow Integer slow EMA length.
#' @param adx_n Integer ADX window.
#' @param adx_threshold Numeric ADX threshold required for live targets.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing EMA and ADX features
#'   are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_ema_cross_adx_tgt_pos <- function(DT, fast = 20L, slow = 50L, adx_n = 14L, adx_threshold = 20, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_ema_cross_adx_features(DT, fast = fast, slow = slow, adx_n = adx_n)
  } else {
    cols_needed <- c(
      paste0("ema_", fast),
      paste0("ema_", slow),
      paste0("adx_", adx_n)
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .ema_cross_adx_signal(
    fast_value = DT[[paste0("ema_", fast)]],
    slow_value = DT[[paste0("ema_", slow)]],
    adx_value = DT[[paste0("adx_", adx_n)]],
    adx_threshold = adx_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' EMA-Cross-ADX Action Plan
#'
#' Applies the EMA-cross-plus-ADX rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_ema_cross_adx_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ema_cross_adx_action_plan <- function(DT, state, fast = 20L, slow = 50L, adx_n = 14L, adx_threshold = 20, target_size = 1.0, compute_features = TRUE, strat_id = 103L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ema_cross_adx_tgt_pos(
    DT,
    fast = fast,
    slow = slow,
    adx_n = adx_n,
    adx_threshold = adx_threshold,
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

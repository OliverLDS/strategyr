.ensure_ema_cross_features <- function(DT, fast, slow, low_atr_threshold, atr_h, atr_window) {
  .validate_market_dt(DT, c("high", "low", "close"))

  ema_cols_needed <- sprintf("ema_%s", c(fast, slow))
  missing_ema <- setdiff(ema_cols_needed, names(DT))
  if (length(missing_ema) > 0L) {
    calc_EMA(DT, ns = sort(unique(c(fast, slow))))
  }

  atr_col <- sprintf("atr_logr_%s", atr_h)
  quantile_col <- sprintf("atr_q_%s_%s_%s", low_atr_threshold, atr_h, atr_window)
  low_atr_col <- sprintf("low_atr_%s", low_atr_threshold)

  if (!atr_col %in% names(DT)) {
    calc_ATR(DT, ns = NULL, hs = atr_h)
  }
  if (!quantile_col %in% names(DT)) {
    calc_ATR_quantile(DT, hs = atr_h, window = atr_window, thresholds = low_atr_threshold / 100)
  }
  if (!low_atr_col %in% names(DT)) {
    DT[, (low_atr_col) := get(atr_col) < get(quantile_col)]
  }

  invisible(low_atr_col)
}

#' EMA-Cross Target Positions
#'
#' Generates target positions from a simple EMA-cross strategy with ATR-based
#' gating. The wrapper can either reuse precomputed features or compute the
#' required EMA and ATR features in place.
#'
#' @param DT A candle `data.table`.
#' @param fast Integer fast EMA length.
#' @param slow Integer slow EMA length.
#' @param low_atr_threshold Numeric percentile threshold used to define the
#'   low-volatility gate.
#' @param freshness_floor Integer maximum cross age allowed for a live target.
#' @param tp_ratio Numeric take-profit guard used by `calc_EMA_cross()`.
#' @param sl_ratio Numeric stop-loss guard used by `calc_EMA_cross()`.
#' @param compute_features Logical; when `TRUE`, missing EMA and ATR features are
#'   added to `DT` in place.
#' @param atr_h Numeric ATR half-life used by the low-volatility gate.
#' @param atr_window Integer rolling window used by the ATR quantile.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   column name.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_ema_cross_tgt_pos <- function(DT, fast = 20L, slow = 50L, low_atr_threshold = 5L, freshness_floor = 18L, tp_ratio = 0.05, sl_ratio = 0.02, compute_features = TRUE, atr_h = 12L, atr_window = 300L, debug = FALSE) {
  target_col <- sprintf("tgt_pos_%s_%s_%s", fast, slow, low_atr_threshold)

  if (compute_features) {
    .ensure_ema_cross_features(DT, fast = fast, slow = slow, low_atr_threshold = low_atr_threshold, atr_h = atr_h, atr_window = atr_window)
  } else {
    .validate_market_dt(
      DT,
      c(
        sprintf("ema_%s", fast),
        sprintf("ema_%s", slow),
        sprintf("low_atr_%s", low_atr_threshold),
        "close"
      )
    )
  }

  if (!target_col %in% names(DT)) {
    calc_EMA_cross(
      DT,
      fast = fast,
      slow = slow,
      low_atr_threshold = low_atr_threshold,
      freshness_floor = freshness_floor,
      tp_ratio = tp_ratio,
      sl_ratio = sl_ratio,
      debug_mode = FALSE
    )
  }

  tgt_pos <- DT[[target_col]]
  if (debug) {
    return(list(tgt_pos = tgt_pos, target_col = target_col))
  }
  tgt_pos
}

#' EMA-Cross Action Plan
#'
#' Applies the EMA-cross target rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_ema_cross_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ema_cross_action_plan <- function(DT, state, fast = 20L, slow = 50L, low_atr_threshold = 5L, freshness_floor = 18L, tp_ratio = 0.05, sl_ratio = 0.02, compute_features = TRUE, atr_h = 12L, atr_window = 300L, strat_id = 101L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ema_cross_tgt_pos(
    DT,
    fast = fast,
    slow = slow,
    low_atr_threshold = low_atr_threshold,
    freshness_floor = freshness_floor,
    tp_ratio = tp_ratio,
    sl_ratio = sl_ratio,
    compute_features = compute_features,
    atr_h = atr_h,
    atr_window = atr_window,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

.ensure_regime_switch_features <- function(DT, fast = 20L, slow = 50L, adx_n = 14L, rv_n = 20L, bb_n = 20L, bb_k = 2, annualization = 252, breadth_col = NULL) {
  .validate_market_dt(DT, "close")
  calc_EMA(DT, ns = sort(unique(c(fast, slow))))
  calc_ADX(DT, ns = adx_n)
  calc_realized_vol(DT, ns = rv_n, annualization = annualization)
  calc_BollingerBands(DT, ns = bb_n, ks = bb_k)
  if (!is.null(breadth_col)) {
    .validate_market_dt(DT, breadth_col)
  }

  k_tag <- .suffix_num(bb_k)
  invisible(c(
    paste0("ema_", fast),
    paste0("ema_", slow),
    paste0("adx_", adx_n),
    paste0("rv_", rv_n),
    paste0("bb_mid_", bb_n),
    paste0("bb_high_", bb_n, "_", k_tag),
    paste0("bb_low_", bb_n, "_", k_tag),
    if (!is.null(breadth_col)) breadth_col
  ))
}

.regime_switch_signal <- function(close, ema_fast, ema_slow, adx_value, rv_value, bb_mid, bb_high, bb_low, breadth_value = NULL, trend_adx_threshold = 25, revert_adx_threshold = 18, high_vol_threshold = 0.4, breadth_long_threshold = -Inf, breadth_short_threshold = Inf, target_size = 1.0) {
  trend_tgt <- rep(0.0, length(close))
  valid_trend <- !is.na(ema_fast) & !is.na(ema_slow)
  trend_tgt[valid_trend & ema_fast > ema_slow] <- target_size
  trend_tgt[valid_trend & ema_fast < ema_slow] <- -target_size

  revert_tgt <- .bollinger_revert_signal(close = close, bb_mid = bb_mid, bb_high = bb_high, bb_low = bb_low, target_size = target_size)

  out <- rep(0.0, length(close))
  for (i in seq_along(close)) {
    adx_i <- adx_value[i]
    rv_i <- rv_value[i]
    breadth_i <- if (is.null(breadth_value)) 0 else breadth_value[i]

    if (is.na(adx_i) || is.na(rv_i)) {
      next
    }

    breadth_ok_trend <- is.null(breadth_value) || (trend_tgt[i] >= 0 && breadth_i >= breadth_long_threshold) || (trend_tgt[i] <= 0 && breadth_i <= breadth_short_threshold)
    breadth_ok_revert <- is.null(breadth_value) || (breadth_i >= breadth_long_threshold && breadth_i <= breadth_short_threshold)

    if (adx_i >= trend_adx_threshold && rv_i <= high_vol_threshold && breadth_ok_trend) {
      out[i] <- trend_tgt[i]
    } else if (adx_i <= revert_adx_threshold && rv_i <= high_vol_threshold && breadth_ok_revert) {
      out[i] <- revert_tgt[i]
    }
  }
  out
}

#' Regime-Switching Target Positions
#'
#' Generates a simple meta-strategy target-position path that switches among a
#' trend rule, a mean-reversion rule, and flat exposure depending on ADX,
#' realized volatility, and optionally a breadth filter.
#'
#' @param DT A candle `data.table`.
#' @param fast Integer fast EMA window for the trend regime.
#' @param slow Integer slow EMA window for the trend regime.
#' @param adx_n Integer ADX window.
#' @param rv_n Integer realized-volatility window.
#' @param bb_n Integer Bollinger window for the reversion regime.
#' @param bb_k Numeric Bollinger width multiplier for the reversion regime.
#' @param trend_adx_threshold Numeric ADX threshold above which the trend regime
#'   is allowed.
#' @param revert_adx_threshold Numeric ADX threshold below which the
#'   mean-reversion regime is allowed.
#' @param high_vol_threshold Maximum realized volatility allowed for active
#'   regimes.
#' @param breadth_col Optional breadth filter column.
#' @param breadth_long_threshold Minimum breadth value required for long trend
#'   states.
#' @param breadth_short_threshold Maximum breadth value required for short trend
#'   states.
#' @param annualization Numeric annualization factor passed to
#'   `calc_realized_vol()`.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing features are added to
#'   `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_regime_switch_tgt_pos <- function(DT, fast = 20L, slow = 50L, adx_n = 14L, rv_n = 20L, bb_n = 20L, bb_k = 2, trend_adx_threshold = 25, revert_adx_threshold = 18, high_vol_threshold = 0.4, breadth_col = NULL, breadth_long_threshold = -Inf, breadth_short_threshold = Inf, annualization = 252, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  k_tag <- .suffix_num(bb_k)
  if (compute_features) {
    cols_needed <- .ensure_regime_switch_features(
      DT,
      fast = fast,
      slow = slow,
      adx_n = adx_n,
      rv_n = rv_n,
      bb_n = bb_n,
      bb_k = bb_k,
      annualization = annualization,
      breadth_col = breadth_col
    )
  } else {
    cols_needed <- c(
      "close",
      paste0("ema_", fast),
      paste0("ema_", slow),
      paste0("adx_", adx_n),
      paste0("rv_", rv_n),
      paste0("bb_mid_", bb_n),
      paste0("bb_high_", bb_n, "_", k_tag),
      paste0("bb_low_", bb_n, "_", k_tag),
      if (!is.null(breadth_col)) breadth_col
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .regime_switch_signal(
    close = DT[["close"]],
    ema_fast = DT[[paste0("ema_", fast)]],
    ema_slow = DT[[paste0("ema_", slow)]],
    adx_value = DT[[paste0("adx_", adx_n)]],
    rv_value = DT[[paste0("rv_", rv_n)]],
    bb_mid = DT[[paste0("bb_mid_", bb_n)]],
    bb_high = DT[[paste0("bb_high_", bb_n, "_", k_tag)]],
    bb_low = DT[[paste0("bb_low_", bb_n, "_", k_tag)]],
    breadth_value = if (!is.null(breadth_col)) DT[[breadth_col]] else NULL,
    trend_adx_threshold = trend_adx_threshold,
    revert_adx_threshold = revert_adx_threshold,
    high_vol_threshold = high_vol_threshold,
    breadth_long_threshold = breadth_long_threshold,
    breadth_short_threshold = breadth_short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Regime-Switching Action Plan
#'
#' Applies the regime-switching rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_regime_switch_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_regime_switch_action_plan <- function(DT, state, fast = 20L, slow = 50L, adx_n = 14L, rv_n = 20L, bb_n = 20L, bb_k = 2, trend_adx_threshold = 25, revert_adx_threshold = 18, high_vol_threshold = 0.4, breadth_col = NULL, breadth_long_threshold = -Inf, breadth_short_threshold = Inf, annualization = 252, target_size = 1.0, compute_features = TRUE, strat_id = 105L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_regime_switch_tgt_pos(
    DT,
    fast = fast,
    slow = slow,
    adx_n = adx_n,
    rv_n = rv_n,
    bb_n = bb_n,
    bb_k = bb_k,
    trend_adx_threshold = trend_adx_threshold,
    revert_adx_threshold = revert_adx_threshold,
    high_vol_threshold = high_vol_threshold,
    breadth_col = breadth_col,
    breadth_long_threshold = breadth_long_threshold,
    breadth_short_threshold = breadth_short_threshold,
    annualization = annualization,
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

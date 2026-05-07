.ensure_iv_directional_overlay_features <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, trend_col = "close", trend_n = 50L) {
  .validate_market_dt(DT, c(date_col, expiry_col, type_col, moneyness_col, iv_col, trend_col))

  feat <- .ensure_iv_skew_features(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness
  )
  work_dt <- feat$data

  trend_dt <- unique(DT[, .(date_tmp = get(date_col), trend_value = get(trend_col))], by = "date_tmp")
  data.table::setnames(trend_dt, "date_tmp", date_col)
  data.table::setorderv(trend_dt, date_col)
  ema_col <- paste0("trend_ema_", trend_n)
  trend_dt[, (ema_col) := ema_ttr_fixed_step(trend_value, trend_n, FALSE)]

  work_dt <- merge(work_dt, trend_dt, by = date_col, all.x = TRUE)
  data.table::setorderv(work_dt, c(date_col, expiry_col))

  list(data = work_dt, skew_col = "iv_skew", trend_value_col = "trend_value", ema_col = ema_col)
}

.iv_directional_overlay_signal <- function(skew, trend_value, trend_ema, skew_long_threshold = 0.02, skew_short_threshold = -0.02, overlay_mode = c("confirm", "flip"), target_size = 1.0) {
  overlay_mode <- match.arg(overlay_mode)
  base_signal <- .threshold_signal(
    skew,
    long_threshold = skew_long_threshold,
    short_threshold = skew_short_threshold,
    target_size = target_size
  )

  out <- rep(0.0, length(base_signal))
  valid <- !is.na(base_signal) & !is.na(trend_value) & !is.na(trend_ema)
  trend_dir <- sign(trend_value - trend_ema)

  if (overlay_mode == "confirm") {
    out[valid & base_signal > 0 & trend_dir > 0] <- target_size
    out[valid & base_signal < 0 & trend_dir < 0] <- -target_size
  } else {
    out[valid & base_signal > 0 & trend_dir > 0] <- target_size
    out[valid & base_signal < 0 & trend_dir < 0] <- -target_size
    out[valid & base_signal > 0 & trend_dir < 0] <- -target_size
    out[valid & base_signal < 0 & trend_dir > 0] <- target_size
  }

  out
}

#' IV-Directional-Overlay Target Positions
#'
#' Generates a target-position path from option implied-volatility skew with a
#' simple underlying-trend overlay. The base skew signal can either be confirmed
#' by the underlying trend or flipped when the trend disagrees.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_skew`, a trend-value column, and a
#'   `trend_ema_*` column when `compute_features = FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param target_abs_moneyness Numeric target absolute log-forward-moneyness
#'   used by `calc_option_iv_skew()`.
#' @param trend_col Underlying trend-value column name.
#' @param trend_n Integer EMA window used for the trend overlay.
#' @param skew_long_threshold Numeric skew threshold above which the base signal
#'   is long.
#' @param skew_short_threshold Numeric skew threshold below which the base
#'   signal is short.
#' @param overlay_mode Either `"confirm"` or `"flip"`.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_iv_directional_overlay_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, trend_col = "close", trend_n = 50L, skew_long_threshold = 0.02, skew_short_threshold = -0.02, overlay_mode = c("confirm", "flip"), target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  overlay_mode <- match.arg(overlay_mode)
  ema_col <- paste0("trend_ema_", trend_n)

  if (compute_features) {
    feat <- .ensure_iv_directional_overlay_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      type_col = type_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col,
      target_abs_moneyness = target_abs_moneyness,
      trend_col = trend_col,
      trend_n = trend_n
    )
    work_dt <- feat$data
    skew_col <- feat$skew_col
    trend_value_col <- feat$trend_value_col
    ema_col <- feat$ema_col
  } else {
    skew_col <- "iv_skew"
    trend_value_col <- trend_col
    .validate_market_dt(DT, c(skew_col, trend_value_col, ema_col))
    work_dt <- DT
  }

  tgt_pos <- .iv_directional_overlay_signal(
    skew = work_dt[[skew_col]],
    trend_value = work_dt[[trend_value_col]],
    trend_ema = work_dt[[ema_col]],
    skew_long_threshold = skew_long_threshold,
    skew_short_threshold = skew_short_threshold,
    overlay_mode = overlay_mode,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = skew_col))
  }
  tgt_pos
}

#' IV-Directional-Overlay Action Plan
#'
#' Applies the IV-skew directional-overlay rule to the latest summarized row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_iv_directional_overlay_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_iv_directional_overlay_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, trend_col = "close", trend_n = 50L, skew_long_threshold = 0.02, skew_short_threshold = -0.02, overlay_mode = c("confirm", "flip"), target_size = 1.0, compute_features = TRUE, strat_id = 710L, tol_pos = 0.1, debug = FALSE) {
  overlay_mode <- match.arg(overlay_mode)
  tgt_out <- strat_iv_directional_overlay_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness,
    trend_col = trend_col,
    trend_n = trend_n,
    skew_long_threshold = skew_long_threshold,
    skew_short_threshold = skew_short_threshold,
    overlay_mode = overlay_mode,
    target_size = target_size,
    compute_features = compute_features,
    debug = compute_features
  )
  latest_tgt_pos <- if (compute_features) .latest_non_na(tgt_out$tgt_pos) else .latest_non_na(tgt_out)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

.ensure_macd_histogram_momentum_features <- function(DT, fast, slow, signal) {
  .validate_market_dt(DT, "close")

  cols_needed <- paste0("macd_hist_", fast, "_", slow, "_", signal)
  if (!all(cols_needed %in% names(DT))) {
    calc_MACD(DT, fast = fast, slow = slow, signal = signal)
  }

  invisible(cols_needed)
}

.macd_histogram_momentum_signal <- function(hist_value, accel_lag = 1L, target_size = 1.0) {
  accel <- hist_value - data.table::shift(hist_value, n = accel_lag, type = "lag")
  out <- rep(0.0, length(hist_value))
  valid <- !is.na(hist_value) & !is.na(accel)
  out[valid & hist_value > 0 & accel > 0] <- target_size
  out[valid & hist_value < 0 & accel < 0] <- -target_size
  out
}

#' MACD-Histogram-Momentum Target Positions
#'
#' Generates a target-position path from MACD histogram direction and
#' acceleration. Long exposure is targeted when the histogram is positive and
#' still rising. Short exposure is targeted when the histogram is negative and
#' still falling.
#'
#' @param DT A candle `data.table`.
#' @param fast Integer fast EMA window.
#' @param slow Integer slow EMA window.
#' @param signal Integer MACD signal EMA window.
#' @param accel_lag Integer lag used to measure histogram acceleration.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing MACD features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_macd_histogram_momentum_tgt_pos <- function(DT, fast = 12L, slow = 26L, signal = 9L, accel_lag = 1L, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_macd_histogram_momentum_features(DT, fast = fast, slow = slow, signal = signal)
  } else {
    cols_needed <- paste0("macd_hist_", fast, "_", slow, "_", signal)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .macd_histogram_momentum_signal(
    hist_value = DT[[paste0("macd_hist_", fast, "_", slow, "_", signal)]],
    accel_lag = accel_lag,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' MACD-Histogram-Momentum Action Plan
#'
#' Applies the histogram-acceleration MACD rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_macd_histogram_momentum_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_macd_histogram_momentum_action_plan <- function(DT, state, fast = 12L, slow = 26L, signal = 9L, accel_lag = 1L, target_size = 1.0, compute_features = TRUE, strat_id = 314L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_macd_histogram_momentum_tgt_pos(
    DT,
    fast = fast,
    slow = slow,
    signal = signal,
    accel_lag = accel_lag,
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

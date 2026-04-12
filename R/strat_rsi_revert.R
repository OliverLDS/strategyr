.ensure_rsi_revert_features <- function(DT, n) {
  .validate_market_dt(DT, "close")

  rsi_col <- paste0("rsi_", n)
  if (!rsi_col %in% names(DT)) {
    calc_RSI(DT, ns = n, hs = NULL)
  }

  invisible(rsi_col)
}

.ensure_rsi_logr_revert_features <- function(DT, h) {
  .validate_market_dt(DT, "close")

  rsi_col <- paste0("rsi_logr_", h)
  if (!rsi_col %in% names(DT)) {
    calc_RSI(DT, ns = NULL, hs = h)
  }

  invisible(rsi_col)
}

.rsi_revert_signal <- function(rsi, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0) {
  out <- rep(0.0, length(rsi))
  pos_now <- 0.0

  for (i in seq_along(rsi)) {
    if (is.na(rsi[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && rsi[i] >= exit_level) {
      pos_now <- 0.0
    } else if (pos_now < 0 && rsi[i] <= exit_level) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (rsi[i] <= oversold) {
        pos_now <- target_size
      } else if (rsi[i] >= overbought) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' RSI-Reversion Target Positions
#'
#' Generates a simple mean-reversion target-position path from classic RSI
#' levels. Oversold RSI opens a long target, overbought RSI opens a short
#' target, and open targets are closed once RSI mean-reverts to the exit level.
#'
#' @param DT A candle `data.table`.
#' @param n Integer RSI window.
#' @param oversold Numeric oversold threshold.
#' @param overbought Numeric overbought threshold.
#' @param exit_level Numeric neutral RSI level used to close open targets.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing RSI features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_rsi_revert_tgt_pos <- function(DT, n = 14L, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    rsi_col <- .ensure_rsi_revert_features(DT, n = n)
  } else {
    rsi_col <- paste0("rsi_", n)
    .validate_market_dt(DT, rsi_col)
  }

  tgt_pos <- .rsi_revert_signal(
    rsi = DT[[rsi_col]],
    oversold = oversold,
    overbought = overbought,
    exit_level = exit_level,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = rsi_col))
  }

  tgt_pos
}

#' RSI-Reversion Action Plan
#'
#' Applies the RSI mean-reversion rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_rsi_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_rsi_revert_action_plan <- function(DT, state, n = 14L, oversold = 30, overbought = 70, exit_level = 50, target_size = 1.0, compute_features = TRUE, strat_id = 303L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_rsi_revert_tgt_pos(
    DT,
    n = n,
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

#' Log-Return-RSI-Reversion Target Positions
#'
#' Generates a simple mean-reversion target-position path from strategyr's
#' log-return RSI feature. Oversold log-return RSI opens a long target,
#' overbought log-return RSI opens a short target, and open targets are closed
#' once the indicator mean-reverts to the exit level.
#'
#' @param DT A candle `data.table`.
#' @param h Numeric half-life for the log-return RSI smoother.
#' @param oversold Numeric oversold threshold. The log-return RSI default is
#'   intentionally closer to the center than classic `30/70` RSI thresholds.
#' @param overbought Numeric overbought threshold. The log-return RSI default
#'   is intentionally closer to the center than classic `30/70` RSI thresholds.
#' @param exit_level Numeric neutral RSI level used to close open targets.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing log-return RSI
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_rsi_logr_revert_tgt_pos <- function(DT, h = 12, oversold = 40, overbought = 60, exit_level = 50, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    rsi_col <- .ensure_rsi_logr_revert_features(DT, h = h)
  } else {
    rsi_col <- paste0("rsi_logr_", h)
    .validate_market_dt(DT, rsi_col)
  }

  tgt_pos <- .rsi_revert_signal(
    rsi = DT[[rsi_col]],
    oversold = oversold,
    overbought = overbought,
    exit_level = exit_level,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = rsi_col))
  }

  tgt_pos
}

#' Log-Return-RSI-Reversion Action Plan
#'
#' Applies the log-return RSI mean-reversion rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_rsi_logr_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_rsi_logr_revert_action_plan <- function(DT, state, h = 12, oversold = 40, overbought = 60, exit_level = 50, target_size = 1.0, compute_features = TRUE, strat_id = 306L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_rsi_logr_revert_tgt_pos(
    DT,
    h = h,
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

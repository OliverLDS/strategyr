.pivot_flags <- function(x, left = 2L, right = 2L) {
  n <- length(x)
  low <- rep(FALSE, n)
  high <- rep(FALSE, n)
  if (n < left + right + 1L) {
    return(list(low = low, high = high))
  }

  for (i in seq.int(left + 1L, n - right)) {
    idx <- seq.int(i - left, i + right)
    window_x <- x[idx]
    center <- x[i]
    if (!all(is.finite(window_x))) {
      next
    }
    low[i] <- center == min(window_x) && sum(window_x == center) == 1L
    high[i] <- center == max(window_x) && sum(window_x == center) == 1L
  }

  list(low = low, high = high)
}

.ensure_rsi_divergence_features <- function(DT, rsi_n) {
  .validate_market_dt(DT, "close")

  rsi_col <- paste0("rsi_", rsi_n)
  if (!rsi_col %in% names(DT)) {
    calc_RSI(DT, ns = rsi_n, hs = NULL)
  }

  invisible(rsi_col)
}

.rsi_divergence_signal <- function(close, rsi, pivot_left = 2L, pivot_right = 2L, exit_level = 50, target_size = 1.0) {
  n <- length(close)
  price_piv <- .pivot_flags(close, left = pivot_left, right = pivot_right)
  rsi_piv <- .pivot_flags(rsi, left = pivot_left, right = pivot_right)

  bull_signal <- rep(FALSE, n)
  bear_signal <- rep(FALSE, n)
  last_low_idx <- NA_integer_
  last_high_idx <- NA_integer_

  for (i in seq_len(n)) {
    if (price_piv$low[i] && rsi_piv$low[i]) {
      if (!is.na(last_low_idx) && close[i] < close[last_low_idx] && rsi[i] > rsi[last_low_idx]) {
        bull_signal[min(n, i + pivot_right)] <- TRUE
      }
      last_low_idx <- i
    }

    if (price_piv$high[i] && rsi_piv$high[i]) {
      if (!is.na(last_high_idx) && close[i] > close[last_high_idx] && rsi[i] < rsi[last_high_idx]) {
        bear_signal[min(n, i + pivot_right)] <- TRUE
      }
      last_high_idx <- i
    }
  }

  out <- rep(0.0, n)
  pos_now <- 0.0

  for (i in seq_len(n)) {
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
      if (bull_signal[i]) {
        pos_now <- target_size
      } else if (bear_signal[i]) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' RSI-Divergence Target Positions
#'
#' Generates a mean-reversion target-position path from simple confirmed RSI
#' divergences. Bullish divergence requires a lower confirmed price pivot low
#' paired with a higher confirmed RSI pivot low. Bearish divergence requires a
#' higher confirmed price pivot high paired with a lower confirmed RSI pivot
#' high.
#'
#' @param DT A candle `data.table`.
#' @param rsi_n Integer RSI window.
#' @param pivot_left Integer number of bars on the left used to confirm pivots.
#' @param pivot_right Integer number of bars on the right used to confirm
#'   pivots.
#' @param exit_level Numeric neutral RSI level used to close open targets.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing RSI features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_rsi_divergence_tgt_pos <- function(DT, rsi_n = 14L, pivot_left = 2L, pivot_right = 2L, exit_level = 50, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    rsi_col <- .ensure_rsi_divergence_features(DT, rsi_n = rsi_n)
  } else {
    rsi_col <- paste0("rsi_", rsi_n)
    .validate_market_dt(DT, c("close", rsi_col))
  }

  tgt_pos <- .rsi_divergence_signal(
    close = DT[["close"]],
    rsi = DT[[rsi_col]],
    pivot_left = pivot_left,
    pivot_right = pivot_right,
    exit_level = exit_level,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = c("close", rsi_col)))
  }

  tgt_pos
}

#' RSI-Divergence Action Plan
#'
#' Applies the RSI-divergence rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_rsi_divergence_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_rsi_divergence_action_plan <- function(DT, state, rsi_n = 14L, pivot_left = 2L, pivot_right = 2L, exit_level = 50, target_size = 1.0, compute_features = TRUE, strat_id = 316L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_rsi_divergence_tgt_pos(
    DT,
    rsi_n = rsi_n,
    pivot_left = pivot_left,
    pivot_right = pivot_right,
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

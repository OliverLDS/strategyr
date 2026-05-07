.ensure_bollinger_squeeze_breakout_features <- function(DT, bb_n, bb_k) {
  .validate_market_dt(DT, "close")

  k_tag <- .suffix_num(bb_k)
  cols_needed <- c(
    paste0("bb_mid_", bb_n),
    paste0("bb_high_", bb_n, "_", k_tag),
    paste0("bb_low_", bb_n, "_", k_tag)
  )

  if (!all(cols_needed %in% names(DT))) {
    calc_BollingerBands(DT, ns = bb_n, ks = bb_k)
  }

  width_col <- paste0("bb_width_", bb_n, "_", k_tag)
  if (!width_col %in% names(DT)) {
    bb_mid <- DT[[paste0("bb_mid_", bb_n)]]
    bb_high <- DT[[paste0("bb_high_", bb_n, "_", k_tag)]]
    bb_low <- DT[[paste0("bb_low_", bb_n, "_", k_tag)]]
    width <- (bb_high - bb_low) / bb_mid
    width[!is.finite(width)] <- NA_real_
    data.table::set(DT, j = width_col, value = width)
  }

  invisible(c(cols_needed, width_col))
}

.bollinger_squeeze_breakout_signal <- function(close, bb_mid, bb_high, bb_low, bb_width, squeeze_width = 0.05, target_size = 1.0) {
  out <- rep(0.0, length(close))
  pos_now <- 0.0
  armed <- FALSE

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(bb_mid[i]) || is.na(bb_high[i]) || is.na(bb_low[i]) || is.na(bb_width[i])) {
      out[i] <- pos_now
      next
    }

    if (bb_width[i] <= squeeze_width) {
      armed <- TRUE
    }

    if (pos_now > 0 && close[i] <= bb_mid[i]) {
      pos_now <- 0.0
      armed <- FALSE
    } else if (pos_now < 0 && close[i] >= bb_mid[i]) {
      pos_now <- 0.0
      armed <- FALSE
    }

    if (pos_now == 0.0 && armed) {
      if (close[i] >= bb_high[i] && i > 1L && !is.na(bb_width[i - 1L]) && bb_width[i] > bb_width[i - 1L]) {
        pos_now <- target_size
        armed <- FALSE
      } else if (close[i] <= bb_low[i] && i > 1L && !is.na(bb_width[i - 1L]) && bb_width[i] > bb_width[i - 1L]) {
        pos_now <- -target_size
        armed <- FALSE
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Bollinger-Squeeze-Breakout Target Positions
#'
#' Generates a volatility-compression breakout target-position path. The
#' strategy arms when Bollinger-band width falls below a squeeze threshold, then
#' enters in the breakout direction when price breaches the outer band during
#' width expansion. Open positions are closed on a return to the mid band.
#'
#' @param DT A candle `data.table`.
#' @param bb_n Integer Bollinger window.
#' @param bb_k Numeric Bollinger width multiplier.
#' @param squeeze_width Numeric relative Bollinger-width threshold used to arm
#'   the breakout.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing Bollinger features are
#'   added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_bollinger_squeeze_breakout_tgt_pos <- function(DT, bb_n = 20L, bb_k = 2, squeeze_width = 0.05, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  k_tag <- .suffix_num(bb_k)
  width_col <- paste0("bb_width_", bb_n, "_", k_tag)
  if (compute_features) {
    cols_needed <- .ensure_bollinger_squeeze_breakout_features(DT, bb_n = bb_n, bb_k = bb_k)
  } else {
    cols_needed <- c(
      "close",
      paste0("bb_mid_", bb_n),
      paste0("bb_high_", bb_n, "_", k_tag),
      paste0("bb_low_", bb_n, "_", k_tag),
      width_col
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .bollinger_squeeze_breakout_signal(
    close = DT[["close"]],
    bb_mid = DT[[paste0("bb_mid_", bb_n)]],
    bb_high = DT[[paste0("bb_high_", bb_n, "_", k_tag)]],
    bb_low = DT[[paste0("bb_low_", bb_n, "_", k_tag)]],
    bb_width = DT[[width_col]],
    squeeze_width = squeeze_width,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Bollinger-Squeeze-Breakout Action Plan
#'
#' Applies the squeeze-breakout rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_bollinger_squeeze_breakout_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_bollinger_squeeze_breakout_action_plan <- function(DT, state, bb_n = 20L, bb_k = 2, squeeze_width = 0.05, target_size = 1.0, compute_features = TRUE, strat_id = 309L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_bollinger_squeeze_breakout_tgt_pos(
    DT,
    bb_n = bb_n,
    bb_k = bb_k,
    squeeze_width = squeeze_width,
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

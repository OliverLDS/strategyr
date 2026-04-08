.ensure_bollinger_revert_features <- function(DT, n, k) {
  .validate_market_dt(DT, "close")

  k_tag <- .suffix_num(k)
  cols_needed <- c(
    paste0("bb_mid_", n),
    paste0("bb_high_", n, "_", k_tag),
    paste0("bb_low_", n, "_", k_tag)
  )

  if (!all(cols_needed %in% names(DT))) {
    calc_BollingerBands(DT, ns = n, ks = k)
  }

  invisible(cols_needed)
}

.bollinger_revert_signal <- function(close, bb_mid, bb_high, bb_low, target_size = 1.0) {
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(bb_mid[i]) || is.na(bb_high[i]) || is.na(bb_low[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && close[i] >= bb_mid[i]) {
      pos_now <- 0.0
    } else if (pos_now < 0 && close[i] <= bb_mid[i]) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (close[i] <= bb_low[i]) {
        pos_now <- target_size
      } else if (close[i] >= bb_high[i]) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Bollinger-Reversion Target Positions
#'
#' Generates a simple mean-reversion target-position path from Bollinger band
#' touches. Touching the lower band opens a long target, touching the upper band
#' opens a short target, and open targets are closed when price returns to the
#' mid band.
#'
#' @param DT A candle `data.table`.
#' @param n Integer Bollinger window.
#' @param k Numeric Bollinger width multiplier.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing Bollinger features are
#'   added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_bollinger_revert_tgt_pos <- function(DT, n = 20L, k = 2, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  k_tag <- .suffix_num(k)
  if (compute_features) {
    cols_needed <- .ensure_bollinger_revert_features(DT, n = n, k = k)
  } else {
    cols_needed <- c(
      paste0("bb_mid_", n),
      paste0("bb_high_", n, "_", k_tag),
      paste0("bb_low_", n, "_", k_tag),
      "close"
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .bollinger_revert_signal(
    close = DT[["close"]],
    bb_mid = DT[[paste0("bb_mid_", n)]],
    bb_high = DT[[paste0("bb_high_", n, "_", k_tag)]],
    bb_low = DT[[paste0("bb_low_", n, "_", k_tag)]],
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Bollinger-Reversion Action Plan
#'
#' Applies the Bollinger-band mean-reversion rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_bollinger_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_bollinger_revert_action_plan <- function(DT, state, n = 20L, k = 2, target_size = 1.0, compute_features = TRUE, strat_id = 301L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_bollinger_revert_tgt_pos(
    DT,
    n = n,
    k = k,
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

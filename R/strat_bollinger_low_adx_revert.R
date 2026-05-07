.ensure_bollinger_low_adx_revert_features <- function(DT, n, k, adx_n) {
  bb_cols <- .ensure_bollinger_revert_features(DT, n = n, k = k)
  adx_col <- paste0("adx_", adx_n)
  if (!adx_col %in% names(DT)) {
    calc_ADX(DT, ns = adx_n)
  }
  invisible(c("close", bb_cols, adx_col))
}

.bollinger_low_adx_revert_signal <- function(close, bb_mid, bb_high, bb_low, adx_value, adx_max = 18, target_size = 1.0) {
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(bb_mid[i]) || is.na(bb_high[i]) || is.na(bb_low[i]) || is.na(adx_value[i])) {
      out[i] <- pos_now
      next
    }

    if (adx_value[i] > adx_max) {
      pos_now <- 0.0
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

#' Bollinger-Low-ADX-Reversion Target Positions
#'
#' Generates a Bollinger-band mean-reversion target-position path that is only
#' active in low-ADX regimes. Open reversion positions are also flattened if ADX
#' rises above the permitted regime threshold.
#'
#' @param DT A candle `data.table`.
#' @param n Integer Bollinger window.
#' @param k Numeric Bollinger width multiplier.
#' @param adx_n Integer ADX window.
#' @param adx_max Numeric maximum ADX allowed for the reversion regime.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing Bollinger and ADX
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_bollinger_low_adx_revert_tgt_pos <- function(DT, n = 20L, k = 2, adx_n = 14L, adx_max = 18, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  k_tag <- .suffix_num(k)
  if (compute_features) {
    cols_needed <- .ensure_bollinger_low_adx_revert_features(DT, n = n, k = k, adx_n = adx_n)
  } else {
    cols_needed <- c("close", paste0("bb_mid_", n), paste0("bb_high_", n, "_", k_tag), paste0("bb_low_", n, "_", k_tag), paste0("adx_", adx_n))
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .bollinger_low_adx_revert_signal(
    close = DT[["close"]],
    bb_mid = DT[[paste0("bb_mid_", n)]],
    bb_high = DT[[paste0("bb_high_", n, "_", k_tag)]],
    bb_low = DT[[paste0("bb_low_", n, "_", k_tag)]],
    adx_value = DT[[paste0("adx_", adx_n)]],
    adx_max = adx_max,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Bollinger-Low-ADX-Reversion Action Plan
#'
#' Applies the low-ADX Bollinger reversion rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_bollinger_low_adx_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_bollinger_low_adx_revert_action_plan <- function(DT, state, n = 20L, k = 2, adx_n = 14L, adx_max = 18, target_size = 1.0, compute_features = TRUE, strat_id = 312L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_bollinger_low_adx_revert_tgt_pos(
    DT,
    n = n,
    k = k,
    adx_n = adx_n,
    adx_max = adx_max,
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

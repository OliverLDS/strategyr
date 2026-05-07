.ensure_pair_spread_bollinger_features <- function(DT, x_col, y_col, n, k) {
  .validate_market_dt(DT, c(x_col, y_col))

  spread_col <- paste0("spread_", x_col, "_", y_col)
  k_tag <- .suffix_num(k)
  mid_col <- paste0("spread_bb_mid_", n)
  high_col <- paste0("spread_bb_high_", n, "_", k_tag)
  low_col <- paste0("spread_bb_low_", n, "_", k_tag)

  if (!spread_col %in% names(DT)) {
    calc_spread(DT, x_col = x_col, y_col = y_col, name = spread_col)
  }
  if (!all(c(mid_col, high_col, low_col) %in% names(DT))) {
    mid <- rolling_mean(DT[[spread_col]], n)
    sdv <- rolling_sd(DT[[spread_col]], n, sample = FALSE)
    data.table::set(DT, j = mid_col, value = mid)
    data.table::set(DT, j = high_col, value = mid + k * sdv)
    data.table::set(DT, j = low_col, value = mid - k * sdv)
  }

  invisible(c(spread_col, mid_col, high_col, low_col))
}

.pair_spread_bollinger_signal <- function(spread_value, bb_mid, bb_high, bb_low, target_size = 1.0) {
  out <- rep(0.0, length(spread_value))
  pos_now <- 0.0

  for (i in seq_along(spread_value)) {
    if (is.na(spread_value[i]) || is.na(bb_mid[i]) || is.na(bb_high[i]) || is.na(bb_low[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && spread_value[i] >= bb_mid[i]) {
      pos_now <- 0.0
    } else if (pos_now < 0 && spread_value[i] <= bb_mid[i]) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (spread_value[i] <= bb_low[i]) {
        pos_now <- target_size
      } else if (spread_value[i] >= bb_high[i]) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Pair-Spread-Bollinger Target Positions
#'
#' Generates a pair-spread mean-reversion target-position path using Bollinger
#' bands on the arithmetic spread between the traded asset and benchmark.
#'
#' @param DT A `data.table` containing the traded and benchmark price columns.
#' @param x_col Traded asset price column.
#' @param y_col Benchmark or paired asset price column.
#' @param n Integer Bollinger window.
#' @param k Numeric Bollinger width multiplier.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing spread and spread-band
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_pair_spread_bollinger_tgt_pos <- function(DT, x_col = "close", y_col = "benchmark_close", n = 20L, k = 2, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  k_tag <- .suffix_num(k)
  if (compute_features) {
    cols_needed <- .ensure_pair_spread_bollinger_features(DT, x_col = x_col, y_col = y_col, n = n, k = k)
  } else {
    cols_needed <- c(
      paste0("spread_", x_col, "_", y_col),
      paste0("spread_bb_mid_", n),
      paste0("spread_bb_high_", n, "_", k_tag),
      paste0("spread_bb_low_", n, "_", k_tag)
    )
    .validate_market_dt(DT, cols_needed)
  }

  spread_col <- paste0("spread_", x_col, "_", y_col)
  tgt_pos <- .pair_spread_bollinger_signal(
    spread_value = DT[[spread_col]],
    bb_mid = DT[[paste0("spread_bb_mid_", n)]],
    bb_high = DT[[paste0("spread_bb_high_", n, "_", k_tag)]],
    bb_low = DT[[paste0("spread_bb_low_", n, "_", k_tag)]],
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Pair-Spread-Bollinger Action Plan
#'
#' Applies the pair-spread Bollinger reversion rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_pair_spread_bollinger_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_pair_spread_bollinger_action_plan <- function(DT, state, x_col = "close", y_col = "benchmark_close", n = 20L, k = 2, target_size = 1.0, compute_features = TRUE, strat_id = 505L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_pair_spread_bollinger_tgt_pos(
    DT,
    x_col = x_col,
    y_col = y_col,
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

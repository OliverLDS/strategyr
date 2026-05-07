.ensure_donchian_retest_breakout_features <- function(DT, n) {
  .validate_market_dt(DT, c("close", "high", "low"))
  cols_needed <- c(paste0("dc_high_", n), paste0("dc_low_", n))
  if (!all(cols_needed %in% names(DT))) {
    calc_DonchianChannels(DT, ns = n)
  }
  invisible(c("close", cols_needed))
}

.donchian_retest_breakout_signal <- function(close, dc_high, dc_low, retest_buffer = 0, confirm_n = 5L, target_size = 1.0) {
  prev_high <- .lag_num(dc_high, 1)
  prev_low <- .lag_num(dc_low, 1)
  out <- rep(0.0, length(close))
  pos_now <- 0.0
  armed_dir <- 0L
  armed_level <- NA_real_
  armed_age <- 0L

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(prev_high[i]) || is.na(prev_low[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now == 0.0 && armed_dir == 0L) {
      if (close[i] > prev_high[i]) {
        armed_dir <- 1L
        armed_level <- prev_high[i]
        armed_age <- 0L
      } else if (close[i] < prev_low[i]) {
        armed_dir <- -1L
        armed_level <- prev_low[i]
        armed_age <- 0L
      }
    } else if (pos_now == 0.0 && armed_dir != 0L) {
      armed_age <- armed_age + 1L
      if (armed_age > confirm_n) {
        armed_dir <- 0L
        armed_level <- NA_real_
        armed_age <- 0L
      } else if (armed_dir > 0L && close[i] >= armed_level - retest_buffer && close[i] <= armed_level + retest_buffer) {
        pos_now <- target_size
        armed_dir <- 0L
        armed_level <- NA_real_
        armed_age <- 0L
      } else if (armed_dir < 0L && close[i] >= armed_level - retest_buffer && close[i] <= armed_level + retest_buffer) {
        pos_now <- -target_size
        armed_dir <- 0L
        armed_level <- NA_real_
        armed_age <- 0L
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Donchian-Retest-Breakout Target Positions
#'
#' Generates a Donchian breakout target-position path that waits for a retest of
#' the broken channel boundary before entering. Breakouts arm a pending state,
#' and entries are only activated if price retests the broken level within a
#' limited confirmation window.
#'
#' @param DT A candle `data.table`.
#' @param n Integer Donchian window.
#' @param retest_buffer Numeric distance around the broken channel allowed for a
#'   valid retest.
#' @param confirm_n Integer maximum number of bars to wait for the retest.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing Donchian features are
#'   added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_donchian_retest_breakout_tgt_pos <- function(DT, n = 20L, retest_buffer = 0, confirm_n = 5L, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_donchian_retest_breakout_features(DT, n = n)
  } else {
    cols_needed <- c("close", paste0("dc_high_", n), paste0("dc_low_", n))
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .donchian_retest_breakout_signal(
    close = DT[["close"]],
    dc_high = DT[[paste0("dc_high_", n)]],
    dc_low = DT[[paste0("dc_low_", n)]],
    retest_buffer = retest_buffer,
    confirm_n = confirm_n,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Donchian-Retest-Breakout Action Plan
#'
#' Applies the Donchian retest-breakout rule to the latest bar and translates
#' the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_donchian_retest_breakout_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_donchian_retest_breakout_action_plan <- function(DT, state, n = 20L, retest_buffer = 0, confirm_n = 5L, target_size = 1.0, compute_features = TRUE, strat_id = 313L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_donchian_retest_breakout_tgt_pos(
    DT,
    n = n,
    retest_buffer = retest_buffer,
    confirm_n = confirm_n,
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

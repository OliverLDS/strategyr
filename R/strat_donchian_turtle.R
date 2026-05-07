.ensure_donchian_turtle_features <- function(DT, entry_n, exit_n) {
  .validate_market_dt(DT, c("high", "low", "close"))

  cols_needed <- c(
    paste0("dc_high_", entry_n),
    paste0("dc_low_", entry_n),
    paste0("dc_high_", exit_n),
    paste0("dc_low_", exit_n)
  )

  if (!all(cols_needed %in% names(DT))) {
    calc_DonchianChannels(DT, ns = sort(unique(c(entry_n, exit_n))))
  }

  invisible(cols_needed)
}

.donchian_turtle_signal <- function(close, entry_high, entry_low, exit_high, exit_low, target_size = 1.0) {
  entry_high_prev <- .lag_num(entry_high, 1L)
  entry_low_prev <- .lag_num(entry_low, 1L)
  exit_high_prev <- .lag_num(exit_high, 1L)
  exit_low_prev <- .lag_num(exit_low, 1L)
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(entry_high_prev[i]) || is.na(entry_low_prev[i]) ||
        is.na(exit_high_prev[i]) || is.na(exit_low_prev[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && close[i] < exit_low_prev[i]) {
      pos_now <- 0.0
    } else if (pos_now < 0 && close[i] > exit_high_prev[i]) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (close[i] > entry_high_prev[i]) {
        pos_now <- target_size
      } else if (close[i] < entry_low_prev[i]) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Donchian-Turtle Target Positions
#'
#' Generates a Turtle-style breakout target-position path using a long-window
#' Donchian entry breakout and a shorter-window Donchian exit breakout.
#'
#' @param DT A candle `data.table`.
#' @param entry_n Integer Donchian entry window.
#' @param exit_n Integer Donchian exit window.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing Donchian features are
#'   added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_donchian_turtle_tgt_pos <- function(DT, entry_n = 55L, exit_n = 20L, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_donchian_turtle_features(DT, entry_n = entry_n, exit_n = exit_n)
  } else {
    cols_needed <- c(
      paste0("dc_high_", entry_n),
      paste0("dc_low_", entry_n),
      paste0("dc_high_", exit_n),
      paste0("dc_low_", exit_n),
      "close"
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .donchian_turtle_signal(
    close = DT[["close"]],
    entry_high = DT[[paste0("dc_high_", entry_n)]],
    entry_low = DT[[paste0("dc_low_", entry_n)]],
    exit_high = DT[[paste0("dc_high_", exit_n)]],
    exit_low = DT[[paste0("dc_low_", exit_n)]],
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Donchian-Turtle Action Plan
#'
#' Applies the Donchian Turtle rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_donchian_turtle_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_donchian_turtle_action_plan <- function(DT, state, entry_n = 55L, exit_n = 20L, target_size = 1.0, compute_features = TRUE, strat_id = 307L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_donchian_turtle_tgt_pos(
    DT,
    entry_n = entry_n,
    exit_n = exit_n,
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

.ensure_macd_zero_line_features <- function(DT, fast, slow, signal) {
  .ensure_macd_cross_features(DT, fast = fast, slow = slow, signal = signal)
  invisible(c(
    paste0("macd_", fast, "_", slow),
    paste0("macd_signal_", fast, "_", slow, "_", signal)
  ))
}

.macd_zero_line_signal <- function(macd, signal_value, target_size = 1.0) {
  out <- rep(0.0, length(macd))
  valid <- !is.na(macd) & !is.na(signal_value)
  out[valid & macd > signal_value & macd > 0] <- target_size
  out[valid & macd < signal_value & macd < 0] <- -target_size
  out
}

#' MACD-Zero-Line Target Positions
#'
#' Generates a target-position path from MACD crossovers filtered by the
#' zero-line. Long targets require bullish crossover with MACD above zero, while
#' short targets require bearish crossover with MACD below zero.
#'
#' @inheritParams strat_macd_cross_tgt_pos
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_macd_zero_line_tgt_pos <- function(DT, fast = 12L, slow = 26L, signal = 9L, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_macd_zero_line_features(DT, fast = fast, slow = slow, signal = signal)
  } else {
    cols_needed <- c(
      paste0("macd_", fast, "_", slow),
      paste0("macd_signal_", fast, "_", slow, "_", signal)
    )
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .macd_zero_line_signal(
    macd = DT[[paste0("macd_", fast, "_", slow)]],
    signal_value = DT[[paste0("macd_signal_", fast, "_", slow, "_", signal)]],
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' MACD-Zero-Line Action Plan
#'
#' Applies the MACD-plus-zero-line rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_macd_zero_line_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_macd_zero_line_action_plan <- function(DT, state, fast = 12L, slow = 26L, signal = 9L, target_size = 1.0, compute_features = TRUE, strat_id = 310L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_macd_zero_line_tgt_pos(
    DT,
    fast = fast,
    slow = slow,
    signal = signal,
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

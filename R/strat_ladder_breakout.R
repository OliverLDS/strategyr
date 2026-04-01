#' Ladder Breakout Target Positions
#'
#' Generates a naive continuation target-position path from signed Fibonacci
#' ladder indices. The public rule is intentionally simple: positions beyond the
#' lower threshold target further downside, and positions beyond the upper
#' threshold target further upside.
#'
#' @inheritParams strat_ladder_bounce_tgt_pos
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_ladder_breakout_tgt_pos <- function(DT, cycle_N = 180L, span = 3L, refined = TRUE, min_swing = 0.05, center_idx = 9L, lower = 7L, upper = 13L, target_size = 1.0, compute_ladder = TRUE, detailed_report = FALSE, debug = FALSE) {
  ladder_col <- sprintf("ladder_index_%s", cycle_N)

  if (compute_ladder && !ladder_col %in% names(DT)) {
    .validate_market_dt(DT, c("datetime", "high", "low", "close"))
    calc_ladder_index(
      DT,
      span = span,
      refined = refined,
      min_swing = min_swing,
      cycle_N = cycle_N,
      center_idx = center_idx,
      detailed_report = detailed_report
    )
  } else {
    .validate_market_dt(DT, ladder_col)
  }

  tgt_pos <- .ladder_breakout_signal(DT[[ladder_col]], lower = lower, upper = upper, target_size = target_size)
  if (debug) {
    return(list(tgt_pos = tgt_pos, ladder_col = ladder_col))
  }
  tgt_pos
}

#' Ladder Breakout Action Plan
#'
#' Applies the naive ladder-breakout rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_ladder_bounce_action_plan
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ladder_breakout_action_plan <- function(DT, state, cycle_N = 180L, span = 3L, refined = TRUE, min_swing = 0.05, center_idx = 9L, lower = 7L, upper = 13L, target_size = 1.0, compute_ladder = TRUE, detailed_report = FALSE, strat_id = 202L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ladder_breakout_tgt_pos(
    DT,
    cycle_N = cycle_N,
    span = span,
    refined = refined,
    min_swing = min_swing,
    center_idx = center_idx,
    lower = lower,
    upper = upper,
    target_size = target_size,
    compute_ladder = compute_ladder,
    detailed_report = detailed_report,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

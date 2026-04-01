#' Buy-And-Hold Target Positions
#'
#' Generates a constant target exposure path for a simple buy-and-hold strategy.
#'
#' @param DT A candle `data.table` containing `datetime`.
#' @param value Numeric scalar target exposure.
#'
#' @return A numeric vector of target positions with length `nrow(DT)`.
#' @export
strat_buy_and_hold_tgt_pos <- function(DT, value = 1.0) {
  .validate_market_dt(DT, "datetime")

  if (identical(value, 1.0)) {
    return(strat_buy_and_hold_rcpp(DT$datetime))
  }

  rep(value, nrow(DT))
}

#' Buy-And-Hold Action Plan
#'
#' Applies the buy-and-hold target rule to the latest bar and translates the
#' resulting target exposure into an executable action plan.
#'
#' @param DT A candle `data.table` containing `datetime`.
#' @param state Named list describing the current trading state.
#' @param value Numeric scalar target exposure.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_buy_and_hold_action_plan <- function(DT, state, value = 1.0, strat_id = 1L, tol_pos = 0) {
  tgt_pos <- strat_buy_and_hold_tgt_pos(DT, value = value)
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
}

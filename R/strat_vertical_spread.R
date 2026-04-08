.ensure_vertical_spread_features <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv") {
  out <- calc_option_put_call_iv_spread(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col
  )
  list(data = out, colname = "iv_put_call_spread")
}

#' Vertical-Spread Target Positions
#'
#' Generates a simple vertical-spread proxy target-position path from the ATM
#' put-minus-call implied-volatility spread. Positive put-call IV spread above
#' the long threshold targets a bearish vertical-spread proxy exposure, and
#' negative spread below the short threshold targets a bullish exposure.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_put_call_spread` when
#'   `compute_features = FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param long_threshold Numeric threshold above which put-call IV spread is
#'   treated as bearish enough to go short.
#' @param short_threshold Numeric threshold below which put-call IV spread is
#'   treated as bullish enough to go long.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_vertical_spread_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", long_threshold = 0.02, short_threshold = -0.02, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    feat <- .ensure_vertical_spread_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      type_col = type_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col
    )
    work_dt <- feat$data
    spread_col <- feat$colname
  } else {
    spread_col <- "iv_put_call_spread"
    .validate_market_dt(DT, spread_col)
    work_dt <- DT
  }

  tgt_pos <- rep(0.0, nrow(work_dt))
  valid <- !is.na(work_dt[[spread_col]])
  tgt_pos[valid & work_dt[[spread_col]] >= long_threshold] <- -target_size
  tgt_pos[valid & work_dt[[spread_col]] <= short_threshold] <- target_size

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = spread_col))
  }
  tgt_pos
}

#' Vertical-Spread Action Plan
#'
#' Applies the vertical-spread proxy rule to the latest summarized row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_vertical_spread_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_vertical_spread_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", long_threshold = 0.02, short_threshold = -0.02, target_size = 1.0, compute_features = TRUE, strat_id = 705L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_vertical_spread_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size,
    compute_features = compute_features,
    debug = compute_features
  )
  latest_tgt_pos <- if (compute_features) .latest_non_na(tgt_out$tgt_pos) else .latest_non_na(tgt_out)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

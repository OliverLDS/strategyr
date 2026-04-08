.ensure_straddle_features <- function(DT, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv") {
  out <- calc_option_iv_term_structure(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col
  )
  list(data = out, colname = "iv_atm_front")
}

#' Straddle Target Positions
#'
#' Generates a simple straddle-proxy target-position path from front-expiry ATM
#' implied volatility. Low front ATM IV targets a long straddle proxy exposure,
#' and high front ATM IV targets a short straddle proxy exposure.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_atm_front` when `compute_features =
#'   FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param long_iv_threshold Numeric threshold at or below which ATM IV is
#'   treated as cheap enough to go long.
#' @param short_iv_threshold Numeric threshold at or above which ATM IV is
#'   treated as rich enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_straddle_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", long_iv_threshold = 0.2, short_iv_threshold = 0.4, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    feat <- .ensure_straddle_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col
    )
    work_dt <- feat$data
    iv_ref_col <- feat$colname
  } else {
    iv_ref_col <- "iv_atm_front"
    .validate_market_dt(DT, iv_ref_col)
    work_dt <- DT
  }

  tgt_pos <- rep(0.0, nrow(work_dt))
  valid <- !is.na(work_dt[[iv_ref_col]])
  tgt_pos[valid & work_dt[[iv_ref_col]] <= long_iv_threshold] <- target_size
  tgt_pos[valid & work_dt[[iv_ref_col]] >= short_iv_threshold] <- -target_size

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = iv_ref_col))
  }
  tgt_pos
}

#' Straddle Action Plan
#'
#' Applies the straddle-proxy rule to the latest summarized row and translates
#' the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_straddle_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_straddle_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", long_iv_threshold = 0.2, short_iv_threshold = 0.4, target_size = 1.0, compute_features = TRUE, strat_id = 703L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_straddle_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    long_iv_threshold = long_iv_threshold,
    short_iv_threshold = short_iv_threshold,
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

.ensure_iv_term_structure_features <- function(DT, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv") {
  out <- calc_option_iv_term_structure(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col
  )
  list(data = out, colname = "iv_term_structure")
}

#' IV-Term-Structure Target Positions
#'
#' Generates a simple target-position path from option ATM implied-volatility
#' term structure. Positive term-structure slope above the long threshold
#' targets a long structure exposure, and negative slope below the short
#' threshold targets a short exposure.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_term_structure` when
#'   `compute_features = FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param long_threshold Numeric threshold above which term structure is treated
#'   as positive enough to go long.
#' @param short_threshold Numeric threshold below which term structure is
#'   treated as negative enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_iv_term_structure_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", long_threshold = 0.01, short_threshold = -0.01, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    feat <- .ensure_iv_term_structure_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col
    )
    work_dt <- feat$data
    ts_col <- feat$colname
  } else {
    ts_col <- "iv_term_structure"
    .validate_market_dt(DT, ts_col)
    work_dt <- DT
  }

  tgt_pos <- .threshold_signal(work_dt[[ts_col]], long_threshold = long_threshold, short_threshold = short_threshold, target_size = target_size)
  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = ts_col))
  }
  tgt_pos
}

#' IV-Term-Structure Action Plan
#'
#' Applies the IV-term-structure rule to the latest summarized row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_iv_term_structure_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_iv_term_structure_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", long_threshold = 0.01, short_threshold = -0.01, target_size = 1.0, compute_features = TRUE, strat_id = 702L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_iv_term_structure_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
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

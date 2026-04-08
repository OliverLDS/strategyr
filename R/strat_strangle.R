.ensure_strangle_features <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1) {
  out <- calc_option_iv_skew(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness
  )
  out[, iv_otm_avg := (iv_put_otm + iv_call_otm) / 2]
  list(data = out, colname = "iv_otm_avg")
}

#' Strangle Target Positions
#'
#' Generates a simple strangle-proxy target-position path from the average
#' front-expiry OTM implied volatility. Low OTM IV targets a long strangle
#' proxy exposure, and high OTM IV targets a short strangle proxy exposure.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_otm_avg` when `compute_features =
#'   FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param target_abs_moneyness Numeric target absolute log-forward-moneyness
#'   used by `calc_option_iv_skew()`.
#' @param long_iv_threshold Numeric threshold at or below which OTM IV is
#'   treated as cheap enough to go long.
#' @param short_iv_threshold Numeric threshold at or above which OTM IV is
#'   treated as rich enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_strangle_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, long_iv_threshold = 0.2, short_iv_threshold = 0.4, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    feat <- .ensure_strangle_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      type_col = type_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col,
      target_abs_moneyness = target_abs_moneyness
    )
    work_dt <- feat$data
    iv_ref_col <- feat$colname
  } else {
    iv_ref_col <- "iv_otm_avg"
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

#' Strangle Action Plan
#'
#' Applies the strangle-proxy rule to the latest summarized row and translates
#' the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_strangle_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_strangle_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, long_iv_threshold = 0.2, short_iv_threshold = 0.4, target_size = 1.0, compute_features = TRUE, strat_id = 704L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_strangle_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness,
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

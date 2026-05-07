.ensure_iv_skew_zscore_features <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, z_n = 60L) {
  feat <- .ensure_iv_skew_features(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness
  )
  work_dt <- feat$data
  skew_col <- feat$colname
  z_col <- paste0("zscore_", skew_col, "_", z_n)
  if (!z_col %in% names(work_dt)) {
    calc_zscore(work_dt, cols = skew_col, ns = z_n)
  }
  list(data = work_dt, skew_col = skew_col, z_col = z_col)
}

.iv_skew_zscore_signal <- function(z_value, long_z = 1, short_z = -1, mode = c("momentum", "reversion"), target_size = 1.0) {
  mode <- match.arg(mode)
  out <- rep(0.0, length(z_value))
  valid <- !is.na(z_value)

  if (mode == "momentum") {
    out[valid & z_value >= long_z] <- target_size
    out[valid & z_value <= short_z] <- -target_size
  } else {
    out[valid & z_value >= long_z] <- -target_size
    out[valid & z_value <= short_z] <- target_size
  }

  out
}

#' IV-Skew-Z-Score Target Positions
#'
#' Generates a target-position path from the rolling z-score of option
#' implied-volatility skew, with either momentum or reversion interpretation.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_skew` and `zscore_iv_skew_*` when
#'   `compute_features = FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param target_abs_moneyness Numeric target absolute log-forward-moneyness
#'   used by `calc_option_iv_skew()`.
#' @param z_n Integer rolling z-score window.
#' @param long_z Numeric upper z-score threshold.
#' @param short_z Numeric lower z-score threshold.
#' @param mode Character strategy mode: `momentum` or `reversion`.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_iv_skew_zscore_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, z_n = 60L, long_z = 1, short_z = -1, mode = c("momentum", "reversion"), target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  mode <- match.arg(mode)
  if (compute_features) {
    feat <- .ensure_iv_skew_zscore_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      type_col = type_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col,
      target_abs_moneyness = target_abs_moneyness,
      z_n = z_n
    )
    work_dt <- feat$data
    skew_col <- feat$skew_col
    z_col <- feat$z_col
  } else {
    skew_col <- "iv_skew"
    z_col <- paste0("zscore_", skew_col, "_", z_n)
    .validate_market_dt(DT, c(skew_col, z_col))
    work_dt <- DT
  }

  tgt_pos <- .iv_skew_zscore_signal(
    z_value = work_dt[[z_col]],
    long_z = long_z,
    short_z = short_z,
    mode = mode,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = skew_col))
  }
  tgt_pos
}

#' IV-Skew-Z-Score Action Plan
#'
#' Applies the IV-skew z-score rule to the latest summarized row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_iv_skew_zscore_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_iv_skew_zscore_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, z_n = 60L, long_z = 1, short_z = -1, mode = c("momentum", "reversion"), target_size = 1.0, compute_features = TRUE, strat_id = 706L, tol_pos = 0.1, debug = FALSE) {
  mode <- match.arg(mode)
  tgt_out <- strat_iv_skew_zscore_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness,
    z_n = z_n,
    long_z = long_z,
    short_z = short_z,
    mode = mode,
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

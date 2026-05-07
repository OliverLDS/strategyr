.ensure_iv_skew_realized_vol_confirm_features <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, close_col = "close", rv_n = 20L, annualization = 252) {
  .validate_market_dt(DT, c(date_col, expiry_col, type_col, moneyness_col, iv_col, close_col))

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

  iv_level_dt <- DT[, {
    put_row <- .pick_nearest_row(.SD[get(type_col) == "put"], moneyness_col, -abs(target_abs_moneyness), side = "left")
    call_row <- .pick_nearest_row(.SD[get(type_col) == "call"], moneyness_col, abs(target_abs_moneyness), side = "right")
    put_iv <- if (nrow(put_row)) put_row[[iv_col]][1] else NA_real_
    call_iv <- if (nrow(call_row)) call_row[[iv_col]][1] else NA_real_
    .(iv_level = mean(c(put_iv, call_iv), na.rm = TRUE))
  }, by = c(date_col, expiry_col)]
  iv_level_dt[!is.finite(iv_level), iv_level := NA_real_]

  close_dt <- unique(DT[, .(date_tmp = get(date_col), close_tmp = get(close_col))], by = "date_tmp")
  data.table::setnames(close_dt, c("date_tmp", "close_tmp"), c(date_col, close_col))

  work_dt <- merge(work_dt, iv_level_dt, by = c(date_col, expiry_col), all.x = TRUE)
  work_dt <- merge(work_dt, close_dt, by = date_col, all.x = TRUE)
  data.table::setorderv(work_dt, c(date_col, expiry_col))

  calc_realized_vol(work_dt, ns = rv_n, annualization = annualization)
  spread_col <- paste0("iv_rv_spread_", rv_n)
  data.table::set(work_dt, j = spread_col, value = work_dt[["iv_level"]] - work_dt[[paste0("rv_", rv_n)]])

  list(
    data = work_dt,
    skew_col = "iv_skew",
    confirm_col = spread_col
  )
}

.iv_skew_realized_vol_confirm_signal <- function(skew, iv_rv_spread, long_threshold = 0.02, short_threshold = -0.02, iv_rv_confirm = 0, target_size = 1.0) {
  out <- rep(0.0, length(skew))
  valid <- !is.na(skew) & !is.na(iv_rv_spread)
  out[valid & skew >= long_threshold & iv_rv_spread >= iv_rv_confirm] <- target_size
  out[valid & skew <= short_threshold & iv_rv_spread <= -iv_rv_confirm] <- -target_size
  out
}

#' IV-Skew-Realized-Vol-Confirm Target Positions
#'
#' Generates a target-position path from option implied-volatility skew, gated
#' by an implied-minus-realized volatility confirmation spread. Positive skew is
#' actionable only when summarized implied volatility remains sufficiently above
#' realized volatility; negative skew is actionable only when the confirmation
#' spread is sufficiently negative.
#'
#' @param DT An option-chain `data.table` when `compute_features = TRUE`, or a
#'   summarized `data.table` containing `iv_skew` and `iv_rv_spread_<rv_n>` when
#'   `compute_features = FALSE`.
#' @param date_col Date or timestamp column name.
#' @param expiry_col Time-to-expiry column name.
#' @param type_col Option-type column name.
#' @param moneyness_col Forward-moneyness feature column name.
#' @param iv_col Implied-volatility column name.
#' @param target_abs_moneyness Numeric target absolute log-forward-moneyness
#'   used by `calc_option_iv_skew()`.
#' @param close_col Underlying close column name used to compute realized
#'   volatility.
#' @param rv_n Integer realized-volatility window.
#' @param annualization Annualization factor passed to `calc_realized_vol()`.
#' @param long_threshold Numeric threshold above which skew is treated as
#'   positive enough to go long.
#' @param short_threshold Numeric threshold below which skew is treated as
#'   negative enough to go short.
#' @param iv_rv_confirm Numeric minimum confirmation spread magnitude.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the chain is summarized before
#'   generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_iv_skew_realized_vol_confirm_tgt_pos <- function(DT, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, close_col = "close", rv_n = 20L, annualization = 252, long_threshold = 0.02, short_threshold = -0.02, iv_rv_confirm = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    feat <- .ensure_iv_skew_realized_vol_confirm_features(
      DT,
      date_col = date_col,
      expiry_col = expiry_col,
      type_col = type_col,
      moneyness_col = moneyness_col,
      iv_col = iv_col,
      target_abs_moneyness = target_abs_moneyness,
      close_col = close_col,
      rv_n = rv_n,
      annualization = annualization
    )
    work_dt <- feat$data
    skew_col <- feat$skew_col
    confirm_col <- feat$confirm_col
  } else {
    skew_col <- "iv_skew"
    confirm_col <- paste0("iv_rv_spread_", rv_n)
    .validate_market_dt(DT, c(skew_col, confirm_col))
    work_dt <- DT
  }

  tgt_pos <- .iv_skew_realized_vol_confirm_signal(
    skew = work_dt[[skew_col]],
    iv_rv_spread = work_dt[[confirm_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    iv_rv_confirm = iv_rv_confirm,
    target_size = target_size
  )
  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = skew_col))
  }
  tgt_pos
}

#' IV-Skew-Realized-Vol-Confirm Action Plan
#'
#' Applies the IV-skew with realized-volatility confirmation rule to the latest
#' summarized row and translates the resulting target exposure into an
#' executable action plan.
#'
#' @inheritParams strat_iv_skew_realized_vol_confirm_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_iv_skew_realized_vol_confirm_action_plan <- function(DT, state, date_col = "date", expiry_col = "T", type_col = "type", moneyness_col = "option_log_forward_moneyness", iv_col = "iv", target_abs_moneyness = 0.1, close_col = "close", rv_n = 20L, annualization = 252, long_threshold = 0.02, short_threshold = -0.02, iv_rv_confirm = 0, target_size = 1.0, compute_features = TRUE, strat_id = 709L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_iv_skew_realized_vol_confirm_tgt_pos(
    DT,
    date_col = date_col,
    expiry_col = expiry_col,
    type_col = type_col,
    moneyness_col = moneyness_col,
    iv_col = iv_col,
    target_abs_moneyness = target_abs_moneyness,
    close_col = close_col,
    rv_n = rv_n,
    annualization = annualization,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    iv_rv_confirm = iv_rv_confirm,
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

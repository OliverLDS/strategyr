.ensure_vol_carry_features <- function(DT, iv_col = "iv", rv_n = 20L, annualization = 252) {
  .validate_market_dt(DT, c("close", iv_col))

  rv_col <- paste0("rv_", rv_n)
  if (!rv_col %in% names(DT)) {
    calc_realized_vol(DT, ns = rv_n, annualization = annualization)
  }

  spread_col <- paste0("iv_rv_spread_", rv_n)
  if (!spread_col %in% names(DT)) {
    spread <- DT[[iv_col]] - DT[[rv_col]]
    spread[!is.finite(spread)] <- NA_real_
    data.table::set(DT, j = spread_col, value = spread)
  }

  invisible(c(iv_col, rv_col, spread_col))
}

.vol_carry_signal <- function(spread_value, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(spread_value))
  valid <- !is.na(spread_value)
  out[valid & spread_value > long_threshold] <- target_size
  out[valid & spread_value < short_threshold] <- -target_size
  out
}

#' Volatility-Carry Target Positions
#'
#' Generates a target-position path from the gap between implied and realized
#' volatility. Positive target values represent short-vol carry exposure when
#' implied volatility is rich relative to realized volatility, while negative
#' targets represent long-vol exposure when implied volatility is cheap.
#'
#' @param DT A `data.table` containing `close` and an implied-volatility column.
#' @param iv_col Implied-volatility column name.
#' @param rv_n Integer realized-volatility window.
#' @param annualization Numeric annualization factor passed to
#'   `calc_realized_vol()`.
#' @param long_threshold Numeric threshold above which IV minus RV is treated as
#'   rich enough to carry a short-vol exposure.
#' @param short_threshold Numeric threshold below which IV minus RV is treated
#'   as cheap enough to go long volatility.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing realized-volatility and
#'   IV-minus-RV features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_vol_carry_tgt_pos <- function(DT, iv_col = "iv", rv_n = 20L, annualization = 252, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  spread_col <- paste0("iv_rv_spread_", rv_n)
  if (compute_features) {
    cols_needed <- .ensure_vol_carry_features(DT, iv_col = iv_col, rv_n = rv_n, annualization = annualization)
  } else {
    cols_needed <- c(iv_col, paste0("rv_", rv_n), spread_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .vol_carry_signal(
    spread_value = DT[[spread_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Volatility-Carry Action Plan
#'
#' Applies the volatility-carry rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_vol_carry_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_vol_carry_action_plan <- function(DT, state, iv_col = "iv", rv_n = 20L, annualization = 252, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 707L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_vol_carry_tgt_pos(
    DT,
    iv_col = iv_col,
    rv_n = rv_n,
    annualization = annualization,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
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

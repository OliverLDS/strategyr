.ensure_curve_steepener_zscore_features <- function(DT, short_rate_col, long_rate_col, z_n) {
  cols_needed <- .ensure_curve_steepener_features(DT, short_rate_col = short_rate_col, long_rate_col = long_rate_col)
  z_col <- paste0("zscore_curve_slope_", z_n)
  if (!z_col %in% names(DT)) {
    calc_zscore(DT, cols = "curve_slope", ns = z_n)
  }
  invisible(c(cols_needed, z_col))
}

.curve_steepener_zscore_signal <- function(z_value, long_z = 1, short_z = -1, mode = c("momentum", "reversion"), target_size = 1.0) {
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

#' Curve-Steepener-Z-Score Target Positions
#'
#' Generates a curve-steepener target-position path from the rolling z-score of
#' curve slope, with either momentum or reversion interpretation.
#'
#' @param DT A `data.table` containing short- and long-rate columns or
#'   precomputed `curve_slope` and `zscore_curve_slope_*` columns.
#' @param short_rate_col Short-end rate column name.
#' @param long_rate_col Long-end rate column name.
#' @param z_n Integer rolling z-score window.
#' @param long_z Numeric upper z-score threshold.
#' @param short_z Numeric lower z-score threshold.
#' @param mode Character strategy mode: `momentum` or `reversion`.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing slope and z-score
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_curve_steepener_zscore_tgt_pos <- function(DT, short_rate_col = "short_rate", long_rate_col = "long_rate", z_n = 252L, long_z = 1, short_z = -1, mode = c("momentum", "reversion"), target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  mode <- match.arg(mode)
  z_col <- paste0("zscore_curve_slope_", z_n)

  if (compute_features) {
    cols_needed <- .ensure_curve_steepener_zscore_features(DT, short_rate_col = short_rate_col, long_rate_col = long_rate_col, z_n = z_n)
  } else {
    cols_needed <- z_col
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .curve_steepener_zscore_signal(
    z_value = DT[[z_col]],
    long_z = long_z,
    short_z = short_z,
    mode = mode,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Curve-Steepener-Z-Score Action Plan
#'
#' Applies the curve-slope z-score rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_curve_steepener_zscore_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_curve_steepener_zscore_action_plan <- function(DT, state, short_rate_col = "short_rate", long_rate_col = "long_rate", z_n = 252L, long_z = 1, short_z = -1, mode = c("momentum", "reversion"), target_size = 1.0, compute_features = TRUE, strat_id = 605L, tol_pos = 0.1, debug = FALSE) {
  mode <- match.arg(mode)
  tgt_pos <- strat_curve_steepener_zscore_tgt_pos(
    DT,
    short_rate_col = short_rate_col,
    long_rate_col = long_rate_col,
    z_n = z_n,
    long_z = long_z,
    short_z = short_z,
    mode = mode,
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

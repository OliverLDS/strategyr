.ensure_curve_steepener_features <- function(DT, short_rate_col, long_rate_col) {
  .validate_market_dt(DT, c(short_rate_col, long_rate_col))
  if (!"curve_slope" %in% names(DT)) {
    data.table::set(
      DT,
      j = "curve_slope",
      value = DT[[long_rate_col]] - DT[[short_rate_col]]
    )
  }
  invisible("curve_slope")
}

.curve_steepener_signal <- function(curve_slope, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(curve_slope))
  valid <- !is.na(curve_slope)
  out[valid & curve_slope > long_threshold] <- target_size
  out[valid & curve_slope < short_threshold] <- -target_size
  out
}

#' Curve-Steepener Target Positions
#'
#' Generates a simple curve-steepener target-position path from yield-curve
#' slope. Positive slope targets a steepener exposure and negative slope targets
#' a flattener exposure.
#'
#' @param DT A `data.table` containing short- and long-rate columns or a
#'   precomputed `curve_slope` column.
#' @param short_rate_col Short-end rate column name.
#' @param long_rate_col Long-end rate column name.
#' @param long_threshold Numeric threshold above which slope is treated as
#'   steepening enough to go long.
#' @param short_threshold Numeric threshold below which slope is treated as
#'   flattening enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing slope features are
#'   added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_curve_steepener_tgt_pos <- function(DT, short_rate_col = "short_rate", long_rate_col = "long_rate", long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_curve_steepener_features(DT, short_rate_col = short_rate_col, long_rate_col = long_rate_col)
  } else {
    cols_needed <- "curve_slope"
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .curve_steepener_signal(
    curve_slope = DT[["curve_slope"]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Curve-Steepener Action Plan
#'
#' Applies the curve-steepener rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_curve_steepener_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_curve_steepener_action_plan <- function(DT, state, short_rate_col = "short_rate", long_rate_col = "long_rate", long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 603L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_curve_steepener_tgt_pos(
    DT,
    short_rate_col = short_rate_col,
    long_rate_col = long_rate_col,
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

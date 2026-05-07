.ensure_curve_butterfly_features <- function(DT, short_rate_col, mid_rate_col, long_rate_col) {
  .validate_market_dt(DT, c(short_rate_col, mid_rate_col, long_rate_col))

  if (!"curve_butterfly" %in% names(DT)) {
    out <- mapply(
      calc_curve_butterfly,
      short_rate = DT[[short_rate_col]],
      mid_rate = DT[[mid_rate_col]],
      long_rate = DT[[long_rate_col]]
    )
    data.table::set(DT, j = "curve_butterfly", value = as.numeric(out))
  }

  invisible("curve_butterfly")
}

.curve_butterfly_signal <- function(curve_butterfly, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(curve_butterfly))
  valid <- !is.na(curve_butterfly)
  out[valid & curve_butterfly > long_threshold] <- target_size
  out[valid & curve_butterfly < short_threshold] <- -target_size
  out
}

#' Curve-Butterfly Target Positions
#'
#' Generates a target-position path from a simple yield-curve butterfly measure.
#' Positive butterfly values target a long-butterfly exposure and negative
#' values target a short-butterfly exposure.
#'
#' @param DT A `data.table` containing the three curve points or a precomputed
#'   `curve_butterfly` column.
#' @param short_rate_col Short-end rate column name.
#' @param mid_rate_col Belly rate column name.
#' @param long_rate_col Long-end rate column name.
#' @param long_threshold Numeric threshold above which butterfly is treated as
#'   positive enough to go long.
#' @param short_threshold Numeric threshold below which butterfly is treated as
#'   negative enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing butterfly features are
#'   added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_curve_butterfly_tgt_pos <- function(DT, short_rate_col = "short_rate", mid_rate_col = "mid_rate", long_rate_col = "long_rate", long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_curve_butterfly_features(DT, short_rate_col = short_rate_col, mid_rate_col = mid_rate_col, long_rate_col = long_rate_col)
  } else {
    cols_needed <- "curve_butterfly"
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .curve_butterfly_signal(
    curve_butterfly = DT[["curve_butterfly"]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Curve-Butterfly Action Plan
#'
#' Applies the curve-butterfly rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_curve_butterfly_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_curve_butterfly_action_plan <- function(DT, state, short_rate_col = "short_rate", mid_rate_col = "mid_rate", long_rate_col = "long_rate", long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 608L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_curve_butterfly_tgt_pos(
    DT,
    short_rate_col = short_rate_col,
    mid_rate_col = mid_rate_col,
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

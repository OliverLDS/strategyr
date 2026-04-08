.ensure_relative_strength_features <- function(DT, x_col, y_col, n, use_log = TRUE) {
  .validate_market_dt(DT, c(x_col, y_col))

  rs_col <- paste0("relative_strength_", n)
  if (!rs_col %in% names(DT)) {
    calc_relative_strength(DT, x_col = x_col, y_col = y_col, ns = n, use_log = use_log)
  }

  invisible(rs_col)
}

.relative_strength_signal <- function(rs_value, long_threshold = 1.02, short_threshold = 0.98, target_size = 1.0) {
  out <- rep(0.0, length(rs_value))
  valid <- !is.na(rs_value)
  out[valid & rs_value >= long_threshold] <- target_size
  out[valid & rs_value <= short_threshold] <- -target_size
  out
}

#' Relative-Strength Target Positions
#'
#' Generates a simple benchmark-relative target-position path from rolling
#' relative-strength values. Strong relative performance targets a long
#' exposure, while weak relative performance targets a short exposure.
#'
#' @param DT A `data.table` containing the traded and benchmark price columns.
#' @param x_col Traded asset price column.
#' @param y_col Benchmark price column.
#' @param n Integer relative-strength lookback window.
#' @param long_threshold Numeric threshold above which the traded asset is
#'   considered relatively strong.
#' @param short_threshold Numeric threshold below which the traded asset is
#'   considered relatively weak.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing relative-strength
#'   features are added to `DT` in place.
#' @param use_log Logical; if `TRUE`, relative strength is based on log-return
#'   aggregation.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_relative_strength_tgt_pos <- function(DT, x_col = "close", y_col = "benchmark_close", n = 20L, long_threshold = 1.02, short_threshold = 0.98, target_size = 1.0, compute_features = TRUE, use_log = TRUE, debug = FALSE) {
  rs_col <- paste0("relative_strength_", n)

  if (compute_features) {
    cols_needed <- .ensure_relative_strength_features(DT, x_col = x_col, y_col = y_col, n = n, use_log = use_log)
  } else {
    cols_needed <- rs_col
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .relative_strength_signal(
    rs_value = DT[[rs_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Relative-Strength Action Plan
#'
#' Applies the relative-strength rule to the latest bar and translates the
#' resulting target exposure for the traded asset into an executable action
#' plan.
#'
#' @inheritParams strat_relative_strength_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_relative_strength_action_plan <- function(DT, state, x_col = "close", y_col = "benchmark_close", n = 20L, long_threshold = 1.02, short_threshold = 0.98, target_size = 1.0, compute_features = TRUE, use_log = TRUE, strat_id = 503L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_relative_strength_tgt_pos(
    DT,
    x_col = x_col,
    y_col = y_col,
    n = n,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size,
    compute_features = compute_features,
    use_log = use_log,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

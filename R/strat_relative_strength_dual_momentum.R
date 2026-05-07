.ensure_relative_strength_dual_momentum_features <- function(DT, x_col, y_col, rs_n, mom_n, use_log = TRUE) {
  .validate_market_dt(DT, c(x_col, y_col))

  rs_col <- paste0("relative_strength_", rs_n)
  mom_col <- paste0("momentum_", x_col, "_", mom_n)

  if (!rs_col %in% names(DT)) {
    calc_relative_strength(DT, x_col = x_col, y_col = y_col, ns = rs_n, use_log = use_log)
  }
  if (!mom_col %in% names(DT)) {
    data.table::set(DT, j = mom_col, value = .momentum_return(DT[[x_col]], mom_n, use_log = use_log))
  }

  invisible(c(rs_col, mom_col))
}

.relative_strength_dual_momentum_signal <- function(rs_value, mom_value, rs_long_threshold = 1.0, rs_short_threshold = 1.0, mom_long_threshold = 0, mom_short_threshold = 0, allow_short = TRUE, target_size = 1.0) {
  out <- rep(0.0, length(rs_value))
  valid <- !is.na(rs_value) & !is.na(mom_value)
  out[valid & rs_value >= rs_long_threshold & mom_value > mom_long_threshold] <- target_size
  if (allow_short) {
    out[valid & rs_value <= rs_short_threshold & mom_value < mom_short_threshold] <- -target_size
  }
  out
}

#' Relative-Strength-Dual-Momentum Target Positions
#'
#' Generates a benchmark-relative target-position path that requires both
#' positive relative strength and positive absolute momentum for long exposure,
#' and optionally allows short exposure when both are negative.
#'
#' @param DT A `data.table` containing the traded and benchmark price columns.
#' @param x_col Traded asset price column.
#' @param y_col Benchmark price column.
#' @param rs_n Integer relative-strength lookback window.
#' @param mom_n Integer absolute-momentum lookback window.
#' @param rs_long_threshold Numeric threshold above which the traded asset is
#'   considered relatively strong enough to go long.
#' @param rs_short_threshold Numeric threshold below which the traded asset is
#'   considered relatively weak enough to go short.
#' @param mom_long_threshold Numeric momentum threshold required for long
#'   exposure.
#' @param mom_short_threshold Numeric momentum threshold required for short
#'   exposure.
#' @param allow_short Logical; if `TRUE`, allows short targets.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing features are added to
#'   `DT` in place.
#' @param use_log Logical; if `TRUE`, relative strength and momentum are based
#'   on log-return aggregation.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_relative_strength_dual_momentum_tgt_pos <- function(DT, x_col = "close", y_col = "benchmark_close", rs_n = 20L, mom_n = 60L, rs_long_threshold = 1.0, rs_short_threshold = 1.0, mom_long_threshold = 0, mom_short_threshold = 0, allow_short = TRUE, target_size = 1.0, compute_features = TRUE, use_log = TRUE, debug = FALSE) {
  rs_col <- paste0("relative_strength_", rs_n)
  mom_col <- paste0("momentum_", x_col, "_", mom_n)

  if (compute_features) {
    cols_needed <- .ensure_relative_strength_dual_momentum_features(
      DT,
      x_col = x_col,
      y_col = y_col,
      rs_n = rs_n,
      mom_n = mom_n,
      use_log = use_log
    )
  } else {
    cols_needed <- c(rs_col, mom_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .relative_strength_dual_momentum_signal(
    rs_value = DT[[rs_col]],
    mom_value = DT[[mom_col]],
    rs_long_threshold = rs_long_threshold,
    rs_short_threshold = rs_short_threshold,
    mom_long_threshold = mom_long_threshold,
    mom_short_threshold = mom_short_threshold,
    allow_short = allow_short,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Relative-Strength-Dual-Momentum Action Plan
#'
#' Applies the dual-momentum relative-strength rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_relative_strength_dual_momentum_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_relative_strength_dual_momentum_action_plan <- function(DT, state, x_col = "close", y_col = "benchmark_close", rs_n = 20L, mom_n = 60L, rs_long_threshold = 1.0, rs_short_threshold = 1.0, mom_long_threshold = 0, mom_short_threshold = 0, allow_short = TRUE, target_size = 1.0, compute_features = TRUE, use_log = TRUE, strat_id = 504L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_relative_strength_dual_momentum_tgt_pos(
    DT,
    x_col = x_col,
    y_col = y_col,
    rs_n = rs_n,
    mom_n = mom_n,
    rs_long_threshold = rs_long_threshold,
    rs_short_threshold = rs_short_threshold,
    mom_long_threshold = mom_long_threshold,
    mom_short_threshold = mom_short_threshold,
    allow_short = allow_short,
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

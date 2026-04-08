.ensure_ratio_revert_features <- function(DT, x_col, y_col, z_n, sample = TRUE) {
  .validate_market_dt(DT, c(x_col, y_col))

  ratio_col <- paste0("ratio_", x_col, "_", y_col)
  z_col <- paste0("zscore_", ratio_col, "_", z_n)

  if (!ratio_col %in% names(DT)) {
    calc_ratio(DT, x_col = x_col, y_col = y_col, name = ratio_col)
  }
  if (!z_col %in% names(DT)) {
    calc_zscore(DT, cols = ratio_col, ns = z_n, sample = sample)
  }

  invisible(c(ratio_col, z_col))
}

#' Ratio-Reversion Target Positions
#'
#' Generates a simple mean-reversion target-position path for a traded asset
#' series relative to a benchmark series using the rolling z-score of the price
#' ratio.
#'
#' @inheritParams strat_pair_spread_revert_tgt_pos
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_ratio_revert_tgt_pos <- function(DT, x_col = "close", y_col = "benchmark_close", z_n = 20L, entry_z = 2, exit_z = 0.5, target_size = 1.0, compute_features = TRUE, sample = TRUE, debug = FALSE) {
  ratio_col <- paste0("ratio_", x_col, "_", y_col)
  z_col <- paste0("zscore_", ratio_col, "_", z_n)

  if (compute_features) {
    cols_needed <- .ensure_ratio_revert_features(DT, x_col = x_col, y_col = y_col, z_n = z_n, sample = sample)
  } else {
    cols_needed <- z_col
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .pair_spread_revert_signal(
    zscore_value = DT[[z_col]],
    entry_z = entry_z,
    exit_z = exit_z,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Ratio-Reversion Action Plan
#'
#' Applies the ratio mean-reversion rule to the latest bar and translates the
#' resulting target exposure for the traded asset into an executable action
#' plan.
#'
#' @inheritParams strat_ratio_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_ratio_revert_action_plan <- function(DT, state, x_col = "close", y_col = "benchmark_close", z_n = 20L, entry_z = 2, exit_z = 0.5, target_size = 1.0, compute_features = TRUE, sample = TRUE, strat_id = 502L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_ratio_revert_tgt_pos(
    DT,
    x_col = x_col,
    y_col = y_col,
    z_n = z_n,
    entry_z = entry_z,
    exit_z = exit_z,
    target_size = target_size,
    compute_features = compute_features,
    sample = sample,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

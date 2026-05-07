.ensure_roll_yield_mean_revert_features <- function(DT, date_col, rank_col, price_col, time_to_expiry_col, rank_front = 1L, rank_deferred = 2L, z_n = 60L) {
  feat <- .ensure_roll_yield_features(
    DT,
    date_col = date_col,
    rank_col = rank_col,
    price_col = price_col,
    time_to_expiry_col = time_to_expiry_col,
    rank_front = rank_front,
    rank_deferred = rank_deferred
  )
  work_dt <- feat$data
  roll_col <- feat$colname
  z_col <- paste0("zscore_", roll_col, "_", z_n)
  if (!z_col %in% names(work_dt)) {
    calc_zscore(work_dt, cols = roll_col, ns = z_n)
  }
  list(data = work_dt, colname = z_col)
}

.roll_yield_mean_revert_signal <- function(zscore_value, long_z = -1, short_z = 1, target_size = 1.0) {
  out <- rep(0.0, length(zscore_value))
  valid <- !is.na(zscore_value)
  out[valid & zscore_value <= long_z] <- target_size
  out[valid & zscore_value >= short_z] <- -target_size
  out
}

#' Roll-Yield-Mean-Revert Target Positions
#'
#' Generates a simple futures target-position path by fading extreme roll-yield
#' z-scores. Unusually low roll yield targets a long exposure and unusually high
#' roll yield targets a short exposure.
#'
#' @param DT A futures curve `data.table` panel when `compute_features = TRUE`,
#'   or a summarized per-date `data.table` containing the relevant
#'   `zscore_roll_yield_*` column when `compute_features = FALSE`.
#' @param date_col Date or timestamp column name for the curve panel.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param time_to_expiry_col Time-to-expiry column name expressed in years.
#' @param rank_front Integer front-contract rank.
#' @param rank_deferred Integer deferred-contract rank.
#' @param z_n Integer rolling window used to standardize roll yield.
#' @param long_z Numeric lower z-score threshold used for long entries.
#' @param short_z Numeric upper z-score threshold used for short entries.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the input panel is summarized
#'   to per-date roll-yield features before generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_roll_yield_mean_revert_tgt_pos <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L, z_n = 60L, long_z = -1, short_z = 1, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  z_col <- paste0("zscore_roll_yield_", rank_front, "_", rank_deferred, "_", z_n)

  if (compute_features) {
    feat <- .ensure_roll_yield_mean_revert_features(
      DT,
      date_col = date_col,
      rank_col = rank_col,
      price_col = price_col,
      time_to_expiry_col = time_to_expiry_col,
      rank_front = rank_front,
      rank_deferred = rank_deferred,
      z_n = z_n
    )
    work_dt <- feat$data
    z_col <- feat$colname
  } else {
    .validate_market_dt(DT, z_col)
    work_dt <- DT
  }

  tgt_pos <- .roll_yield_mean_revert_signal(
    zscore_value = work_dt[[z_col]],
    long_z = long_z,
    short_z = short_z,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = z_col))
  }
  tgt_pos
}

#' Roll-Yield-Mean-Revert Action Plan
#'
#' Applies the roll-yield mean-reversion rule to the latest summarized row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_roll_yield_mean_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_roll_yield_mean_revert_action_plan <- function(DT, state, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L, z_n = 60L, long_z = -1, short_z = 1, target_size = 1.0, compute_features = TRUE, strat_id = 614L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_roll_yield_mean_revert_tgt_pos(
    DT,
    date_col = date_col,
    rank_col = rank_col,
    price_col = price_col,
    time_to_expiry_col = time_to_expiry_col,
    rank_front = rank_front,
    rank_deferred = rank_deferred,
    z_n = z_n,
    long_z = long_z,
    short_z = short_z,
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

.ensure_roll_yield_momentum_features <- function(DT, date_col, rank_col, price_col, time_to_expiry_col, rank_front = 1L, rank_deferred = 2L, mom_n = 20L) {
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
  mom_col <- paste0("front_momentum_", mom_n)
  if (!mom_col %in% names(work_dt)) {
    data.table::set(work_dt, j = mom_col, value = .momentum_return(work_dt[["front_price"]], mom_n, use_log = TRUE))
  }
  list(data = work_dt, roll_col = roll_col, mom_col = mom_col)
}

.roll_yield_momentum_signal <- function(roll_yield, momentum_value, long_roll_threshold = 0, short_roll_threshold = 0, long_mom_threshold = 0, short_mom_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(roll_yield))
  valid <- !is.na(roll_yield) & !is.na(momentum_value)
  out[valid & roll_yield > long_roll_threshold & momentum_value > long_mom_threshold] <- target_size
  out[valid & roll_yield < short_roll_threshold & momentum_value < short_mom_threshold] <- -target_size
  out
}

#' Roll-Yield-Momentum Target Positions
#'
#' Generates a futures target-position path from annualized roll yield with a
#' front-contract momentum confirmation filter.
#'
#' @param DT A futures curve `data.table` panel when `compute_features = TRUE`,
#'   or a summarized per-date `data.table` containing the relevant roll-yield
#'   and front-momentum columns when `compute_features = FALSE`.
#' @param date_col Date or timestamp column name for the curve panel.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param time_to_expiry_col Time-to-expiry column name expressed in years.
#' @param rank_front Integer front-contract rank.
#' @param rank_deferred Integer deferred-contract rank.
#' @param mom_n Integer momentum lookback window applied to the summarized
#'   front-contract price.
#' @param long_roll_threshold Numeric roll-yield threshold required for long
#'   exposure.
#' @param short_roll_threshold Numeric roll-yield threshold required for short
#'   exposure.
#' @param long_mom_threshold Numeric momentum threshold required for long
#'   exposure.
#' @param short_mom_threshold Numeric momentum threshold required for short
#'   exposure.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the input panel is summarized
#'   before generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_roll_yield_momentum_tgt_pos <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L, mom_n = 20L, long_roll_threshold = 0, short_roll_threshold = 0, long_mom_threshold = 0, short_mom_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    feat <- .ensure_roll_yield_momentum_features(
      DT,
      date_col = date_col,
      rank_col = rank_col,
      price_col = price_col,
      time_to_expiry_col = time_to_expiry_col,
      rank_front = rank_front,
      rank_deferred = rank_deferred,
      mom_n = mom_n
    )
    work_dt <- feat$data
    roll_col <- feat$roll_col
    mom_col <- feat$mom_col
  } else {
    roll_col <- paste0("roll_yield_", rank_front, "_", rank_deferred)
    mom_col <- paste0("front_momentum_", mom_n)
    .validate_market_dt(DT, c(roll_col, mom_col))
    work_dt <- DT
  }

  tgt_pos <- .roll_yield_momentum_signal(
    roll_yield = work_dt[[roll_col]],
    momentum_value = work_dt[[mom_col]],
    long_roll_threshold = long_roll_threshold,
    short_roll_threshold = short_roll_threshold,
    long_mom_threshold = long_mom_threshold,
    short_mom_threshold = short_mom_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = roll_col))
  }
  tgt_pos
}

#' Roll-Yield-Momentum Action Plan
#'
#' Applies the roll-yield-plus-momentum rule to the latest summarized row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_roll_yield_momentum_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_roll_yield_momentum_action_plan <- function(DT, state, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L, mom_n = 20L, long_roll_threshold = 0, short_roll_threshold = 0, long_mom_threshold = 0, short_mom_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 607L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_roll_yield_momentum_tgt_pos(
    DT,
    date_col = date_col,
    rank_col = rank_col,
    price_col = price_col,
    time_to_expiry_col = time_to_expiry_col,
    rank_front = rank_front,
    rank_deferred = rank_deferred,
    mom_n = mom_n,
    long_roll_threshold = long_roll_threshold,
    short_roll_threshold = short_roll_threshold,
    long_mom_threshold = long_mom_threshold,
    short_mom_threshold = short_mom_threshold,
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

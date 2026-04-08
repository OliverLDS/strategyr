.ensure_roll_yield_features <- function(DT, date_col, rank_col, price_col, time_to_expiry_col, rank_front = 1L, rank_deferred = 2L) {
  out <- calc_roll_yield(
    DT,
    date_col = date_col,
    rank_col = rank_col,
    price_col = price_col,
    time_to_expiry_col = time_to_expiry_col,
    rank_front = rank_front,
    rank_deferred = rank_deferred
  )
  colname <- paste0("roll_yield_", rank_front, "_", rank_deferred)
  list(data = out, colname = colname)
}

.roll_yield_signal <- function(roll_yield, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(roll_yield))
  valid <- !is.na(roll_yield)
  out[valid & roll_yield > long_threshold] <- target_size
  out[valid & roll_yield < short_threshold] <- -target_size
  out
}

#' Roll-Yield Target Positions
#'
#' Generates a simple futures target-position path from annualized roll yield.
#' Positive roll yield targets a long exposure and negative roll yield targets a
#' short exposure.
#'
#' @param DT A futures curve `data.table` panel when `compute_features = TRUE`,
#'   or a summarized per-date `data.table` containing the relevant
#'   `roll_yield_*` column when `compute_features = FALSE`.
#' @param date_col Date or timestamp column name for the curve panel.
#' @param rank_col Contract-rank column name.
#' @param price_col Price column name.
#' @param time_to_expiry_col Time-to-expiry column name expressed in years.
#' @param rank_front Integer front-contract rank.
#' @param rank_deferred Integer deferred-contract rank.
#' @param long_threshold Numeric threshold above which roll yield is treated as
#'   positive enough to go long.
#' @param short_threshold Numeric threshold below which roll yield is treated as
#'   negative enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, the input panel is summarized
#'   to per-date roll-yield features before generating targets.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and summary data.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_roll_yield_tgt_pos <- function(DT, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
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
  } else {
    roll_col <- paste0("roll_yield_", rank_front, "_", rank_deferred)
    .validate_market_dt(DT, roll_col)
    work_dt <- DT
  }

  tgt_pos <- .roll_yield_signal(
    roll_yield = work_dt[[roll_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, data = work_dt, feature_col = roll_col))
  }
  tgt_pos
}

#' Roll-Yield Action Plan
#'
#' Applies the roll-yield rule to the latest summarized row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_roll_yield_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_roll_yield_action_plan <- function(DT, state, date_col = "date", rank_col = "contract_rank", price_col = "close", time_to_expiry_col = "time_to_expiry", rank_front = 1L, rank_deferred = 2L, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 604L, tol_pos = 0.1, debug = FALSE) {
  tgt_out <- strat_roll_yield_tgt_pos(
    DT,
    date_col = date_col,
    rank_col = rank_col,
    price_col = price_col,
    time_to_expiry_col = time_to_expiry_col,
    rank_front = rank_front,
    rank_deferred = rank_deferred,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size,
    compute_features = compute_features,
    debug = compute_features
  )

  if (compute_features) {
    latest_tgt_pos <- .latest_non_na(tgt_out$tgt_pos)
  } else {
    latest_tgt_pos <- .latest_non_na(tgt_out)
  }

  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

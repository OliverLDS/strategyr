.ensure_credit_spread_momentum_features <- function(DT, spread_col = "credit_spread", n = 20L, yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", momentum_col = NULL) {
  if (is.null(momentum_col)) {
    momentum_col <- paste0("credit_spread_mom_", n)
  }

  if (!spread_col %in% names(DT)) {
    .validate_market_dt(DT, c(yield_col, benchmark_yield_col))
    calc_credit_spread(DT, yield_col = yield_col, benchmark_yield_col = benchmark_yield_col, name = spread_col)
  } else {
    .validate_market_dt(DT, spread_col)
  }

  if (!momentum_col %in% names(DT)) {
    mom <- .momentum_return(DT[[spread_col]], n = n, use_log = FALSE)
    data.table::set(DT, j = momentum_col, value = mom)
  }

  invisible(c(spread_col, momentum_col))
}

.credit_spread_momentum_signal <- function(momentum_value, long_threshold = 0, short_threshold = 0, target_size = 1.0) {
  out <- rep(0.0, length(momentum_value))
  valid <- !is.na(momentum_value)
  out[valid & momentum_value >= long_threshold] <- target_size
  out[valid & momentum_value <= short_threshold] <- -target_size
  out
}

#' Credit-Spread-Momentum Target Positions
#'
#' Generates a target-position path from momentum in credit spreads. Positive
#' spread momentum targets a long widener exposure and negative spread momentum
#' targets a short or tightening exposure.
#'
#' @param DT A `data.table` containing credit-spread inputs or a precomputed
#'   spread column.
#' @param spread_col Credit-spread column name.
#' @param n Integer momentum lookback window.
#' @param yield_col Issuer-yield column used when `spread_col` must be built.
#' @param benchmark_yield_col Benchmark-yield column used when `spread_col` must
#'   be built.
#' @param momentum_col Optional precomputed momentum column name.
#' @param long_threshold Numeric threshold above which momentum is treated as
#'   positive enough to go long.
#' @param short_threshold Numeric threshold below which momentum is treated as
#'   negative enough to go short.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing spread and momentum
#'   features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_credit_spread_momentum_tgt_pos <- function(DT, spread_col = "credit_spread", n = 20L, yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", momentum_col = NULL, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (is.null(momentum_col)) {
    momentum_col <- paste0("credit_spread_mom_", n)
  }

  if (compute_features) {
    cols_needed <- .ensure_credit_spread_momentum_features(
      DT,
      spread_col = spread_col,
      n = n,
      yield_col = yield_col,
      benchmark_yield_col = benchmark_yield_col,
      momentum_col = momentum_col
    )
  } else {
    cols_needed <- c(spread_col, momentum_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .credit_spread_momentum_signal(
    momentum_value = DT[[momentum_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' Credit-Spread-Momentum Action Plan
#'
#' Applies the credit-spread momentum rule to the latest row and translates the
#' resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_credit_spread_momentum_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_credit_spread_momentum_action_plan <- function(DT, state, spread_col = "credit_spread", n = 20L, yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", momentum_col = NULL, long_threshold = 0, short_threshold = 0, target_size = 1.0, compute_features = TRUE, strat_id = 609L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_credit_spread_momentum_tgt_pos(
    DT,
    spread_col = spread_col,
    n = n,
    yield_col = yield_col,
    benchmark_yield_col = benchmark_yield_col,
    momentum_col = momentum_col,
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

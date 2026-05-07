.ensure_credit_spread_revert_features <- function(DT, signal_col = "excess_spread", z_n = 20L, spread_col = "credit_spread", benchmark_spread_col = "benchmark_spread", yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", sample = TRUE) {
  if (!signal_col %in% names(DT)) {
    if (identical(signal_col, "excess_spread")) {
      if (!spread_col %in% names(DT)) {
        calc_credit_spread(DT, yield_col = yield_col, benchmark_yield_col = benchmark_yield_col, name = spread_col)
      }
      calc_excess_spread(DT, spread_col = spread_col, benchmark_spread_col = benchmark_spread_col, name = signal_col)
    } else if (identical(signal_col, spread_col)) {
      if (!spread_col %in% names(DT)) {
        calc_credit_spread(DT, yield_col = yield_col, benchmark_yield_col = benchmark_yield_col, name = spread_col)
      }
    } else {
      .validate_market_dt(DT, signal_col)
    }
  } else {
    .validate_market_dt(DT, signal_col)
  }

  z_col <- paste0("zscore_", signal_col, "_", z_n)
  if (!z_col %in% names(DT)) {
    calc_zscore(DT, cols = signal_col, ns = z_n, sample = sample)
  }
  invisible(c(signal_col, z_col))
}

.credit_spread_revert_signal <- function(zscore_value, entry_z = 2, exit_z = 0.5, target_size = 1.0) {
  out <- rep(0.0, length(zscore_value))
  pos_now <- 0.0

  for (i in seq_along(zscore_value)) {
    z <- zscore_value[i]
    if (is.na(z)) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && z <= exit_z) {
      pos_now <- 0.0
    } else if (pos_now < 0 && z >= -exit_z) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (z >= entry_z) {
        pos_now <- target_size
      } else if (z <= -entry_z) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Credit-Spread-Reversion Target Positions
#'
#' Generates a mean-reversion target-position path from standardized credit
#' spread or excess-spread signals. Wide positive excess spread targets a long
#' tightening exposure, while strongly negative excess spread targets a short
#' exposure.
#'
#' @param DT A `data.table` containing credit-spread inputs or a precomputed
#'   signal column.
#' @param signal_col Credit signal column name. Defaults to `excess_spread`.
#' @param z_n Integer rolling window used for z-scores.
#' @param entry_z Numeric absolute z-score threshold used for entries.
#' @param exit_z Numeric absolute z-score threshold used for exits.
#' @param spread_col Credit-spread column used when `signal_col` must be built.
#' @param benchmark_spread_col Benchmark or sector spread column used when
#'   `signal_col = "excess_spread"` must be built.
#' @param yield_col Issuer-yield column used when `spread_col` must be built.
#' @param benchmark_yield_col Benchmark-yield column used when `spread_col` must
#'   be built.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing signal and z-score
#'   features are added to `DT` in place.
#' @param sample Logical; if `TRUE`, rolling z-score uses sample standard
#'   deviation.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_credit_spread_revert_tgt_pos <- function(DT, signal_col = "excess_spread", z_n = 20L, entry_z = 2, exit_z = 0.5, spread_col = "credit_spread", benchmark_spread_col = "benchmark_spread", yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", target_size = 1.0, compute_features = TRUE, sample = TRUE, debug = FALSE) {
  z_col <- paste0("zscore_", signal_col, "_", z_n)
  if (compute_features) {
    cols_needed <- .ensure_credit_spread_revert_features(
      DT,
      signal_col = signal_col,
      z_n = z_n,
      spread_col = spread_col,
      benchmark_spread_col = benchmark_spread_col,
      yield_col = yield_col,
      benchmark_yield_col = benchmark_yield_col,
      sample = sample
    )
  } else {
    cols_needed <- c(signal_col, z_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .credit_spread_revert_signal(
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

#' Credit-Spread-Reversion Action Plan
#'
#' Applies the credit-spread mean-reversion rule to the latest row and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_credit_spread_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_credit_spread_revert_action_plan <- function(DT, state, signal_col = "excess_spread", z_n = 20L, entry_z = 2, exit_z = 0.5, spread_col = "credit_spread", benchmark_spread_col = "benchmark_spread", yield_col = "ytm", benchmark_yield_col = "benchmark_ytm", target_size = 1.0, compute_features = TRUE, sample = TRUE, strat_id = 610L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_credit_spread_revert_tgt_pos(
    DT,
    signal_col = signal_col,
    z_n = z_n,
    entry_z = entry_z,
    exit_z = exit_z,
    spread_col = spread_col,
    benchmark_spread_col = benchmark_spread_col,
    yield_col = yield_col,
    benchmark_yield_col = benchmark_yield_col,
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

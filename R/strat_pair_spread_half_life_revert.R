.rolling_half_life_proxy <- function(x, n) {
  out <- rep(NA_real_, length(x))
  if (n < 3L || length(x) < n) {
    return(out)
  }

  for (i in seq.int(n, length(x))) {
    window_x <- x[seq.int(i - n + 1L, i)]
    if (anyNA(window_x) || length(unique(window_x)) < 2L) {
      next
    }
    y1 <- window_x[-length(window_x)]
    y2 <- window_x[-1L]
    fit <- try(stats::lm(y2 ~ y1), silent = TRUE)
    if (inherits(fit, "try-error")) {
      next
    }
    beta <- stats::coef(fit)[["y1"]]
    if (!is.finite(beta) || beta <= 0 || beta >= 1) {
      next
    }
    out[i] <- -log(2) / log(beta)
  }
  out
}

.ensure_pair_spread_half_life_revert_features <- function(DT, x_col, y_col, z_n, hl_n, sample = TRUE) {
  .validate_market_dt(DT, c(x_col, y_col))

  spread_col <- paste0("spread_", x_col, "_", y_col)
  z_col <- paste0("zscore_", spread_col, "_", z_n)
  hl_col <- paste0("half_life_", spread_col, "_", hl_n)

  if (!spread_col %in% names(DT)) {
    calc_spread(DT, x_col = x_col, y_col = y_col, name = spread_col)
  }
  if (!z_col %in% names(DT)) {
    calc_zscore(DT, cols = spread_col, ns = z_n, sample = sample)
  }
  if (!hl_col %in% names(DT)) {
    data.table::set(DT, j = hl_col, value = .rolling_half_life_proxy(DT[[spread_col]], hl_n))
  }

  invisible(c(spread_col, z_col, hl_col))
}

.pair_spread_half_life_revert_signal <- function(zscore_value, half_life_value, max_half_life = 20, long_z = -1, short_z = 1, exit_z = 0, target_size = 1.0) {
  out <- rep(0.0, length(zscore_value))
  pos_now <- 0.0

  for (i in seq_along(zscore_value)) {
    z <- zscore_value[i]
    hl <- half_life_value[i]

    if (is.na(z)) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && z >= exit_z) {
      pos_now <- 0.0
    } else if (pos_now < 0 && z <= exit_z) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0 && !is.na(hl) && hl <= max_half_life) {
      if (z <= long_z) {
        pos_now <- target_size
      } else if (z >= short_z) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' Pair-Spread-Half-Life-Reversion Target Positions
#'
#' Generates a mean-reversion target-position path for a traded asset series
#' relative to a benchmark series, but only when a rolling half-life proxy
#' suggests the spread is mean-reverting quickly enough.
#'
#' @param DT A `data.table` containing the traded and benchmark price columns.
#' @param x_col Traded asset price column.
#' @param y_col Benchmark or paired asset price column.
#' @param z_n Integer rolling window used for spread z-scores.
#' @param hl_n Integer rolling window used for the half-life proxy.
#' @param max_half_life Numeric maximum half-life allowed for entries.
#' @param long_z Numeric lower z-score threshold used for long entries.
#' @param short_z Numeric upper z-score threshold used for short entries.
#' @param exit_z Numeric z-score threshold used for exits.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing spread, z-score, and
#'   half-life features are added to `DT` in place.
#' @param sample Logical; if `TRUE`, rolling z-score uses sample standard
#'   deviation.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_pair_spread_half_life_revert_tgt_pos <- function(DT, x_col = "close", y_col = "benchmark_close", z_n = 20L, hl_n = 60L, max_half_life = 20, long_z = -1, short_z = 1, exit_z = 0, target_size = 1.0, compute_features = TRUE, sample = TRUE, debug = FALSE) {
  spread_col <- paste0("spread_", x_col, "_", y_col)
  z_col <- paste0("zscore_", spread_col, "_", z_n)
  hl_col <- paste0("half_life_", spread_col, "_", hl_n)

  if (compute_features) {
    cols_needed <- .ensure_pair_spread_half_life_revert_features(DT, x_col = x_col, y_col = y_col, z_n = z_n, hl_n = hl_n, sample = sample)
  } else {
    cols_needed <- c(z_col, hl_col)
    .validate_market_dt(DT, cols_needed)
  }

  tgt_pos <- .pair_spread_half_life_revert_signal(
    zscore_value = DT[[z_col]],
    half_life_value = DT[[hl_col]],
    max_half_life = max_half_life,
    long_z = long_z,
    short_z = short_z,
    exit_z = exit_z,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }

  tgt_pos
}

#' Pair-Spread-Half-Life-Reversion Action Plan
#'
#' Applies the pair-spread half-life mean-reversion rule to the latest bar and
#' translates the resulting target exposure for the traded asset into an
#' executable action plan.
#'
#' @inheritParams strat_pair_spread_half_life_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_pair_spread_half_life_revert_action_plan <- function(DT, state, x_col = "close", y_col = "benchmark_close", z_n = 20L, hl_n = 60L, max_half_life = 20, long_z = -1, short_z = 1, exit_z = 0, target_size = 1.0, compute_features = TRUE, sample = TRUE, strat_id = 510L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_pair_spread_half_life_revert_tgt_pos(
    DT,
    x_col = x_col,
    y_col = y_col,
    z_n = z_n,
    hl_n = hl_n,
    max_half_life = max_half_life,
    long_z = long_z,
    short_z = short_z,
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

.ensure_rsi_dynamic_threshold_revert_features <- function(DT, rsi_n, threshold_n, lower_q, upper_q, exit_level) {
  .validate_market_dt(DT, "close")

  rsi_col <- paste0("rsi_", rsi_n)
  if (!rsi_col %in% names(DT)) {
    calc_RSI(DT, ns = rsi_n, hs = NULL)
  }

  calc_rolling_quantile(DT, cols = rsi_col, ns = threshold_n, probs = c(lower_q, exit_level, upper_q))

  q_cols <- c(
    paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(lower_q)),
    paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(exit_level)),
    paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(upper_q))
  )
  invisible(c(rsi_col, q_cols))
}

.rsi_dynamic_threshold_revert_signal <- function(rsi, lower_threshold, exit_threshold, upper_threshold, target_size = 1.0) {
  out <- rep(0.0, length(rsi))
  pos_now <- 0.0

  for (i in seq_along(rsi)) {
    if (is.na(rsi[i]) || is.na(lower_threshold[i]) || is.na(exit_threshold[i]) || is.na(upper_threshold[i])) {
      out[i] <- pos_now
      next
    }

    if (pos_now > 0 && rsi[i] >= exit_threshold[i]) {
      pos_now <- 0.0
    } else if (pos_now < 0 && rsi[i] <= exit_threshold[i]) {
      pos_now <- 0.0
    }

    if (pos_now == 0.0) {
      if (rsi[i] <= lower_threshold[i]) {
        pos_now <- target_size
      } else if (rsi[i] >= upper_threshold[i]) {
        pos_now <- -target_size
      }
    }

    out[i] <- pos_now
  }

  out
}

#' RSI-Dynamic-Threshold-Reversion Target Positions
#'
#' Generates a mean-reversion target-position path from RSI with rolling
#' quantile thresholds. Long exposure is opened when RSI falls into the rolling
#' lower tail, short exposure is opened when RSI rises into the rolling upper
#' tail, and positions are closed once RSI reverts toward a rolling neutral
#' quantile.
#'
#' @param DT A candle `data.table`.
#' @param rsi_n Integer RSI window.
#' @param threshold_n Integer rolling window used to estimate dynamic
#'   thresholds.
#' @param lower_q Numeric lower quantile used for long entries.
#' @param upper_q Numeric upper quantile used for short entries.
#' @param exit_level Numeric rolling quantile used as the neutral exit level.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing RSI and rolling
#'   quantile features are added to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_rsi_dynamic_threshold_revert_tgt_pos <- function(DT, rsi_n = 14L, threshold_n = 252L, lower_q = 0.1, upper_q = 0.9, exit_level = 0.5, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    cols_needed <- .ensure_rsi_dynamic_threshold_revert_features(
      DT,
      rsi_n = rsi_n,
      threshold_n = threshold_n,
      lower_q = lower_q,
      upper_q = upper_q,
      exit_level = exit_level
    )
  } else {
    rsi_col <- paste0("rsi_", rsi_n)
    cols_needed <- c(
      rsi_col,
      paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(lower_q)),
      paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(exit_level)),
      paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(upper_q))
    )
    .validate_market_dt(DT, cols_needed)
  }

  rsi_col <- paste0("rsi_", rsi_n)
  lower_col <- paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(lower_q))
  exit_col <- paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(exit_level))
  upper_col <- paste0("quantile_", rsi_col, "_", threshold_n, "_", .suffix_num(upper_q))

  tgt_pos <- .rsi_dynamic_threshold_revert_signal(
    rsi = DT[[rsi_col]],
    lower_threshold = DT[[lower_col]],
    exit_threshold = DT[[exit_col]],
    upper_threshold = DT[[upper_col]],
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = cols_needed))
  }
  tgt_pos
}

#' RSI-Dynamic-Threshold-Reversion Action Plan
#'
#' Applies the dynamic-threshold RSI mean-reversion rule to the latest bar and
#' translates the resulting target exposure into an executable action plan.
#'
#' @inheritParams strat_rsi_dynamic_threshold_revert_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_rsi_dynamic_threshold_revert_action_plan <- function(DT, state, rsi_n = 14L, threshold_n = 252L, lower_q = 0.1, upper_q = 0.9, exit_level = 0.5, target_size = 1.0, compute_features = TRUE, strat_id = 315L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_rsi_dynamic_threshold_revert_tgt_pos(
    DT,
    rsi_n = rsi_n,
    threshold_n = threshold_n,
    lower_q = lower_q,
    upper_q = upper_q,
    exit_level = exit_level,
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

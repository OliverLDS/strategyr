.ensure_atr_breakout_features <- function(DT, n) {
  .validate_market_dt(DT, c("high", "low", "close"))

  atr_col <- paste0("atr_", n)
  if (!atr_col %in% names(DT)) {
    calc_ATR(DT, ns = n, hs = NULL)
  }

  invisible(atr_col)
}

.atr_breakout_signal <- function(close, atr, atr_mult = 1, target_size = 1.0) {
  prev_close <- .lag_num(close, 1)
  prev_atr <- .lag_num(atr, 1)
  out <- rep(0.0, length(close))
  pos_now <- 0.0

  for (i in seq_along(close)) {
    if (is.na(close[i]) || is.na(prev_close[i]) || is.na(prev_atr[i])) {
      out[i] <- pos_now
      next
    }

    if (close[i] >= prev_close[i] + atr_mult * prev_atr[i]) {
      pos_now <- target_size
    } else if (close[i] <= prev_close[i] - atr_mult * prev_atr[i]) {
      pos_now <- -target_size
    }

    out[i] <- pos_now
  }

  out
}

#' ATR-Breakout Target Positions
#'
#' Generates a simple breakout target-position path from close-to-close moves
#' measured against prior ATR. Upside moves larger than `atr_mult * ATR`
#' target a long exposure, while downside moves larger than the same threshold
#' target a short exposure.
#'
#' @param DT A candle `data.table`.
#' @param n Integer ATR window.
#' @param atr_mult Numeric ATR multiple used for the breakout threshold.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, missing ATR features are added
#'   to `DT` in place.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_atr_breakout_tgt_pos <- function(DT, n = 14L, atr_mult = 1, target_size = 1.0, compute_features = TRUE, debug = FALSE) {
  if (compute_features) {
    atr_col <- .ensure_atr_breakout_features(DT, n = n)
  } else {
    atr_col <- paste0("atr_", n)
    .validate_market_dt(DT, c("close", atr_col))
  }

  tgt_pos <- .atr_breakout_signal(
    close = DT[["close"]],
    atr = DT[[atr_col]],
    atr_mult = atr_mult,
    target_size = target_size
  )

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = c("close", atr_col)))
  }

  tgt_pos
}

#' ATR-Breakout Action Plan
#'
#' Applies the ATR breakout rule to the latest bar and translates the resulting
#' target exposure into an executable action plan.
#'
#' @inheritParams strat_atr_breakout_tgt_pos
#' @param state Named list describing the current trading state.
#' @param strat_id Integer strategy identifier recorded on generated actions.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_atr_breakout_action_plan <- function(DT, state, n = 14L, atr_mult = 1, target_size = 1.0, compute_features = TRUE, strat_id = 401L, tol_pos = 0.1, debug = FALSE) {
  tgt_pos <- strat_atr_breakout_tgt_pos(
    DT,
    n = n,
    atr_mult = atr_mult,
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

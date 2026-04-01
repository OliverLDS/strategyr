.validate_market_dt <- function(DT, required_cols) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(length(required_cols) > 0L)
  stopifnot(nrow(DT) > 0L)
  stopifnot(all(required_cols %in% names(DT)))
  invisible(TRUE)
}

.validate_trade_state <- function(state) {
  stopifnot(is.list(state))

  required_fields <- c(
    "ctr_size", "ctr_step", "lev", "last_px",
    "ctr_unit", "avg_price", "cash", "pos_dir"
  )
  stopifnot(all(required_fields %in% names(state)))

  state$tol_pos <- if (is.null(state$tol_pos)) 0 else state$tol_pos
  state$strat_id <- if (is.null(state$strat_id)) 0L else as.integer(state$strat_id)
  state
}

.latest_value <- function(x) {
  stopifnot(length(x) > 0L)
  x[[length(x)]]
}

.latest_non_na <- function(x) {
  stopifnot(length(x) > 0L)
  idx <- which(!is.na(x))
  if (length(idx) == 0L) {
    return(NA)
  }
  x[[idx[[length(idx)]]]]
}

.action_plan_from_tgt_pos <- function(latest_tgt_pos, state, strat_id, tol_pos = 0) {
  state <- .validate_trade_state(state)

  gen_action_plan_rcpp(
    ctr_size = state$ctr_size,
    ctr_step = state$ctr_step,
    lev = state$lev,
    last_px = state$last_px,
    ctr_unit = state$ctr_unit,
    avg_price = state$avg_price,
    cash = state$cash,
    tgt_pos = latest_tgt_pos,
    tol_pos = tol_pos,
    strat_id = as.integer(strat_id),
    pos_dir = as.integer(state$pos_dir)
  )
}

.normalize_ladder_idx <- function(idx) {
  if (is.integer(idx)) {
    return(idx)
  }
  as.integer(idx)
}

.ladder_bounce_signal <- function(idx, lower = 7L, upper = 13L, target_size = 1.0) {
  idx <- .normalize_ladder_idx(idx)
  out <- rep(0.0, length(idx))
  out[idx %in% c(lower, -lower)] <- target_size
  out[idx %in% c(upper, -upper)] <- -target_size
  out
}

.ladder_breakout_signal <- function(idx, lower = 7L, upper = 13L, target_size = 1.0) {
  idx <- .normalize_ladder_idx(idx)
  out <- rep(0.0, length(idx))
  out[!is.na(idx) & idx <= lower] <- -target_size
  out[!is.na(idx) & idx >= upper] <- target_size
  out
}

.cross_sectional_rank_allocator_weights <- function(signal, long_n = 1L, short_n = 0L, gross_exposure = 1.0) {
  n <- length(signal)
  out <- rep(0.0, n)
  valid <- which(!is.na(signal))
  if (!length(valid)) {
    return(out)
  }

  ord_desc <- valid[order(signal[valid], decreasing = TRUE)]
  ord_asc <- valid[order(signal[valid], decreasing = FALSE)]

  long_idx <- ord_desc[seq_len(min(long_n, length(ord_desc)))]
  short_idx <- if (short_n > 0L) ord_asc[seq_len(min(short_n, length(ord_asc)))] else integer()
  short_idx <- setdiff(short_idx, long_idx)

  if (length(long_idx)) {
    out[long_idx] <- gross_exposure / max(1L, length(long_idx))
  }
  if (length(short_idx)) {
    out[short_idx] <- -gross_exposure / max(1L, length(short_idx))
  }
  out
}

#' Cross-Sectional-Rank-Allocator Target Positions
#'
#' Generates per-row target weights from cross-sectional signal ranks within
#' each date. Top-ranked assets receive positive target weights and optionally
#' bottom-ranked assets receive negative target weights.
#'
#' @param DT A panel `data.table` containing `date`, `asset`, and a signal
#'   column.
#' @param date_col Date column used for cross-sectional grouping.
#' @param asset_col Asset identifier column.
#' @param signal_col Ranking signal column.
#' @param long_n Integer number of assets to allocate long each date.
#' @param short_n Integer number of assets to allocate short each date.
#' @param gross_exposure Numeric gross exposure allocated across each side.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target weights aligned with `DT`, or a list when
#'   `debug = TRUE`.
#' @export
strat_cross_sectional_rank_allocator_tgt_pos <- function(DT, date_col = "date", asset_col = "asset", signal_col = "score", long_n = 1L, short_n = 0L, gross_exposure = 1.0, debug = FALSE) {
  .validate_market_dt(DT, c(date_col, asset_col, signal_col))

  tgt_pos <- DT[, .cross_sectional_rank_allocator_weights(
    signal = get(signal_col),
    long_n = long_n,
    short_n = short_n,
    gross_exposure = gross_exposure
  ), by = date_col][["V1"]]

  if (debug) {
    return(list(tgt_pos = tgt_pos, feature_cols = c(date_col, asset_col, signal_col)))
  }
  tgt_pos
}

#' Cross-Sectional-Rank-Allocator Action Plan
#'
#' Applies the latest cross-sectional target weights to a portfolio state and
#' converts them into a portfolio-adjustment table plus order intents.
#'
#' @inheritParams strat_cross_sectional_rank_allocator_tgt_pos
#' @param portfolio_state A `data.table` containing current per-asset portfolio
#'   state.
#' @param equity Numeric total portfolio equity used to scale target weights
#'   into target notionals.
#' @param strat_id Integer strategy identifier recorded on generated order
#'   intents.
#' @param pricing_method Pricing method passed to `build_order_intents()`.
#'
#' @return A list with `adjustment_plan`, `order_intents`, and the latest target
#'   weights.
#' @export
strat_cross_sectional_rank_allocator_action_plan <- function(DT, portfolio_state, equity, date_col = "date", asset_col = "asset", signal_col = "score", long_n = 1L, short_n = 0L, gross_exposure = 1.0, strat_id = 507L, pricing_method = "market", debug = FALSE) {
  .validate_market_dt(DT, c(date_col, asset_col, signal_col))
  stopifnot(data.table::is.data.table(portfolio_state))

  work_dt <- data.table::copy(DT)
  tgt_pos <- strat_cross_sectional_rank_allocator_tgt_pos(
    work_dt,
    date_col = date_col,
    asset_col = asset_col,
    signal_col = signal_col,
    long_n = long_n,
    short_n = short_n,
    gross_exposure = gross_exposure,
    debug = FALSE
  )
  data.table::set(work_dt, j = "target_weight", value = tgt_pos)

  latest_date <- max(work_dt[[date_col]], na.rm = TRUE)
  latest_targets <- work_dt[get(date_col) == latest_date, .(asset = get(asset_col), target_weight)]

  plan_dt <- merge(
    data.table::copy(portfolio_state),
    latest_targets,
    by = "asset",
    all = TRUE
  )
  stopifnot(all(c("price", "current_units") %in% names(plan_dt)))
  plan_dt[is.na(target_weight), target_weight := 0.0]

  adjustment_plan <- plan_portfolio_adjustment(plan_dt, equity = equity)
  order_intents <- build_order_intents(adjustment_plan, pricing_method = pricing_method)
  if (nrow(order_intents)) {
    order_intents[, strat_id := strat_id]
  }

  out <- list(
    adjustment_plan = adjustment_plan,
    order_intents = order_intents,
    latest_target_weights = latest_targets
  )
  if (debug) {
    out$tgt_pos <- tgt_pos
  }
  out
}

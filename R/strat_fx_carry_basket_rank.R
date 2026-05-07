.fx_carry_basket_rank_weights <- function(signal, long_n = 1L, short_n = 1L, gross_exposure = 1.0) {
  .cross_sectional_rank_allocator_weights(
    signal = signal,
    long_n = long_n,
    short_n = short_n,
    gross_exposure = gross_exposure
  )
}

#' FX-Carry-Basket-Rank Target Positions
#'
#' Generates per-row target weights by ranking an FX carry signal across assets
#' within each date. Top-ranked assets receive positive weights and
#' bottom-ranked assets receive negative weights.
#'
#' @param DT A panel `data.table` already summarized to one row per asset-date.
#' @param date_col Date column used for cross-sectional grouping.
#' @param asset_col Asset identifier column.
#' @param signal_col FX carry signal column used for ranking.
#' @param long_n Integer number of assets to allocate long each date.
#' @param short_n Integer number of assets to allocate short each date.
#' @param gross_exposure Numeric gross exposure allocated across each side.
#' @param debug Logical; when `TRUE`, returns a list with the generated target
#'   vector and feature column names.
#'
#' @return A numeric vector of target positions, or a list when `debug = TRUE`.
#' @export
strat_fx_carry_basket_rank_tgt_pos <- function(DT, date_col = "date", asset_col = "asset", signal_col = "fx_carry", long_n = 1L, short_n = 1L, gross_exposure = 1.0, debug = FALSE) {
  .validate_market_dt(DT, c(date_col, asset_col, signal_col))

  tgt_pos <- DT[, .fx_carry_basket_rank_weights(
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

#' FX-Carry-Basket-Rank Action Plan
#'
#' Applies the latest cross-sectional FX carry target weights to a portfolio
#' state and converts them into a portfolio-adjustment table plus order intents.
#'
#' @inheritParams strat_fx_carry_basket_rank_tgt_pos
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
strat_fx_carry_basket_rank_action_plan <- function(DT, portfolio_state, equity, date_col = "date", asset_col = "asset", signal_col = "fx_carry", long_n = 1L, short_n = 1L, gross_exposure = 1.0, strat_id = 511L, pricing_method = "market", debug = FALSE) {
  .validate_market_dt(DT, c(date_col, asset_col, signal_col))
  stopifnot(data.table::is.data.table(portfolio_state))

  work_dt <- data.table::copy(DT)
  tgt_pos <- strat_fx_carry_basket_rank_tgt_pos(
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

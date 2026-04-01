#' Plan Portfolio Adjustments
#'
#' Converts target portfolio weights into per-asset rebalancing deltas using the
#' current portfolio state. This is a minimal multi-asset bridge between
#' quantitative allocation outputs and executable portfolio adjustments.
#'
#' @param portfolio_state A `data.table` containing at least `asset`, `price`,
#'   `current_units`, and `target_weight`.
#' @param equity Total portfolio equity used to scale target weights into target
#'   notionals.
#' @param contract_size_col Column name containing contract-size multipliers.
#' @param lot_step_col Column name containing minimum trade increments.
#'
#' @return A `data.table` with current, target, and delta notional/unit columns.
#' @export
plan_portfolio_adjustment <- function(portfolio_state, equity, contract_size_col = "contract_size", lot_step_col = "lot_step") {
  stopifnot(data.table::is.data.table(portfolio_state))
  stopifnot(is.numeric(equity), length(equity) == 1L, is.finite(equity))

  required_cols <- c("asset", "price", "current_units", "target_weight")
  stopifnot(all(required_cols %in% names(portfolio_state)))

  DT <- data.table::copy(portfolio_state)
  if (!contract_size_col %in% names(DT)) {
    DT[, (contract_size_col) := 1.0]
  }
  if (!lot_step_col %in% names(DT)) {
    DT[, (lot_step_col) := 1.0]
  }

  DT[, current_notional := current_units * get(contract_size_col) * price]
  DT[, target_notional := target_weight * equity]
  DT[, delta_notional := target_notional - current_notional]
  DT[, raw_target_units := target_notional / (get(contract_size_col) * price)]
  DT[, raw_delta_units := raw_target_units - current_units]
  DT[, target_units := round(raw_target_units / get(lot_step_col)) * get(lot_step_col)]
  DT[, delta_units := target_units - current_units]
  DT[]
}

#' Build Portfolio Order Intents
#'
#' Converts rebalancing deltas into a minimal order-intent table suitable for
#' downstream execution adapters.
#'
#' @param adjustment_plan A `data.table` produced by
#'   [plan_portfolio_adjustment()].
#' @param pricing_method Pricing method recorded on each order intent.
#'
#' @return A `data.table` containing one row per non-zero rebalance intent.
#' @export
build_order_intents <- function(adjustment_plan, pricing_method = "market") {
  stopifnot(data.table::is.data.table(adjustment_plan))
  stopifnot(all(c("asset", "price", "delta_units") %in% names(adjustment_plan)))

  DT <- data.table::copy(adjustment_plan)
  intents <- DT[delta_units != 0, .(
    asset,
    side = ifelse(delta_units > 0, "buy", "sell"),
    units = abs(delta_units),
    reference_price = price,
    pricing_method = pricing_method,
    intent_type = "rebalance"
  )]
  intents[]
}

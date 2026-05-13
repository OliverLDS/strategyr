#' Backtest Portfolio Target Weights
#'
#' Evaluates a panel of portfolio target weights with open-price rebalancing and
#' close-price mark-to-market. This is a minimal portfolio-level companion to
#' the single-instrument backtest engine: strategy logic supplies target weights,
#' while this helper applies path-dependent portfolio accounting.
#'
#' @param DT A long `data.table` panel containing date, asset, open, close, and
#'   target-weight columns.
#' @param date_col Date or timestamp column name.
#' @param asset_col Asset identifier column name.
#' @param open_col Open/execution price column name.
#' @param close_col Close/mark price column name.
#' @param target_weight_col Target portfolio-weight column name.
#' @param initial_equity Numeric starting equity.
#' @param fee_rt Proportional transaction fee applied to traded notional.
#' @param rebalance_tol_weight Absolute weight-difference tolerance below which
#'   rebalances are skipped.
#' @param contract_size_col Optional contract-size column name. Defaults to
#'   one when absent.
#' @param allow_short Logical; when `FALSE`, negative target weights error.
#' @param keep_positions Logical; when `TRUE`, return per-date position records.
#'
#' @return A list with `equity` and, when requested, `positions` tables.
#' @export
backtest_portfolio_weights <- function(
  DT,
  date_col = "date",
  asset_col = "asset",
  open_col = "open",
  close_col = "close",
  target_weight_col = "target_weight",
  initial_equity = 1.0,
  fee_rt = 0.0,
  rebalance_tol_weight = 0.0,
  contract_size_col = "contract_size",
  allow_short = TRUE,
  keep_positions = FALSE
) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(length(initial_equity) == 1L, is.finite(initial_equity), initial_equity > 0)
  stopifnot(length(fee_rt) == 1L, is.finite(fee_rt), fee_rt >= 0)
  stopifnot(length(rebalance_tol_weight) == 1L, is.finite(rebalance_tol_weight), rebalance_tol_weight >= 0)
  stopifnot(is.logical(allow_short), length(allow_short) == 1L)
  stopifnot(is.logical(keep_positions), length(keep_positions) == 1L)

  .validate_market_dt(DT, c(date_col, asset_col, open_col, close_col, target_weight_col))
  work_dt <- data.table::copy(DT)
  data.table::setorderv(work_dt, c(date_col, asset_col))
  if (!contract_size_col %in% names(work_dt)) {
    work_dt[, (contract_size_col) := 1.0]
  }
  if (!allow_short && any(work_dt[[target_weight_col]] < 0, na.rm = TRUE)) {
    stop("Negative target weights are not allowed when `allow_short = FALSE`.", call. = FALSE)
  }

  if (!keep_positions) {
    dates <- sort(unique(work_dt[[date_col]]))
    assets <- sort(unique(as.character(work_dt[[asset_col]])))
    core_dt <- data.table::copy(work_dt)
    core_dt[, .date_id := match(get(date_col), dates)]
    core_dt[, .asset_id := match(as.character(get(asset_col)), assets)]
    core <- backtest_portfolio_weights_core_cpp(
      date_id = core_dt$.date_id,
      asset_id = core_dt$.asset_id,
      open = core_dt[[open_col]],
      close = core_dt[[close_col]],
      target_weight = core_dt[[target_weight_col]],
      contract_size = core_dt[[contract_size_col]],
      initial_equity = initial_equity,
      fee_rt = fee_rt,
      rebalance_tol_weight = rebalance_tol_weight,
      n_assets = length(assets)
    )
    equity_dt <- data.table::as.data.table(core)
    equity_dt[, date := dates[date_id]]
    data.table::setcolorder(equity_dt, c("date", setdiff(names(equity_dt), "date")))
    equity_dt[, date_id := NULL]
    return(list(equity = equity_dt))
  }

  assets <- sort(unique(as.character(work_dt[[asset_col]])))
  units <- stats::setNames(rep(0.0, length(assets)), assets)
  cash <- initial_equity
  dates <- sort(unique(work_dt[[date_col]]))
  equity_rows <- vector("list", length(dates))
  position_rows <- if (keep_positions) vector("list", length(dates)) else NULL

  for (i in seq_along(dates)) {
    d <- dates[[i]]
    day_dt <- work_dt[get(date_col) == d]
    day_assets <- as.character(day_dt[[asset_col]])
    open_px <- stats::setNames(day_dt[[open_col]], day_assets)
    close_px <- stats::setNames(day_dt[[close_col]], day_assets)
    contract_size <- stats::setNames(day_dt[[contract_size_col]], day_assets)
    target_weight <- stats::setNames(data.table::fifelse(is.na(day_dt[[target_weight_col]]), 0, day_dt[[target_weight_col]]), day_assets)

    known_assets <- intersect(names(units), day_assets)
    open_equity <- cash + sum(units[known_assets] * contract_size[known_assets] * open_px[known_assets], na.rm = TRUE)
    if (!is.finite(open_equity) || open_equity <= 0) {
      stop("Portfolio equity became non-positive or non-finite.", call. = FALSE)
    }

    current_weight <- stats::setNames(rep(0.0, length(day_assets)), day_assets)
    current_weight[known_assets] <- units[known_assets] * contract_size[known_assets] * open_px[known_assets] / open_equity
    rebalance_assets <- day_assets[abs(target_weight[day_assets] - current_weight[day_assets]) > rebalance_tol_weight]

    target_notional <- target_weight[day_assets] * open_equity
    target_units <- target_notional / (contract_size[day_assets] * open_px[day_assets])
    delta_units <- target_units - units[day_assets]
    delta_units[setdiff(day_assets, rebalance_assets)] <- 0.0
    traded_notional <- abs(delta_units) * contract_size[day_assets] * open_px[day_assets]
    fee_paid <- sum(traded_notional, na.rm = TRUE) * fee_rt

    cash <- cash - sum(delta_units * contract_size[day_assets] * open_px[day_assets], na.rm = TRUE) - fee_paid
    units[day_assets] <- units[day_assets] + delta_units

    close_equity <- cash + sum(units[day_assets] * contract_size[day_assets] * close_px[day_assets], na.rm = TRUE)
    close_notional <- units[day_assets] * contract_size[day_assets] * close_px[day_assets]
    gross_exposure <- sum(abs(close_notional), na.rm = TRUE) / close_equity
    net_exposure <- sum(close_notional, na.rm = TRUE) / close_equity
    turnover <- sum(traded_notional, na.rm = TRUE) / open_equity

    equity_rows[[i]] <- data.table::data.table(
      date = d,
      equity = close_equity,
      cash = cash,
      gross_exposure = gross_exposure,
      net_exposure = net_exposure,
      turnover = turnover,
      fee_paid = fee_paid,
      n_assets = length(day_assets)
    )

    if (keep_positions) {
      position_rows[[i]] <- data.table::data.table(
        date = d,
        asset = day_assets,
        units = units[day_assets],
        open = open_px[day_assets],
        close = close_px[day_assets],
        target_weight = target_weight[day_assets],
        close_weight = close_notional / close_equity
      )
    }
  }

  out <- list(equity = data.table::rbindlist(equity_rows))
  if (keep_positions) {
    out$positions <- data.table::rbindlist(position_rows)
  }
  out
}

.validate_portfolio_daily_target_input <- function(ohlc, target_weights, date_col, asset_col, open_col, high_col, low_col, close_col, target_weight_col, rebalance_col) {
  .validate_portfolio_daily_ohlc(ohlc, date_col, asset_col, open_col, high_col, low_col, close_col)
  if (!data.table::is.data.table(target_weights)) {
    stop("`target_weights` must be a data.table.", call. = FALSE)
  }
  required <- c(date_col, asset_col, target_weight_col)
  .validate_market_dt(target_weights, required)
  if (anyNA(target_weights[[date_col]]) || anyNA(target_weights[[asset_col]])) {
    stop("Target weights cannot contain missing dates or asset identifiers.", call. = FALSE)
  }
  if (anyDuplicated(target_weights[, .(get(date_col), get(asset_col))])) {
    stop("Target weights must contain at most one row per date and asset.", call. = FALSE)
  }
  if (!is.numeric(target_weights[[target_weight_col]]) || any(!is.finite(target_weights[[target_weight_col]]))) {
    stop("Target weights must be finite numeric values.", call. = FALSE)
  }
  if (any(target_weights[[target_weight_col]] < 0)) {
    stop("This daily portfolio backtest supports long-only target weights.", call. = FALSE)
  }
  if (!is.null(rebalance_col) && rebalance_col %in% names(target_weights) &&
      (!is.logical(target_weights[[rebalance_col]]) || anyNA(target_weights[[rebalance_col]]))) {
    stop("`rebalance_col` must be a non-missing logical column when supplied.", call. = FALSE)
  }

  market_pairs <- unique(ohlc[, c(date_col, asset_col), with = FALSE])
  target_pairs <- unique(target_weights[, c(date_col, asset_col), with = FALSE])
  data.table::setnames(market_pairs, c(date_col, asset_col), c(".date", ".asset"))
  data.table::setnames(target_pairs, c(date_col, asset_col), c(".date", ".asset"))
  if (!data.table::fsetequal(market_pairs, target_pairs)) {
    stop("Target weights must contain exactly one row for every available OHLC date-asset pair.", call. = FALSE)
  }

  gross <- target_weights[, sum(get(target_weight_col)), by = date_col][["V1"]]
  if (any(gross > 1 + 1e-12)) {
    stop("Target weights must have gross exposure no greater than one on every date.", call. = FALSE)
  }
}

.strat_portfolio_daily_backtest_reference <- function(
  ohlc,
  target_weights,
  initial_cash = 1000000,
  fee_rt = 0.0005,
  rebalance_tolerance = 0,
  date_col = "date",
  asset_col = "asset",
  open_col = "open",
  high_col = "high",
  low_col = "low",
  close_col = "close",
  target_weight_col = "target_weight",
  rebalance_col = "rebalance_due"
) {
  stopifnot(length(initial_cash) == 1L, is.finite(initial_cash), initial_cash > 0)
  stopifnot(length(fee_rt) == 1L, is.finite(fee_rt), fee_rt >= 0)
  stopifnot(length(rebalance_tolerance) == 1L, is.finite(rebalance_tolerance), rebalance_tolerance >= 0)
  stopifnot(is.null(rebalance_col) || (is.character(rebalance_col) && length(rebalance_col) == 1L))
  .validate_portfolio_daily_target_input(
    ohlc, target_weights, date_col, asset_col, open_col, high_col, low_col,
    close_col, target_weight_col, rebalance_col
  )

  market_dt <- data.table::copy(ohlc)
  target_dt <- data.table::copy(target_weights)
  data.table::setnames(market_dt, c(date_col, asset_col, open_col, close_col), c(".date", ".asset", ".open", ".close"))
  data.table::setnames(target_dt, c(date_col, asset_col, target_weight_col), c(".date", ".asset", ".target_weight"))
  target_keep <- c(".date", ".asset", ".target_weight")
  use_rebalance_col <- !is.null(rebalance_col) && rebalance_col %in% names(target_dt)
  if (use_rebalance_col) {
    data.table::setnames(target_dt, rebalance_col, ".rebalance_eligible")
    target_keep <- c(target_keep, ".rebalance_eligible")
  }
  work_dt <- merge(market_dt, target_dt[, ..target_keep], by = c(".date", ".asset"), all = FALSE, sort = TRUE)
  if (!use_rebalance_col) {
    data.table::set(work_dt, j = ".rebalance_eligible", value = TRUE)
  }

  dates <- sort(unique(work_dt$.date))
  assets <- sort(unique(as.character(work_dt$.asset)))
  units <- stats::setNames(rep(0.0, length(assets)), assets)
  last_close <- stats::setNames(rep(NA_real_, length(assets)), assets)
  cash <- initial_cash
  equity_rows <- vector("list", length(dates))
  weight_rows <- vector("list", length(dates))
  rebalance_rows <- vector("list", length(dates))
  date_run <- rle(match(work_dt$.date, dates))
  row_end <- cumsum(date_run$lengths)
  row_start <- c(1L, head(row_end, -1L) + 1L)

  for (date_idx in seq_along(dates)) {
    date_value <- dates[[date_idx]]
    day_rows <- row_start[[date_idx]]:row_end[[date_idx]]
    day_dt <- work_dt[day_rows]
    day_assets <- as.character(day_dt$.asset)
    available <- assets %in% day_assets
    open_px <- stats::setNames(day_dt$.open, day_assets)
    close_px <- stats::setNames(day_dt$.close, day_assets)
    target_map <- stats::setNames(day_dt$.target_weight, day_assets)
    eligible_map <- stats::setNames(day_dt$.rebalance_eligible, day_assets)

    valuation_open <- last_close
    valuation_open[day_assets] <- open_px[day_assets]
    if (any(units != 0 & !is.finite(valuation_open))) {
      stop("A held asset has no available valuation price.", call. = FALSE)
    }
    open_equity <- cash + sum(units * valuation_open, na.rm = TRUE)
    if (!is.finite(open_equity) || open_equity <= 0) {
      stop("Portfolio equity became non-positive or non-finite.", call. = FALSE)
    }

    current_weight <- stats::setNames(rep(0.0, length(assets)), assets)
    current_weight[is.finite(valuation_open)] <- units[is.finite(valuation_open)] * valuation_open[is.finite(valuation_open)] / open_equity
    rebalance_assets <- day_assets[eligible_map[day_assets] & abs(target_map[day_assets] - current_weight[day_assets]) > rebalance_tolerance]
    delta_units <- stats::setNames(rep(0.0, length(assets)), assets)
    if (length(rebalance_assets)) {
      delta_units[rebalance_assets] <- target_map[rebalance_assets] * open_equity / open_px[rebalance_assets] - units[rebalance_assets]
    }

    sell_assets <- names(delta_units)[delta_units < 0]
    buy_assets <- names(delta_units)[delta_units > 0]
    sell_notional <- sum(-delta_units[sell_assets] * open_px[sell_assets], na.rm = TRUE)
    sell_fee <- sell_notional * fee_rt
    cash_after_sells <- cash + sell_notional - sell_fee
    planned_buy_notional <- sum(delta_units[buy_assets] * open_px[buy_assets], na.rm = TRUE)
    buy_scale <- if (planned_buy_notional > 0) min(1, max(0, cash_after_sells) / (planned_buy_notional * (1 + fee_rt))) else 1
    delta_units[buy_assets] <- delta_units[buy_assets] * buy_scale
    traded_notional <- sum(abs(delta_units) * open_px[names(delta_units)], na.rm = TRUE)
    fee_paid <- traded_notional * fee_rt
    cash <- cash - sum(delta_units * open_px[names(delta_units)], na.rm = TRUE) - fee_paid
    units <- units + delta_units

    last_close[day_assets] <- close_px[day_assets]
    if (any(units != 0 & !is.finite(last_close))) {
      stop("A held asset has no close valuation price.", call. = FALSE)
    }
    close_notional <- units * last_close
    close_equity <- cash + sum(close_notional, na.rm = TRUE)
    if (!is.finite(close_equity) || close_equity <= 0) {
      stop("Portfolio equity became non-positive or non-finite.", call. = FALSE)
    }
    realized_weight <- close_notional / close_equity
    realized_weight[!is.finite(realized_weight)] <- 0.0
    target_output <- stats::setNames(rep(NA_real_, length(assets)), assets)
    target_output[day_assets] <- target_map[day_assets]
    eligible_output <- stats::setNames(rep(FALSE, length(assets)), assets)
    eligible_output[day_assets] <- eligible_map[day_assets]
    stale_valuation <- !available & is.finite(last_close)
    turnover <- traded_notional / open_equity

    equity_rows[[date_idx]] <- data.table::data.table(
      date = date_value,
      equity = close_equity,
      cash = cash,
      cash_weight = cash / close_equity,
      gross_exposure = sum(abs(close_notional), na.rm = TRUE) / close_equity,
      net_exposure = sum(close_notional, na.rm = TRUE) / close_equity,
      turnover = turnover,
      fee_paid = fee_paid,
      available_assets = sum(available),
      unavailable_assets = sum(!available)
    )
    weight_rows[[date_idx]] <- data.table::data.table(
      date = date_value,
      asset = assets,
      target_weight = unname(target_output),
      realized_weight = unname(realized_weight),
      units = unname(units),
      valuation_price = unname(last_close),
      availability = ifelse(available, "available", "unavailable"),
      stale_valuation = stale_valuation,
      rebalance_eligible = unname(eligible_output)
    )
    rebalance_rows[[date_idx]] <- data.table::data.table(
      date = date_value,
      rebalance_eligible = any(eligible_map),
      traded_assets = sum(abs(delta_units) > 0),
      traded_notional = traded_notional,
      turnover = turnover,
      fee_paid = fee_paid,
      buy_scale = buy_scale,
      unavailable_assets = sum(!available)
    )
  }

  equity_dt <- data.table::rbindlist(equity_rows)
  equity_dt[, daily_return := c(0.0, equity[-1L] / equity[-.N] - 1)]
  data.table::setcolorder(equity_dt, c("date", "equity", "daily_return", setdiff(names(equity_dt), c("date", "equity", "daily_return"))))
  list(
    equity = equity_dt,
    weights = data.table::rbindlist(weight_rows),
    rebalances = data.table::rbindlist(rebalance_rows)
  )
}

#' Backtest Daily Portfolio Target Weights
#'
#' Executes a long-only daily target-weight panel with an explicit cash
#' residual. A target row is executed at that row's open; target generators
#' such as [strat_equal_weight_rebalance_target_weights()] already shift
#' completed-bar signals to that next eligible open. Missing market rows are
#' reported as unavailable, are never traded, and retain their last close only
#' as an explicitly flagged stale valuation.
#'
#' @param ohlc Long daily OHLC `data.table` with one row per available
#'   `date`-`asset` pair.
#' @param target_weights Long target-weight `data.table` with exactly one row
#'   for every available `date`-`asset` pair in `ohlc`.
#' @param initial_cash Initial portfolio cash.
#' @param fee_rt Proportional fee charged on traded notional.
#' @param rebalance_tolerance Absolute weight difference below which an
#'   otherwise eligible rebalance is skipped.
#' @param date_col,asset_col Date and asset identifier column names.
#' @param open_col,high_col,low_col,close_col Daily OHLC column names.
#' @param target_weight_col Target-weight column name.
#' @param rebalance_col Optional logical eligibility column in
#'   `target_weights`. When it exists, only `TRUE` rows may rebalance; when it
#'   is absent or `NULL`, every available target row is eligible.
#'
#' @return A named list of stable `data.table`s:
#'   * `equity`: daily equity, return, cash, exposure, turnover, fees, and
#'   availability counts;
#'   * `weights`: daily target and realized weights for the full known universe,
#'   with explicit `availability` and `stale_valuation` states;
#'   * `rebalances`: daily public-safe rebalance and cost summary.
#' @export
strat_portfolio_daily_backtest <- function(
  ohlc,
  target_weights,
  initial_cash = 1000000,
  fee_rt = 0.0005,
  rebalance_tolerance = 0,
  date_col = "date",
  asset_col = "asset",
  open_col = "open",
  high_col = "high",
  low_col = "low",
  close_col = "close",
  target_weight_col = "target_weight",
  rebalance_col = "rebalance_due"
) {
  stopifnot(length(initial_cash) == 1L, is.finite(initial_cash), initial_cash > 0)
  stopifnot(length(fee_rt) == 1L, is.finite(fee_rt), fee_rt >= 0)
  stopifnot(length(rebalance_tolerance) == 1L, is.finite(rebalance_tolerance), rebalance_tolerance >= 0)
  stopifnot(is.null(rebalance_col) || (is.character(rebalance_col) && length(rebalance_col) == 1L))
  .validate_portfolio_daily_target_input(
    ohlc, target_weights, date_col, asset_col, open_col, high_col, low_col,
    close_col, target_weight_col, rebalance_col
  )

  market_dt <- data.table::copy(ohlc)
  target_dt <- data.table::copy(target_weights)
  data.table::setnames(market_dt, c(date_col, asset_col, open_col, close_col), c(".date", ".asset", ".open", ".close"))
  data.table::setnames(target_dt, c(date_col, asset_col, target_weight_col), c(".date", ".asset", ".target_weight"))
  target_keep <- c(".date", ".asset", ".target_weight")
  use_rebalance_col <- !is.null(rebalance_col) && rebalance_col %in% names(target_dt)
  if (use_rebalance_col) {
    data.table::setnames(target_dt, rebalance_col, ".rebalance_eligible")
    target_keep <- c(target_keep, ".rebalance_eligible")
  }
  work_dt <- merge(market_dt, target_dt[, ..target_keep], by = c(".date", ".asset"), all = FALSE, sort = TRUE)
  if (!use_rebalance_col) {
    data.table::set(work_dt, j = ".rebalance_eligible", value = TRUE)
  }

  dates <- sort(unique(work_dt$.date))
  assets <- sort(unique(as.character(work_dt$.asset)))
  core <- strat_portfolio_daily_backtest_core_cpp(
    date_id = match(work_dt$.date, dates),
    asset_id = match(as.character(work_dt$.asset), assets),
    open = work_dt$.open,
    close = work_dt$.close,
    target_weight = work_dt$.target_weight,
    rebalance_eligible = work_dt$.rebalance_eligible,
    n_dates = length(dates),
    n_assets = length(assets),
    initial_cash = initial_cash,
    fee_rt = fee_rt,
    rebalance_tolerance = rebalance_tolerance
  )

  equity_dt <- data.table::data.table(
    date = dates,
    equity = core$equity,
    cash = core$cash,
    cash_weight = core$cash_weight,
    gross_exposure = core$gross_exposure,
    net_exposure = core$net_exposure,
    turnover = core$turnover,
    fee_paid = core$fee_paid,
    available_assets = core$available_assets,
    unavailable_assets = core$unavailable_assets
  )
  equity_dt[, daily_return := c(0.0, equity[-1L] / equity[-.N] - 1)]
  data.table::setcolorder(equity_dt, c("date", "equity", "daily_return", setdiff(names(equity_dt), c("date", "equity", "daily_return"))))

  date_index <- rep(seq_along(dates), each = length(assets))
  asset_index <- rep(seq_along(assets), times = length(dates))
  weights_dt <- data.table::data.table(
    date = dates[date_index],
    asset = assets[asset_index],
    target_weight = core$target_weight,
    realized_weight = core$realized_weight,
    units = core$units,
    valuation_price = core$valuation_price,
    availability = ifelse(as.logical(core$available), "available", "unavailable"),
    stale_valuation = as.logical(core$stale_valuation),
    rebalance_eligible = as.logical(core$rebalance_eligible)
  )
  rebalances_dt <- data.table::data.table(
    date = dates,
    rebalance_eligible = as.logical(core$rebalance_due),
    traded_assets = core$traded_assets,
    traded_notional = core$traded_notional,
    turnover = core$turnover,
    fee_paid = core$fee_paid,
    buy_scale = core$buy_scale,
    unavailable_assets = core$unavailable_assets
  )
  list(equity = equity_dt, weights = weights_dt, rebalances = rebalances_dt)
}

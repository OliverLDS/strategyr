library(data.table)
library(testthat)

make_daily_portfolio_backtest_ohlc <- function(n_dates = 40L, assets = c("AAA", "BBB", "CCC")) {
  dates <- as.Date("2024-01-01") + seq_len(n_dates) - 1L
  out <- data.table::CJ(date = dates, asset = assets)
  asset_idx <- match(out$asset, assets)
  day_idx <- as.integer(out$date - min(out$date))
  close <- 100 + asset_idx * 10 + day_idx * c(0.8, 0.25, -0.2)[asset_idx]
  out[, `:=`(
    open = close - 0.1,
    high = close + 0.2,
    low = close - 0.3,
    close = close
  )]
  out[]
}

test_that("daily portfolio backtest executes an eligible target at that open", {
  DT <- make_daily_portfolio_backtest_ohlc(n_dates = 3L, assets = c("AAA", "BBB"))
  DT[asset == "AAA", `:=`(open = c(100, 110, 121), close = c(100, 121, 133.1))]
  DT[asset == "BBB", `:=`(open = 100, close = 100)]
  DT[, high := pmax(open, close)]
  DT[, low := pmin(open, close)]
  targets <- DT[, .(
    date,
    asset,
    target_weight = data.table::fifelse(asset == "AAA", 1.0, 0.0),
    rebalance_due = date == min(date) + 1L
  )]

  out <- strat_portfolio_daily_backtest(DT, targets, initial_cash = 1000, fee_rt = 0)

  expect_equal(out$equity$equity, c(1000, 1100, 1210))
  expect_equal(out$weights[date == min(date) + 1L & asset == "AAA", units], 1000 / 110)
  expect_equal(out$rebalances$traded_assets, c(0L, 1L, 0L))
})

test_that("daily portfolio backtest returns stable schemas for allocation generators", {
  DT <- make_daily_portfolio_backtest_ohlc()
  target_sets <- list(
    strat_equal_weight_rebalance_target_weights(DT, rebalance_n = 5L),
    strat_inverse_volatility_allocation_target_weights(DT, vol_n = 5L, min_obs = 5L, rebalance_n = 5L),
    strat_cross_asset_trend_allocation_target_weights(DT, trend_n = 5L, vol_n = 3L, min_obs = 5L, rebalance_n = 5L)
  )

  for (targets in target_sets) {
    out <- strat_portfolio_daily_backtest(DT, targets, initial_cash = 100000, fee_rt = 0.0005)
    expect_s3_class(out$equity, "data.table")
    expect_s3_class(out$weights, "data.table")
    expect_s3_class(out$rebalances, "data.table")
    expect_true(all(c("date", "equity", "daily_return", "cash", "turnover", "fee_paid") %in% names(out$equity)))
    expect_true(all(c("date", "asset", "target_weight", "realized_weight", "availability") %in% names(out$weights)))
    expect_true(all(c("date", "traded_notional", "fee_paid", "buy_scale") %in% names(out$rebalances)))
    expect_true(all(is.finite(out$equity$equity)))
    expect_true(all(out$equity$cash >= -1e-8))
  }
})

test_that("daily portfolio backtest reports late assets as unavailable", {
  DT <- make_daily_portfolio_backtest_ohlc(n_dates = 10L, assets = c("AAA", "BBB"))
  first_date <- min(DT$date)
  DT <- DT[!(asset == "BBB" & date < first_date + 4L)]
  targets <- strat_equal_weight_rebalance_target_weights(DT, rebalance_n = 1L)

  out <- strat_portfolio_daily_backtest(DT, targets, initial_cash = 10000, fee_rt = 0)
  unavailable <- out$weights[date < first_date + 4L & asset == "BBB"]

  expect_true(nrow(unavailable) > 0L)
  expect_true(all(unavailable$availability == "unavailable"))
  expect_true(all(is.na(unavailable$target_weight)))
  expect_true(all(unavailable$units == 0))
  expect_true(all(out$equity[date < first_date + 4L, unavailable_assets] == 1L))
})

test_that("daily portfolio backtest is deterministic and validates target panels", {
  DT <- make_daily_portfolio_backtest_ohlc()
  targets <- strat_inverse_volatility_allocation_target_weights(DT, vol_n = 5L, min_obs = 5L, rebalance_n = 3L)
  one <- strat_portfolio_daily_backtest(DT, targets)
  two <- strat_portfolio_daily_backtest(DT[sample(.N)], targets[sample(.N)])

  expect_equal(one, two)
  expect_error(
    strat_portfolio_daily_backtest(DT, targets[-1L]),
    "exactly one row"
  )
  targets[1L, target_weight := Inf]
  expect_error(
    strat_portfolio_daily_backtest(DT, targets),
    "finite"
  )
})

test_that("daily portfolio execution timing reconciles with Tradesimr", {
  skip_if_not_installed("tradesimr")
  exchange_new <- getExportedValue("tradesimr", "sim_exchange_new")
  target_step <- getExportedValue("tradesimr", "sim_portfolio_target_step")
  market_step <- getExportedValue("tradesimr", "sim_portfolio_market_step")
  execution_config <- getExportedValue("tradesimr", "sim_portfolio_execution")

  DT <- make_daily_portfolio_backtest_ohlc(n_dates = 3L, assets = "AAA")
  DT[, `:=`(open = c(100, 110, 121), close = c(100, 121, 133.1))]
  DT[, `:=`(high = pmax(open, close), low = pmin(open, close))]
  targets <- DT[, .(date, asset, target_weight = 1.0, rebalance_due = date == min(date) + 1L)]
  strategyr_result <- strat_portfolio_daily_backtest(DT, targets, initial_cash = 1000, fee_rt = 0)

  exchange <- exchange_new(list(cash = 1000, lev = 1, mmr = 0, auto_register_assets = TRUE))
  execution <- execution_config(fee_rt = 0, lev = 1, mmr = 0, max_gross_weight = 1)
  bars <- data.table::copy(DT)
  bars[, `:=`(timestamp = as.POSIXct(date, tz = "UTC"), symbol = asset, asset_id = 1L)]
  target_step(
    exchange,
    agent_id = "strategyr-test",
    bars = bars[1L, .(timestamp, symbol, asset_id, open, high, low, close)],
    target_weights = c(AAA = 1.0),
    execution = execution,
    allowed_symbols = "AAA"
  )
  market_step(exchange, bars[2L, .(timestamp, symbol, asset_id, open, high, low, close)], execution = execution)

  fills <- exchange$portfolio_fills
  expect_equal(nrow(fills), 1L)
  expect_equal(as.Date(fills$timestamp[[1L]]), DT$date[[2L]])
  expect_equal(fills$price[[1L]], DT$open[[2L]])
  ours <- strategyr_result$weights[date == DT$date[[2L]] & asset == "AAA", units]
  expect_lte(abs(ours - fills$ctr_qty[[1L]]), 1)
})

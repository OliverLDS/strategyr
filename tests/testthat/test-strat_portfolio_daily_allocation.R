make_test_daily_portfolio_ohlc <- function(n_dates = 12L, assets = c("AAA", "BBB", "CCC")) {
  dates <- as.Date("2024-01-01") + seq_len(n_dates) - 1L
  out <- data.table::CJ(date = dates, asset = assets)
  asset_idx <- match(out$asset, assets)
  day_idx <- as.integer(out$date - min(out$date))
  close <- 100 + asset_idx * 10 + day_idx * c(1.0, 0.3, -0.5)[asset_idx]
  data.table::set(out, j = "close", value = close)
  data.table::set(out, j = "open", value = close - 0.1)
  data.table::set(out, j = "high", value = pmax(out$open, out$close) + 0.2)
  data.table::set(out, j = "low", value = pmin(out$open, out$close) - 0.2)
  out[]
}

test_that("equal-weight targets are aligned for next-open execution", {
  DT <- make_test_daily_portfolio_ohlc()
  out <- strat_equal_weight_rebalance_target_weights(DT, rebalance_n = 3L, min_obs = 1L)

  expect_true(all(c("target_weight", "eligible", "rebalance_due", "signal_date") %in% names(out)))
  expect_true(is.numeric(out$target_weight))
  expect_true(all(is.finite(out$target_weight)))
  expect_true(all(out[out$date == min(out$date), ]$target_weight == 0))
  expect_true(all(out[out$date == min(out$date) + 1, ]$target_weight == 1 / 3))
  expect_true(all(out[out$date == min(out$date) + 1, ]$rebalance_due))
  expect_equal(unique(out[out$date == min(out$date) + 1, ]$signal_date), min(DT$date))
})

test_that("daily portfolio targets do not use the current bar close", {
  DT <- make_test_daily_portfolio_ohlc()
  latest_date <- max(DT$date)
  baseline <- strat_equal_weight_rebalance_target_weights(DT, rebalance_n = 1L)
  changed <- data.table::copy(DT)
  idx <- which(changed$date == latest_date)
  data.table::set(changed, i = idx, j = "open", value = changed$open[idx] * 10)
  data.table::set(changed, i = idx, j = "close", value = changed$close[idx] * 10)
  data.table::set(changed, i = idx, j = "high", value = changed$high[idx] * 10)
  data.table::set(changed, i = idx, j = "low", value = changed$low[idx] * 10)
  revised <- strat_equal_weight_rebalance_target_weights(changed, rebalance_n = 1L)

  expect_equal(
    baseline[baseline$date == latest_date, ]$target_weight,
    revised[revised$date == latest_date, ]$target_weight
  )
})

test_that("allocation strategies handle late assets and insufficient history as cash", {
  DT <- make_test_daily_portfolio_ohlc(n_dates = 8L)
  DT <- DT[!(DT$asset == "CCC" & DT$date < min(DT$date) + 5L), ]
  out <- strat_inverse_volatility_allocation_target_weights(
    DT,
    vol_n = 2L,
    min_obs = 3L,
    rebalance_n = 1L,
    weight_cap = 1.0
  )

  first_ccc_date <- min(out[out$asset == "CCC", ]$date)
  expect_equal(out[out$asset == "CCC" & out$date == first_ccc_date, ]$target_weight, 0)
  expect_true(all(out[out$date <= min(out$date) + 2L, ]$target_weight == 0))
})

test_that("cross-asset trend remains cash when no asset has positive momentum", {
  DT <- make_test_daily_portfolio_ohlc()
  close <- 200 - as.integer(DT$date - min(DT$date))
  data.table::set(DT, j = "close", value = close)
  data.table::set(DT, j = "open", value = close - 0.1)
  data.table::set(DT, j = "high", value = close + 0.2)
  data.table::set(DT, j = "low", value = close - 0.3)
  out <- strat_cross_asset_trend_allocation_target_weights(
    DT,
    trend_n = 3L,
    vol_n = 2L,
    min_obs = 3L,
    rebalance_n = 1L
  )

  expect_true(all(out$target_weight == 0))
})

test_that("portfolio allocation caps gross and per-asset weights deterministically", {
  DT <- make_test_daily_portfolio_ohlc()
  out <- strat_inverse_volatility_allocation_target_weights(
    DT,
    vol_n = 2L,
    min_obs = 3L,
    rebalance_n = 1L,
    gross_exposure = 0.9,
    weight_cap = 0.3
  )
  repeat_out <- strat_inverse_volatility_allocation_target_weights(
    DT[sample(nrow(DT)), ],
    vol_n = 2L,
    min_obs = 3L,
    rebalance_n = 1L,
    gross_exposure = 0.9,
    weight_cap = 0.3
  )

  expect_true(all(out$target_weight <= 0.3 + 1e-12))
  expect_true(all(out[, sum(abs(target_weight)), by = date][["V1"]] <= 0.9 + 1e-12))
  expect_equal(out, repeat_out)
  expect_silent(backtest_portfolio_weights(out, allow_short = FALSE))
})

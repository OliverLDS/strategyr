library(testthat)
library(data.table)

make_mining_market <- function(close) {
  data.table(
    datetime = as.POSIXct("2024-01-01", tz = "UTC") + seq_along(close) * 86400,
    open = close,
    high = close * 1.01,
    low = close * 0.99,
    close = close
  )
}

toy_threshold_strategy <- function(DT, threshold = 0, target_size = 1) {
  out <- rep(0, nrow(DT))
  out[DT$close > threshold] <- target_size
  out
}

toy_asset_year_strategy <- function(DT, target_size = 1) {
  rep(target_size, nrow(DT))
}

toy_warmup_strategy <- function(DT, target_size = 1) {
  rep(if (DT$close[[1L]] < 90) target_size else 0, nrow(DT))
}

test_that("calc_backtest_performance computes Sortino-centered metrics", {
  perf <- calc_backtest_performance(c(100, 101, 99, 103), annualization = 252)

  expect_equal(perf$n_obs, 4)
  expect_true(is.finite(perf$total_return))
  expect_true(is.finite(perf$max_drawdown))
  expect_true("sortino" %in% names(perf))
})

test_that("calc_backtest_performance does not score partial invalid equity", {
  perf_na <- calc_backtest_performance(c(100, 110, NA_real_, 120), annualization = 252)
  perf_zero <- calc_backtest_performance(c(100, 110, 0, 120), annualization = 252)

  expect_equal(perf_na$n_obs, 4)
  expect_true(is.na(perf_na$sortino))
  expect_equal(perf_zero$total_return, -1)
  expect_equal(perf_zero$max_drawdown, 1)
  expect_equal(perf_zero$sortino, -Inf)
})

test_that("mine_strategy_params ranks parameter grids by Sortino", {
  DT <- make_mining_market(c(100, 101, 102, 103, 104, 105))

  res <- mine_strategy_params(
    DT,
    strategy_fun = toy_threshold_strategy,
    param_grid = list(threshold = c(99, 200), target_size = 1),
    strat_id = 901L,
    ctr_size = 1,
    ctr_step = 0.01,
    lev = 2,
    fee_rt = 0,
    tol_pos = 0,
    keep_paths = TRUE
  )

  expect_equal(nrow(res), 2)
  expect_equal(res$rank, 1:2)
  expect_equal(res$threshold[[1]], 99)
  expect_false(is.na(res$sortino[[1]]))
  expect_true(is.list(res$equity))
  expect_equal(length(res$equity[[1]]), nrow(DT))
})

test_that("mine_strategy_assets ranks assets with fixed strategy parameters", {
  up_dt <- make_mining_market(c(100, 101, 102, 103, 104, 105))
  down_dt <- make_mining_market(c(100, 99, 98, 97, 96, 95))

  res <- mine_strategy_assets(
    list(up = up_dt, down = down_dt),
    strategy_fun = toy_threshold_strategy,
    strategy_params = list(threshold = 0, target_size = 1),
    strat_id = 902L,
    ctr_size = 1,
    ctr_step = 0.01,
    lev = 2,
    fee_rt = 0,
    tol_pos = 0
  )

  expect_equal(nrow(res), 2)
  expect_equal(res$asset[[1]], "up")
  expect_true(res$sortino[[1]] >= res$sortino[[2]])
})

test_that("mine_strategy_asset_years ranks outperforming asset-year pairs", {
  make_year_market <- function(start_date, open, close) {
    data.table(
      datetime = as.POSIXct(start_date, tz = "UTC") + 0:2 * 86400,
      open = rep(open, 3),
      high = rep(max(open, close) * 1.01, 3),
      low = rep(min(open, close) * 0.99, 3),
      close = rep(close, 3)
    )
  }

  beat_dt <- rbind(
    make_year_market("2024-01-01", open = 100, close = 100),
    make_year_market("2025-01-01", open = 100, close = 100)
  )
  lag_dt <- rbind(
    make_year_market("2024-01-01", open = 100, close = 120),
    make_year_market("2025-01-01", open = 100, close = 120)
  )
  short_dt <- make_year_market("2024-01-01", open = 100, close = 100)[1:2]

  res <- mine_strategy_asset_years(
    market_data_list = list(SPY = beat_dt, AGG = lag_dt, IAU = short_dt),
    strategy_fun = toy_asset_year_strategy,
    param_grid = data.table(target_size = c(0, 1.5)),
    seed_assets = c("SPY", "AGG"),
    years = c(2024L, 2025L),
    min_year_rows = 3L,
    strat_id = 903L,
    ctr_size = 1,
    ctr_step = 0.01,
    lev = 10,
    fee_rt = 0,
    tol_pos = 0
  )

  expect_true(all(c("seed_params", "candidate_params", "asset_year_results") %in% names(res)))
  expect_true(nrow(res$candidate_params) >= 1L)
  expect_gt(nrow(res$asset_year_results), 0L)
  expect_true(all(res$asset_year_results$n_obs >= 3L))
  expect_false("IAU" %in% res$asset_year_results$asset)
  expect_true(all(res$asset_year_results$total_return > res$asset_year_results$buy_hold_total_return))
  expect_equal(res$asset_year_results$rank, seq_len(nrow(res$asset_year_results)))
})

test_that("mine_strategy_asset_years computes signals with warmup history", {
  DT <- data.table(
    datetime = as.POSIXct(c("2023-12-27", "2023-12-28", "2023-12-29", "2024-01-02", "2024-01-03", "2024-01-04"), tz = "UTC"),
    open = c(80, 80, 80, 100, 110, 120),
    high = c(81, 81, 81, 101, 111, 121),
    low = c(79, 79, 79, 99, 109, 119),
    close = c(80, 80, 80, 100, 110, 120)
  )

  warm_res <- mine_strategy_asset_years(
    market_data_list = list(SPY = DT),
    strategy_fun = toy_warmup_strategy,
    param_grid = data.table(target_size = c(1.5, 3, 5)),
    seed_assets = "SPY",
    seed_n_best = 3L,
    from = as.Date("2024-01-01"),
    years = 2024L,
    min_year_rows = 3L,
    warmup_days = 10L,
    strat_id = 904L,
    ctr_size = 1,
    ctr_step = 0.01,
    lev = 10,
    fee_rt = 0,
    tol_pos = 0,
    keep_paths = TRUE
  )

  cold_res <- mine_strategy_asset_years(
    market_data_list = list(SPY = DT),
    strategy_fun = toy_warmup_strategy,
    param_grid = data.table(target_size = c(1.5, 3, 5)),
    seed_assets = "SPY",
    seed_n_best = 3L,
    from = as.Date("2024-01-01"),
    years = 2024L,
    min_year_rows = 3L,
    warmup_days = 0L,
    strat_id = 904L,
    ctr_size = 1,
    ctr_step = 0.01,
    lev = 10,
    fee_rt = 0,
    tol_pos = 0,
    keep_paths = TRUE
  )

  expect_gt(nrow(warm_res$asset_year_results), 0L)
  expect_equal(nrow(cold_res$asset_year_results), 0L)
  expect_equal(warm_res$asset_year_results$warmup_n_obs[[1L]], 3L)
  expect_false(warm_res$asset_year_results$warmup_insufficient[[1L]])
  expect_lt(warm_res$asset_year_results$signal_start[[1L]], warm_res$asset_year_results$trade_start[[1L]])
  expect_true(all(warm_res$asset_year_results$tgt_pos[[1L]] > 0))
})

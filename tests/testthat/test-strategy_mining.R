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

test_that("calc_backtest_performance computes Sortino-centered metrics", {
  perf <- calc_backtest_performance(c(100, 101, 99, 103), annualization = 252)

  expect_equal(perf$n_obs, 4)
  expect_true(is.finite(perf$total_return))
  expect_true(is.finite(perf$max_drawdown))
  expect_true("sortino" %in% names(perf))
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

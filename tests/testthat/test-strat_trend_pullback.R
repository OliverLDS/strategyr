library(testthat)
library(data.table)

test_that("strat_trend_pullback_tgt_pos enters on RSI pullbacks within trend", {
  DT <- data.table(
    close = c(105, 105, 95, 95),
    ema_20 = c(100, 100, 100, 100),
    rsi_14 = c(35, 55, 65, 45)
  )

  tgt_pos <- strat_trend_pullback_tgt_pos(
    DT,
    trend_n = 20L,
    rsi_n = 14L,
    pullback_long = 40,
    pullback_short = 60,
    exit_rsi = 50,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_trend_pullback_action_plan uses latest target", {
  DT <- data.table(
    close = c(100, 95),
    ema_20 = c(100, 100),
    rsi_14 = c(50, 65)
  )
  state <- list(
    ctr_size = 1.0,
    ctr_step = 1.0,
    lev = 10.0,
    last_px = 95.0,
    ctr_unit = 0.0,
    avg_price = NaN,
    cash = 10000.0,
    pos_dir = 0L
  )

  plan <- strat_trend_pullback_action_plan(
    DT,
    state,
    trend_n = 20L,
    rsi_n = 14L,
    pullback_long = 40,
    pullback_short = 60,
    exit_rsi = 50,
    compute_features = FALSE,
    strat_id = 403L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 403L)
})

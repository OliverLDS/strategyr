library(testthat)
library(data.table)

test_that("strat_macd_cross_tgt_pos follows MACD spread sign", {
  DT <- data.table(
    macd_12_26 = c(-1, 0.5, -0.2),
    macd_signal_12_26_9 = c(0, 0, 0)
  )

  tgt_pos <- strat_macd_cross_tgt_pos(
    DT,
    fast = 12L,
    slow = 26L,
    signal = 9L,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 1, -1))
})

test_that("strat_macd_cross_action_plan uses latest target", {
  DT <- data.table(
    macd_12_26 = c(-1, 0.5),
    macd_signal_12_26_9 = c(0, 0)
  )
  state <- list(
    ctr_size = 1.0,
    ctr_step = 1.0,
    lev = 10.0,
    last_px = 100.0,
    ctr_unit = 0.0,
    avg_price = NaN,
    cash = 10000.0,
    pos_dir = 0L
  )

  plan <- strat_macd_cross_action_plan(
    DT,
    state,
    fast = 12L,
    slow = 26L,
    signal = 9L,
    compute_features = FALSE,
    strat_id = 304L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 304L)
})

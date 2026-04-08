library(testthat)
library(data.table)

test_that("strat_atr_breakout_tgt_pos reacts to ATR-sized moves", {
  DT <- data.table(
    close = c(100, 103, 101, 97),
    atr_14 = c(NA, 2, 2, 2)
  )

  tgt_pos <- strat_atr_breakout_tgt_pos(
    DT,
    n = 14L,
    atr_mult = 1,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(0, 0, -1, -1))
})

test_that("strat_atr_breakout_action_plan uses latest target", {
  DT <- data.table(
    close = c(100, 102, 105),
    atr_14 = c(NA, 2, 2)
  )
  state <- list(
    ctr_size = 1.0,
    ctr_step = 1.0,
    lev = 10.0,
    last_px = 105.0,
    ctr_unit = 0.0,
    avg_price = NaN,
    cash = 10000.0,
    pos_dir = 0L
  )

  plan <- strat_atr_breakout_action_plan(
    DT,
    state,
    n = 14L,
    atr_mult = 1,
    compute_features = FALSE,
    strat_id = 401L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 401L)
})

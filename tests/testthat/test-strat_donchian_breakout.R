library(testthat)
library(data.table)

test_that("strat_donchian_breakout_tgt_pos uses prior channel breaches", {
  DT <- data.table(
    close = c(100, 106, 104, 94),
    dc_high_20 = c(105, 105, 105, 105),
    dc_low_20 = c(95, 95, 95, 95)
  )

  tgt_pos <- strat_donchian_breakout_tgt_pos(
    DT,
    n = 20L,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(0, 1, 1, -1))
})

test_that("strat_donchian_breakout_action_plan uses latest target", {
  DT <- data.table(
    close = c(100, 106),
    dc_high_20 = c(105, 105),
    dc_low_20 = c(95, 95)
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

  plan <- strat_donchian_breakout_action_plan(
    DT,
    state,
    n = 20L,
    compute_features = FALSE,
    strat_id = 302L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 302L)
})

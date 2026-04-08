library(testthat)
library(data.table)

test_that("strat_vol_target_tgt_pos scales trend direction by realized vol", {
  DT <- data.table(
    close = c(100, 105, 95),
    ema_20 = c(100, 100, 100),
    rv_20 = c(0.2, 0.4, 0.1)
  )

  tgt_pos <- strat_vol_target_tgt_pos(
    DT,
    trend_n = 20L,
    rv_n = 20L,
    vol_target = 0.2,
    max_leverage = 1.0,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(0, 0.5, -1.0))
})

test_that("strat_vol_target_action_plan uses latest target", {
  DT <- data.table(
    close = c(100, 95),
    ema_20 = c(100, 100),
    rv_20 = c(0.2, 0.1)
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

  plan <- strat_vol_target_action_plan(
    DT,
    state,
    trend_n = 20L,
    rv_n = 20L,
    vol_target = 0.2,
    max_leverage = 1.0,
    compute_features = FALSE,
    strat_id = 402L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 402L)
})

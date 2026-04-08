library(testthat)
library(data.table)

test_that("strat_bollinger_revert_tgt_pos enters on touches and exits at mid", {
  DT <- data.table(
    close = c(89, 100, 111, 100),
    bb_mid_20 = c(100, 100, 100, 100),
    bb_high_20_2 = c(110, 110, 110, 110),
    bb_low_20_2 = c(90, 90, 90, 90)
  )

  tgt_pos <- strat_bollinger_revert_tgt_pos(
    DT,
    n = 20L,
    k = 2,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_bollinger_revert_action_plan uses latest target", {
  DT <- data.table(
    close = c(100, 111),
    bb_mid_20 = c(100, 100),
    bb_high_20_2 = c(110, 110),
    bb_low_20_2 = c(90, 90)
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

  plan <- strat_bollinger_revert_action_plan(
    DT,
    state,
    n = 20L,
    k = 2,
    compute_features = FALSE,
    strat_id = 301L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 301L)
})

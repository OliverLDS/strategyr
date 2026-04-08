library(testthat)
library(data.table)

test_that("strat_straddle_tgt_pos thresholds ATM front IV", {
  DT <- data.table(iv_atm_front = c(0.15, 0.30, 0.45))

  tgt_pos <- strat_straddle_tgt_pos(
    DT,
    long_iv_threshold = 0.2,
    short_iv_threshold = 0.4,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1))
})

test_that("strat_straddle_action_plan uses latest target", {
  DT <- data.table(iv_atm_front = c(0.30, 0.45))
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

  plan <- strat_straddle_action_plan(
    DT,
    state,
    compute_features = FALSE,
    strat_id = 703L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 703L)
})

library(testthat)
library(data.table)

test_that("strat_pair_spread_revert_tgt_pos follows spread z-score reversion", {
  DT <- data.table(zscore_spread_close_benchmark_close_20 = c(-2.5, -0.2, 2.5, 0.2))

  tgt_pos <- strat_pair_spread_revert_tgt_pos(
    DT,
    z_n = 20L,
    entry_z = 2,
    exit_z = 0.5,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_pair_spread_revert_action_plan uses latest target", {
  DT <- data.table(zscore_spread_close_benchmark_close_20 = c(0, 2.5))
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

  plan <- strat_pair_spread_revert_action_plan(
    DT,
    state,
    z_n = 20L,
    entry_z = 2,
    exit_z = 0.5,
    compute_features = FALSE,
    strat_id = 501L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 501L)
})

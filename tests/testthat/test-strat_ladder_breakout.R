library(testthat)
library(data.table)

test_that("strat_ladder_breakout_tgt_pos maps outer ladders to continuation targets", {
  DT <- data.table(ladder_index_180 = c(6L, 7L, 10L, 13L, 14L))

  tgt_pos <- strat_ladder_breakout_tgt_pos(DT, cycle_N = 180L, compute_ladder = FALSE, lower = 7L, upper = 13L)

  expect_equal(tgt_pos, c(-1, -1, 0, 1, 1))
})

test_that("strat_ladder_breakout_action_plan uses latest target", {
  DT <- data.table(ladder_index_180 = c(10L, 13L))
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

  plan <- strat_ladder_breakout_action_plan(DT, state, cycle_N = 180L, compute_ladder = FALSE, lower = 7L, upper = 13L, strat_id = 202L, tol_pos = 0)

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$action, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 202L)
})

library(testthat)
library(data.table)

test_that("strat_bollinger_squeeze_breakout_tgt_pos arms on squeeze and trades breakout", {
  DT <- data.table(
    close = c(100, 100.1, 100.2, 102, 101, 100),
    bb_mid_20 = c(100, 100, 100, 100.5, 100.5, 100.5),
    bb_high_20_2 = c(101, 101, 101, 101.5, 101.5, 101.5),
    bb_low_20_2 = c(99, 99, 99, 99.5, 99.5, 99.5),
    bb_width_20_2 = c(0.02, 0.02, 0.02, 0.04, 0.04, 0.04)
  )

  tgt_pos <- strat_bollinger_squeeze_breakout_tgt_pos(DT, bb_n = 20L, bb_k = 2, squeeze_width = 0.03, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 0, 0, 1, 1, 0))
})

test_that("strat_bollinger_squeeze_breakout_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(100, 101), bb_mid_20 = c(100, 100), bb_high_20_2 = c(101, 101), bb_low_20_2 = c(99, 99), bb_width_20_2 = c(0.02, 0.04))
  state <- make_test_state(last_px = 101)
  plan <- strat_bollinger_squeeze_breakout_action_plan(DT, state, bb_n = 20L, bb_k = 2, squeeze_width = 0.03, compute_features = FALSE, strat_id = 309L, tol_pos = 0)
  expect_true(is.list(plan))
})

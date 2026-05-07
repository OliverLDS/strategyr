library(testthat)
library(data.table)

test_that("strat_donchian_retest_breakout_tgt_pos waits for a retest before entry", {
  DT <- data.table(
    close = c(100, 106, 105, 107),
    dc_high_20 = c(105, 105, 105, 106),
    dc_low_20 = c(95, 95, 95, 95)
  )
  tgt_pos <- strat_donchian_retest_breakout_tgt_pos(DT, n = 20L, retest_buffer = 0, confirm_n = 3L, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 0, 1, 1))
})

test_that("strat_donchian_retest_breakout_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(100, 106, 105), dc_high_20 = c(105, 105, 105), dc_low_20 = c(95, 95, 95))
  plan <- strat_donchian_retest_breakout_action_plan(DT, make_test_state(), n = 20L, retest_buffer = 0, confirm_n = 3L, compute_features = FALSE, strat_id = 313L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 313L)
})

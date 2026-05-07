library(testthat)
library(data.table)

test_that("strat_atr_breakout_trailing_stop_tgt_pos enters on breakout and exits on trailing stop", {
  DT <- data.table(
    close = c(100, 103, 106, 104),
    atr_14 = c(1, 1, 1, 1)
  )

  tgt_pos <- strat_atr_breakout_trailing_stop_tgt_pos(DT, n = 14L, atr_mult = 1, trail_mult = 2, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 1, 1, 0))
})

test_that("strat_atr_breakout_trailing_stop_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(100, 103), atr_14 = c(1, 1))
  plan <- strat_atr_breakout_trailing_stop_action_plan(DT, make_test_state(), n = 14L, compute_features = FALSE, strat_id = 405L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 405L)
})

library(testthat)
library(data.table)

test_that("strat_macd_zero_line_tgt_pos gates MACD crosses by the zero line", {
  DT <- data.table(
    macd_12_26 = c(0.2, -0.2, 0.1),
    macd_signal_12_26_9 = c(0.1, -0.1, 0.2)
  )
  tgt_pos <- strat_macd_zero_line_tgt_pos(DT, fast = 12L, slow = 26L, signal = 9L, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, -1, 0))
})

test_that("strat_macd_zero_line_action_plan returns planner-shaped output", {
  DT <- data.table(macd_12_26 = c(-0.2, -0.3), macd_signal_12_26_9 = c(-0.1, -0.2))
  plan <- strat_macd_zero_line_action_plan(DT, make_test_state(), fast = 12L, slow = 26L, signal = 9L, compute_features = FALSE, strat_id = 310L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 310L)
})

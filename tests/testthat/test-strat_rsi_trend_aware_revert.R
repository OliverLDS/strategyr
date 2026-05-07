library(testthat)
library(data.table)

test_that("strat_rsi_trend_aware_revert_tgt_pos only enters with trend support", {
  DT <- data.table(
    close = c(101, 102, 99, 98, 100),
    ema_50 = c(100, 100, 100, 100, 100),
    rsi_14 = c(25, 55, 75, 45, 25)
  )

  tgt_pos <- strat_rsi_trend_aware_revert_tgt_pos(DT, rsi_n = 14L, trend_n = 50L, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, -1, 0, 0))
})

test_that("strat_rsi_trend_aware_revert_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(101, 101), ema_50 = c(100, 100), rsi_14 = c(25, 25))
  plan <- strat_rsi_trend_aware_revert_action_plan(DT, make_test_state(), rsi_n = 14L, trend_n = 50L, compute_features = FALSE, strat_id = 311L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 311L)
})

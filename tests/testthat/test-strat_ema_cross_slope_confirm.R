library(testthat)
library(data.table)

test_that("strat_ema_cross_slope_confirm_tgt_pos requires alignment and slope confirmation", {
  DT <- data.table(
    ema_20 = c(100, 101, 102, 101),
    ema_50 = c(101, 100.5, 101, 101.5)
  )

  tgt_pos <- strat_ema_cross_slope_confirm_tgt_pos(DT, fast = 20L, slow = 50L, slope_lag = 1L, compute_features = FALSE)
  dbg <- strat_ema_cross_slope_confirm_tgt_pos(DT, fast = 20L, slow = 50L, slope_lag = 1L, compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(0, 0, 1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_ema_cross_slope_confirm_action_plan returns planner-shaped output", {
  DT <- data.table(ema_20 = c(100, 101, 102), ema_50 = c(101, 100.5, 101))
  plan <- strat_ema_cross_slope_confirm_action_plan(DT, make_test_state(), fast = 20L, slow = 50L, compute_features = FALSE, strat_id = 104L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 104L)
})

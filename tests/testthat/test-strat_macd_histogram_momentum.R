library(testthat)
library(data.table)

test_that("strat_macd_histogram_momentum_tgt_pos requires sign and acceleration agreement", {
  DT <- data.table(macd_hist_12_26_9 = c(0.10, 0.20, -0.10, -0.30, 0.20))

  tgt_pos <- strat_macd_histogram_momentum_tgt_pos(DT, compute_features = FALSE)
  dbg <- strat_macd_histogram_momentum_tgt_pos(DT, compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(0, 1, -1, -1, 1))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_macd_histogram_momentum_action_plan returns planner-shaped output", {
  DT <- data.table(macd_hist_12_26_9 = c(-0.20, -0.30))
  plan <- strat_macd_histogram_momentum_action_plan(DT, make_test_state(), compute_features = FALSE, strat_id = 314L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 314L)
})

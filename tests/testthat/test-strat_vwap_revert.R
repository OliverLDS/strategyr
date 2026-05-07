library(testthat)
library(data.table)

test_that("strat_vwap_revert_tgt_pos trades back toward VWAP", {
  DT <- data.table(vwap_dev_20 = c(-0.02, -0.015, -0.001), order_imbalance = c(0, 0, 0))
  tgt_pos <- strat_vwap_revert_tgt_pos(DT, vwap_n = 20L, entry_dev = 0.01, exit_dev = 0.0025, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 1, 0))
})

test_that("strat_vwap_revert_action_plan returns planner-shaped output", {
  DT <- data.table(vwap_dev_20 = c(0, -0.02), order_imbalance = c(0, 0))
  state <- make_test_state()
  plan <- strat_vwap_revert_action_plan(DT, state, vwap_n = 20L, compute_features = FALSE, strat_id = 506L, tol_pos = 0)
  expect_true(is.list(plan))
})

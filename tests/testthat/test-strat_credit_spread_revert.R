library(testthat)
library(data.table)

test_that("strat_credit_spread_revert_tgt_pos mean reverts wide excess spread", {
  DT <- data.table(excess_spread = c(0, 0.03, 0.01), zscore_excess_spread_20 = c(0, 2.5, 0.2))
  tgt_pos <- strat_credit_spread_revert_tgt_pos(DT, signal_col = "excess_spread", z_n = 20L, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 1, 0))
})

test_that("strat_credit_spread_revert_action_plan returns planner-shaped output", {
  DT <- data.table(excess_spread = c(0, 0.03), zscore_excess_spread_20 = c(0, 2.5))
  state <- make_test_state()
  plan <- strat_credit_spread_revert_action_plan(DT, state, signal_col = "excess_spread", z_n = 20L, compute_features = FALSE, strat_id = 610L, tol_pos = 0)
  expect_true(is.list(plan))
})

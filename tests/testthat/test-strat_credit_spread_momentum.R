library(testthat)
library(data.table)

test_that("strat_credit_spread_momentum_tgt_pos follows spread momentum", {
  DT <- data.table(credit_spread = c(0.01, 0.02, 0.03), credit_spread_mom_2 = c(NA, NA, 0.02))
  tgt_pos <- strat_credit_spread_momentum_tgt_pos(DT, n = 2L, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 0, 1))
})

test_that("strat_credit_spread_momentum_action_plan returns planner-shaped output", {
  DT <- data.table(credit_spread = c(0.01, 0.03), credit_spread_mom_2 = c(NA, 0.02))
  state <- make_test_state()
  plan <- strat_credit_spread_momentum_action_plan(DT, state, n = 2L, compute_features = FALSE, strat_id = 609L, tol_pos = 0)
  expect_true(is.list(plan))
})

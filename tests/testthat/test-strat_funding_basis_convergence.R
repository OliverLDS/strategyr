library(testthat)
library(data.table)

test_that("strat_funding_basis_convergence_tgt_pos fades extreme z-scores", {
  DT <- data.table(zscore_funding_basis_signal_1m_20 = c(-2.5, -0.2, 2.5))
  tgt_pos <- strat_funding_basis_convergence_tgt_pos(DT, z_n = 20L, tenor_tag = "1m", compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, -1))
})

test_that("strat_funding_basis_convergence_action_plan returns planner-shaped output", {
  DT <- data.table(zscore_funding_basis_signal_1m_20 = c(0, -2.5))
  state <- make_test_state()
  plan <- strat_funding_basis_convergence_action_plan(DT, state, z_n = 20L, tenor_tag = "1m", compute_features = FALSE, strat_id = 611L, tol_pos = 0)
  expect_true(is.list(plan))
})

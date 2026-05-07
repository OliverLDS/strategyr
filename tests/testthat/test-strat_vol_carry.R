library(testthat)
library(data.table)

test_that("strat_vol_carry_tgt_pos follows implied-realized spread", {
  DT <- data.table(iv = c(0.2, 0.2, 0.2), rv_20 = c(0.1, 0.2, 0.3), iv_rv_spread_20 = c(0.1, 0, -0.1))
  tgt_pos <- strat_vol_carry_tgt_pos(DT, rv_n = 20L, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, -1))
})

test_that("strat_vol_carry_action_plan returns planner-shaped output", {
  DT <- data.table(iv = c(0.2, 0.2), rv_20 = c(0.1, 0.1), iv_rv_spread_20 = c(0.1, 0.1))
  state <- make_test_state()
  plan <- strat_vol_carry_action_plan(DT, state, rv_n = 20L, compute_features = FALSE, strat_id = 707L, tol_pos = 0)
  expect_true(is.list(plan))
})

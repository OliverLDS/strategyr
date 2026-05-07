library(testthat)
library(data.table)

test_that("strat_curve_butterfly_tgt_pos follows butterfly sign", {
  DT <- data.table(curve_butterfly = c(-0.01, 0, 0.02))
  tgt_pos <- strat_curve_butterfly_tgt_pos(DT, compute_features = FALSE)
  expect_equal(tgt_pos, c(-1, 0, 1))
})

test_that("strat_curve_butterfly_action_plan returns planner-shaped output", {
  DT <- data.table(curve_butterfly = c(0, 0.01))
  state <- make_test_state()
  plan <- strat_curve_butterfly_action_plan(DT, state, compute_features = FALSE, strat_id = 608L, tol_pos = 0)
  expect_true(is.list(plan))
})

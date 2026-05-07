library(testthat)
library(data.table)

test_that("strat_gamma_scalp_support_tgt_pos activates when realized exceeds implied", {
  DT <- data.table(T = c(30 / 252, 30 / 252, 30 / 252), gamma_scalp_edge_20 = c(-0.05, 0.01, 0.02))
  tgt_pos <- strat_gamma_scalp_support_tgt_pos(DT, rv_n = 20L, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 1, 1))
})

test_that("strat_gamma_scalp_support_action_plan can attach a hedge plan", {
  DT <- data.table(T = c(30 / 252, 30 / 252), gamma_scalp_edge_20 = c(0.01, 0.02))
  state <- make_test_state()
  out <- strat_gamma_scalp_support_action_plan(DT, state, rv_n = 20L, compute_features = FALSE, strat_id = 708L, tol_pos = 0, current_delta = 5, hedge_delta = 1)
  expect_true(is.list(out))
  expect_true("hedge_plan" %in% names(out))
})

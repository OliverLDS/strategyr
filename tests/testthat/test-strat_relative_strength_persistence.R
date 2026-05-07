library(testthat)
library(data.table)

test_that("strat_relative_strength_persistence_tgt_pos requires consecutive confirmation", {
  DT <- data.table(relative_strength_20 = c(1.03, 1.03, 0.97, 0.97, 1.03))
  tgt_pos <- strat_relative_strength_persistence_tgt_pos(DT, n = 20L, long_threshold = 1.02, short_threshold = 0.98, persist_n = 2L, compute_features = FALSE)
  expect_equal(tgt_pos, c(0, 1, 0, -1, 0))
})

test_that("strat_relative_strength_persistence_action_plan returns planner-shaped output", {
  DT <- data.table(relative_strength_20 = c(1.03, 1.03))
  plan <- strat_relative_strength_persistence_action_plan(DT, make_test_state(), n = 20L, persist_n = 2L, compute_features = FALSE, strat_id = 508L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 508L)
})

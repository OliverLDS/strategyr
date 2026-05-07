library(testthat)
library(data.table)

test_that("strat_curve_steepener_zscore_tgt_pos supports momentum and reversion modes", {
  DT <- data.table(zscore_curve_slope_252 = c(1.2, -1.2, 0.2))

  mom <- strat_curve_steepener_zscore_tgt_pos(DT, z_n = 252L, mode = "momentum", compute_features = FALSE)
  rev <- strat_curve_steepener_zscore_tgt_pos(DT, z_n = 252L, mode = "reversion", compute_features = FALSE)
  dbg <- strat_curve_steepener_zscore_tgt_pos(DT, z_n = 252L, mode = "momentum", compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(mom))
  expect_length(mom, nrow(DT))
  expect_equal(mom, c(1, -1, 0))
  expect_equal(rev, c(-1, 1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_curve_steepener_zscore_action_plan returns planner-shaped output", {
  DT <- data.table(zscore_curve_slope_252 = c(1.2, 1.2))

  plan <- strat_curve_steepener_zscore_action_plan(DT, make_test_state(), z_n = 252L, mode = "momentum", compute_features = FALSE, strat_id = 605L, tol_pos = 0)

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 605L)
})

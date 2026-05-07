library(testthat)
library(data.table)

test_that("strat_rsi_dynamic_threshold_revert_tgt_pos uses rolling quantile thresholds", {
  DT <- data.table(
    rsi_14 = c(15, 40, 85, 55),
    quantile_rsi_14_252_0p1 = c(20, 20, 20, 20),
    quantile_rsi_14_252_0p5 = c(50, 50, 50, 50),
    quantile_rsi_14_252_0p9 = c(80, 80, 80, 80)
  )

  tgt_pos <- strat_rsi_dynamic_threshold_revert_tgt_pos(DT, compute_features = FALSE)
  dbg <- strat_rsi_dynamic_threshold_revert_tgt_pos(DT, compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(1, 1, -1, -1))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_rsi_dynamic_threshold_revert exits at the rolling neutral quantile", {
  DT <- data.table(
    rsi_14 = c(15, 55, 85, 45),
    quantile_rsi_14_252_0p1 = c(20, 20, 20, 20),
    quantile_rsi_14_252_0p5 = c(50, 50, 50, 50),
    quantile_rsi_14_252_0p9 = c(80, 80, 80, 80)
  )

  tgt_pos <- strat_rsi_dynamic_threshold_revert_tgt_pos(DT, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_rsi_dynamic_threshold_revert_action_plan returns planner-shaped output", {
  DT <- data.table(
    rsi_14 = c(85, 85),
    quantile_rsi_14_252_0p1 = c(20, 20),
    quantile_rsi_14_252_0p5 = c(50, 50),
    quantile_rsi_14_252_0p9 = c(80, 80)
  )
  plan <- strat_rsi_dynamic_threshold_revert_action_plan(DT, make_test_state(), compute_features = FALSE, strat_id = 315L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 315L)
})

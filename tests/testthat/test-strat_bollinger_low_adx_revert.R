library(testthat)
library(data.table)

test_that("strat_bollinger_low_adx_revert_tgt_pos only reverts in low-ADX regimes", {
  DT <- data.table(
    close = c(98, 100, 102, 100),
    bb_mid_20 = c(100, 100, 100, 100),
    bb_high_20_2 = c(101, 101, 101, 101),
    bb_low_20_2 = c(99, 99, 99, 99),
    adx_14 = c(10, 10, 25, 10)
  )
  tgt_pos <- strat_bollinger_low_adx_revert_tgt_pos(DT, n = 20L, k = 2, adx_n = 14L, adx_max = 18, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, 0, 0))
})

test_that("strat_bollinger_low_adx_revert_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(98, 98), bb_mid_20 = c(100, 100), bb_high_20_2 = c(101, 101), bb_low_20_2 = c(99, 99), adx_14 = c(10, 10))
  plan <- strat_bollinger_low_adx_revert_action_plan(DT, make_test_state(), n = 20L, k = 2, adx_n = 14L, compute_features = FALSE, strat_id = 312L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 312L)
})

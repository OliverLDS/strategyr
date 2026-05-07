library(testthat)
library(data.table)

test_that("strat_bollinger_revert_rsi_tgt_pos enters and exits with RSI confirmation", {
  DT <- data.table(
    close = c(89, 95, 111, 99),
    bb_mid_20 = c(100, 100, 100, 100),
    bb_high_20_2 = c(110, 110, 110, 110),
    bb_low_20_2 = c(90, 90, 90, 90),
    rsi_14 = c(25, 55, 75, 45)
  )

  tgt_pos <- strat_bollinger_revert_rsi_tgt_pos(
    DT,
    bb_n = 20L,
    bb_k = 2,
    rsi_n = 14L,
    oversold = 30,
    overbought = 70,
    exit_level = 50,
    compute_features = FALSE
  )
  dbg <- strat_bollinger_revert_rsi_tgt_pos(DT, bb_n = 20L, bb_k = 2, rsi_n = 14L, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(1, 0, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_bollinger_revert_rsi_action_plan returns planner-shaped output", {
  DT <- data.table(
    close = c(111, 111),
    bb_mid_20 = c(100, 100),
    bb_high_20_2 = c(110, 110),
    bb_low_20_2 = c(90, 90),
    rsi_14 = c(75, 75)
  )

  plan <- strat_bollinger_revert_rsi_action_plan(DT, make_test_state(), bb_n = 20L, bb_k = 2, rsi_n = 14L, compute_features = FALSE, strat_id = 308L, tol_pos = 0)

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 308L)
})

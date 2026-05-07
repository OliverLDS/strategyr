library(testthat)
library(data.table)

test_that("strat_pair_spread_bollinger_tgt_pos enters and exits on spread bands", {
  DT <- data.table(
    spread_close_benchmark_close = c(-3, 0.5, 3, -0.5),
    spread_bb_mid_20 = c(0, 0, 0, 0),
    spread_bb_high_20_2 = c(2, 2, 2, 2),
    spread_bb_low_20_2 = c(-2, -2, -2, -2)
  )

  tgt_pos <- strat_pair_spread_bollinger_tgt_pos(DT, n = 20L, k = 2, compute_features = FALSE)
  dbg <- strat_pair_spread_bollinger_tgt_pos(DT, n = 20L, k = 2, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(1, 0, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_pair_spread_bollinger_action_plan returns planner-shaped output", {
  DT <- data.table(
    spread_close_benchmark_close = c(3, 3),
    spread_bb_mid_20 = c(0, 0),
    spread_bb_high_20_2 = c(2, 2),
    spread_bb_low_20_2 = c(-2, -2)
  )

  plan <- strat_pair_spread_bollinger_action_plan(DT, make_test_state(), n = 20L, k = 2, compute_features = FALSE, strat_id = 505L, tol_pos = 0)

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 505L)
})

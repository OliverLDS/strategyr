library(testthat)
library(data.table)

test_that("strat_pair_spread_half_life_revert_tgt_pos gates entries by half-life", {
  DT <- data.table(
    zscore_spread_close_benchmark_close_20 = c(-1.5, -1.5, 1.5, 1.5),
    half_life_spread_close_benchmark_close_60 = c(10, 25, 10, 10)
  )

  tgt_pos <- strat_pair_spread_half_life_revert_tgt_pos(
    DT,
    z_n = 20L,
    hl_n = 60L,
    compute_features = FALSE
  )
  dbg <- strat_pair_spread_half_life_revert_tgt_pos(
    DT,
    z_n = 20L,
    hl_n = 60L,
    compute_features = FALSE,
    debug = TRUE
  )

  expect_equal(tgt_pos, c(1, 1, -1, -1))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_pair_spread_half_life_revert exits on z-score reversion", {
  DT <- data.table(
    zscore_spread_close_benchmark_close_20 = c(-1.5, 0.2, 1.5, -0.2),
    half_life_spread_close_benchmark_close_60 = c(10, 10, 10, 10)
  )

  tgt_pos <- strat_pair_spread_half_life_revert_tgt_pos(
    DT,
    z_n = 20L,
    hl_n = 60L,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_pair_spread_half_life_revert_action_plan returns planner-shaped output", {
  DT <- data.table(
    zscore_spread_close_benchmark_close_20 = c(0, -1.5),
    half_life_spread_close_benchmark_close_60 = c(10, 10)
  )

  plan <- strat_pair_spread_half_life_revert_action_plan(
    DT,
    make_test_state(),
    z_n = 20L,
    hl_n = 60L,
    compute_features = FALSE,
    strat_id = 510L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 510L)
})

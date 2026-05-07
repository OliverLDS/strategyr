library(testthat)
library(data.table)

test_that("strat_rsi_divergence_tgt_pos opens long on bullish divergence", {
  DT <- data.table(
    close = c(100, 98, 95, 97, 99, 96, 94, 98, 100),
    rsi_14 = c(50, 40, 30, 45, 50, 42, 35, 55, 60)
  )

  tgt_pos <- strat_rsi_divergence_tgt_pos(
    DT,
    rsi_n = 14L,
    pivot_left = 1L,
    pivot_right = 1L,
    exit_level = 80,
    compute_features = FALSE
  )
  dbg <- strat_rsi_divergence_tgt_pos(
    DT,
    rsi_n = 14L,
    pivot_left = 1L,
    pivot_right = 1L,
    exit_level = 80,
    compute_features = FALSE,
    debug = TRUE
  )

  expect_equal(tgt_pos, c(0, 0, 0, 0, 0, 0, 0, 1, 1))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_rsi_divergence_tgt_pos opens short on bearish divergence", {
  DT <- data.table(
    close = c(100, 102, 105, 103, 101, 104, 106, 102, 100),
    rsi_14 = c(50, 60, 70, 55, 50, 60, 65, 45, 40)
  )

  tgt_pos <- strat_rsi_divergence_tgt_pos(
    DT,
    rsi_n = 14L,
    pivot_left = 1L,
    pivot_right = 1L,
    exit_level = 20,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(0, 0, 0, 0, 0, 0, 0, -1, -1))
})

test_that("strat_rsi_divergence_tgt_pos stays flat without divergence", {
  DT <- data.table(
    close = c(100, 98, 95, 97, 99, 96, 95, 98, 100),
    rsi_14 = c(50, 40, 30, 45, 50, 42, 28, 55, 60)
  )

  tgt_pos <- strat_rsi_divergence_tgt_pos(
    DT,
    rsi_n = 14L,
    pivot_left = 1L,
    pivot_right = 1L,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, rep(0, nrow(DT)))
})

test_that("strat_rsi_divergence_action_plan returns planner-shaped output", {
  DT <- data.table(
    close = c(100, 98, 95, 97, 99, 96, 94, 98, 100),
    rsi_14 = c(50, 40, 30, 45, 50, 42, 35, 55, 60)
  )

  plan <- strat_rsi_divergence_action_plan(
    DT,
    make_test_state(),
    rsi_n = 14L,
    pivot_left = 1L,
    pivot_right = 1L,
    exit_level = 80,
    compute_features = FALSE,
    strat_id = 316L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 316L)
})

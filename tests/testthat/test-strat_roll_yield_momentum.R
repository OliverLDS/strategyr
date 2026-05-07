library(testthat)
library(data.table)

test_that("strat_roll_yield_momentum_tgt_pos combines roll yield and momentum", {
  DT <- data.table(
    roll_yield_1_2 = c(0.03, -0.03, 0.01),
    front_momentum_20 = c(0.05, -0.05, -0.02)
  )

  tgt_pos <- strat_roll_yield_momentum_tgt_pos(DT, rank_front = 1L, rank_deferred = 2L, mom_n = 20L, compute_features = FALSE)
  dbg <- strat_roll_yield_momentum_tgt_pos(DT, rank_front = 1L, rank_deferred = 2L, mom_n = 20L, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(1, -1, 0))
  expect_named(dbg, c("tgt_pos", "data", "feature_col"))
})

test_that("strat_roll_yield_momentum_action_plan returns planner-shaped output", {
  DT <- data.table(roll_yield_1_2 = c(-0.03, -0.03), front_momentum_20 = c(-0.05, -0.05))

  plan <- strat_roll_yield_momentum_action_plan(
    DT,
    make_test_state(),
    rank_front = 1L,
    rank_deferred = 2L,
    mom_n = 20L,
    compute_features = FALSE,
    strat_id = 607L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 607L)
})

library(testthat)
library(data.table)

test_that("strat_roll_yield_mean_revert_tgt_pos fades z-score extremes on summarized input", {
  DT <- data.table(zscore_roll_yield_1_2_60 = c(-1.5, 0, 1.5))

  tgt_pos <- strat_roll_yield_mean_revert_tgt_pos(
    DT,
    rank_front = 1L,
    rank_deferred = 2L,
    z_n = 60L,
    compute_features = FALSE
  )
  dbg <- strat_roll_yield_mean_revert_tgt_pos(
    DT,
    rank_front = 1L,
    rank_deferred = 2L,
    z_n = 60L,
    compute_features = FALSE,
    debug = TRUE
  )

  expect_equal(tgt_pos, c(1, 0, -1))
  expect_named(dbg, c("tgt_pos", "data", "feature_col"))
})

test_that("strat_roll_yield_mean_revert_action_plan uses latest target on summarized input", {
  DT <- data.table(zscore_roll_yield_1_2_60 = c(0, -1.5))

  plan <- strat_roll_yield_mean_revert_action_plan(
    DT,
    make_test_state(),
    rank_front = 1L,
    rank_deferred = 2L,
    z_n = 60L,
    compute_features = FALSE,
    strat_id = 614L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 614L)
})

library(testthat)
library(data.table)

test_that("strat_ema_cross_adx_tgt_pos applies EMA direction and ADX gate", {
  DT <- data.table(
    ema_20 = c(101, 101, 99),
    ema_50 = c(100, 100, 100),
    adx_14 = c(10, 25, 25)
  )

  tgt_pos <- strat_ema_cross_adx_tgt_pos(DT, fast = 20L, slow = 50L, adx_n = 14L, adx_threshold = 20, compute_features = FALSE)
  dbg <- strat_ema_cross_adx_tgt_pos(DT, fast = 20L, slow = 50L, adx_n = 14L, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(0, 1, -1))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_ema_cross_adx_action_plan returns planner-shaped output", {
  DT <- data.table(ema_20 = c(101, 99), ema_50 = c(100, 100), adx_14 = c(25, 25))

  plan <- strat_ema_cross_adx_action_plan(
    DT,
    make_test_state(),
    fast = 20L,
    slow = 50L,
    adx_n = 14L,
    compute_features = FALSE,
    strat_id = 103L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 103L)
})

library(testthat)
library(data.table)

test_that("strat_iv_directional_overlay_tgt_pos confirm mode suppresses conflicting trades", {
  DT <- data.table(
    iv_skew = c(0.05, -0.05, 0.05),
    close = c(101, 99, 99),
    trend_ema_50 = c(100, 100, 100)
  )

  tgt_pos <- strat_iv_directional_overlay_tgt_pos(DT, trend_n = 50L, overlay_mode = "confirm", compute_features = FALSE)
  dbg <- strat_iv_directional_overlay_tgt_pos(DT, trend_n = 50L, overlay_mode = "confirm", compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(1, -1, 0))
  expect_named(dbg, c("tgt_pos", "data", "feature_col"))
})

test_that("strat_iv_directional_overlay_tgt_pos flip mode reverses conflicting trades", {
  DT <- data.table(
    iv_skew = c(0.05, -0.05),
    close = c(99, 101),
    trend_ema_50 = c(100, 100)
  )

  tgt_pos <- strat_iv_directional_overlay_tgt_pos(DT, trend_n = 50L, overlay_mode = "flip", compute_features = FALSE)
  expect_equal(tgt_pos, c(-1, 1))
})

test_that("strat_iv_directional_overlay_action_plan returns planner-shaped output", {
  DT <- data.table(
    iv_skew = c(0.05, 0.05),
    close = c(101, 101),
    trend_ema_50 = c(100, 100)
  )

  plan <- strat_iv_directional_overlay_action_plan(
    DT,
    make_test_state(),
    trend_n = 50L,
    overlay_mode = "confirm",
    compute_features = FALSE,
    strat_id = 710L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 710L)
})

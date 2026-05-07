library(testthat)
library(data.table)

test_that("strat_trend_pullback_atr_tgt_pos requires ATR pullback zone", {
  DT <- data.table(
    close = c(105, 106, 95, 96),
    ema_20 = c(100, 100, 100, 100),
    rsi_14 = c(35, 55, 65, 45),
    atr_14 = c(5, 5, 5, 5)
  )

  tgt_pos <- strat_trend_pullback_atr_tgt_pos(
    DT,
    trend_n = 20L,
    rsi_n = 14L,
    atr_n = 14L,
    min_atr_pullback = 0.5,
    max_atr_pullback = 3,
    compute_features = FALSE
  )
  dbg <- strat_trend_pullback_atr_tgt_pos(DT, trend_n = 20L, rsi_n = 14L, atr_n = 14L, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(1, 0, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_trend_pullback_atr_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(105, 105), ema_20 = c(100, 100), rsi_14 = c(35, 35), atr_14 = c(5, 5))

  plan <- strat_trend_pullback_atr_action_plan(DT, make_test_state(), trend_n = 20L, rsi_n = 14L, atr_n = 14L, compute_features = FALSE, strat_id = 404L, tol_pos = 0)

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 404L)
})

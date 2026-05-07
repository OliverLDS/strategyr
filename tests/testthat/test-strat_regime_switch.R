library(testthat)
library(data.table)

test_that("strat_regime_switch_tgt_pos switches between trend and reversion", {
  DT <- data.table(
    close = c(100, 98, 102),
    ema_20 = c(101, 99, 103),
    ema_50 = c(100, 100, 100),
    adx_14 = c(30, 10, 30),
    rv_20 = c(0.2, 0.2, 0.2),
    bb_mid_20 = c(100, 100, 100),
    bb_high_20_2 = c(101, 101, 101),
    bb_low_20_2 = c(99, 99, 99)
  )
  tgt_pos <- strat_regime_switch_tgt_pos(DT, fast = 20L, slow = 50L, adx_n = 14L, rv_n = 20L, bb_n = 20L, bb_k = 2, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 1, 1))
})

test_that("strat_regime_switch_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(100, 102), ema_20 = c(101, 103), ema_50 = c(100, 100), adx_14 = c(30, 30), rv_20 = c(0.2, 0.2), bb_mid_20 = c(100, 100), bb_high_20_2 = c(101, 101), bb_low_20_2 = c(99, 99))
  state <- make_test_state()
  plan <- strat_regime_switch_action_plan(DT, state, fast = 20L, slow = 50L, adx_n = 14L, rv_n = 20L, bb_n = 20L, bb_k = 2, compute_features = FALSE, strat_id = 105L, tol_pos = 0)
  expect_true(is.list(plan))
})

library(testthat)
library(data.table)

test_that("strat_vol_target_regime_floor_tgt_pos forces flat above volatility ceiling", {
  DT <- data.table(
    close = c(101, 101, 99),
    ema_20 = c(100, 100, 100),
    rv_20 = c(0.2, 0.5, 0.2)
  )
  tgt_pos <- strat_vol_target_regime_floor_tgt_pos(DT, trend_n = 20L, rv_n = 20L, vol_target = 0.2, max_leverage = 1, rv_ceiling = 0.4, compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, -1))
})

test_that("strat_vol_target_regime_floor_action_plan returns planner-shaped output", {
  DT <- data.table(close = c(101, 101), ema_20 = c(100, 100), rv_20 = c(0.2, 0.2))
  plan <- strat_vol_target_regime_floor_action_plan(DT, make_test_state(), trend_n = 20L, rv_n = 20L, compute_features = FALSE, strat_id = 406L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 406L)
})

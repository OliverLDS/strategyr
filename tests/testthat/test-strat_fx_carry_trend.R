library(testthat)
library(data.table)

test_that("strat_fx_carry_trend_tgt_pos requires both carry and trend agreement", {
  DT <- data.table(
    fx_carry_1m = c(0.02, 0.02, -0.02, -0.02),
    spot = c(101, 99, 99, 101),
    ema_50_spot = c(100, 100, 100, 100)
  )

  tgt_pos <- strat_fx_carry_trend_tgt_pos(DT, tenor_tag = "1m", trend_n = 50L, spot_col = "spot", compute_features = FALSE)
  expect_equal(tgt_pos, c(1, 0, -1, 0))
})

test_that("strat_fx_carry_trend_action_plan returns planner-shaped output", {
  DT <- data.table(fx_carry_1m = c(0.02, 0.02), spot = c(101, 101), ema_50_spot = c(100, 100))
  plan <- strat_fx_carry_trend_action_plan(DT, make_test_state(), tenor_tag = "1m", trend_n = 50L, spot_col = "spot", compute_features = FALSE, strat_id = 612L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 612L)
})

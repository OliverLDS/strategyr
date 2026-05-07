library(testthat)
library(data.table)

test_that("strat_iv_skew_realized_vol_confirm_tgt_pos requires skew and IV-RV confirmation", {
  DT <- data.table(
    iv_skew = c(0.05, -0.05, 0.05),
    iv_rv_spread_20 = c(0.03, -0.03, -0.01)
  )

  tgt_pos <- strat_iv_skew_realized_vol_confirm_tgt_pos(DT, rv_n = 20L, compute_features = FALSE)
  dbg <- strat_iv_skew_realized_vol_confirm_tgt_pos(DT, rv_n = 20L, compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(1, -1, 0))
  expect_named(dbg, c("tgt_pos", "data", "feature_col"))
})

test_that("strat_iv_skew_realized_vol_confirm_action_plan returns planner-shaped output", {
  DT <- data.table(
    iv_skew = c(0.05, 0.05),
    iv_rv_spread_20 = c(0.03, 0.03)
  )

  plan <- strat_iv_skew_realized_vol_confirm_action_plan(
    DT,
    make_test_state(),
    rv_n = 20L,
    compute_features = FALSE,
    strat_id = 709L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 709L)
})

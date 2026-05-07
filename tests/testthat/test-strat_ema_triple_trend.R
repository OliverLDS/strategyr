library(testthat)
library(data.table)

test_that("strat_ema_triple_trend_tgt_pos follows triple alignment", {
  DT <- data.table(
    ema_20 = c(103, 99, 101),
    ema_50 = c(102, 100, 102),
    ema_100 = c(101, 101, 100)
  )

  tgt_pos <- strat_ema_triple_trend_tgt_pos(DT, fast = 20L, mid = 50L, slow = 100L, compute_features = FALSE)
  dbg <- strat_ema_triple_trend_tgt_pos(DT, fast = 20L, mid = 50L, slow = 100L, compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(1, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_ema_triple_trend_action_plan returns planner-shaped output", {
  DT <- data.table(ema_20 = c(103, 104), ema_50 = c(102, 103), ema_100 = c(101, 102))
  plan <- strat_ema_triple_trend_action_plan(DT, make_test_state(), fast = 20L, mid = 50L, slow = 100L, compute_features = FALSE, strat_id = 106L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 106L)
})

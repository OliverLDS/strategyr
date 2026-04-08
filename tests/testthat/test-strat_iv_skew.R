library(testthat)
library(data.table)

test_that("strat_iv_skew_tgt_pos thresholds summarized skew", {
  DT <- data.table(iv_skew = c(-0.03, 0, 0.04))

  tgt_pos <- strat_iv_skew_tgt_pos(
    DT,
    long_threshold = 0.02,
    short_threshold = -0.02,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 0, 1))
})

test_that("strat_iv_skew_action_plan uses latest target", {
  DT <- data.table(iv_skew = c(0, 0.04))
  state <- list(
    ctr_size = 1.0,
    ctr_step = 1.0,
    lev = 10.0,
    last_px = 100.0,
    ctr_unit = 0.0,
    avg_price = NaN,
    cash = 10000.0,
    pos_dir = 0L
  )

  plan <- strat_iv_skew_action_plan(
    DT,
    state,
    compute_features = FALSE,
    strat_id = 701L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 701L)
})

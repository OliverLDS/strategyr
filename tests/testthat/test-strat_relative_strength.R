library(testthat)
library(data.table)

test_that("strat_relative_strength_tgt_pos thresholds relative strength", {
  DT <- data.table(relative_strength_20 = c(0.97, 1.00, 1.03))

  tgt_pos <- strat_relative_strength_tgt_pos(
    DT,
    n = 20L,
    long_threshold = 1.02,
    short_threshold = 0.98,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 0, 1))
})

test_that("strat_relative_strength_action_plan uses latest target", {
  DT <- data.table(relative_strength_20 = c(1.00, 1.03))
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

  plan <- strat_relative_strength_action_plan(
    DT,
    state,
    n = 20L,
    long_threshold = 1.02,
    short_threshold = 0.98,
    compute_features = FALSE,
    strat_id = 503L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 503L)
})

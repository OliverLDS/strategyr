library(testthat)
library(data.table)

test_that("strat_curve_steepener_tgt_pos follows slope sign", {
  DT <- data.table(curve_slope = c(-0.01, 0, 0.02))

  tgt_pos <- strat_curve_steepener_tgt_pos(
    DT,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 0, 1))
})

test_that("strat_curve_steepener_action_plan uses latest target", {
  DT <- data.table(curve_slope = c(0, -0.01))
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

  plan <- strat_curve_steepener_action_plan(
    DT,
    state,
    compute_features = FALSE,
    strat_id = 603L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, -1L)
  expect_equal(plan$actions[[1]]$strat, 603L)
})

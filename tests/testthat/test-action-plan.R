library(testthat)

test_that("gen_action_plan_rcpp: no position -> OPEN long", {
  plan <- gen_action_plan_rcpp(
    last_px   = 100,
    ctr_unit  = 0,          # no existing position
    avg_price = NaN,
    cash      = 10000,
    tgt_pos   = 1.0,        # full long exposure
    tol_pos   = 0,
    strat_id  = 42
  )

  expect_type(plan, "list")
  expect_equal(plan$n, 1L)
  expect_equal(length(plan$actions), 1L)

  a1 <- plan$actions[[1]]

  # basic fields
  expect_equal(a1$strat, 42L)
  expect_equal(a1$action_id, 1L)

  # enums encoded as integers
  # ActionCode: OPEN = 1
  expect_equal(a1$action, 1L)
  # Dir: LONG = 1
  expect_equal(a1$dir, 1L)
  # OrderType: MARKET = 0
  expect_equal(a1$type, 0L)

  # contract size should be positive and roughly 100 with defaults
  expect_gt(a1$ctr_qty, 0)
  expect_equal(a1$ctr_qty, 100, tolerance = 1e-8)

  # limit price should be NaN for market order
  expect_true(is.nan(a1$px))
})

test_that("gen_action_plan_rcpp: within tolerance -> no action", {
  plan <- gen_action_plan_rcpp(
    last_px   = 100,
    ctr_unit  = 0,
    avg_price = NaN,
    cash      = 10000,
    tgt_pos   = 0.1,   # small desired exposure
    tol_pos   = 0.5,   # tolerance larger than gap
    strat_id  = 1
  )

  # gap_pos = 0.1, tol_pos = 0.5 -> abs(gap_pos) < tol_pos => no trade
  expect_equal(plan$n, 0L)
  expect_equal(length(plan$actions), 0L)
})

test_that("gen_action_plan_rcpp: NA target -> no action", {
  plan <- gen_action_plan_rcpp(
    last_px   = 100,
    ctr_unit  = 0,
    avg_price = NaN,
    cash      = 10000,
    tgt_pos   = NA_real_,  # NA target => no opinion
    tol_pos   = 0,
    strat_id  = 1
  )

  expect_equal(plan$n, 0L)
  expect_equal(length(plan$actions), 0L)
})

test_that("gen_action_plan_rcpp: flip from long to short -> CLOSE + OPEN", {
  plan <- gen_action_plan_rcpp(
    last_px   = 100,
    ctr_unit  = 50,     # 50 contracts
    avg_price = 100,
    cash      = 10000,
    tgt_pos   = -1.0,   # want to go short
    tol_pos   = 0,
    strat_id  = 7,
    pos_dir   = 1       # <-- currently LONG
  )

  expect_equal(plan$n, 2L)
  expect_equal(length(plan$actions), 2L)

  a1 <- plan$actions[[1]]
  a2 <- plan$actions[[2]]

  # first action: CLOSE existing long
  # ActionCode: CLOSE = -1
  expect_equal(a1$action, -1L)
  # Dir: FLAT = 0
  expect_equal(a1$dir, 0L)
  expect_equal(a1$ctr_qty, 50)   # close all current contracts
  expect_equal(a1$action_id, 1L)
  expect_equal(a1$strat, 7L)

  # second action: OPEN new short
  # ActionCode: OPEN = 1
  expect_equal(a2$action, 1L)
  # Dir: SHORT = -1
  expect_equal(a2$dir, -1L)
  expect_equal(a2$action_id, 2L)
  expect_equal(a2$strat, 7L)

  # must open some non-zero contracts in the opposite direction
  expect_gt(a2$ctr_qty, 0)
})

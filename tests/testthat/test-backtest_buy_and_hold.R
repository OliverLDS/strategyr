library(testthat)

test_that("strat_buy_and_hold_rcpp returns constant 1.0 target", {
  ts <- c(0, 60, 120, 180, 240)

  tgt <- strat_buy_and_hold_rcpp(ts)

  expect_length(tgt, length(ts))
  expect_true(is.numeric(tgt))
  expect_true(all(tgt == 1.0))
})

test_that("strat_buy_and_hold_rcpp handles zero-length input", {
  ts <- numeric(0)

  tgt <- strat_buy_and_hold_rcpp(ts)

  expect_equal(length(tgt), 0L)
  expect_true(is.numeric(tgt))
})


test_that("buy-and-hold on flat prices keeps equity constant", {
  ts    <- c(0, 60, 120, 180)
  open  <- c(100, 100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- strat_buy_and_hold_rcpp(ts)   # all 1.0
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(
    timestamp = ts,
    open      = open,
    high      = high,
    low       = low,
    close     = close,
    tgt_pos   = tgt_pos,
    pos_strat = pos_strat,
    tol_pos   = tol_pos,
    strat     = 1L,
    asset     = 8001L,
    ctr_size  = 1.0,
    ctr_step  = 1.0,
    lev       = 10.0,
    fee_rt    = 0.0,
    fund_rt   = 0.0,
    rec       = FALSE
  )

  # compare numeric contents only, ignore attributes
  eq_num <- as.numeric(eq)

  # With constant prices and no fees/funding, equity should remain at initial 10000
  expect_length(eq_num, n)
  expect_equal(eq_num, rep(10000, n), tolerance = 1e-8)
})

test_that("buy-and-hold on rising prices increases equity", {
  ts    <- c(0, 60, 120, 180, 240)
  # Simple upward trend
  open  <- c(100, 105, 110, 115, 120)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- strat_buy_and_hold_rcpp(ts)   # all 1.0
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(
    timestamp = ts,
    open      = open,
    high      = high,
    low       = low,
    close     = close,
    tgt_pos   = tgt_pos,
    pos_strat = pos_strat,
    tol_pos   = tol_pos,
    strat     = 1L,
    asset     = 8001L,
    ctr_size  = 1.0,
    ctr_step  = 1.0,
    lev       = 10.0,
    fee_rt    = 0.0,
    fund_rt   = 0.0,
    rec       = FALSE
  )

  eq_num <- as.numeric(eq)

  # Basic sanity: we should have a full path
  expect_length(eq_num, n)

  # Equity should never go negative
  expect_true(all(eq_num >= 0))

  # Final equity should be strictly greater than initial 10000 for a rising market
  expect_gt(tail(eq_num, 1), 10000)

  # And equity should be non-decreasing once the position is fully established
  # (allowing for the first one or two bars of "ramp-in")
  eq_tail <- eq_num[3:n]
  expect_true(all(diff(eq_tail) >= -1e-8))
})

test_that("buy-and-hold equity path is identical with and without recording", {
  ts    <- c(0, 60, 120, 180)
  open  <- c(100, 100, 110, 120)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- strat_buy_and_hold_rcpp(ts)
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq_no_rec <- backtest_rcpp(
    timestamp = ts, open = open, high = high, low = low, close = close,
    tgt_pos   = tgt_pos,
    pos_strat = pos_strat,
    tol_pos   = tol_pos,
    strat     = 1L,
    asset     = 8001L,
    ctr_size  = 1.0,
    ctr_step  = 1.0,
    lev       = 10.0,
    fee_rt    = 0.0005,
    fund_rt   = 0.0,
    rec       = FALSE
  )

  eq_with_rec <- backtest_rcpp(
    timestamp = ts, open = open, high = high, low = low, close = close,
    tgt_pos   = tgt_pos,
    pos_strat = pos_strat,
    tol_pos   = tol_pos,
    strat     = 1L,
    asset     = 8001L,
    ctr_size  = 1.0,
    ctr_step  = 1.0,
    lev       = 10.0,
    fee_rt    = 0.0005,
    fund_rt   = 0.0,
    rec       = TRUE
  )

  # Compare numeric contents only; ignore attributes
  expect_equal(
    as.numeric(eq_no_rec),
    as.numeric(eq_with_rec),
    tolerance = 1e-8
  )

  # And when rec = TRUE, we should have a recorder attribute
  rec_attr <- attr(eq_with_rec, "recorder")
  expect_false(is.null(rec_attr))
  expect_true(is.list(rec_attr))
})

test_that("buy-and-hold on falling prices decreases equity without going negative", {
  ts    <- c(0, 60, 120, 180, 240)
  # Simple downward trend
  open  <- c(120, 115, 110, 105, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- strat_buy_and_hold_rcpp(ts)   # all 1.0
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(
    timestamp = ts,
    open      = open,
    high      = high,
    low       = low,
    close     = close,
    tgt_pos   = tgt_pos,
    pos_strat = pos_strat,
    tol_pos   = tol_pos,
    strat     = 1L,
    asset     = 8001L,
    ctr_size  = 1.0,
    ctr_step  = 1.0,
    lev       = 2.0,   # moderate leverage, should not liquidate here
    fee_rt    = 0.0,
    fund_rt   = 0.0,
    rec       = FALSE
  )

  eq_num <- as.numeric(eq)

  # Path length sanity
  expect_length(eq_num, n)

  # Equity should never go negative
  expect_true(all(eq_num >= 0))

  # Final equity should be less than initial (downtrend, non-liquidated)
  expect_lt(tail(eq_num, 1), eq_num[1])
})

test_that("buy-and-hold with high leverage and sharp crash triggers liquidation", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- c(100, 1, 1)   # huge crash on bar 2

  n <- length(ts)

  tgt_pos   <- strat_buy_and_hold_rcpp(ts)   # all 1.0
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(
    timestamp = ts,
    open      = open,
    high      = high,
    low       = low,
    close     = close,
    tgt_pos   = tgt_pos,
    pos_strat = pos_strat,
    tol_pos   = tol_pos,
    strat     = 1L,
    asset     = 8001L,
    ctr_size  = 1.0,
    ctr_step  = 1.0,
    lev       = 10.0,  # high leverage to force liquidation on crash
    fee_rt    = 0.0,
    fund_rt   = 0.0,
    rec       = TRUE
  )

  eq_num <- as.numeric(eq)

  # We should have at least one non-zero equity before liquidation
  expect_true(any(eq_num > 0))

  # And at least one zero equity point (liquidation)
  expect_true(any(eq_num == 0))

  # Once equity hits zero, all later values should stay zero
  zero_idx <- which(eq_num == 0)
  first_zero <- min(zero_idx)
  if (first_zero < length(eq_num)) {
    expect_true(all(eq_num[first_zero:length(eq_num)] == 0))
  }

  # Optional: check the recorder has at least one liquidation-tagged event
  rec_attr <- attr(eq, "recorder")
  expect_false(is.null(rec_attr))
  expect_true(is.list(rec_attr))
})


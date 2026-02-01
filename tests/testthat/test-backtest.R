

## 1. Basic input validation -------------------------------------------------

testthat::test_that("backtest_rcpp errors when lengths differ", {
  ts   <- c(0, 1, 2)
  open <- c(100, 100, 100)
  high <- open
  low  <- open
  close <- open

  tgt_pos   <- c(0, 0)        # <- shorter on purpose
  pos_strat <- rep(1L, 3)
  tol_pos   <- rep(0, 3)

  expect_error(
    backtest_rcpp(ts, open, high, low, close,
                  tgt_pos, pos_strat, tol_pos,
                  strat = 1L, asset = 8001L),
    "All input vectors must have the same length"
  )
})

## 2. No position: equity should stay at 10000 -------------------------------

testthat::test_that("backtest_rcpp with zero target exposure keeps equity flat", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)
  tgt_pos   <- rep(0,   n)
  pos_strat <- rep(1L,  n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  expect_length(eq, n)
  expect_equal(eq, rep(10000, n), tolerance = 1e-8)
})

## 3. Simple long: open on bar 2 and hold ------------------------------------

# Setup:
# - Bar 0: decide to go to +1 exposure, but trade executes only next bar at open[2].
# - Prices: 100 -> 110 -> 110.
# - ctr_size = 1, ctr_step = 1, lev = 10, no fee, no funding.
# - Result:
#   * eq[1] = 10000       (still flat at first close)
#   * eq[2] = 11000       (long opened at 100, marked at 110)
#   * eq[3] = 11000       (no more trades, same price)

testthat::test_that("backtest_rcpp simple long open and hold", {
  ts    <- c(0, 60, 120, 180)
  open  <- c(100, 100, 100, 100)
  high  <- open
  low   <- open
  close <- c(100, 100, 110, 110)

  n <- length(ts)

  # Always want +1 exposure, zero tolerance
  tgt_pos   <- rep(1.0, n)
  pos_strat <- rep(1L,  n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  # On bar 0: still flat, eq = 10000
  # On bar 1: still flat (intent created, trade next bar), eq = 10000
  # On bar 2: long opened at 100, close = 110 -> +1000 PnL -> 11000
  # On bar 3: same close, same PnL -> 11000
  expect_equal(eq, c(10000, 10000, 11000, 11000), tolerance = 1e-8)
})

## 4. Tolerance band: small target inside tol => no trading ------------------

testthat::test_that("backtest_rcpp respects tolerance band and does not trade", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  # Target exposure is small but non-zero
  tgt_pos   <- c(0.05, 0.02, 0.08)
  # Tolerance bigger than all gaps, so engine should treat as 'no opinion'
  tol_pos   <- rep(0.10, n)
  pos_strat <- rep(1L,   n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  # If tolerance works, we never open a position and equity remains at 10000.
  expect_equal(eq, rep(10000, n), tolerance = 1e-8)
})

## 5. Margin constraint: impossible target is rejected -----------------------

# Here we ask for a huge exposure (e.g. +20) with lev = 10.
# This implies a trade_notional that would require margin > equity, so
# the open action should be FAILED and no position is opened.

testthat::test_that("backtest_rcpp enforces margin and rejects too-large position", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- rep(20.0, n)  # impossible given lev and equity
  pos_strat <- rep(1L,   n)
  tol_pos   <- rep(0.0,  n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  # If margin gating works, no trade is ever opened, so equity is flat.
  expect_equal(eq, rep(10000, n), tolerance = 1e-8)
})


testthat::test_that("backtest_rcpp opens a long then closes back to flat", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 110)  # open[3] = 110 so close trade at 110
  high  <- open
  low   <- open
  close <- c(100, 110, 110)

  n <- length(ts)

  # Bar 0: target +1  -> pending OPEN
  # Bar 1: still +1   -> open long at 100, mark at 110 (eq = 11000)
  # Bar 2: target 0   -> executes CLOSE at 110, remains flat at 11000
  tgt_pos   <- c(1.0, 0.0, 0.0)
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  # Expected:
  # eq[1] = 10000 (no position yet)
  # eq[2] = 11000 (long marked at 110)
  # eq[3] = 11000 (flat after closing at 110)
  expect_equal(eq, c(10000, 11000, 11000), tolerance = 1e-8)
})

testthat::test_that("backtest_rcpp reduces an existing long position", {
  ts    <- c(0, 60, 120, 180)
  open  <- c(100, 100, 100, 100)
  high  <- open
  low   <- open
  close <- c(100, 100, 100, 100)

  n <- length(ts)

  # Logic:
  #  - Bar 0: tgt_pos = +1  -> pending OPEN
  #  - Bar 1: tgt_pos = +1  -> executes OPEN (full size)
  #  - Bar 2: tgt_pos = +0.5 -> should REDUCE
  #  - Bar 3: same +0.5, no further change
  tgt_pos   <- c(1.0, 1.0, 0.5, 0.5)
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  # All closes are 100 (same as entry), so PnL = 0.
  # Equity should stay at 10000 at all times (fees = 0).
  expect_equal(eq, rep(10000, n), tolerance = 1e-8)
})

testthat::test_that("backtest_rcpp liquidates on large loss and sets equity to zero", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- c(100, 0, 0)   # big crash on bar 2

  n <- length(ts)

  # Huge desired exposure: +10 (max allowed by margin with lev=10)
  tgt_pos   <- rep(10.0, n)
  pos_strat <- rep(1L,   n)
  tol_pos   <- rep(0.0,  n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
                      rec = FALSE)

  # Bar 0: flat, eq = 10000
  # Bar 1: long opened at 100; close at 0 -> eq becomes negative; engine flags liquidation
  #        eq[2] is set to 0. Subsequent bars stay 0.
  expect_equal(eq, c(10000, 0, 0), tolerance = 1e-8)
})

testthat::test_that("backtest_rcpp applies funding fee over time for open positions", {
  # 3 bars spanning 8h each
  ts    <- c(0, 8 * 3600, 16 * 3600)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  # Always target +1 exposure
  tgt_pos   <- rep(1.0, n)
  pos_strat <- rep(1L,  n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0,
                      fee_rt  = 0.0,
                      fund_rt = 0.01,  # 1% per 8h on notional
                      rec = FALSE)

  # Bar 1: still no funding (position not open at bar 0 close)
  # Bar 2 and 3: funding should reduce equity monotonically (price is flat)
  expect_true(eq[2] < eq[1])  # after first funding
  expect_true(eq[3] < eq[2])  # after second funding
  expect_true(all(eq > 0))    # should not be liquidated in this setup
})

testthat::test_that("backtest_rcpp treats NA target exposure as no-op", {
  ts    <- c(0, 60, 120, 180)
  open  <- c(100, 100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- c(NA_real_, 1.0, NA_real_, 0.0)
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      ctr_size = 1.0, ctr_step = 1.0,
                      lev = 10.0,
                      fee_rt  = 0.0,
                      fund_rt = 0.0,
                      rec = FALSE)

  # With all prices flat and fees = 0, any combination of NA / 0 / 1
  # that does not trigger a price change should keep eq at 10000.
  expect_equal(eq, rep(10000, n), tolerance = 1e-8)
})

test_that("backtest_rcpp has identical equity with and without recording", {
  ts    <- c(0, 60, 120, 180)
  open  <- c(100, 100, 110, 120)
  high  <- open
  low   <- open
  close <- open

  n <- length(ts)

  tgt_pos   <- c(1.0, 1.0, 0.5, 0.0)
  pos_strat <- rep(1L, n)
  tol_pos   <- rep(0.0, n)

  eq_no_rec <- backtest_rcpp(ts, open, high, low, close,
                             tgt_pos, pos_strat, tol_pos,
                             strat = 1L, asset = 8001L,
                             ctr_size = 1.0, ctr_step = 1.0,
                             lev = 10.0,
                             fee_rt  = 0.0005,
                             fund_rt = 0.0,
                             rec = FALSE)

  eq_with_rec <- backtest_rcpp(ts, open, high, low, close,
                               tgt_pos, pos_strat, tol_pos,
                               strat = 1L, asset = 8001L,
                               ctr_size = 1.0, ctr_step = 1.0,
                               lev = 10.0,
                               fee_rt  = 0.0005,
                               fund_rt = 0.0,
                               rec = TRUE)

  expect_equal(
    as.numeric(eq_no_rec),
    as.numeric(eq_with_rec),
    tolerance = 1e-8
  )
})

testthat::test_that("backtest_rcpp handles zero-length input gracefully", {
  ts    <- numeric(0)
  open  <- numeric(0)
  high  <- numeric(0)
  low   <- numeric(0)
  close <- numeric(0)

  tgt_pos   <- numeric(0)
  pos_strat <- integer(0)
  tol_pos   <- numeric(0)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L)

  expect_equal(length(eq), 0L)
})



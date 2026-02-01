test_that("backtest_rcpp returns a well-formed recorder attribute when rec=TRUE", {
  ts    <- c(0, 60, 120)
  open  <- c(100, 100, 100)
  high  <- open
  low   <- open
  close <- open

  tgt_pos   <- c(1, 1, 0)   # open then close
  pos_strat <- c(1L, 1L, 1L)
  tol_pos   <- c(0, 0, 0)

  eq <- backtest_rcpp(
    ts, open, high, low, close,
    tgt_pos, pos_strat, tol_pos,
    strat = 1L, asset = 8001L,
    ctr_size = 1.0, ctr_step = 1.0,
    lev = 10.0, fee_rt = 0.0, fund_rt = 0.0,
    rec = TRUE
  )

  rec_attr <- attr(eq, "recorder")

  # basic existence / type checks
  expect_false(is.null(rec_attr))
  expect_true(is.list(rec_attr))

  needed_names <- c(
    "ts", "bar_stage", "strat_id", "tx_id", "status",
    "liquidation", "action", "dir", "ctr_qty", "price", "eq"
  )
  expect_true(all(needed_names %in% names(rec_attr)))

  # type checks for the enum / logical fields
  expect_true(is.numeric(rec_attr$ts))
  expect_true(is.integer(rec_attr$bar_stage))
  expect_true(is.integer(rec_attr$strat_id))
  expect_true(is.integer(rec_attr$tx_id))
  expect_true(is.integer(rec_attr$status))
  expect_true(is.logical(rec_attr$liquidation))
  expect_true(is.integer(rec_attr$action))
  expect_true(is.integer(rec_attr$dir))
  expect_true(is.numeric(rec_attr$ctr_qty))
  expect_true(is.numeric(rec_attr$price))
  expect_true(is.numeric(rec_attr$eq))

  # internal consistency: all recorder vectors must have the same length
  lens <- vapply(rec_attr, length, integer(1))
  expect_equal(length(unique(lens)), 1L)

  # we do NOT assume length(recorder$ts) == length(eq)
})


testthat::test_that("backtest_rcpp has no recorder attribute when rec=FALSE", {
  ts    <- c(0, 60)
  open  <- c(100, 100)
  high  <- open
  low   <- open
  close <- open

  tgt_pos   <- c(1, 1)
  pos_strat <- c(1L, 1L)
  tol_pos   <- c(0, 0)

  eq <- backtest_rcpp(ts, open, high, low, close,
                      tgt_pos, pos_strat, tol_pos,
                      strat = 1L, asset = 8001L,
                      rec = FALSE)

  expect_null(attr(eq, "recorder"))
})

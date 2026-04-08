library(testthat)
library(data.table)

test_that("calc_bond_risk_state returns a compact bond risk snapshot", {
  out <- calc_bond_risk_state(
    par = 100,
    c_rate = 0.06,
    T = 3,
    freq = 2,
    ytm = 0.05,
    accrual_frac = 0.25
  )

  expect_true(data.table::is.data.table(out))
  expect_equal(nrow(out), 1)
  expect_true(all(c("clean_price", "dirty_price", "ytm", "duration", "mduration", "convexity", "dv01", "pv01") %in% names(out)))
  expect_equal(out$ytm, 0.05)
  expect_gt(out$dirty_price, out$clean_price)
  expect_gt(out$duration, 0)
  expect_gt(out$dv01, 0)
})

test_that("calc_bond_risk_state can include z-spread from an explicit curve", {
  out <- calc_bond_risk_state(
    par = 100,
    c_rate = 0.06,
    T = 3,
    freq = 2,
    ytm = 0.05,
    tenor = c(0.5, 1, 2, 3),
    zero_rate = c(0.04, 0.042, 0.045, 0.047)
  )

  expect_true("zspread" %in% names(out))
  expect_true(is.finite(out$zspread))
})

test_that("plan_duration_neutral_adjustment solves the required hedge units", {
  out <- plan_duration_neutral_adjustment(
    current_dv01 = 2500,
    target_dv01 = 0,
    hedge_dv01 = -125
  )

  expect_true(data.table::is.data.table(out))
  expect_equal(out$dv01_gap, -2500)
  expect_equal(out$hedge_units, 20)
})

test_that("plan_curve_trade_adjustment aligns key-rate gaps tenor by tenor", {
  out <- plan_curve_trade_adjustment(
    current_krd = c(`2Y` = 1.5, `5Y` = 2.0),
    target_krd = c(`2Y` = 1.0, `5Y` = 1.0),
    hedge_krd = c(`2Y` = -0.25, `5Y` = -0.5)
  )

  expect_true(data.table::is.data.table(out))
  expect_equal(out$tenor, c("2Y", "5Y"))
  expect_equal(out$krd_gap, c(-0.5, -1.0))
  expect_equal(out$hedge_units, c(2, 2))
})

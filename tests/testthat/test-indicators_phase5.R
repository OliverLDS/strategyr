library(testthat)
library(data.table)

test_that("FX forward points and carry are added in place", {
  DT <- data.table(
    spot = c(1.10, 1.12),
    forward = c(1.11, 1.125),
    r_domestic = c(0.05, 0.05),
    r_foreign = c(0.02, 0.02),
    tenor_years = c(1 / 12, 1 / 12)
  )

  calc_fx_forward_points(DT)
  calc_fx_carry(DT, tenor_col = "tenor_years")

  expect_equal(DT$fx_forward_points_1m, c(0.01, 0.005), tolerance = 1e-12)
  expect_equal(DT$fx_carry_1m, rep((0.05 - 0.02) / 12, 2), tolerance = 1e-12)
})

test_that("FX basis matches the covered-interest-parity gap", {
  DT <- data.table(
    spot = 1.10,
    forward = 1.11,
    r_domestic = 0.05,
    r_foreign = 0.02,
    tenor_years = 1 / 12
  )

  calc_fx_basis(DT)

  implied_forward <- DT$spot * exp((DT$r_domestic - DT$r_foreign) * DT$tenor_years)
  expect_equal(DT$fx_basis_1m, (DT$forward - implied_forward) / DT$spot, tolerance = 1e-12)
})

test_that("FX realized carry combines spot return and rate accrual", {
  DT <- data.table(
    spot = c(1.10, 1.12),
    r_domestic = c(0.05, 0.05),
    r_foreign = c(0.02, 0.02),
    tenor_years = c(1 / 12, 1 / 12)
  )

  calc_fx_realized_carry(DT, tenor_col = "tenor_years")

  ref <- c(NA_real_, log(1.12 / 1.10) + (0.05 - 0.02) / 12)
  expect_equal(DT$fx_realized_carry_1m, ref, tolerance = 1e-12)
})

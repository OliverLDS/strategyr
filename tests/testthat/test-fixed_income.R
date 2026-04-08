test_that("calc_period_rate converts annual yield to per-period rate", {
  expect_equal(calc_period_rate(0.06, freq = 2), 0.03)
  expect_equal(calc_period_rate(c(0.04, 0.06), freq = 4), c(0.01, 0.015))
})

test_that("calc_bond_cashflows generates level coupon bond cash flows", {
  expect_equal(
    calc_bond_cashflows(par = 100, c_rate = 0.06, T = 2, freq = 2),
    c(3, 3, 3, 103)
  )
})

test_that("calc_bond_npv matches manual present value", {
  cashflows <- c(3, 3, 3, 103)
  rates <- rep(0.03, 4)
  ref <- sum(cashflows / (1 + rates)^seq_along(cashflows))

  expect_equal(calc_bond_npv(cashflows = cashflows, rates = rates), ref)
  expect_equal(
    calc_bond_npv(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.06),
    ref
  )
})

test_that("duration, modified duration, and convexity match manual formulas", {
  cashflows <- c(3, 3, 3, 103)
  rates <- rep(0.03, 4)
  period <- seq_along(cashflows)
  pv <- cashflows / (1 + rates)^period
  npv <- sum(pv)

  ref_duration <- sum(period * pv) / npv / 2
  ref_mduration <- sum(period * pv / (1 + rates)) / npv / 2
  ref_convexity <- sum((period + 1) * period * pv / (1 + rates)^2) / npv / 4

  expect_equal(calc_bond_duration(cashflows = cashflows, rates = rates, freq = 2), ref_duration)
  expect_equal(calc_bond_mduration(cashflows = cashflows, rates = rates, freq = 2), ref_mduration)
  expect_equal(calc_bond_convexity(cashflows = cashflows, rates = rates, freq = 2), ref_convexity)
})

test_that("effective duration and convexity match finite-difference formulas", {
  cashflows <- c(3, 3, 3, 103)
  rates <- rep(0.03, 4)
  period <- seq_along(cashflows)
  bump <- 100 / 10000 / 2
  pv_0 <- sum(cashflows / (1 + rates)^period)
  pv_up <- sum(cashflows / (1 + rates + bump)^period)
  pv_dn <- sum(cashflows / (1 + rates - bump)^period)

  ref_edur <- (pv_dn - pv_up) / ((100 / 10000) * 2 * pv_0)
  ref_econv <- (pv_dn + pv_up - 2 * pv_0) / (((100 / 10000)^2) * pv_0)

  expect_equal(calc_bond_eduration(cashflows = cashflows, rates = rates, freq = 2, bases = 100), ref_edur)
  expect_equal(calc_bond_econvexity(cashflows = cashflows, rates = rates, freq = 2, bases = 100), ref_econv)
})

test_that("accrued interest, dirty price, and clean price are internally consistent", {
  accrued <- calc_bond_accrued_interest(par = 100, c_rate = 0.06, freq = 2, accrual_frac = 0.4)
  dirty <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.4)
  clean <- calc_bond_clean_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.4)

  expect_equal(accrued, 3 * 0.4)
  expect_equal(clean, dirty - accrued)
})

test_that("bond yield solves back to the target dirty and clean prices", {
  dirty <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.25)
  clean <- calc_bond_clean_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.25)

  expect_equal(
    calc_bond_yield(dirty, par = 100, c_rate = 0.06, T = 2, freq = 2, accrual_frac = 0.25, price_type = "dirty"),
    0.05,
    tolerance = 1e-10
  )
  expect_equal(
    calc_bond_yield(clean, par = 100, c_rate = 0.06, T = 2, freq = 2, accrual_frac = 0.25, price_type = "clean"),
    0.05,
    tolerance = 1e-10
  )
})

test_that("bond dv01 matches central-difference dirty price shock", {
  ytm <- 0.05
  bump <- 1 / 10000
  p_up <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = ytm + bump, accrual_frac = 0.3)
  p_dn <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = ytm - bump, accrual_frac = 0.3)
  ref <- (p_dn - p_up) / 2

  expect_equal(
    calc_bond_dv01(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = ytm, accrual_frac = 0.3),
    ref,
    tolerance = 1e-10
  )
})

test_that("bond pv01 equals dv01 under the same shock convention", {
  expect_equal(
    calc_bond_pv01(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.3),
    calc_bond_dv01(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.3),
    tolerance = 1e-12
  )
})

test_that("bond price change approximation matches duration-convexity formula", {
  ytm <- 0.05
  shock <- 0.01
  price <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = ytm, accrual_frac = 0.2)
  mdur <- calc_bond_mduration(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = ytm)
  conv <- calc_bond_convexity(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = ytm)
  ref <- price * (-mdur * shock + 0.5 * conv * shock^2)

  expect_equal(
    calc_bond_price_change_approx(ytm = ytm, yield_shock = shock, par = 100, c_rate = 0.06, T = 2, freq = 2, accrual_frac = 0.2),
    ref,
    tolerance = 1e-10
  )
})

test_that("bond current yield uses clean price", {
  clean <- 101
  dirty <- clean + calc_bond_accrued_interest(par = 100, c_rate = 0.06, freq = 2, accrual_frac = 0.25)

  expect_equal(calc_bond_current_yield(clean, c_rate = 0.06, par = 100, price_type = "clean"), 6 / 101)
  expect_equal(
    calc_bond_current_yield(dirty, c_rate = 0.06, par = 100, freq = 2, price_type = "dirty", accrual_frac = 0.25),
    6 / 101
  )
})

test_that("bond holding period return handles dirty and clean price inputs", {
  dirty_hpr <- calc_bond_holding_period_return(
    begin_price = 100,
    end_price = 101,
    coupon_income = 3,
    price_type = "dirty"
  )
  clean_hpr <- calc_bond_holding_period_return(
    begin_price = 100 - calc_bond_accrued_interest(par = 100, c_rate = 0.06, freq = 2, accrual_frac = 0.2),
    end_price = 101 - calc_bond_accrued_interest(par = 100, c_rate = 0.06, freq = 2, accrual_frac = 0.4),
    coupon_income = 3,
    price_type = "clean",
    par = 100,
    c_rate = 0.06,
    freq = 2,
    begin_accrual_frac = 0.2,
    end_accrual_frac = 0.4
  )

  expect_equal(dirty_hpr, 0.04)
  expect_equal(clean_hpr, 0.04)
})

test_that("bond key rate duration isolates sensitivity at the shocked period", {
  cashflows <- c(0, 0, 0, 100)
  rates <- rep(0.03, 4)
  pv_0 <- sum(cashflows / (1 + rates)^seq_along(cashflows))
  bump <- 1 / 10000 / 2
  pv_up <- 100 / (1 + (0.03 + bump))^4
  pv_dn <- 100 / (1 + (0.03 - bump))^4
  ref_krd <- (pv_dn - pv_up) / ((1 / 10000) * 2 * pv_0)

  out <- calc_bond_key_rate_duration(cashflows = cashflows, rates = rates, freq = 2, key_periods = 1:4, bases = 1)

  expect_equal(out$key_period, 1:4)
  expect_equal(out$krd[1:3], c(0, 0, 0), tolerance = 1e-12)
  expect_equal(out$krd[4], ref_krd, tolerance = 1e-10)
})

test_that("bond carry matches coupon-accrual minus financing definition", {
  begin_dirty <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.2)
  ref <- (100 * 0.06 * 0.5 - begin_dirty * 0.02 * 0.5) / begin_dirty

  expect_equal(
    calc_bond_carry(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.2, holding_years = 0.5, funding_rate = 0.02),
    ref,
    tolerance = 1e-10
  )
})

test_that("bond roll-down return matches unchanged-yield shorter-maturity price move", {
  begin_dirty <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.2)
  end_dirty <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 1.5, freq = 2, ytm = 0.05, accrual_frac = 0.2)
  ref <- (end_dirty - begin_dirty) / begin_dirty

  expect_equal(
    calc_bond_roll_down_return(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05, accrual_frac = 0.2, holding_years = 0.5),
    ref,
    tolerance = 1e-10
  )
})

test_that("bond z-spread recovers flat-curve spread difference", {
  cashflows <- calc_bond_cashflows(par = 100, c_rate = 0.06, T = 2, freq = 2)
  spot_rates <- rep(0.04 / 2, length(cashflows))
  price <- calc_bond_dirty_price(par = 100, c_rate = 0.06, T = 2, freq = 2, ytm = 0.05)

  expect_equal(
    calc_bond_zspread(price = price, spot_rates = spot_rates, cashflows = cashflows, freq = 2, price_type = "dirty"),
    0.01,
    tolerance = 1e-8
  )
})

test_that("bond nominal spread matches yield difference", {
  expect_equal(calc_bond_nominal_spread(0.0525, 0.041), 0.0115)
})

test_that("curve slope matches long minus short yield", {
  expect_equal(calc_curve_slope(0.03, 0.045), 0.015)
})

test_that("curve butterfly matches belly minus average wings", {
  expect_equal(calc_curve_butterfly(0.03, 0.04, 0.05), 0)
  expect_equal(calc_curve_butterfly(0.03, 0.045, 0.05), 0.005)
})

test_that("bond rate return approximation matches duration-convexity definition", {
  expect_equal(
    calc_bond_rate_return_approx(duration = 4.2, convexity = 18, delta_y = 0.01),
    -4.2 * 0.01 + 0.5 * 18 * 0.01^2
  )
})

test_that("bond spread return approximation matches spread-duration definition", {
  expect_equal(
    calc_bond_spread_return_approx(spread_duration = 5.5, delta_s = 0.002),
    -5.5 * 0.002
  )
})

test_that("bond carry-roll decomposition sums correctly", {
  out <- calc_bond_carry_roll_decomp(carry = 0.012, roll_down_return = 0.008)

  expect_equal(out$carry, 0.012)
  expect_equal(out$roll_down, 0.008)
  expect_equal(out$carry_roll, 0.02)
})

test_that("bond total return decomposition sums correctly", {
  out <- calc_bond_total_return_decomp(
    carry = 0.012,
    roll_down_return = 0.008,
    rate_return = -0.015,
    spread_return = 0.004,
    residual = 0.001
  )

  expect_equal(out$carry, 0.012)
  expect_equal(out$roll_down, 0.008)
  expect_equal(out$rate_return, -0.015)
  expect_equal(out$spread_return, 0.004)
  expect_equal(out$residual, 0.001)
  expect_equal(out$total_return, 0.01)
})

test_that("curve zero-rate interpolation returns exact endpoints and linear interior values", {
  tenor <- c(1, 3, 5)
  zero_rate <- c(0.02, 0.03, 0.04)

  expect_equal(calc_curve_zero_rate(tenor_out = c(1, 5), tenor = tenor, zero_rate = zero_rate), c(0.02, 0.04))
  expect_equal(calc_curve_zero_rate(tenor_out = 2, tenor = tenor, zero_rate = zero_rate), 0.025)
})

test_that("curve shock applies parallel and tenor-specific bumps correctly", {
  tenor <- c(1, 3, 5)
  zero_rate <- c(0.02, 0.03, 0.04)

  parallel <- calc_curve_shock(tenor = tenor, zero_rate = zero_rate, shock_bp = 10)
  keyed <- calc_curve_shock(tenor = tenor, zero_rate = zero_rate, shock_bp = 10, shock_tenor = 3)

  expect_equal(parallel$zero_rate_shocked, zero_rate + 0.001)
  expect_equal(keyed$zero_rate_shocked, c(0.02, 0.031, 0.04))
})

test_that("tenor-based bond key rate duration is positive for a standard long bond", {
  out <- calc_bond_key_rate_duration_tenor(
    par = 100,
    c_rate = 0.06,
    T = 3,
    freq = 2,
    tenor = c(0.5, 1, 2, 3),
    zero_rate = c(0.04, 0.042, 0.045, 0.047),
    key_tenor = 2,
    shock_bp = 1
  )

  expect_true(is.finite(out))
  expect_gt(out, 0)
})

test_that("bond spread duration is positive for a standard long bond", {
  out <- calc_bond_spread_duration(
    par = 100,
    c_rate = 0.06,
    T = 3,
    freq = 2,
    tenor = c(0.5, 1, 2, 3),
    zero_rate = c(0.04, 0.042, 0.045, 0.047),
    zspread = 0.01,
    shock_bp = 1
  )

  expect_true(is.finite(out))
  expect_gt(out, 0)
})

test_that("bond z-spread return approximation matches spread-duration definition", {
  expect_equal(
    calc_bond_zspread_return_approx(spread_duration = 5.5, delta_z = 0.002),
    -5.5 * 0.002
  )
})

test_that("day-count fraction handles common fixed-income conventions", {
  start_date <- as.Date("2025-01-15")
  end_date <- as.Date("2025-07-15")

  expect_equal(calc_day_count_frac(start_date, end_date, convention = "ACT/365"), 181 / 365)
  expect_equal(calc_day_count_frac(start_date, end_date, convention = "ACT/360"), 181 / 360)
  expect_equal(calc_day_count_frac(start_date, end_date, convention = "30/360"), 0.5)
})

test_that("coupon schedule returns regular dates after issue and up to maturity", {
  out <- calc_coupon_schedule(
    issue_date = as.Date("2024-01-15"),
    maturity_date = as.Date("2025-01-15"),
    freq = 2
  )

  expect_equal(out, as.Date(c("2024-07-15", "2025-01-15")))
})

test_that("previous and next coupon dates are internally consistent", {
  settle_date <- as.Date("2024-09-01")
  issue_date <- as.Date("2024-01-15")
  maturity_date <- as.Date("2025-01-15")

  expect_equal(
    calc_bond_prev_coupon(settle_date, maturity_date = maturity_date, freq = 2, issue_date = issue_date),
    as.Date("2024-07-15")
  )
  expect_equal(
    calc_bond_next_coupon(settle_date, maturity_date = maturity_date, freq = 2, issue_date = issue_date),
    as.Date("2025-01-15")
  )
})

test_that("coupon lookups honor issue-date bounds", {
  issue_date <- as.Date("2024-01-15")
  maturity_date <- as.Date("2025-01-15")
  settle_date <- as.Date("2024-01-01")

  expect_true(is.na(calc_bond_prev_coupon(settle_date, maturity_date = maturity_date, freq = 2, issue_date = issue_date)))
  expect_equal(
    calc_bond_next_coupon(settle_date, maturity_date = maturity_date, freq = 2, issue_date = issue_date),
    as.Date("2024-07-15")
  )
})

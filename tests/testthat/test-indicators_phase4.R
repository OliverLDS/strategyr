library(testthat)
library(data.table)

test_that("front-next spread and regime reflect contango/backwardation", {
  panel <- data.table(
    date = rep(1:2, each = 3),
    contract_rank = rep(1:3, times = 2),
    close = c(100, 102, 104, 100, 98, 96),
    time_to_expiry = rep(c(30, 60, 90) / 365, times = 2)
  )

  spread <- calc_front_next_spread(panel)
  regime <- calc_contango_backwardation(panel)

  expect_equal(spread$front_next_spread, c(2, -2), tolerance = 1e-12)
  expect_equal(regime$contango_backwardation_1_2, c(1, -1))
})

test_that("curve slope captures upward and downward term structure", {
  panel <- data.table(
    date = rep(1:2, each = 3),
    time_to_expiry = rep(c(30, 60, 90) / 365, times = 2),
    close = c(100, 102, 104, 100, 98, 96)
  )

  out <- calc_futures_curve_slope(panel)

  expect_gt(out$curve_slope[1], 0)
  expect_lt(out$curve_slope[2], 0)
})

test_that("curve butterfly is zero for a linear curve and nonzero for curvature", {
  linear_panel <- data.table(
    date = rep(1, 3),
    contract_rank = 1:3,
    close = c(100, 102, 104)
  )
  curved_panel <- data.table(
    date = rep(2, 3),
    contract_rank = 1:3,
    close = c(100, 105, 103)
  )

  out <- calc_futures_curve_butterfly(rbind(linear_panel, curved_panel))

  expect_equal(out$curve_butterfly_1_2_3[1], 0, tolerance = 1e-12)
  expect_equal(out$curve_butterfly_1_2_3[2], 7, tolerance = 1e-12)
})

test_that("roll yield and term-structure carry have expected signs", {
  panel <- data.table(
    date = rep(1:2, each = 2),
    contract_rank = rep(1:2, times = 2),
    close = c(100, 102, 100, 98),
    time_to_expiry = rep(c(30, 60) / 365, times = 2)
  )

  roll <- calc_roll_yield(panel)
  carry <- calc_term_structure_carry(panel)

  expect_lt(roll$roll_yield_1_2[1], 0)
  expect_gt(roll$roll_yield_1_2[2], 0)
  expect_lt(carry$term_structure_carry_1_2[1], 0)
  expect_gt(carry$term_structure_carry_1_2[2], 0)
})

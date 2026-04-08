library(testthat)
library(data.table)

test_that("strat_bond_carry_roll_tgt_pos follows carry-plus-roll sign", {
  DT <- data.table(
    bond_carry = c(-0.01, 0.01, 0.02),
    bond_roll_down_return = c(-0.01, 0.00, 0.01)
  )

  tgt_pos <- strat_bond_carry_roll_tgt_pos(
    DT,
    compute_features = FALSE
  )

  expect_equal(tgt_pos, c(-1, 1, 1))
})

test_that("strat_bond_carry_roll_action_plan uses latest target", {
  DT <- data.table(
    bond_carry = c(0, 0.01),
    bond_roll_down_return = c(0, 0.01)
  )
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

  plan <- strat_bond_carry_roll_action_plan(
    DT,
    state,
    compute_features = FALSE,
    strat_id = 602L,
    tol_pos = 0
  )

  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$dir, 1L)
  expect_equal(plan$actions[[1]]$strat, 602L)
})

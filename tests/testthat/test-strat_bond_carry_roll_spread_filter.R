library(testthat)
library(data.table)

test_that("strat_bond_carry_roll_spread_filter_tgt_pos applies spread gates", {
  DT <- data.table(
    bond_carry = c(0.04, 0.04, -0.03, -0.03),
    bond_roll_down_return = c(0.02, 0.02, -0.02, -0.02),
    credit_spread = c(0.03, 0.01, 0.02, 0.05)
  )

  tgt_pos <- strat_bond_carry_roll_spread_filter_tgt_pos(
    DT,
    spread_col = "credit_spread",
    min_long_spread = 0.02,
    max_short_spread = 0.03,
    compute_features = FALSE
  )
  dbg <- strat_bond_carry_roll_spread_filter_tgt_pos(DT, spread_col = "credit_spread", min_long_spread = 0.02, max_short_spread = 0.03, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(1, 0, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_bond_carry_roll_spread_filter_action_plan returns planner-shaped output", {
  DT <- data.table(
    bond_carry = c(-0.03, -0.03),
    bond_roll_down_return = c(-0.02, -0.02),
    credit_spread = c(0.02, 0.02)
  )

  plan <- strat_bond_carry_roll_spread_filter_action_plan(
    DT,
    make_test_state(),
    spread_col = "credit_spread",
    max_short_spread = 0.03,
    compute_features = FALSE,
    strat_id = 606L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 606L)
})

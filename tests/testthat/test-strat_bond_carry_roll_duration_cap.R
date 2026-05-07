library(testthat)
library(data.table)

test_that("strat_bond_carry_roll_duration_cap_tgt_pos applies duration caps", {
  DT <- data.table(
    bond_carry = c(0.04, 0.04, -0.03, -0.03),
    bond_roll_down_return = c(0.02, 0.02, -0.02, -0.02),
    bond_mduration = c(3, 8, 3, 8)
  )

  tgt_pos <- strat_bond_carry_roll_duration_cap_tgt_pos(DT, duration_col = "bond_mduration", duration_max = 5, compute_features = FALSE)
  dbg <- strat_bond_carry_roll_duration_cap_tgt_pos(DT, duration_col = "bond_mduration", duration_max = 5, compute_features = FALSE, debug = TRUE)

  expect_equal(tgt_pos, c(1, 0, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_bond_carry_roll_duration_cap_action_plan returns planner-shaped output", {
  DT <- data.table(
    bond_carry = c(0.04, 0.04),
    bond_roll_down_return = c(0.02, 0.02),
    bond_mduration = c(3, 3)
  )

  plan <- strat_bond_carry_roll_duration_cap_action_plan(DT, make_test_state(), duration_col = "bond_mduration", duration_max = 5, compute_features = FALSE, strat_id = 613L, tol_pos = 0)
  expect_true(is.list(plan))
  expect_equal(plan$actions[[1]]$strat, 613L)
})

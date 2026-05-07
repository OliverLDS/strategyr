library(testthat)
library(data.table)

test_that("strat_relative_strength_dual_momentum_tgt_pos combines relative and absolute momentum", {
  DT <- data.table(
    relative_strength_20 = c(1.10, 0.90, 1.05),
    momentum_close_60 = c(0.10, -0.10, 0.10)
  )

  tgt_pos <- strat_relative_strength_dual_momentum_tgt_pos(
    DT,
    rs_n = 20L,
    mom_n = 60L,
    rs_short_threshold = 0.95,
    compute_features = FALSE
  )
  tgt_no_short <- strat_relative_strength_dual_momentum_tgt_pos(
    DT,
    rs_n = 20L,
    mom_n = 60L,
    rs_short_threshold = 0.95,
    allow_short = FALSE,
    compute_features = FALSE
  )
  dbg <- strat_relative_strength_dual_momentum_tgt_pos(DT, rs_n = 20L, mom_n = 60L, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(1, -1, 1))
  expect_equal(tgt_no_short, c(1, 0, 1))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_relative_strength_dual_momentum_action_plan returns planner-shaped output", {
  DT <- data.table(relative_strength_20 = c(1.10, 1.05), momentum_close_60 = c(0.10, 0.10))

  plan <- strat_relative_strength_dual_momentum_action_plan(
    DT,
    make_test_state(),
    rs_n = 20L,
    mom_n = 60L,
    compute_features = FALSE,
    strat_id = 504L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 504L)
})

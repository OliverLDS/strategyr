library(testthat)
library(data.table)

test_that("strat_donchian_turtle_tgt_pos follows entry and exit channels", {
  DT <- data.table(
    close = c(100, 106, 104, 94, 97),
    dc_high_55 = c(105, 105, 105, 105, 105),
    dc_low_55 = c(95, 95, 95, 95, 95),
    dc_high_20 = c(96, 96, 96, 96, 96),
    dc_low_20 = c(99, 99, 99, 99, 99)
  )

  tgt_pos <- strat_donchian_turtle_tgt_pos(DT, entry_n = 55L, exit_n = 20L, compute_features = FALSE)
  dbg <- strat_donchian_turtle_tgt_pos(DT, entry_n = 55L, exit_n = 20L, compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(tgt_pos))
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos, c(0, 1, 1, -1, 0))
  expect_named(dbg, c("tgt_pos", "feature_cols"))
})

test_that("strat_donchian_turtle_action_plan returns planner-shaped output", {
  DT <- data.table(
    close = c(100, 106),
    dc_high_55 = c(105, 105),
    dc_low_55 = c(95, 95),
    dc_high_20 = c(96, 96),
    dc_low_20 = c(99, 99)
  )

  plan <- strat_donchian_turtle_action_plan(
    DT,
    make_test_state(last_px = 106),
    entry_n = 55L,
    exit_n = 20L,
    compute_features = FALSE,
    strat_id = 307L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 307L)
})

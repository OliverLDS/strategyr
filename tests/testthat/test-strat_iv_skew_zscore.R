library(testthat)
library(data.table)

test_that("strat_iv_skew_zscore_tgt_pos supports momentum and reversion modes", {
  DT <- data.table(
    iv_skew = c(-0.05, 0.05),
    zscore_iv_skew_60 = c(-1.2, 1.2)
  )

  mom <- strat_iv_skew_zscore_tgt_pos(DT, z_n = 60L, mode = "momentum", compute_features = FALSE)
  rev <- strat_iv_skew_zscore_tgt_pos(DT, z_n = 60L, mode = "reversion", compute_features = FALSE)
  dbg <- strat_iv_skew_zscore_tgt_pos(DT, z_n = 60L, mode = "momentum", compute_features = FALSE, debug = TRUE)

  expect_true(is.numeric(mom))
  expect_length(mom, nrow(DT))
  expect_equal(mom, c(-1, 1))
  expect_equal(rev, c(1, -1))
  expect_named(dbg, c("tgt_pos", "data", "feature_col"))
})

test_that("strat_iv_skew_zscore_action_plan returns planner-shaped output", {
  DT <- data.table(iv_skew = c(0.05, 0.05), zscore_iv_skew_60 = c(1.2, 1.2))

  plan <- strat_iv_skew_zscore_action_plan(
    DT,
    make_test_state(),
    z_n = 60L,
    mode = "momentum",
    compute_features = FALSE,
    strat_id = 706L,
    tol_pos = 0
  )

  expect_true(is.list(plan))
  expect_equal(plan$n, 1L)
  expect_equal(plan$actions[[1]]$strat, 706L)
})

library(testthat)
library(data.table)

test_that("strat_cross_sectional_rank_allocator_tgt_pos allocates top ranks", {
  DT <- make_test_cross_sectional_market(n_dates = 2L)
  tgt_pos <- strat_cross_sectional_rank_allocator_tgt_pos(DT, long_n = 1L, short_n = 1L, gross_exposure = 1.0)
  expect_equal(length(tgt_pos), nrow(DT))
  expect_equal(sum(tgt_pos[DT$date == DT$date[1]]), 0)
})

test_that("strat_cross_sectional_rank_allocator_action_plan returns adjustment outputs", {
  DT <- make_test_cross_sectional_market(n_dates = 2L)
  portfolio_state <- make_test_portfolio_state()
  out <- strat_cross_sectional_rank_allocator_action_plan(DT, portfolio_state, equity = 10000, long_n = 1L, short_n = 1L, strat_id = 507L)
  expect_true(is.list(out))
  expect_true(data.table::is.data.table(out$adjustment_plan))
})

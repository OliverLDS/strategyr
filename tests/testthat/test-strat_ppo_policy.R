test_that("PPO policy strategy maps R function actions to target positions", {
  DT <- make_test_ohlc(6L)
  policy <- function(obs) {
    c(0L, 1L, 2L, 2L, 1L, 0L)
  }

  tgt_pos <- strat_ppo_policy_tgt_pos(
    DT,
    model = policy,
    feature_cols = "close",
    action_map = c(-1, 0, 1),
    target_size = 0.75
  )

  expect_type(tgt_pos, "double")
  expect_equal(tgt_pos, c(-0.75, 0, 0.75, 0.75, 0, -0.75))
})

test_that("PPO policy strategy supports action plans and debug output", {
  DT <- make_test_ohlc(6L)
  policy <- function(obs) rep(2L, nrow(obs))
  state <- make_test_state(last_px = tail(DT$close, 1L))

  dbg <- strat_ppo_policy_tgt_pos(
    DT,
    model = policy,
    feature_cols = "close",
    debug = TRUE
  )
  plan <- strat_ppo_policy_action_plan(
    DT,
    state,
    model = policy,
    feature_cols = "close",
    target_size = 0.5
  )

  expect_named(dbg, c("tgt_pos", "actions", "feature_cols"))
  expect_true(is.list(plan))
})

test_that("PPO training adapter is optional behind reticulate", {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    expect_error(
      train_ppo_policy_py(NULL, total_timesteps = 1L),
      "`reticulate` is required",
      fixed = TRUE
    )
  } else {
    skip("Python PPO training depends on an external stable-baselines3 environment.")
  }
})

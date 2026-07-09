test_that("LSTM forecast strategy uses precomputed forecasts without torch", {
  DT <- make_test_ohlc(40L)
  DT[, lstm_forecast_close_5_1 := close * c(rep(NA_real_, 5L), rep(1.02, .N - 5L))]

  tgt_pos <- strat_lstm_forecast_tgt_pos(
    DT,
    lookback = 5L,
    horizon = 1L,
    long_threshold = 0.01,
    short_threshold = -0.01,
    target_size = 0.5,
    compute_features = FALSE
  )

  expect_type(tgt_pos, "double")
  expect_length(tgt_pos, nrow(DT))
  expect_equal(tgt_pos[seq_len(5L)], rep(0, 5L))
  expect_true(all(tgt_pos[-seq_len(5L)] == 0.5))
})

test_that("LSTM forecast strategy supports action plans and debug output", {
  DT <- make_test_ohlc(40L)
  DT[, lstm_forecast_close_5_1 := close * 0.98]
  state <- make_test_state(last_px = tail(DT$close, 1L))

  dbg <- strat_lstm_forecast_tgt_pos(
    DT,
    lookback = 5L,
    horizon = 1L,
    long_threshold = 0.01,
    short_threshold = -0.01,
    target_size = 0.25,
    compute_features = FALSE,
    debug = TRUE
  )
  plan <- strat_lstm_forecast_action_plan(
    DT,
    state,
    lookback = 5L,
    horizon = 1L,
    long_threshold = 0.01,
    short_threshold = -0.01,
    target_size = 0.25,
    compute_features = FALSE
  )

  expect_named(dbg, c("tgt_pos", "forecast_col", "feature_cols"))
  expect_true(is.list(plan))
})

test_that("calc_lstm_forecast is optional behind torch", {
  DT <- make_test_ohlc(30L)

  if (!requireNamespace("torch", quietly = TRUE)) {
    expect_error(
      calc_lstm_forecast(DT, lookback = 5L, epochs = 1L),
      "`torch` is required",
      fixed = TRUE
    )
  } else {
    forecast <- calc_lstm_forecast(
      DT,
      lookback = 5L,
      horizon = 1L,
      epochs = 1L,
      hidden_size = 4L,
      seed = 1L
    )
    expect_type(forecast, "double")
    expect_length(forecast, nrow(DT))
  }
})

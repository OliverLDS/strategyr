.lstm_forecast_signal <- function(forecast, reference, long_threshold = 0, short_threshold = 0, allow_short = TRUE, target_size = 1.0) {
  ret <- forecast / reference - 1
  out <- rep(0.0, length(ret))
  valid <- is.finite(ret)
  out[valid & ret > long_threshold] <- target_size
  if (allow_short) {
    out[valid & ret < short_threshold] <- -target_size
  }
  out
}

#' LSTM-Forecast Target Positions
#'
#' Generates target positions from an experimental LSTM forecast. The model is
#' optional and requires `torch`; existing precomputed forecast columns can be
#' supplied with `compute_features = FALSE`.
#'
#' @inheritParams calc_lstm_forecast
#' @param long_threshold Numeric forecast return threshold for long exposure.
#' @param short_threshold Numeric forecast return threshold for short exposure.
#' @param allow_short Logical; when `FALSE`, negative targets are disabled.
#' @param target_size Numeric absolute target exposure.
#' @param compute_features Logical; when `TRUE`, train/apply the LSTM forecast.
#'
#' @return A numeric target-position vector, or a list when `debug = TRUE`.
#' @export
strat_lstm_forecast_tgt_pos <- function(
  DT,
  feature_cols = "close",
  target_col = "close",
  lookback = 20L,
  horizon = 1L,
  train_ratio = 0.8,
  epochs = 20L,
  hidden_size = 16L,
  num_layers = 1L,
  lr = 0.001,
  scale = TRUE,
  seed = NULL,
  forecast_col = NULL,
  long_threshold = 0,
  short_threshold = 0,
  allow_short = TRUE,
  target_size = 1.0,
  compute_features = TRUE,
  debug = FALSE
) {
  if (is.null(forecast_col)) {
    forecast_col <- .lstm_forecast_col(target_col, lookback, horizon)
  }
  if (compute_features) {
    forecast <- calc_lstm_forecast(
      DT,
      feature_cols = feature_cols,
      target_col = target_col,
      lookback = lookback,
      horizon = horizon,
      train_ratio = train_ratio,
      epochs = epochs,
      hidden_size = hidden_size,
      num_layers = num_layers,
      lr = lr,
      scale = scale,
      seed = seed,
      forecast_col = forecast_col,
      add_col = TRUE,
      debug = FALSE
    )
  } else {
    .validate_market_dt(DT, c(target_col, forecast_col))
    forecast <- DT[[forecast_col]]
  }

  tgt_pos <- .lstm_forecast_signal(
    forecast = forecast,
    reference = DT[[target_col]],
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    allow_short = allow_short,
    target_size = target_size
  )
  if (debug) {
    return(list(tgt_pos = tgt_pos, forecast_col = forecast_col, feature_cols = c(feature_cols, target_col)))
  }
  tgt_pos
}

#' LSTM-Forecast Action Plan
#'
#' Applies the latest LSTM-forecast target position and translates it into an
#' executable action plan.
#'
#' @inheritParams strat_lstm_forecast_tgt_pos
#' @param state Named list describing current trading state.
#' @param strat_id Integer strategy identifier.
#' @param tol_pos Numeric tolerance passed to the action planner.
#'
#' @return A list produced by `gen_action_plan_rcpp()`.
#' @export
strat_lstm_forecast_action_plan <- function(
  DT,
  state,
  feature_cols = "close",
  target_col = "close",
  lookback = 20L,
  horizon = 1L,
  train_ratio = 0.8,
  epochs = 20L,
  hidden_size = 16L,
  num_layers = 1L,
  lr = 0.001,
  scale = TRUE,
  seed = NULL,
  forecast_col = NULL,
  long_threshold = 0,
  short_threshold = 0,
  allow_short = TRUE,
  target_size = 1.0,
  compute_features = TRUE,
  strat_id = 901L,
  tol_pos = 0.1,
  debug = FALSE
) {
  tgt_pos <- strat_lstm_forecast_tgt_pos(
    DT,
    feature_cols = feature_cols,
    target_col = target_col,
    lookback = lookback,
    horizon = horizon,
    train_ratio = train_ratio,
    epochs = epochs,
    hidden_size = hidden_size,
    num_layers = num_layers,
    lr = lr,
    scale = scale,
    seed = seed,
    forecast_col = forecast_col,
    long_threshold = long_threshold,
    short_threshold = short_threshold,
    allow_short = allow_short,
    target_size = target_size,
    compute_features = compute_features,
    debug = FALSE
  )
  latest_tgt_pos <- .latest_non_na(tgt_pos)
  plan <- .action_plan_from_tgt_pos(latest_tgt_pos, state, strat_id = strat_id, tol_pos = tol_pos)
  if (debug) {
    return(list(plan = plan, latest_tgt_pos = latest_tgt_pos))
  }
  plan
}

# Optional ML/RL example: synthetic precomputed LSTM-style forecasts.
#
# This example does not train a neural network and makes no real-data trading
# claim. It demonstrates the intended architecture: model output is converted
# into a standard strategyr target-position path.

library(data.table)
library(strategyr)

set.seed(1L)

n <- 260L
DT <- data.table(
  datetime = as.POSIXct("2020-01-01", tz = "UTC") + seq_len(n) * 86400
)
DT[, close := 100 + cumsum(0.03 + sin(seq_len(.N) / 14) * 0.4 + rnorm(.N, sd = 0.25))]
DT[, open := shift(close, fill = close[1L])]
DT[, high := pmax(open, close) + 0.4]
DT[, low := pmin(open, close) - 0.4]

# Pretend this column came from an offline model forecast. In a real workflow,
# the forecast should be generated without looking ahead and then stored.
DT[, synthetic_model_return := shift(sign(sin(seq_len(.N) / 18)), 1L, fill = 0) * 0.008]
DT[, lstm_forecast_close_20_1 := close * (1 + synthetic_model_return)]

tgt_pos <- strat_lstm_forecast_tgt_pos(
  DT,
  lookback = 20L,
  horizon = 1L,
  forecast_col = "lstm_forecast_close_20_1",
  long_threshold = 0.005,
  short_threshold = -0.005,
  target_size = 0.5,
  compute_features = FALSE
)

DT[, tgt_pos := tgt_pos]
DT[, close_return := close / shift(close) - 1]
DT[, strategy_return := shift(tgt_pos, fill = 0) * close_return]
DT[, equity_proxy := cumprod(1 + fifelse(is.na(strategy_return), 0, strategy_return))]

print(DT[, .(
  n_rows = .N,
  final_equity_proxy = tail(equity_proxy, 1L),
  avg_abs_target = mean(abs(tgt_pos)),
  n_target_changes = sum(abs(diff(tgt_pos)) > 0)
)])

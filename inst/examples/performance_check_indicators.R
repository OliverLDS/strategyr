library(data.table)
library(microbenchmark)
library(TTR)
library(strategyr)

set.seed(1)
n <- 5000L
DT <- data.table(
  high = cumsum(runif(n, -1, 1)) + 105,
  low = cumsum(runif(n, -1, 1)) + 95,
  close = cumsum(runif(n, -1, 1)) + 100
)
DT[, `:=`(
  high = pmax(high, low, close),
  low = pmin(low, high, close)
)]

microbenchmark(
  ttr_macd = TTR::MACD(DT$close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE),
  strategyr_macd = {
    temp <- copy(DT)
    strategyr::calc_MACD(temp, fast = 12, slow = 26, signal = 9)
    temp[, .(macd_12_26, macd_signal_12_26_9, macd_hist_12_26_9)]
  },
  times = 50L
)

microbenchmark(
  ttr_roc = TTR::ROC(DT$close, n = 10, type = "discrete"),
  strategyr_roc = {
    temp <- copy(DT)
    strategyr::calc_ROC(temp, ns = 10, scale = 1)
    temp$roc_10
  },
  times = 50L
)

microbenchmark(
  ttr_bbands = TTR::BBands(DT$close, n = 20, sd = 2, maType = "SMA"),
  strategyr_bbands = {
    temp <- copy(DT)
    strategyr::calc_BollingerBands(temp, ns = 20, ks = 2)
    temp[, .(bb_low_20_2, bb_mid_20, bb_high_20_2)]
  },
  times = 50L
)

microbenchmark(
  ttr_stoch = TTR::stoch(
    as.matrix(DT[, .(high, low, close)]),
    nFastK = 14,
    nFastD = 3,
    nSlowD = 1,
    maType = "SMA",
    bounded = TRUE
  ),
  strategyr_stoch = {
    temp <- copy(DT)
    strategyr::calc_StochasticOscillator(temp, ns = 14, d_ns = 3)
    temp[, .(stoch_k_14, stoch_d_14_3)]
  },
  times = 50L
)

microbenchmark(
  ttr_cci = TTR::CCI(as.matrix(DT[, .(high, low, close)]), n = 20, c = 0.015, maType = "SMA"),
  strategyr_cci = {
    temp <- copy(DT)
    strategyr::calc_CCI(temp, ns = 20)
    temp$cci_20
  },
  times = 50L
)

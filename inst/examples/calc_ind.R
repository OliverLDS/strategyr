library(data.table)
library(strategyr)

set.seed(0)

# Number of bars
n <- 2000L

# 1-minute timestamps starting from 0 (seconds)
timestamp <- seq(0, by = 60, length.out = n)

# Simulate log-returns and build a price path
ret   <- rnorm(n, mean = 0.0002, sd = 0.01)  # small drift, some noise
price <- 100 * exp(cumsum(ret))

# Build simple OHLC from the price path
open  <- price
high  <- price * runif(n, 1.00, 1.01)  # small random wiggle
low   <- price * runif(n, 0.99, 1.00)
close <- price

DT <- data.table::data.table(
  datetime = timestamp,
  open  = open,
  high  = high,
  low   = low,
  close = close
)

calc_EMA(DT, ns = c(20, 50))
calc_ATR(DT, ns = NULL, hs = c(12))
calc_ATR_quantile(DT, hs = c(12), thresholds = c(0.05, 0.1, 0.2, 0.3))
calc_ladder_index(DT, cycle_N = 180L)

head(DT[, .(
  datetime,
  close,
  ema_20,
  ema_50,
  atr_logr_12,
  ladder_index_180
)])
  

library(data.table)
library(strategyr)

set.seed(123)

# Number of bars
n <- 200L

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

DT <- data.table(
  datetime = timestamp,
  open  = open,
  high  = high,
  low   = low,
  close = close
)

head(DT)

tgt_pos <- strat_buy_and_hold_tgt_pos(DT)

# One strategy ID for all bars
pos_strat <- rep(1L, nrow(DT))

# Zero tolerance: always try to match tgt_pos exactly
tol_pos <- rep(0.0, nrow(DT))

eq <- backtest_rcpp(
  timestamp = DT$datetime,
  open      = DT$open,
  high      = DT$high,
  low       = DT$low,
  close     = DT$close,
  tgt_pos   = tgt_pos,
  pos_strat = pos_strat,
  tol_pos   = tol_pos,
  strat     = 1L,
  asset     = 8001L,   # e.g. AssetID::BTC_USDT_SWAP
  ctr_size  = 1.0,
  ctr_step  = 1.0,
  lev       = 5.0,
  fee_rt    = 0.0005,
  fund_rt   = 0.0,
  rec       = TRUE     # <-- important: we want recorder attribute
)

# Equity path as plain numeric
eq_num <- as.numeric(eq)

# Attach equity to the candle table
DT[, eq := eq_num]

head(DT)

recorder <- attr(eq, "recorder")

# Basic checks
str(recorder)

# Turn into a data.frame for easier inspection / plotting
rec_df <- as.data.frame(recorder)

head(rec_df)

rec_df$bar_stage <- factor(
  rec_df$bar_stage,
  levels = c(1L, 2L, 3L),
  labels = c("OPEN", "INTRA", "CLOSE")
)

rec_df$status <- factor(
  rec_df$status,
  levels = c(-1L, 0L, 1L),
  labels = c("FAILED", "PENDING", "FILLED")
)

rec_df$action <- factor(
  rec_df$action,
  levels = c(-2L, -1L, 0L, 1L, 2L),
  labels = c("REDUCE", "CLOSE", "NONE", "OPEN", "INCREASE")
)

rec_df$dir <- factor(
  rec_df$dir,
  levels = c(-1L, 0L, 1L),
  labels = c("SHORT", "FLAT", "LONG")
)

head(rec_df)

plot(
  DT$datetime, DT$eq,
  type = "l",
  xlab = "Time (sec)",
  ylab = "Equity",
  main = "Buy & Hold Backtest Equity Curve"
)

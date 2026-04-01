library(data.table)
library(strategyr)

DT <- data.table(datetime = c(0, 60, 120))
state_flat <- list(
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  last_px = 100.0,
  ctr_unit = 0.0,
  avg_price = NaN,
  cash = 10000.0,
  pos_dir = 0L
)

## 1) From flat to long via the public strategy wrapper
plan_open <- strat_buy_and_hold_action_plan(
  DT = DT,
  state = state_flat,
  value = 1.0,
  strat_id = 1L,
  tol_pos = 0
)

plan_open

## 2) Direct planner call with explicit state
plan_flip <- gen_action_plan_rcpp(
  ctr_size = 1.0,
  ctr_step = 1.0,
  lev = 10.0,
  last_px = 100,
  ctr_unit = 50,
  avg_price = 100,
  cash = 10000,
  tgt_pos = -1.0,
  tol_pos = 0,
  strat_id = 2,
  pos_dir = 1
)

plan_flip

devtools::load_all("~/Documents/2025/_2025-07-13_strategyr/strategyr")

## 1) From flat to long (OPEN)
plan_open <- gen_action_plan_rcpp(
  last_px   = 100,
  ctr_unit  = 0,
  avg_price = NaN,
  cash      = 10000,
  tgt_pos   = 1.0,
  tol_pos   = 0,
  strat_id  = 1,
  pos_dir   = 0   # FLAT
)

plan_open

## 2) Flip from long to short (CLOSE + OPEN)
plan_flip <- gen_action_plan_rcpp(
  last_px   = 100,
  ctr_unit  = 50,
  avg_price = 100,
  cash      = 10000,
  tgt_pos   = -1.0,
  tol_pos   = 0,
  strat_id  = 2,
  pos_dir   = 1   # LONG
)

plan_flip
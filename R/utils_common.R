#' @import data.table
#' @useDynLib strategyr, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

# data.table column names used through non-standard evaluation.
utils::globalVariables(c(
  ".", "..cols", "..metric_cols", "..param_cols", ".asset_id", ".date_id",
  ".mine_year", "K", "S", "adv_volume", "asset", "avg_score",
  "avg_score_decay", "avg_total_return", "breadth_ad", "breadth_adl",
  "breadth_adv", "breadth_adv_volume", "breadth_dec", "breadth_dec_volume",
  "breadth_high", "breadth_high_low", "breadth_high_low_ratio", "breadth_low",
  "breadth_ratio", "breadth_trin", "current_notional", "current_units",
  "cycle_bg_price", "cycle_ed_price", "date_id", "datetime", "dec_volume",
  "deferred_price", "delta", "delta_notional", "delta_pos", "delta_units",
  "front_next_spread", "front_next_spread_rel", "front_price", "gamma_pos",
  "h", "is_adv", "is_dec", "is_new_high", "is_new_low", "is_unch",
  "iv_call_otm", "iv_level", "iv_otm_avg", "iv_put_otm", "n_rows",
  "n_windows", "param_id", "positive_return_rate", "prev_close", "price",
  "prior_high", "prior_low", "px_1", "px_2", "px_3", "r",
  "raw_delta_units", "raw_target_units", "rho", "rho_pos", "sigma",
  "target_notional", "target_units", "target_weight", "theta", "theta_pos",
  "trend_value", "type", "vega", "vega_pos", "warmup_insufficient_count",
  "warmup_insufficient_rate"
))

.new_order <- function(inst_id = character(), type = character(), pos = character(), size = numeric(), price = numeric(), pricing_method = character(), trade_reason = character()) {
  data.table::data.table(
    inst_id = inst_id,
    type = type, # OPEN or CLOSE
    pos = pos, # long or short
    size = size,
    price = price,
    pricing_method = pricing_method, # market or limit
    trade_reason = trade_reason
  )  
}

# gen_ind_normalize, gen_ind_zscore, etc.	
# Useful transforms for modeling

.suffix_num <- function(x) {
  gsub("\\.", "p", format(x, trim = TRUE, scientific = FALSE))
}

.EMA_n_to_h <- function(n) {
  log(0.5)/log(1-2/(n+1))
}

.RMA_n_to_h <- function(n) {
  log(0.5)/log(1-1/n)
}

.tau_to_h <- function(tau) {
  tau*log(2)
}

.h_to_EMA_n <- function(h) {
  2/(1 - 0.5^(1/h)) - 1
}

.h_to_RMA_n <- function(h) {
  1/(1 - 0.5^(1/h))
}

.h_to_tau <- function(h) {
  h/log(2)
}



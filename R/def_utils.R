#' @import data.table
#' @useDynLib strategyr, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

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

.h_to_RMA_n <- function(n) {
  1/(1 - 0.5^(1/h))
}

.h_to_tau <- function(h) {
  h/log(2)
}





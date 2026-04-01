.calc_ATR <- function(high, low, close, n) {
  len <- length(close)
  stopifnot(length(high) == len, length(low) == len)
  
  idx <- 2:len
  close_lag <- close[idx - 1L]; hi <- high[idx]; lo <- low[idx]
  tr <- pmax(abs(hi-lo), abs(hi-close_lag), abs(lo-close_lag))
  
  out <- rep(NA_real_, len)
  out[idx] <- ema_ttr_fixed_step(tr, n, TRUE)
  out
}

.calc_ATR_logr <- function(high, low, close, h, scale = 100) {
  len <- length(close)
  stopifnot(length(high) == len, length(low) == len)
	
  idx <- 2:len
  close_lag <- close[idx - 1L]; hi <- high[idx]; lo <- low[idx]
	
	log_hl <- log(hi / lo); log_hc <- log(hi / close_lag); log_lc <- log(lo / close_lag)
	
	tr_core <- pmax(abs(log_hl), abs(log_hc), abs(log_lc))
	tr_up_core   <- pmax(log_hc, 0)
  tr_down_core <- pmax(-log_lc, 0)
	
	tr_logr       <- scale * tr_core
  tr_logr_up    <- scale * tr_up_core
  tr_logr_down  <- scale * tr_down_core
  
  atr_logr <- atr_logr_up <- atr_logr_down <- rep(NA_real_, len)
	tau <- .h_to_tau(h)
	
	atr_logr[idx]        <- ema_fixed_step_tau(tr_logr,        tau)
  atr_logr_up[idx]     <- ema_fixed_step_tau(tr_logr_up,     tau)
  atr_logr_down[idx]   <- ema_fixed_step_tau(tr_logr_down,   tau)

  data.table::data.table(
    atr_logr        = atr_logr,
    atr_logr_up     = atr_logr_up,
    atr_logr_down   = atr_logr_down
  )
}

#' Add Average True Range Features
#'
#' Computes Wilder-style ATR columns and log-range ATR columns on a candle
#' `data.table` in place.
#'
#' @param DT A `data.table` containing `high`, `low`, and `close`.
#' @param ns Integer vector of ATR window sizes for classic ATR columns.
#' @param hs Numeric vector of half-life values for log-range ATR columns.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ATR <- function(DT, ns = c(10, 14, 30), hs = c(12, 24)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c('high', 'low', 'close') %in% names(DT)))
	if (!is.null(ns)) {
	  for (n in ns) {
	    data.table::set(DT, j = paste0("atr_", n), value = .calc_ATR(DT$high, DT$low, DT$close, n))
	  }
	}
	if (!is.null(hs)) {
	  for (h in hs) {
			atr_dt <- .calc_ATR_logr(DT$high, DT$low, DT$close, h)
			base_names <- names(atr_dt)
			new_names  <- paste0(base_names, "_", h)
			data.table::setnames(atr_dt, base_names, new_names)
			DT[, (new_names) := atr_dt]
	  }
	}
  invisible(DT)
}

#' Add ATR Quantile Features
#'
#' Computes rolling quantiles for existing log-range ATR columns on a candle
#' `data.table` in place.
#'
#' @param DT A `data.table` containing `atr_logr_*` columns for each requested
#'   half-life.
#' @param hs Numeric vector of half-life values whose ATR columns should be
#'   summarized.
#' @param window Integer rolling window size.
#' @param thresholds Numeric vector of quantile probabilities.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ATR_quantile <- function(DT, hs = c(12, 24), window = 300, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 0.95)) { # for hs only
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(sprintf('atr_logr_%s', hs) %in% names(DT)))
  for (h in hs) {
    ATR_col <- sprintf('atr_logr_%s', h)
    ATR_quantile_cols <- sprintf('atr_q_%s_%s_%s', thresholds*100, h, window)
    ATR_quantile_values <- data.table::as.data.table(rolling_quantiles(DT[[ATR_col]], window, probs = thresholds))
    DT[, (ATR_quantile_cols) := ATR_quantile_values]
  }
  invisible(DT)
}


# gen_ind_stddev — raw standard deviation
# gen_ind_range — high-low range over N bars

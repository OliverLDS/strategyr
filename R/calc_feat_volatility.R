#' True Range and ATR (price space)
#'
#' Internal helper computing an ATR-style volatility measure from
#' \code{high}, \code{low}, and \code{close} prices using Wilder-style
#' smoothing.
#'
#' @param high Numeric vector of high prices.
#' @param low Numeric vector of low prices.
#' @param close Numeric vector of close prices.
#' @param n Integer smoothing parameter interpreted as the TTR/Wilder
#'   ATR lookback length (controls the decay rate via
#'   \code{alpha = 1 / n}).
#'
#' @details
#' The true range (TR) from bar 2 onward is:
#' \deqn{
#'   TR_t = \max\left(
#'     |H_t - L_t|,
#'     |H_t - C_{t-1}|,
#'     |L_t - C_{t-1}|
#'   \right).
#' }
#'
#' The resulting TR series is smoothed using
#' [ema_ttr_fixed_step()] with \code{wilder = TRUE}, equivalent to
#' Wilder's ATR recursion.
#'
#' The first output value is \code{NA_real_}.
#'
#' @return
#' A numeric vector of ATR values of the same length as \code{close},
#' with \code{NA_real_} in the first position.
#'
#' @keywords internal
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

#' True Range and ATR (log-return scale, half-life smoothing)
#'
#' Internal helper computing a log-return based ATR-like volatility
#' measure expressed in percentage points, smoothed using a
#' half-life parameterisation.
#'
#' @param high Numeric vector of high prices.
#' @param low Numeric vector of low prices.
#' @param close Numeric vector of close prices.
#' @param h Numeric half-life parameter representing the number of bars
#'   required for an innovation’s influence on the ATR estimate to decay
#'   by 50\%.
#'
#' @details
#' From bar 2 onward, the log true range is:
#' \deqn{
#'   TR^{(log)}_t =
#'   100 \cdot
#'   \max\left(
#'     |\log(H_t / L_t)|,
#'     |\log(H_t / C_{t-1})|,
#'     |\log(L_t / C_{t-1})|
#'   \right),
#' }
#' giving a volatility measure in percentage points.
#'
#' The smoothing uses an EMA with time constant:
#' \deqn{ \tau = \frac{h}{\log 2}, }
#' ensuring that the influence of a shock decays by half after \code{h}
#' bars — i.e., \code{h} is a true statistical half-life parameter.
#'
#' The first output value is \code{NA_real_}.
#'
#' @return
#' A numeric vector of ATR values on the log-return scale, with
#' \code{NA_real_} in the first position.
#'
#' @keywords internal
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
	
	atr_logr[idx]        <- ema_tau_fixed_step(tr_logr,        tau)
  atr_logr_up[idx]     <- ema_tau_fixed_step(tr_logr_up,     tau)
  atr_logr_down[idx]   <- ema_tau_fixed_step(tr_logr_down,   tau)

  data.table::data.table(
    atr_logr        = atr_logr,
    atr_logr_up     = atr_logr_up,
    atr_logr_down   = atr_logr_down
  )
}

#' Calculate Average True Range (ATR) indicators
#'
#' Add one or more ATR-style volatility features to a price
#' [`data.table`][data.table::data.table()], in both price units
#' and log-return (percentage) scale.
#'
#' @param DT A [`data.table`][data.table::data.table()] containing
#'   numeric columns `"high"`, `"low"`, and `"close"`. The table is
#'   modified **by reference**.
#'
#' @param ns Optional integer vector controlling ATR variants computed in
#'   price space. For each value \code{n}, a column named \code{atr_n} is
#'   added using Wilder smoothing via [ema_ttr_fixed_step()].
#'
#' @param hs Optional numeric vector controlling ATR variants computed on
#'   log-return scale. Each value \code{h} is interpreted as a
#'   **statistical half-life**, meaning the number of bars required for a
#'   shock’s impact to decay by 50\%. For each \code{h}, a column named
#'   \code{atr_logr_h} is added using [ema_tau_fixed_step()] with
#'   \code{tau = h / log(2)}.
#'
#' @details
#' - `atr_n`: Price-space ATR using Wilder smoothing, consistent with
#'   legacy technical analysis formulations.
#'
#' - `atr_logr_h`: Volatility expressed in percentage points using
#'   log-price true range and statistically meaningful half-life decay.
#'
#' The first observation of each ATR column is set to \code{NA_real_},
#' because ATR is defined from the second bar onward.
#'
#' If `ns` or `hs` is `NULL`, the corresponding family of ATR features
#' is skipped.
#'
#' @return
#' The modified input `DT`, invisibly, with added ATR columns.
#'
#' @examples
#' library(data.table)
#'
#' DT <- data.table(
#'   high  = c(10, 11, 12, 13, 14),
#'   low   = c( 9, 10, 11, 12, 13),
#'   close = c( 9.5, 10.5, 11.5, 12.5, 13.5)
#' )
#'
#' calc_ind_ATR(DT, ns = 14, hs = 12)
#' DT[]
#'
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




# gen_ind_stddev — raw standard deviation
# gen_ind_range — high-low range over N bars

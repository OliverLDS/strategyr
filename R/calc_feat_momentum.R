#' Relative Strength Index (classic, price space)
#'
#' Internal helper computing the classic Wilder-style RSI from a vector
#' of closing prices.
#'
#' @param close Numeric vector of close prices.
#' @param n Integer smoothing parameter passed to
#'   [ema_ttr_fixed_step()] with `wilder = TRUE`. This plays the same
#'   role as the lookback length in the traditional RSI formulation.
#'
#' @details
#' Let \code{d_t = close_t - close_{t-1}}. Define
#' \code{gain_t = max(d_t, 0)} and \code{loss_t = max(-d_t, 0)}.
#' Smoothed gain and loss are computed using a Wilder-style EMA with
#' parameter \code{n}, and the relative strength is
#' \deqn{
#'   RS_t = \frac{EMA(gain_t)}{EMA(loss_t)}.
#' }
#' The RSI is then
#' \deqn{
#'   RSI_t = 100 - \frac{100}{1 + RS_t},
#' }
#' yielding an oscillator on the \eqn{[0, 100]} scale.
#'
#' The first output value is \code{NA_real_}.
#'
#' @return
#' A numeric vector of RSI values with the same length as \code{close},
#' with \code{NA_real_} in the first position.
#'
#' @keywords internal
.calc_RSI <- function(close, n) {
  d <- diff(close); gain <- pmax(d,0); loss <- pmax(-d,0)
  rs  <- ema_ttr_fixed_step(gain, n, TRUE) / ema_ttr_fixed_step(loss, n, TRUE)
  c(NA_real_, 100 - 100 / (1 + rs))
}

#' Relative Strength Index (log-return scale, half-life smoothing)
#'
#' Internal helper computing an RSI-style oscillator on log returns,
#' using a half-life parameterisation for the smoothing.
#'
#' @param close Numeric vector of close prices.
#' @param h Numeric half-life parameter representing the number of bars
#'   required for the influence of a single return shock on the smoothed
#'   gain/loss to decay by 50\%.
#'
#' @details
#' Define log returns
#' \deqn{
#'   r_t = \log\left(\frac{C_t}{C_{t-1}}\right).
#' }
#' Long and short components are
#' \deqn{
#'   \text{long\_gain}_t  = \max(r_t, 0), \quad
#'   \text{short\_gain}_t = \max(-r_t, 0).
#' }
#'
#' Smoothing uses an EMA with time constant
#' \deqn{
#'   \tau = \frac{h}{\log 2},
#' }
#' so that the effect of a shock decays by half after \code{h} bars.
#' The relative strength is
#' \deqn{
#'   RS_t = \frac{EMA_\tau(\text{long\_gain}_t)}
#'               {EMA_\tau(\text{short\_gain}_t)},
#' }
#' and the RSI-style oscillator is
#' \deqn{
#'   RSI^{(log)}_t = 100 - \frac{100}{1 + RS_t},
#' }
#' again lying on the \eqn{[0, 100]} scale.
#'
#' The first output value is \code{NA_real_}.
#'
#' @return
#' A numeric vector of log-return based RSI values with the same length
#' as \code{close}, with \code{NA_real_} in the first position.
#'
#' @keywords internal
.calc_RSI_logr <- function(close, h) {
  len <- length(close)
  log_ret <- log(close[-1]/close[-len])
  long_gain <- pmax(log_ret, 0); short_gain <- pmax(-log_ret, 0)
  tau <- .h_to_tau(h)
  rs  <- ema_tau_fixed_step(long_gain, tau) / ema_tau_fixed_step(short_gain, tau)
  c(NA_real_, 100 - 100 / (1 + rs))
}

#' Calculate Relative Strength Index (RSI) indicators
#'
#' Add one or more RSI-style oscillators to a price
#' [`data.table`][data.table::data.table()], in both classic
#' price-based form and log-return form with half-life smoothing.
#'
#' @param DT A [`data.table`][data.table::data.table()] containing a
#'   numeric column `"close"`. The table is modified **by reference**.
#'
#' @param ns Optional integer vector for classic RSI variants. For each
#'   value \code{n} in \code{ns}, a column named \code{rsi_n} is added,
#'   using Wilder-style EMA smoothing via [ema_ttr_fixed_step()].
#'
#' @param hs Optional numeric vector for log-return based RSI variants.
#'   Each value \code{h} is interpreted as a **half-life in bars** for
#'   the smoothing of gains and losses. For each \code{h}, a column
#'   named \code{rsi_logr_h} is added, using [ema_tau_fixed_step()]
#'   with \code{tau = h / log(2)}.
#'
#' @details
#' - `rsi_n`: Classic RSI on price differences, compatible with
#'   the Wilder/TTR formulation.
#'
#' - `rsi_logr_h`: RSI-style oscillator built from log returns, with
#'   smoothing controlled by a statistically meaningful half-life
#'   parameter \code{h}. This version is invariant to price scale
#'   and aligns better with return-based modelling.
#'
#' The first row of each RSI column is set to \code{NA_real_}, since
#' RSI is defined from the second bar onward.
#'
#' If `ns` or `hs` is `NULL`, the corresponding family of RSI features
#' is skipped.
#'
#' @return
#' The modified input `DT`, invisibly, with added RSI columns.
#'
#' @examples
#' library(data.table)
#'
#' DT <- data.table(
#'   close = c(10, 10.5, 11, 10.8, 11.2, 11.5)
#' )
#'
#' calc_ind_RSI(DT, ns = 14, hs = 12)
#' DT[]
#'
#' @export
calc_RSI <- function(DT, ns = c(9, 14, 21), hs = c(12, 24)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot('close' %in% names(DT))
	if (!is.null(ns)) {
	  for (n in ns) {
	    data.table::set(DT, j = paste0("rsi_", n), value = .calc_RSI(DT$close, n))
	  }
	}
	if (!is.null(hs)) {
	  for (h in hs) {
			data.table::set(DT, j = paste0("rsi_logr_", h), value = .calc_RSI_logr(DT$close, h))
	  }
	}
  invisible(DT)
}


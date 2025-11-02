.gen_strat_desc_EMA_trend <- function(mode, fast, slow) {
  list(
    strat_name = 'EMA_trend',
    strat_par = list(pos_mode = mode, ema_fast = fast, ema_slow = slow),
    strat_label = sprintf("ema_trend_%s_%d_%d", mode, fast, slow)
  )
}

#' @export
gen_pos_EMA_trend <- function(DT, ema_pairs = list(c(8, 21), c(10, 30), c(9, 50), c(20, 50), c(50, 200)), pos_modes = c('both', 'long', 'short')) {
  for (pair in ema_pairs) {
    fast <- pair[1]
    slow <- pair[2]
    trend_sign_col <- sprintf("ema_trend_sign_%d_%d", fast, slow)
    trend_score_col <- sprintf("ema_trend_norm_score_%d_%d", fast, slow)
    stopifnot(all(c(trend_sign_col, trend_score_col) %in% names(DT)))
    for (mode in pos_modes) {
      strat_desc <- .gen_strat_desc_EMA_trend(mode, fast, slow)
      pos_col  <- paste0('pos_', strat_desc$strat_label)
      if (mode == 'both') {
        signal <- DT[[trend_sign_col]] * DT[[trend_score_col]]
      } else if (mode == 'long') {
        signal <- pmax(DT[[trend_sign_col]] * DT[[trend_score_col]], 0)
      } else if (mode == 'short') {
        signal <- pmin(DT[[trend_sign_col]] * DT[[trend_score_col]], 0)
      }
      signal[is.na(signal)] <- 0
      data.table::set(DT, j = pos_col, value = signal)
      data.table::setattr(DT[[pos_col]], "strat_name", strat_desc$strat_name)
      data.table::setattr(DT[[pos_col]], "strat_par", strat_desc$strat_par)
    }
  }
  invisible(DT)
}

# gen_sig_trend.R	Trend-following signals (e.g., EMA cross)
# gen_sig_reversal.R	Reversal patterns (e.g., RSI, engulfing)
# gen_sig_breakout.R	Breakout signals (e.g., Donchian, BB)
# gen_sig_range.R	Range/fade signals (e.g., fib touch, zone bounce)
# gen_sig_volume.R	Volume-based confirmation
# gen_sig_composite.R	Logic-combined signal rules
# 
# gen_sig_trend_ema_cross	Buy/sell on EMA crossover
# gen_sig_reversal_rsi_pullback	Buy during RSI dip in uptrend
# gen_sig_breakout_bbands	Breakout from Bollinger Band
# gen_sig_range_zq_bounce	Signal from zone quantile bounce
# gen_sig_volume_spike	Volume-based momentum trigger
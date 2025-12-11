#' @export
gen_evt_EMA_trend <- function(DT, ema_pairs = list(c(8, 21), c(10, 30), c(9, 50), c(20, 50), c(50, 200)), pos_modes = c('long', 'short')) {
  for (pair in ema_pairs) {
    fast <- pair[1]
    slow <- pair[2]
    trend_sign_col <- sprintf("ema_trend_sign_%d_%d", fast, slow)
    trend_age_col <- sprintf("ema_trend_raw_age_%d_%d", fast, slow)
    stopifnot(all(c(trend_sign_col, trend_age_col) %in% names(DT)))
    trend_sign <- DT[[trend_sign_col]]
    trend_age <- DT[[trend_age_col]]
    for (mode in pos_modes) {
      event_col  <- sprintf("event_ema_cross_%s_%d_%d", mode, fast, slow)
      if (mode == 'long') {
        signal <- ifelse(trend_sign == 1L & trend_age == 0L, 1L, 0L)
      } else if (mode == 'short') {
        signal <- ifelse(trend_sign == -1L & trend_age == 0L, 1L, 0L)
      }
      signal[is.na(signal)] <- 0
      data.table::set(DT, j = event_col, value = signal)
    }
  }
  invisible(DT)
}
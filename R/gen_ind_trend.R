#' @export
gen_ind_SMA <- function(DT, periods = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  for (n in periods) {
    colname <- paste0("sma_", n)
    data.table::set(DT, j = colname, value = sma_cpp(DT[["close"]], n))
  }
  invisible(DT)
}

#' @export
gen_ind_EMA <- function(DT, periods = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  for (n in periods) {
    colname <- paste0("ema_", n)
    data.table::set(DT, j = colname, value = ema_cpp(DT[["close"]], n))
  }
  invisible(DT)
}

#' @export
gen_ind_EMA_trend <- function(DT,
                              ema_pairs = list(c(8, 21), c(10, 30), c(9, 50), c(20, 50), c(50, 200)),
                              slope_window = 3L) {

  for (pair in ema_pairs) {
    fast <- pair[1]
    slow <- pair[2]

    fast_col <- sprintf("ema_%d", fast)
    slow_col <- sprintf("ema_%d", slow)
    stopifnot(all(c(fast_col, slow_col) %in% names(DT)))
    fast_now  <- DT[[fast_col]]
    slow_now  <- DT[[slow_col]]
    
    sig_col  <- sprintf("ema_trend_sign_%d_%d", fast, slow)
    raw_age_col <- sprintf("ema_trend_raw_age_%d_%d", fast, slow)
    raw_score_col <- sprintf("ema_trend_raw_score_%d_%d", fast, slow)
    norm_score_col <- sprintf("ema_trend_norm_score_%d_%d", fast, slow)
    cycle_id_col <- sprintf("ema_trend_cycle_id_%d_%d", fast, slow)

    res <- ema_metrics_cpp(fast_now, slow_now, slope_window = slope_window)
    
    data.table::set(DT, j = sig_col,         value = res$trend_signal)
    data.table::set(DT, j = raw_age_col,     value = res$bars_since_cross)
    data.table::set(DT, j = raw_score_col,   value = res$raw_confidence_score)
    data.table::set(DT, j = norm_score_col,  value = res$confidence_score)
    data.table::set(DT, j = cycle_id_col,  value = res$cycle_id)

  }

  invisible(DT)
}

# gen_ind_DonchianChannels
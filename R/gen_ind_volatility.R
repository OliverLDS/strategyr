#' @export
gen_ind_ATR <- function(
  DT,
  periods = c(10, 14, 30),
  include_rising = TRUE,
  include_delta = FALSE,
  include_pct_slope = FALSE
) {
  stopifnot(all(c('high', 'low', 'close') %in% names(DT)))
  high <- DT$high
  low <- DT$low
  close <- DT$close
  for (n in periods) {
    atr_col <- paste0("atr_", n)
    atr_value <- atr_cpp(high, low, close, n)
    data.table::set(DT, j = atr_col, value = atr_value)
    
    if (include_rising || include_delta || include_pct_slope) {
      atr_prev_value <- c(NA, head(atr_value, -1))
    }
    
    if (include_rising) {
      data.table::set(DT, j = paste0(atr_col, "_rising"), value = atr_prev_value < atr_value)
    }

    if (include_delta) {
      data.table::set(DT, j = paste0(atr_col, "_delta"), value = atr_value - atr_prev_value)
    }

    if (include_pct_slope) {
      data.table::set(DT, j = paste0(atr_col, "_pct_slope"), value = (atr_value - atr_prev_value)/atr_prev_value)
    }

  }
  invisible(DT)
}

#' @export
gen_ind_ATR_ma <- function(DT, periods = c(10, 14, 30), ma_windows = c(20, 50)) {
  for (n in periods) {
    atr_col <- paste0("atr_", n)
    stopifnot(atr_col %in% names(DT))
    atr_col_value <- DT[[atr_col]]
    for (m in ma_windows) {
      ma_col <- paste0(atr_col, "_ma_", m)
      ma_col_value <- data.table::frollmean(atr_col_value, m)
      
      data.table::set(DT, j = ma_col, value = ma_col_value)
      data.table::set(DT, j = paste0(atr_col, "_ratio_ma_", m), value = atr_col_value/ma_col_value)
      data.table::set(DT, j = paste0(atr_col, "_rising_ma_", m), value = atr_col_value>ma_col_value)
    }
  }
  invisible(DT)
}

# gen_ind_stddev — raw standard deviation
# gen_ind_range — high-low range over N bars

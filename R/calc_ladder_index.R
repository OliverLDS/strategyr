# this function assigns cycles (with limited records) to the whole candle's datetime
.assign_cylces_to_datetime <- function(cycles_dt, datetime, detailed_mode = FALSE) { 
  out <- cycles_dt[DT[, .(datetime)], on = 'datetime', roll = TRUE]
  if (!detailed_mode) {
    out <- out[, .(cycle_bg_price, cycle_ed_price)]
  }
  invisible(out)
}

# if ladder center is drifted, we can use the following function to generate center_idx; usually we don't need it.
# in get_fib_ladder_index_cpp, we try to find the nearest 'inner' ladder for current price to determine its optimal weight; so we need a center_idx; by default, it is 50% between H and L of the cycle
# fib_all_cpp can be used to verify this
.center_from_w <- function(w) {
  idx <- max(which(w <= 0), na.rm = TRUE)
  if (!is.finite(idx)) idx <- floor((length(w)-1)/2) + 1L  # 1-based fallback
  idx - 1L  # convert to 0-based for C++
}

# This function actually generates cycle range now; all the following calculation (for price-independent optimal ladder weights and price-dependent position) are in gen_pos_dca_ladder
# ladder_index should be one of 1L:19L or -1L:-19L
#' @export
calc_ladder_index <- function(DT, span = 3, latest_n = NULL, refined = TRUE, min_swing = 0.05, cycle_N = 360L, aware_seconds = 0, cycle_prefix = NULL, center_idx = 9L, detailed_report = FALSE) {

  pivots <- detect_pivots_cpp(DT$high, DT$low, DT$datetime, span = span, latest_n = latest_n, refined = refined, min_swing = min_swing)
  cycles <- detect_main_cycles_cpp(pivots$idx, pivots$price, pivots$datetime, cycle_N = cycle_N)
  cycles_dt <- data.table::data.table(idx = pivots$idx, datetime = pivots$availtime, cycle_bg_price = cycles$cycle_bg_price, cycle_ed_price = cycles$cycle_ed_price)[!duplicated(datetime)]
  
  if (is.null(cycle_prefix)) cycle_prefix <- as.character(cycle_N)
  cycle_names <- c(sprintf('cycle_%s_bg_price', cycle_prefix), sprintf('cycle_%s_ed_price', cycle_prefix))
  cycle_columns <- .assign_cylces_to_datetime(cycles_dt, DT$datetime)
  
  idx_name <- sprintf('ind_dca_ladder_%s', cycle_prefix)
  idx <- sign(cycle_columns[[2]] - cycle_columns[[1]]) * get_fib_ladder_index_cpp(DT$close, cycle_columns[[1]], cycle_columns[[2]], center_idx = center_idx)
  
  if (detailed_report) {
    return(invisible(DT[, (cycle_names) := cycle_columns][, (idx_name) := idx]))
  } else {
    return(invisible(DT[, (idx_name) := idx]))
  }

}
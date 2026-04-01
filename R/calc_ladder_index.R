# this function assigns cycles (with limited records) to the whole candle's datetime
.assign_cylces_to_datetime <- function(cycles_dt, datetime, debug_mode = FALSE) { 
  datetime_dt <- data.table::data.table(datetime = datetime)
  out <- cycles_dt[datetime_dt, on = 'datetime', roll = TRUE]
  if (!debug_mode) {
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
#' Add Signed Fibonacci Ladder Indices
#'
#' Detects recent pivot cycles and maps each bar's close to a signed Fibonacci
#' ladder index. Positive and negative signs encode cycle direction, while the
#' absolute value encodes the ladder level selected by the native engine.
#'
#' @param DT A candle `data.table` containing `datetime`, `high`, `low`, and
#'   `close`.
#' @param span Integer pivot span passed to the pivot detector.
#' @param latest_n Optional integer tail length for pivot detection.
#' @param refined Logical; whether to refine raw pivots before cycle detection.
#' @param min_swing Minimum relative swing used by the pivot refinement step.
#' @param cycle_N Integer cycle lookback width in bars.
#' @param cycle_prefix Optional suffix used in generated column names.
#' @param center_idx Zero-based native center index for the ladder mapping.
#' @param detailed_report Logical; when `TRUE`, cycle boundary columns are also
#'   added.
#'
#' @return The input `DT`, modified by reference and returned invisibly.
#' @export
calc_ladder_index <- function(DT, span = 3, latest_n = NULL, refined = TRUE, min_swing = 0.05, cycle_N = 360L, cycle_prefix = NULL, center_idx = 9L, detailed_report = FALSE) {

  # pivots and cycles are both data.frame from rcpp; by design, they have the same number of rows; key issue here is to put availtime into datetime when you combine pivots and cycles into cycles_dt, so later when you use candle data's full datetime to filling merge, cycle information will be available in the merged row
  # also cycle information is observable in the beginning of the merged row, the ladder_index is only observable in the end of the merged row; therefore, when you try to use the ladder_index information (like build strategy pos), it should be on or after the close of this bar
  pivots <- detect_pivots_cpp(DT$high, DT$low, DT$datetime, span = span, latest_n = latest_n, refined = refined, min_swing = min_swing)
  cycles <- detect_main_cycles_cpp(pivots$idx, pivots$price, pivots$datetime, cycle_N = cycle_N)
  cycles_dt <- data.table::data.table(idx = pivots$idx, datetime = pivots$availtime, cycle_bg_price = cycles$cycle_bg_price, cycle_ed_price = cycles$cycle_ed_price)[!duplicated(datetime)]
  
  if (is.null(cycle_prefix)) cycle_prefix <- as.character(cycle_N)
  cycle_names <- c(sprintf('cycle_%s_bg_price', cycle_prefix), sprintf('cycle_%s_ed_price', cycle_prefix))
  cycle_columns <- .assign_cylces_to_datetime(cycles_dt, DT$datetime)
  
  idx_name <- sprintf('ladder_index_%s', cycle_prefix)
  cycle_direction <- sign(cycle_columns[[2]] - cycle_columns[[1]]) # 1 represents for upward cycle; -1 for downward cycle
  idx <- cycle_direction * get_fib_ladder_index_cpp(DT$close, cycle_columns[[1]], cycle_columns[[2]], center_idx = center_idx)
  
  if (detailed_report) {
    return(invisible(DT[, (cycle_names) := cycle_columns][, (idx_name) := idx]))
  } else {
    return(invisible(DT[, (idx_name) := idx]))
  }

}

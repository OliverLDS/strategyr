.get_bt_cycles <- function(pivots, cycle_N = 360L, aware_seconds = 0) { # cycle_N’s value determines how fresh the cycles are; aware_seconds should be based on the time unit and the spans you used to figure out pivots
  stopifnot(inherits(pivots, "data.table"))
  DT <- data.table::copy(pivots)[order(idx)]
  tz <- attr(DT$datetime, "tzone")
  out <- get_bt_cycles_cpp(
    idx       = as.integer(DT$idx),
    price     = as.numeric(DT$price),
    datetime  = as.numeric(DT$datetime),
    cycle_N   = as.integer(cycle_N)
  )
  res <- DT[, .(datetime = datetime + 60*60*4*aware_seconds, idx)][ # temporarily hard code here
    , `:=`(
      cycle_start   = as.POSIXct(out$cycle_start, origin = "1970-01-01", tz = tz),
      cycle_end     = as.POSIXct(out$cycle_end,   origin = "1970-01-01", tz = tz),
      cycle_bg_price= out$cycle_bg_price,
      cycle_ed_price= out$cycle_ed_price
    )
  ]
  # De-dup same-bar H/L edge case exactly like your original:
  res[!duplicated(datetime)][, !c("idx")]
  # The reason we have duplicated datetime is in some extreme situations, the same 4H bar can be both H and L pivots, which leads to duplicated cycles.
}

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
# ind_dca_ladder should be one of 1L:19L or -1L:-19L
#' @export
gen_ind_dca_ladder <- function(DT, span = 3, latest_n = NULL, refined = TRUE, min_swing = 0.05, cycle_N = 360L, aware_seconds = 0, cycle_prefix = NULL, center_idx = 9L, detailed_report = FALSE) {

  pivots <- get_now_pivots(DT, span = span, latest_n = latest_n, refined = refined, min_swing = min_swing)
  
  if (is.null(cycle_prefix)) cycle_prefix <- as.character(cycle_N)
  cycle_names <- c(sprintf('cycle_%s_bg_price', cycle_prefix), sprintf('cycle_%s_ed_price', cycle_prefix))
  cycle_columns <- .assign_cylces_to_datetime(.get_bt_cycles(pivots, cycle_N, aware_seconds = aware_seconds), DT$datetime)
  
  idx_name <- sprintf('ind_dca_ladder_%s', cycle_prefix)
  idx <- sign(cycle_columns[[2]] - cycle_columns[[1]]) * get_fib_ladder_index_cpp(DT$close, cycle_columns[[1]], cycle_columns[[2]], center_idx = center_idx)
  
  if (detailed_report) {
    return(invisible(DT[, (cycle_names) := cycle_columns][, (idx_name) := idx]))
  } else {
    return(invisible(DT[, (idx_name) := idx]))
  }

}
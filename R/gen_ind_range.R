#' @export
gen_ind_previous_high_low <- function(DT, windows = c(12, 25, 50)) {
  stopifnot(all(c('high', 'low') %in% names(DT)))
  for (w in windows) {
    high_vals <- data.table::shift(data.table::frollapply(DT[["high"]], w, max), 1)
    low_vals <- data.table::shift(data.table::frollapply(DT[["low"]], w, min), 1)
    
    data.table::set(DT, j = paste0("high_max_", w), value = high_vals)
    data.table::set(DT, j = paste0("low_min_", w),  value = low_vals)
  }
  invisible(DT)
}

# gen_ind_inside_bar
# gen_ind_outside_bar
# gen_ind_true_range
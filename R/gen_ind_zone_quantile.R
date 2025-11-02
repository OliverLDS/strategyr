#' @export
gen_ind_zones_quantile_fixed <- function(DT, zone_windows = c(12, 25, 30, 50, 100)) {
  stopifnot(all(c('high', 'low') %in% names(DT)))
  for (w in zone_windows) {
    zones <- zones_quantile_fixed_cpp(DT$high, DT$low, window = w)
    names(zones) <- paste0(names(zones), "_", w)
    data.table::setDT(zones)
    for (col in names(zones)) {
      data.table::set(DT, j = col, value = zones[[col]])
    }
  }
  invisible(DT)
}

#' @export
gen_ind_zones_quantile_dyn <- function(DT) {
  stopifnot(all(c('high', 'low', 'close', 'atr_14') %in% names(DT)))
  zones <- zones_quantile_dyn_cpp(
    high = DT$high,
    low = DT$low,
    close = DT$close,
    atr14 = DT$atr_14,
    mom_threshold = 0.015,
    min_window = 20,
    max_window = 120
  )
  data.table::setDT(zones)
  for (col in names(zones)) {
    data.table::set(DT, j = col, value = zones[[col]])
  }
  invisible(DT)
}
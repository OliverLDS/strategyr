#' @export
gen_ind_zones_pivots <- function(DT) {
  stopifnot(all(c("high", "low", "atr_14") %in% names(DT)))
  zones <- zones_pivots_cpp(
    high = DT$high,
    low = DT$low,
    atr = DT$atr_14,
    span = 2,
    k = 6,
    tol_mult = 0.15
  )
  data.table::setDT(zones)
  for (col in names(zones)) {
    data.table::set(DT, j = col, value = zones[[col]])
  }
  invisible(DT)
}

# gen_ind_pivot_flags
#' @export
gen_ind_BBands <- function(DT, periods = c(5, 10, 20, 30, 50, 100), sd = 2.0) {
  stopifnot('close' %in% names(DT))
  for (n in periods) {
    bb <- bbands_cpp(DT$close, n = n, sd = sd)
    data.table::set(DT, j = paste0("bb_upper_", n), value = bb$up)
    data.table::set(DT, j = paste0("bb_lower_", n), value = bb$dn)
    data.table::set(DT, j = paste0("bb_width_", n), value = bb$up - bb$dn)
  }
  invisible(DT)
}

# gen_ind_Keltner
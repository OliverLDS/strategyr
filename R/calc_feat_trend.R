
#' @export
calc_EMA <- function(DT, ns = c(5, 8, 9, 10, 12, 20, 21, 26, 30, 50, 100, 200)) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot('close' %in% names(DT))
  for (n in ns) {
    colname <- paste0("ema_", n)
    data.table::set(DT, j = colname, value = ema_ttr_fixed_step(DT[["close"]], n, FALSE))
  }
  invisible(DT)
}

# gen_ind_DonchianChannels
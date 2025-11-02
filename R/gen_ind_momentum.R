#' @export
gen_ind_RSI <- function(DT, periods = c(9, 14, 21)) {
  stopifnot('close' %in% names(DT))
  for (n in periods) {
    data.table::set(DT, j = paste0("rsi_", n), value = rsi_cpp(DT$close, n))
  }
  invisible(DT)
}
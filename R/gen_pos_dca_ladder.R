#' @export
gen_pos_dca_ladder <- function(DT) {
  mat <- strategyr:::fib_all_vec_cpp(DT$cycle_m_bg_price, DT$cycle_m_ed_price)
  w <- c(rep(0,6), -1, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 1, rep(0,6))
  idx <- 1L + rowSums(sweep(mat, 1L, DT$close, FUN = "<="))
  DT[, pos_dca_ladder := w[idx]]
}
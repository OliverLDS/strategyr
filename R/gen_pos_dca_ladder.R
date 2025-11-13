# .gen_pos_dca_ladder <- function(close, cycle_bg_price, cycle_ed_price, w = c(rep(0,6), -1, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6, 1, rep(0,6))) {
#   mat <- fib_all_vec_cpp(cycle_bg_price, cycle_ed_price)
#   idx <- 1L + rowSums(sweep(mat, 1L, close, FUN = "<="))
#   invisible(w[idx])
# }

# .gen_w <- function(k = 6L, breakout = TRUE) {
#   stopifnot(k %in% 1L:9L)
#   if (breakout) {
#     w <- c(rep(0, 10-k), -1, rep(0, 2*k-2), 1, rep(0, 10-k))
#   } else {
#     w <- c(rep(0, 10-k), 1, rep(0, 2*k-2), -1, rep(0, 10-k))
#   }
#   invisible(w)
# }

# I believe length(w) should be 19 instead of 20; so the optimal weight of center is always 0, unless you hold drifting assumption; there are still 9 ks glow from the center
.gen_w <- function(k = 3L, breakout = TRUE) {
  stopifnot(k %in% 1L:9L)
  if (breakout) {
    w <- c(rep(0, 9-k), -1, rep(0, 2*k-1), 1, rep(0, 9-k))
  } else {
    w <- c(rep(0, 9-k), 1, rep(0, 2*k-1), -1, rep(0, 9-k))
  }
  invisible(w)
}

.gen_pos_dca_ladder <- function(idx, w = NULL, k = 3L, dir = c('both', 'long', 'short'), mode = c('both', 'continuous', 'reversal'), breakout = TRUE) {
  
  idx <- data.table::shift(idx, type = "lag")
  
  dir <- match.arg(dir)
  mode <- match.arg(mode)
  sign_cycle <- sign(idx)
  idx <- abs(idx)
  if (is.null(w)) w <- .gen_w(k = k, breakout = breakout)
  out <- rep(NA_real_, length(idx))
  ok  <- !is.na(idx) & idx >= 1L & idx <= length(w)
  out[ok] <- w[idx[ok]]
  if (mode == 'continuous') {
    out[sign_cycle == -sign(out)] <- 0L
  } else if (mode == 'reversal') {
    out[sign_cycle == sign(out)] <- 0L
  }
  if (dir == 'long') {
    out[out<0] <- 0L
  } else if (dir == 'short') {
    out[out>0] <- 0L
  }
  invisible(out)
}


#' @export
gen_pos_dca_ladder <- function(DT, idx_col_name = 'ind_dca_ladder_360', pos_col_name = 'pos_dca_ladder_360', w = NULL, k = 3L, dir = c('both', 'long', 'short'), mode = c('both', 'continuous', 'reversal'), breakout = TRUE) {
  dir <- match.arg(dir)
  mode <- match.arg(mode)
  invisible(DT[, (pos_col_name) := .gen_pos_dca_ladder(DT[[idx_col_name]], w = w, k = k, dir = dir, mode = mode, breakout = breakout)])
}

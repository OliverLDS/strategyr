ref_rolling_mean_abs_dev <- function(x, n) {
  len <- length(x)
  out <- rep(NA_real_, len)
  if (n <= 0 || n > len) {
    return(out)
  }

  for (i in seq.int(n, len)) {
    w <- x[(i - n + 1):i]
    out[i] <- if (any(is.na(w))) NA_real_ else mean(abs(w - mean(w)))
  }
  out
}

ref_rolling_sum <- function(x, n) {
  len <- length(x)
  out <- rep(NA_real_, len)
  if (n <= 0 || n > len) {
    return(out)
  }

  for (i in seq.int(n, len)) {
    w <- x[(i - n + 1):i]
    out[i] <- if (any(is.na(w))) NA_real_ else sum(w)
  }
  out
}

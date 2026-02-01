ref_window_quantile_type1ish <- function(w, probs) {
  w <- w[!is.na(w)]
  m <- length(w)
  if (m == 0) return(rep(NaN, length(probs)))
  w <- sort(w)

  vapply(probs, function(p) {
    if (is.na(p) || p < 0 || p > 1) return(NaN)
    k <- if (p <= 0) 1 else if (p >= 1) m else ceiling(p * m)
    k <- max(1, min(m, k))
    w[k]
  }, numeric(1))
}

ref_rolling_quantiles <- function(x, n, probs) {
  len <- length(x)
  P <- length(probs)
  out <- matrix(NaN, nrow = len, ncol = P)
  if (P == 0 || n <= 0 || n > len) return(out)

  for (i in seq_len(len)) {
    if (i >= n) {
      w <- x[(i - n + 1):i]
      out[i, ] <- ref_window_quantile_type1ish(w, probs)
    }
  }
  out
}

testthat::test_that("rolling quantiles matches reference (no NA)", {
  set.seed(1)
  x <- rnorm(50)
  n <- 10
  probs <- c(0, 0.25, 0.5, 0.9, 1)

  got <- rolling_quantiles(x, n, probs)
  ref <- ref_rolling_quantiles(x, n, probs)

  testthat::expect_equal(got, ref, tolerance = 1e-12)
})

testthat::test_that("rolling quantiles matches reference (with NA)", {
  set.seed(2)
  x <- rnorm(40)
  x[c(5, 6, 20)] <- NA_real_
  n <- 8
  probs <- c(0.1, 0.5, 0.9)

  got <- rolling_quantiles(x, n, probs)
  ref <- ref_rolling_quantiles(x, n, probs)

  testthat::expect_equal(got, ref, tolerance = 1e-12)
})

testthat::test_that("rolling quantiles returns all-NaN matrix when n invalid", {
  x <- rnorm(5)
  probs <- c(0.25, 0.5)

  got1 <- rolling_quantiles(x, 0, probs)
  testthat::expect_true(all(is.nan(got1)))

  got2 <- rolling_quantiles(x, 6, probs) # n > len
  testthat::expect_true(all(is.nan(got2)))
})

testthat::test_that("rolling quantiles returns all-NaN if probs invalid (NA/out-of-range)", {
  x <- rnorm(20)
  n <- 5

  got1 <- rolling_quantiles(x, n, c(0.5, NA_real_))
  testthat::expect_true(all(is.nan(got1)))

  got2 <- rolling_quantiles(x, n, c(-0.1, 0.5))
  testthat::expect_true(all(is.nan(got2)))

  got3 <- rolling_quantiles(x, n, c(0.5, 1.1))
  testthat::expect_true(all(is.nan(got3)))
})

testthat::test_that("rolling quantiles all NA input => all NaN output", {
  x <- rep(NA_real_, 30)
  n <- 10
  probs <- c(0.25, 0.5, 0.75)

  got <- rolling_quantiles(x, n, probs)
  testthat::expect_true(all(is.nan(got)))
})


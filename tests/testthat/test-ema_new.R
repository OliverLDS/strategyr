ref_ema_fixed_step_tau <- function(x, tau) {
  len <- length(x)
  if (len == 0 || tau <= 0) stop("invalid tau/len")
  alpha <- 1 - exp(-1 / tau)
  out <- numeric(len)
  ema <- x[1]
  out[1] <- ema
  if (len >= 2) {
    for (i in 2:len) {
      ema <- (x[i] - ema) * alpha + ema
      out[i] <- ema
    }
  }
  out
}

ref_ema_tau_irregular <- function(x, t, tau) {
  len <- length(x)
  if (len == 0 || tau <= 0) stop("invalid tau/len")
  if (length(t) != len) stop("t length mismatch")
  out <- numeric(len)
  ema <- x[1]
  out[1] <- ema
  for (i in 2:len) {
    if (t[i] <= t[i-1]) stop("t must be strictly increasing")
    dt <- t[i] - t[i-1]
    alpha <- 1 - exp(-1 / (tau * dt))
    ema <- (x[i] - ema) * alpha + ema
    out[i] <- ema
  }
  out
}

testthat::test_that("EMA fixed step (tau) matches reference", {
  set.seed(4)
  x <- rnorm(80)
  tau <- 10

  got <- ema_fixed_step_tau(x, tau)
  ref <- ref_ema_fixed_step_tau(x, tau)

  testthat::expect_equal(got, ref, tolerance = 1e-12)
})

testthat::test_that("EMA irregular (tau) matches reference", {
  set.seed(5)
  x <- rnorm(50)
  t <- cumsum(sample(1:3, length(x), replace = TRUE)) # strictly increasing
  tau <- 8

  got <- ema_tau_irregular(x, t, tau)
  ref <- ref_ema_tau_irregular(x, t, tau)

  testthat::expect_equal(got, ref, tolerance = 1e-12)
})


testthat::test_that("EMA fixed step tau errors for tau<=0 or len=0", {
  testthat::expect_error(ema_fixed_step_tau(numeric(0), 10))
  testthat::expect_error(ema_fixed_step_tau(1:5, 0))
  testthat::expect_error(ema_fixed_step_tau(1:5, -1))
})

testthat::test_that("EMA irregular errors for non-increasing t", {
  x <- rnorm(5)
  t <- c(1L, 2L, 2L, 4L, 5L) # not strictly increasing
  testthat::expect_error(ema_tau_irregular(x, t, 10))
})

testthat::test_that("EMA irregular errors for mismatched t length or negative t", {
  x <- rnorm(5)
  testthat::expect_error(ema_tau_irregular(x, 1:4, 10))
  testthat::expect_error(ema_tau_irregular(x, c(1L, -1L, 3L, 4L, 5L), 10))
})
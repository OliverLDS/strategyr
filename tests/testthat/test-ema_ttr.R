ref_ema_ttr <- function(x, n, wilder = FALSE) {
  as.numeric(TTR::EMA(x, n = n, wilder = wilder))
}

testthat::test_that("EMA TTR fixed-step matches TTR::EMA", {
  set.seed(1)
  x <- rnorm(100)
  n <- 14

  expect_equal(
    ema_ttr_fixed_step(x, n, wilder = FALSE),
    ref_ema_ttr(x, n, FALSE),
    tolerance = 1e-12
  )

  expect_equal(
    ema_ttr_fixed_step(x, n, wilder = TRUE),
    ref_ema_ttr(x, n, TRUE),
    tolerance = 1e-12
  )
})

testthat::test_that("EMA TTR fixed step errors for n=0", {
  x <- rnorm(10)
  testthat::expect_error(cpp_ema_ttr_fixed_step(x, 0))
})
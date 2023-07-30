test_that("diagonal of distmatrtix is 0 for identical inputs", {
  n <- 6
  d <- 40
  x1 <- matrix(rnorm(n * d), n, d)
  expect_equal(diag(sqdist(x1, x1)), rep(0, n))
})

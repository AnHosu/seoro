test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 1, 1)
  x2 <- c(3, 5, 2.5)
  k <- white_noise_kernel(sigma = 3)
  expect_equal(k(x1, x2), matrix(9))
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  k <- white_noise_kernel()
  expect_equal(k(x1, x2), matrix(1))
})

test_that("only the diagonal is nonzero", {
  x1 <- matrix(rnorm(20), 5, 4)
  x2 <- matrix(rnorm(24), 6, 4)
  k <- white_noise_kernel(sigma = 2)
  mat <- k(x1, x2)
  expect_equal(diag(mat), rep(4, 5))
  expect_equal(mat - diag(4, 5, 6), matrix(0, 5, 6))
})

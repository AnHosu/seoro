test_that("kernel evaluated on identical vectors is one", {
  x <- rnorm(10)
  k <- matern_kernel(l = 2)
  expect_equal(k(x, x), matrix(1), tolerance = 1e-7)
})

test_that("kernel identities hold", {
  x1 <- matrix(rnorm(12), 3, 4)
  x2 <- matrix(rnorm(20), 5, 4)
  k1 <- matern_kernel(l = 2, nu = 0.5)
  k2 <- exponential_kernel(l = 2)
  k3 <- matern_kernel(l = 2, nu = 150)
  k4 <- rbf_kernel(l = 2)
  expect_equal(k1(x1, x2), k2(x1, x2))
  # RBF and Matern are only equal for nu -> inf, but this should be close
  expect_equal(k3(x1, x2), k4(x1, x2), tolerance = 0.1)
})

test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 1, 1)
  x2 <- c(3, 5, 2.5)
  k <- matern_kernel(l = 3, nu = 2)
  t <- sqrt(22.25) * 2 / 3
  expected <- 0.5 * t^2 * besselK(t, 2)
  expect_equal(k(x1, x2), matrix(expected))
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  k <- matern_kernel()
  t <- 1.5 * sqrt(5)
  expected <- 2^(-1.5) / gamma(2.5) * t^2.5 * besselK(t, 2.5)
  expect_equal(k(x1, x2), matrix(expected))
})

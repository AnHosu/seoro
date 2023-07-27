test_that("kernel evaluated on identical vectors is one", {
  x <- rnorm(10)
  k <- periodic_kernel(l = 2)
  expect_equal(k(x, x), matrix(1), tolerance = 1e-7)
})

test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 1, 1)
  x2 <- c(3, 5, 2.5)
  k <- periodic_kernel(l = 3, omega = 2 * pi)
  expect_equal(k(x1, x2), matrix(exp(- 2 * sin(sqrt(22.25) / 2)^2 / 9)))
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  k <- periodic_kernel()
  expect_equal(k(x1, x2), matrix(exp(- 2 * sin(pi * sqrt(2.25))^2)))
})

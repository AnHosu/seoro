test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 1, 1)
  x2 <- c(3, 5, 2.5)
  k <- polynomial_kernel(sigma = 3, nu = 3)
  expect_equal(k(x1, x2), matrix(19.5^3))
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  k <- polynomial_kernel()
  expect_equal(k(x1, x2), matrix(2.5^2))
})

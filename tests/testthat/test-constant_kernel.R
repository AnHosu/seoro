test_that("dimensions are correct", {
  n1 <- 4
  n2 <- 6
  d <- 3
  x1 <- matrix(rnorm(d * n1), n1, d)
  x2 <- matrix(rnorm(d * n2), n2, d)
  k <- constant_kernel(sigma = 2)
  expect_equal(k(x1, x2), matrix(4, n1, n2))
})

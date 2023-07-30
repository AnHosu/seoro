test_that("kernel evaluated on identical vectors is one", {
  x <- rnorm(10)
  k <- rq_kernel(l = 2, alpha = 0.5)
  expect_equal(k(x, x), matrix(1))
})

test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 5, 3.5)
  x2 <- c(2, 5, 2)
  k <- rq_kernel(l = 0.5, alpha = 0.5)
  expect_equal(k(x1, x2), matrix(1 / sqrt(14)))
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  k <- rq_kernel()
  expect_equal(k(x1, x2), matrix(1 / (4.25 / 2)))
})


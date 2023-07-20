test_that("kernel evaluated on identical vectors is one", {
  x <- rnorm(10)
  k <- rbf_kernel(l = 2)
  expect_equal(k(x, x), matrix(1))
})

test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 1, 1)
  x2 <- c(3, 5, 2.5)
  k <- rbf_kernel(l = 3)
  expect_equal(round(k(x1, x2), 7), matrix(0.2905118))
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  k <- rbf_kernel()
  expect_equal(round(k(x1, x2), 7), matrix(0.3246525))
})

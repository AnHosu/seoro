test_that("kernel evaluated on identical vectors is one", {
  x <- rnorm(10)
  expect_equal(rbf_kernel(x, x), 1)
})

test_that("kernel value can be calculated for a pair of vectors", {
  x1 <- c(1, 1, 1)
  x2 <- c(3, 5, 2.5)
  expect_equal(round(rbf_kernel(x1, x2, l = 3), 7), 0.2905118)
})

test_that("kernel value can be calculated for a pair of scalars", {
  x1 <- 1
  x2 <- 2.5
  expect_equal(round(rbf_kernel(x1, x2, l = 1), 7), 0.3246525)
})

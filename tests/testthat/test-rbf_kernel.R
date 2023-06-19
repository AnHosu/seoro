test_that("kernel evaluated on identical vectors is one", {
  x <- rnorm(10)
  expect_equal(rbf_kernel(x, x), 1)
})

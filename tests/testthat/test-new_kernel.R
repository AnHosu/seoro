test_that("new kernel constructor can be constructed", {
  test_exp_kernel <- new_kernel(
    expr = {
      exp(-sqrt(sqdist(x1, x2)) / l)
    },
    name = "test_exp",
    l = 2
  )
  k1 <- test_exp_kernel(l = 3)
  k2 <- exponential_kernel(l = 3)
  x1 <- matrix(rnorm(10), 5, 2)
  x2 <- matrix(rnorm(6), 3, 2)
  expect_equal(k1(x1, x2), k2(x1, x2))
})

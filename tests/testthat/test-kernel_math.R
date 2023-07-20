test_that("kernel addition works", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rbf_kernel(l = 4)
  x1 <- matrix(rnorm(12), 4, 3)
  x2 <- matrix(rnorm(18), 6, 3)
  k3 <- k1 + k2
  expect_equal(k3(x1, x2), (k1(x1, x2) + k2(x1, x2)), tolerance = 0.0001)
  k4 <- rbf_kernel(l = 3.5)
  k5 <- rbf_kernel(l = 0.2)
  k6 <- k1 + k2 + (k3 + k4) + k5
  expect_equal(
    k6(x1, x2),
    (k1(x1, x2) + k2(x1, x2) + k3(x1, x2) + k4(x1, x2) + k5(x1, x2)),
    tolerance = 0.0001
  )
})

test_that("kernel names remain unique", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rbf_kernel(l = 4)
  k3 <- k1 + k2
  k4 <- rbf_kernel(l = 3.5)
  k5 <- rbf_kernel(l = 0.2)
  k6 <- k1 + k2 + (k3 + k4) + k5
  expect_equal(length(unique(names(attr(k6, ".quo")))), 6)
})

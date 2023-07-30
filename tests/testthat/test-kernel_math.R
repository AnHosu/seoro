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

test_that("kernel multiplication works", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rbf_kernel(l = 4)
  x1 <- matrix(rnorm(12), 4, 3)
  x2 <- matrix(rnorm(18), 6, 3)
  k3 <- k1 * k2
  expect_equal(k3(x1, x2), (k1(x1, x2) * k2(x1, x2)), tolerance = 0.0001)
  k4 <- rbf_kernel(l = 3.5)
  k5 <- rbf_kernel(l = 0.2)
  k6 <- k1 * k2 * (k3 * k4) * k5
  expect_equal(
    k6(x1, x2),
    (k1(x1, x2) * k2(x1, x2) * k3(x1, x2) * k4(x1, x2) * k5(x1, x2)),
    tolerance = 0.0001
  )
})

test_that("kernel names remain unique after addition", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rbf_kernel(l = 4)
  k3 <- k1 + k2
  k4 <- rbf_kernel(l = 3.5)
  k5 <- rbf_kernel(l = 0.2)
  k6 <- k1 + k2 + (k3 + k4) + k5
  expect_equal(length(unique(names(attr(k6, ".quo")))), 6)
})

test_that("kernel names remain unique after multiplication", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rbf_kernel(l = 4)
  k3 <- k1 * k2
  k4 <- rbf_kernel(l = 3.5)
  k5 <- rbf_kernel(l = 0.2)
  k6 <- k1 * k2 * (k3 * k4) * k5
  expect_equal(length(unique(names(attr(k6, ".quo")))), 6)
})

test_that("the interface is preserved for addition", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rq_kernel(l = 2, alpha = 1)
  k3 <- k1 + k2
  x1 <- 1
  x2 <- 4
  x3 <- c(1, 3, 2.5)
  x4 <- c(1.1, 3, 4)
  expect_equal(k3(x1, x2), k1(x1, x2) + k2(x1, x2))
  expect_equal(k3(x1, x2), k3(matrix(x1), matrix(x2)))
  expect_equal(k3(x3, x4), k1(x3, x4) + k2(x3, x4))
  expect_equal(k3(x3, x4), k3(matrix(x3, nrow = 1), matrix(x4, nrow = 1)))
})

test_that("the interface is preserved for multiplication", {
  k1 <- rbf_kernel(l = 2)
  k2 <- rq_kernel(l = 2, alpha = 1)
  k3 <- k1 * k2
  x1 <- 1
  x2 <- 4
  x3 <- c(1, 3, 2.5)
  x4 <- c(1.1, 3, 4)
  expect_equal(k3(x1, x2), k1(x1, x2) * k2(x1, x2))
  expect_equal(k3(x1, x2), k3(matrix(x1), matrix(x2)))
  expect_equal(k3(x3, x4), k1(x3, x4) * k2(x3, x4))
  expect_equal(k3(x3, x4), k3(matrix(x3, nrow = 1), matrix(x4, nrow = 1)))
})

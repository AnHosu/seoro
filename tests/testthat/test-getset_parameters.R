test_that("parameters can be retrieved from a kernel", {
  k1 <- rbf_kernel(l = 2)
  expect_equal(get_par(k1), list(rbf = list(l = 2)))
})

sqdist <- function(x1, x2, tolerance = 9) {
  round(
    sweep(
      (- 2 * (x1 %*% t(x2))) + rowSums(x1**2, dims = 1),
      2,
      rowSums(x2**2, dims = 1),
      `+`
    ),
    tolerance
  )
}

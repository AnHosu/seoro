#' Matrix of Squared Euclidean Distances
#' 
#' The matrix of squared 2-norm distances between two matrices.
#'
#' @param x1 matrix
#' @param x2 matrix
#' @param tolerance integer, number of digits. The distances are rounded to this
#' tolerance primarily to ensure that the distance between two identical vectors
#' is exactly zero. 
#'
#' @return matrix
#' @export
#'
#' @examples
#' sqdist(matrix(rnorm(4), 2, 2), matrix(rnorm(10), 5, 2))
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

#' Polynomial Kernel
#' 
#' The polynomial kernel on a pair of matrices.
#' The plonomial kernel is defined as:
#' \deqn{ k(x1, x2) = (\sigma^2 + dot(x1, x2))^\nu}
#'
#' @param sigma homogeneity parameter, scalar.
#' @param nu degree parameter, positive integer
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- polynomial_kernel(sigma = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
polynomial_kernel <- new_kernel(
  expr = {
    (sigma^2 + x1 %*% t(x2))^nu
  },
  name = "linear",
  sigma = 0,
  nu = 2
)

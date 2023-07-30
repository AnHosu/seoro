#' Linear Kernel
#' 
#' The linear kernel on a pair of matrices.
#' The linear kernel is defined as:
#' \deqn{ k(x1, x2) = \sigma^2 + dot(x1, x2)}
#'
#' @param sigma homogeneity parameter, scalar.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- linear_kernel(sigma = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
linear_kernel <- new_kernel(
  expr = {
    sigma^2 + x1 %*% t(x2)
  },
  name = "linear",
  sigma = 0
)

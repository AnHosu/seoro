#' Constant Kernel
#' 
#' The constant aka the white noise kernel on a pair of vectors.
#' The RBF kernel is defined as:
#' \deqn{ \sigma^2 }
#'
#' @param sigma scale parameter, scalar
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- constant_kernel(sigma = 2)
#' k(rnorm(4), rnorm(4, 1, 2))
constant_kernel <- new_kernel(
  expr = {
    matrix(sigma^2, dim(x1)[[1]], dim(x2)[[1]])
  },
  name = "constant",
  sigma = 1
)

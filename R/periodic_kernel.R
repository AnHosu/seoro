#' Basic Periodic Kernel
#' 
#' The basic periodic kernel on a pair of matrices.
#'
#' @param l Length scale.
#' @param omega period parameter, scalar.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- periodic_kernel(l = 3, omega = 2 * pi)
#' k(rnorm(4), rnorm(4, 1, 2))
periodic_kernel <- new_kernel(
  expr = {
    exp(-2 * sin(pi * sqrt(sqdist(x1, x2)) / omega)^2 / l^2)
  },
  name = "periodic",
  l = 1,
  omega = 1
)

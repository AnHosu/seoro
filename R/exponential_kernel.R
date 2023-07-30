#' Exponential Kernel
#' 
#' The exponential kernel on a pair of matrices.
#' The exponential kernel is defined as:
#' \deqn{ k(x1, x2) = e^( -(||x1 - x2|| / l)) }
#'
#' @param l Length scale.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- exponential_kernel(l = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
exponential_kernel <- new_kernel(
  expr = {
    exp(-sqrt(sqdist(x1, x2)) / l)
  },
  name = "exponential",
  l = 1
)

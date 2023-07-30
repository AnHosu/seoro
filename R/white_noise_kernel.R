#' White Noise Kernel
#' 
#' The white noise aka the white noise kernel on a pair of matrices.
#' The white noise kernel is defined as:
#' \deqn{ \sigma^2\delta_{ij} }
#'
#' @param sigma scale parameter, scalar.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- white_noise_kernel(sigma = 2)
#' k(rnorm(4), rnorm(4, 1, 2))
white_noise_kernel <- new_kernel(
  expr = {
    diag(sigma^2, dim(x1)[[1]], dim(x2)[[1]])
  },
  name = "constant",
  sigma = 1
)

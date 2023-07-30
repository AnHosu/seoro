#' Radial Basis Function Kernel
#' 
#' The Radial Basis Function (RBF) kernel on a pair of matrices.
#' The RBF kernel is defined as:
#' \deqn{ k(x1, x2) = e^( -0.5(||x1 - x2|| / l)^2) }
#'
#' @param l Length scale.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- rbf_kernel(l = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
rbf_kernel <- new_kernel(
  expr = {
    exp(-0.5 * sqdist(x1, x2) / l**2)
  },
  name = "rbf",
  l = 1
)

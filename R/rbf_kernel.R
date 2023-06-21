#' Radial Basis Function Kernel
#' 
#' The Radial Basis Function (RBF) kernel on a pair of vectors.
#' The RBF kernel is defined as:
#' \deqn{ k(x1, x2) = e^( -0.5(||x1 - x2|| / l)^2) }
#'
#' @param x1 First numeric vector of length d
#' @param x2 Second numeric vector of length d
#' @param l Length scale. Scalar or numeric vector of length d.
#'
#' @return scalar
#' @export
#'
#' @examples
#' rbf_kernel(rnorm(4), rnorm(4, 1, 2), l = 3)
rbf_kernel <- function(x1, x2, l = 1.0) {
  exp(- 0.5 * sum(((x1 - x2) / l)^2))
}

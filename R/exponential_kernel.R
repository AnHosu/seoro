#' Exponential Kernel
#' 
#' The Radial Basis Function (RBF) kernel on a pair of vectors.
#' The RBF kernel is defined as:
#' \deqn{ k(x1, x2) = e^( -(||x1 - x2|| / l)) }
#'
#' @param l Length scale. Scalar or numeric vector of length d.
#'
#' @return scalar
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- exponential_kernel(l = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
exponential_kernel <- new_kernel(
  expr = {
    sqdist <- sweep(
      (- 2 * (x1 %*% t(x2))) + rowSums(x1**2, dims = 1),
      2,
      rowSums(x2**2, dims = 1),
      `+`
    )
    exp(-sqrt(sqdist) / l)
  },
  name = "exponential",
  l = 1
)
#' Matérn Kernel
#' 
#' The Matérn kernel on a pair of matrices.
#'
#' @param l Length scale.
#' @param nu smoothness parameter, positive scalar.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- exponential_kernel(l = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
matern_kernel <- new_kernel(
  expr = {
    distance <- sqrt(sqdist(x1, x2))
    term <- sqrt(2 * nu) * distance / l
    K <- (2^(1 - nu) / gamma(nu)) * (term^nu) * besselK(term, nu)
    K[distance == 0] <- 1
    K
  },
  name = "matern",
  l = 1,
  nu = 2.5
)

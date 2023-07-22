#' Radial Basis Function Kernel
#' 
#' The Radial Basis Function (RBF) kernel on a pair of vectors.
#' The RBF kernel is defined as:
#' \deqn{ k(x1, x2) = e^( -0.5(||x1 - x2|| / l)^2) }
#'
#' @param l Length scale. Scalar or numeric vector of length d.
#'
#' @return scalar
#' @include new_kernel.R
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
#' k <- rbf_kernel(l = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
rbf_kernel <- new_kernel(
  expr = {
    (- 2*(x1 %*% t(x2))) %>%
      `+`(rowSums(x1**2, dims = 1)) %>%
      sweep(2, rowSums(x2**2, dims = 1), `+`) %>%
      `*`(-0.5 / l**2) %>%
      exp()
  },
  name = "rbf",
  l = 1
)

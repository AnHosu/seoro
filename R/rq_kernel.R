#' Rational Quadratic Kernel
#' 
#' The Rational Quadratic (RQ) kernel on a pair of matrices.
#' The RQ kernel is defined as:
#' \deqn{ k(x1, x2) = (1 + (||x1 - x2||^2 / (2\alpha l^2)))^(-\alpha) }
#'
#' @param l Length scale.
#' @param alpha mixture parameter, positive scalar.
#'
#' @return matrix
#' @include new_kernel.R
#' @export
#'
#' @examples
#' k <- rq_kernel(l = 0.5, alpha = 0.5)
#' k(rnorm(4), rnorm(4, 1, 2))
rq_kernel <- new_kernel(
  expr = {
    sqdist <- sweep(
      (- 2 * (x1 %*% t(x2))) + rowSums(x1**2, dims = 1),
      2,
      rowSums(x2**2, dims = 1),
      `+`
    )
    (1 + sqdist / (2 * alpha * l^2))^(-alpha)
  },
  name = "rq",
  l = 1,
  alpha = 1
)

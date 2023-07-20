#' Constant Kernel
#' 
#' The constant aka the white noise kernel on a pair of vectors.
#' The RBF kernel is defined as:
#' \deqn{ \sigma^2 }
#'
#' @param sigma scale parameter, scalar
#'
#' @return matrix
#' @export
#'
#' @examples
#' k <- constant_kernel(sigma = 2)
#' k(rnorm(4), rnorm(4, 1, 2))
constant_kernel <- function(sigma = 1) {
  .exp <- rlang::quo(matrix(sigma^2, dim(x1)[[1]], dim(x2)[[1]]))
  structure(
    function(x1, x2) {
      if (is.null(dim(x1))) dim(x1) <- c(1, length(x1))
      if (is.null(dim(x2))) dim(x2) <- c(1, length(x2))
      rlang::eval_tidy(.exp, data = list(x1 = x1, x2 = x2))
    },
    .exp = .exp,
    .quo = rlang::set_names(rlang::quos(!!.exp), "constant"),
    class = "seoro_kernel"
  )
}

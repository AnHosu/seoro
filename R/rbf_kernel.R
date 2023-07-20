#' Radial Basis Function Kernel
#' 
#' The Radial Basis Function (RBF) kernel on a pair of vectors.
#' The RBF kernel is defined as:
#' \deqn{ k(x1, x2) = e^( -0.5(||x1 - x2|| / l)^2) }
#'
#' @param l Length scale. Scalar or numeric vector of length d.
#'
#' @return scalar
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
#' k <- rbf_kernel(l = 3)
#' k(rnorm(4), rnorm(4, 1, 2))
rbf_kernel <- function(l = 1) {
  .exp <- rlang::quo(
    (- 2*(x1 %*% t(x2))) %>%
      `+`(rowSums(x1**2, dims = 1)) %>%
      sweep(2, rowSums(x2**2, dims = 1), `+`) %>%
      `*`(-0.5 / l**2) %>%
      exp()
  )
  structure(
    function(x1, x2) {
      if (is.null(dim(x1))) dim(x1) <- c(1, length(x1))
      if (is.null(dim(x2))) dim(x2) <- c(1, length(x2))
      rlang::eval_tidy(.exp, data = list(x1 = x1, x2 = x2))
    },
    .exp = .exp,
    .quo = rlang::set_names(rlang::quos(!!.exp), "rbf"),
    class = "seoro_kernel"
  )
}

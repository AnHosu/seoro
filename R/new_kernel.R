#' Construct a New Kernel Constructor
#'
#' @param expr kernel expression that calculates the covariance matrix given two
#' input matrices x1 and x2.
#' @param name name of the kernel
#' @param ... default values for any parameters in the expression. Arguments
#' must be named.
#'
#' @return function
#' @export
new_kernel <- function(expr, name, ...) {
  expr <- rlang::enexpr(expr)
  f <- function(l = 1) {
    .exp <- rlang::quo(!!expr)
    structure(
      function(x1, x2) {
        if (is.null(dim(x1))) dim(x1) <- c(1, length(x1))
        if (is.null(dim(x2))) dim(x2) <- c(1, length(x2))
        rlang::eval_tidy(.exp, data = list(x1 = x1, x2 = x2))
      },
      .exp = .exp,
      .quo = rlang::set_names(rlang::quos(!!.exp), name),
      class = "seoro_kernel"
    )
  }
  formals(f) <- rlang::pairlist2(...)
  return(f)
}

#' Add Two Kernels
#'
#' @param l kernel
#' @param r kernel
#'
#' @return kernel
#' @export
`+.seoro_kernel` <- function(l, r) {
  # TODO: handle the case of l or r being a scalar
  l_quo <- attr(l, ".quo")
  r_quo <- attr(r, ".quo")
  .quo <- rlang::set_names(
    c(l_quo, r_quo),
    make.unique(c(names(l_quo), names(r_quo)), sep = "_")
  )
  .exp <- rlang::quo(!!attr(l, ".exp") + !!attr(r, ".exp"))
  structure(
    function(x1, x2) {
      if (is.null(dim(x1))) dim(x1) <- c(1, length(x1))
      if (is.null(dim(x2))) dim(x2) <- c(1, length(x2))
      rlang::eval_tidy(.exp, data = list(x1 = x1, x2 = x2))
    },
    .exp = .exp,
    .quo = .quo,
    class = "seoro_kernel"
  )
}

#' Multiply Two Kernels
#'
#' @param l kernel
#' @param r kernel
#'
#' @return kernel
#' @export
`*.seoro_kernel` <- function(l, r) {
  # TODO: handle the case of l or r being a scalar
  # TODO: tests
  l_quo <- attr(l, ".quo")
  r_quo <- attr(r, ".quo")
  .quo <- rlang::set_names(
    c(l_quo, r_quo),
    make.unique(c(names(l_quo), names(r_quo)), sep = "_")
  )
  .exp <- rlang::quo(!!attr(l, ".exp") * !!attr(r, ".exp"))
  structure(
    function(x1, x2) {
      if (is.null(dim(x1))) dim(x1) <- c(1, length(x1))
      if (is.null(dim(x2))) dim(x2) <- c(1, length(x2))
      rlang::eval_tidy(.exp, data = list(x1 = x1, x2 = x2))
    },
    .exp = .exp,
    .quo = .quo,
    class = "seoro_kernel"
  )
}

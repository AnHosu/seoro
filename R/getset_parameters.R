#' Get and Set Kernel Parameters
#'
#' @param k a kernel
#'
#' @name parameters
NULL

#' @export
#' @rdname parameters
get_par <- function(k) UseMethod("get_par")

#' @export
#' @rdname parameters
get_par.seoro_kernel <- function(k) {
  quos <- attr(k, ".quo")
  purrr::map(quos, function(q) {
    env <- rlang::quo_get_env(q)
    rlang::env_get_list(env, ls(env, all.names = FALSE))
  })
}

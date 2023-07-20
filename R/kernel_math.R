`+.seoro_kernel` <- function(l, r) {
  l_quo <- attr(l, ".quo")
  r_quo <- attr(r, ".quo")
  .quo <- rlang::set_names(
    c(l_quo, r_quo),
    make.unique(c(names(l_quo), names(r_quo)), sep = "_")
  )
  .exp <- rlang::quo(!!attr(l, ".exp") + !!attr(r, ".exp"))
  structure(
    function(x1, x2) {
      rlang::eval_tidy(.exp, data = list(x1 = x1, x2 = x2))
    },
    .exp = .exp,
    .quo = .quo,
    class = "seoro_kernel"
  )
}

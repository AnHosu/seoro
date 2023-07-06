
rbf_kernel <- function(l = 1) {
  exp <- rlang::expr(exp(- 0.5 * sum(((x1 - x2) / l)^2)))
  par <- list(l = l)
  structure(
    function(x1, x2) {
      #rlang::eval_tidy(exp, data = list(x1 = x1, x2 = x2), env = par)
      eval(exp)
    },
    .exp = exp,
    .par = par,
    class = "seoro_kernel"
  )
}

constant_kernel <- function(sigma = 1) {
  exp <- rlang::expr(sigma^2)
  par <- list(sigma = sigma)
  structure(
    function(x1, x2) {
      #rlang::eval_tidy(exp, data = list(x1 = x1, x2 = x2), env = par)
      eval(exp)
    },
    .exp = exp,
    .par = par,
    class = "seoro_kernel"
  )
}


k <- rbf_kernel(l = 2)

k(1, 4)

# `update_parameters<-` <- function(k, ...) UseMethod("update_parameters<-")
# `update_parameters<-.seoro_kernel` <- function(k, value) {
#   # par <- rlang::env(!!! value) # TODO check match with current .par
#   # TODO check match with current .par
#   exp <- attr(k, ".exp")
#   structure(
#     function(x1, x2) {
#       #rlang::eval_tidy(exp, data = list(x1 = x1, x2 = x2), env = par)
#       eval(exp)
#     },
#     .exp = exp,
#     .par = value,
#     class = "seoro_kernel"
#   )
# }

update_parameters <- function(k, ...) UseMethod("update_parameters")
update_parameters.seoro_kernel <- function(k, ...) {
  # par <- rlang::env(...) # TODO check match with current .par
  # TODO check match with current .par
  exp <- attr(k, ".exp")
  structure(
    function(x1, x2) {
      rlang::eval_tidy(exp, data = list(x1 = x1, x2 = x2, ...))
    },
    .exp = exp,
    .par = list(...),
    class = "seoro_kernel"
  )
}

# update_parameters(k) <- list(l = 5)

k1 <- update_parameters(k, l = 1)
k(1, 4)
k1(1, 4)
attr(k1, ".par")$l

`+.seoro_kernel` <- function(l, r) {
  # TODO handle case where parameters clash
  exp <- rlang::expr(!!attr(l, ".exp") + !!attr(r, ".exp"))
  par <- c(attr(l, ".par"), attr(r, ".par"))
  structure(
    function(x1, x2) {
      rlang::eval_tidy(exp, data = c(list(x1 = x1, x2 = x2), par))
    },
    .exp = exp,
    .par = par,
    class = "seoro_kernel"
  )
}
k + k

ck <- constant_kernel(sigma = 2)
comp_k <- k + ck
comp_k(1, 4)

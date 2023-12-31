% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rq_kernel.R
\name{rq_kernel}
\alias{rq_kernel}
\title{Rational Quadratic Kernel}
\usage{
rq_kernel(l = 1, alpha = 1)
}
\arguments{
\item{l}{Length scale.}

\item{alpha}{mixture parameter, positive scalar.}
}
\value{
matrix
}
\description{
The Rational Quadratic (RQ) kernel on a pair of matrices.
The RQ kernel is defined as:
\deqn{ k(x1, x2) = (1 + (||x1 - x2||^2 / (2\alpha l^2)))^(-\alpha) }
}
\examples{
k <- rq_kernel(l = 0.5, alpha = 0.5)
k(rnorm(4), rnorm(4, 1, 2))
}

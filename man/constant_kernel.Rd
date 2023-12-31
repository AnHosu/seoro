% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constant_kernel.R
\name{constant_kernel}
\alias{constant_kernel}
\title{Constant Kernel}
\usage{
constant_kernel(sigma = 1)
}
\arguments{
\item{sigma}{scale parameter, scalar.}
}
\value{
matrix
}
\description{
The constant kernel on a pair of matrices.
The constant kernel is defined as:
\deqn{ \sigma^2 }
}
\examples{
k <- constant_kernel(sigma = 2)
k(rnorm(4), rnorm(4, 1, 2))
}


# seoro - Kernel Functions in R

<!-- badges: start -->
<!-- badges: end -->

The goal of seoro is to provide simple and robust Kernel functions in R.

## Installation

You can install the development version of seoro from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AnHosu/seoro")
```

## Example Usage

``` r
seoro::rbf_kernel(rnorm(4), rnorm(4, 1, 2), l = 3)
#> [1] 0.09685834
```

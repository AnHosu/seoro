
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
my_kernel <- seoro::rbf_kernel(l = 2)
my_kernel(x1 = rnorm(2), x2 = rnorm(2))
#>           [,1]
#> [1,] 0.6035253
```

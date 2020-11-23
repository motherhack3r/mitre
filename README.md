
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mitre

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/motherhack3r/mitre.svg?branch=devel)](https://travis-ci.org/motherhack3r/mitre)
[![CRAN
status](https://www.r-pkg.org/badges/version/mitre)](https://cran.r-project.org/package=mitre)
<!-- badges: end -->

mitre package is designed to provide easy access to cybersecurity data
standards. You can expect functions to get data frames for every
standard object. For deep exploratory, it provide visNetwork objects for
graph and relationship analysis.

## Installation

You can install the released version of mitre from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mitre")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("motherhack3r/mitre")
```

## Basic usage

This is a basic example which shows you how to view shield tactics ids
and names:

``` r
library(mitre)
tactics <- getShieldTactics()
tactics[, 1:2]
#>        id       name
#> 1 DTA0001    Channel
#> 2 DTA0002    Collect
#> 3 DTA0003    Contain
#> 4 DTA0004     Detect
#> 5 DTA0005    Disrupt
#> 6 DTA0006 Facilitate
#> 7 DTA0007 Legitimize
#> 8 DTA0008       Test
```

## Network visualization

This is a example which shows you how to visualize shield network:

``` r
# library(mitre)
# shieldnet <- getShieldNetwork()
# shieldnet
```

For more advanced visualizations with shiny, check vignettes.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://pkgdown.r-lib.org/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.

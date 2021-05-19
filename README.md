
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mitre

<!-- badges: start -->

[![Travis build
status](https://www.travis-ci.com/motherhack3r/mitre.svg?branch=master)](https://www.travis-ci.com/motherhack3r/mitre)
[![CRAN
status](https://www.r-pkg.org/badges/version/mitre)](https://cran.r-project.org/package=mitre)
[![](https://cranlogs.r-pkg.org/badges/grand-total/mitre)](https://cran.r-project.org/package=mitre)
[![](https://www.rdocumentation.org/badges/version/mitre)](https://www.rdocumentation.org/packages/mitre)
<!-- badges: end -->

mitre package is designed to provide easy access to cybersecurity data
standards. You can expect sample data frames for every standard object.
It provide a directed graph with all relationships for deep exploratory
analysis. Using scripts in data-raw folder you can build the data sets
using the latest public source files.

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

## Explore cybersecurity standards

This is a basic example which shows you how to view shield tactics ids
and names:

``` r
library(mitre)
shield <- mitre::shield.tactics
shield[, c("id", "name")]
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

This example shows the distribution of vulnerability risk:

``` r
hist(mitre::cve.nist$cvss3.score, 
     main = "CVE risk distribution", xlab = "cvss3")
```

<img src="man/figures/README-example2-1.png" width="100%" />

## Network visualization

This is a example which shows you how to visualize shield network:

``` r
# library(visNetwork)
# mitrenet <- mitre::build_network(as_igraph = FALSE)
# g <- visNetwork::visNetwork(nodes = mitrenet$nodes,
#                             edges = mitrenet$edges)
# g
```

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://pkgdown.r-lib.org/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.

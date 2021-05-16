
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
standards. You can expect functions to get data frames for every
standard object. It provide a directed graph with all relationships for
deep exploratory analysis. You could avoid full parsing process using
the latest public Rdata sets.

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
# library(mitre)
# mitredata <- mitre::getLatestDataSet()
# shield <- mitredata$standards$shield
# shield$tactics[, c("id", "name")]
```

This example shows the number of ATT&CK Techniques by domain:

``` r
# table(mitredata$standards$attck$techniques$domain)
```

## Network visualization

This is a example which shows you how to visualize shield network:

``` r
# library(visNetwork)
# g <- visNetwork::visNetwork(nodes = shield$shieldnet$nodes,
#                             edges = shield$shieldnet$edges)
# g
```

![Shield network zoom in](inst/doc/images/readme_example.png)

Find some more examples in vignettes to build your own graph like
[this](https://security.shinyapps.io/mitreshield/).

## Advanced exploratory analysis

Check [this](https://datadrivensecurity-project.web.app/) proof of
concept project. It is a Rmarkdown document performing an exploratory
analysis with mitre network and [this data
set](https://github.com/hrbrmstr/attckr/tree/master/inst/extdat).

-   [Exploratory
    analysis](https://github.com/Barbero95/DataDrivenSecurity-Project):
    developed by [barbero95](https://github.com/Barbero95) and
    [Marta](https://github.com/martavilab).
-   Sample incidents data set from rpackage
    [attckr](https://github.com/hrbrmstr/attckr/tree/master/inst/extdat)
    developed by [Bob Rudis](https://github.com/hrbrmstr).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://pkgdown.r-lib.org/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.

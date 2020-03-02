
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RDO (Reproducible Data Objects)

<img src='man/figures/logo.png' align="right" height="139" />

by [Kamil Wais](https://kalimu.github.io/)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Licence](https://img.shields.io/badge/licence-MIT-blue.svg)](https://www.r-project.org/Licenses/MIT)
[![HitCount](http://hits.dwyl.com/openpharma/rdo.svg)](http://hits.dwyl.com/openpharma/rdo)
<!-- badges: end -->

> The **RDO** R package allows you to create and interact with
> **Reproducible Data Objects (RDO)**. An RDO object **encapsulates both
> data and R code** needed to reproduce the data.

> The development of the RDO package is **supported by Roche**.

RDOs can have other RDOs as dependencies and can be composed into
complex hierarchy (RDO trees).

Interacting with such RDO tree is similar to interacting with a single
RDO, as each RDO has the same programming interface thanks to composite
programming design pattern.

You can (re)run the RDO reproducible code and refresh the data cache,
check status, validate if the code still gives the same cached data,
clear data cache, access code and data cache of any of the dependencies.

Both a single RDO and a complex RDO tree can be cloned, and code of the
cloned dependencies can be also modified later.

## Installation

~~You can install the released version of RDO from
[CRAN](https://CRAN.R-project.org) with:~~

``` r
# install.packages("RDO")
# Not yet on CRAN
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("openpharma/RDO")
```

## Basic example

Let’s assume we have some exemplary R code that changes a dataset in
some way:

``` r
data_mtcars <- mtcars
data_mtcars <- data_mtcars[data_mtcars$mpg > 30, c("cyl", "mpg")]
```

Now, we would like to encapsulate this code and the changed data inside
an RDO.

The `RDO` is an `R6` class so to create a new RDO object just use the
constructor function:

``` r
# library(RDO)
data_mtcars <- RDO::RDO$new(name = "data_mtcars")
data_mtcars
#> <RDO>
#> Name: data_mtcars 
#> Dependencies: none.
#> Status:
#> - created:        2020-03-02 12:51:37 UTC 
#> - last changed:   2020-03-02 12:51:37 UTC 
#> - last touched:   (never) 
#> - last validated: (never) 
#> - last run time:   
#> - run time total: 0 
#> - is validated?   FALSE 
#> - is locked?      FALSE 
#> - cache size:     0 Mb 
#> - cached total:   0 Mb
```

Now we can add the reproducible R code as an R `expression`.

``` r
data_mtcars$code <- expression({
  data_mtcars <- mtcars
  data_mtcars <- data_mtcars[data_mtcars$mpg > 30, c("cyl", "mpg")]
})
```

If we have a code, we can run it and the result will be cached inside
RDO.

``` r
data_mtcars$run()
#> Evaluating RDO: data_mtcars ... done!
#> ...evaluation of data_mtcars completed.
```

We can now access the cache…

``` r
data_mtcars$cache
#>                cyl  mpg
#> Fiat 128         4 32.4
#> Honda Civic      4 30.4
#> Toyota Corolla   4 33.9
#> Lotus Europa     4 30.4
```

Let’s see if the RDO is validated…

``` r
data_mtcars$is_validated()
#> RDO: 'data_mtcars' is validated.
#> [1] TRUE
```

The RDO is validated, so we can be pretty sure that last evaluation of
the code give in result the cached data.

Let’s update the code just a bit and check the validation status…

``` r
data_mtcars$code <- expression({
  data_mtcars <- mtcars
  data_mtcars <- data_mtcars[data_mtcars$mpg > 31, c("cyl", "mpg")]
})
data_mtcars$is_validated()
#> RDO: 'data_mtcars' is NOT VALIDATED!
#> [1] FALSE
```

The RDO is NOT validated\! We need to update the cache…

``` r
data_mtcars$run()
#> Evaluating RDO: data_mtcars ... done!
#> ...evaluation of data_mtcars completed.
data_mtcars
#> <RDO>
#> Name: data_mtcars 
#> Dependencies: none.
#> Status:
#> - created:        2020-03-02 12:51:37 UTC 
#> - last changed:   2020-03-02 12:51:38 UTC 
#> - last touched:   2020-03-02 12:51:38 UTC 
#> - last validated: 2020-03-02 12:51:38 UTC 
#> - last run time:  0.0200021266937256 
#> - run time total: 0.0200021266937256 
#> - is validated?   TRUE 
#> - is locked?      FALSE 
#> - cache size:     0.001 Mb 
#> - cached total:   0.001 Mb
```

We have a validated RDO. At any time we can extract the cached data and
the reproducible R code.

``` r
data_mtcars$cache
#>                cyl  mpg
#> Fiat 128         4 32.4
#> Toyota Corolla   4 33.9
data_mtcars$code
#> <RDO code>
#> data_mtcars <- mtcars
#> data_mtcars <- data_mtcars[data_mtcars$mpg > 31, c("cyl", "mpg")]
#> <RDO>
#> Name: data_mtcars 
#> Dependencies: none.
#> Status:
#> - created:        2020-03-02 12:51:37 UTC 
#> - last changed:   2020-03-02 12:51:38 UTC 
#> - last touched:   2020-03-02 12:51:38 UTC 
#> - last validated: 2020-03-02 12:51:38 UTC 
#> - last run time:  0.0200021266937256 
#> - run time total: 0.0200021266937256 
#> - is validated?   TRUE 
#> - is locked?      FALSE 
#> - cache size:     0.001 Mb 
#> - cached total:   0.001 Mb
```

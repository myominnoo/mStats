
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mStats <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
maturing](man/figures/lifecycle-maturing.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->

mStats is a R package that provides a consistent set of functions that
help epidemiologists, and public health professionals solve common data
management and anlaysis challenges in health research. It comprises of
three major sets of functions:

-   `data management`
-   `statistical analysis`
-   `calculation of epidemiological measures`

These functions are in turn supported by another set of helper functions
which allows statistical calculation, displaying well-formatted output
and transferring final outputs to process further.

In a nutshell, mStats is designed to make epidemiological data analysis
quick and easy to create the final report for your project. You can see
it in action to [Get Started](./docs/articles/mStats.html).

## Installation

You can install the released version of mStats from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mStats")
```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of mStats from
[GitHub](https://github.com/).

``` r
# install.packages("devtools")
devtools::install_github("myominnoo/mStats")
```

## Cheat Sheet

\[to add later\]

## Masking

The `mStats`package contains two functions (`append`, `replace`) that
have the same names (doing different operation) with base R packages
(`stats` and `base`). Loading the `mStats` masks the functions from base
R. It means that when you use `append` function, you are using the
function from `mStats`. To avoid this:

-   use the syntax `package::function()`, for example `base::append()`
    or `mStats::append()`.
-   remove `mStats` from the session using `detach(package:mStats)`.

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(mStats)


## Describe dataset after data import
codebook(iris)
#>        Codebook
#>    Dataset's Name : `iris`
#>   Dataset's Label : 
#>       Vars : 5
#>        Obs : 150
#>  + -- ------------ + ----- ------- --- -- ----- +
#>  | No     Variable | Label    Type Obs NA NA(%) |
#>  + -- ------------ + ----- ------- --- -- ----- +
#>  |  1 Sepal.Length |       numeric 150  0   0.0 |
#>  |  2  Sepal.Width |       numeric 150  0   0.0 |
#>  |  3 Petal.Length |       numeric 150  0   0.0 |
#>  |  4  Petal.Width |       numeric 150  0   0.0 |
#>  |  5      Species |        factor 150  0   0.0 |
#>  + -- ------------ + ----- ------- --- -- ----- +


## Label variables and dataset
iris <- label(iris, "Edgar Anderson's Iris Data")
#>   (`iris` labeled as `Edgar Anderson's Iris Data`)
iris <- label(iris, Sepal.Length = "Length of Sepal",
              Petal.Length = "Length of Petal",
              Species = "Type of species")
#>   (`Sepal.Length` labeled as `Length of Sepal`)
#>   (`Petal.Length` labeled as `Length of Petal`)
#>   (`Species` labeled as `Type of species`)
codebook(iris)
#>        Codebook
#>    Dataset's Name : `iris`
#>   Dataset's Label : Edgar Anderson's Iris Data
#>       Vars : 5
#>        Obs : 150
#>  + -- ------------ + --------------- ------- --- -- ----- +
#>  | No     Variable |           Label    Type Obs NA NA(%) |
#>  + -- ------------ + --------------- ------- --- -- ----- +
#>  |  1 Sepal.Length | Length of Sepal numeric 150  0   0.0 |
#>  |  2  Sepal.Width |                 numeric 150  0   0.0 |
#>  |  3 Petal.Length | Length of Petal numeric 150  0   0.0 |
#>  |  4  Petal.Width |                 numeric 150  0   0.0 |
#>  |  5      Species | Type of species  factor 150  0   0.0 |
#>  + -- ------------ + --------------- ------- --- -- ----- +
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/myominnoo/mStats/issues). For questions and
other discussion, please directly email me
[dr.myominnoo@gmail.com](mailto::dr.myominnoo@gmail.com) or use the
[mStats mailing list](https://groups.google.com/g/mstats).

------------------------------------------------------------------------

Please note that this project is looking for contributors. By
participating in this project, you agree to abide by its terms with
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/),
version 1.0.0, available at
<https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/>.

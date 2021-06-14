
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mStats <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version-ago/mStats)](https://cran.r-project.org/package=mStats)
[![Downloads](https://cranlogs.r-pkg.org/badges/mStats)](https://cran.r-project.org/package=mStats)
[![Lifecycle:
maturing](man/figures/lifecycle-maturing.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->

mStats is a open-source R package to facilitate data management,
analysis and report writing with a focus on health research. It allows
to manipulate data, run statistical analysis, and calculate common
epidemiological measures. Their outputs, in turn, can be further
processed by highly sensible functions to produce publication-ready
tables.

In a nutshell, mStats is designed to make data analysis practical, quick
and easy for health researchers to create the final report in time. You
can see it in action to [Get
Started](https://mmoo.netlify.app/r/mStats/).

## Installation

You can install the released version of mStats from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mStats")
```

### Development version

If you want to use the development version of the bookdown package, you
can install the package from GitHub via the {remotes
package}(<https://remotes.r-lib.org/>):

``` r
remotes::install_github("myominnoo/mStats")
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

The easiest way to get started with mStats is to follow the guide
[here](https://myominnoo.github.io/mStats/). Below is a quick
demonstration of what mStats can do.

``` r
## to use pipe %>% for expression of sequential operations 
## Hence, no need to provide `data` argument in subsequent operations
library(magrittr) 

library(mStats)

## Describe dataset
codebook(iris)
#>   Codebook
#>            Name  : iris
#>            Label : 
#>            Vars  : 5
#>            Obs   : 150
#>  + -- ------------ + ----- ------- --- -- ----- +
#>  | No     Variable | Label    Type Obs NA NA(%) |
#>  + -- ------------ + ----- ------- --- -- ----- +
#>  |  1 Sepal.Length |       numeric 150  0   0.0 |
#>  |  2  Sepal.Width |       numeric 150  0   0.0 |
#>  |  3 Petal.Length |       numeric 150  0   0.0 |
#>  |  4  Petal.Width |       numeric 150  0   0.0 |
#>  |  5      Species |        factor 150  0   0.0 |
#>  + -- ------------ + ----- ------- --- -- ----- +
```

Below, `iris` dataset is passed to the function `codebook` via piping
`%>%`. Note that dataset passed through piping is assigned as object `.`
in this operation.

``` r
iris %>% 
  codebook
#>   Codebook
#>            Name  : .
#>            Label : 
#>            Vars  : 5
#>            Obs   : 150
#>  + -- ------------ + ----- ------- --- -- ----- +
#>  | No     Variable | Label    Type Obs NA NA(%) |
#>  + -- ------------ + ----- ------- --- -- ----- +
#>  |  1 Sepal.Length |       numeric 150  0   0.0 |
#>  |  2  Sepal.Width |       numeric 150  0   0.0 |
#>  |  3 Petal.Length |       numeric 150  0   0.0 |
#>  |  4  Petal.Width |       numeric 150  0   0.0 |
#>  |  5      Species |        factor 150  0   0.0 |
#>  + -- ------------ + ----- ------- --- -- ----- +
```

Here we demonstrate two sets of operations: labeling dataset and
variables.

``` r
## Label variables and dataset
## new object is assigned to make the changes permanent
iris_demo <- iris %>% 
  label("Edgar Anderson's Iris Data") %>% 
  label(Sepal.Length = "Length of Sepal",
        Petal.Length = "Length of Petal",
        Species = "Type of species") 
#>   (`.` labeled as `Edgar Anderson's Iris Data`)
#>   (`Sepal.Length` labeled as `Length of Sepal`)
#>   (`Petal.Length` labeled as `Length of Petal`)
#>   (`Species` labeled as `Type of species`)

codebook(iris_demo)
#>   Codebook
#>            Name  : iris_demo
#>            Label : Edgar Anderson's Iris Data
#>            Vars  : 5
#>            Obs   : 150
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

## The MIT License (MIT)

Copyright © 2013-2019 RStudio and others.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

------------------------------------------------------------------------

Please note that this project is looking for contributors. By
participating in this project, you agree to abide by its terms with
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/),
version 1.0.0, available at
<https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/>.
